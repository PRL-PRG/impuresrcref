package_install_command <- function(package) {
  # Source install + package source options is required for parse-data retention.
  sprintf(
    "local({ options(keep.source.pkgs = TRUE, keep.parse.data.pkgs = TRUE); install.packages(\"%s\", type = \"source\") })",
    package
  )
}

validate_package_name <- function(package) {
  if (!is.character(package) || length(package) != 1L || is.na(package) || identical(package, "")) {
    stop("`package` must be a single non-empty package name", call. = FALSE)
  }
}

package_function_names <- function(ns, include_internal = TRUE) {
  # Discover mutable closure functions in namespace scope.
  nms <- if (include_internal) {
    ls(ns, all.names = TRUE)
  } else {
    getNamespaceExports(getNamespaceName(ns))
  }

  out <- character()
  for (nm in nms) {
    if (!exists(nm, envir = ns, inherits = FALSE)) {
      next
    }
    obj <- get(nm, envir = ns, inherits = FALSE)
    if (is.function(obj) && !is.primitive(obj)) {
      out <- c(out, nm)
    }
  }

  sort(unique(out))
}

function_has_parse_data <- function(fn) {
  # Parse data is available only when srcref -> srcfile exists and
  # getParseData(srcfile) returns a non-empty table.
  sr <- attr(fn, "srcref", exact = TRUE)
  if (is.null(sr)) {
    return(FALSE)
  }

  srcfile <- attr(sr, "srcfile", exact = TRUE)
  if (is.null(srcfile)) {
    return(FALSE)
  }

  pd <- tryCatch(getParseData(srcfile), error = function(e) NULL)
  !is.null(pd) && nrow(pd) > 0L
}

#' Check whether an installed package has parse data on its functions.
#'
#' Inspects functions in a package namespace and reports whether any of them
#' carry parse data retrievable via `getParseData(attr(fn, "srcref")$srcfile)`.
#'
#' @param package Package name.
#' @param include_internal If `TRUE`, inspect all namespace functions.
#'
#' @return A list with parse-data coverage summary and an `install_command`
#'   string that can be used to reinstall the package with parse data enabled.
#'
#' @examples
#' \dontrun{
#' check_package_parse_data("MASS")
#' }
#' @export
check_package_parse_data <- function(package, include_internal = TRUE) {
  validate_package_name(package)
  ns <- loadNamespace(package)
  fn_names <- package_function_names(ns, include_internal = include_internal)

  with_srcref <- 0L
  with_parse_data <- 0L

  for (nm in fn_names) {
    fn <- get(nm, envir = ns, inherits = FALSE)
    sr <- attr(fn, "srcref", exact = TRUE)
    if (!is.null(sr)) {
      with_srcref <- with_srcref + 1L
    }
    if (function_has_parse_data(fn)) {
      with_parse_data <- with_parse_data + 1L
    }
  }

  list(
    package = package,
    include_internal = include_internal,
    total_functions = length(fn_names),
    functions_with_srcref = with_srcref,
    functions_with_parse_data = with_parse_data,
    has_parse_data = with_parse_data > 0L,
    install_command = package_install_command(package)
  )
}

#' Check parse data for a package and impute srcrefs on all its functions.
#'
#' If parse data is not available, this function does not patch and instead can
#' print an installation command that enables package-level source/parse-data
#' retention for source installs.
#'
#' @param package Package name.
#' @param include_internal If `TRUE`, patch all namespace functions.
#' @param prompt_install If `TRUE`, print reinstall command when parse data is missing.
#' @param verbose If `TRUE`, print patch summary.
#'
#' @return Invisibly, a list containing parse-data status plus patch results:
#' - `patched`: patched function names
#' - `patched_count`: number of patched functions
#' - `failed`: failures (if any)
#'
#' @examples
#' \dontrun{
#' impute_package_srcrefs("MASS")
#' }
#' @export
impute_package_srcrefs <- function(
  package,
  include_internal = TRUE,
  prompt_install = TRUE,
  verbose = TRUE
) {
  report <- check_package_parse_data(package, include_internal = include_internal)

  if (!report$has_parse_data) {
    if (prompt_install) {
      message(
        sprintf(
          "Package `%s` has no parse data. Reinstall with:\n%s",
          package,
          report$install_command
        )
      )
    }

    report$patched <- character()
    report$patched_count <- 0L
    report$failed <- character()
    return(invisible(report))
  }

  ns <- asNamespace(package)
  fn_names <- package_function_names(ns, include_internal = include_internal)
  attach_env_name <- paste0("package:", package)
  attach_env <- if (attach_env_name %in% search()) as.environment(attach_env_name) else NULL
  exported <- getNamespaceExports(package)

  patched <- character()
  failed <- character()

  patch_binding <- function(env, nm, value) {
    # Namespace/attached bindings are often locked; patch by temporarily
    # unlocking and restoring lock state afterwards.
    locked <- bindingIsLocked(nm, env)
    if (locked) {
      unlockBinding(nm, env)
    }
    assign(nm, value, envir = env)
    if (locked) {
      lockBinding(nm, env)
    }
  }

  for (nm in fn_names) {
    fn <- get(nm, envir = ns, inherits = FALSE)
    patched_fn <- tryCatch(impute_srcrefs(fn), error = function(e) e)
    if (inherits(patched_fn, "error")) {
      failed <- c(failed, sprintf("%s: %s", nm, conditionMessage(patched_fn)))
      next
    }

    ns_result <- tryCatch(
      {
        patch_binding(ns, nm, patched_fn)
        TRUE
      },
      error = function(e) e
    )
    if (inherits(ns_result, "error")) {
      failed <- c(failed, sprintf("%s: %s", nm, conditionMessage(ns_result)))
      next
    }

    if (!is.null(attach_env) && nm %in% exported && exists(nm, envir = attach_env, inherits = FALSE)) {
      # Keep attached package environment in sync with patched namespace export.
      attach_result <- tryCatch(
        {
          patch_binding(attach_env, nm, patched_fn)
          TRUE
        },
        error = function(e) e
      )
      if (inherits(attach_result, "error")) {
        failed <- c(failed, sprintf("%s (search path): %s", nm, conditionMessage(attach_result)))
      }
    }

    patched <- c(patched, nm)
  }

  report$patched <- sort(unique(patched))
  report$patched_count <- length(report$patched)
  report$failed <- failed

  if (verbose) {
    message(
      sprintf(
        "Patched %d function(s) in package `%s`.",
        report$patched_count,
        package
      )
    )
    if (length(failed) > 0L) {
      message(sprintf("Failed to patch %d function(s).", length(failed)))
    }
  }

  invisible(report)
}
