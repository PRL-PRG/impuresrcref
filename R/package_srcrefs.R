package_function_names <- function(ns, include_internal = TRUE) {
  nms <- ls(ns, include_internal)
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

#' Check parse data for a package and impute srcrefs on all its functions.
#'
#' @param package Package name.
#' @param include_internal If `TRUE`, inspect all namespace functions.
#' @param verbose If `TRUE`, print a patch summary.
#'
#' @return Invisibly returns a list with:
#' - `package`: package name
#' - `fn_names`: inspected function names
#' - `failed`: failure messages (`NA` for successfully patched functions)
#' - `patched_count`: number of patched functions
#'
#' @export
impute_package_srcrefs <- function(
    package,
    include_internal = TRUE,
    verbose = TRUE) {
  if (!is.character(package) || length(package) != 1L || is.na(package) || identical(package, "")) {
    stop("`package` must be a single non-empty package name")
  }

  env <- loadNamespace(package)
  fn_names <- package_function_names(env, include_internal)
  failed <- character(length(fn_names))


  for (i in seq_along(fn_names)) {
    nm <- fn_names[i]
    fn <- get(nm, envir = env, inherits = FALSE)
    failed[i] <- NA

    parse_data <- tryCatch(getParseData(fn), error = function(e) NULL)
    if (is.null(parse_data)) {
      failed[i] <- "missing parse data"
      next
    }

    patched_fn <- tryCatch(impute_srcrefs(fn), error = function(e) e)
    if (inherits(patched_fn, "error")) {
      failed[i] <- paste(conditionMessage(patched_fn))
      next
    }

    locked <- bindingIsLocked(nm, env)

    if (locked) {
      unlockBinding(nm, env)
    }

    assign(nm, patched_fn, envir = env)

    if (locked) {
      lockBinding(nm, env)
    }
  }

  patched <- sum(is.na(failed))

  report <- list(
    package = package,
    fn_names = fn_names,
    failed = failed,
    patched_count = patched
  )

  if (verbose) {
    message(
      sprintf(
        "Patched %d/%d function(s) in package `%s`.",
        patched,
        length(fn_names),
        package
      )
    )
  }

  invisible(report)
}
