#' Source an R file and impute srcrefs on loaded functions.
#'
#' Sources a file with [base::sys.source()] and then rewrites function bindings
#' that were created or changed by that source call via [impute_srcrefs()].
#' This is the recommended entry point for source files because it can enforce
#' both source retention and parse-data retention.
#'
#' @param file Path to an R source file.
#' @param envir Environment where the file is sourced.
#' @param chdir Passed to [base::sys.source()].
#' @param keep.source Passed to [base::sys.source()]. Defaults to `TRUE`.
#' @param keep.parse.data Passed to [base::sys.source()]. Defaults to `TRUE`.
#' @param toplevel.env Passed to [base::sys.source()].
#' @param all.names Include hidden bindings when scanning for functions.
#'
#' @return Invisibly returns a list with:
#' - `file`: normalized file path
#' - `functions`: names of patched functions
#' - `count`: number of patched functions
#' - `keep.source` / `keep.parse.data`: effective source flags
#'
#' @examples
#' \dontrun{
#' tf <- tempfile(fileext = ".R")
#' writeLines("f <- function(x, y) if (x && y) f() else g()", tf)
#' env <- new.env(parent = baseenv())
#' source_impute_srcrefs(tf, envir = env)
#' env$f
#' }
#' @export
source_impute_srcrefs <- function(
  file,
  envir = parent.frame(),
  chdir = FALSE,
  keep.source = TRUE,
  keep.parse.data = TRUE,
  toplevel.env = as.environment(envir),
  all.names = TRUE
) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || identical(file, "")) {
    stop("`file` must be a single non-empty path string", call. = FALSE)
  }
  if (!file.exists(file)) {
    stop(sprintf("File does not exist: %s", file), call. = FALSE)
  }
  if (!is.environment(envir)) {
    stop("`envir` must be an environment", call. = FALSE)
  }

  pre_names <- ls(envir, all.names = all.names)
  # Snapshot pre-source bindings so we only patch functions introduced or
  # modified by this source call.
  pre_values <- mget(pre_names, envir = envir, inherits = FALSE, ifnotfound = vector("list", length(pre_names)))

  sys.source(
    file = file,
    envir = envir,
    chdir = chdir,
    keep.source = keep.source,
    keep.parse.data = keep.parse.data,
    toplevel.env = toplevel.env
  )

  post_names <- ls(envir, all.names = all.names)
  imputed <- character()

  for (nm in post_names) {
    if (!exists(nm, envir = envir, inherits = FALSE)) {
      next
    }

    value <- get(nm, envir = envir, inherits = FALSE)
    if (!is.function(value)) {
      next
    }

    changed <- !(nm %in% pre_names) || !identical(value, pre_values[[nm]])
    if (!changed) {
      next
    }

    if (bindingIsLocked(nm, envir)) {
      # Do not mutate locked user bindings in place.
      warning(sprintf("Skipping locked binding `%s`", nm), call. = FALSE)
      next
    }

    # Rewrite only changed function bindings with srcref-imputed variants.
    assign(nm, impute_srcrefs(value), envir = envir)
    imputed <- c(imputed, nm)
  }

  imputed <- sort(unique(imputed))

  invisible(list(
    file = normalizePath(file, mustWork = FALSE),
    envir = envir,
    functions = imputed,
    count = length(imputed),
    keep.source = keep.source,
    keep.parse.data = keep.parse.data
  ))
}
