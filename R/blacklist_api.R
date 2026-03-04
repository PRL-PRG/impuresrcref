blacklist_option_name <- "imputesrcref.wrap_arg_blacklist"

specialsxp_builtin_names <- local({
  nms <- builtins()
  out <- nms[vapply(nms, function(nm) {
    obj <- get(nm, envir = baseenv(), inherits = FALSE)
    is.primitive(obj) && identical(typeof(obj), "special")
  }, logical(1))]
  sort(unique(out))
})

normalize_blacklist_names <- function(functions, arg = "functions") {
  if (is.null(functions)) {
    return(character())
  }
  if (!is.character(functions)) {
    stop(sprintf("`%s` must be NULL or a character vector", arg), call. = FALSE)
  }

  out <- trimws(functions)
  out <- out[!is.na(out) & nzchar(out)]
  sort(unique(out))
}

user_blacklist_names <- function() {
  normalize_blacklist_names(getOption(blacklist_option_name, NULL), arg = blacklist_option_name)
}

effective_blacklist_names <- function() {
  sort(unique(c(specialsxp_builtin_names, user_blacklist_names())))
}

#' Get call names blacklisted from generic argument wrapping.
#'
#' By default, [impute_srcrefs()] skips argument wrapping for primitive
#' `SPECIALSXP` calls discovered from [builtins()]. This getter can return only
#' user-configured entries, or the effective blacklist including defaults.
#'
#' @param include_default If `TRUE`, include built-in `SPECIALSXP` names.
#'
#' @return A sorted unique character vector of call names.
#'
#' @examples
#' head(get_impute_blacklist())
#' set_impute_blacklist(c("str_c", "paste"))
#' get_impute_blacklist(include_default = FALSE)
#' reset_impute_blacklist()
#' @export
get_impute_blacklist <- function(include_default = TRUE) {
  if (!is.logical(include_default) || length(include_default) != 1L || is.na(include_default)) {
    stop("`include_default` must be TRUE or FALSE", call. = FALSE)
  }

  if (isTRUE(include_default)) {
    return(effective_blacklist_names())
  }

  user_blacklist_names()
}

#' Set user call names blacklisted from generic argument wrapping.
#'
#' User entries are stored in `options(imputesrcref.wrap_arg_blacklist = ...)`.
#'
#' @param functions Character vector of call names. `NULL` clears user entries.
#' @param append If `TRUE` append to existing user entries; otherwise replace.
#'
#' @return Invisibly returns current user-configured blacklist entries.
#' @export
set_impute_blacklist <- function(functions, append = TRUE) {
  if (!is.logical(append) || length(append) != 1L || is.na(append)) {
    stop("`append` must be TRUE or FALSE", call. = FALSE)
  }

  incoming <- normalize_blacklist_names(functions, arg = "functions")
  current <- if (isTRUE(append)) user_blacklist_names() else character()
  next_values <- sort(unique(c(current, incoming)))

  options(imputesrcref.wrap_arg_blacklist = next_values)
  invisible(next_values)
}

#' Reset user call names blacklisted from generic argument wrapping.
#'
#' Clears user entries configured via [set_impute_blacklist()].
#'
#' @return Invisibly returns an empty character vector.
#' @export
reset_impute_blacklist <- function() {
  options(imputesrcref.wrap_arg_blacklist = NULL)
  invisible(character())
}
