deparse_one_line <- function(expr) {
  txt <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")
  gsub("[[:space:]]+", " ", txt)
}

format_srcref_tuple <- function(sr) {
  vals <- unclass(sr)
  paste0("(", paste(vals, collapse = ","), ")")
}

collect_srcref_sites <- function(expr_or_fn) {
  out <- character()

  walk <- function(node, path) {
    if (is.call(node)) {
      if (is_braced(node)) {
        sr <- attr(node, "srcref", exact = TRUE)
        if (!is.null(sr)) {
          sr_list <- if (is.list(sr)) sr else list(sr)
          sr1 <- if (length(sr_list) >= 1L) format_srcref_tuple(sr_list[[1L]]) else "NA"
          sr2 <- if (length(sr_list) >= 2L) format_srcref_tuple(sr_list[[2L]]) else "NA"

          out <<- c(
            out,
            sprintf(
              "path=%s node=%s sr1=%s sr2=%s",
              path,
              deparse_one_line(node),
              sr1,
              sr2
            )
          )
        }
      }

      parts <- as.list(node)
      for (i in seq_along(parts)) {
        walk(parts[[i]], sprintf("%s/%d", path, i))
      }
      return(invisible(NULL))
    }

    if (is.expression(node) || is.list(node) || is.pairlist(node)) {
      for (i in seq_along(node)) {
        walk(node[[i]], sprintf("%s/%d", path, i))
      }
    }

    invisible(NULL)
  }

  if (is.function(expr_or_fn)) {
    fmls <- formals(expr_or_fn)
    fml_text <- as.character(fmls)

    for (i in seq_along(fml_text)) {
      if (identical(fml_text[[i]], "")) {
        next
      }

      nm <- names(fmls)[[i]]
      if (is.null(nm) || identical(nm, "")) {
        nm <- as.character(i)
      }

      walk(fmls[[i]], sprintf("formals/%s", nm))
    }

    walk(body(expr_or_fn), "body")
  } else {
    walk(expr_or_fn, "root")
  }

  out
}

write_srcref_sites <- function(expr_or_fn, path) {
  lines <- collect_srcref_sites(expr_or_fn)
  writeLines(lines, con = path, useBytes = TRUE)
  invisible(lines)
}
