deparse_one_line <- function(expr) {
  # Canonical single-line representation for stable snapshot output.
  txt <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")
  gsub("[[:space:]]+", " ", txt)
}

format_srcref_tuple <- function(sr) {
  vals <- unclass(sr)
  paste0("(", paste(vals, collapse = ","), ")")
}

srcref_to_text <- function(sr) {
  # Extract exact text span covered by an srcref from its backing srcfile.
  srcfile <- attr(sr, "srcfile", exact = TRUE)
  if (is.null(srcfile)) {
    return(NA_character_)
  }

  lines <- getSrcLines(srcfile, sr[1], sr[3])
  if (length(lines) == 0L) {
    return(NA_character_)
  }

  if (length(lines) == 1L) {
    lines[1L] <- substr(lines[1L], sr[2], sr[4])
  } else {
    lines[1L] <- substr(lines[1L], sr[2], nchar(lines[1L]))
    lines[length(lines)] <- substr(lines[length(lines)], 1L, sr[4])
  }

  paste(lines, collapse = "\n")
}

canonicalize_text <- function(text) {
  # Parse/deparse normalize before text comparison to avoid formatting noise.
  if (is.na(text)) {
    return(NA_character_)
  }

  parsed <- tryCatch(parse(text = text, keep.source = FALSE), error = function(e) NULL)
  if (is.null(parsed) || length(parsed) != 1L) {
    return(NA_character_)
  }

  deparse_one_line(parsed[[1L]])
}

transparent_srcref <- function(node) {
  # Injected transparent braces store the same srcref twice (brace + payload).
  # If those entries differ, treat it as non-transparent.
  sr <- attr(node, "srcref", exact = TRUE)
  if (is.null(sr)) {
    return(NULL)
  }

  sr_list <- if (is.list(sr)) sr else list(sr)
  if (length(sr_list) < 2L) {
    return(NULL)
  }

  if (identical(sr_list[[1L]], sr_list[[2L]])) {
    return(sr_list[[1L]])
  }

  NULL
}

strip_transparent_braces <- function(expr) {
  # Remove only injected transparent braces so semantic expression comparison
  # can ignore imputed wrapper calls.
  if (is.call(expr)) {
    if (is_braced(expr) && length(expr) >= 2L && !is.null(transparent_srcref(expr))) {
      return(strip_transparent_braces(expr[[2L]]))
    }

    parts <- as.list(expr)
    for (i in seq_along(parts)) {
      parts[[i]] <- strip_transparent_braces(parts[[i]])
    }
    return(as.call(parts))
  }

  expr
}

collect_transparent_srcref_checks <- function(expr_or_fn) {
  lines <- character()
  failures <- character()
  checked <- 0L

  walk <- function(node, path) {
    if (is.call(node)) {
      if (is_braced(node)) {
        sr <- transparent_srcref(node)
        if (!is.null(sr) && length(node) >= 2L) {
          checked <<- checked + 1L

          sr_text <- canonicalize_text(srcref_to_text(sr))
          expr_text <- deparse_one_line(strip_transparent_braces(node[[2L]]))
          ok <- !is.na(sr_text) && identical(sr_text, expr_text)

          lines <<- c(
            lines,
            sprintf(
              "check path=%s ok=%s sr_text=%s expr_text=%s",
              path,
              ok,
              encodeString(sr_text, quote = "\""),
              encodeString(expr_text, quote = "\"")
            )
          )

          if (!ok) {
            failures <<- c(failures, path)
          }
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
    # For functions, check both default arguments and body expressions.
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

  list(
    checked = checked,
    ok = length(failures) == 0L,
    failures = failures,
    lines = lines
  )
}

assert_transparent_srcref_consistency <- function(expr_or_fn) {
  checks <- collect_transparent_srcref_checks(expr_or_fn)
  if (!checks$ok) {
    stop(
      sprintf(
        "Transparent srcref text mismatch at: %s",
        paste(checks$failures, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(checks)
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
    # Emit paths for formals and body so snapshots are easy to locate in AST.
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
