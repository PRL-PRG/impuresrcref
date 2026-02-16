is_call_named <- function(expr, name) {
  is.call(expr) && is.symbol(expr[[1]]) && identical(as.character(expr[[1]]), name)
}

is_braced <- function(expr) {
  is_call_named(expr, "{")
}

is_missing_arg <- function(x) {
  is.symbol(x) && identical(as.character(x), "")
}

is_logical_op_call <- function(expr) {
  is.call(expr) &&
    is.symbol(expr[[1]]) &&
    as.character(expr[[1]]) %in% c("&&", "||", "&", "|")
}

expr_children <- function(node_id, ctx) {
  # `node_id` is an `expr` row id from parse data. This returns child `expr`
  # node ids in source order so AST argument positions can be mapped to parse
  # ranges deterministically.
  rows <- ctx$expr[ctx$expr$parent == node_id, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(integer())
  }

  rows <- rows[order(rows$line1, rows$col1, rows$id), , drop = FALSE]
  rows$id
}

node_srcref <- function(node_id, ctx) {
  # Build an srcref from parse-data coordinates for a single expression node.
  # The offsets are needed because parsed text can start at a non-1 line/column
  # inside the original source file.
  row <- ctx$expr_index[[as.character(node_id)]]
  if (is.null(row)) {
    stop(sprintf("Missing parse node id %s", node_id), call. = FALSE)
  }

  line1 <- row$line1 + ctx$line_offset
  line2 <- row$line2 + ctx$line_offset
  col1 <- row$col1 + if (row$line1 == 1L) ctx$first_col_offset else 0L
  col2 <- row$col2 + if (row$line2 == 1L) ctx$first_col_offset else 0L

  sr <- c(line1, col1, line2, col2, col1, col2, line1, line2)
  sr <- as.integer(sr)
  attr(sr, "srcfile") <- ctx$srcfile
  class(sr) <- "srcref"
  sr
}

wrap_with_transparent_brace <- function(expr, sr) {
  # Transparent wrapper: both srcref entries point to the wrapped expression
  # span, making injected braces source-invisible for mapping purposes.
  out <- call("{", expr)
  attr(out, "srcref") <- list(sr, sr)
  out
}

source_text_from_srcref <- function(fn) {
  sr <- attr(fn, "srcref", exact = TRUE)

  # Fallback for functions that do not carry srcref metadata.
  if (is.null(sr)) {
    if (!isTRUE(getOption("impuresrcref.allow_deparse_fallback", FALSE))) {
      stop(
        paste(
          "Function has no srcref metadata.",
          "Set options(impuresrcref.allow_deparse_fallback = TRUE)",
          "to enable deparse-based fallback."
        ),
        call. = FALSE
      )
    }

    txt <- paste(deparse(fn, width.cutoff = 500L), collapse = "\n")
    return(list(
      text = txt,
      srcfile = srcfilecopy("<deparse>", txt),
      line_offset = 0L,
      first_col_offset = 0L
    ))
  }

  srcfile <- attr(sr, "srcfile", exact = TRUE)
  if (is.null(srcfile)) {
    stop("Function srcref is missing srcfile", call. = FALSE)
  }

  lines <- getSrcLines(srcfile, sr[1], sr[3])
  if (length(lines) == 0L) {
    stop("Could not read source lines for function", call. = FALSE)
  }

  if (length(lines) == 1L) {
    lines[1] <- substr(lines[1], sr[2], sr[4])
  } else {
    lines[1] <- substr(lines[1], sr[2], nchar(lines[1]))
    lines[length(lines)] <- substr(lines[length(lines)], 1L, sr[4])
  }

  list(
    # `text` is what we parse to obtain getParseData(); offsets map parse
    # coordinates back to the original source file coordinates.
    text = paste(lines, collapse = "\n"),
    srcfile = srcfile,
    line_offset = sr[1] - 1L,
    first_col_offset = sr[2] - 1L
  )
}

map_generic_indices <- function(parts, child_ids, drop_first_child = FALSE) {
  ids <- child_ids
  if (drop_first_child && length(ids) > 0L) {
    ids <- ids[-1L]
  }

  n <- length(parts)
  present <- which(!vapply(parts, is_missing_arg, logical(1)))

  if (length(ids) == n) {
    return(list(indices = seq_len(n), ids = ids))
  }
  if (length(ids) == n - 1L && n >= 2L) {
    return(list(indices = seq.int(2L, n), ids = ids))
  }
  if (length(ids) == length(present)) {
    return(list(indices = present, ids = ids))
  }
  if (length(present) >= 1L && length(ids) == length(present) - 1L) {
    return(list(indices = present[-1L], ids = ids))
  }

  NULL
}

node_has_token <- function(node_id, token, ctx) {
  any(ctx$pd$parent == node_id & ctx$pd$token == token)
}

rebuild_call <- function(parts, template) {
  out <- as.call(parts)
  attrs <- attributes(template)
  if (!is.null(attrs)) {
    attributes(out) <- attrs
  }
  out
}

transform_expr <- function(expr, node_id, ctx) {
  # Core imputation walker.
  #
  # - `expr`: current language object (AST node).
  # - `node_id`: parse-data `expr` id corresponding to `expr`.
  # - `ctx`: parse-data context with:
  #     * `pd`: full parse table.
  #     * `expr`: parse rows where token == "expr".
  #     * `expr_index`: fast lookup from expr id -> row.
  #     * `srcfile`, `line_offset`, `first_col_offset`: srcref reconstruction
  #       metadata.
  #
  # The recursion keeps AST and parse tree aligned. Whenever a target slot is
  # not already `{ ... }`, we inject a brace call and impute its srcref from the
  # corresponding parse-data node.
  if (!is.call(expr)) {
    return(expr)
  }

  if (is_braced(expr) && !node_has_token(node_id, "'{'", ctx)) {
    # Already-injected transparent braces may not exist in original parse data.
    # Recurse into the child with the same node_id and preserve brace srcref.
    parts <- as.list(expr)
    sr <- attr(expr, "srcref", exact = TRUE)

    if (length(parts) >= 2L) {
      parts[[2L]] <- transform_expr(parts[[2L]], node_id, ctx)
    }

    out <- rebuild_call(parts, expr)
    if (!is.null(sr)) {
      attr(out, "srcref") <- sr
    }
    return(out)
  }

  parts <- as.list(expr)
  op <- if (is.symbol(parts[[1]])) as.character(parts[[1]]) else ""
  child_ids <- expr_children(node_id, ctx)

  recurse_slot <- function(i, child_id, wrap = FALSE) {
    # Helper for "recurse into slot i, then optionally brace-wrap and impute".
    value <- transform_expr(parts[[i]], child_id, ctx)
    if (wrap && !is_braced(value)) {
      value <- wrap_with_transparent_brace(value, node_srcref(child_id, ctx))
    }
    parts[[i]] <<- value
  }

  if (identical(op, "function")) {
    fmls <- parts[[2]]
    fml_text <- as.character(fmls)
    cursor <- 1L

    for (idx in seq_along(fml_text)) {
      if (identical(fml_text[[idx]], "")) {
        next
      }

      if (cursor > length(child_ids)) {
        stop("Parse mapping mismatch for function formals", call. = FALSE)
      }

      cid <- child_ids[[cursor]]
      cursor <- cursor + 1L

      val <- fmls[[idx]]
      val <- transform_expr(val, cid, ctx)
      if (!is_braced(val)) {
        val <- wrap_with_transparent_brace(val, node_srcref(cid, ctx))
      }
      fmls[[idx]] <- val
    }

    if (cursor > length(child_ids)) {
      stop("Parse mapping mismatch for function body", call. = FALSE)
    }

    body_id <- child_ids[[cursor]]
    body_expr <- transform_expr(parts[[3]], body_id, ctx)
    if (!is_braced(body_expr)) {
      body_expr <- wrap_with_transparent_brace(body_expr, node_srcref(body_id, ctx))
    }

    parts[[2]] <- fmls
    parts[[3]] <- body_expr
    return(rebuild_call(parts, expr))
  }

  if (identical(op, "if")) {
    if (length(child_ids) < 2L) {
      stop("Parse mapping mismatch for if expression", call. = FALSE)
    }

    cond <- transform_expr(parts[[2L]], child_ids[[1L]], ctx)
    if (!is_braced(cond) && !is_logical_op_call(cond)) {
      cond <- wrap_with_transparent_brace(cond, node_srcref(child_ids[[1L]], ctx))
    }
    parts[[2L]] <- cond

    recurse_slot(3L, child_ids[[2L]], wrap = TRUE)
    if (length(parts) >= 4L && length(child_ids) >= 3L) {
      recurse_slot(4L, child_ids[[3L]], wrap = TRUE)
    }

    return(rebuild_call(parts, expr))
  }

  if (identical(op, "while")) {
    if (length(child_ids) < 2L) {
      stop("Parse mapping mismatch for while expression", call. = FALSE)
    }

    cond <- transform_expr(parts[[2L]], child_ids[[1L]], ctx)
    if (!is_braced(cond) && !is_logical_op_call(cond)) {
      cond <- wrap_with_transparent_brace(cond, node_srcref(child_ids[[1L]], ctx))
    }
    parts[[2L]] <- cond

    recurse_slot(3L, child_ids[[2L]], wrap = TRUE)
    return(rebuild_call(parts, expr))
  }

  if (identical(op, "for")) {
    # Parse data represents `for (...)` sequence under a dedicated `forcond`
    # subtree, so we fetch that expr id explicitly instead of relying only on
    # direct child expr ids.
    forcond <- ctx$pd[ctx$pd$parent == node_id & ctx$pd$token == "forcond", , drop = FALSE]
    seq_id <- integer()
    if (nrow(forcond) == 1L) {
      seq_rows <- ctx$pd[ctx$pd$parent == forcond$id[[1L]] & ctx$pd$token == "expr", , drop = FALSE]
      if (nrow(seq_rows) >= 1L) {
        seq_rows <- seq_rows[order(seq_rows$line1, seq_rows$col1, seq_rows$id), , drop = FALSE]
        seq_id <- seq_rows$id[[1L]]
      }
    }

    body_ids <- child_ids
    if (length(body_ids) >= 1L && length(seq_id) == 1L && identical(body_ids[[1L]], seq_id)) {
      body_ids <- body_ids[-1L]
    }

    if (length(seq_id) != 1L || length(body_ids) < 1L) {
      stop("Parse mapping mismatch for for expression", call. = FALSE)
    }

    recurse_slot(3L, seq_id[[1L]], wrap = TRUE)
    recurse_slot(4L, body_ids[[1L]], wrap = TRUE)
    return(rebuild_call(parts, expr))
  }

  if (identical(op, "repeat")) {
    if (length(child_ids) < 1L) {
      stop("Parse mapping mismatch for repeat expression", call. = FALSE)
    }

    recurse_slot(2L, child_ids[[1L]], wrap = TRUE)
    return(rebuild_call(parts, expr))
  }

  if (identical(op, "switch")) {
    ids <- child_ids
    if (length(ids) > 0L) {
      ids <- ids[-1L]
    }

    cursor <- 1L
    for (i in seq.int(2L, length(parts))) {
      if (is_missing_arg(parts[[i]])) {
        next
      }

      if (cursor > length(ids)) {
        break
      }

      cid <- ids[[cursor]]
      cursor <- cursor + 1L

      recurse_slot(i, cid, wrap = TRUE)
    }

    return(rebuild_call(parts, expr))
  }

  if (op %in% c("&&", "||", "&", "|")) {
    if (length(child_ids) < 2L) {
      stop(sprintf("Parse mapping mismatch for %s expression", op), call. = FALSE)
    }

    recurse_slot(2L, child_ids[[1L]], wrap = TRUE)
    recurse_slot(3L, child_ids[[2L]], wrap = TRUE)
    return(rebuild_call(parts, expr))
  }

  mapping <- map_generic_indices(parts, child_ids)
  if (is.null(mapping)) {
    return(expr)
  }

  for (k in seq_along(mapping$indices)) {
    i <- mapping$indices[[k]]
    cid <- mapping$ids[[k]]
    parts[[i]] <- transform_expr(parts[[i]], cid, ctx)
  }

  rebuild_call(parts, expr)
}

#' Impute srcrefs for missing control-flow braces in a function AST.
#'
#' Traverses a function's AST and wraps unbraced expressions in control-flow
#' positions with `{ ... }` while attaching transparent srcrefs to the injected
#' brace calls. The srcref assigned to an injected brace matches the span of the
#' wrapped expression so that source mapping stays aligned with original code.
#'
#' Covered constructs include:
#' - `if` / `else`
#' - `for`, `while`, `repeat`
#' - `switch`
#' - logical operators (`&&`, `||`, `&`, `|`)
#' - function defaults and function bodies
#'
#' @param fn A function.
#'
#' @return A function with transformed body/formals and preserved function-level
#'   attributes (including srcref/srcfile metadata when present).
#'
#' @details
#' For functions without srcref metadata, deparse-based fallback is disabled by
#' default. To allow fallback, set
#' `options(impuresrcref.allow_deparse_fallback = TRUE)`.
#'
#' @examples
#' options(keep.source = TRUE)
#' f <- eval(parse(text = "function(x, y) if (x && y) f() else g()", keep.source = TRUE)[[1]])
#' g <- impute_srcrefs(f)
#' g
#' @export
impute_srcrefs <- function(fn) {
  if (!is.function(fn)) {
    stop("`fn` must be a function", call. = FALSE)
  }

  fn_attrs <- attributes(fn)
  src <- source_text_from_srcref(fn)
  parsed <- parse(text = src$text, keep.source = TRUE)
  pd <- getParseData(parsed)

  if (is.null(pd) || nrow(pd) == 0L) {
    stop("Could not obtain parse data for function", call. = FALSE)
  }

  expr_rows <- pd[pd$token == "expr", , drop = FALSE]
  if (nrow(expr_rows) == 0L) {
    stop("Parse data does not contain expression nodes", call. = FALSE)
  }

  root_rows <- expr_rows[expr_rows$parent == 0L, , drop = FALSE]
  if (nrow(root_rows) != 1L) {
    stop("Expected exactly one top-level expression for function source", call. = FALSE)
  }

  root_id <- root_rows$id[[1L]]

  # Shared parse context used by the recursive walker.
  ctx <- list(
    pd = pd,
    expr = expr_rows,
    expr_index = split(expr_rows, expr_rows$id),
    srcfile = src$srcfile,
    line_offset = src$line_offset,
    first_col_offset = src$first_col_offset
  )

  fn_expr <- as.call(list(as.name("function"), formals(fn), body(fn)))
  # Start traversal from the top-level function expression parse node.
  transformed <- transform_expr(fn_expr, root_id, ctx)

  out <- fn
  formals(out) <- transformed[[2L]]
  body(out) <- transformed[[3L]]

  if (!is.null(fn_attrs)) {
    for (nm in names(fn_attrs)) {
      attr(out, nm) <- fn_attrs[[nm]]
    }
  }

  out
}
