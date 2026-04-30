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

is_unquote_call <- function(expr) {
  # rlang's !! and !!! unquoting operators appear in the R AST as double/triple
  # negation: !!x -> !(!(x)), !!!x -> !(!(!(x))). Wrapping them in transparent
  # braces changes semantics when the surrounding call is later processed by
  # rlang::inject() or similar, because {!!x} injects a block instead of the
  # bare value that !! would produce.
  # The parenthesized form (!!x) parses to `(`(!(!(x))): the outer call is `(`
  # not `!`, so we recurse into the `(` argument to check for the double-neg.
  if (!is.call(expr) || length(expr) != 2L) {
    return(FALSE)
  }
  head <- expr[[1L]]
  if (identical(head, as.name("!"))) {
    inner <- expr[[2L]]
    return(
      is.call(inner) &&
        length(inner) == 2L &&
        identical(inner[[1L]], as.name("!"))
    )
  }
  if (identical(head, as.name("("))) {
    return(is_unquote_call(expr[[2L]]))
  }
  FALSE
}

is_unary_arith_call <- function(expr) {
  # Unary - and + must not be brace-wrapped as generic call arguments.
  # In DSL contexts (tidyselect, dplyr) -f() is a structural "deselect f()"
  # operator: wrapping to {-f()} causes the block to evaluate to a negative
  # integer, which DSL validators reject ("Selections can't have negative values").
  is.call(expr) &&
    length(expr) == 2L &&
    is.symbol(expr[[1L]]) &&
    as.character(expr[[1L]]) %in% c("-", "+")
}

visual_col_to_byte_col <- function(line, visual_col) {
  # Convert a visual (tab-expanded, 8-wide tab stops) column number to the
  # character position in `line` that starts at that visual column.
  # R's srcref slots [5] and [6] record visual columns while substr() requires
  # character (byte) positions: for tab-indented code these differ.
  # Example: `\t  function(x)` has visual col 11 for `f` but byte pos 4.
  chars <- strsplit(line, "", fixed = TRUE)[[1L]]
  vis <- 1L
  for (i in seq_along(chars)) {
    if (vis >= visual_col) {
      return(i)
    }
    if (chars[[i]] == "\t") {
      vis <- ((vis - 1L) %/% 8L + 1L) * 8L + 1L
    } else {
      vis <- vis + 1L
    }
  }
  length(chars)
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
    if (!isTRUE(getOption("imputesrcref.allow_deparse_fallback", FALSE))) {
      message(
        paste(
          "Function has no srcref metadata and deparse fallback is disabled; no changes were made.",
          "Set options(imputesrcref.allow_deparse_fallback = TRUE)",
          "to enable deparse-based fallback."
        )
      )
      return(NULL)
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

  sr <- as.integer(sr)

  # For installed package functions with preserved parse data, srcref often
  # stores parse-table line span in slots 7/8 while slots 1/3 are remapped by
  # #line directives. Try parsed span first, then fall back.
  candidates <- list(
    c(sr[7], sr[8], sr[5], sr[6]),
    c(sr[1], sr[3], sr[5], sr[6]),
    c(sr[1], sr[3], sr[2], sr[4])
  )

  chosen <- NULL
  lines <- character()

  for (cand in candidates) {
    if (length(cand) != 4L || any(is.na(cand)) || any(cand <= 0L)) {
      next
    }

    start_line <- cand[[1L]]
    end_line <- cand[[2L]]
    start_col <- cand[[3L]]
    end_col <- cand[[4L]]

    if (end_line < start_line) {
      next
    }
    if (end_line == start_line && end_col < start_col) {
      next
    }

    attempt <- tryCatch(
      getSrcLines(srcfile, start_line, end_line),
      error = function(e) character()
    )

    if (length(attempt) == 0L) {
      next
    }

    chosen <- list(
      start_line = start_line,
      end_line = end_line,
      start_col = start_col,
      end_col = end_col
    )
    lines <- attempt
    break
  }

  if (is.null(chosen) || length(lines) == 0L) {
    stop("Could not read source lines for function", call. = FALSE)
  }

  if (length(lines) == 1L) {
    byte_sc <- visual_col_to_byte_col(lines[1L], chosen$start_col)
    byte_ec <- visual_col_to_byte_col(lines[1L], chosen$end_col)
    lines[1L] <- substr(lines[1L], byte_sc, byte_ec)
  } else {
    byte_sc <- visual_col_to_byte_col(lines[1L], chosen$start_col)
    lines[1L] <- substr(lines[1L], byte_sc, nchar(lines[1L]))
    byte_ec <- visual_col_to_byte_col(lines[length(lines)], chosen$end_col)
    lines[length(lines)] <- substr(lines[length(lines)], 1L, byte_ec)
  }

  list(
    # `text` is what we parse to obtain getParseData(); offsets map parse
    # coordinates back to the original source file coordinates.
    text = paste(lines, collapse = "\n"),
    srcfile = srcfile,
    line_offset = chosen$start_line - 1L,
    first_col_offset = chosen$start_col - 1L
  )
}

map_generic_indices <- function(parts, child_ids, drop_first_child = FALSE) {
  ids <- child_ids
  if (drop_first_child && length(ids) > 0L) {
    ids <- ids[-1L]
  }

  n <- length(parts)
  present <- which(!vapply(parts, is_missing_arg, logical(1)))

  # Prefer mappings that naturally skip explicit missing arguments.
  if (length(ids) == length(present)) {
    return(list(indices = present, ids = ids))
  }
  if (length(present) >= 1L && length(ids) == length(present) - 1L) {
    return(list(indices = present[-1L], ids = ids))
  }

  if (length(ids) == n) {
    return(list(indices = seq_len(n), ids = ids))
  }
  if (length(ids) == n - 1L && n >= 2L) {
    return(list(indices = seq.int(2L, n), ids = ids))
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

set_element <- function(x, i, value) {
  # Preserve explicit NULL arguments/defaults. `[[<- NULL` deletes an element.
  # `[<-` coerces pairlists to regular lists; restore the original type so that
  # nested `function(...)` formal lists remain pairlists as R requires.
  is_pl <- is.pairlist(x)
  x[i] <- list(value)
  if (is_pl) as.pairlist(x) else x
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
      parts <- set_element(parts, 2L, transform_expr(parts[[2L]], node_id, ctx))
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
    # Do not wrap expressions whose callee is blacklisted (e.g. the ~ formula
    # operator is a SPECIALSXP): wrapping `~cyl == 2` to `{~cyl == 2}` changes
    # the AST structure, breaking functions that build quosures/formulas using
    # `&`/`|` on formula objects and then inspect them with identical().
    slot_expr <- parts[[i]]
    slot_callee_blacklisted <- is.call(slot_expr) &&
      is.symbol(slot_expr[[1L]]) &&
      as.character(slot_expr[[1L]]) %in% ctx$arg_wrap_blacklist
    if (wrap && !is_braced(value) && !slot_callee_blacklisted) {
      value <- wrap_with_transparent_brace(value, node_srcref(child_id, ctx))
    }
    parts <<- set_element(parts, i, value)
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
      if (!is_braced(val) && is.call(val)) {
        val <- wrap_with_transparent_brace(val, node_srcref(cid, ctx))
      }
      fmls <- set_element(fmls, idx, val)
    }

    if (cursor > length(child_ids)) {
      stop("Parse mapping mismatch for function body", call. = FALSE)
    }

    body_id <- child_ids[[cursor]]
    body_expr <- transform_expr(parts[[3]], body_id, ctx)
    # Only wrap the body if it is not already a call (e.g. a symbol or literal).
    # Wrapping a call body in { } changes body(fn)[[1]] from the callee to `{`,
    # breaking meta-programming tests that introspect function body structure.
    # Call-type bodies already have their sub-expressions tracked individually.
    if (!is_braced(body_expr) && !is.call(body_expr)) {
      body_expr <- wrap_with_transparent_brace(body_expr, node_srcref(body_id, ctx))
    }

    if (is.null(fmls)) {
      # `function()` stores formals as NULL. Preserve that slot explicitly,
      # because `parts[[2]] <- NULL` would delete the element.
      out <- as.call(list(as.name("function"), NULL, body_expr))
      attrs <- attributes(expr)
      if (!is.null(attrs)) {
        attributes(out) <- attrs
      }
      return(out)
    }

    parts <- set_element(parts, 2L, fmls)
    parts <- set_element(parts, 3L, body_expr)
    return(rebuild_call(parts, expr))
  }

  if (identical(op, "if")) {
    if (length(child_ids) < 2L) {
      # Some package parse tables do not expose expected `if` child expr nodes.
      # Fall back to structural recursion without additional brace imputation.
      if (length(parts) >= 2L) {
        parts <- set_element(parts, 2L, transform_expr(parts[[2L]], node_id, ctx))
      }
      if (length(parts) >= 3L) {
        parts <- set_element(parts, 3L, transform_expr(parts[[3L]], node_id, ctx))
      }
      if (length(parts) >= 4L) {
        parts <- set_element(parts, 4L, transform_expr(parts[[4L]], node_id, ctx))
      }
      return(rebuild_call(parts, expr))
    }

    cond <- transform_expr(parts[[2L]], child_ids[[1L]], ctx)
    if (!is_braced(cond) && !is_logical_op_call(cond)) {
      cond <- wrap_with_transparent_brace(cond, node_srcref(child_ids[[1L]], ctx))
    }
    parts <- set_element(parts, 2L, cond)

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
    parts <- set_element(parts, 2L, cond)

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
      # Some package parse tables do not expose both operand expr nodes.
      # Fall back to structural recursion without brace imputation.
      if (length(child_ids) == 1L) {
        parts <- set_element(parts, 2L, transform_expr(parts[[2L]], child_ids[[1L]], ctx))
        if (length(parts) >= 3L) {
          parts <- set_element(parts, 3L, transform_expr(parts[[3L]], node_id, ctx))
        }
      } else {
        if (length(parts) >= 2L) {
          parts <- set_element(parts, 2L, transform_expr(parts[[2L]], node_id, ctx))
        }
        if (length(parts) >= 3L) {
          parts <- set_element(parts, 3L, transform_expr(parts[[3L]], node_id, ctx))
        }
      }
      return(rebuild_call(parts, expr))
    }

    recurse_slot(2L, child_ids[[1L]], wrap = TRUE)
    recurse_slot(3L, child_ids[[2L]], wrap = TRUE)
    return(rebuild_call(parts, expr))
  }

  mapping <- map_generic_indices(parts, child_ids)
  if (is.null(mapping)) {
    return(expr)
  }

  # For plain parenthesized calls, wrap argument slots but never the callee.
  # Wrapping is restricted to call expressions to avoid touching simple values
  # (e.g. f(x + 1, 1) -> f({x + 1}, 1)).
  is_blacklisted <- nzchar(op) && op %in% ctx$arg_wrap_blacklist
  wrap_generic_args <- node_has_token(node_id, "'('", ctx) &&
    !identical(op, "(") &&
    !is_blacklisted &&
    isTRUE(ctx$wrap_call_args)

  # Assignment operators: the LHS target (slot 2) must never be brace-wrapped.
  # Wrapping `x[i]` in braces makes it `{x[i]} <- value`, which causes R to
  # look for the nonexistent `{<-` replacement function.
  is_assign_op <- nzchar(op) && op %in% c("<-", "<<-", "=", "->", "->>")

  lhs_slot <- if (op %in% c("->", "->>")) 3L else 2L

  for (k in seq_along(mapping$indices)) {
    i <- mapping$indices[[k]]
    cid <- mapping$ids[[k]]
    if (is_missing_arg(parts[[i]])) {
      next
    }
    is_assign_lhs <- is_assign_op && i == lhs_slot
    # When recursing into an assignment LHS (e.g. `length(slot(x, nm)) <- 0L`),
    # suppress wrap_call_args so that nested call arguments inside the LHS are
    # not brace-wrapped.  Wrapping produces e.g. `length({slot(x, nm)}) <- 0L`
    # which causes R to look for the nonexistent `{<-` replacement function.
    recurse_ctx <- if (is_assign_lhs && isTRUE(ctx$wrap_call_args)) {
      c(ctx[names(ctx) != "wrap_call_args"], list(wrap_call_args = FALSE))
    } else {
      ctx
    }
    value <- transform_expr(parts[[i]], cid, recurse_ctx)
    # Mirror the `recurse_slot` check: do not brace-wrap an argument whose
    # callee is a blacklisted primitive (e.g. `[`, `[[`, `$`, `~`). Wrapping
    # these changes the deparsed form of error messages and breaks packages
    # (like Matrix) that compare test output against saved `.Rout.save` files.
    slot_callee_blacklisted <- is.call(parts[[i]]) &&
      is.symbol(parts[[i]][[1L]]) &&
      as.character(parts[[i]][[1L]]) %in% ctx$arg_wrap_blacklist
    if (wrap_generic_args && i > 1L && !is_assign_lhs && is.call(parts[[i]]) && !is_braced(value) && !is_unquote_call(parts[[i]]) && !is_call_named(parts[[i]], ":=") && !is_unary_arith_call(parts[[i]]) && !slot_callee_blacklisted) {
      value <- wrap_with_transparent_brace(value, node_srcref(cid, ctx))
    }
    parts <- set_element(parts, i, value)
  }

  rebuild_call(parts, expr)
}

#' Impute transparent srcrefs for injected braces in a function AST.
#'
#' Traverses a function's AST and wraps unbraced expressions in targeted
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
#' - function call arguments (optional; call expressions only)
#'
#' @param fn A function.
#' @param wrap_call_args If `TRUE` (default), wrap generic function-call
#'   arguments that are call expressions.
#'
#' @return A function with transformed body/formals and preserved function-level
#'   attributes (including srcref/srcfile metadata when present).
#'
#' @details
#' For functions without srcref metadata, deparse-based fallback is disabled by
#' default. To allow fallback, set
#' `options(impuresrcref.allow_deparse_fallback = TRUE)`.
#'
#' Generic call argument wrapping skips blacklisted callee names. By default the
#' blacklist includes primitive `SPECIALSXP` calls from `builtins()`. Use
#' [set_impute_blacklist()] / [reset_impute_blacklist()] to customize.
#'
#' @examples
#' options(keep.source = TRUE)
#' f <- eval(parse(text = "function(x, y) if (x && y) f() else g()", keep.source = TRUE)[[1]])
#' g <- impute_srcrefs(f)
#' g
#' @export
impute_srcrefs <- function(fn, wrap_call_args = TRUE) {
  if (!is.function(fn)) {
    stop("`fn` must be a function", call. = FALSE)
  }
  if (!is.logical(wrap_call_args) || length(wrap_call_args) != 1L || is.na(wrap_call_args)) {
    stop("`wrap_call_args` must be TRUE or FALSE", call. = FALSE)
  }

  fn_attrs <- attributes(fn)
  src <- source_text_from_srcref(fn)
  if (is.null(src)) {
    return(fn)
  }
  parsed <- tryCatch(
    parse(text = src$text, keep.source = TRUE),
    error = function(e) e
  )
  # Functions defined inside a parenthesized call (e.g. setMethod("f", sig,
  # function(x) if(cond)\n  then\nelse else_expr)) rely on the surrounding `(`
  # to suppress the implicit semicolon that R inserts after the then-arm on its
  # own line. Parsing the extracted text standalone fails with "unexpected else".
  # Retry wrapped in `(...)` to restore that syntactic context.
  paren_wrapped <- FALSE
  if (inherits(parsed, "error")) {
    parsed <- tryCatch(
      parse(text = paste0("(", src$text, ")"), keep.source = TRUE),
      error = function(e) stop(conditionMessage(e), call. = FALSE)
    )
    paren_wrapped <- TRUE
  }

  pd <- utils::getParseData(parsed)

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

  if (paren_wrapped) {
    # The outer node is the `(...)` wrapper; descend to the actual function
    # expression. The prepended `(` shifts all line-1 columns in parse data
    # right by 1, so reduce first_col_offset to compensate.
    inner_rows <- expr_rows[expr_rows$parent == root_id, , drop = FALSE]
    if (nrow(inner_rows) != 1L) {
      stop("Expected exactly one expression inside paren wrapper", call. = FALSE)
    }
    root_id <- inner_rows$id[[1L]]
    src$first_col_offset <- src$first_col_offset - 1L
  }

  # Shared parse context used by the recursive walker.
  ctx <- list(
    pd = pd,
    expr = expr_rows,
    expr_index = split(expr_rows, expr_rows$id),
    srcfile = src$srcfile,
    line_offset = src$line_offset,
    first_col_offset = src$first_col_offset,
    arg_wrap_blacklist = effective_blacklist_names(),
    wrap_call_args = wrap_call_args
  )

  fn_expr <- as.call(list(as.name("function"), formals(fn), body(fn)))
  # Start traversal from the top-level function expression parse node.
  transformed <- transform_expr(fn_expr, root_id, ctx)

  out <- fn
  transformed_formals <- transformed[[2L]]
  if (!is.null(transformed_formals)) {
    formals(out) <- transformed_formals
  }
  body(out) <- transformed[[3L]]

  if (!is.null(fn_attrs)) {
    for (nm in names(fn_attrs)) {
      attr(out, nm) <- fn_attrs[[nm]]
    }
  }

  out
}
