options(keep.source = TRUE)

make_fn <- function(code) {
  eval(parse(text = code, keep.source = TRUE)[[1L]], envir = new.env(parent = baseenv()))
}

srcref_cases <- list(
  list(
    name = "if-logical",
    code = "function(x, y) { if (x && y) f() else g() }"
  ),
  list(
    name = "loops",
    code = "function(x, y) { for (i in 1:3) if (x || y) a() else b(); while (x & y) c(); repeat d() }"
  ),
  list(
    name = "switch",
    code = "function(k) { switch(k, aa = f(), g()) }"
  ),
  list(
    name = "defaults-and-unbraced-body",
    code = "function(a = x && y, b = if (p) u else v) if (a) m() else n()"
  ),
  list(
    name = "already-braced",
    code = "function(x, y) { if ({x} && {y}) {f()} else {g()} }"
  ),
  list(
    name = "null-args-preserved",
    code = "function() { f(NULL, if (x) y else z, NULL, if (p) q else r, NULL) }"
  ),
  list(
    name = "generic-call-args",
    code = "function(x, y) g(x+1, f(y+1))"
  ),
  list(
    name = "missing-call-arg",
    code = "function() quote(expr = )"
  )
)

render_case <- function(case) {
  fn <- make_fn(case$code)
  one <- imputesrcref::impute_srcrefs(fn)
  two <- imputesrcref::impute_srcrefs(one)
  checks <- imputesrcref:::assert_transparent_srcref_consistency(one)

  idempotent <- identical(body(one), body(two)) && identical(formals(one), formals(two))
  if (!idempotent) {
    stop(sprintf("Idempotence failed for case %s", case$name), call. = FALSE)
  }

  c(
    sprintf("=== %s ===", case$name),
    sprintf("idempotent=%s", idempotent),
    sprintf("transparent_checks=%d", checks$checked),
    sprintf("transparent_checks_ok=%s", checks$ok),
    imputesrcref:::collect_srcref_sites(one),
    checks$lines,
    ""
  )
}

render_source_case <- function() {
  src <- tempfile("source-impute-", fileext = ".R")
  writeLines(
    c(
      "f1 <- function(x, y) if (x && y) f() else g()",
      "f2 <- function(a = p || q) while (a) h()",
      "not_a_function <- 42"
    ),
    con = src,
    useBytes = TRUE
  )

  env <- new.env(parent = baseenv())
  res <- imputesrcref::source_impute_srcrefs(src, envir = env)
  checks_f1 <- imputesrcref:::assert_transparent_srcref_consistency(env$f1)
  checks_f2 <- imputesrcref:::assert_transparent_srcref_consistency(env$f2)

  parse_rows <- function(fn) {
    sr <- attr(fn, "srcref", exact = TRUE)
    if (is.null(sr)) {
      return(NA_integer_)
    }
    srcfile <- attr(sr, "srcfile", exact = TRUE)
    if (is.null(srcfile)) {
      return(NA_integer_)
    }
    pd <- utils::getParseData(srcfile)
    if (is.null(pd)) {
      return(NA_integer_)
    }
    nrow(pd)
  }

  c(
    "=== source-file ===",
    sprintf("count=%d", res$count),
    sprintf("functions=%s", paste(res$functions, collapse = ",")),
    sprintf("f1_parse_rows=%d", parse_rows(env$f1)),
    sprintf("f2_parse_rows=%d", parse_rows(env$f2)),
    sprintf("f1_transparent_checks=%d", checks_f1$checked),
    sprintf("f2_transparent_checks=%d", checks_f2$checked),
    sprintf("f1_transparent_ok=%s", checks_f1$ok),
    sprintf("f2_transparent_ok=%s", checks_f2$ok),
    imputesrcref:::collect_srcref_sites(env$f1),
    checks_f1$lines,
    imputesrcref:::collect_srcref_sites(env$f2),
    checks_f2$lines,
    ""
  )
}

render_package_like_srcref_case <- function() {
  txt <- paste(
    c(
      "# preamble",
      "#line 100 \"pkg/file.R\"",
      "f <- function(x, y) if (x && y) f() else g()"
    ),
    collapse = "\n"
  )

  env <- new.env(parent = baseenv())
  expr <- parse(text = txt, keep.source = TRUE)
  eval(expr, envir = env)

  fn <- env$f
  sr <- attr(fn, "srcref", exact = TRUE)
  out <- imputesrcref::impute_srcrefs(fn)
  checks <- imputesrcref:::assert_transparent_srcref_consistency(out)

  c(
    "=== package-like-srcref ===",
    sprintf("sr_slots_1_3=%d-%d", sr[[1L]], sr[[3L]]),
    sprintf("sr_slots_7_8=%d-%d", sr[[7L]], sr[[8L]]),
    sprintf("transparent_checks=%d", checks$checked),
    sprintf("transparent_checks_ok=%s", checks$ok),
    imputesrcref:::collect_srcref_sites(out),
    checks$lines,
    ""
  )
}

render_zero_formals_case <- function() {
  fn <- make_fn("function() if (TRUE) f() else g()")
  one <- imputesrcref::impute_srcrefs(fn)
  two <- imputesrcref::impute_srcrefs(one)
  checks <- imputesrcref:::assert_transparent_srcref_consistency(one)
  idempotent <- identical(body(one), body(two)) && identical(formals(one), formals(two))

  c(
    "=== zero-formals ===",
    sprintf("formals_is_null=%s", is.null(formals(one))),
    sprintf("idempotent=%s", idempotent),
    sprintf("transparent_checks=%d", checks$checked),
    sprintf("transparent_checks_ok=%s", checks$ok),
    imputesrcref:::collect_srcref_sites(one),
    checks$lines,
    ""
  )
}

render_package_case <- function() {
  run <- imputesrcref::impute_package_srcrefs("base", include_internal = FALSE, verbose = FALSE)

  c(
    "=== package-base ===",
    sprintf("patched_count=%d", run$patched_count),
    sprintf("install_command=%s", run$install_command),
    ""
  )
}

render_no_srcref_fallback_case <- function() {
  fn <- eval(
    parse(text = "function(x, y) if (x && y) f() else g()", keep.source = FALSE)[[1L]],
    envir = new.env(parent = baseenv())
  )

  old_opt <- getOption("impuresrcref.allow_deparse_fallback")
  on.exit(options(impuresrcref.allow_deparse_fallback = old_opt), add = TRUE)
  options(impuresrcref.allow_deparse_fallback = FALSE)
  err <- tryCatch(
    {
      imputesrcref::impute_srcrefs(fn)
      NULL
    },
    error = function(e) e
  )

  blocked <- inherits(err, "error") &&
    grepl("impuresrcref.allow_deparse_fallback", conditionMessage(err), fixed = TRUE)

  options(impuresrcref.allow_deparse_fallback = TRUE)
  out <- imputesrcref::impute_srcrefs(fn)
  checks <- imputesrcref:::assert_transparent_srcref_consistency(out)

  c(
    "=== no-srcref-fallback ===",
    sprintf("default_blocked=%s", blocked),
    sprintf("fallback_checks=%d", checks$checked),
    sprintf("fallback_ok=%s", checks$ok),
    imputesrcref:::collect_srcref_sites(out),
    checks$lines,
    ""
  )
}

render_srcref_snapshot <- function() {
  actual <- unlist(lapply(srcref_cases, render_case), use.names = FALSE)
  actual <- c(actual, render_source_case())
  actual <- c(actual, render_package_like_srcref_case())
  actual <- c(actual, render_zero_formals_case())
  actual <- c(actual, render_package_case())
  actual <- c(actual, render_no_srcref_fallback_case())
  actual
}
