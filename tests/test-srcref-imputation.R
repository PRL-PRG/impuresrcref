#!/usr/bin/env Rscript
options(keep.source = TRUE)

cmd_args <- commandArgs(trailingOnly = TRUE)
full_args <- commandArgs()
script_arg <- grep("^--file=", full_args, value = TRUE)

if (length(script_arg) == 1L) {
  script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
} else {
  script_path <- normalizePath("tests/test-srcref-imputation.R", mustWork = TRUE)
}

test_dir <- dirname(script_path)
pkg_root <- dirname(test_dir)

r_files <- list.files(file.path(pkg_root, "R"), pattern = "\\.[Rr]$", full.names = TRUE)
r_files <- sort(r_files)
for (f in r_files) {
  source(f, local = .GlobalEnv)
}

make_fn <- function(code) {
  eval(parse(text = code, keep.source = TRUE)[[1L]], envir = new.env(parent = baseenv()))
}

cases <- list(
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
  )
)

render_case <- function(case) {
  fn <- make_fn(case$code)
  one <- impute_srcrefs(fn)
  two <- impute_srcrefs(one)
  checks <- assert_transparent_srcref_consistency(one)

  idempotent <- identical(body(one), body(two)) && identical(formals(one), formals(two))
  if (!idempotent) {
    stop(sprintf("Idempotence failed for case %s", case$name), call. = FALSE)
  }

  c(
    sprintf("=== %s ===", case$name),
    sprintf("idempotent=%s", idempotent),
    sprintf("transparent_checks=%d", checks$checked),
    sprintf("transparent_checks_ok=%s", checks$ok),
    collect_srcref_sites(one),
    checks$lines,
    ""
  )
}

actual <- unlist(lapply(cases, render_case), use.names = FALSE)

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
  res <- source_impute_srcrefs(src, envir = env)
  checks_f1 <- assert_transparent_srcref_consistency(env$f1)
  checks_f2 <- assert_transparent_srcref_consistency(env$f2)

  parse_rows <- function(fn) {
    sr <- attr(fn, "srcref", exact = TRUE)
    if (is.null(sr)) {
      return(NA_integer_)
    }
    srcfile <- attr(sr, "srcfile", exact = TRUE)
    if (is.null(srcfile)) {
      return(NA_integer_)
    }
    pd <- getParseData(srcfile)
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
    collect_srcref_sites(env$f1),
    checks_f1$lines,
    collect_srcref_sites(env$f2),
    checks_f2$lines,
    ""
  )
}

actual <- c(actual, render_source_case())

render_package_case <- function() {
  chk <- check_package_parse_data("base", include_internal = FALSE)
  run <- impute_package_srcrefs("base", include_internal = FALSE, prompt_install = FALSE, verbose = FALSE)

  c(
    "=== package-base ===",
    sprintf("has_parse_data=%s", chk$has_parse_data),
    sprintf("patched_count=%d", run$patched_count),
    sprintf("install_command=%s", run$install_command),
    ""
  )
}

actual <- c(actual, render_package_case())

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
      impute_srcrefs(fn)
      NULL
    },
    error = function(e) e
  )

  blocked <- inherits(err, "error") &&
    grepl("impuresrcref.allow_deparse_fallback", conditionMessage(err), fixed = TRUE)

  options(impuresrcref.allow_deparse_fallback = TRUE)
  out <- impute_srcrefs(fn)
  checks <- assert_transparent_srcref_consistency(out)

  c(
    "=== no-srcref-fallback ===",
    sprintf("default_blocked=%s", blocked),
    sprintf("fallback_checks=%d", checks$checked),
    sprintf("fallback_ok=%s", checks$ok),
    collect_srcref_sites(out),
    checks$lines,
    ""
  )
}

actual <- c(actual, render_no_srcref_fallback_case())

expected_path <- file.path(test_dir, "test-srcref-imputation.out")
update_snapshots <- identical(Sys.getenv("UPDATE_SNAPSHOTS", unset = ""), "1") ||
  ("--update" %in% cmd_args)

if (update_snapshots) {
  writeLines(actual, con = expected_path, useBytes = TRUE)
  cat("Updated snapshot:", expected_path, "\n")
  quit(status = 0L)
}

if (!file.exists(expected_path)) {
  stop(
    sprintf(
      "Expected snapshot missing: %s (run with UPDATE_SNAPSHOTS=1 Rscript %s)",
      expected_path,
      script_path
    ),
    call. = FALSE
  )
}

expected <- readLines(expected_path, warn = FALSE)
if (!identical(actual, expected)) {
  out_path <- tempfile("srcref-imputation-", fileext = ".out")
  writeLines(actual, con = out_path, useBytes = TRUE)
  stop(
    sprintf(
      "Snapshot mismatch. Run `UPDATE_SNAPSHOTS=1 Rscript %s`. Actual written to %s",
      script_path,
      out_path
    ),
    call. = FALSE
  )
}

cat("Snapshot OK:", expected_path, "\n")
