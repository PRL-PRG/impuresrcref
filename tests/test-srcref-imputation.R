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

  idempotent <- identical(body(one), body(two)) && identical(formals(one), formals(two))
  if (!idempotent) {
    stop(sprintf("Idempotence failed for case %s", case$name), call. = FALSE)
  }

  c(
    sprintf("=== %s ===", case$name),
    sprintf("idempotent=%s", idempotent),
    collect_srcref_sites(one),
    ""
  )
}

actual <- unlist(lapply(cases, render_case), use.names = FALSE)

expected_path <- file.path(test_dir, "test-srcref-imputation.out")

if ("--update" %in% cmd_args) {
  writeLines(actual, con = expected_path, useBytes = TRUE)
  cat("Updated snapshot:", expected_path, "\n")
  quit(status = 0L)
}

if (!file.exists(expected_path)) {
  stop(
    sprintf("Expected snapshot missing: %s (run with --update)", expected_path),
    call. = FALSE
  )
}

expected <- readLines(expected_path, warn = FALSE)
if (!identical(actual, expected)) {
  out_path <- tempfile("srcref-imputation-", fileext = ".out")
  writeLines(actual, con = out_path, useBytes = TRUE)
  stop(
    sprintf(
      "Snapshot mismatch. Run `Rscript %s --update`. Actual written to %s",
      script_path,
      out_path
    ),
    call. = FALSE
  )
}

cat("Snapshot OK:", expected_path, "\n")
