#!/usr/bin/env Rscript
options(keep.source = TRUE)

if (!identical(Sys.getenv("FULL_TEST", unset = ""), "1")) {
  cat("Skipping ggplot2 full package test (set FULL_TEST=1 to run)\n")
  quit(status = 0L)
}

full_args <- commandArgs()
script_arg <- grep("^--file=", full_args, value = TRUE)
if (length(script_arg) == 1L) {
  script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
} else if (file.exists("tests/test-package-srcref-imputation.R")) {
  script_path <- normalizePath("tests/test-package-srcref-imputation.R", mustWork = TRUE)
} else if (file.exists("test-package-srcref-imputation.R")) {
  script_path <- normalizePath("test-package-srcref-imputation.R", mustWork = TRUE)
} else {
  stop("Could not locate test-package-srcref-imputation.R", call. = FALSE)
}

pkg_root <- dirname(dirname(script_path))
r_files <- sort(list.files(file.path(pkg_root, "R"), pattern = "\\.[Rr]$", full.names = TRUE))
if (length(r_files) > 0L) {
  for (f in r_files) {
    source(f, local = .GlobalEnv)
  }
} else if ("impuresrcref" %in% loadedNamespaces() || requireNamespace("impuresrcref", quietly = TRUE)) {
  impute_package_srcrefs <- impuresrcref::impute_package_srcrefs
} else {
  stop("Could not load impuresrcref functions for full test", call. = FALSE)
}

cat("Running ggplot2 full package srcref imputation test\n")
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("FULL_TEST=1 requires ggplot2 to be installed with parse data", call. = FALSE)
}

res <- impute_package_srcrefs("ggplot2", include_internal = TRUE, verbose = FALSE)
fn_names <- res$fn_names
failed <- sum(!is.na(res$failed))
patched <- res$patched_count
non_missing <- sum(!is.na(res$failed) & !grepl("missing parse data", res$failed, fixed = TRUE))

cat(sprintf("ggplot2 functions=%d patched=%d failed=%d non_missing=%d\n", length(fn_names), patched, failed, non_missing))

if (length(fn_names) == 0L) {
  stop("No namespace functions discovered in ggplot2", call. = FALSE)
}
if (patched <= 0L) {
  stop("Expected at least one patched ggplot2 function", call. = FALSE)
}
if (failed == length(fn_names)) {
  stop("All ggplot2 functions failed srcref imputation", call. = FALSE)
}
if (non_missing > 0L) {
  stop("Found non-missing srcref imputation failures", call. = FALSE)
}

cat("ggplot2 full package srcref imputation test OK\n")
