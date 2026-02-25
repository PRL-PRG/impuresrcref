test_that("ggplot2 full package srcref imputation is stable", {
  skip_if_not(
    identical(Sys.getenv("FULL_TEST", unset = ""), "1"),
    "Set FULL_TEST=1 to run."
  )
  skip_if_not_installed("ggplot2")

  res <- imputesrcref::impute_package_srcrefs("ggplot2", include_internal = TRUE, verbose = FALSE)

  fn_names <- res$fn_names
  failed <- sum(!is.na(res$failed))
  patched <- res$patched_count
  non_missing <- sum(!is.na(res$failed) & !grepl("missing parse data", res$failed, fixed = TRUE))

  expect_gt(length(fn_names), 0L)
  expect_gt(patched, 0L)
  expect_lt(failed, length(fn_names))
  expect_equal(non_missing, 0L)
})
