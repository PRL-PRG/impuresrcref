test_that("srcref imputation snapshot matches expected output", {
  actual <- render_srcref_snapshot()
  expected_path <- testthat::test_path("test-srcref-imputation.out")

  if (identical(Sys.getenv("UPDATE_SNAPSHOTS", unset = ""), "1")) {
    writeLines(actual, con = expected_path, useBytes = TRUE)
    skip(sprintf("Updated snapshot: %s", expected_path))
  }

  expect_true(file.exists(expected_path))
  expected <- readLines(expected_path, warn = FALSE)
  expect_identical(actual, expected)
})
