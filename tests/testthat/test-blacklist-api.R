test_that("SPECIALSXP calls are blacklisted by default", {
  old <- getOption("imputesrcref.wrap_arg_blacklist")
  on.exit(options(imputesrcref.wrap_arg_blacklist = old), add = TRUE)
  imputesrcref::reset_impute_blacklist()

  expect_true("quote" %in% imputesrcref::get_impute_blacklist(include_default = TRUE))
  expect_identical(imputesrcref::get_impute_blacklist(include_default = FALSE), character())

  fn <- make_fn("function() quote(x + 1)")
  out <- imputesrcref::impute_srcrefs(fn)
  sites <- imputesrcref:::collect_srcref_sites(out)

  expect_false(any(grepl("quote\\(\\{ x \\+ 1 \\}\\)", sites)))
  expect_true(any(grepl("quote\\(x \\+ 1\\)", sites)))
})

test_that("user blacklist entries are applied and resettable", {
  old <- getOption("imputesrcref.wrap_arg_blacklist")
  on.exit(options(imputesrcref.wrap_arg_blacklist = old), add = TRUE)
  imputesrcref::reset_impute_blacklist()

  fn <- make_fn("function(x) g(x + 1)")
  wrapped <- imputesrcref::impute_srcrefs(fn)
  wrapped_sites <- imputesrcref:::collect_srcref_sites(wrapped)
  expect_true(any(grepl("g\\(\\{ x \\+ 1 \\}\\)", wrapped_sites)))

  imputesrcref::set_impute_blacklist("g")
  expect_true("g" %in% imputesrcref::get_impute_blacklist(include_default = FALSE))

  skipped <- imputesrcref::impute_srcrefs(fn)
  skipped_sites <- imputesrcref:::collect_srcref_sites(skipped)
  expect_false(any(grepl("g\\(\\{ x \\+ 1 \\}\\)", skipped_sites)))
  expect_true(any(grepl("g\\(x \\+ 1\\)", skipped_sites)))

  imputesrcref::reset_impute_blacklist()
  expect_identical(imputesrcref::get_impute_blacklist(include_default = FALSE), character())
})
