test_that("complete-case scan respects the minimum retained sample constraint", {
  compute_scan <- ugplot_test_internal("compute_exhaustive_threshold_scan")

  predictors <- data.frame(
    c1 = c(NA, 2, 3, 4),
    c2 = c(1, NA, 3, 4),
    c3 = c(1, NA, 3, 4),
    check.names = FALSE
  )

  loose <- compute_scan(
    predictors,
    missing_definition = "na",
    min_rows_retained = 0.5,
    mode = "complete_case"
  )
  strict <- compute_scan(
    predictors,
    missing_definition = "na",
    min_rows_retained = 0.75,
    mode = "complete_case"
  )

  expect_gt(nrow(loose), 0)
  expect_gt(nrow(strict), 0)
  expect_equal(loose$missing_cells_after[[1]], 0)
  expect_equal(strict$missing_cells_after[[1]], 0)
  expect_equal(loose$n_cols_after[[1]], 3)
  expect_equal(loose$n_rows_after[[1]], 2)
  expect_equal(strict$n_cols_after[[1]], 2)
  expect_equal(strict$n_rows_after[[1]], 3)
  expect_equal(strict$scan_order[[1]], "rows_first")
  expect_true(strict$rows_retained[[1]] >= 0.75)
})

test_that("missing mask treats NA-like strings case-insensitively", {
  build_mask <- ugplot_test_internal("build_missing_mask")

  mask <- build_mask(
    data.frame(
      value = c(NA, "na", "Na", "NA", " nA ", "ok", ""),
      stringsAsFactors = FALSE
    ),
    missing_definition = c("empty", "na")
  )

  expect_equal(as.vector(mask[, "value"]), c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))
})
