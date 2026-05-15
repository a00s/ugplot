test_that("server dependency helper reports R package names", {
  packages <- ugplot_test_internal("ugplot_server_r_packages")()
  expect_true(all(c("callr", "httr", "jsonlite", "plumber") %in% packages))
})

test_that("system dependency commands are available", {
  commands <- ugplot_test_internal("ugplot_server_system_dependency_commands")()
  expect_type(commands, "character")
  expect_gt(length(commands), 0)
})

test_that("model dependency helper reports caret model status", {
  model_dependency_status <- ugplot_test_internal("ugplot_model_dependency_status")
  status <- model_dependency_status(models = "lm")
  expect_true("lm" %in% status$models$model)
  expect_true("models_installed" %in% names(status))
  expect_true("packages_to_install" %in% names(status))
})
