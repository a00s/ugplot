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
  local_env <- new.env(parent = globalenv())
  server_deps_path <- file.path("R", "server_deps.R")
  if (!file.exists(server_deps_path)) {
    server_deps_path <- file.path("..", "..", "R", "server_deps.R")
  }
  sys.source(server_deps_path, envir = local_env)
  status <- local_env$ugplot_model_dependency_status(models = "lm")
  expect_true("lm" %in% status$models$model)
  expect_true("models_installed" %in% names(status))
  expect_true("packages_to_install" %in% names(status))
})
