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

test_that("model dependency status requires packages to be loadable", {
  ugplot_test_local_namespace_binding(
    "ugplot_r_package_available",
    function(package, installed_packages = NULL) !identical(package, "glmnet")
  )
  status <- ugplot_test_internal("ugplot_model_dependency_status")(models = "glmnet")

  expect_false(status$models$installed)
  expect_equal(status$models$missing_packages, "glmnet")
  expect_true("glmnet" %in% status$packages_to_install)
})

test_that("package installation gets a noninteractive CRAN fallback", {
  repositories <- ugplot_test_internal("ugplot_install_repositories")

  expect_equal(
    unname(repositories(c(CRAN = "@CRAN@"))),
    "https://cloud.r-project.org"
  )
  expect_equal(
    repositories(c(CRAN = "https://cran.rstudio.com")),
    c(CRAN = "https://cran.rstudio.com")
  )
})
