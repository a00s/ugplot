test_that("server dependency helper reports R package names", {
  packages <- ugplot_test_internal("ugplot_server_r_packages")()
  expect_true(all(c("callr", "jsonlite", "plumber") %in% packages))
})

test_that("system dependency commands are available", {
  commands <- ugplot_test_internal("ugplot_server_system_dependency_commands")()
  expect_type(commands, "character")
  expect_gt(length(commands), 0)
})
