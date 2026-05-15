test_that("remote server config stores CPU limits and migrates old records", {
  config_dir <- tempfile("ugplot-config-")
  ugplot_test_local_namespace_binding("ugplot_remote_servers_path", function() {
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
    file.path(config_dir, "remote_servers.rds")
  })

  read_remote_servers <- ugplot_test_internal("ugplot_read_remote_servers")
  upsert_remote_server <- ugplot_test_internal("ugplot_upsert_remote_server")
  remote_servers_path <- ugplot_test_internal("ugplot_remote_servers_path")

  old_servers <- data.frame(
    name = "Old",
    url = "http://example.test:8080",
    token = "",
    stringsAsFactors = FALSE
  )
  saveRDS(old_servers, remote_servers_path())
  migrated <- read_remote_servers()

  expect_true("cpu_limit" %in% names(migrated))
  expect_true(migrated$cpu_limit[[1]] >= 1L)

  saved <- upsert_remote_server(
    name = "Remote",
    url = "http://remote.test:8080",
    token = "secret",
    cpu_limit = 7
  )

  remote <- saved[saved$name == "Remote", , drop = FALSE]
  expect_equal(remote$cpu_limit, 7L)
})
