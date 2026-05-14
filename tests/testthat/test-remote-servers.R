test_that("remote server config stores CPU limits and migrates old records", {
  local_env <- new.env(parent = globalenv())
  local_env$`%||%` <- function(lhs, rhs) {
    if (is.null(lhs)) rhs else lhs
  }
  local_env$ugplot_ensure_dir <- function(path) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    invisible(path)
  }
  config_dir <- tempfile("ugplot-config-")
  local_env$ugplot_remote_servers_path <- function() {
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
    file.path(config_dir, "remote_servers.rds")
  }
  remote_servers_path <- file.path("R", "remote_servers.R")
  if (!file.exists(remote_servers_path)) {
    remote_servers_path <- file.path("..", "..", "R", "remote_servers.R")
  }
  sys.source(remote_servers_path, envir = local_env)
  local_env$ugplot_remote_servers_path <- function() {
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
    file.path(config_dir, "remote_servers.rds")
  }

  old_servers <- data.frame(
    name = "Old",
    url = "http://example.test:8080",
    token = "",
    stringsAsFactors = FALSE
  )
  saveRDS(old_servers, local_env$ugplot_remote_servers_path())
  migrated <- local_env$ugplot_read_remote_servers()

  expect_true("cpu_limit" %in% names(migrated))
  expect_true(migrated$cpu_limit[[1]] >= 1L)

  saved <- local_env$ugplot_upsert_remote_server(
    name = "Remote",
    url = "http://remote.test:8080",
    token = "secret",
    cpu_limit = 7
  )

  remote <- saved[saved$name == "Remote", , drop = FALSE]
  expect_equal(remote$cpu_limit, 7L)
})
