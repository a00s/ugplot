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
  expect_true("cpu_max" %in% names(migrated))
  expect_true(migrated$cpu_limit[[1]] >= 1L)
  expect_true(migrated$cpu_max[[1]] >= migrated$cpu_limit[[1]])

  saved <- upsert_remote_server(
    name = "Remote",
    url = "http://remote.test:8080",
    token = "secret",
    cpu_limit = 7,
    cpu_max = 12
  )

  remote <- saved[saved$name == "Remote", , drop = FALSE]
  expect_equal(remote$cpu_limit, 7L)
  expect_equal(remote$cpu_max, 12L)
})

test_that("build versions compare date and suffix versions", {
  if (!exists("ugplot_compare_build_versions", inherits = TRUE)) {
    version_file <- c(file.path("R", "00_version.R"), file.path("..", "..", "R", "00_version.R"))
    version_file <- version_file[file.exists(version_file)][[1]]
    source(version_file, local = globalenv())
  }
  compare_versions <- get("ugplot_compare_build_versions", inherits = TRUE)
  mismatch_message <- get("ugplot_version_mismatch_message", inherits = TRUE)

  expect_equal(compare_versions("20260517", "20260517"), 0L)
  expect_equal(compare_versions("20260517.1", "20260517"), 1L)
  expect_equal(compare_versions("20260517", "20260517.2"), -1L)
  expect_equal(compare_versions("20260518", "20260517.9"), 1L)
  expect_true(is.na(compare_versions("20260517", "")))

  expect_match(
    mismatch_message("20260517.1", "20260517"),
    "Update the remote server to 20260517.1",
    fixed = TRUE
  )
  expect_match(
    mismatch_message("20260517", "20260517.2"),
    "Update this interface to 20260517.2",
    fixed = TRUE
  )
})

test_that("lightweight remote monitor summarizes distributed screening", {
  summarize <- ugplot_test_internal("ugplot_remote_distributed_summary")
  structured <- summarize(list(
    message = "Distributed screening: 3/2148 group(s); active Fy2:TG2, Fy3:TG3",
    distributed_state = list(
      completed = 3L, total = 2148L, active = 2L,
      active_groups = c("Fy2:TG2", "Fy3:TG3")
    )
  ))
  expect_equal(structured$completed, 3)
  expect_equal(structured$total, 2148)
  expect_equal(structured$processing, 2)
  expect_equal(structured$pending, 2143)
  expect_equal(structured$active_groups, c("Fy2:TG2", "Fy3:TG3"))

  legacy <- summarize(list(
    message = "Distributed screening: 9/771 group(s); active Fy3:TG5, Fy2:TG17"
  ))
  expect_equal(legacy$completed, 9)
  expect_equal(legacy$total, 771)
  expect_equal(legacy$processing, 2)
  expect_equal(legacy$active_groups, c("Fy3:TG5", "Fy2:TG17"))

  complete <- summarize(list(
    message = "Distributed complete analysis: 4/2148 group(s); active Fy2:TG7"
  ))
  expect_equal(complete$completed, 4)
  expect_equal(complete$total, 2148)
  expect_equal(complete$processing, 1)
  expect_equal(complete$active_groups, "Fy2:TG7")
})
