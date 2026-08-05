test_that("Science Collab background state uses a safe named path", {
  state_dir <- tempfile("ugplot-collab-state-")
  withr::local_envvar(UGPLOT_SCIENCE_COLLAB_STATE_DIR = state_dir)
  state_path <- ugplot_test_internal("ugplot_science_collab_state_path")

  expect_equal(
    state_path("lab machine/2"),
    file.path(state_dir, "lab_machine_2.rds")
  )
})

test_that("Science Collab background client can start, report status, and stop", {
  skip_if_not_installed("callr")
  state_dir <- tempfile("ugplot-collab-control-")
  withr::local_envvar(UGPLOT_SCIENCE_COLLAB_STATE_DIR = state_dir)

  started <- ugPlotScienceCollabStart(
    coordinator = "127.0.0.1:1",
    scientist_name = "Test scientist",
    cpu_limit = 1,
    install_model_deps = FALSE,
    install_client_deps = FALSE,
    poll_seconds = 1,
    name = "test-client"
  )
  on.exit({
    if (ugplot_test_internal("ugplot_process_alive")(started$pid)) {
      try(ugplot_test_internal("ugplot_terminate_process")(started$pid), silent = TRUE)
    }
  }, add = TRUE)

  expect_true(started$running)
  expect_true(file.exists(started$log_file))
  expect_true(ugplot_test_internal("ugplot_process_alive")(started$pid))

  status <- capture.output(current <- ugPlotScienceCollabStatus("test-client"))
  expect_true(current$running)
  expect_match(paste(status, collapse = "\n"), "Test scientist", fixed = TRUE)

  expect_true(ugPlotScienceCollabStop("test-client"))
  stopped_output <- capture.output(stopped <- ugPlotScienceCollabStatus("test-client"))
  expect_false(stopped$running)
  expect_match(paste(stopped_output, collapse = "\n"), "not running", ignore.case = TRUE)
})
