test_that("Science Collab background state uses a safe named path", {
  state_dir <- tempfile("ugplot-collab-state-")
  withr::local_envvar(UGPLOT_SCIENCE_COLLAB_STATE_DIR = state_dir)
  state_path <- ugplot_test_internal("ugplot_science_collab_state_path")

  expect_equal(
    state_path("lab machine/2"),
    file.path(state_dir, "lab_machine_2.rds")
  )
})

test_that("Science Collab mission files use its persistent work directory", {
  state_dir <- tempfile("ugplot-collab-state-")
  withr::local_envvar(UGPLOT_SCIENCE_COLLAB_STATE_DIR = state_dir)
  collab_tempfile <- ugplot_test_internal("ugplot_science_collab_tempfile")

  mission_path <- collab_tempfile("mission-", ".rds")

  expect_true(dir.exists(file.path(state_dir, "work")))
  expect_equal(
    normalizePath(dirname(mission_path), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(state_dir, "work"), winslash = "/", mustWork = FALSE)
  )

  unlink(file.path(state_dir, "work"), recursive = TRUE, force = TRUE)
  recreated_path <- collab_tempfile("mission-", ".rds")
  expect_true(dir.exists(file.path(state_dir, "work")))
  expect_equal(
    normalizePath(dirname(recreated_path), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(state_dir, "work"), winslash = "/", mustWork = FALSE)
  )
})

test_that("Science Collab keeps completed results until delivery succeeds", {
  state_dir <- tempfile("ugplot-collab-spool-")
  withr::local_envvar(UGPLOT_SCIENCE_COLLAB_STATE_DIR = state_dir)
  store <- ugplot_test_internal("ugplot_science_collab_store_delivery")
  attempt <- ugplot_test_internal("ugplot_science_collab_attempt_delivery")
  calls <- 0L
  ugplot_test_local_namespace_binding("ugplot_remote_collaboration_complete", function(...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("coordinator offline")
    list(accepted = TRUE)
  })

  path <- store(list(
    server_url = "http://coordinator:8080", task_id = "task-1",
    lease_id = "lease-1", client_id = "client-1", result = list(value = 1)
  ))
  first <- attempt(path)
  expect_false(first$done)
  expect_true(file.exists(path))

  second <- attempt(path)
  expect_true(second$done)
  expect_true(second$accepted)
  expect_false(file.exists(path))
})

test_that("Science Collab refreshes its lease while delivery is pending", {
  state_dir <- tempfile("ugplot-collab-delivery-heartbeat-")
  withr::local_envvar(UGPLOT_SCIENCE_COLLAB_STATE_DIR = state_dir)
  result_path <- tempfile(fileext = ".rds")
  saveRDS(list(kind = "test-result"), result_path)
  alive_checks <- 0L
  delivery_calls <- 0L
  heartbeats <- list()
  process <- new.env(parent = emptyenv())
  process$is_alive <- function() {
    alive_checks <<- alive_checks + 1L
    alive_checks == 1L
  }
  process$get_exit_status <- function() 0L
  process$kill_tree <- function() invisible(TRUE)

  ugplot_test_local_namespace_binding("ugplot_model_dependency_status", function(...) {
    list(models_missing = character(), unknown_models = character())
  })
  ugplot_test_local_namespace_binding("ugplot_science_collab_worker", function(...) {
    list(
      process = process, event_path = tempfile(), result_path = result_path,
      files = result_path, stderr_path = tempfile()
    )
  })
  ugplot_test_local_namespace_binding("ugplot_science_collab_attempt_delivery", function(...) {
    delivery_calls <<- delivery_calls + 1L
    if (delivery_calls == 1L) {
      return(list(done = FALSE, accepted = FALSE, error = "schema update pending"))
    }
    list(done = TRUE, accepted = TRUE, reason = "")
  })
  ugplot_test_local_namespace_binding("ugplot_remote_collaboration_heartbeat", function(...) {
    heartbeats[[length(heartbeats) + 1L]] <<- list(...)
    list(accepted = TRUE)
  })

  claimed <- list(
    task = list(
      task_id = "parent:analyze:TG383", lease_id = "lease-1",
      requirements = list(models = "lm"), offline_delivery = TRUE
    ),
    payload_path = tempfile(fileext = ".rds")
  )
  saveRDS(list(), claimed$payload_path)
  run <- ugplot_test_internal("ugplot_science_collab_run_mission")

  expect_message(
    accepted <- run(
      claimed, "http://coordinator:8080", "client-1", 1L,
      spool_dir = file.path(state_dir, "spool"), poll_seconds = 1
    ),
    "waiting to deliver: schema update pending",
    fixed = TRUE
  )

  expect_true(accepted)
  expect_length(heartbeats, 2L)
  delivery_heartbeat <- utils::tail(heartbeats, 1L)[[1]]
  expect_equal(delivery_heartbeat$telemetry$progress, 1)
  expect_match(delivery_heartbeat$telemetry$message, "waiting to deliver", fixed = TRUE)
})

test_that("Science Collab computation survives a coordinator heartbeat outage", {
  state_dir <- tempfile("ugplot-collab-offline-run-")
  withr::local_envvar(UGPLOT_SCIENCE_COLLAB_STATE_DIR = state_dir)
  result_path <- tempfile(fileext = ".rds")
  saveRDS(list(kind = "test-result"), result_path)
  alive_checks <- 0L
  process <- new.env(parent = emptyenv())
  process$is_alive <- function() {
    alive_checks <<- alive_checks + 1L
    alive_checks == 1L
  }
  process$get_exit_status <- function() 0L
  process$kill_tree <- function() invisible(TRUE)

  ugplot_test_local_namespace_binding("ugplot_model_dependency_status", function(...) {
    list(models_missing = character(), unknown_models = character())
  })
  ugplot_test_local_namespace_binding("ugplot_science_collab_worker", function(...) {
    list(
      process = process, event_path = tempfile(), result_path = result_path,
      files = result_path, stderr_path = tempfile()
    )
  })
  ugplot_test_local_namespace_binding("ugplot_remote_collaboration_heartbeat", function(...) {
    stop("planned coordinator maintenance")
  })
  ugplot_test_local_namespace_binding("ugplot_remote_collaboration_complete", function(...) {
    list(accepted = TRUE)
  })

  claimed <- list(
    task = list(
      task_id = "parent:analyze:TG1", lease_id = "lease-1",
      requirements = list(models = "lm"), offline_delivery = TRUE
    ),
    payload_path = tempfile(fileext = ".rds")
  )
  saveRDS(list(), claimed$payload_path)
  run <- ugplot_test_internal("ugplot_science_collab_run_mission")
  expect_message(
    accepted <- run(
      claimed, "http://coordinator:8080", "client-1", 1L,
      spool_dir = file.path(state_dir, "spool"), poll_seconds = 1
    ),
    "computation continues offline"
  )
  expect_true(accepted)
  expect_length(list.files(file.path(state_dir, "spool")), 0L)
})

test_that("Science Collab worker does not serialize library paths before launch", {
  worker_source <- paste(
    deparse(body(ugplot_test_internal("ugplot_science_collab_worker"))),
    collapse = "\n"
  )

  # The former saveRDS(.libPaths(), ...) call was the only gzfile() user on
  # the parent-side path immediately after "starting computation". Library
  # paths are plain process arguments now, so launching a mission cannot fail
  # at that serialization step.
  expect_false(grepl("saveRDS\\(\\.libPaths", worker_source))
  expect_false(grepl("ugplot-collab-libs-", worker_source, fixed = TRUE))
  expect_match(worker_source, ".libPaths(args[-seq_len(4L)])", fixed = TRUE)
})

test_that("Science Collab background client can start, report status, and stop", {
  skip_if_not_installed("processx")
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
  expect_true(dir.exists(started$runtime_dir))
  expect_true(file.exists(file.path(started$runtime_dir, "launcher.R")))
  expect_true(file.exists(file.path(started$runtime_dir, "config.rds")))
  expect_false(grepl("callr-res-", started$runtime_dir, fixed = TRUE))
  expect_true(ugplot_test_internal("ugplot_process_alive")(started$pid))

  status <- capture.output(current <- ugPlotScienceCollabStatus("test-client"))
  expect_true(current$running)
  expect_match(paste(status, collapse = "\n"), "Test scientist", fixed = TRUE)

  expect_true(ugPlotScienceCollabStop("test-client"))
  stopped_output <- capture.output(stopped <- ugPlotScienceCollabStatus("test-client"))
  expect_false(stopped$running)
  expect_match(paste(stopped_output, collapse = "\n"), "not running", ignore.case = TRUE)
})
