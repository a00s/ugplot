test_that("job store creates and lists jobs", {
  jobs_dir <- tempfile("ugplot-jobs-")
  dataset <- data.frame(x = 1:3, y = c("a", "b", "c"))

  create_job <- ugplot_test_internal("ugplot_create_job")
  list_jobs <- ugplot_test_internal("ugplot_list_jobs")

  status <- create_job(dataset, config = list(runner = "ugplot_run_placeholder_job"), jobs_dir = jobs_dir)

  expect_equal(status$state, "queued")
  expect_true(dir.exists(file.path(jobs_dir, status$id)))
  expect_true(file.exists(file.path(jobs_dir, status$id, "dataset.rds")))

  listed <- list_jobs(jobs_dir)
  expect_equal(nrow(listed), 1)
  expect_equal(listed$id, status$id)
})

test_that("stopped GEO jobs can repair distributed workers atomically", {
  jobs_dir <- tempfile("ugplot-worker-repair-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_status <- ugplot_test_internal("ugplot_write_job_status")
  replace_workers <- ugplot_test_internal("ugplot_replace_job_distributed_workers")
  status <- create_job(
    data.frame(request = 1),
    config = list(
      runner = "ugplot_run_geo_pipeline_job",
      type = "geo",
      distributed_workers = list(list(
        name = "Local 8080", url = "http://127.0.0.1:8080", token = "old",
        cpu_limit = 6L, cpu_max = 8L
      ))
    ),
    jobs_dir = jobs_dir,
    type = "geo"
  )
  status$state <- "stopped"
  write_status(status$id, status, jobs_dir)

  repaired <- replace_workers(status$id, list(
    list(name = "Fy2", url = "http://fy2:8080", token = "shared", cpu_limit = 6L, cpu_max = 8L),
    list(name = "Fy3", url = "http://fy3:8080", token = "shared", cpu_limit = 5L, cpu_max = 6L)
  ), jobs_dir)
  config <- readRDS(file.path(jobs_dir, status$id, "config.rds"))
  backup <- readRDS(file.path(jobs_dir, status$id, "config-before-worker-repair.rds"))

  expect_equal(vapply(config$distributed_workers, `[[`, character(1), "name"), c("Fy2", "Fy3"))
  expect_equal(backup$distributed_workers[[1]]$name, "Local 8080")
  expect_false("token" %in% names(repaired$workers[[1]]))
  expect_error(
    replace_workers(status$id, list(
      list(name = "Fy2", url = "not-a-url", token = "shared")
    ), jobs_dir),
    "HTTP"
  )
})

test_that("active jobs reject distributed worker replacement", {
  jobs_dir <- tempfile("ugplot-active-worker-repair-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  replace_workers <- ugplot_test_internal("ugplot_replace_job_distributed_workers")
  status <- create_job(
    data.frame(request = 1),
    config = list(runner = "ugplot_run_geo_pipeline_job", type = "geo"),
    jobs_dir = jobs_dir,
    type = "geo"
  )
  expect_error(
    replace_workers(status$id, list(
      list(name = "Fy2", url = "http://fy2:8080", token = "shared")
    ), jobs_dir),
    "Drain or stop"
  )
})

test_that("focused job monitor returns a compact single-job snapshot", {
  jobs_dir <- tempfile("ugplot-monitor-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  append_resources <- ugplot_test_internal("ugplot_append_job_resources")
  monitor_snapshot <- ugplot_test_internal("ugplot_job_monitor_snapshot")

  status <- create_job(
    data.frame(x = 1:3),
    config = list(runner = "ugplot_run_placeholder_job"),
    jobs_dir = jobs_dir
  )
  append_resources(
    status$id,
    data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      alive = TRUE,
      process_cpu_pct = 42,
      process_rss_mb = 128,
      current_message = "Working on selected job",
      stringsAsFactors = FALSE
    ),
    jobs_dir = jobs_dir
  )

  snapshot <- monitor_snapshot(status$id, jobs_dir, include_groups = FALSE, resource_lines = 1L)

  expect_equal(snapshot$protocol_version, 1L)
  expect_equal(snapshot$status$id, status$id)
  expect_equal(nrow(snapshot$resources), 1L)
  expect_equal(snapshot$resources$current_message, "Working on selected job")
  expect_equal(nrow(snapshot$group_activity$groups), 0L)
})

test_that("focused job monitor does not perform a full status refresh", {
  jobs_dir <- tempfile("ugplot-monitor-light-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  monitor_snapshot <- ugplot_test_internal("ugplot_job_monitor_snapshot")

  status <- create_job(
    data.frame(x = 1:3),
    config = list(runner = "ugplot_run_placeholder_job"),
    jobs_dir = jobs_dir
  )
  ugplot_test_local_namespace_binding("ugplot_read_job_status", function(...) {
    stop("full status refresh should not be used")
  })

  snapshot <- monitor_snapshot(status$id, jobs_dir, include_groups = FALSE)
  expect_equal(snapshot$status$id, status$id)
})

test_that("lightweight job listing does not refresh or reopen job configuration", {
  jobs_dir <- tempfile("ugplot-list-light-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  list_jobs <- ugplot_test_internal("ugplot_list_jobs")

  status <- create_job(
    data.frame(x = 1:3),
    config = list(target = "age", models = c("lm", "glm")),
    jobs_dir = jobs_dir
  )
  ugplot_test_local_namespace_binding("ugplot_refresh_job_status", function(...) {
    stop("full status refresh should not be used")
  })

  listed <- list_jobs(jobs_dir, lightweight = TRUE)
  expect_equal(listed$id, status$id)
  expect_equal(listed$target, "age")
  expect_equal(listed$models, "lm, glm")

  persisted <- readRDS(file.path(jobs_dir, status$id, "status.rds"))
  persisted$state <- "failed"
  persisted$resumable <- FALSE
  saveRDS(persisted, file.path(jobs_dir, status$id, "status.rds"))
  listed_after_restart <- list_jobs(jobs_dir, lightweight = TRUE)
  expect_true(listed_after_restart$resumable)
})

test_that("Windows tasklist receives its PID filter as one quoted argument", {
  tasklist_args <- ugplot_test_internal("ugplot_windows_tasklist_args")(12345)

  expect_length(tasklist_args, 3L)
  expect_equal(tasklist_args[c(1, 3)], c("/FI", "/NH"))
  expect_match(tasklist_args[[2]], "PID eq 12345", fixed = TRUE)
  expect_true(grepl("^['\"].*['\"]$", tasklist_args[[2]]))
})

test_that("internal worker jobs are hidden and request ids are idempotent", {
  jobs_dir <- tempfile("ugplot-worker-jobs-")
  dir.create(jobs_dir)
  create_job <- ugplot_test_internal("ugplot_create_job")
  list_jobs <- ugplot_test_internal("ugplot_list_jobs")
  find_request <- ugplot_test_internal("ugplot_find_job_by_request_id")

  public <- create_job(
    data.frame(x = 1),
    config = list(runner = "ugplot_run_placeholder_job"),
    jobs_dir = jobs_dir
  )
  internal <- create_job(
    data.frame(x = 2),
    config = list(
      runner = "ugplot_run_geo_screen_group_job",
      internal_worker_task = TRUE,
      parent_job_id = public$id,
      worker_name = "Fy2",
      request_id = "parent:screen:TG1"
    ),
    jobs_dir = jobs_dir,
    type = "geo_worker"
  )

  expect_equal(list_jobs(jobs_dir)$id, public$id)
  expect_setequal(list_jobs(jobs_dir, include_internal = TRUE)$id, c(public$id, internal$id))
  expect_equal(find_request("parent:screen:TG1", jobs_dir)$id, internal$id)
})

test_that("stopping a coordinator immediately stops local and remote worker jobs", {
  jobs_dir <- tempfile("ugplot-stop-tree-")
  dir.create(jobs_dir)
  create_job <- ugplot_test_internal("ugplot_create_job")
  update_status <- ugplot_test_internal("ugplot_update_job_status")
  read_status <- ugplot_test_internal("ugplot_read_job_status")
  stop_job <- ugplot_test_internal("ugplot_stop_job")
  workers <- list(
    list(name = "Fy2", url = "http://fy2:8080", token = "local"),
    list(name = "Fy3", url = "http://fy3:8080", token = "remote")
  )
  parent <- create_job(
    data.frame(x = 1),
    config = list(runner = "ugplot_run_geo_pipeline_job", type = "geo", distributed_workers = workers),
    jobs_dir = jobs_dir, type = "geo"
  )
  child <- create_job(
    data.frame(x = 1),
    config = list(
      runner = "ugplot_run_geo_complete_group_job", internal_worker_task = TRUE,
      parent_job_id = parent$id, worker_name = "Fy2",
      distributed_group = data.frame(GroupID = "TG1")
    ),
    jobs_dir = jobs_dir, type = "geo_worker"
  )
  update_status(child$id, jobs_dir, state = "running", message = "Working")
  update_status(
    parent$id, jobs_dir, state = "running", message = "Distributed work",
    distributed_state = list(active = 2L, active_groups = c("Fy2:TG1", "Fy3:TG2"))
  )
  remote_stopped <- character(0)
  ugplot_test_local_namespace_binding("ugplot_remote_list_jobs", function(...) {
    data.frame(
      id = "remote-TG2", parent_job_id = parent$id, state = "running",
      stringsAsFactors = FALSE
    )
  })
  ugplot_test_local_namespace_binding("ugplot_remote_stop_job", function(server_url, job_id, ...) {
    remote_stopped <<- c(remote_stopped, job_id)
    list(id = job_id, state = "stopped")
  })
  withr::local_envvar(UGPLOT_SERVER_NAME = "Fy2")

  stopped <- stop_job(parent$id, jobs_dir)
  expect_equal(stopped$state, "stopped")
  expect_equal(read_status(child$id, jobs_dir)$state, "stopped")
  expect_equal(remote_stopped, "remote-TG2")
  expect_match(stopped$message, "stopped 2 active worker task", ignore.case = TRUE)
})

test_that("idempotent worker requests restart an existing failed task", {
  jobs_dir <- tempfile("ugplot-retry-worker-jobs-")
  dir.create(jobs_dir)
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  start_job <- ugplot_test_internal("ugplot_start_background_job")
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")

  internal <- create_job(
    data.frame(x = 2),
    config = list(
      runner = "ugplot_run_geo_complete_group_job",
      internal_worker_task = TRUE,
      parent_job_id = "parent",
      worker_name = "Fy2",
      request_id = "parent:analyze:TG1"
    ),
    jobs_dir = jobs_dir,
    type = "geo_worker"
  )
  internal$state <- "failed"
  internal$message <- "Failed"
  internal$error <- "Failure produced by an older worker build"
  write_job_status(internal$id, internal, jobs_dir)

  launched <- character(0)
  ugplot_test_local_namespace_binding("ugplot_launch_background_job", function(job_id, jobs_dir, ...) {
    launched <<- c(launched, job_id)
    list(job = read_job_status(job_id, jobs_dir), process = NULL)
  })

  restarted <- start_job(
    data.frame(x = 2),
    config = list(request_id = "parent:analyze:TG1"),
    jobs_dir = jobs_dir
  )

  expect_identical(restarted$job$id, internal$id)
  expect_identical(launched, internal$id)
  expect_true(restarted$reused)
  expect_true(restarted$restarted)
  expect_equal(readRDS(file.path(jobs_dir, internal$id, "status.rds"))$state, "queued")
})

test_that("background job creation can acknowledge before worker startup", {
  jobs_dir <- tempfile("ugplot-async-start-jobs-")
  dir.create(jobs_dir)
  start_job <- ugplot_test_internal("ugplot_start_background_job")
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")

  received_wait <- NULL
  ugplot_test_local_namespace_binding(
    "ugplot_launch_background_job",
    function(job_id, jobs_dir, startup_wait_seconds) {
      received_wait <<- startup_wait_seconds
      list(job = read_job_status(job_id, jobs_dir), process = NULL)
    }
  )

  started <- start_job(
    data.frame(x = 1),
    config = list(target = "x"),
    jobs_dir = jobs_dir,
    startup_wait_seconds = 0
  )

  expect_identical(received_wait, 0)
  expect_identical(started$job$state, "queued")
})

test_that("public job listing does not refresh internal worker jobs", {
  jobs_dir <- tempfile("ugplot-fast-public-jobs-")
  dir.create(jobs_dir)
  create_job <- ugplot_test_internal("ugplot_create_job")
  list_jobs <- ugplot_test_internal("ugplot_list_jobs")
  refresh_status <- ugplot_test_internal("ugplot_refresh_job_status")

  public <- create_job(
    data.frame(x = 1),
    config = list(runner = "ugplot_run_placeholder_job"),
    jobs_dir = jobs_dir
  )
  for (index in seq_len(4L)) {
    create_job(
      data.frame(x = index),
      config = list(
        runner = "ugplot_run_geo_screen_group_job",
        internal_worker_task = TRUE,
        parent_job_id = public$id,
        request_id = paste0(public$id, ":screen:TG", index)
      ),
      jobs_dir = jobs_dir,
      type = "geo_worker"
    )
  }

  refreshed_ids <- character(0)
  ugplot_test_local_namespace_binding("ugplot_refresh_job_status", function(status, jobs_dir) {
    refreshed_ids <<- c(refreshed_ids, as.character(status$id))
    refresh_status(status, jobs_dir)
  })

  listed <- list_jobs(jobs_dir)
  expect_equal(listed$id, public$id)
  expect_equal(refreshed_ids, public$id)
})

test_that("lightweight worker status preserves live progress without a full refresh", {
  jobs_dir <- tempfile("ugplot-light-worker-status-")
  dir.create(jobs_dir)
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  read_lightweight <- ugplot_test_internal("ugplot_read_job_status_lightweight")

  status <- create_job(
    data.frame(x = 1),
    config = list(
      runner = "ugplot_run_geo_complete_group_job",
      internal_worker_task = TRUE,
      request_id = "parent:analyze:TG2"
    ),
    jobs_dir = jobs_dir,
    type = "geo_worker"
  )
  status$state <- "running"
  status$pid <- 12345L
  status$progress <- 0.42
  status$message <- "Screening TG2: Running RRF dataset seed 1 training seed 4"
  status$current_model <- "RRF"
  write_job_status(status$id, status, jobs_dir)

  ugplot_test_local_namespace_binding("ugplot_process_alive", function(...) TRUE)
  ugplot_test_local_namespace_binding("ugplot_running_job_timed_out", function(...) FALSE)
  ugplot_test_local_namespace_binding("ugplot_refresh_job_status", function(...) {
    stop("full refresh must not run for a live worker")
  })

  observed <- read_lightweight(status$id, jobs_dir)
  expect_equal(observed$progress, 0.42)
  expect_equal(observed$current_model, "RRF")
  expect_match(observed$message, "Screening TG2")
  expect_false(observed$resumable)
})

test_that("lightweight worker status still refreshes a dead process", {
  jobs_dir <- tempfile("ugplot-dead-worker-status-")
  dir.create(jobs_dir)
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  read_lightweight <- ugplot_test_internal("ugplot_read_job_status_lightweight")

  status <- create_job(
    data.frame(x = 1),
    config = list(internal_worker_task = TRUE),
    jobs_dir = jobs_dir,
    type = "geo_worker"
  )
  status$state <- "running"
  status$pid <- 12345L
  write_job_status(status$id, status, jobs_dir)

  ugplot_test_local_namespace_binding("ugplot_process_alive", function(...) FALSE)
  ugplot_test_local_namespace_binding("ugplot_refresh_job_status", function(status, ...) {
    status$state <- "failed"
    status$message <- "Background process stopped before finishing"
    status
  })

  observed <- read_lightweight(status$id, jobs_dir)
  expect_equal(observed$state, "failed")
  expect_match(observed$message, "stopped before finishing")
})

test_that("job bundles redact distributed worker tokens", {
  redact <- ugplot_test_internal("ugplot_redact_job_config")
  config <- list(
    distributed_workers = list(
      list(name = "Fy2", url = "http://fy2:8080", token = "secret"),
      list(name = "Fy3", url = "http://fy3:8080", token = "other")
    )
  )
  redacted <- redact(config)
  expect_equal(vapply(redacted$distributed_workers, `[[`, character(1), "token"), c("", ""))
  expect_equal(vapply(config$distributed_workers, `[[`, character(1), "token"), c("secret", "other"))
})

test_that("job runner records progress and result", {
  jobs_dir <- tempfile("ugplot-jobs-")
  dataset <- data.frame(x = 1:4, y = 5:8)
  create_job <- ugplot_test_internal("ugplot_create_job")
  run_job_from_dir <- ugplot_test_internal("ugplot_run_job_from_dir")
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")

  status <- create_job(
    dataset,
    config = list(runner = "ugplot_run_placeholder_job", steps = 2, delay = 0),
    jobs_dir = jobs_dir
  )

  result <- run_job_from_dir(status$id, jobs_dir)
  final_status <- read_job_status(status$id, jobs_dir)

  expect_equal(final_status$state, "finished")
  expect_equal(final_status$progress, 1)
  expect_true(file.exists(final_status$result_path))
  expect_equal(result$summary$rows, 4)
  expect_equal(result$summary$columns, 2)
})

test_that("job runner accepts structured progress phases from GEO workers", {
  jobs_dir <- tempfile("ugplot-phased-progress-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  run_job_from_dir <- ugplot_test_internal("ugplot_run_job_from_dir")

  status <- create_job(
    data.frame(x = 1:3),
    config = list(runner = "ugplot_run_placeholder_job"),
    jobs_dir = jobs_dir
  )
  ugplot_test_local_namespace_binding("ugplot_run_placeholder_job", function(
      dataset, config = list(), progress_callback = function(...) NULL) {
    progress_callback(
      progress = 0.5,
      message = "Screening transcript group",
      phase = "screening",
      future_metadata = "accepted without breaking the job"
    )
    list(ok = TRUE)
  })

  expect_silent(run_job_from_dir(status$id, jobs_dir))
  final_status <- readRDS(file.path(jobs_dir, status$id, "status.rds"))
  expect_equal(final_status$state, "finished")
  expect_equal(final_status$current_phase, "screening")
})

test_that("background jobs survive launcher process object collection", {
  skip_if_not_installed("callr")
  skip_on_os("windows")

  jobs_dir <- tempfile("ugplot-jobs-detached-")
  start_job <- ugplot_test_internal("ugplot_start_background_job")
  stop_job <- ugplot_test_internal("ugplot_stop_job")
  process_alive <- ugplot_test_internal("ugplot_process_alive")
  terminate_process <- ugplot_test_internal("ugplot_terminate_process")

  started <- start_job(
    data.frame(x = 1:3),
    config = list(runner = "ugplot_run_placeholder_job", steps = 1L, delay = 2),
    jobs_dir = jobs_dir
  )
  job_id <- started$job$id
  pid <- started$job$pid
  on.exit({
    if (process_alive(pid)) {
      try(terminate_process(pid), silent = TRUE)
    }
  }, add = TRUE)

  expect_equal(started$job$state, "running")
  rm(started)
  gc()
  Sys.sleep(0.2)

  raw_status <- readRDS(file.path(jobs_dir, job_id, "status.rds"))
  expect_true(process_alive(pid))
  expect_equal(raw_status$state, "running")
  expect_equal(stop_job(job_id, jobs_dir)$state, "stopped")
})

test_that("process termination includes isolated descendants", {
  skip_on_os("windows")
  skip_if_not(dir.exists("/proc"))
  skip_if_not_installed("processx")
  skip_if_not_installed("ps")
  terminate_process <- ugplot_test_internal("ugplot_terminate_process")
  process_alive <- ugplot_test_internal("ugplot_process_alive")
  process_tree <- ugplot_test_internal("ugplot_linux_process_tree_metrics")
  process <- processx::process$new(
    "/bin/sh", c("-c", "sleep 60 & wait"),
    cleanup = FALSE, cleanup_tree = FALSE
  )
  on.exit(try(process$kill_tree(), silent = TRUE), add = TRUE)
  Sys.sleep(0.2)
  metrics <- process_tree(process$get_pid())
  expect_gte(metrics$process_count, 2L)
  descendants <- setdiff(metrics$pids, process$get_pid())
  descendant_handles <- Filter(Negate(is.null), lapply(descendants, function(pid) {
    tryCatch(ps::ps_handle(pid), error = function(e) NULL)
  }))
  expect_gte(length(descendant_handles), 1L)

  terminate_process(process$get_pid())

  expect_false(process_alive(process$get_pid()))
  deadline <- Sys.time() + 3
  while (any(vapply(descendant_handles, ps::ps_is_running, logical(1))) && Sys.time() < deadline) {
    Sys.sleep(0.05)
  }
  expect_false(any(vapply(descendant_handles, ps::ps_is_running, logical(1))))
})

test_that("job listing marks dead background processes as failed", {
  jobs_dir <- tempfile("ugplot-jobs-")
  dataset <- data.frame(x = 1:3)
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")
  list_jobs <- ugplot_test_internal("ugplot_list_jobs")

  status <- create_job(dataset, config = list(runner = "ugplot_run_placeholder_job"), jobs_dir = jobs_dir)
  status$state <- "running"
  status$progress <- 0.25
  status$message <- "Running"
  status$pid <- 2147483647L
  write_job_status(status$id, status, jobs_dir)

  refreshed <- read_job_status(status$id, jobs_dir)
  listed <- list_jobs(jobs_dir)

  expect_equal(refreshed$state, "failed")
  expect_match(refreshed$message, "Background process stopped")
  expect_equal(listed$state, "failed")
})

test_that("job listing reports CpG columns from the submitted dataset", {
  jobs_dir <- tempfile("ugplot-jobs-cpg-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  list_jobs <- ugplot_test_internal("ugplot_list_jobs")
  status <- create_job(
    data.frame(target = 1:3, cg0001 = 0.1, cg0002 = 0.2, other = 1),
    config = list(models = "lm"),
    jobs_dir = jobs_dir
  )

  jobs <- list_jobs(jobs_dir, lightweight = TRUE)
  expect_equal(jobs$cpgs[jobs$id == status$id], 2L)
})

test_that("stopped jobs keep partial results available", {
  jobs_dir <- tempfile("ugplot-jobs-")
  dataset <- data.frame(x = 1:3)
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_partial_result <- ugplot_test_internal("ugplot_write_job_partial_result")
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")
  read_job_preview_result <- ugplot_test_internal("ugplot_read_job_preview_result")
  read_job_result <- ugplot_test_internal("ugplot_read_job_result")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  stop_job <- ugplot_test_internal("ugplot_stop_job")

  best_model <- stats::lm(x ~ 1, data = dataset)
  status <- create_job(dataset, config = list(runner = "ugplot_run_placeholder_job"), jobs_dir = jobs_dir)
  write_job_partial_result(status$id, list(
    results_table = data.frame(Model = "lm", R2 = 0.5, dataset_seed = 1L, training_seed = 1L, Status = "OK"),
    final_summary = list(dataset_seed = 1L, training_seed = 1L),
    best_model_name = "lm",
    best_model = best_model
  ), jobs_dir)
  status <- read_job_status(status$id, jobs_dir)

  status$state <- "running"
  status$pid <- 2147483647L
  write_job_status(status$id, status, jobs_dir)

  stopped <- stop_job(status$id, jobs_dir)

  expect_equal(stopped$state, "stopped")
  expect_true(file.exists(stopped$result_path))
  expect_equal(readRDS(stopped$result_path)$results_table$R2, 0.5)
  expect_true(file.exists(stopped$preview_result_path))
  expect_true(file.exists(stopped$best_model_path))
  expect_equal(stopped$resume_completed_keys, paste("lm", "1", "1", sep = "\r"))
  preview <- read_job_preview_result(status$id, jobs_dir)
  expect_equal(preview$results_table$R2, 0.5)
  expect_null(preview$best_model)
  loaded <- read_job_result(status$id, jobs_dir)
  expect_s3_class(loaded$best_model, "lm")
})

test_that("job previews retain elapsed time and summarize model timeout behavior", {
  preview <- ugplot_test_internal("ugplot_job_result_preview")(list(
    results_table = data.frame(
      Model = c("slow", "slow", "slow", "fast"),
      Status = c("TIMEOUT", "TIMEOUT", "SKIPPED_TIMEOUT", "OK"),
      elapsed_seconds = c(1200, 1200, 0, 12),
      Error = "",
      stringsAsFactors = FALSE
    )
  ))
  summarize <- ugplot_test_internal("ugplot_model_timing_summary")
  summary <- summarize(preview$results_table)

  expect_true("elapsed_seconds" %in% names(preview$results_table))
  slow <- summary[summary$Model == "slow", , drop = FALSE]
  expect_equal(slow$Attempts, 2)
  expect_equal(slow$Timeouts, 2)
  expect_equal(slow$Skipped, 1)
  expect_equal(slow$`Timeout rate`, 100)
  expect_equal(slow$Signal, "Frequent timeout")
  expect_equal(summary$Signal[summary$Model == "fast"], "Healthy")
})

test_that("job model policy toggles only future effective models", {
  jobs_dir <- tempfile("ugplot-model-policy-")
  dir.create(jobs_dir, recursive = TRUE)
  create_job <- ugplot_test_internal("ugplot_create_job")
  read_policy <- ugplot_test_internal("ugplot_read_job_model_policy")
  set_enabled <- ugplot_test_internal("ugplot_set_job_model_enabled")
  effective <- ugplot_test_internal("ugplot_effective_models_for_job_dir")
  job_dir <- ugplot_test_internal("ugplot_job_dir")

  status <- create_job(
    data.frame(x = 1:3, y = 4:6),
    config = list(models = c("lm", "rpart", "glmnet")),
    jobs_dir = jobs_dir
  )
  expect_equal(read_policy(status$id, jobs_dir)$enabled_models, c("lm", "rpart", "glmnet"))

  disabled <- set_enabled(status$id, "rpart", FALSE, jobs_dir)
  expect_equal(disabled$disabled_models, "rpart")
  expect_equal(
    effective(c("lm", "rpart", "glmnet"), job_dir(status$id, jobs_dir)),
    c("lm", "glmnet")
  )

  reenabled <- set_enabled(status$id, "rpart", TRUE, jobs_dir)
  expect_length(reenabled$disabled_models, 0L)
  expect_error(set_enabled(status$id, "unknown", FALSE, jobs_dir), "not configured")
  set_enabled(status$id, "rpart", FALSE, jobs_dir)
  set_enabled(status$id, "glmnet", FALSE, jobs_dir)
  expect_error(set_enabled(status$id, "lm", FALSE, jobs_dir), "At least one model")
})

test_that("model attempt diagnostics retain full error context", {
  details <- ugplot_test_internal("ugplot_model_attempt_details")(list(
    data.frame(
      Model = c("slow", "slow", "ok"), Analysis = "TG10/screen_result.rds",
      dataset_seed = 1L, training_seed = c(1L, 2L, 1L),
      Status = c("TIMEOUT", "ERROR", "OK"), elapsed_seconds = c(1200, 3, 1),
      Error = c("Timed out after 1200 seconds", "package dependency failed: details", ""),
      stringsAsFactors = FALSE
    )
  ), model = "slow")
  expect_equal(nrow(details), 2L)
  expect_equal(unique(details$Model), "slow")
  expect_true(all(c("Analysis", "Status", "Error", "elapsed_seconds") %in% names(details)))
  expect_match(details$Error[[2]], "dependency failed", fixed = TRUE)
})

test_that("local GEO stages clear stale active workers without losing progress", {
  idle <- ugplot_test_internal("ugplot_idle_distributed_state")
  active_groups <- ugplot_test_internal("ugplot_active_distributed_group_ids")
  state <- list(
    workers = c("Fy2", "Fy3"), completed = 10L, total = 2148L, active = 2L,
    active_groups = c("Fy3:TG9", "Fy2:TG17"),
    active_tasks = data.frame(worker = c("Fy3", "Fy2"), group = c("TG9", "TG17"))
  )
  waiting <- idle(state)

  expect_equal(waiting$completed, 10L)
  expect_equal(waiting$total, 2148L)
  expect_equal(waiting$workers, c("Fy2", "Fy3"))
  expect_equal(waiting$active, 0L)
  expect_length(waiting$active_groups, 0L)
  expect_equal(nrow(waiting$active_tasks), 0L)
  expect_length(active_groups(list(
    message = "Building transcript ML datasets for |rho| >= 0.7",
    distributed_state = state
  )), 0L)
  expect_equal(active_groups(list(
    message = "Distributed complete analysis: 10/2148 group(s); active Fy3:TG9, Fy2:TG17",
    distributed_state = state
  )), c("TG9", "TG17"))
})

test_that("model timing can recover elapsed values from older compact previews", {
  jobs_dir <- tempfile("ugplot-jobs-timing-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_atomic <- ugplot_test_internal("ugplot_write_rds_atomic")
  update_status <- ugplot_test_internal("ugplot_update_job_status")
  result_path <- ugplot_test_internal("ugplot_result_path")
  preview_path <- ugplot_test_internal("ugplot_preview_result_path")
  read_timing <- ugplot_test_internal("ugplot_read_job_model_timing")
  status <- create_job(data.frame(target = 1:2, cg1 = c(0.1, 0.2)), jobs_dir = jobs_dir)
  full_path <- result_path(status$id, jobs_dir, partial = TRUE)
  compact_path <- preview_path(status$id, jobs_dir)
  write_atomic(list(results_table = data.frame(
    Model = "gbm", Status = "TIMEOUT", elapsed_seconds = 600,
    stringsAsFactors = FALSE
  )), full_path)
  write_atomic(list(results_table = data.frame(
    Model = "gbm", Status = "TIMEOUT", stringsAsFactors = FALSE
  )), compact_path)
  update_status(
    status$id, jobs_dir,
    partial_result_path = full_path,
    preview_result_path = compact_path
  )

  timing <- read_timing(status$id, jobs_dir)
  expect_equal(timing$`Max seconds`, 600)
})

test_that("running jobs that exceed timeout are stopped with partial result", {
  ugplot_test_local_namespace_binding("ugplot_process_alive", function(pid) TRUE)
  ugplot_test_local_namespace_binding("ugplot_terminate_process", function(pid) TRUE)

  jobs_dir <- tempfile("ugplot-jobs-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_partial_result <- ugplot_test_internal("ugplot_write_job_partial_result")
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")
  write_rds_atomic <- ugplot_test_internal("ugplot_write_rds_atomic")
  status_path <- ugplot_test_internal("ugplot_status_path")
  refresh_job_status <- ugplot_test_internal("ugplot_refresh_job_status")

  status <- create_job(
    data.frame(x = 1:3),
    config = list(runner = "ugplot_run_placeholder_job", timeout = 1),
    jobs_dir = jobs_dir
  )
  write_job_partial_result(status$id, list(results_table = data.frame(x = 2)), jobs_dir)
  status <- read_job_status(status$id, jobs_dir)
  status$state <- "running"
  status$pid <- 2147483647L
  status$updated_at <- format(Sys.time() - 3600, "%Y-%m-%d %H:%M:%S %z")
  write_rds_atomic(status, status_path(status$id, jobs_dir))

  refreshed <- refresh_job_status(status, jobs_dir)

  expect_equal(refreshed$state, "stopped")
  expect_match(refreshed$message, "Timed out")
  expect_true(file.exists(refreshed$result_path))
  expect_equal(readRDS(refreshed$result_path)$results_table$x, 2)
})

test_that("job delete removes finished jobs and protects running jobs", {
  jobs_dir <- tempfile("ugplot-jobs-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  delete_job <- ugplot_test_internal("ugplot_delete_job")
  job_dir <- ugplot_test_internal("ugplot_job_dir")

  finished <- create_job(data.frame(x = 1), config = list(), jobs_dir = jobs_dir)
  finished$state <- "finished"
  write_job_status(finished$id, finished, jobs_dir)

  deleted <- delete_job(finished$id, jobs_dir)
  expect_true(deleted$deleted)
  expect_false(dir.exists(job_dir(finished$id, jobs_dir)))

  ugplot_test_local_namespace_binding("ugplot_process_alive", function(pid) TRUE)
  ugplot_test_local_namespace_binding("ugplot_terminate_process", function(pid) TRUE)
  running <- create_job(data.frame(x = 2), config = list(), jobs_dir = jobs_dir)
  running$state <- "running"
  running$pid <- 2147483647L
  write_job_status(running$id, running, jobs_dir)

  expect_error(delete_job(running$id, jobs_dir), "Stop the job before deleting it")
  expect_true(dir.exists(job_dir(running$id, jobs_dir)))

  forced <- delete_job(running$id, jobs_dir, force = TRUE)
  expect_true(forced$deleted)
  expect_false(dir.exists(job_dir(running$id, jobs_dir)))
})

test_that("job status keeps resume metadata and job bundle", {
  jobs_dir <- tempfile("ugplot-jobs-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")
  read_job_bundle <- ugplot_test_internal("ugplot_read_job_bundle")
  list_jobs <- ugplot_test_internal("ugplot_list_jobs")

  dataset <- data.frame(x = 1:3, y = 4:6)
  status <- create_job(
    dataset,
    config = list(runner = "ugplot_run_placeholder_job", target = "y", models = c("lm", "glm")),
    jobs_dir = jobs_dir
  )
  status$state <- "failed"
  status$message <- "Failed"
  write_job_status(status$id, status, jobs_dir)

  refreshed <- read_job_status(status$id, jobs_dir)
  expect_true(refreshed$resumable)
  expect_equal(refreshed$config_summary$target, "y")
  expect_equal(refreshed$config_summary$models, "lm, glm")

  listed <- list_jobs(jobs_dir)
  expect_true(listed$resumable)
  expect_equal(listed$target, "y")
  expect_equal(listed$models, "lm, glm")

  bundle <- read_job_bundle(status$id, jobs_dir)
  expect_equal(bundle$dataset, dataset)
  expect_equal(bundle$config$target, "y")
  expect_true(bundle$status$resumable)

  ugplot_test_local_namespace_binding("ugplot_process_alive", function(pid) TRUE)
  running <- create_job(dataset, config = list(target = "y"), jobs_dir = jobs_dir)
  running$state <- "running"
  running$pid <- 2147483647L
  write_job_status(running$id, running, jobs_dir)

  expect_error(read_job_bundle(running$id, jobs_dir), "Full job bundle is not available while the job is active")
})

test_that("resume migrates old ML jobs to isolated model timeouts", {
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")
  ugplot_test_local_namespace_binding("ugplot_launch_background_job", function(job_id, jobs_dir, ...) {
    list(job = read_job_status(job_id, jobs_dir), process = NULL)
  })

  jobs_dir <- tempfile("ugplot-jobs-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  resume_job <- ugplot_test_internal("ugplot_resume_background_job")

  status <- create_job(
    data.frame(y = 1:3, x = 1:3),
    config = list(
      runner = "ugplot_run_ml_job",
      target = "y",
      models = "Rborist",
      timeout = 1200,
      use_callr_timeout = FALSE
    ),
    jobs_dir = jobs_dir
  )
  status$state <- "stopped"
  status$error <- "The job process is no longer running."
  status$current_run_key <- paste("Rborist", 1, 1, sep = "\r")
  status$current_model <- "Rborist"
  status$current_dataset_seed <- 1L
  status$current_training_seed <- 1L
  write_job_status(status$id, status, jobs_dir)

  resume_job(status$id, jobs_dir)
  config <- readRDS(file.path(jobs_dir, status$id, "config.rds"))
  resumed_status <- readRDS(file.path(jobs_dir, status$id, "status.rds"))

  expect_true(config$use_callr_timeout)
  expect_equal(config$watchdog_timeout_multiplier, 3)
  expect_equal(resumed_status$watchdog_timeout_multiplier, 3)
  expect_false(paste("Rborist", 1, 1, sep = "\r") %in% (config$resume_completed_keys %||% character(0)))
  expect_equal(config$resume_failed_runs[[1]]$model, "Rborist")
  expect_true(endsWith(config$model_log_dir, "model-logs"))
})

test_that("GEO resume keeps large checkpoints out of the job config", {
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")
  ugplot_test_local_namespace_binding("ugplot_launch_background_job", function(job_id, jobs_dir, ...) {
    list(job = read_job_status(job_id, jobs_dir), process = NULL)
  })

  jobs_dir <- tempfile("ugplot-jobs-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  write_partial <- ugplot_test_internal("ugplot_write_job_partial_result")
  resume_job <- ugplot_test_internal("ugplot_resume_background_job")

  status <- create_job(
    data.frame(request = 1),
    config = list(
      runner = "ugplot_run_geo_pipeline_job",
      type = "geo",
      accession = "GSE87571",
      target_column = "age",
      models = c("lm", "rpart")
    ),
    jobs_dir = jobs_dir
  )
  checkpoint <- list(
    kind = "geo_pipeline",
    accession = "GSE87571",
    target_column = "age",
    large_table = data.frame(value = rep("checkpoint-payload", 10000))
  )
  write_partial(status$id, checkpoint, jobs_dir)
  status <- read_job_status(status$id, jobs_dir)
  status$state <- "failed"
  status$message <- "Background process stopped before finishing"
  write_job_status(status$id, status, jobs_dir)

  resume_job(status$id, jobs_dir)
  job_dir <- file.path(jobs_dir, status$id)
  resumed_config <- readRDS(file.path(job_dir, "config.rds"))
  backup_config <- readRDS(file.path(job_dir, "config-resume-backup.rds"))

  expect_equal(resumed_config$resume_result_path, file.path(job_dir, "partial-result.rds"))
  expect_null(resumed_config[["resume_result"]])
  expect_equal(backup_config$runner, "ugplot_run_geo_pipeline_job")
  expect_lt(file.info(file.path(job_dir, "config.rds"))$size,
            file.info(file.path(job_dir, "partial-result.rds"))$size)
})

test_that("GEO resume recovers an old corrupt config from a child worker", {
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")
  ugplot_test_local_namespace_binding("ugplot_launch_background_job", function(job_id, jobs_dir, ...) {
    list(job = read_job_status(job_id, jobs_dir), process = NULL)
  })
  ugplot_test_local_namespace_binding("ugplot_read_remote_servers", function() {
    data.frame(
      name = c("Fy2", "Fy3"),
      url = c("http://fy2:8080", "http://fy3:8080"),
      token = c("stale-token-2", "token-3"),
      cpu_limit = c(6L, 5L),
      cpu_max = c(8L, 6L),
      stringsAsFactors = FALSE
    )
  })
  ugplot_test_local_namespace_binding("ugplot_worker_accepts_token", function(worker, token) {
    identical(as.character(worker$name), "Fy3") &&
      identical(as.character(token), "current-token-2")
  })
  withr::local_envvar(UGPLOT_SERVER_TOKEN = "current-token-2")

  jobs_dir <- tempfile("ugplot-jobs-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  write_partial <- ugplot_test_internal("ugplot_write_job_partial_result")
  resume_job <- ugplot_test_internal("ugplot_resume_background_job")

  parent <- create_job(
    data.frame(request = 1),
    config = list(
      runner = "ugplot_run_geo_pipeline_job",
      type = "geo",
      accession = "GSE87571",
      target_column = "age"
    ),
    jobs_dir = jobs_dir
  )
  worker <- create_job(
    data.frame(target = 1:3, cg1 = 3:1),
    config = list(
      runner = "ugplot_run_geo_complete_group_job",
      type = "geo_worker",
      internal_worker_task = TRUE,
      parent_job_id = parent$id,
      worker_name = "Fy2",
      distributed_group = data.frame(GroupID = "TG3"),
      accession = "GSE87571",
      matrix_source = "raw_sesame",
      target_column = "age",
      models = c("lm", "rpart")
    ),
    jobs_dir = jobs_dir
  )
  write_partial(parent$id, list(
    kind = "geo_pipeline",
    accession = "GSE87571",
    matrix_source = "raw_sesame",
    target_column = "age"
  ), jobs_dir)
  parent <- read_job_status(parent$id, jobs_dir)
  parent$state <- "failed"
  parent$message <- "Background process stopped before finishing"
  parent$distributed_state <- list(workers = c("Fy2", "Fy3"))
  write_job_status(parent$id, parent, jobs_dir)
  config_path <- file.path(jobs_dir, parent$id, "config.rds")
  writeBin(charToRaw("truncated-config"), config_path)

  resume_job(parent$id, jobs_dir)
  recovered <- readRDS(config_path)
  corrupt_files <- Sys.glob(file.path(jobs_dir, parent$id, "config-corrupt-*.rds"))

  expect_equal(recovered$runner, "ugplot_run_geo_pipeline_job")
  expect_equal(recovered$models, c("lm", "rpart"))
  expect_equal(
    vapply(recovered$distributed_workers, function(server) server$name, character(1)),
    c("Fy2", "Fy3")
  )
  expect_equal(
    vapply(recovered$distributed_workers, function(server) server$token, character(1)),
    c("current-token-2", "current-token-2")
  )
  expect_null(recovered$internal_worker_task)
  expect_null(recovered$distributed_group)
  expect_length(corrupt_files, 1L)
  expect_true(file.exists(file.path(jobs_dir, worker$id, "config.rds")))
})

test_that("crashed ML jobs auto-resume with an attempt limit", {
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")
  ugplot_test_local_namespace_binding("ugplot_launch_background_job", function(job_id, jobs_dir, ...) {
    list(job = read_job_status(job_id, jobs_dir), process = NULL)
  })

  jobs_dir <- tempfile("ugplot-jobs-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  auto_resume <- ugplot_test_internal("ugplot_auto_resume_crashed_jobs")

  status <- create_job(
    data.frame(y = 1:3, x = 1:3),
    config = list(
      runner = "ugplot_run_ml_job",
      target = "y",
      models = "bagEarthGCV",
      timeout = 1200,
      auto_resume_max_attempts = 2
    ),
    jobs_dir = jobs_dir
  )
  status$state <- "failed"
  status$message <- "Background process stopped before finishing"
  status$error <- "The job process is no longer running."
  status$current_run_key <- paste("bagEarthGCV", 2, 1, sep = "\r")
  status$current_model <- "bagEarthGCV"
  status$current_dataset_seed <- 2L
  status$current_training_seed <- 1L
  write_job_status(status$id, status, jobs_dir)

  auto_resume(jobs_dir)
  resumed_status <- readRDS(file.path(jobs_dir, status$id, "status.rds"))
  resumed_config <- readRDS(file.path(jobs_dir, status$id, "config.rds"))

  expect_equal(resumed_status$state, "queued")
  expect_equal(resumed_status$auto_resume_count, 1L)
  expect_equal(resumed_config$resume_failed_runs[[1]]$model, "bagEarthGCV")

  resumed_status$state <- "failed"
  resumed_status$message <- "Background process stopped before finishing"
  resumed_status$auto_resume_count <- 2L
  write_job_status(status$id, resumed_status, jobs_dir)

  auto_resume(jobs_dir)
  limited_status <- readRDS(file.path(jobs_dir, status$id, "status.rds"))
  expect_equal(limited_status$state, "failed")
  expect_equal(limited_status$auto_resume_count, 2L)
})

test_that("only GEO coordinators auto-resume after a server restart", {
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")
  ugplot_test_local_namespace_binding("ugplot_launch_background_job", function(job_id, jobs_dir, ...) {
    list(job = read_job_status(job_id, jobs_dir), process = NULL)
  })

  jobs_dir <- tempfile("ugplot-geo-worker-jobs-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  auto_resume <- ugplot_test_internal("ugplot_auto_resume_crashed_jobs")
  statuses <- lapply(c("ugplot_run_geo_pipeline_job", "ugplot_run_geo_complete_group_job"), function(runner) {
    status <- create_job(
      data.frame(target = 1:3, cg1 = c(0.1, 0.2, 0.3)),
      config = list(
        runner = runner,
        internal_worker_task = !identical(runner, "ugplot_run_geo_pipeline_job"),
        auto_resume_max_attempts = 2L
      ),
      jobs_dir = jobs_dir
    )
    status$state <- "failed"
    status$message <- "Background process stopped before finishing"
    status$error <- "The server stopped."
    write_job_status(status$id, status, jobs_dir)
    status
  })

  auto_resume(jobs_dir)
  resumed_statuses <- lapply(statuses, function(status) {
    readRDS(file.path(jobs_dir, status$id, "status.rds"))
  })

  expect_equal(vapply(resumed_statuses, `[[`, character(1), "state"), c("queued", "failed"))
  expect_equal(resumed_statuses[[1]]$auto_resume_count, 1L)
  expect_null(resumed_statuses[[2]]$auto_resume_count)
})

test_that("resource monitor persists Linux job and host diagnostics", {
  skip_on_os("windows")
  skip_if_not(dir.exists("/proc"))

  jobs_dir <- tempfile("ugplot-jobs-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  monitor_jobs <- ugplot_test_internal("ugplot_monitor_active_jobs")
  read_resources <- ugplot_test_internal("ugplot_read_job_resources")
  server_snapshot <- ugplot_test_internal("ugplot_server_resource_snapshot")

  status <- create_job(data.frame(x = 1:3), config = list(), jobs_dir = jobs_dir)
  status$state <- "running"
  status$pid <- Sys.getpid()
  status$current_model <- "lm"
  write_job_status(status$id, status, jobs_dir)

  monitor_state <- new.env(parent = emptyenv())
  first <- monitor_jobs(jobs_dir, monitor_state)
  Sys.sleep(0.05)
  second <- monitor_jobs(jobs_dir, monitor_state)
  resources <- read_resources(status$id, jobs_dir)

  expect_named(first, status$id)
  expect_named(second, status$id)
  expect_equal(nrow(resources), 2L)
  expect_true(all(resources$alive))
  expect_true(all(resources$pid == Sys.getpid()))
  expect_true(all(resources$process_count >= 1L))
  expect_true(all(resources$process_rss_mb > 0))
  expect_equal(resources$current_model, rep("lm", 2L))
  expect_true(all(c(
    "host_mem_available_mb", "host_swap_free_mb", "memory_psi_some_avg10",
    "vm_oom_kill", "cgroup_oom_kill", "process_cpu_pct", "disk_available_mb",
    "disk_used_pct"
  ) %in% names(resources)))
  expect_equal(nrow(read_resources(status$id, jobs_dir, max_lines = 1L)), 1L)
  snapshot <- server_snapshot(jobs_dir)
  expect_equal(snapshot$active_processes, 1L)
  expect_true(is.finite(snapshot$process_rss_mb))
  expect_true(is.finite(snapshot$host_mem_used_pct))
  expect_match(paste(snapshot$tasks, collapse = " "), "lm|Running")
})

test_that("smooth drain stops a job at a cooperative boundary", {
  jobs_dir <- tempfile("ugplot-drain-")
  create_job <- ugplot_test_internal("ugplot_create_job")
  request_drain <- ugplot_test_internal("ugplot_request_job_drain")
  run_job <- ugplot_test_internal("ugplot_run_job_from_dir")

  status <- create_job(
    data.frame(x = 1:3),
    config = list(runner = "ugplot_run_placeholder_job", steps = 3L),
    jobs_dir = jobs_dir
  )
  requested <- request_drain(status$id, jobs_dir)
  expect_equal(requested$state, "draining")
  expect_false(requested$resumable)

  run_job(status$id, jobs_dir)
  drained <- readRDS(file.path(jobs_dir, status$id, "status.rds"))
  expect_equal(drained$state, "stopped")
  expect_match(drained$message, "Drained safely")
})
test_that("job ids cannot traverse outside the jobs directory", {
  validate_job_id <- ugplot_test_internal("ugplot_validate_job_id")
  job_dir <- ugplot_test_internal("ugplot_job_dir")
  jobs_dir <- tempfile("ugplot-safe-jobs-")

  expect_error(validate_job_id(".."), "Invalid job id", fixed = TRUE)
  expect_error(validate_job_id("."), "Invalid job id", fixed = TRUE)
  expect_error(validate_job_id("../private"), "Invalid job id", fixed = TRUE)
  expect_error(validate_job_id("folder/private"), "Invalid job id", fixed = TRUE)
  expect_equal(job_dir("20260714-safe_job.1", jobs_dir),
               file.path(jobs_dir, "20260714-safe_job.1"))
})
