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

test_that("stopped jobs keep partial results available", {
  jobs_dir <- tempfile("ugplot-jobs-")
  dataset <- data.frame(x = 1:3)
  create_job <- ugplot_test_internal("ugplot_create_job")
  write_job_partial_result <- ugplot_test_internal("ugplot_write_job_partial_result")
  read_job_status <- ugplot_test_internal("ugplot_read_job_status")
  read_job_preview_result <- ugplot_test_internal("ugplot_read_job_preview_result")
  write_job_status <- ugplot_test_internal("ugplot_write_job_status")
  stop_job <- ugplot_test_internal("ugplot_stop_job")

  status <- create_job(dataset, config = list(runner = "ugplot_run_placeholder_job"), jobs_dir = jobs_dir)
  write_job_partial_result(status$id, list(
    results_table = data.frame(Model = "lm", R2 = 0.5, dataset_seed = 1L, training_seed = 1L, Status = "OK"),
    best_model = stats::lm(x ~ 1, data = dataset)
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
  expect_equal(stopped$resume_completed_keys, paste("lm", "1", "1", sep = "\r"))
  preview <- read_job_preview_result(status$id, jobs_dir)
  expect_equal(preview$results_table$R2, 0.5)
  expect_null(preview$best_model)
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
  status$updated_at <- format(Sys.time() - 120, "%Y-%m-%d %H:%M:%S %z")
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
})
