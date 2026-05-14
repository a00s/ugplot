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
  local_env <- new.env(parent = globalenv())
  local_env$`%||%` <- function(lhs, rhs) {
    if (is.null(lhs)) rhs else lhs
  }
  job_store_path <- file.path("R", "job_store.R")
  if (!file.exists(job_store_path)) {
    job_store_path <- file.path("..", "..", "R", "job_store.R")
  }
  sys.source(job_store_path, envir = local_env)

  jobs_dir <- tempfile("ugplot-jobs-")
  dataset <- data.frame(x = 1:3)
  create_job <- local_env$ugplot_create_job
  write_job_status <- local_env$ugplot_write_job_status
  read_job_status <- local_env$ugplot_read_job_status
  list_jobs <- local_env$ugplot_list_jobs

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
  local_env <- new.env(parent = globalenv())
  local_env$`%||%` <- function(lhs, rhs) {
    if (is.null(lhs)) rhs else lhs
  }
  job_store_path <- file.path("R", "job_store.R")
  if (!file.exists(job_store_path)) {
    job_store_path <- file.path("..", "..", "R", "job_store.R")
  }
  sys.source(job_store_path, envir = local_env)

  jobs_dir <- tempfile("ugplot-jobs-")
  dataset <- data.frame(x = 1:3)
  status <- local_env$ugplot_create_job(dataset, config = list(runner = "ugplot_run_placeholder_job"), jobs_dir = jobs_dir)
  local_env$ugplot_write_job_partial_result(status$id, list(results_table = data.frame(x = 1)), jobs_dir)
  status <- local_env$ugplot_read_job_status(status$id, jobs_dir)

  status$state <- "running"
  status$pid <- 2147483647L
  local_env$ugplot_write_job_status(status$id, status, jobs_dir)

  stopped <- local_env$ugplot_stop_job(status$id, jobs_dir)

  expect_equal(stopped$state, "stopped")
  expect_true(file.exists(stopped$result_path))
  expect_equal(readRDS(stopped$result_path)$results_table$x, 1)
})

test_that("running jobs that exceed timeout are stopped with partial result", {
  local_env <- new.env(parent = globalenv())
  local_env$`%||%` <- function(lhs, rhs) {
    if (is.null(lhs)) rhs else lhs
  }
  job_store_path <- file.path("R", "job_store.R")
  if (!file.exists(job_store_path)) {
    job_store_path <- file.path("..", "..", "R", "job_store.R")
  }
  sys.source(job_store_path, envir = local_env)
  local_env$ugplot_process_alive <- function(pid) TRUE
  local_env$ugplot_terminate_process <- function(pid) TRUE

  jobs_dir <- tempfile("ugplot-jobs-")
  status <- local_env$ugplot_create_job(
    data.frame(x = 1:3),
    config = list(runner = "ugplot_run_placeholder_job", timeout = 1),
    jobs_dir = jobs_dir
  )
  local_env$ugplot_write_job_partial_result(status$id, list(results_table = data.frame(x = 2)), jobs_dir)
  status <- local_env$ugplot_read_job_status(status$id, jobs_dir)
  status$state <- "running"
  status$pid <- 2147483647L
  status$updated_at <- format(Sys.time() - 120, "%Y-%m-%d %H:%M:%S %z")
  local_env$ugplot_write_rds_atomic(status, local_env$ugplot_status_path(status$id, jobs_dir))

  refreshed <- local_env$ugplot_refresh_job_status(status, jobs_dir)

  expect_equal(refreshed$state, "stopped")
  expect_match(refreshed$message, "Timed out")
  expect_true(file.exists(refreshed$result_path))
  expect_equal(readRDS(refreshed$result_path)$results_table$x, 2)
})
