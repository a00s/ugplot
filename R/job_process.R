ugplot_run_placeholder_job <- function(dataset, config = list(), progress_callback = function(...) NULL) {
  steps <- config$steps %||% 5L
  steps <- max(1L, as.integer(steps))
  delay <- config$delay %||% 0
  delay <- max(0, as.numeric(delay))

  for (step in seq_len(steps)) {
    if (delay > 0) {
      Sys.sleep(delay)
    }
    progress_callback(
      progress = step / steps,
      message = paste0("Processed step ", step, " of ", steps)
    )
  }

  list(
    summary = data.frame(
      rows = nrow(dataset),
      columns = ncol(dataset),
      stringsAsFactors = FALSE
    ),
    config = config
  )
}

ugplot_run_job_from_dir <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  dataset <- readRDS(file.path(job_dir, "dataset.rds"))
  config <- readRDS(file.path(job_dir, "config.rds"))
  runner <- config$runner %||% "ugplot_run_placeholder_job"

  ugplot_update_job_status(
    job_id,
    jobs_dir,
    state = "running",
    progress = 0,
    message = "Running",
    pid = Sys.getpid()
  )
  ugplot_append_job_log(job_id, paste0("Started runner: ", runner), jobs_dir)

  progress_callback <- function(progress = NULL, message = NULL) {
    updates <- list()
    if (!is.null(progress)) {
      updates$progress <- max(0, min(1, as.numeric(progress)))
    }
    if (!is.null(message)) {
      updates$message <- as.character(message)
      ugplot_append_job_log(job_id, as.character(message), jobs_dir)
    }
    do.call(ugplot_update_job_status, c(list(job_id = job_id, jobs_dir = jobs_dir), updates))
  }

  tryCatch({
    runner_fun <- get(runner, mode = "function")
    result <- runner_fun(dataset = dataset, config = config, progress_callback = progress_callback)
    result_path <- file.path(job_dir, "result.rds")
    saveRDS(result, result_path)
    ugplot_update_job_status(
      job_id,
      jobs_dir,
      state = "finished",
      progress = 1,
      message = "Finished",
      result_path = result_path
    )
    invisible(result)
  }, error = function(e) {
    ugplot_append_job_log(job_id, paste0("Failed: ", conditionMessage(e)), jobs_dir)
    ugplot_update_job_status(
      job_id,
      jobs_dir,
      state = "failed",
      message = "Failed",
      error = conditionMessage(e)
    )
    stop(e)
  })
}

ugplot_start_background_job <- function(dataset, config = list(), jobs_dir = ugplot_default_jobs_dir()) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required to start background jobs.", call. = FALSE)
  }
  status <- ugplot_create_job(dataset = dataset, config = config, jobs_dir = jobs_dir)
  lib_paths <- .libPaths()
  process <- callr::r_bg(
    func = function(job_id, jobs_dir, lib_paths) {
      .libPaths(lib_paths)
      library(ugplot)
      get("ugplot_run_job_from_dir", envir = asNamespace("ugplot"))(job_id, jobs_dir)
    },
    args = list(job_id = status$id, jobs_dir = jobs_dir, lib_paths = lib_paths),
    supervise = TRUE
  )
  ugplot_update_job_status(
    status$id,
    jobs_dir,
    pid = process$get_pid(),
    message = "Started background process"
  )
  list(job = ugplot_read_job_status(status$id, jobs_dir), process = process)
}
