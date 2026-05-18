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
  if (exists("ugplot_build_version", mode = "function", inherits = TRUE)) {
    ugplot_append_job_log(job_id, paste0("Runner build version: ", ugplot_build_version()), jobs_dir)
  }
  ugplot_append_job_log(job_id, paste0("Runner pid: ", Sys.getpid()), jobs_dir)

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
  partial_callback <- function(result) {
    ugplot_write_job_partial_result(job_id, result, jobs_dir)
  }

  tryCatch({
    runner_fun <- get(runner, mode = "function")
    runner_args <- list(dataset = dataset, config = config, progress_callback = progress_callback)
    if ("partial_callback" %in% names(formals(runner_fun))) {
      runner_args$partial_callback <- partial_callback
    }
    result <- do.call(runner_fun, runner_args)
    result_path <- ugplot_result_path(job_id, jobs_dir)
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

ugplot_launch_background_job <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required to start background jobs.", call. = FALSE)
  }
  lib_paths <- .libPaths()
  source_dir <- if (file.exists(file.path(getwd(), "R", "app.R"))) normalizePath(getwd(), mustWork = FALSE) else NULL
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  stdout_path <- file.path(job_dir, "stdout.log")
  stderr_path <- file.path(job_dir, "stderr.log")
  process <- callr::r_bg(
    func = function(job_id, jobs_dir, lib_paths, source_dir) {
      mark_startup_failed <- function(message) {
        status_path <- file.path(jobs_dir, job_id, "status.rds")
        if (file.exists(status_path)) {
          status <- readRDS(status_path)
          status$state <- "failed"
          status$message <- "Failed"
          status$error <- message
          status$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
          saveRDS(status, status_path)
        }
        log_path <- file.path(jobs_dir, job_id, "log.txt")
        cat(
          paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), " Startup failed: ", message, "\n"),
          file = log_path,
          append = TRUE
        )
      }

      .libPaths(lib_paths)
      tryCatch({
        if (!is.null(source_dir) && file.exists(file.path(source_dir, "R", "app.R"))) {
          source(file.path(source_dir, "R", "00_version.R"), local = .GlobalEnv)
          source(file.path(source_dir, "R", "app.R"), local = .GlobalEnv)
          source(file.path(source_dir, "R", "job_store.R"), local = .GlobalEnv)
          source(file.path(source_dir, "R", "ml_runner.R"), local = .GlobalEnv)
          source(file.path(source_dir, "R", "job_process.R"), local = .GlobalEnv)
          ugplot_run_job_from_dir(job_id, jobs_dir)
        } else {
          library(ugplot)
          get("ugplot_run_job_from_dir", envir = asNamespace("ugplot"))(job_id, jobs_dir)
        }
      }, error = function(e) {
        mark_startup_failed(conditionMessage(e))
        stop(e)
      })
    },
    args = list(job_id = job_id, jobs_dir = jobs_dir, lib_paths = lib_paths, source_dir = source_dir),
    supervise = TRUE,
    stdout = stdout_path,
    stderr = stderr_path
  )
  ugplot_update_job_status(
    job_id,
    jobs_dir,
    pid = process$get_pid(),
    message = "Started background process"
  )
  list(job = ugplot_read_job_status(job_id, jobs_dir), process = process)
}

ugplot_start_background_job <- function(dataset, config = list(), jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_create_job(dataset = dataset, config = config, jobs_dir = jobs_dir)
  ugplot_launch_background_job(status$id, jobs_dir)
}

ugplot_resume_background_job <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  if (is.null(status)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  if (!file.exists(file.path(job_dir, "dataset.rds")) || !file.exists(file.path(job_dir, "config.rds"))) {
    stop("Job dataset/config is not available for resume.", call. = FALSE)
  }
  config_path <- file.path(job_dir, "config.rds")
  config <- readRDS(config_path)
  partial_path <- status$partial_result_path %||% ugplot_result_path(job_id, jobs_dir, partial = TRUE)
  resume_result <- NULL
  if (!is.null(partial_path) && file.exists(partial_path)) {
    config$resume_result_path <- partial_path
    resume_result <- tryCatch(readRDS(partial_path), error = function(e) NULL)
    if (is.null(resume_result)) {
      preview_path <- status$preview_result_path %||% ugplot_preview_result_path(job_id, jobs_dir)
      if (!is.null(preview_path) && file.exists(preview_path)) {
        resume_result <- tryCatch(readRDS(preview_path), error = function(e) NULL)
      }
    }
    if (is.list(resume_result) && is.data.frame(resume_result$results_table)) {
      config$resume_result <- ugplot_job_result_preview(resume_result)
    }
    resume_keys <- unique(c(
      status$resume_completed_keys %||% character(0),
      ugplot_job_completed_run_keys(resume_result)
    ))
    if (length(resume_keys) > 0) {
      config$resume_completed_keys <- resume_keys
    }
  } else if (length(status$resume_completed_keys %||% character(0)) > 0) {
    config$resume_completed_keys <- status$resume_completed_keys
  }
  saveRDS(config, config_path)
  pid <- suppressWarnings(as.integer(status$pid %||% NA_integer_))
  if ((status$state %||% "") %in% c("queued", "running") && !is.na(pid) && ugplot_process_alive(pid)) {
    stop("Job is already running.", call. = FALSE)
  }
  status$state <- "queued"
  status$message <- "Queued for resume"
  status$error <- NULL
  status$pid <- NA_integer_
  status$resumable <- FALSE
  ugplot_write_job_status(job_id, status, jobs_dir)
  ugplot_launch_background_job(job_id, jobs_dir)
}
