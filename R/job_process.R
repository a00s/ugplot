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
  config$jobs_dir <- jobs_dir
  config$job_dir <- job_dir
  config$model_log_dir <- file.path(job_dir, "model-logs")
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

  progress_callback <- function(progress = NULL, message = NULL, current_run = NULL,
                                distributed_state = NULL, stage_progress = NULL,
                                phase = NULL, ...) {
    updates <- list()
    if (!is.null(progress)) {
      updates$progress <- max(0, min(1, as.numeric(progress)))
    }
    if (!is.null(message)) {
      updates$message <- as.character(message)
      ugplot_append_job_log(job_id, as.character(message), jobs_dir)
    }
    if (is.list(current_run)) {
      if (isTRUE(current_run$clear)) {
        updates$current_run_key <- NA_character_
        updates$current_model <- NA_character_
        updates$current_dataset_seed <- NA_integer_
        updates$current_training_seed <- NA_integer_
        updates$current_run_started_at <- NA_character_
      } else {
        updates$current_run_key <- current_run$key %||% NA_character_
        updates$current_model <- current_run$model %||% NA_character_
        updates$current_dataset_seed <- current_run$dataset_seed %||% NA_integer_
        updates$current_training_seed <- current_run$training_seed %||% NA_integer_
        updates$current_run_started_at <- current_run$started_at %||% NA_character_
      }
    }
    if (is.list(distributed_state)) {
      updates$distributed_state <- distributed_state
    }
    if (is.list(stage_progress)) {
      updates$stage_progress <- stage_progress
      cpg_count <- suppressWarnings(as.integer(stage_progress$matrix_cpgs_total %||% NA_integer_))
      if (length(cpg_count) > 0L && is.finite(cpg_count) && cpg_count > 0L) {
        updates$cpgs <- cpg_count
      }
    }
    if (!is.null(phase)) {
      updates$current_phase <- as.character(phase)
    }
    do.call(ugplot_update_job_status, c(list(job_id = job_id, jobs_dir = jobs_dir), updates))
    if (identical(runner, "ugplot_run_geo_pipeline_job") &&
        exists("ugplot_refresh_job_discovery_snapshot", mode = "function", inherits = TRUE)) {
      try(ugplot_refresh_job_discovery_snapshot(job_id, jobs_dir), silent = TRUE)
    }
    safe_boundary <- identical(runner, "ugplot_run_placeholder_job") ||
      isTRUE(current_run$clear %||% FALSE) ||
      grepl("^(Finished|Not enough data)", as.character(message %||% ""))
    if (!identical(runner, "ugplot_run_geo_pipeline_job") &&
        isTRUE(safe_boundary) && ugplot_job_drain_requested(job_dir)) {
      ugplot_signal_job_drained()
    }
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
    if (identical(runner, "ugplot_run_geo_pipeline_job") &&
        exists("ugplot_refresh_job_discovery_snapshot", mode = "function", inherits = TRUE)) {
      try(ugplot_refresh_job_discovery_snapshot(job_id, jobs_dir), silent = TRUE)
    }
    invisible(result)
  }, ugplot_job_drained = function(e) {
    partial_path <- ugplot_result_path(job_id, jobs_dir, partial = TRUE)
    has_partial <- file.exists(partial_path)
    ugplot_append_job_log(job_id, conditionMessage(e), jobs_dir)
    ugplot_update_job_status(
      job_id, jobs_dir,
      state = "stopped",
      message = conditionMessage(e),
      error = NULL,
      result_path = if (has_partial) partial_path else NULL,
      drained_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    )
    invisible(NULL)
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
          source(file.path(source_dir, "R", "geo_pipeline_runner.R"), local = .GlobalEnv)
          source(file.path(source_dir, "R", "server_api.R"), local = .GlobalEnv)
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
    cleanup = FALSE,
    poll_connection = FALSE,
    stdout = stdout_path,
    stderr = stderr_path
  )
  ugplot_update_job_status(
    job_id,
    jobs_dir,
    pid = process$get_pid(),
    message = "Started background process"
  )
  startup_deadline <- Sys.time() + 60
  repeat {
    startup_status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
    if (is.list(startup_status) && identical(startup_status$state %||% "", "running")) {
      break
    }
    if (!process$is_alive() || Sys.time() >= startup_deadline) {
      break
    }
    Sys.sleep(0.1)
  }
  list(job = ugplot_read_job_status(job_id, jobs_dir), process = process)
}

ugplot_start_background_job <- function(dataset, config = list(), jobs_dir = ugplot_default_jobs_dir()) {
  request_id <- as.character(config$request_id %||% "")
  if (nzchar(request_id)) {
    existing <- ugplot_find_job_by_request_id(request_id, jobs_dir)
    if (is.list(existing)) {
      if ((existing$state %||% "") %in% c("failed", "stopped")) {
        resumed <- ugplot_resume_background_job(existing$id, jobs_dir)
        resumed$reused <- TRUE
        resumed$restarted <- TRUE
        return(resumed)
      }
      return(list(job = existing, process = NULL, reused = TRUE))
    }
  }
  status <- ugplot_create_job(dataset = dataset, config = config, jobs_dir = jobs_dir, type = config$type %||% "ml")
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
  config$job_dir <- job_dir
  config$model_log_dir <- file.path(job_dir, "model-logs")
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
    if (is.list(resume_result) &&
        identical(resume_result$kind %||% "", "geo_pipeline") &&
        !nzchar(as.character(config$target_column %||% "")) &&
        nzchar(as.character(resume_result$target_column %||% ""))) {
      config$target_column <- as.character(resume_result$target_column)
    }
    if (is.list(resume_result) && identical(resume_result$kind %||% "", "geo_pipeline")) {
      config$resume_result <- resume_result
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
  if (identical(config$runner %||% "", "ugplot_run_ml_job")) {
    config$use_callr_timeout <- TRUE
    config$watchdog_timeout_multiplier <- suppressWarnings(as.numeric(config$watchdog_timeout_multiplier %||% 3))
    if (is.na(config$watchdog_timeout_multiplier) || config$watchdog_timeout_multiplier < 1) {
      config$watchdog_timeout_multiplier <- 3
    }
    status$watchdog_timeout_multiplier <- config$watchdog_timeout_multiplier
    failed_run_key <- as.character(status$current_run_key %||% "")
    if (nzchar(failed_run_key) && !(failed_run_key %in% (config$resume_completed_keys %||% character(0)))) {
      config$resume_failed_runs <- c(
        config$resume_failed_runs %||% list(),
        list(list(
          key = failed_run_key,
          model = as.character(status$current_model %||% ""),
          dataset_seed = suppressWarnings(as.integer(status$current_dataset_seed %||% NA_integer_)),
          training_seed = suppressWarnings(as.integer(status$current_training_seed %||% NA_integer_)),
          error = paste0(
            "Previous remote process stopped while this run was active",
            if (nzchar(status$error %||% "")) paste0(": ", status$error) else ""
          )
        ))
      )
    }
  }
  saveRDS(config, config_path)
  pid <- suppressWarnings(as.integer(status$pid %||% NA_integer_))
  if ((status$state %||% "") %in% c("queued", "running", "draining") && !is.na(pid) && ugplot_process_alive(pid)) {
    stop("Job is already running.", call. = FALSE)
  }
  unlink(ugplot_drain_request_path(job_id, jobs_dir), force = TRUE)
  status$state <- "queued"
  status$message <- "Queued for resume"
  status$error <- NULL
  status$pid <- NA_integer_
  status$resumable <- FALSE
  ugplot_write_job_status(job_id, status, jobs_dir)
  ugplot_launch_background_job(job_id, jobs_dir)
}

ugplot_auto_resume_crashed_jobs <- function(jobs_dir = ugplot_default_jobs_dir()) {
  if (!dir.exists(jobs_dir)) {
    return(invisible(list()))
  }
  job_ids <- basename(list.dirs(jobs_dir, full.names = TRUE, recursive = FALSE))
  resumed <- list()
  for (job_id in job_ids) {
    status <- tryCatch(ugplot_read_job_status(job_id, jobs_dir), error = function(e) NULL)
    if (!is.list(status) || !isTRUE(status$resumable)) {
      next
    }
    if (!identical(status$state %||% "", "failed") ||
        !identical(status$message %||% "", "Background process stopped before finishing")) {
      next
    }
    config_path <- file.path(ugplot_job_dir(job_id, jobs_dir), "config.rds")
    config <- tryCatch(readRDS(config_path), error = function(e) list())
    # Distributed worker jobs are owned by their coordinator. Resuming every
    # stale worker checkpoint when a worker server restarts can launch many
    # CPU-sized clusters at once. The coordinator polls the assigned job and
    # explicitly resumes only the checkpoint that is still part of its
    # manifest.
    if (isTRUE(status$internal_worker_task) || isTRUE(config$internal_worker_task)) {
      next
    }
    resumable_runners <- c(
      "ugplot_run_ml_job",
      "ugplot_run_geo_pipeline_job",
      "ugplot_run_geo_complete_group_job",
      "ugplot_run_geo_screen_group_job"
    )
    if (!(config$runner %||% "") %in% resumable_runners) {
      next
    }
    if (identical(config$auto_resume_crashed_jobs, FALSE)) {
      next
    }
    recency_seconds <- suppressWarnings(as.numeric(config$auto_resume_crash_recency_seconds %||% 300))
    if (!is.finite(recency_seconds) || recency_seconds < 1) recency_seconds <- 300
    crashed_at <- ugplot_status_time(status$updated_at %||% NA_character_)
    crash_age <- if (is.na(crashed_at)) Inf else as.numeric(difftime(Sys.time(), crashed_at, units = "secs"))
    if (!is.finite(crash_age) || crash_age > recency_seconds) {
      next
    }
    max_attempts <- suppressWarnings(as.integer(config$auto_resume_max_attempts %||% 5L))
    if (is.na(max_attempts) || max_attempts < 1L) {
      max_attempts <- 5L
    }
    attempt_count <- suppressWarnings(as.integer(status$auto_resume_count %||% 0L))
    if (is.na(attempt_count)) {
      attempt_count <- 0L
    }
    if (attempt_count >= max_attempts) {
      next
    }
    status$auto_resume_count <- attempt_count + 1L
    status$auto_resume_last_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ugplot_write_job_status(job_id, status, jobs_dir)
    ugplot_append_job_log(
      job_id,
      paste0("Auto-resuming crashed job attempt ", status$auto_resume_count, "/", max_attempts),
      jobs_dir
    )
    resumed[[job_id]] <- tryCatch(ugplot_resume_background_job(job_id, jobs_dir), error = function(e) {
      ugplot_append_job_log(job_id, paste0("Auto-resume failed: ", conditionMessage(e)), jobs_dir)
      NULL
    })
  }
  invisible(resumed)
}

ugplot_monitor_active_jobs <- function(jobs_dir = ugplot_default_jobs_dir(), state = new.env(parent = emptyenv())) {
  if (!dir.exists(jobs_dir) || .Platform$OS.type == "windows") {
    return(invisible(list()))
  }
  job_ids <- basename(list.dirs(jobs_dir, full.names = TRUE, recursive = FALSE))
  samples <- list()
  for (job_id in job_ids) {
    status <- tryCatch(ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir)), error = function(e) NULL)
    if (!is.list(status) || !(status$state %||% "") %in% c("queued", "running", "draining")) {
      next
    }
    previous <- if (exists(job_id, envir = state, inherits = FALSE)) get(job_id, envir = state) else NULL
    sample <- tryCatch(ugplot_sample_job_resources(status, previous, jobs_dir), error = function(e) NULL)
    if (!is.data.frame(sample) || nrow(sample) == 0) {
      next
    }
    try(ugplot_append_job_resources(job_id, sample, jobs_dir), silent = TRUE)
    current <- list(
      pid = sample$pid[[1]],
      process_cpu_ticks = sample$process_cpu_ticks[[1]],
      system_cpu_ticks = sample$system_cpu_ticks[[1]],
      vm_oom_kill = sample$vm_oom_kill[[1]],
      cgroup_oom_kill = sample$cgroup_oom_kill[[1]]
    )
    assign(job_id, current, envir = state)
    samples[[job_id]] <- sample
  }
  invisible(samples)
}

ugplot_start_auto_resume_monitor <- function(jobs_dir = ugplot_default_jobs_dir(),
                                             interval = 30,
                                             source_dir = NULL,
                                             lib_paths = .libPaths()) {
  interval <- suppressWarnings(as.numeric(interval %||% 30))
  if (is.na(interval) || interval <= 0) {
    return(NULL)
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required to start the auto-resume monitor.", call. = FALSE)
  }
  ugplot_ensure_dir(jobs_dir)
  callr::r_bg(
    func = function(jobs_dir, interval, source_dir, lib_paths) {
      `%||%` <- function(lhs, rhs) {
        if (is.null(lhs) || length(lhs) == 0) rhs else lhs
      }
      assign("%||%", `%||%`, envir = .GlobalEnv)
      .libPaths(lib_paths)
      if (!is.null(source_dir) && file.exists(file.path(source_dir, "R", "job_process.R"))) {
        source(file.path(source_dir, "R", "00_version.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "server_control.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "job_store.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "ml_runner.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "job_process.R"), local = .GlobalEnv)
      } else {
        library(ugplot)
      }
      monitor_state <- new.env(parent = emptyenv())
      repeat {
        try(ugplot_monitor_active_jobs(jobs_dir, monitor_state), silent = TRUE)
        try(ugplot_auto_resume_crashed_jobs(jobs_dir), silent = TRUE)
        Sys.sleep(interval)
      }
    },
    args = list(
      jobs_dir = jobs_dir,
      interval = max(1, interval),
      source_dir = source_dir,
      lib_paths = lib_paths
    ),
    supervise = TRUE,
    stdout = file.path(jobs_dir, "auto-resume-monitor.stdout.log"),
    stderr = file.path(jobs_dir, "auto-resume-monitor.stderr.log")
  )
}
