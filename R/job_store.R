ugplot_default_jobs_dir <- function() {
  configured_dir <- Sys.getenv("UGPLOT_JOBS_DIR", unset = "")
  if (nzchar(configured_dir)) {
    return(normalizePath(configured_dir, mustWork = FALSE))
  }
  normalizePath(file.path(path.expand("~"), ".ugplot", "jobs"), mustWork = FALSE)
}

ugplot_new_job_id <- function() {
  random_part <- paste(sample(c(letters, LETTERS, 0:9), 12, replace = TRUE), collapse = "")
  paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", random_part)
}

ugplot_ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Could not create directory: ", path, call. = FALSE)
  }
  invisible(path)
}

ugplot_validate_job_id <- function(job_id) {
  if (!is.character(job_id) || length(job_id) != 1 || !grepl("^[A-Za-z0-9._-]+$", job_id)) {
    stop("Invalid job id.", call. = FALSE)
  }
  job_id
}

ugplot_job_dir <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  job_id <- ugplot_validate_job_id(job_id)
  file.path(jobs_dir, job_id)
}

ugplot_status_path <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  file.path(ugplot_job_dir(job_id, jobs_dir), "status.rds")
}

ugplot_result_path <- function(job_id, jobs_dir = ugplot_default_jobs_dir(), partial = FALSE) {
  file.path(ugplot_job_dir(job_id, jobs_dir), if (isTRUE(partial)) "partial-result.rds" else "result.rds")
}

ugplot_preview_result_path <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  file.path(ugplot_job_dir(job_id, jobs_dir), "preview-result.rds")
}

ugplot_best_model_path <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  file.path(ugplot_job_dir(job_id, jobs_dir), "best-model.rds")
}

ugplot_job_result_preview <- function(result) {
  if (!is.list(result)) {
    return(result)
  }
  preview <- list(
    results_table = result$results_table %||% data.frame(),
    final_summary = result$final_summary %||% NULL,
    partial = isTRUE(result$partial),
    updated_at = result$updated_at %||% as.character(Sys.time())
  )
  if (is.data.frame(preview$results_table)) {
    keep_columns <- intersect(
      c("Model", "R2", "Accuracy", "MAE", "RMSE", "dataset_seed", "training_seed", "Status", "Error"),
      names(preview$results_table)
    )
    preview$results_table <- preview$results_table[, keep_columns, drop = FALSE]
  }
  preview
}

ugplot_job_partial_result <- function(result) {
  if (!is.list(result)) {
    return(result)
  }
  partial_result <- result
  partial_result$best_model <- NULL
  partial_result$predictions <- NULL
  partial_result$partial_model_omitted <- TRUE
  partial_result
}

ugplot_attach_job_best_model <- function(result, status) {
  if (!is.list(result)) {
    return(result)
  }
  best_model_path <- status$best_model_path %||% ""
  if (nzchar(best_model_path) && file.exists(best_model_path)) {
    best_model <- tryCatch(readRDS(best_model_path), error = function(e) NULL)
    if (!is.null(best_model)) {
      result$best_model <- best_model
    }
  }
  result
}

ugplot_job_completed_run_keys <- function(result) {
  if (!is.list(result) || !is.data.frame(result$results_table)) {
    return(character(0))
  }
  rows <- result$results_table
  required_columns <- c("Model", "dataset_seed", "training_seed")
  if (!all(required_columns %in% names(rows))) {
    return(character(0))
  }
  keys <- paste(
    as.character(rows$Model),
    as.character(suppressWarnings(as.integer(rows$dataset_seed))),
    as.character(suppressWarnings(as.integer(rows$training_seed))),
    sep = "\r"
  )
  unique(keys[nzchar(keys) & !is.na(keys)])
}

ugplot_write_rds_atomic <- function(object, path) {
  ugplot_ensure_dir(dirname(path))
  tmp_path <- paste0(path, ".tmp-", Sys.getpid(), "-", as.integer(stats::runif(1, 1, 1e9)))
  saveRDS(object, tmp_path)
  if (!file.rename(tmp_path, path)) {
    unlink(tmp_path)
    stop("Could not write file: ", path, call. = FALSE)
  }
  invisible(path)
}

ugplot_read_rds_or_null <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path)
}

ugplot_process_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (is.na(pid) || pid <= 0) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    output <- tryCatch(
      suppressWarnings(system2("tasklist", c("/FI", paste0("PID eq ", pid), "/NH"), stdout = TRUE, stderr = FALSE)),
      error = function(e) character()
    )
    return(any(grepl(paste0("\\b", pid, "\\b"), output)))
  }
  result <- tryCatch(tools::pskill(pid, signal = 0), error = function(e) FALSE)
  isTRUE(result)
}

ugplot_terminate_process <- function(pid) {
  if (.Platform$OS.type == "windows") {
    system2("taskkill", c("/PID", as.character(as.integer(pid)), "/T", "/F"), stdout = FALSE, stderr = FALSE)
    return(invisible(TRUE))
  }
  tools::pskill(as.integer(pid), signal = tools::SIGTERM)
  Sys.sleep(0.5)
  if (ugplot_process_alive(pid)) {
    tools::pskill(as.integer(pid), signal = tools::SIGKILL)
  }
  invisible(TRUE)
}

ugplot_status_time <- function(value) {
  parsed <- tryCatch(
    as.POSIXct(value, format = "%Y-%m-%d %H:%M:%S %z"),
    error = function(e) as.POSIXct(NA)
  )
  if (is.na(parsed)) {
    parsed <- tryCatch(as.POSIXct(value), error = function(e) as.POSIXct(NA))
  }
  parsed
}

ugplot_job_timeout_seconds <- function(status) {
  timeout <- suppressWarnings(as.numeric(status$timeout %||% NA_real_))
  if (length(timeout) == 0 || is.na(timeout) || timeout <= 0) {
    return(NA_real_)
  }
  max(1, timeout)
}

ugplot_running_job_timed_out <- function(status) {
  if (!identical(status$state %||% "", "running")) {
    return(FALSE)
  }
  timeout <- ugplot_job_timeout_seconds(status)
  if (is.na(timeout)) {
    return(FALSE)
  }
  updated_at <- ugplot_status_time(status$updated_at %||% NA_character_)
  if (is.na(updated_at)) {
    return(FALSE)
  }
  watchdog_multiplier <- suppressWarnings(as.numeric(status$watchdog_timeout_multiplier %||% NA_real_))
  if (is.na(watchdog_multiplier) || watchdog_multiplier < 1) {
    watchdog_multiplier <- 3
  }
  grace <- max(300, min(1800, timeout * 0.5))
  age <- as.numeric(difftime(Sys.time(), updated_at, units = "secs"))
  is.finite(age) && age > ((timeout * watchdog_multiplier) + grace)
}

ugplot_create_job <- function(dataset, config = list(), jobs_dir = ugplot_default_jobs_dir(), type = "ml") {
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data.frame.", call. = FALSE)
  }
  if (!is.list(config)) {
    stop("config must be a list.", call. = FALSE)
  }

  ugplot_ensure_dir(jobs_dir)
  job_id <- ugplot_new_job_id()
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  ugplot_ensure_dir(job_dir)

  saveRDS(dataset, file.path(job_dir, "dataset.rds"))
  saveRDS(config, file.path(job_dir, "config.rds"))

  status <- list(
    id = job_id,
    name = config$job_name %||% "",
    type = type,
    state = "queued",
    progress = 0,
    message = "Queued",
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    pid = NA_integer_,
    error = NULL,
    result_path = NULL,
    partial_result_path = NULL,
    timeout = suppressWarnings(as.numeric(config$timeout %||% NA_real_)),
    watchdog_timeout_multiplier = suppressWarnings(as.numeric(config$watchdog_timeout_multiplier %||% 3))
  )
  ugplot_write_job_status(job_id, status, jobs_dir)
  status
}

ugplot_read_job_status <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  if (is.null(status)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }
  ugplot_refresh_job_status(status, jobs_dir)
}

ugplot_job_resumable <- function(status, jobs_dir = ugplot_default_jobs_dir()) {
  if (!is.list(status) || is.null(status$id)) {
    return(FALSE)
  }
  state <- status$state %||% ""
  if (state %in% c("queued", "running", "finished")) {
    return(FALSE)
  }
  job_dir <- ugplot_job_dir(status$id, jobs_dir)
  file.exists(file.path(job_dir, "dataset.rds")) && file.exists(file.path(job_dir, "config.rds"))
}

ugplot_job_config_summary <- function(status, jobs_dir = ugplot_default_jobs_dir()) {
  empty_summary <- list(target = "", models = "")
  if (!is.list(status) || is.null(status$id)) {
    return(empty_summary)
  }
  config_path <- file.path(ugplot_job_dir(status$id, jobs_dir), "config.rds")
  if (!file.exists(config_path)) {
    return(empty_summary)
  }
  config <- tryCatch(readRDS(config_path), error = function(e) list())
  models <- config$models %||% config$model_names %||% character(0)
  if (identical(config$type %||% "", "geo") || identical(config$runner %||% "", "ugplot_run_geo_pipeline_job")) {
    return(list(
      target = as.character(config$accession %||% ""),
      models = paste(c(config$matrix_source %||% "", config$target_column %||% ""), collapse = " / ")
    ))
  }
  list(
    target = as.character(config$target %||% config$target_name %||% ""),
    models = paste(as.character(models), collapse = ", ")
  )
}

ugplot_write_job_status <- function(job_id, status, jobs_dir = ugplot_default_jobs_dir()) {
  status$id <- job_id
  status$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ugplot_write_rds_atomic(status, ugplot_status_path(job_id, jobs_dir))
  invisible(status)
}

ugplot_update_job_status <- function(job_id, jobs_dir = ugplot_default_jobs_dir(), ...) {
  status <- ugplot_read_job_status(job_id, jobs_dir)
  updates <- list(...)
  for (name in names(updates)) {
    status[[name]] <- updates[[name]]
  }
  ugplot_write_job_status(job_id, status, jobs_dir)
}

ugplot_write_job_partial_result <- function(job_id, result, jobs_dir = ugplot_default_jobs_dir()) {
  partial_path <- ugplot_result_path(job_id, jobs_dir, partial = TRUE)
  preview_path <- ugplot_preview_result_path(job_id, jobs_dir)
  best_model_path <- ugplot_best_model_path(job_id, jobs_dir)
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  completed_keys <- unique(c(
    status$resume_completed_keys %||% character(0),
    ugplot_job_completed_run_keys(result)
  ))
  best_model_signature <- paste(
    result$best_model_name %||% "",
    result$final_summary$dataset_seed %||% "",
    result$final_summary$training_seed %||% "",
    sep = "\r"
  )
  best_model_updates <- list()
  if (!is.null(result$best_model) &&
      nzchar(best_model_signature) &&
      !identical(status$best_model_signature %||% "", best_model_signature)) {
    ugplot_write_rds_atomic(result$best_model, best_model_path)
    best_model_updates$best_model_path <- best_model_path
    best_model_updates$best_model_signature <- best_model_signature
  }
  ugplot_write_rds_atomic(ugplot_job_partial_result(result), partial_path)
  ugplot_write_rds_atomic(ugplot_job_result_preview(result), preview_path)
  status_updates <- c(
    list(
      partial_result_path = partial_path,
      preview_result_path = preview_path,
      resume_completed_keys = completed_keys,
      partial_saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ),
    best_model_updates
  )
  do.call(ugplot_update_job_status, c(list(job_id = job_id, jobs_dir = jobs_dir), status_updates))
  invisible(partial_path)
}

ugplot_stop_job <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  if (is.null(status)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }
  state <- status$state %||% ""
  pid <- suppressWarnings(as.integer(status$pid %||% NA_integer_))

  if (state %in% c("finished", "failed", "stopped")) {
    return(status)
  }

  if (!is.na(pid) && ugplot_process_alive(pid)) {
    ugplot_terminate_process(pid)
  }

  partial_path <- status$partial_result_path %||% ugplot_result_path(job_id, jobs_dir, partial = TRUE)
  has_partial <- !is.null(partial_path) && file.exists(partial_path)
  ugplot_update_job_status(
    job_id,
    jobs_dir,
    state = "stopped",
    message = if (has_partial) "Stopped; partial result is available" else "Stopped",
    error = NULL,
    result_path = if (has_partial) partial_path else status$result_path
  )
  ugplot_read_job_status(job_id, jobs_dir)
}

ugplot_delete_job <- function(job_id, jobs_dir = ugplot_default_jobs_dir(), force = FALSE) {
  job_id <- ugplot_validate_job_id(job_id)
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  if (!dir.exists(job_dir)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }

  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  state <- status$state %||% ""
  pid <- suppressWarnings(as.integer(status$pid %||% NA_integer_))
  is_active <- state %in% c("queued", "running") && !is.na(pid) && ugplot_process_alive(pid)
  if (is_active && !isTRUE(force)) {
    stop("Stop the job before deleting it.", call. = FALSE)
  }
  if (is_active && isTRUE(force)) {
    ugplot_terminate_process(pid)
  }

  removed <- unlink(job_dir, recursive = TRUE, force = TRUE)
  if (!identical(removed, 0L) || dir.exists(job_dir)) {
    stop("Could not delete job: ", job_id, call. = FALSE)
  }
  list(id = job_id, deleted = TRUE)
}

ugplot_refresh_job_status <- function(status, jobs_dir = ugplot_default_jobs_dir()) {
  state <- status$state %||% ""
  pid <- status$pid %||% NA_integer_
  should_check_pid <- state %in% c("queued", "running") && !is.na(suppressWarnings(as.integer(pid)))
  if (!should_check_pid) {
    status$resumable <- ugplot_job_resumable(status, jobs_dir)
    status$config_summary <- ugplot_job_config_summary(status, jobs_dir)
    return(status)
  }

  if (ugplot_process_alive(pid)) {
    if (!ugplot_running_job_timed_out(status)) {
      status$resumable <- FALSE
      status$config_summary <- ugplot_job_config_summary(status, jobs_dir)
      return(status)
    }
    ugplot_terminate_process(pid)
    partial_path <- status$partial_result_path %||% ugplot_result_path(status$id, jobs_dir, partial = TRUE)
    has_partial <- !is.null(partial_path) && file.exists(partial_path)
    status$state <- if (has_partial) "stopped" else "failed"
    status$message <- if (has_partial) "Timed out; partial result is available" else "Timed out"
    status$error <- paste0("The job process exceeded the configured timeout without a progress update.")
    if (has_partial) {
      status$result_path <- partial_path
    }
    ugplot_write_job_status(status$id, status, jobs_dir)
    status$resumable <- ugplot_job_resumable(status, jobs_dir)
    status$config_summary <- ugplot_job_config_summary(status, jobs_dir)
    return(status)
  }

  status$state <- "failed"
  status$message <- "Background process stopped before finishing"
  status$error <- "The job process is no longer running. The server may have restarted or crashed."
  status$progress <- status$progress %||% 0
  ugplot_write_job_status(status$id, status, jobs_dir)
  status$resumable <- ugplot_job_resumable(status, jobs_dir)
  status$config_summary <- ugplot_job_config_summary(status, jobs_dir)
  status
}

ugplot_list_jobs <- function(jobs_dir = ugplot_default_jobs_dir()) {
  if (!dir.exists(jobs_dir)) {
    return(data.frame())
  }
  job_ids <- basename(list.dirs(jobs_dir, full.names = TRUE, recursive = FALSE))
  statuses <- lapply(job_ids, function(job_id) {
    tryCatch(ugplot_read_job_status(job_id, jobs_dir), error = function(e) NULL)
  })
  statuses <- Filter(Negate(is.null), statuses)
  if (length(statuses) == 0) {
    return(data.frame())
  }
  rows <- lapply(statuses, function(status) {
    data.frame(
      id = status$id %||% NA_character_,
      name = status$name %||% NA_character_,
      type = status$type %||% NA_character_,
      state = status$state %||% NA_character_,
      progress = status$progress %||% NA_real_,
      message = status$message %||% NA_character_,
      target = status$config_summary$target %||% NA_character_,
      models = status$config_summary$models %||% NA_character_,
      created_at = status$created_at %||% NA_character_,
      updated_at = status$updated_at %||% NA_character_,
      pid = status$pid %||% NA_integer_,
      resumable = isTRUE(status$resumable %||% ugplot_job_resumable(status, jobs_dir)),
      stringsAsFactors = FALSE
    )
  })
  jobs <- do.call(rbind, rows)
  jobs[order(jobs$created_at, decreasing = TRUE), , drop = FALSE]
}

ugplot_append_job_log <- function(job_id, message, jobs_dir = ugplot_default_jobs_dir()) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), " ", message)
  cat(line, "\n", file = file.path(ugplot_job_dir(job_id, jobs_dir), "log.txt"), append = TRUE)
  invisible(line)
}

ugplot_read_job_log <- function(job_id, jobs_dir = ugplot_default_jobs_dir(), max_lines = 200L) {
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  log_paths <- c(
    file.path(job_dir, "log.txt"),
    file.path(job_dir, "stdout.log"),
    file.path(job_dir, "stderr.log"),
    utils::tail(sort(list.files(file.path(job_dir, "model-logs"), pattern = "\\.log$", full.names = TRUE)), 12)
  )
  sections <- lapply(log_paths[file.exists(log_paths)], function(path) {
    lines <- readLines(path, warn = FALSE)
    lines <- utils::tail(lines, max(1L, as.integer(max_lines)))
    c(paste0("== ", basename(path), " =="), lines)
  })
  paste(unlist(sections, use.names = FALSE), collapse = "\n")
}

ugplot_read_job_result <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_job_status(job_id, jobs_dir)
  result_path <- status$result_path %||% status$partial_result_path
  if (is.null(result_path) || !file.exists(result_path)) {
    stop("Result is not available for job: ", job_id, call. = FALSE)
  }
  ugplot_attach_job_best_model(readRDS(result_path), status)
}

ugplot_read_job_preview_result <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_job_status(job_id, jobs_dir)
  preview_path <- status$preview_result_path %||% ugplot_preview_result_path(job_id, jobs_dir)
  if (!is.null(preview_path) && file.exists(preview_path)) {
    return(readRDS(preview_path))
  }
  ugplot_job_result_preview(ugplot_read_job_result(job_id, jobs_dir))
}

ugplot_read_job_bundle <- function(job_id, jobs_dir = ugplot_default_jobs_dir(), allow_active = FALSE) {
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  if (!dir.exists(job_dir)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }
  status <- ugplot_read_job_status(job_id, jobs_dir)
  if (!isTRUE(allow_active) && status$state %in% c("queued", "running")) {
    stop("Full job bundle is not available while the job is active. Use preview, or stop/wait before Load.", call. = FALSE)
  }
  dataset_path <- file.path(job_dir, "dataset.rds")
  config_path <- file.path(job_dir, "config.rds")
  if (!file.exists(dataset_path) || !file.exists(config_path)) {
    stop("Job dataset/config is not available for job: ", job_id, call. = FALSE)
  }
  list(
    id = job_id,
    status = status,
    dataset = readRDS(dataset_path),
    config = readRDS(config_path),
    result = tryCatch(ugplot_read_job_result(job_id, jobs_dir), error = function(e) NULL)
  )
}
