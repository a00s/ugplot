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
    result_path = NULL
  )
  ugplot_write_job_status(job_id, status, jobs_dir)
  status
}

ugplot_read_job_status <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  if (is.null(status)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }
  status
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
      created_at = status$created_at %||% NA_character_,
      updated_at = status$updated_at %||% NA_character_,
      pid = status$pid %||% NA_integer_,
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

ugplot_read_job_result <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_job_status(job_id, jobs_dir)
  result_path <- status$result_path
  if (is.null(result_path) || !file.exists(result_path)) {
    stop("Result is not available for job: ", job_id, call. = FALSE)
  }
  readRDS(result_path)
}
