ugplot_collaboration_dir <- function(jobs_dir = ugplot_default_jobs_dir()) {
  file.path(jobs_dir, "collaboration")
}

ugplot_collaboration_task_dir <- function(task_id, jobs_dir = ugplot_default_jobs_dir()) {
  task_id <- gsub("[^A-Za-z0-9._-]", "_", as.character(task_id %||% ""))
  if (!nzchar(task_id)) stop("A collaboration task ID is required.", call. = FALSE)
  file.path(ugplot_collaboration_dir(jobs_dir), task_id)
}

ugplot_collaboration_lock_stale <- function(lock_dir, legacy_stale_seconds = 60) {
  owner_path <- file.path(lock_dir, "owner.rds")
  owner <- if (file.exists(owner_path)) {
    tryCatch(readRDS(owner_path), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.list(owner)) {
    owner_pid <- suppressWarnings(as.integer(owner$pid %||% NA_integer_))
    if (!is.na(owner_pid)) return(!ugplot_process_alive(owner_pid))
  }
  info <- suppressWarnings(file.info(lock_dir))
  if (nrow(info) == 0L || is.na(info$mtime[[1]])) return(FALSE)
  age <- as.numeric(difftime(Sys.time(), info$mtime[[1]], units = "secs"))
  is.finite(age) && age >= legacy_stale_seconds
}

ugplot_collaboration_with_lock <- function(task_dir, code, timeout_seconds = 5,
                                           legacy_stale_seconds = 60) {
  ugplot_ensure_dir(task_dir)
  lock_dir <- file.path(task_dir, ".lock")
  deadline <- Sys.time() + timeout_seconds
  repeat {
    if (dir.create(lock_dir, showWarnings = FALSE)) {
      saveRDS(
        list(pid = Sys.getpid(), acquired_at = Sys.time()),
        file.path(lock_dir, "owner.rds")
      )
      break
    }
    if (ugplot_collaboration_lock_stale(lock_dir, legacy_stale_seconds)) {
      stale_dir <- paste0(
        lock_dir, ".stale-", Sys.getpid(), "-",
        paste(sample(c(letters, 0:9), 8L, TRUE), collapse = "")
      )
      if (file.rename(lock_dir, stale_dir)) {
        unlink(stale_dir, recursive = TRUE, force = TRUE)
        next
      }
    }
    if (Sys.time() >= deadline) stop("Collaboration task is busy.", call. = FALSE)
    Sys.sleep(0.05)
  }
  on.exit(unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)
  force(code)
}

ugplot_collaboration_read_task <- function(task_id, jobs_dir = ugplot_default_jobs_dir()) {
  path <- file.path(ugplot_collaboration_task_dir(task_id, jobs_dir), "task.rds")
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

ugplot_collaboration_write_task <- function(task, jobs_dir = ugplot_default_jobs_dir()) {
  task_dir <- ugplot_collaboration_task_dir(task$task_id, jobs_dir)
  ugplot_ensure_dir(task_dir)
  ugplot_write_rds_atomic(task, file.path(task_dir, "task.rds"))
  invisible(task)
}

ugplot_collaboration_publish_task <- function(task_id, parent_job_id, payload,
                                              requirements = list(), mission = list(),
                                              jobs_dir = ugplot_default_jobs_dir()) {
  task_dir <- ugplot_collaboration_task_dir(task_id, jobs_dir)
  ugplot_collaboration_with_lock(task_dir, {
    existing <- ugplot_collaboration_read_task(task_id, jobs_dir)
    if (is.list(existing) && identical(existing$state %||% "", "pending")) {
      if (identical(existing$requirements %||% list(), requirements) &&
          identical(existing$mission %||% list(), mission) &&
          file.exists(existing$payload_path %||% "")) {
        return(existing)
      }
      ugplot_write_rds_atomic(payload, existing$payload_path)
      existing$requirements <- requirements
      existing$mission <- mission
      existing$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
      ugplot_collaboration_write_task(existing, jobs_dir)
      return(existing)
    }
    if (is.list(existing) && existing$state %in% c("leased", "completed")) return(existing)
    payload_path <- file.path(task_dir, "payload.rds")
    ugplot_write_rds_atomic(payload, payload_path)
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    task <- list(
      task_id = as.character(task_id),
      parent_job_id = as.character(parent_job_id),
      state = "pending",
      created_at = now,
      updated_at = now,
      requirements = requirements,
      mission = mission,
      payload_path = payload_path,
      lease_id = "",
      client_id = "",
      scientist_name = "",
      client_progress = 0,
      client_message = "",
      client_candidate = "",
      lease_expires_at = as.POSIXct(NA),
      heartbeat_at = as.POSIXct(NA),
      completed_at = as.POSIXct(NA),
      result_path = ""
    )
    ugplot_collaboration_write_task(task, jobs_dir)
    task
  })
}

ugplot_collaboration_models_compatible <- function(requirements, capabilities) {
  required <- unique(as.character(requirements$models %||% character(0)))
  available <- unique(as.character(capabilities$models %||% character(0)))
  required <- required[nzchar(required)]
  all(required %in% available)
}

ugplot_collaboration_parent_job_status <- function(task,
                                                   jobs_dir = ugplot_default_jobs_dir()) {
  parent_job_id <- trimws(as.character(task$parent_job_id %||% ""))
  if (!nzchar(parent_job_id)) {
    return(list(active = FALSE, state = "missing", parent_job_id = ""))
  }
  status <- tryCatch(
    ugplot_read_rds_or_null(ugplot_status_path(parent_job_id, jobs_dir)),
    error = function(e) NULL
  )
  state <- if (is.list(status)) as.character(status$state %||% "unknown") else "missing"
  pid <- if (is.list(status)) suppressWarnings(as.integer(status$pid %||% NA_integer_)) else NA_integer_
  list(
    active = identical(state, "running") && !is.na(pid) && ugplot_process_alive(pid),
    state = state,
    parent_job_id = parent_job_id
  )
}

ugplot_collaboration_required_models <- function(config) {
  declared <- unique(as.character(config$collaboration_required_models %||% character(0)))
  declared <- declared[nzchar(declared)]
  if (length(declared) > 0L) return(declared)
  models <- unique(as.character(config$models %||% character(0)))
  models <- models[nzchar(models)]
  if (isTRUE(config$geo_ml_quick_models) &&
      exists("ugplot_geo_ml_quick_models", mode = "function", inherits = TRUE)) {
    models <- ugplot_geo_ml_quick_models(models)
  }
  models
}

ugplot_collaboration_reap_task <- function(task, now = Sys.time()) {
  if (is.list(task) && identical(task$state %||% "", "leased")) {
    expiry <- suppressWarnings(as.POSIXct(task$lease_expires_at))
    if (!is.na(expiry) && expiry <= now) {
      task$state <- "pending"
      task$lease_id <- ""
      task$client_id <- ""
      task$scientist_name <- ""
      task$client_progress <- 0
      task$client_message <- "Waiting for a contributor"
      task$client_candidate <- ""
      task$lease_expires_at <- as.POSIXct(NA)
      task$lease_expired_count <- as.integer(task$lease_expired_count %||% 0L) + 1L
      task$fallback_requested <- TRUE
      task$updated_at <- format(now, "%Y-%m-%d %H:%M:%S %z")
    }
  }
  task
}

ugplot_collaboration_consume_fallback <- function(task_id, jobs_dir = ugplot_default_jobs_dir()) {
  task_dir <- ugplot_collaboration_task_dir(task_id, jobs_dir)
  ugplot_collaboration_with_lock(task_dir, {
    task <- ugplot_collaboration_reap_task(ugplot_collaboration_read_task(task_id, jobs_dir))
    requested <- is.list(task) && isTRUE(task$fallback_requested)
    if (requested) task$fallback_requested <- FALSE
    if (is.list(task)) ugplot_collaboration_write_task(task, jobs_dir)
    requested
  })
}

ugplot_collaboration_refresh_task <- function(task_id, jobs_dir = ugplot_default_jobs_dir()) {
  task_dir <- ugplot_collaboration_task_dir(task_id, jobs_dir)
  ugplot_collaboration_with_lock(task_dir, {
    task <- ugplot_collaboration_read_task(task_id, jobs_dir)
    refreshed <- ugplot_collaboration_reap_task(task)
    if (is.list(refreshed)) ugplot_collaboration_write_task(refreshed, jobs_dir)
    refreshed
  })
}

ugplot_collaboration_claim_task <- function(client_id, capabilities = list(),
                                            lease_seconds = 120,
                                            jobs_dir = ugplot_default_jobs_dir()) {
  client_id <- trimws(as.character(client_id %||% ""))
  if (!nzchar(client_id)) stop("A collaboration client ID is required.", call. = FALSE)
  root <- ugplot_collaboration_dir(jobs_dir)
  if (!dir.exists(root)) return(NULL)
  task_ids <- basename(list.dirs(root, full.names = TRUE, recursive = FALSE))
  for (task_id in task_ids) {
    claimed <- ugplot_collaboration_with_lock(ugplot_collaboration_task_dir(task_id, jobs_dir), {
      task <- ugplot_collaboration_read_task(task_id, jobs_dir)
      task <- ugplot_collaboration_reap_task(task)
      parent <- if (is.list(task)) {
        ugplot_collaboration_parent_job_status(task, jobs_dir)
      } else {
        list(active = FALSE)
      }
      if (!is.list(task) || !identical(task$state %||% "", "pending") ||
          !isTRUE(parent$active) ||
          !ugplot_collaboration_models_compatible(task$requirements %||% list(), capabilities)) {
        if (is.list(task) && identical(task$state %||% "", "pending") && !isTRUE(parent$active)) {
          task$state <- "cancelled"
          task$cancel_reason <- "parent_job_not_running"
          task$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
        }
        if (is.list(task)) ugplot_collaboration_write_task(task, jobs_dir)
        NULL
      } else {
        lease_id <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", paste(sample(c(letters, LETTERS, 0:9), 12L, TRUE), collapse = ""))
        task$state <- "leased"
        task$lease_id <- lease_id
        task$client_id <- client_id
        task$scientist_name <- trimws(as.character(capabilities$scientist_name %||% client_id))
        task$client_progress <- 0
        task$client_message <- "Mission reserved"
        task$client_candidate <- ""
        task$heartbeat_at <- Sys.time()
        task$lease_expires_at <- Sys.time() + max(30, as.numeric(lease_seconds))
        task$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
        ugplot_collaboration_write_task(task, jobs_dir)
        payload <- readRDS(task$payload_path)
        list(task = task, payload = payload)
      }
    })
    if (!is.null(claimed)) return(claimed)
  }
  NULL
}

ugplot_collaboration_heartbeat <- function(task_id, lease_id, client_id,
                                          lease_seconds = 120,
                                          telemetry = list(),
                                          jobs_dir = ugplot_default_jobs_dir()) {
  task_dir <- ugplot_collaboration_task_dir(task_id, jobs_dir)
  ugplot_collaboration_with_lock(task_dir, {
    task <- ugplot_collaboration_read_task(task_id, jobs_dir)
    valid <- is.list(task) && identical(task$state %||% "", "leased") &&
      identical(as.character(task$lease_id %||% ""), as.character(lease_id)) &&
      identical(as.character(task$client_id %||% ""), as.character(client_id))
    if (!valid) return(list(accepted = FALSE, reason = "lease_not_active"))
    task$heartbeat_at <- Sys.time()
    task$lease_expires_at <- Sys.time() + max(30, as.numeric(lease_seconds))
    if (is.list(telemetry)) {
      progress <- suppressWarnings(as.numeric(telemetry$progress %||% task$client_progress %||% 0))
      if (length(progress) == 1L && is.finite(progress)) {
        task$client_progress <- max(0, min(1, progress))
      }
      task$client_message <- as.character(telemetry$message %||% task$client_message %||% "")
      task$client_candidate <- as.character(telemetry$candidate %||% task$client_candidate %||% "")
      task$client_completed <- suppressWarnings(as.integer(telemetry$completed %||% task$client_completed %||% 0L))
    }
    task$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ugplot_collaboration_write_task(task, jobs_dir)
    list(accepted = TRUE, lease_expires_at = task$lease_expires_at)
  })
}

ugplot_collaboration_release_task <- function(task_id, lease_id, client_id,
                                              jobs_dir = ugplot_default_jobs_dir()) {
  task_dir <- ugplot_collaboration_task_dir(task_id, jobs_dir)
  ugplot_collaboration_with_lock(task_dir, {
    task <- ugplot_collaboration_read_task(task_id, jobs_dir)
    valid <- is.list(task) && identical(task$state %||% "", "leased") &&
      identical(as.character(task$lease_id %||% ""), as.character(lease_id)) &&
      identical(as.character(task$client_id %||% ""), as.character(client_id))
    if (!valid) return(list(released = FALSE, reason = "lease_not_active"))
    task$state <- "pending"
    task$lease_id <- ""
    task$client_id <- ""
    task$scientist_name <- ""
    task$client_progress <- 0
    task$client_message <- "Waiting for a contributor"
    task$client_candidate <- ""
    task$lease_expires_at <- as.POSIXct(NA)
    task$heartbeat_at <- as.POSIXct(NA)
    task$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ugplot_collaboration_write_task(task, jobs_dir)
    list(released = TRUE, task_id = task_id)
  })
}

ugplot_collaboration_complete_task <- function(task_id, lease_id, client_id, result,
                                               jobs_dir = ugplot_default_jobs_dir()) {
  task_dir <- ugplot_collaboration_task_dir(task_id, jobs_dir)
  ugplot_collaboration_with_lock(task_dir, {
    task <- ugplot_collaboration_read_task(task_id, jobs_dir)
    if (is.list(task) && identical(task$state %||% "", "completed")) {
      return(list(accepted = FALSE, reason = "already_completed"))
    }
    valid <- is.list(task) && identical(task$state %||% "", "leased") &&
      identical(as.character(task$lease_id %||% ""), as.character(lease_id)) &&
      identical(as.character(task$client_id %||% ""), as.character(client_id))
    if (!valid) return(list(accepted = FALSE, reason = "lease_not_active"))
    expiry <- suppressWarnings(as.POSIXct(task$lease_expires_at))
    if (is.na(expiry) || expiry < Sys.time()) return(list(accepted = FALSE, reason = "lease_expired"))
    result_path <- file.path(task_dir, "result.rds")
    ugplot_write_rds_atomic(result, result_path)
    task$state <- "completed"
    task$result_path <- result_path
    task$completed_at <- Sys.time()
    task$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ugplot_collaboration_write_task(task, jobs_dir)
    list(accepted = TRUE, task_id = task_id)
  })
}

ugplot_collaboration_take_result <- function(task_id, jobs_dir = ugplot_default_jobs_dir()) {
  task_dir <- ugplot_collaboration_task_dir(task_id, jobs_dir)
  ugplot_collaboration_with_lock(task_dir, {
    task <- ugplot_collaboration_read_task(task_id, jobs_dir)
    if (!is.list(task) || !identical(task$state %||% "", "completed") || !file.exists(task$result_path %||% "")) {
      return(NULL)
    }
    list(task = task, result = readRDS(task$result_path))
  })
}

ugplot_collaboration_cancel_task <- function(task_id, reason = "completed_elsewhere",
                                             jobs_dir = ugplot_default_jobs_dir()) {
  task_dir <- ugplot_collaboration_task_dir(task_id, jobs_dir)
  ugplot_collaboration_with_lock(task_dir, {
    task <- ugplot_collaboration_read_task(task_id, jobs_dir)
    if (!is.list(task) || identical(task$state %||% "", "completed")) return(FALSE)
    task$state <- "cancelled"
    task$cancel_reason <- as.character(reason)
    task$lease_id <- ""
    task$client_id <- ""
    task$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ugplot_collaboration_write_task(task, jobs_dir)
    TRUE
  })
}

ugplot_collaboration_close_pending_task <- function(task_id, reason = "coordinator_draining",
                                                    jobs_dir = ugplot_default_jobs_dir()) {
  task_dir <- ugplot_collaboration_task_dir(task_id, jobs_dir)
  ugplot_collaboration_with_lock(task_dir, {
    task <- ugplot_collaboration_read_task(task_id, jobs_dir)
    if (!is.list(task) || !identical(task$state %||% "", "pending")) return(FALSE)
    task$state <- "cancelled"
    task$cancel_reason <- as.character(reason)
    task$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ugplot_collaboration_write_task(task, jobs_dir)
    TRUE
  })
}

ugplot_collaboration_encode_rds <- function(value) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(value, path)
  base64enc::base64encode(path)
}

ugplot_collaboration_public_status <- function(jobs_dir = ugplot_default_jobs_dir()) {
  root <- ugplot_collaboration_dir(jobs_dir)
  task_ids <- if (dir.exists(root)) basename(list.dirs(root, full.names = TRUE, recursive = FALSE)) else character(0)
  tasks <- Filter(Negate(is.null), lapply(task_ids, ugplot_collaboration_read_task, jobs_dir = jobs_dir))
  pending_tasks <- Filter(function(task) identical(task$state %||% "", "pending"), tasks)
  parent_cache <- new.env(parent = emptyenv())
  pending_parent <- lapply(pending_tasks, function(task) {
    parent_job_id <- as.character(task$parent_job_id %||% "")
    cache_key <- if (nzchar(parent_job_id)) parent_job_id else "<missing>"
    if (!exists(cache_key, envir = parent_cache, inherits = FALSE)) {
      assign(
        cache_key,
        ugplot_collaboration_parent_job_status(task, jobs_dir),
        envir = parent_cache
      )
    }
    get(cache_key, envir = parent_cache, inherits = FALSE)
  })
  pending_active <- vapply(pending_parent, function(parent) isTRUE(parent$active), logical(1))
  nonpending_tasks <- Filter(function(task) !identical(task$state %||% "", "pending"), tasks)
  states <- if (length(nonpending_tasks) > 0L) {
    table(vapply(nonpending_tasks, function(task) as.character(task$state %||% "unknown"), character(1)))
  } else {
    integer(0)
  }
  state_count <- function(name) {
    value <- suppressWarnings(as.integer(states[name]))
    if (length(value) == 0L || is.na(value)) 0L else value
  }
  list(
    status = "open",
    protocol_version = 1L,
    pending = sum(pending_active),
    inactive_pending = length(pending_active) - sum(pending_active),
    leased = state_count("leased"),
    completed = state_count("completed"),
    lease_seconds = 120L
  )
}

ugplot_collaboration_compatibility <- function(capabilities = list(),
                                               jobs_dir = ugplot_default_jobs_dir()) {
  root <- ugplot_collaboration_dir(jobs_dir)
  task_ids <- if (dir.exists(root)) basename(list.dirs(root, full.names = TRUE, recursive = FALSE)) else character(0)
  available <- unique(as.character(capabilities$models %||% character(0)))
  inspected <- Filter(Negate(is.null), lapply(task_ids, function(task_id) {
    task <- ugplot_collaboration_refresh_task(task_id, jobs_dir)
    if (!is.list(task) || !identical(task$state %||% "", "pending")) return(NULL)
    task
  }))
  parent_cache <- new.env(parent = emptyenv())
  inspected <- lapply(inspected, function(task) {
    parent_job_id <- as.character(task$parent_job_id %||% "")
    cache_key <- if (nzchar(parent_job_id)) parent_job_id else "<missing>"
    if (!exists(cache_key, envir = parent_cache, inherits = FALSE)) {
      assign(
        cache_key,
        ugplot_collaboration_parent_job_status(task, jobs_dir),
        envir = parent_cache
      )
    }
    list(
      task = task,
      parent = get(cache_key, envir = parent_cache, inherits = FALSE)
    )
  })
  active <- Filter(function(item) isTRUE(item$parent$active), inspected)
  inactive <- Filter(function(item) !isTRUE(item$parent$active), inspected)
  missions <- lapply(active, function(item) {
    task <- item$task
    required <- unique(as.character(task$requirements$models %||% character(0)))
    required <- required[nzchar(required)]
    missing <- setdiff(required, available)
    list(
      task_id = as.character(task$task_id),
      parent_job_id = as.character(task$parent_job_id %||% ""),
      title = as.character(task$mission$title %||% "Scientific mission"),
      compatible = length(missing) == 0L,
      required_models = required,
      missing_models = missing
    )
  })
  list(
    protocol_version = 1L,
    pending = length(missions),
    compatible = sum(vapply(missions, function(mission) isTRUE(mission$compatible), logical(1))),
    missions = missions,
    inactive_pending = length(inactive),
    inactive_missions = lapply(inactive, function(item) list(
      task_id = as.character(item$task$task_id %||% ""),
      parent_job_id = as.character(item$parent$parent_job_id %||% ""),
      parent_state = as.character(item$parent$state %||% "unknown")
    ))
  )
}

ugplot_collaboration_job_group_activity <- function(job_id,
                                                    jobs_dir = ugplot_default_jobs_dir()) {
  job_id <- ugplot_validate_job_id(job_id)
  config_path <- file.path(ugplot_job_dir(job_id, jobs_dir), "config.rds")
  if (!file.exists(config_path)) stop("Job config is not available: ", job_id, call. = FALSE)
  config <- readRDS(config_path)
  if (!identical(as.character(config$type %||% "geo"), "geo") &&
      !identical(as.character(config$runner %||% ""), "ugplot_run_geo_pipeline_job")) {
    return(list(job_id = job_id, total = 0L, completed = 0L, processing = 0L, pending = 0L, groups = data.frame()))
  }
  accession <- trimws(as.character(config$accession %||% ""))
  source <- as.character(config$matrix_source %||% "processed")
  target <- as.character(config$target_column %||% "")
  threshold <- suppressWarnings(as.numeric(config$transcript_absrho_threshold %||% 0.8))
  min_samples <- suppressWarnings(as.numeric(config$transcript_min_samples %||% 80))
  run_key <- as.character(config$geo_transcript_ml_run_key %||% "")
  if (!nzchar(run_key) && nzchar(target) && is.finite(threshold) && is.finite(min_samples)) {
    run_key <- ugplot_geo_transcript_ml_run_key(target, threshold, min_samples)
  }
  pipeline_dir <- if (nzchar(accession)) {
    ugplot_geo_transcript_ml_dir(ugplot_geo_cache_dir(accession), source, run_key)
  } else {
    ""
  }
  manifest_path <- if (nzchar(pipeline_dir)) ugplot_geo_distributed_manifest_path(pipeline_dir) else ""
  if (!nzchar(manifest_path) || !file.exists(manifest_path)) {
    return(list(job_id = job_id, total = 0L, completed = 0L, processing = 0L, pending = 0L, groups = data.frame()))
  }
  manifest <- tryCatch(readRDS(manifest_path), error = function(e) data.frame())
  if (!is.data.frame(manifest) || nrow(manifest) == 0L || !"GroupID" %in% names(manifest)) {
    return(list(job_id = job_id, total = 0L, completed = 0L, processing = 0L, pending = 0L, groups = data.frame()))
  }
  value_at <- function(column, index, default = "") {
    if (!(column %in% names(manifest))) return(default)
    value <- manifest[[column]][[index]]
    if (length(value) == 0L || is.na(value)) default else value
  }
  rows <- lapply(seq_len(nrow(manifest)), function(index) {
    group_id <- as.character(manifest$GroupID[[index]])
    manifest_state <- as.character(value_at("State", index, "pending"))
    worker <- as.character(value_at("Worker", index, ""))
    progress <- suppressWarnings(as.numeric(value_at("Progress", index, 0)))
    if (!is.finite(progress)) progress <- 0
    message <- as.character(value_at("Message", index, value_at("Error", index, "")))
    task <- ugplot_collaboration_read_task(paste(job_id, "screen", group_id, sep = ":"), jobs_dir)
    task <- ugplot_collaboration_reap_task(task)
    task_state <- if (is.list(task)) as.character(task$state %||% "") else ""
    state <- "pending"
    executor <- ""
    executor_type <- ""
    if (identical(manifest_state, "completed")) {
      state <- "completed"
      progress <- 1
      executor <- if (is.list(task) && nzchar(task$scientist_name %||% "")) {
        as.character(task$scientist_name)
      } else {
        worker
      }
    } else if (identical(task_state, "leased")) {
      state <- "processing"
      executor <- as.character(task$scientist_name %||% task$client_id %||% "Public scientist")
      executor_type <- "collaboration"
      progress <- suppressWarnings(as.numeric(task$client_progress %||% 0))
      if (!is.finite(progress)) progress <- 0
      message <- as.character(task$client_message %||% "Collaborative experiment running")
      candidate <- as.character(task$client_candidate %||% "")
      if (nzchar(candidate)) message <- paste(message, "—", candidate)
    } else if (manifest_state %in% c("dispatching", "submitted", "running")) {
      state <- "processing"
      executor <- worker
      executor_type <- "server"
    } else if (identical(task_state, "completed")) {
      state <- "processing"
      progress <- 1
      executor <- as.character(task$scientist_name %||% task$client_id %||% "Public scientist")
      executor_type <- "collaboration"
      message <- "Contribution returned; validating"
    }
    data.frame(
      group_id = group_id,
      state = state,
      progress = max(0, min(1, progress)),
      executor = executor,
      executor_type = executor_type,
      message = message,
      stringsAsFactors = FALSE
    )
  })
  groups <- do.call(rbind, rows)
  list(
    job_id = job_id,
    total = nrow(groups),
    completed = sum(groups$state == "completed"),
    processing = sum(groups$state == "processing"),
    pending = sum(groups$state == "pending"),
    groups = groups
  )
}

ugplot_collaboration_append_event <- function(path, type, data = list()) {
  events <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) list()) else list()
  events[[length(events) + 1L]] <- list(
    sequence = length(events) + 1L,
    type = as.character(type),
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    data = data
  )
  ugplot_write_rds_atomic(events, path)
  invisible(utils::tail(events, 1L)[[1]])
}

ugplot_collaboration_run_payload <- function(payload, cpu_limit = 1L, event_path = tempfile(fileext = ".rds")) {
  if (!is.list(payload) || !is.data.frame(payload$dataset) || !is.list(payload$config)) {
    stop("The collaboration payload is invalid.", call. = FALSE)
  }
  config <- payload$config
  runner_name <- as.character(config$runner %||% "")
  allowed_runners <- c("ugplot_run_geo_screen_group_job")
  if (!(runner_name %in% allowed_runners)) stop("Unsupported collaboration runner.", call. = FALSE)
  config$cpu_limit <- max(1L, suppressWarnings(as.integer(cpu_limit)))
  config$parallel_enabled <- config$cpu_limit > 1L
  config$job_dir <- tempfile("ugplot-collaboration-task-")
  dir.create(config$job_dir, recursive = TRUE)
  on.exit(unlink(config$job_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dataset <- payload$dataset
  missing_count <- sum(is.na(dataset))
  variable_names <- names(dataset)
  variable_types <- vapply(dataset, function(column) {
    if (is.numeric(column)) "numeric" else if (is.factor(column)) "category" else class(column)[[1]]
  }, character(1))
  configured_target <- as.character(config$target_column %||% "")
  target_name <- if ("target" %in% variable_names) {
    "target"
  } else if (nzchar(configured_target) && configured_target %in% variable_names) {
    configured_target
  } else if (length(variable_names) > 0L) {
    variable_names[[1]]
  } else {
    ""
  }
  target_label <- if (nzchar(configured_target)) configured_target else target_name
  target_values <- if (nzchar(target_name)) dataset[[target_name]] else NULL
  target_distribution <- list(kind = "none", labels = character(0), counts = numeric(0))
  target_summary <- list(distinct = length(unique(target_values[!is.na(target_values)])))
  if (is.numeric(target_values)) {
    finite_target <- as.numeric(target_values[is.finite(target_values)])
    if (length(finite_target) > 0L) {
      histogram <- graphics::hist(
        finite_target,
        breaks = min(16L, max(5L, ceiling(sqrt(length(finite_target))))),
        plot = FALSE, include.lowest = TRUE
      )
      target_distribution <- list(
        kind = "numeric", labels = signif(histogram$mids, 5), counts = as.numeric(histogram$counts)
      )
      target_summary <- c(target_summary, list(
        minimum = min(finite_target), median = stats::median(finite_target),
        mean = mean(finite_target), maximum = max(finite_target)
      ))
    }
  } else if (length(target_values) > 0L) {
    counts <- utils::head(sort(table(as.character(target_values), useNA = "no"), decreasing = TRUE), 14L)
    target_distribution <- list(
      kind = "categorical", labels = names(counts), counts = as.numeric(counts)
    )
  }
  group_metadata <- config$distributed_group %||% list()
  if (is.data.frame(group_metadata) && nrow(group_metadata) > 0L) {
    group_metadata <- as.list(group_metadata[1, , drop = FALSE])
  }
  metadata <- list()
  if (is.list(group_metadata)) {
    excluded <- c("GroupKey", "DatasetPath", "CpGs")
    for (metadata_name in setdiff(names(group_metadata), excluded)) {
      value <- unlist(group_metadata[[metadata_name]], use.names = FALSE)
      if (length(value) == 1L && !is.na(value) && nzchar(as.character(value)) && nchar(as.character(value)) <= 160L) {
        metadata[[metadata_name]] <- as.character(value)
      }
    }
  }
  ugplot_collaboration_append_event(event_path, "mission_received", list())
  ugplot_collaboration_append_event(event_path, "dataset_profiled", list(
    rows = nrow(dataset), columns = ncol(dataset),
    total_values = nrow(dataset) * ncol(dataset),
    non_missing_values = nrow(dataset) * ncol(dataset) - missing_count,
    missing_pct = if (length(dataset) > 0L) 100 * missing_count / length(as.matrix(dataset)) else 0,
    variable_names = variable_names, variable_types = variable_types,
    numeric_variables = sum(variable_types == "numeric"),
    target_name = target_name, target_label = target_label,
    target_summary = target_summary, target_distribution = target_distribution,
    metadata = metadata
  ))
  progress_callback <- function(...) {
    args <- list(...)
    current <- args$current_run %||% list()
    event_type <- if (is.list(current) && nzchar(as.character(current$model %||% ""))) "experiment_started" else "progress_updated"
    ugplot_collaboration_append_event(event_path, event_type, list(
      progress = suppressWarnings(as.numeric(args$progress %||% NA_real_)),
      message = as.character(args$message %||% ""),
      candidate = as.character(current$model %||% ""),
      dataset_seed = current$dataset_seed %||% NULL,
      training_seed = current$training_seed %||% NULL
    ))
  }
  partial_callback <- function(partial) {
    table <- partial$results_table %||% data.frame()
    if (!is.data.frame(table) || nrow(table) == 0L) return(invisible(NULL))
    latest <- table[nrow(table), , drop = FALSE]
    metric_names <- intersect(c("R2", "Accuracy", "MAE", "RMSE"), names(latest))
    metrics <- lapply(metric_names, function(name) suppressWarnings(as.numeric(latest[[name]][[1]])))
    names(metrics) <- metric_names
    metrics <- metrics[vapply(metrics, function(value) length(value) == 1L && is.finite(value), logical(1))]
    ugplot_collaboration_append_event(event_path, "metric_updated", list(
      candidate = as.character(latest$Model[[1]] %||% ""),
      status = as.character(latest$Status[[1]] %||% ""),
      metrics = metrics,
      completed = nrow(table)
    ))
  }
  runner <- get(runner_name, mode = "function", inherits = TRUE)
  result <- runner(dataset, config, progress_callback = progress_callback, partial_callback = partial_callback)
  ugplot_collaboration_append_event(event_path, "validation_completed", list(
    group_id = as.character(result$group_id %||% ""),
    candidate = as.character(result$screen_result$best_model_name %||% "")
  ))
  result
}
