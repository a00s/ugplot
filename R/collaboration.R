ugplot_collaboration_dir <- function(jobs_dir = ugplot_default_jobs_dir()) {
  file.path(jobs_dir, "collaboration")
}

ugplot_collaboration_task_dir <- function(task_id, jobs_dir = ugplot_default_jobs_dir()) {
  task_id <- as.character(task_id %||% "")
  if (length(task_id) != 1L || is.na(task_id) || !nzchar(task_id) ||
      nchar(task_id, type = "chars") > 200L || !grepl("^[A-Za-z0-9._:-]+$", task_id)) {
    stop("A valid collaboration task ID is required.", call. = FALSE)
  }
  file.path(ugplot_collaboration_dir(jobs_dir), gsub(":", "_", task_id, fixed = TRUE))
}

ugplot_collaboration_index_path <- function(jobs_dir = ugplot_default_jobs_dir()) {
  file.path(ugplot_collaboration_dir(jobs_dir), "task-index.rds")
}

ugplot_collaboration_index_columns <- function() {
  c("task_id", "parent_job_id", "state", "updated_at")
}

ugplot_collaboration_index_row <- function(task) {
  data.frame(
    task_id = as.character(task$task_id %||% ""),
    parent_job_id = as.character(task$parent_job_id %||% ""),
    state = as.character(task$state %||% "unknown"),
    updated_at = as.character(task$updated_at %||% ""),
    stringsAsFactors = FALSE
  )
}

ugplot_collaboration_build_index <- function(jobs_dir = ugplot_default_jobs_dir()) {
  root <- ugplot_collaboration_dir(jobs_dir)
  ugplot_ensure_dir(root)
  task_dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  task_dirs <- task_dirs[file.exists(file.path(task_dirs, "task.rds"))]
  rows <- Filter(Negate(is.null), lapply(file.path(task_dirs, "task.rds"), function(path) {
    task <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.list(task) || !nzchar(as.character(task$task_id %||% ""))) return(NULL)
    ugplot_collaboration_index_row(task)
  }))
  index <- if (length(rows) == 0L) {
    data.frame(task_id = character(), parent_job_id = character(), state = character(),
               updated_at = character(), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, rows)
  }
  ugplot_write_rds_atomic(index, ugplot_collaboration_index_path(jobs_dir))
  index
}

ugplot_collaboration_read_index <- function(jobs_dir = ugplot_default_jobs_dir()) {
  path <- ugplot_collaboration_index_path(jobs_dir)
  index <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
  required <- ugplot_collaboration_index_columns()
  if (!is.data.frame(index) || !all(required %in% names(index))) {
    index <- ugplot_collaboration_build_index(jobs_dir)
  }
  index[, required, drop = FALSE]
}

ugplot_collaboration_update_index <- function(task, jobs_dir = ugplot_default_jobs_dir()) {
  root <- ugplot_collaboration_dir(jobs_dir)
  ugplot_ensure_dir(root)
  lock_root <- file.path(root, ".task-index")
  ugplot_collaboration_with_lock(lock_root, {
    index <- ugplot_collaboration_read_index(jobs_dir)
    row <- ugplot_collaboration_index_row(task)
    match_index <- match(row$task_id[[1]], index$task_id)
    unchanged <- !is.na(match_index) &&
      identical(as.character(index$parent_job_id[[match_index]]), row$parent_job_id[[1]]) &&
      identical(as.character(index$state[[match_index]]), row$state[[1]])
    if (!unchanged) {
      if (is.na(match_index)) index <- rbind(index, row) else index[match_index, ] <- row
      ugplot_write_rds_atomic(index, ugplot_collaboration_index_path(jobs_dir))
    }
    invisible(index)
  })
}

ugplot_collaboration_task_ids <- function(jobs_dir = ugplot_default_jobs_dir(), states = NULL,
                                          parent_job_id = NULL) {
  index <- ugplot_collaboration_read_index(jobs_dir)
  if (!is.null(states)) index <- index[index$state %in% as.character(states), , drop = FALSE]
  if (!is.null(parent_job_id)) {
    index <- index[index$parent_job_id %in% as.character(parent_job_id), , drop = FALSE]
  }
  as.character(index$task_id)
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
  ugplot_collaboration_update_index(task, jobs_dir)
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
  resume_screen <- config$distributed_resume_screen %||% NULL
  if (is.list(resume_screen) && is.data.frame(resume_screen$summary) &&
      nrow(resume_screen$summary) > 0L && "BestModel" %in% names(resume_screen$summary)) {
    best_model <- unique(as.character(resume_screen$summary$BestModel))
    best_model <- best_model[nzchar(best_model) & best_model != "-"]
    if (length(best_model) > 0L) return(best_model)
  }
  models <- unique(as.character(config$models %||% character(0)))
  models <- models[nzchar(models)]
  if (isTRUE(config$geo_ml_quick_models) &&
      exists("ugplot_geo_ml_quick_models", mode = "function", inherits = TRUE)) {
    models <- ugplot_geo_ml_quick_models(models)
  }
  models
}

ugplot_collaboration_text <- function(value, field, max_chars, allow_empty = FALSE,
                                      pattern = NULL) {
  value <- unlist(value %||% "", use.names = FALSE)
  if (length(value) != 1L || is.na(value)) {
    stop(field, " must be a single text value.", call. = FALSE)
  }
  value <- trimws(gsub("[[:cntrl:]]", " ", as.character(value)))
  value <- gsub("[[:space:]]+", " ", value)
  if (!allow_empty && !nzchar(value)) stop(field, " is required.", call. = FALSE)
  if (nchar(value, type = "chars") > max_chars) stop(field, " is too long.", call. = FALSE)
  if (!is.null(pattern) && nzchar(value) && !grepl(pattern, value)) {
    stop(field, " contains unsupported characters.", call. = FALSE)
  }
  value
}

ugplot_collaboration_table <- function(value, field, allowed_columns = NULL,
                                       required_columns = character(0),
                                       max_rows = 10000L, max_columns = 64L,
                                       max_text_chars = 500L) {
  if (is.null(value) || (is.list(value) && length(value) == 0L)) return(data.frame())
  rows <- if (is.data.frame(value)) {
    lapply(seq_len(nrow(value)), function(i) as.list(value[i, , drop = FALSE]))
  } else if (is.list(value) && all(vapply(value, is.list, logical(1)))) {
    value
  } else {
    stop(field, " must be a JSON table.", call. = FALSE)
  }
  if (length(rows) > max_rows) stop(field, " has too many rows.", call. = FALSE)
  column_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  if (length(column_names) > max_columns) stop(field, " has too many columns.", call. = FALSE)
  if (length(column_names) > 0L && any(
    is.na(column_names) | !grepl("^[A-Za-z][A-Za-z0-9_.]{0,79}$", column_names)
  )) stop(field, " contains invalid column names.", call. = FALSE)
  if (!is.null(allowed_columns) && any(!column_names %in% allowed_columns)) {
    stop(field, " contains unsupported columns.", call. = FALSE)
  }
  if (!all(required_columns %in% column_names)) {
    stop(field, " is missing required columns.", call. = FALSE)
  }
  for (row_index in seq_along(rows)) {
    row <- rows[[row_index]]
    if (is.null(names(row)) || anyDuplicated(names(row))) stop(field, " contains an invalid row.", call. = FALSE)
    for (column_name in names(row)) {
      cell <- row[[column_name]]
      cell <- unlist(cell, recursive = FALSE, use.names = FALSE)
      if (length(cell) > 1L || (length(cell) == 1L && (is.raw(cell) || is.complex(cell)))) {
        stop(field, " contains a non-scalar value.", call. = FALSE)
      }
      if (length(cell) == 1L && is.character(cell) && nchar(cell, type = "chars") > max_text_chars) {
        stop(
          field, " contains oversized text in column ", column_name,
          " (row ", row_index, ").", call. = FALSE
        )
      }
    }
  }
  if (length(rows) == 0L) return(data.frame())
  columns <- lapply(column_names, function(column_name) {
    cells <- lapply(rows, function(row) {
      cell <- row[[column_name]]
      cell <- unlist(cell, recursive = FALSE, use.names = FALSE)
      if (length(cell) == 0L) NA else cell[[1]]
    })
    non_missing <- cells[!vapply(cells, function(cell) length(cell) == 1L && is.na(cell), logical(1))]
    if (length(non_missing) > 0L && all(vapply(non_missing, is.logical, logical(1)))) {
      return(vapply(cells, function(cell) if (is.na(cell)) NA else isTRUE(cell), logical(1)))
    }
    if (length(non_missing) > 0L && all(vapply(non_missing, is.numeric, logical(1)))) {
      return(vapply(cells, function(cell) suppressWarnings(as.numeric(cell)), numeric(1)))
    }
    vapply(cells, function(cell) {
      if (length(cell) == 0L || is.na(cell)) return(NA_character_)
      text <- gsub("[[:cntrl:]]", " ", as.character(cell))
      if (grepl("^[=+@]", text) || grepl("^-([0-9]|[A-Za-z])", text)) text <- paste0("'", text)
      text
    }, character(1))
  })
  names(columns) <- column_names
  as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)
}

ugplot_collaboration_portable_result <- function(result) {
  if (!is.list(result)) stop("Science Collab result must be a list.", call. = FALSE)
  portable_summary <- function(summary) {
    redundant_fields <- c(
      "GroupKey", "TranscriptMembers", "GeneMembers", "ExtraTranscripts", "CpGs",
      "DatasetPath", "ScreenResultPath", "ImportancePath", "StabilityResultPath"
    )
    if (is.data.frame(summary)) {
      return(summary[, setdiff(names(summary), redundant_fields), drop = FALSE])
    }
    if (is.list(summary) && all(vapply(summary, is.list, logical(1)))) {
      return(lapply(summary, function(row) {
        row[setdiff(names(row) %||% character(0), redundant_fields)]
      }))
    }
    summary
  }
  portable_run <- function(run) {
    if (!is.list(run)) return(NULL)
    scalar_summary <- run$final_summary %||% list()
    scalar_summary <- scalar_summary[vapply(scalar_summary, function(value) {
      value <- unlist(value, recursive = FALSE, use.names = FALSE)
      length(value) <= 1L && (length(value) == 0L || is.atomic(value))
    }, logical(1))]
    list(
      best_model_name = as.character(run$best_model_name %||% ""),
      results_table = run$results_table %||% data.frame(),
      final_summary = scalar_summary,
      partial = isTRUE(run$partial),
      updated_at = as.character(run$updated_at %||% ""),
      finished_at = as.character(run$finished_at %||% "")
    )
  }
  artifacts <- result$stability_artifacts %||% list()
  list(
    kind = as.character(result$kind %||% ""),
    protocol_version = 2L,
    parent_job_id = as.character(result$parent_job_id %||% ""),
    worker_name = as.character(result$worker_name %||% ""),
    group_id = as.character(result$group_id %||% ""),
    summary = portable_summary(result$summary %||% data.frame()),
    screen_result = portable_run(result$screen_result),
    importance = result$importance %||% data.frame(),
    stability_summary = portable_summary(result$stability_summary %||% data.frame()),
    stability_artifacts = lapply(artifacts, function(artifact) list(
      summary = portable_summary(artifact$summary %||% data.frame()),
      result = portable_run(artifact$result),
      importance = artifact$importance %||% data.frame()
    ))
  )
}

ugplot_collaboration_validate_result <- function(result, task) {
  result <- ugplot_collaboration_portable_result(result)
  kind <- ugplot_collaboration_text(result$kind, "result kind", 40L, pattern = "^geo_complete_group$")
  expected_group <- ugplot_collaboration_text(
    task$mission$entity$id %||% sub("^.*:(analyze|screen):", "", task$task_id %||% ""),
    "expected group", 80L, pattern = "^[A-Za-z0-9._-]+$"
  )
  group_id <- ugplot_collaboration_text(
    result$group_id, "result group", 80L, pattern = "^[A-Za-z0-9._-]+$"
  )
  if (!identical(group_id, expected_group)) stop("Science Collab result group does not match its lease.", call. = FALSE)
  allowed_models <- unique(as.character(task$requirements$models %||% character(0)))
  summary_columns <- c(
    "Source", "Phase", "GroupID", "GroupKey", "PrincipalTranscript", "Gene", "Columns", "Samples",
    "TranscriptCount", "TranscriptMembers", "GeneMembers", "ExtraTranscripts", "CpGs", "TriggerMaxAbsRho",
    "TriggerBestCpG", "TriggerBestRho", "BestModel", "MetricName", "BestMetric", "MedianMetric", "MeanMetric",
    "MinMetric", "MaxMetric", "MetricSE", "SeedsRun", "SeedStrategy", "ModelsRun", "ModelsOK", "DatasetPath",
    "ScreenResultPath", "ImportancePath", "StratumColumn", "StratumValue", "StratumSamples", "Stable",
    "StabilityDetail", "StabilityResultPath", "ModelRank", "RhoRank", "CombinedRank"
  )
  run_columns <- c(
    "Model", "Status", "elapsed_seconds", "R2", "Accuracy", "MAE", "RMSE", "Error",
    "dataset_seed", "training_seed", "threshold_scope", "imputation_scope"
  )
  validate_summary <- function(value, field, allow_empty = FALSE) {
    table <- ugplot_collaboration_table(
      value, field, summary_columns,
      required_columns = if (allow_empty) character(0) else c("GroupID", "BestModel"),
      max_rows = if (allow_empty) 64L else 1L, max_columns = length(summary_columns)
    )
    if (nrow(table) > 0L && any(as.character(table$GroupID) != expected_group)) {
      stop(field, " contains a different group.", call. = FALSE)
    }
    if (nrow(table) > 0L && "BestModel" %in% names(table) && length(allowed_models) > 0L &&
        any(!as.character(table$BestModel) %in% c(allowed_models, "-"))) {
      stop(field, " contains an unauthorized model.", call. = FALSE)
    }
    table
  }
  validate_run <- function(run, field) {
    if (is.null(run)) return(NULL)
    if (!is.list(run)) stop(field, " is invalid.", call. = FALSE)
    best_model <- ugplot_collaboration_text(run$best_model_name %||% "", paste(field, "model"), 80L, allow_empty = TRUE)
    if (nzchar(best_model) && length(allowed_models) > 0L && !best_model %in% c(allowed_models, "-")) {
      stop(field, " contains an unauthorized model.", call. = FALSE)
    }
    table <- ugplot_collaboration_table(run$results_table, paste(field, "runs"), run_columns,
      required_columns = c("Model", "Status"), max_rows = 10000L, max_columns = length(run_columns))
    if (nrow(table) > 0L) {
      if (length(allowed_models) > 0L && any(!as.character(table$Model) %in% allowed_models)) {
        stop(field, " contains unauthorized run models.", call. = FALSE)
      }
      allowed_status <- c("OK", "TIMEOUT", "SKIPPED_TIMEOUT", "INCOMPATIBLE", "INVALID_METRICS", "ERROR")
      if (any(!as.character(table$Status) %in% allowed_status)) stop(field, " contains invalid run status.", call. = FALSE)
      for (seed_name in intersect(c("dataset_seed", "training_seed"), names(table))) {
        seeds <- suppressWarnings(as.numeric(table[[seed_name]]))
        if (any(is.finite(seeds) & (seeds < 1 | seeds > 1000000 | seeds != floor(seeds)))) {
          stop(field, " contains invalid seeds.", call. = FALSE)
        }
      }
      for (metric_name in intersect(c("R2", "Accuracy", "MAE", "RMSE", "elapsed_seconds"), names(table))) {
        metrics <- suppressWarnings(as.numeric(table[[metric_name]]))
        if (any(is.finite(metrics) & abs(metrics) > 1e9)) stop(field, " contains invalid metrics.", call. = FALSE)
      }
      if ("Accuracy" %in% names(table)) {
        accuracy <- suppressWarnings(as.numeric(table$Accuracy))
        if (any(is.finite(accuracy) & (accuracy < 0 | accuracy > 1))) stop(field, " contains invalid accuracy.", call. = FALSE)
      }
    }
    final_summary <- run$final_summary %||% list()
    if (!is.list(final_summary) || length(final_summary) > 64L || any(nchar(names(final_summary) %||% "") > 80L)) {
      stop(field, " summary is invalid.", call. = FALSE)
    }
    for (value in final_summary) {
      value <- unlist(value, recursive = FALSE, use.names = FALSE)
      if (length(value) > 1L || (length(value) == 1L && is.character(value) && nchar(value) > 500L)) {
        stop(field, " summary contains invalid values.", call. = FALSE)
      }
    }
    list(best_model_name = best_model, results_table = table, final_summary = final_summary,
      partial = isTRUE(run$partial), updated_at = ugplot_collaboration_text(run$updated_at %||% "", paste(field, "updated_at"), 80L, TRUE),
      finished_at = ugplot_collaboration_text(run$finished_at %||% "", paste(field, "finished_at"), 80L, TRUE))
  }
  summary <- validate_summary(result$summary, "screening summary")
  screen_result <- validate_run(result$screen_result, "screening result")
  importance <- ugplot_collaboration_table(result$importance, "screening importance", max_rows = 10000L, max_columns = 64L)
  stability_summary <- validate_summary(result$stability_summary, "stability summary", allow_empty = TRUE)
  artifacts <- result$stability_artifacts %||% list()
  if (!is.list(artifacts) || length(artifacts) > 64L) stop("Too many stability artifacts.", call. = FALSE)
  artifacts <- lapply(seq_along(artifacts), function(i) {
    artifact <- artifacts[[i]]
    if (!is.list(artifact)) stop("Invalid stability artifact.", call. = FALSE)
    summary <- validate_summary(artifact$summary, paste0("stability artifact ", i))
    run <- validate_run(artifact$result, paste0("stability artifact ", i, " result"))
    if (is.null(run) || !is.data.frame(run$results_table) || nrow(run$results_table) == 0L) {
      stop("Stability artifacts require seed-level results.", call. = FALSE)
    }
    rows <- run$results_table
    if (!all(c("dataset_seed", "training_seed") %in% names(rows))) {
      stop("Stability artifacts require dataset and training seeds.", call. = FALSE)
    }
    ok <- as.character(rows$Status) == "OK"
    metric_column <- if ("R2" %in% names(rows)) "R2" else if ("Accuracy" %in% names(rows)) "Accuracy" else ""
    metrics <- if (nzchar(metric_column)) suppressWarnings(as.numeric(rows[[metric_column]][ok])) else numeric(0)
    metrics <- metrics[is.finite(metrics)]
    if (length(metrics) < 2L) stop("Stability artifacts require at least two valid metrics.", call. = FALSE)
    dataset_seeds <- unique(suppressWarnings(as.integer(rows$dataset_seed[ok])))
    training_seeds <- unique(suppressWarnings(as.integer(rows$training_seed[ok])))
    dataset_seeds <- dataset_seeds[!is.na(dataset_seeds)]
    training_seeds <- training_seeds[!is.na(training_seeds)]
    if (length(dataset_seeds) < 2L || !identical(training_seeds, 1L)) {
      stop("Stability artifacts must vary dataset partitions with training seed 1.", call. = FALSE)
    }
    summary$BestMetric <- max(metrics)
    summary$MedianMetric <- stats::median(metrics)
    summary$MeanMetric <- mean(metrics)
    summary$MinMetric <- min(metrics)
    summary$MaxMetric <- max(metrics)
    summary$MetricSE <- stats::sd(metrics) / sqrt(length(metrics))
    summary$SeedsRun <- length(metrics)
    summary$SeedStrategy <- ugplot_geo_ml_seed_strategy()
    payload <- tryCatch(readRDS(task$payload_path), error = function(e) NULL)
    config <- if (is.list(payload) && is.list(payload$config)) payload$config else list()
    stable_state <- ugplot_geo_stability_state(
      metrics,
      max(2L, as.integer(config$geo_ml_min_stability_seeds %||% 30L)),
      max(2L, as.integer(config$geo_ml_stability_window %||% 30L)),
      max(0, as.numeric(config$geo_ml_stability_tolerance %||% 0.01))
    )
    summary$Stable <- isTRUE(stable_state$stable)
    summary$StabilityDetail <- stable_state$reason
    list(summary = summary, result = run,
      importance = ugplot_collaboration_table(artifact$importance, paste0("stability artifact ", i, " importance"), max_rows = 10000L, max_columns = 64L))
  })
  list(kind = kind, protocol_version = 2L, parent_job_id = "", worker_name = "", group_id = group_id,
    summary = summary, screen_result = screen_result, importance = importance,
    stability_summary = stability_summary, stability_artifacts = artifacts)
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
  client_id <- ugplot_collaboration_text(
    client_id, "collaboration client ID", 128L,
    pattern = "^[A-Za-z0-9._:-]+$"
  )
  if (!is.list(capabilities)) stop("Collaboration capabilities must be an object.", call. = FALSE)
  if (!is.null(capabilities$protocol_version)) {
    protocol_version <- suppressWarnings(as.integer(unlist(capabilities$protocol_version, use.names = FALSE)))
    if (length(protocol_version) != 1L || is.na(protocol_version) || protocol_version != 2L) {
      stop("Science Collab protocol version 2 is required.", call. = FALSE)
    }
  }
  models <- unique(unlist(capabilities$models %||% character(0), use.names = FALSE))
  if (length(models) > 256L) stop("Too many collaboration models were declared.", call. = FALSE)
  models <- vapply(models, ugplot_collaboration_text, character(1),
    field = "collaboration model", max_chars = 80L, pattern = "^[A-Za-z0-9._-]+$")
  capabilities$models <- models
  capabilities$scientist_name <- ugplot_collaboration_text(
    capabilities$scientist_name %||% client_id, "scientist name", 80L
  )
  root <- ugplot_collaboration_dir(jobs_dir)
  if (!dir.exists(root)) return(NULL)
  task_ids <- ugplot_collaboration_task_ids(jobs_dir, states = c("pending", "leased"))
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
        task$scientist_name <- capabilities$scientist_name
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
  if (!file.exists(file.path(task_dir, "task.rds"))) return(list(accepted = FALSE, reason = "task_not_found"))
  lease_id <- ugplot_collaboration_text(lease_id, "lease ID", 80L, pattern = "^[A-Za-z0-9._:-]+$")
  client_id <- ugplot_collaboration_text(client_id, "collaboration client ID", 128L, pattern = "^[A-Za-z0-9._:-]+$")
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
      task$client_message <- ugplot_collaboration_text(
        telemetry$message %||% task$client_message %||% "", "telemetry message", 180L, allow_empty = TRUE
      )
      task$client_candidate <- ugplot_collaboration_text(
        telemetry$candidate %||% task$client_candidate %||% "", "telemetry candidate", 80L,
        allow_empty = TRUE, pattern = "^[A-Za-z0-9._ -]*$"
      )
      completed <- suppressWarnings(as.integer(telemetry$completed %||% task$client_completed %||% 0L))
      if (length(completed) != 1L || is.na(completed) || completed < 0L || completed > 1000000L) {
        stop("Telemetry completed count is invalid.", call. = FALSE)
      }
      task$client_completed <- completed
    }
    task$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ugplot_collaboration_write_task(task, jobs_dir)
    list(accepted = TRUE, lease_expires_at = task$lease_expires_at)
  })
}

ugplot_collaboration_release_task <- function(task_id, lease_id, client_id,
                                              jobs_dir = ugplot_default_jobs_dir()) {
  task_dir <- ugplot_collaboration_task_dir(task_id, jobs_dir)
  if (!file.exists(file.path(task_dir, "task.rds"))) return(list(released = FALSE, reason = "task_not_found"))
  lease_id <- ugplot_collaboration_text(lease_id, "lease ID", 80L, pattern = "^[A-Za-z0-9._:-]+$")
  client_id <- ugplot_collaboration_text(client_id, "collaboration client ID", 128L, pattern = "^[A-Za-z0-9._:-]+$")
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
  if (!file.exists(file.path(task_dir, "task.rds"))) return(list(accepted = FALSE, reason = "task_not_found"))
  lease_id <- ugplot_collaboration_text(lease_id, "lease ID", 80L, pattern = "^[A-Za-z0-9._:-]+$")
  client_id <- ugplot_collaboration_text(client_id, "collaboration client ID", 128L, pattern = "^[A-Za-z0-9._:-]+$")
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
    result <- ugplot_collaboration_validate_result(result, task)
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
  index <- ugplot_collaboration_read_index(jobs_dir)
  open_ids <- as.character(index$task_id[index$state %in% c("pending", "leased")])
  tasks <- Filter(Negate(is.null), lapply(open_ids, ugplot_collaboration_refresh_task, jobs_dir = jobs_dir))
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
  index <- ugplot_collaboration_read_index(jobs_dir)
  states <- table(as.character(index$state))
  state_count <- function(name) {
    value <- suppressWarnings(as.integer(states[name]))
    if (length(value) == 0L || is.na(value)) 0L else value
  }
  list(
    status = "open",
    protocol_version = 2L,
    pending = sum(pending_active),
    inactive_pending = length(pending_active) - sum(pending_active),
    leased = state_count("leased"),
    completed = state_count("completed"),
    lease_seconds = 120L
  )
}

ugplot_collaboration_compatibility <- function(capabilities = list(),
                                               jobs_dir = ugplot_default_jobs_dir()) {
  task_ids <- ugplot_collaboration_task_ids(jobs_dir, states = "pending")
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
    protocol_version = 2L,
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
                                                    jobs_dir = ugplot_default_jobs_dir(),
                                                    inspect_collaboration = TRUE) {
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
    run_key <- ugplot_geo_transcript_ml_run_key(
      target, threshold, min_samples,
      config$geo_metadata_numeric_predictors %||% character(0),
      config$geo_metadata_categorical_predictors %||% character(0)
    )
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
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  active_server_groups <- ugplot_active_distributed_group_ids(status)
  # The focused monitor deliberately avoids opening two task files for every
  # transcript group.  Keep that path cheap while still consulting the compact
  # collaboration index for the handful of currently leased missions.
  active_collaboration <- if (!isTRUE(inspect_collaboration)) {
    tryCatch(
      ugplot_collaboration_active_contributors(job_id, jobs_dir),
      error = function(e) data.frame()
    )
  } else {
    data.frame()
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
    task <- if (isTRUE(inspect_collaboration)) {
      complete_task <- ugplot_collaboration_read_task(paste(job_id, "analyze", group_id, sep = ":"), jobs_dir)
      if (is.list(complete_task)) complete_task else ugplot_collaboration_read_task(paste(job_id, "screen", group_id, sep = ":"), jobs_dir)
    } else {
      NULL
    }
    task <- if (is.list(task)) ugplot_collaboration_reap_task(task) else NULL
    task_state <- if (is.list(task)) as.character(task$state %||% "") else ""
    contributor_index <- if (is.data.frame(active_collaboration) &&
                             nrow(active_collaboration) > 0L &&
                             "group_id" %in% names(active_collaboration)) {
      match(group_id, as.character(active_collaboration$group_id))
    } else {
      NA_integer_
    }
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
    } else if (!is.na(contributor_index)) {
      contributor <- active_collaboration[contributor_index, , drop = FALSE]
      state <- "processing"
      executor <- as.character(contributor$executor[[1]] %||% "Public scientist")
      executor_type <- "collaboration"
      progress <- suppressWarnings(as.numeric(contributor$progress[[1]] %||% 0))
      if (!is.finite(progress)) progress <- 0
      message <- as.character(contributor$message[[1]] %||% "Collaborative experiment running")
      candidate <- as.character(contributor$candidate[[1]] %||% "")
      if (nzchar(candidate)) message <- paste(message, "—", candidate)
    } else if (manifest_state %in% c("dispatching", "submitted", "running") &&
               group_id %in% active_server_groups) {
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

ugplot_collaboration_active_contributors <- function(job_id,
                                                     jobs_dir = ugplot_default_jobs_dir(),
                                                     max_candidates = 128L) {
  job_id <- ugplot_validate_job_id(job_id)
  root <- ugplot_collaboration_dir(jobs_dir)
  if (!dir.exists(root)) return(data.frame())
  task_ids <- ugplot_collaboration_task_ids(jobs_dir, states = "leased", parent_job_id = job_id)
  task_dirs <- file.path(root, gsub("[^A-Za-z0-9._-]", "_", task_ids))
  if (length(task_dirs) == 0L) return(data.frame())
  task_paths <- file.path(task_dirs, "task.rds")
  info <- suppressWarnings(file.info(task_paths))
  task_paths <- task_paths[!is.na(info$mtime)]
  info <- info[!is.na(info$mtime), , drop = FALSE]
  if (length(task_paths) == 0L) return(data.frame())
  task_paths <- task_paths[order(info$mtime, decreasing = TRUE)]
  max_candidates <- suppressWarnings(as.integer(max_candidates))
  if (is.na(max_candidates) || max_candidates < 1L) max_candidates <- 128L
  task_paths <- utils::head(task_paths, max_candidates)
  rows <- Filter(Negate(is.null), lapply(task_paths, function(path) {
    task <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.list(task) || !identical(as.character(task$parent_job_id %||% ""), job_id)) return(NULL)
    task <- ugplot_collaboration_reap_task(task)
    if (!identical(as.character(task$state %||% ""), "leased")) return(NULL)
    progress <- suppressWarnings(as.numeric(task$client_progress %||% 0))
    if (!is.finite(progress)) progress <- 0
    task_id <- as.character(task$task_id %||% "")
    task_id_prefixes <- paste0(job_id, c(":analyze:", ":screen:"))
    matched_prefix <- task_id_prefixes[startsWith(task_id, task_id_prefixes)]
    group_id <- if (length(matched_prefix) > 0L) {
      substring(task_id, nchar(matched_prefix[[1]]) + 1L)
    } else task_id
    data.frame(
      group_id = group_id,
      executor = as.character(task$scientist_name %||% task$client_id %||% "Public scientist"),
      executor_type = "collaboration",
      progress = max(0, min(1, progress)),
      message = as.character(task$client_message %||% "Collaborative experiment running"),
      candidate = as.character(task$client_candidate %||% ""),
      stringsAsFactors = FALSE
    )
  }))
  if (length(rows) == 0L) data.frame() else do.call(rbind, rows)
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
  allowed_runners <- c("ugplot_run_geo_complete_group_job", "ugplot_run_geo_screen_group_job")
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
