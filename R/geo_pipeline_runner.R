if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(lhs, rhs) {
    if (is.null(lhs)) rhs else lhs
  }
}

ugplot_geo_safe_token <- function(value) {
  token <- gsub("[^A-Za-z0-9._-]+", "_", as.character(value %||% ""))
  token <- gsub("^_+|_+$", "", token)
  if (!nzchar(token)) "value" else token
}

ugplot_geo_analysis_dir <- function(cache_dir, source = "processed", create = TRUE) {
  path <- file.path(cache_dir, "analysis", ugplot_geo_safe_token(source))
  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

ugplot_geo_spearman_paths <- function(cache_dir, target_column, source = "processed", create = TRUE) {
  analysis_dir <- ugplot_geo_analysis_dir(cache_dir, source, create = create)
  safe_target <- ugplot_geo_safe_token(target_column)
  list(
    raw = file.path(analysis_dir, paste0("ugplot_geo_spearman_", safe_target, ".csv")),
    annotated = file.path(analysis_dir, paste0("ugplot_geo_spearman_", safe_target, "_annotated.csv")),
    by_transcript = file.path(analysis_dir, paste0("ugplot_geo_spearman_", safe_target, "_by_transcript.csv")),
    by_gene = file.path(analysis_dir, paste0("ugplot_geo_spearman_", safe_target, "_by_gene.csv"))
  )
}

ugplot_geo_transcript_cache_version <- function() {
  "reader_v3"
}

ugplot_geo_transcript_missing_definition <- function() {
  c("empty", "na", "zero")
}

ugplot_geo_group_key <- function(values) {
  paste(sort(unique(as.character(values))), collapse = "\r")
}

ugplot_geo_transcript_dataset_path <- function(cache_dir, transcript, target_column, source = "processed", raw = FALSE) {
  transcript_dir <- file.path(
    ugplot_geo_analysis_dir(cache_dir, source),
    "transcript_datasets",
    ugplot_geo_safe_token(target_column),
    if (isTRUE(raw)) "_raw" else ""
  )
  dir.create(transcript_dir, recursive = TRUE, showWarnings = FALSE)
  suffix <- if (isTRUE(raw)) "_raw.csv" else ".csv"
  file.path(transcript_dir, paste0(ugplot_geo_safe_token(transcript), suffix))
}

ugplot_geo_transcript_group_paths <- function(cache_dir, target_column, threshold, min_samples_pct, source = "processed") {
  safe_target <- ugplot_geo_safe_token(target_column)
  safe_threshold <- ugplot_geo_safe_token(format(threshold, trim = TRUE, scientific = FALSE))
  safe_min_samples <- ugplot_geo_safe_token(format(min_samples_pct, trim = TRUE, scientific = FALSE))
  safe_missing <- ugplot_geo_safe_token(paste(ugplot_geo_transcript_missing_definition(), collapse = "_"))
  prefix <- file.path(ugplot_geo_analysis_dir(cache_dir, source), paste0(
    "ugplot_geo_transcript_ml_groups_", safe_target,
    "_", ugplot_geo_transcript_cache_version(),
    "_absrho_", safe_threshold,
    "_minsamples_", safe_min_samples,
    "_missing_", safe_missing
  ))
  list(
    summary = paste0(prefix, "_summary.csv"),
    details = paste0(prefix, "_details.csv"),
    progress = paste0(prefix, "_progress.rds")
  )
}

ugplot_geo_transcript_ml_dir <- function(cache_dir, source = "processed") {
  path <- file.path(ugplot_geo_analysis_dir(cache_dir, source), "transcript_ml_pipeline")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

ugplot_geo_transcript_ml_group_dir <- function(cache_dir, source, group_id) {
  path <- file.path(ugplot_geo_transcript_ml_dir(cache_dir, source), ugplot_geo_safe_token(group_id))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

ugplot_geo_bind_rows <- function(rows) {
  rows <- rows[vapply(rows, is.data.frame, logical(1))]
  rows <- rows[vapply(rows, nrow, integer(1)) > 0]
  if (length(rows) == 0) {
    return(data.frame())
  }
  all_columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  normalized <- lapply(rows, function(row) {
    for (column_name in setdiff(all_columns, names(row))) {
      row[[column_name]] <- NA
    }
    row[, all_columns, drop = FALSE]
  })
  result <- do.call(rbind, normalized)
  rownames(result) <- NULL
  result
}

ugplot_geo_build_group_tables_remote <- function(progress_rows, candidates = NULL) {
  compatible <- progress_rows[progress_rows$Status == "compatible", , drop = FALSE]
  if (!is.data.frame(compatible) || nrow(compatible) == 0) {
    return(list(summary = data.frame(), details = data.frame()))
  }
  compatible$GroupKey <- paste(compatible$CpGKey, compatible$SampleKey, sep = "\f")
  group_keys <- unique(compatible$GroupKey)
  summary_rows <- lapply(seq_along(group_keys), function(group_index) {
    group_df <- compatible[compatible$GroupKey == group_keys[[group_index]], , drop = FALSE]
    group_df <- group_df[order(-group_df$TriggerMaxAbsRho, -group_df$Columns, -group_df$Samples, group_df$Transcript), , drop = FALSE]
    principal <- group_df[1, , drop = FALSE]
    data.frame(
      GroupID = paste0("TG", group_index),
      PrincipalTranscript = principal$Transcript[[1]],
      Gene = principal$Gene[[1]],
      Columns = principal$Columns[[1]],
      Samples = principal$Samples[[1]],
      TranscriptCount = nrow(group_df),
      ExtraTranscripts = paste(setdiff(group_df$Transcript, principal$Transcript[[1]]), collapse = ";"),
      CpGs = principal$KeptCpGs[[1]],
      TriggerMaxAbsRho = principal$TriggerMaxAbsRho[[1]],
      TriggerBestCpG = principal$TriggerBestCpG[[1]] %||% "",
      TriggerBestRho = suppressWarnings(as.numeric(principal$TriggerBestRho[[1]] %||% NA_real_)),
      DatasetPath = principal$DatasetPath[[1]],
      GroupKey = group_keys[[group_index]],
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, summary_rows)
  summary <- summary[order(-summary$TriggerMaxAbsRho, -summary$Columns, -summary$Samples, summary$PrincipalTranscript), , drop = FALSE]
  summary$GroupID <- paste0("TG", seq_len(nrow(summary)))
  detail_rows <- lapply(seq_len(nrow(compatible)), function(i) {
    row <- compatible[i, , drop = FALSE]
    group_id <- summary$GroupID[match(row$GroupKey[[1]], summary$GroupKey)]
    transcript_id <- as.character(row$Transcript[[1]])
    kept_cpgs <- unlist(strsplit(as.character(row$KeptCpGs[[1]] %||% ""), ";", fixed = TRUE), use.names = FALSE)
    kept_cpgs <- kept_cpgs[nzchar(kept_cpgs)]
    candidate_rows <- data.frame()
    if (is.data.frame(candidates) && nrow(candidates) > 0 && all(c("Transcript", "CpG") %in% names(candidates))) {
      candidate_rows <- candidates[as.character(candidates$Transcript) == transcript_id, , drop = FALSE]
    }
    if (!is.data.frame(candidate_rows) || nrow(candidate_rows) == 0) {
      candidate_rows <- data.frame(CpG = kept_cpgs, stringsAsFactors = FALSE)
    }
    if (!"Transcript" %in% names(candidate_rows)) candidate_rows$Transcript <- transcript_id
    if (!"Gene" %in% names(candidate_rows)) candidate_rows$Gene <- row$Gene[[1]]
    candidate_rows$GroupID <- group_id
    candidate_rows$DatasetPath <- row$DatasetPath[[1]]
    candidate_rows$CpGKeptForML <- as.character(candidate_rows$CpG) %in% kept_cpgs
    candidate_rows$KeptCpGs <- row$KeptCpGs[[1]]
    leading <- intersect(
      c("GroupID", "Transcript", "CpG", "Gene", "GeneRegion", "Chr", "Position", "SpearmanRho", "AbsRho", "PValue", "N", "CpGKeptForML", "DatasetPath", "KeptCpGs"),
      names(candidate_rows)
    )
    candidate_rows[, c(leading, setdiff(names(candidate_rows), leading)), drop = FALSE]
  })
  details <- ugplot_geo_bind_rows(detail_rows)
  rownames(summary) <- NULL
  rownames(details) <- NULL
  list(summary = summary, details = details)
}

ugplot_geo_filter_transcript_dataset <- function(dataset, target_column, min_samples_pct) {
  if (!is.data.frame(dataset) || nrow(dataset) == 0 || !target_column %in% names(dataset)) {
    return(list(status = "dataset_unavailable", dataset = data.frame(), kept_cpgs = character(0), kept_samples = character(0)))
  }
  missing_definition <- ugplot_geo_transcript_missing_definition()
  target_missing <- if (exists("build_missing_mask", mode = "function", inherits = TRUE)) {
    build_missing_mask(dataset[, target_column, drop = FALSE], missing_definition = missing_definition)[, 1]
  } else {
    is.na(dataset[[target_column]]) | !nzchar(as.character(dataset[[target_column]]))
  }
  analysis_dataset <- dataset[!target_missing, , drop = FALSE]
  predictor_cols <- setdiff(names(analysis_dataset), c("sample_id", target_column))
  if (length(predictor_cols) == 0 || nrow(analysis_dataset) == 0) {
    return(list(status = "no_predictors", dataset = data.frame(), kept_cpgs = character(0), kept_samples = character(0)))
  }
  predictors <- analysis_dataset[, predictor_cols, drop = FALSE]
  min_samples_required <- max(3L, ceiling((min_samples_pct / 100) * nrow(analysis_dataset)))
  if (exists("compute_exhaustive_threshold_scan", mode = "function", inherits = TRUE) &&
      exists("apply_missing_filters_with_order", mode = "function", inherits = TRUE)) {
    scan <- compute_exhaustive_threshold_scan(
      predictors = predictors,
      missing_definition = missing_definition,
      min_rows_retained = min(1, min_samples_required / max(1, nrow(analysis_dataset))),
      mode = "complete_case"
    )
    if (is.data.frame(scan) && nrow(scan) > 0) {
      best <- scan[1, , drop = FALSE]
      if (isTRUE(best$complete_case[[1]]) && isTRUE(best$meets_min_samples[[1]]) &&
          best$n_rows_after[[1]] >= min_samples_required) {
        filtered <- apply_missing_filters_with_order(
          predictors = predictors,
          missing_definition = missing_definition,
          threshold_cols = best$thr_col[[1]],
          threshold_rows = best$thr_row[[1]],
          order = as.character(best$scan_order[[1]])
        )
        filtered_dataset <- cbind(
          analysis_dataset[filtered$keep_rows, c("sample_id", target_column), drop = FALSE],
          filtered$filtered_predictors
        )
        return(list(
          status = "compatible",
          dataset = filtered_dataset,
          kept_cpgs = colnames(filtered$filtered_predictors),
          kept_samples = analysis_dataset$sample_id[filtered$keep_rows]
        ))
      }
    }
    return(list(status = "no_complete_case_at_min_samples", dataset = data.frame(), kept_cpgs = character(0), kept_samples = character(0)))
  }

  complete <- stats::complete.cases(predictors) & !is.na(analysis_dataset[[target_column]])
  if (sum(complete) < min_samples_required) {
    return(list(status = "no_complete_case_at_min_samples", dataset = data.frame(), kept_cpgs = character(0), kept_samples = character(0)))
  }
  filtered_dataset <- analysis_dataset[complete, c("sample_id", target_column, predictor_cols), drop = FALSE]
  list(status = "compatible", dataset = filtered_dataset, kept_cpgs = predictor_cols, kept_samples = filtered_dataset$sample_id)
}

ugplot_geo_build_transcript_groups_remote <- function(candidates, matrix_files, metadata, cache_dir, target_column,
                                                      threshold, min_samples_pct, source = "processed",
                                                      progress_callback = NULL) {
  transcripts <- unique(as.character(stats::na.omit(candidates$Transcript)))
  transcripts <- transcripts[nzchar(transcripts)]
  paths <- ugplot_geo_transcript_group_paths(cache_dir, target_column, threshold, min_samples_pct, source)
  progress_rows <- if (file.exists(paths$progress)) {
    tryCatch(readRDS(paths$progress), error = function(e) data.frame())
  } else {
    data.frame()
  }
  processed <- if (is.data.frame(progress_rows) && "Transcript" %in% names(progress_rows)) {
    unique(as.character(progress_rows$Transcript))
  } else {
    character(0)
  }
  for (transcript_id in setdiff(transcripts, processed)) {
    transcript_rows <- candidates[as.character(candidates$Transcript) == transcript_id, , drop = FALSE]
    transcript_cpgs <- unique(as.character(stats::na.omit(transcript_rows$CpG)))
    transcript_cpgs <- transcript_cpgs[nzchar(transcript_cpgs)]
    dataset_path <- ugplot_geo_transcript_dataset_path(cache_dir, transcript_id, target_column, source)
    raw_dataset_path <- ugplot_geo_transcript_dataset_path(cache_dir, transcript_id, target_column, source, raw = TRUE)
    transcript_dataset <- if (file.exists(raw_dataset_path)) {
      tryCatch(utils::read.csv(raw_dataset_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    } else {
      tryCatch(
        ugplot_geo_transcript_dataset(matrix_files, metadata, target_column, transcript_cpgs),
        error = function(e) data.frame()
      )
    }
    if (is.data.frame(transcript_dataset) && nrow(transcript_dataset) > 0 && !file.exists(raw_dataset_path)) {
      utils::write.csv(transcript_dataset, raw_dataset_path, row.names = FALSE)
    }
    filtered <- ugplot_geo_filter_transcript_dataset(transcript_dataset, target_column, min_samples_pct)
    if (identical(filtered$status, "compatible")) {
      utils::write.csv(filtered$dataset, dataset_path, row.names = FALSE)
    }
    trigger_max <- suppressWarnings(max(transcript_rows$TriggerMaxAbsRho, transcript_rows$AbsRho, na.rm = TRUE))
    if (!is.finite(trigger_max)) {
      trigger_max <- NA_real_
    }
    progress_row <- data.frame(
      Transcript = transcript_id,
      Gene = paste(unique(stats::na.omit(transcript_rows$Gene)), collapse = ";"),
      Status = filtered$status,
      Columns = length(filtered$kept_cpgs),
      Samples = length(filtered$kept_samples),
      KeptCpGs = paste(filtered$kept_cpgs, collapse = ";"),
      CpGKey = ugplot_geo_group_key(filtered$kept_cpgs),
      SampleKey = ugplot_geo_group_key(filtered$kept_samples),
      TriggerMaxAbsRho = trigger_max,
      TriggerBestCpG = if ("TriggerBestCpG" %in% names(transcript_rows)) transcript_rows$TriggerBestCpG[[1]] else transcript_rows$CpG[[1]] %||% "",
      TriggerBestRho = if ("TriggerBestRho" %in% names(transcript_rows)) suppressWarnings(as.numeric(transcript_rows$TriggerBestRho[[1]])) else suppressWarnings(as.numeric(transcript_rows$SpearmanRho[[1]] %||% NA_real_)),
      DatasetPath = if (identical(filtered$status, "compatible")) dataset_path else "",
      RawDatasetPath = raw_dataset_path,
      stringsAsFactors = FALSE
    )
    progress_rows <- ugplot_geo_bind_rows(list(progress_rows, progress_row))
    tables <- ugplot_geo_build_group_tables_remote(progress_rows, candidates = candidates)
    utils::write.csv(tables$summary, paths$summary, row.names = FALSE)
    utils::write.csv(tables$details, paths$details, row.names = FALSE)
    saveRDS(progress_rows, paths$progress)
    if (!is.null(progress_callback)) {
      progress_callback(
        min(1, nrow(progress_rows) / max(1, length(transcripts))),
        paste0("Transcript datasets: ", nrow(progress_rows), "/", length(transcripts), "; groups ", nrow(tables$summary))
      )
    }
  }
  tables <- ugplot_geo_build_group_tables_remote(progress_rows, candidates = candidates)
  utils::write.csv(tables$summary, paths$summary, row.names = FALSE)
  utils::write.csv(tables$details, paths$details, row.names = FALSE)
  list(summary = tables$summary, details = tables$details, paths = paths, progress = progress_rows)
}

ugplot_geo_ml_metric_values <- function(result) {
  if (!is.list(result) || !is.data.frame(result$results_table) || nrow(result$results_table) == 0) {
    return(numeric(0))
  }
  rows <- result$results_table
  if ("Status" %in% names(rows)) {
    rows <- rows[as.character(rows$Status) == "OK", , drop = FALSE]
  }
  metric_col <- if ("R2" %in% names(rows)) "R2" else if ("Accuracy" %in% names(rows)) "Accuracy" else ""
  if (!nzchar(metric_col)) {
    return(numeric(0))
  }
  values <- suppressWarnings(as.numeric(rows[[metric_col]]))
  values[is.finite(values)]
}

ugplot_geo_ml_model_run_counts <- function(result) {
  results_table <- result$results_table
  if (!is.data.frame(results_table) || nrow(results_table) == 0 || !"Model" %in% names(results_table)) {
    return(c(ModelsRun = NA_integer_, ModelsOK = NA_integer_))
  }
  models_run <- length(unique(as.character(results_table$Model[nzchar(as.character(results_table$Model))])))
  models_ok <- if ("Status" %in% names(results_table)) {
    status_ok <- !is.na(results_table$Status) & as.character(results_table$Status) == "OK"
    length(unique(as.character(results_table$Model[status_ok])))
  } else {
    NA_integer_
  }
  c(ModelsRun = models_run, ModelsOK = models_ok)
}

ugplot_geo_ml_importance_table <- function(model, group, source, phase) {
  if (is.null(model)) {
    return(data.frame())
  }
  importance <- tryCatch(caret::varImp(model), error = function(e) NULL)
  if (is.null(importance) || !is.data.frame(importance$importance) || nrow(importance$importance) == 0) {
    return(data.frame())
  }
  imp <- importance$importance
  imp$CpG <- rownames(imp)
  score_cols <- setdiff(names(imp), "CpG")
  imp$Importance <- if (length(score_cols) == 1) {
    suppressWarnings(as.numeric(imp[[score_cols[[1]]]]))
  } else {
    apply(imp[, score_cols, drop = FALSE], 1, function(x) max(suppressWarnings(as.numeric(x)), na.rm = TRUE))
  }
  imp <- imp[, c("CpG", "Importance"), drop = FALSE]
  imp <- imp[is.finite(imp$Importance), , drop = FALSE]
  if (nrow(imp) == 0) {
    return(data.frame())
  }
  imp$ImportanceRank <- rank(-imp$Importance, ties.method = "first")
  imp$GroupID <- group$GroupID[[1]]
  imp$PrincipalTranscript <- group$PrincipalTranscript[[1]]
  imp$Source <- source
  imp$Phase <- phase
  imp[order(imp$ImportanceRank), c("Source", "GroupID", "PrincipalTranscript", "Phase", "CpG", "Importance", "ImportanceRank"), drop = FALSE]
}

ugplot_geo_enrich_ml_summary_remote <- function(summary, source, phase = c("screening", "stability")) {
  phase <- match.arg(phase)
  if (!is.data.frame(summary) || nrow(summary) == 0) {
    return(summary)
  }
  if (!"ModelsRun" %in% names(summary)) summary$ModelsRun <- NA_integer_
  if (!"ModelsOK" %in% names(summary)) summary$ModelsOK <- NA_integer_
  if (!"ImportancePath" %in% names(summary)) summary$ImportancePath <- ""
  result_column <- if (identical(phase, "stability")) "StabilityResultPath" else "ScreenResultPath"
  for (row_i in seq_len(nrow(summary))) {
    result_path <- if (result_column %in% names(summary)) as.character(summary[[result_column]][[row_i]] %||% "") else ""
    if (is.na(result_path) || !nzchar(result_path) || !file.exists(result_path)) {
      next
    }
    result <- tryCatch(readRDS(result_path), error = function(e) NULL)
    if (!is.list(result)) {
      next
    }
    counts <- ugplot_geo_ml_model_run_counts(result)
    if (is.na(summary$ModelsRun[[row_i]])) summary$ModelsRun[[row_i]] <- counts[["ModelsRun"]]
    if (is.na(summary$ModelsOK[[row_i]])) summary$ModelsOK[[row_i]] <- counts[["ModelsOK"]]
    importance_path <- as.character(summary$ImportancePath[[row_i]] %||% "")
    if (is.na(importance_path) || !nzchar(importance_path) || !file.exists(importance_path)) {
      group_dir <- dirname(result_path)
      importance_path <- file.path(group_dir, if (identical(phase, "stability")) "importance.csv" else "screen_importance.csv")
      model <- result$best_model
      importance <- ugplot_geo_ml_importance_table(model, summary[row_i, , drop = FALSE], source, phase)
      if (is.data.frame(importance) && nrow(importance) > 0) {
        utils::write.csv(importance, importance_path, row.names = FALSE)
        summary$ImportancePath[[row_i]] <- importance_path
      }
    }
  }
  summary
}

ugplot_geo_collect_ml_importance_remote <- function(summary) {
  if (!is.data.frame(summary) || nrow(summary) == 0 || !"ImportancePath" %in% names(summary)) {
    return(data.frame())
  }
  rows <- lapply(seq_len(nrow(summary)), function(row_i) {
    path <- as.character(summary$ImportancePath[[row_i]] %||% "")
    if (is.na(path) || !nzchar(path) || !file.exists(path)) {
      return(data.frame())
    }
    importance <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    if (!is.data.frame(importance) || nrow(importance) == 0) {
      return(data.frame())
    }
    if (!"GroupID" %in% names(importance) && "GroupID" %in% names(summary)) {
      importance$GroupID <- summary$GroupID[[row_i]]
    }
    if (!"Phase" %in% names(importance) && "Phase" %in% names(summary)) {
      importance$Phase <- summary$Phase[[row_i]]
    }
    if (!"Source" %in% names(importance) && "Source" %in% names(summary)) {
      importance$Source <- summary$Source[[row_i]]
    }
    importance
  })
  ugplot_geo_bind_rows(rows)
}

ugplot_geo_ml_rank_summary <- function(summary) {
  if (!is.data.frame(summary) || nrow(summary) == 0) {
    return(data.frame())
  }
  metric <- suppressWarnings(as.numeric(summary$MedianMetric %||% summary$BestMetric))
  trigger <- suppressWarnings(as.numeric(summary$TriggerMaxAbsRho %||% NA_real_))
  summary <- summary[order(-metric, -trigger, summary$GroupID, na.last = TRUE), , drop = FALSE]
  summary$Rank <- seq_len(nrow(summary))
  rownames(summary) <- NULL
  summary
}

ugplot_geo_ml_pipeline_config <- function(models, seed_end, timeout, best_only_model = NULL,
                                          cpu_limit = 1L, parallel_enabled = FALSE,
                                          restart_parallel_each_model = TRUE,
                                          retry_parallel_connection_errors = TRUE) {
  cpu_limit <- suppressWarnings(as.integer(cpu_limit %||% 1L))
  if (is.na(cpu_limit) || cpu_limit < 1L) {
    cpu_limit <- 1L
  }
  list(
    target = "target",
    models = if (is.null(best_only_model)) models else best_only_model,
    dataset_seed_start = 1,
    dataset_seed_end = 1,
    training_seed_start = 1,
    training_seed_end = seed_end,
    timeout = timeout,
    performance_mode = "default",
    missing_definition = ugplot_geo_transcript_missing_definition(),
    missing_strategy = "none",
    missing_threshold_cols = 100,
    missing_threshold_rows = 100,
    complete_case_min_samples = 0,
    imputation_scope = "split_separate",
    cpu_limit = cpu_limit,
    parallel_enabled = isTRUE(parallel_enabled) && cpu_limit > 1L,
    use_callr_timeout = TRUE,
    restart_parallel_each_model = isTRUE(restart_parallel_each_model),
    retry_parallel_connection_errors = isTRUE(retry_parallel_connection_errors)
  )
}

ugplot_geo_ml_group_dataset <- function(group) {
  dataset_path <- as.character(group$DatasetPath[[1]])
  if (!nzchar(dataset_path) || !file.exists(dataset_path)) {
    stop("Transcript group dataset file is missing: ", dataset_path, call. = FALSE)
  }
  dataset <- utils::read.csv(dataset_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"target" %in% names(dataset)) {
    target_candidates <- setdiff(names(dataset), c("sample_id", grep("^cg", names(dataset), value = TRUE)))
    target_name <- target_candidates[[1]] %||% ""
    if (!nzchar(target_name)) {
      stop("Could not identify target column in transcript dataset.", call. = FALSE)
    }
    names(dataset)[names(dataset) == target_name] <- "target"
  }
  sample_count <- nrow(dataset)
  dataset <- dataset[, setdiff(names(dataset), "sample_id"), drop = FALSE]
  list(dataset = dataset, dataset_path = dataset_path, sample_count = sample_count)
}

ugplot_geo_ml_quick_models <- function(available) {
  available <- unique(as.character(available))
  available <- available[nzchar(available)]
  families <- list(
    linear = c("glmnet", "lm", "ridge", "lasso"),
    tree = c("rpart", "rf", "ranger"),
    boosting = c("xgbTree", "gbm"),
    neural = c("nnet", "avNNet")
  )
  selected <- vapply(families, function(candidates) {
    hit <- intersect(candidates, available)
    if (length(hit) > 0) hit[[1]] else NA_character_
  }, character(1), USE.NAMES = FALSE)
  selected <- stats::na.omit(selected)
  if (length(selected) < 4) {
    selected <- unique(c(selected, utils::head(setdiff(available, selected), 4 - length(selected))))
  }
  unique(as.character(selected))
}

ugplot_geo_run_transcript_ml_remote <- function(groups, cache_dir, source = "processed", config = list(),
                                                progress_callback = NULL) {
  if (!is.data.frame(groups) || nrow(groups) == 0) {
    return(data.frame())
  }
  min_absrho <- suppressWarnings(as.numeric(config$geo_ml_min_absrho %||% 0.7))
  if (!is.finite(min_absrho)) min_absrho <- 0.7
  eligible <- groups[suppressWarnings(as.numeric(groups$TriggerMaxAbsRho)) >= min_absrho, , drop = FALSE]
  eligible <- eligible[order(-suppressWarnings(as.numeric(eligible$TriggerMaxAbsRho)), eligible$GroupID), , drop = FALSE]
  rank_limit <- suppressWarnings(as.integer(config$geo_ml_rank_limit %||% NA_integer_))
  if (is.finite(rank_limit) && rank_limit > 0 && nrow(eligible) > rank_limit) {
    eligible <- utils::head(eligible, rank_limit)
  }
  if (!is.data.frame(eligible) || nrow(eligible) == 0) {
    return(data.frame())
  }
  models <- unique(as.character(config$models %||% character(0)))
  models <- models[nzchar(models)]
  if (length(models) == 0) {
    models <- c("glmnet", "rpart")
  }
  if (isTRUE(config$geo_ml_quick_models)) {
    models <- ugplot_geo_ml_quick_models(models)
  }
  screen_seeds <- max(1L, as.integer(config$geo_ml_screen_seeds %||% 3))
  timeout <- max(1, as.numeric(config$geo_ml_timeout %||% 1200))
  cpu_limit <- suppressWarnings(as.integer(config$cpu_limit %||% 1L))
  if (is.na(cpu_limit) || cpu_limit < 1L) {
    cpu_limit <- 1L
  }
  parallel_enabled <- isTRUE(config$parallel_enabled)
  restart_parallel_each_model <- isTRUE(config$restart_parallel_each_model %||% TRUE)
  retry_parallel_connection_errors <- isTRUE(config$retry_parallel_connection_errors %||% TRUE)
  pipeline_dir <- ugplot_geo_transcript_ml_dir(cache_dir, source)
  summary_path <- file.path(pipeline_dir, "screening_summary.csv")
  summaries <- if (file.exists(summary_path)) {
    tryCatch(utils::read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  } else {
    data.frame()
  }
  processed_groups <- if (is.data.frame(summaries) && "GroupID" %in% names(summaries)) unique(as.character(summaries$GroupID)) else character(0)
  for (group_i in seq_len(nrow(eligible))) {
    group <- eligible[group_i, , drop = FALSE]
    group_id <- as.character(group$GroupID[[1]])
    if (group_id %in% processed_groups) {
      next
    }
    dataset_info <- ugplot_geo_ml_group_dataset(group)
    group_dir <- ugplot_geo_transcript_ml_group_dir(cache_dir, source, group_id)
    screen_path <- file.path(group_dir, "screen_result.rds")
    screen_config <- ugplot_geo_ml_pipeline_config(
      models,
      screen_seeds,
      timeout,
      cpu_limit = cpu_limit,
      parallel_enabled = parallel_enabled,
      restart_parallel_each_model = restart_parallel_each_model,
      retry_parallel_connection_errors = retry_parallel_connection_errors
    )
    screen_config$resume_result_path <- screen_path
    screen_config$model_log_dir <- file.path(group_dir, "logs", "screen")
    if (!is.null(progress_callback)) {
      progress_callback((group_i - 1) / nrow(eligible), paste0("Screening ", group_id, " with ", length(models), " model(s)"))
    }
    screen_result <- ugplot_run_ml_job(
      dataset_info$dataset,
      screen_config,
      progress_callback = function(...) {
        args <- list(...)
        if (!is.null(progress_callback)) {
          progress_callback((group_i - 1) / nrow(eligible), paste0("Screening ", group_id, ": ", args$message %||% ""))
        }
      },
      partial_callback = function(partial) saveRDS(partial, screen_path)
    )
    saveRDS(screen_result, screen_path)
    metric_values <- ugplot_geo_ml_metric_values(screen_result)
    model_counts <- ugplot_geo_ml_model_run_counts(screen_result)
    importance_path <- file.path(group_dir, "screen_importance.csv")
    importance <- ugplot_geo_ml_importance_table(screen_result$best_model, group, source, "screening")
    if (is.data.frame(importance) && nrow(importance) > 0) {
      utils::write.csv(importance, importance_path, row.names = FALSE)
    }
    summary_row <- data.frame(
      Source = source,
      Phase = "screening",
      GroupID = group_id,
      PrincipalTranscript = group$PrincipalTranscript[[1]],
      Gene = group$Gene[[1]],
      TriggerMaxAbsRho = suppressWarnings(as.numeric(group$TriggerMaxAbsRho[[1]])),
      TriggerBestCpG = group$TriggerBestCpG[[1]] %||% "",
      TriggerBestRho = suppressWarnings(as.numeric(group$TriggerBestRho[[1]] %||% NA_real_)),
      BestModel = screen_result$best_model_name %||% "",
      MetricName = screen_result$final_summary$metric_name %||% "R2",
      BestMetric = suppressWarnings(as.numeric(screen_result$final_summary$metric_value %||% NA_real_)),
      MedianMetric = if (length(metric_values) > 0) stats::median(metric_values) else NA_real_,
      MeanMetric = if (length(metric_values) > 0) mean(metric_values) else NA_real_,
      SeedsRun = length(metric_values),
      ModelsRun = model_counts[["ModelsRun"]],
      ModelsOK = model_counts[["ModelsOK"]],
      DatasetPath = dataset_info$dataset_path,
      ScreenResultPath = screen_path,
      ImportancePath = if (file.exists(importance_path)) importance_path else "",
      stringsAsFactors = FALSE
    )
    summaries <- ugplot_geo_bind_rows(list(summaries[as.character(summaries$GroupID) != group_id, , drop = FALSE], summary_row))
    summaries <- ugplot_geo_ml_rank_summary(summaries)
    utils::write.csv(summaries, summary_path, row.names = FALSE)
    processed_groups <- union(processed_groups, group_id)
  }
  summaries <- ugplot_geo_enrich_ml_summary_remote(summaries, source = source, phase = "screening")
  summaries <- ugplot_geo_ml_rank_summary(summaries)
  if (is.data.frame(summaries) && nrow(summaries) > 0) {
    utils::write.csv(summaries, summary_path, row.names = FALSE)
  }
  summaries
}

ugplot_geo_stability_state <- function(values, min_seeds, window, tolerance) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  n <- length(values)
  if (n < min_seeds || n < (2 * window)) {
    return(list(stable = FALSE, reason = paste0("collecting seeds: ", n)))
  }
  recent <- utils::tail(values, window)
  previous <- utils::tail(utils::head(values, n - window), window)
  mean_shift <- abs(mean(recent) - mean(previous))
  median_shift <- abs(stats::median(recent) - stats::median(previous))
  se <- stats::sd(values) / sqrt(n)
  stable <- is.finite(mean_shift) && is.finite(median_shift) &&
    mean_shift <= tolerance && median_shift <= tolerance &&
    (!is.finite(se) || se <= tolerance)
  list(stable = isTRUE(stable), reason = paste0("n=", n, "; delta mean=", signif(mean_shift, 4), "; delta median=", signif(median_shift, 4), "; SE=", signif(se, 4)))
}

ugplot_geo_run_transcript_stability_remote <- function(screen_summary, cache_dir, source = "processed", config = list(),
                                                       progress_callback = NULL) {
  if (!is.data.frame(screen_summary) || nrow(screen_summary) == 0) {
    return(data.frame())
  }
  min_seeds <- max(2L, as.integer(config$geo_ml_min_stability_seeds %||% 30))
  max_seeds <- max(min_seeds, as.integer(config$geo_ml_max_stability_seeds %||% 4000))
  window <- min(max_seeds, max(2L, as.integer(config$geo_ml_stability_window %||% 30)))
  tolerance <- max(0, as.numeric(config$geo_ml_stability_tolerance %||% 0.01))
  timeout <- max(1, as.numeric(config$geo_ml_timeout %||% 1200))
  cpu_limit <- suppressWarnings(as.integer(config$cpu_limit %||% 1L))
  if (is.na(cpu_limit) || cpu_limit < 1L) {
    cpu_limit <- 1L
  }
  parallel_enabled <- isTRUE(config$parallel_enabled)
  restart_parallel_each_model <- isTRUE(config$restart_parallel_each_model %||% TRUE)
  retry_parallel_connection_errors <- isTRUE(config$retry_parallel_connection_errors %||% TRUE)
  pipeline_dir <- ugplot_geo_transcript_ml_dir(cache_dir, source)
  summary_path <- file.path(pipeline_dir, "summary.csv")
  summaries <- if (file.exists(summary_path)) {
    tryCatch(utils::read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  } else {
    data.frame()
  }
  processed <- if (is.data.frame(summaries) && "GroupID" %in% names(summaries)) unique(as.character(summaries$GroupID)) else character(0)
  for (row_i in seq_len(nrow(screen_summary))) {
    row <- screen_summary[row_i, , drop = FALSE]
    group_id <- as.character(row$GroupID[[1]])
    if (group_id %in% processed) {
      next
    }
    best_model <- as.character(row$BestModel[[1]] %||% "")
    if (!nzchar(best_model) || identical(best_model, "-")) {
      next
    }
    dataset <- utils::read.csv(row$DatasetPath[[1]], stringsAsFactors = FALSE, check.names = FALSE)
    if (!"target" %in% names(dataset)) {
      target_candidates <- setdiff(names(dataset), c("sample_id", grep("^cg", names(dataset), value = TRUE)))
      names(dataset)[names(dataset) == target_candidates[[1]]] <- "target"
    }
    dataset <- dataset[, setdiff(names(dataset), "sample_id"), drop = FALSE]
    group_dir <- ugplot_geo_transcript_ml_group_dir(cache_dir, source, group_id)
    stability_path <- file.path(group_dir, "stability_result.rds")
    current_end <- min_seeds
    stability_result <- if (file.exists(stability_path)) tryCatch(readRDS(stability_path), error = function(e) NULL) else NULL
    stable_state <- list(stable = FALSE, reason = "not started")
    repeat {
      existing_n <- length(ugplot_geo_ml_metric_values(stability_result))
      current_end <- max(current_end, min(max_seeds, existing_n + window))
      stability_config <- ugplot_geo_ml_pipeline_config(
        best_model,
        current_end,
        timeout,
        best_only_model = best_model,
        cpu_limit = cpu_limit,
        parallel_enabled = parallel_enabled,
        restart_parallel_each_model = restart_parallel_each_model,
        retry_parallel_connection_errors = retry_parallel_connection_errors
      )
      stability_config$resume_result_path <- stability_path
      stability_config$model_log_dir <- file.path(group_dir, "logs", "stability")
      if (!is.null(progress_callback)) {
        progress_callback((row_i - 1) / nrow(screen_summary), paste0("Stability ", group_id, " with ", best_model, " to seed ", current_end))
      }
      stability_result <- ugplot_run_ml_job(
        dataset,
        stability_config,
        progress_callback = function(...) {
          args <- list(...)
          if (!is.null(progress_callback)) {
            progress_callback((row_i - 1) / nrow(screen_summary), paste0("Stability ", group_id, ": ", args$message %||% ""))
          }
        },
        partial_callback = function(partial) saveRDS(partial, stability_path)
      )
      saveRDS(stability_result, stability_path)
      metric_values <- ugplot_geo_ml_metric_values(stability_result)
      stable_state <- ugplot_geo_stability_state(metric_values, min_seeds, window, tolerance)
      if (isTRUE(stable_state$stable) || length(metric_values) >= max_seeds || current_end >= max_seeds) {
        break
      }
      current_end <- min(max_seeds, length(metric_values) + window)
    }
    metric_values <- ugplot_geo_ml_metric_values(stability_result)
    importance_path <- file.path(group_dir, "importance.csv")
    importance <- ugplot_geo_ml_importance_table(stability_result$best_model, row, source, "stability")
    if (is.data.frame(importance) && nrow(importance) > 0) {
      utils::write.csv(importance, importance_path, row.names = FALSE)
    }
    summary_row <- row
    summary_row$Phase <- "stability"
    summary_row$BestMetric <- suppressWarnings(as.numeric(stability_result$final_summary$metric_value %||% NA_real_))
    summary_row$MedianMetric <- if (length(metric_values) > 0) stats::median(metric_values) else NA_real_
    summary_row$MeanMetric <- if (length(metric_values) > 0) mean(metric_values) else NA_real_
    summary_row$MetricSE <- if (length(metric_values) > 1) stats::sd(metric_values) / sqrt(length(metric_values)) else NA_real_
    summary_row$SeedsRun <- length(metric_values)
    summary_row$Stable <- isTRUE(stable_state$stable)
    summary_row$StabilityDetail <- stable_state$reason
    summary_row$StabilityResultPath <- stability_path
    summary_row$ImportancePath <- if (file.exists(importance_path)) importance_path else ""
    summaries <- ugplot_geo_bind_rows(list(summaries[as.character(summaries$GroupID) != group_id, , drop = FALSE], summary_row))
    summaries <- ugplot_geo_ml_rank_summary(summaries)
    utils::write.csv(summaries, summary_path, row.names = FALSE)
    processed <- union(processed, group_id)
  }
  summaries <- ugplot_geo_enrich_ml_summary_remote(summaries, source = source, phase = "stability")
  summaries <- ugplot_geo_ml_rank_summary(summaries)
  if (is.data.frame(summaries) && nrow(summaries) > 0) {
    utils::write.csv(summaries, summary_path, row.names = FALSE)
  }
  summaries
}

ugplot_geo_download_selected_files <- function(remote_files, cache_dir, source = "processed", progress_callback = NULL) {
  source <- if (identical(source, "raw_sesame")) "raw_sesame" else "processed"
  selected <- if (identical(source, "raw_sesame")) {
    remote_files[remote_files$Type %in% c("IDAT", "archive"), , drop = FALSE]
  } else {
    remote_files[remote_files$Loadable, , drop = FALSE]
  }
  if (!is.data.frame(selected) || nrow(selected) == 0) {
    stop("No GEO files matched the selected matrix source.", call. = FALSE)
  }
  selected <- ugplot_geo_annotate_remote_files(selected, cache_dir)
  pending <- selected[selected$NeedsDownload, , drop = FALSE]
  if (!is.data.frame(pending) || nrow(pending) == 0) {
    return(ugplot_geo_annotate_remote_files(remote_files, cache_dir))
  }
  total_bytes <- sum(ugplot_geo_size_bytes(pending), na.rm = TRUE)
  completed_bytes <- 0
  for (file_i in seq_len(nrow(pending))) {
    remote_file <- pending[file_i, , drop = FALSE]
    destination <- file.path(cache_dir, remote_file$File[[1]])
    expected_size <- ugplot_geo_size_bytes(remote_file)[[1]]
    start_progress <- if (total_bytes > 0) completed_bytes / total_bytes else (file_i - 1) / nrow(pending)
    if (!is.null(progress_callback)) {
      progress_callback(start_progress, paste0("Downloading ", file_i, "/", nrow(pending), ": ", remote_file$File[[1]]))
    }
    last_progress_sent <- start_progress
    ugplot_geo_download_file(
      remote_file$URL[[1]],
      destination,
      expected_size = expected_size,
      progress_callback = function(downloaded_bytes, total_file_bytes) {
        if (is.null(progress_callback)) {
          return()
        }
        current_total <- if (is.finite(total_file_bytes) && total_file_bytes > 0) total_file_bytes else expected_size
        progress <- if (total_bytes > 0) {
          min(0.98, (completed_bytes + downloaded_bytes) / total_bytes)
        } else if (is.finite(current_total) && current_total > 0) {
          min(0.98, ((file_i - 1) + downloaded_bytes / current_total) / nrow(pending))
        } else {
          start_progress
        }
        if (progress < 0.98 && progress < last_progress_sent + 0.01) {
          return()
        }
        last_progress_sent <<- progress
        progress_callback(progress, paste0("Downloading ", remote_file$File[[1]]))
      }
    )
    if (is.finite(expected_size)) {
      completed_bytes <- completed_bytes + expected_size
    }
    if (!is.null(progress_callback)) {
      done_progress <- if (total_bytes > 0) min(0.98, completed_bytes / total_bytes) else file_i / nrow(pending)
      progress_callback(done_progress, paste0("Downloaded ", remote_file$File[[1]]))
    }
  }
  ugplot_geo_annotate_remote_files(remote_files, cache_dir)
}

ugplot_geo_extract_processed_files <- function(remote_files, cache_dir, progress_callback = NULL) {
  remote_files <- ugplot_geo_annotate_remote_files(remote_files, cache_dir)
  extract_files <- remote_files[
    remote_files$Loadable &
      remote_files$LocalStatus == "downloaded" &
      grepl("\\.gz$", remote_files$LocalPath, ignore.case = TRUE),
    ,
    drop = FALSE
  ]
  if (!is.data.frame(extract_files) || nrow(extract_files) == 0) {
    return(remote_files)
  }
  for (file_i in seq_len(nrow(extract_files))) {
    source_path <- extract_files$LocalPath[[file_i]]
    if (!is.null(progress_callback)) {
      progress_callback((file_i - 1) / nrow(extract_files), paste0("Extracting ", basename(source_path)))
    }
    ugplot_geo_extract_gzip(source_path)
  }
  if (!is.null(progress_callback)) {
    progress_callback(1, "Extraction complete")
  }
  ugplot_geo_annotate_remote_files(remote_files, cache_dir)
}

ugplot_run_geo_pipeline_job <- function(dataset, config = list(), progress_callback = function(...) NULL,
                                        partial_callback = NULL) {
  accession <- trimws(as.character(config$accession %||% ""))
  if (!nzchar(accession)) {
    stop("GEO accession is required for remote GEO pipeline.", call. = FALSE)
  }
  source <- as.character(config$matrix_source %||% "processed")
  source <- if (identical(source, "raw_sesame")) "raw_sesame" else "processed"
  target_column <- as.character(config$target_column %||% "")
  if (!nzchar(target_column) &&
      is.list(config$resume_result) &&
      identical(config$resume_result$kind %||% "", "geo_pipeline") &&
      nzchar(as.character(config$resume_result$target_column %||% ""))) {
    target_column <- as.character(config$resume_result$target_column)
  }
  cache_dir <- ugplot_geo_cache_dir(accession)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  result <- list(
    kind = "geo_pipeline",
    accession = accession,
    matrix_source = source,
    target_column = target_column,
    cache_dir = cache_dir,
    paths = list(),
    tables = list(),
    stage = "queued",
    updated_at = as.character(Sys.time()),
    settings = list(
      spearman_max_cpgs = config$spearman_max_cpgs %||% 0,
      spearman_min_samples_pct = config$spearman_min_samples_pct %||% 80,
      transcript_absrho_threshold = config$transcript_absrho_threshold %||% 0.8,
      transcript_min_samples = config$transcript_min_samples %||% 80,
      idat_detection_p = config$idat_detection_p %||% 0.05,
      idat_max_failed_fraction = config$idat_max_failed_fraction %||% 0.05,
      idat_sesame_prep = config$idat_sesame_prep %||% "QCDPB",
      geo_ml_min_absrho = config$geo_ml_min_absrho %||% 0.7,
      geo_ml_rank_limit = config$geo_ml_rank_limit %||% NA_integer_,
      geo_ml_screen_seeds = config$geo_ml_screen_seeds %||% 3,
      geo_ml_timeout = config$geo_ml_timeout %||% 1200,
      geo_ml_min_stability_seeds = config$geo_ml_min_stability_seeds %||% 30,
      geo_ml_max_stability_seeds = config$geo_ml_max_stability_seeds %||% 4000,
      geo_ml_stability_window = config$geo_ml_stability_window %||% 30,
      geo_ml_stability_tolerance = config$geo_ml_stability_tolerance %||% 0.01
    )
  )
  last_publish_progress <- -Inf
  last_publish_time <- Sys.time() - 60
  last_partial_time <- Sys.time() - 60
  publish <- function(progress, message, force = FALSE) {
    result$stage <<- message
    result$updated_at <<- as.character(Sys.time())
    now <- Sys.time()
    progress_value <- suppressWarnings(as.numeric(progress))
    progress_delta <- progress_value - last_publish_progress
    publish_elapsed <- as.numeric(difftime(now, last_publish_time, units = "secs"))
    should_publish <- isTRUE(force) ||
      isTRUE(progress_delta >= 0.01) ||
      isTRUE(publish_elapsed >= 10) ||
      isTRUE(is.finite(progress_value) && progress_value >= 1)
    if (should_publish) {
      progress_callback(progress = progress, message = message)
      last_publish_progress <<- progress_value
      last_publish_time <<- now
    }
    partial_elapsed <- as.numeric(difftime(now, last_partial_time, units = "secs"))
    if (!is.null(partial_callback) && (isTRUE(force) || isTRUE(partial_elapsed >= 30) || isTRUE(is.finite(progress_value) && progress_value >= 1))) {
      partial_callback(result)
      last_partial_time <<- now
    }
  }

  publish(0.02, paste0("Inspecting GEO accession ", accession), force = TRUE)
  remote_files <- ugplot_geo_remote_supp_files(accession)
  remote_files <- ugplot_geo_annotate_remote_files(remote_files, cache_dir)
  ugplot_geo_write_manifest(cache_dir, accession, remote_files)
  result$tables$remote_files <- remote_files

  publish(0.08, "Fetching sample metadata", force = TRUE)
  metadata <- ugplot_geo_fetch_sample_metadata(accession, cache_dir)
  result$tables$metadata_preview <- utils::head(metadata, 50)
  if (!nzchar(target_column)) {
    stop("Remote GEO pipeline requires a target metadata field selected by the client.", call. = FALSE)
  }

  publish(0.15, "Downloading selected GEO files on remote server", force = TRUE)
  remote_files <- ugplot_geo_download_selected_files(
    remote_files,
    cache_dir,
    source = source,
    progress_callback = function(value, message) publish(0.15 + 0.25 * value, message)
  )
  result$tables$remote_files <- remote_files

  if (identical(source, "raw_sesame")) {
    publish(0.42, "Reprocessing raw IDAT files with sesame", force = TRUE)
    sesame_result <- ugplot_geo_reprocess_idats_sesame(
      cache_dir = cache_dir,
      detection_p = as.numeric(config$idat_detection_p %||% 0.05),
      max_failed_probe_fraction = as.numeric(config$idat_max_failed_fraction %||% 0.05),
      prep = as.character(config$idat_sesame_prep %||% "QCDPB"),
      progress_callback = function(done, total, detail) {
        value <- if (is.finite(total) && total > 0) done / total else 0
        publish(0.42 + 0.18 * min(1, max(0, value)), detail)
      }
    )
    result$tables$idat_qc <- sesame_result$qc
    result$paths$sesame_beta <- sesame_result$beta_path
    result$paths$sesame_qc <- sesame_result$qc_path
  } else {
    publish(0.42, "Extracting processed matrix files", force = TRUE)
    remote_files <- ugplot_geo_extract_processed_files(
      remote_files,
      cache_dir,
      progress_callback = function(value, message) publish(0.42 + 0.18 * value, message)
    )
    result$tables$remote_files <- remote_files
  }

  matrix_files <- ugplot_geo_matrix_files(cache_dir, source = source)
  if (length(matrix_files) == 0) {
    stop("No matrix files are available after remote GEO preparation.", call. = FALSE)
  }
  result$paths$matrix_files <- matrix_files

  if (nzchar(target_column)) {
    publish(0.63, paste0("Running CpG Spearman scan for ", target_column), force = TRUE)
    sample_map <- ugplot_geo_matrix_sample_map(matrix_files, metadata)
    target <- suppressWarnings(as.numeric(as.character(metadata[[target_column]])))
    matched_numeric <- if (is.data.frame(sample_map) && nrow(sample_map) > 0) {
      sum(!is.na(target[sample_map$MetadataRow]))
    } else {
      0L
    }
    min_pct <- suppressWarnings(as.numeric(config$spearman_min_samples_pct %||% 80))
    min_matched <- max(3L, ceiling(matched_numeric * min_pct / 100))
    spearman_results <- ugplot_geo_spearman_scan(
      matrix_files,
      metadata,
      target_column,
      max_cpgs = as.integer(config$spearman_max_cpgs %||% 0),
      min_matched_samples = min_matched,
      progress_callback = function(scanned) {
        publish(0.63, paste0("Scanned ", format(scanned, big.mark = ","), " CpGs"))
      }
    )
    spearman_paths <- ugplot_geo_spearman_paths(cache_dir, target_column, source = source)
    utils::write.csv(spearman_results, spearman_paths$raw, row.names = FALSE)
    result$paths$spearman_raw <- spearman_paths$raw
    result$tables$spearman_preview <- utils::head(spearman_results, 100)

    publish(0.82, "Building/loading CpG annotation cache", force = TRUE)
    annotation_map <- ugplot_geo_build_annotation_cache(ugplot_geo_detect_platform(metadata))
    annotated <- ugplot_geo_join_spearman_annotation(spearman_results, annotation_map)
    utils::write.csv(annotated, spearman_paths$annotated, row.names = FALSE)
    transcript_summary <- ugplot_geo_group_spearman_annotation(annotated, "Transcript")
    gene_summary <- ugplot_geo_group_spearman_annotation(annotated, "Gene")
    utils::write.csv(transcript_summary, spearman_paths$by_transcript, row.names = FALSE)
    utils::write.csv(gene_summary, spearman_paths$by_gene, row.names = FALSE)
    result$paths$spearman_annotated <- spearman_paths$annotated
    result$paths$spearman_by_transcript <- spearman_paths$by_transcript
    result$paths$spearman_by_gene <- spearman_paths$by_gene
    result$tables$transcript_spearman_preview <- utils::head(transcript_summary, 100)

    threshold <- suppressWarnings(as.numeric(config$transcript_absrho_threshold %||% 0.8))
    if (!is.finite(threshold)) {
      threshold <- 0.8
    }
    min_transcript_samples <- suppressWarnings(as.numeric(config$transcript_min_samples %||% 80))
    if (!is.finite(min_transcript_samples)) {
      min_transcript_samples <- 80
    }
    result$settings <- c(result$settings %||% list(), list(
      transcript_absrho_threshold = threshold,
      transcript_min_samples = min_transcript_samples
    ))
    publish(0.86, paste0("Building transcript ML datasets for |rho| >= ", threshold), force = TRUE)
    candidates <- ugplot_geo_transcript_candidates(spearman_results, annotation_map, threshold)
    candidates_path <- file.path(
      ugplot_geo_analysis_dir(cache_dir, source),
      paste0("ugplot_geo_transcript_candidates_", ugplot_geo_safe_token(target_column), "_absrho_", ugplot_geo_safe_token(threshold), ".csv")
    )
    result$tables$transcript_candidates_preview <- utils::head(candidates, 100)
    if (is.data.frame(candidates) && nrow(candidates) > 0) {
      utils::write.csv(candidates, candidates_path, row.names = FALSE)
      result$paths$transcript_candidates <- candidates_path
      group_result <- ugplot_geo_build_transcript_groups_remote(
        candidates = candidates,
        matrix_files = matrix_files,
        metadata = metadata,
        cache_dir = cache_dir,
        target_column = target_column,
        threshold = threshold,
        min_samples_pct = min_transcript_samples,
        source = source,
        progress_callback = function(value, message) publish(0.86 + 0.06 * value, message)
      )
      result$paths$transcript_group_summary <- group_result$paths$summary
      result$paths$transcript_group_details <- group_result$paths$details
      result$tables$transcript_groups <- group_result$summary
      result$tables$transcript_group_details <- group_result$details

      if (is.data.frame(group_result$summary) && nrow(group_result$summary) > 0) {
        publish(0.93, "Running remote transcript ML screening", force = TRUE)
        screen_summary <- ugplot_geo_run_transcript_ml_remote(
          groups = group_result$summary,
          cache_dir = cache_dir,
          source = source,
          config = config,
          progress_callback = function(value, message) publish(0.93 + 0.04 * value, message)
        )
        result$paths$transcript_ml_screening_summary <- file.path(ugplot_geo_transcript_ml_dir(cache_dir, source), "screening_summary.csv")
        result$tables$transcript_ml_screening <- screen_summary
        result$tables$transcript_ml_importance <- ugplot_geo_collect_ml_importance_remote(screen_summary)

        if (is.data.frame(screen_summary) && nrow(screen_summary) > 0) {
          publish(0.97, "Running remote transcript ML stability", force = TRUE)
          stability_summary <- ugplot_geo_run_transcript_stability_remote(
            screen_summary = screen_summary,
            cache_dir = cache_dir,
            source = source,
            config = config,
            progress_callback = function(value, message) publish(0.97 + 0.02 * value, message)
          )
          result$paths$transcript_ml_summary <- file.path(ugplot_geo_transcript_ml_dir(cache_dir, source), "summary.csv")
          result$tables$transcript_ml_summary <- stability_summary
          stability_importance <- ugplot_geo_collect_ml_importance_remote(stability_summary)
          if (is.data.frame(stability_importance) && nrow(stability_importance) > 0) {
            result$tables$transcript_ml_importance <- stability_importance
          }
        }
      }
    } else {
      result$tables$transcript_groups <- data.frame()
      result$tables$transcript_group_details <- data.frame()
      publish(0.92, paste0("No transcript candidates found for |rho| >= ", threshold), force = TRUE)
    }
  }

  result$stage <- "finished"
  result$updated_at <- as.character(Sys.time())
  publish(1, "Remote GEO pipeline finished", force = TRUE)
  result
}
