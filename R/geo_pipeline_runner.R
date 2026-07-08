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
  "reader_v4"
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

ugplot_geo_transcript_ml_run_key <- function(target_column, threshold, min_samples_pct) {
  safe_target <- ugplot_geo_safe_token(target_column)
  safe_threshold <- ugplot_geo_safe_token(format(threshold, trim = TRUE, scientific = FALSE))
  safe_min_samples <- ugplot_geo_safe_token(format(min_samples_pct, trim = TRUE, scientific = FALSE))
  safe_missing <- ugplot_geo_safe_token(paste(ugplot_geo_transcript_missing_definition(), collapse = "_"))
  paste0(
    "target_", safe_target,
    "_", ugplot_geo_transcript_cache_version(),
    "_absrho_", safe_threshold,
    "_minsamples_", safe_min_samples,
    "_missing_", safe_missing
  )
}

ugplot_geo_transcript_ml_dir <- function(cache_dir, source = "processed", run_key = NULL) {
  path <- file.path(ugplot_geo_analysis_dir(cache_dir, source), "transcript_ml_pipeline")
  run_key <- as.character(run_key %||% "")
  if (nzchar(run_key)) {
    path <- file.path(path, ugplot_geo_safe_token(run_key))
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

ugplot_geo_transcript_ml_group_dir <- function(cache_dir, source, group_id, run_key = NULL) {
  path <- file.path(ugplot_geo_transcript_ml_dir(cache_dir, source, run_key), ugplot_geo_safe_token(group_id))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

ugplot_geo_filter_spearman_min_samples_remote <- function(results, min_samples_pct = 80) {
  if (!is.data.frame(results) || nrow(results) == 0 || !"N" %in% names(results)) {
    return(results)
  }
  min_samples_pct <- suppressWarnings(as.numeric(min_samples_pct %||% 80))
  if (!is.finite(min_samples_pct)) {
    min_samples_pct <- 80
  }
  min_samples_pct <- max(0, min(100, min_samples_pct))
  n_values <- suppressWarnings(as.numeric(results$N))
  max_n <- suppressWarnings(max(n_values, na.rm = TRUE))
  if (!is.finite(max_n) || max_n <= 0) {
    return(results)
  }
  min_samples <- max(3L, ceiling(max_n * min_samples_pct / 100))
  filtered <- results[n_values >= min_samples, , drop = FALSE]
  rownames(filtered) <- NULL
  filtered
}

ugplot_geo_cpg_summary_for_job <- function(job_id, jobs_dir, threshold,
                                           spearman_min_samples_pct = 80,
                                           bin_width = 0.05) {
  if (!exists("ugplot_job_dir", mode = "function", inherits = TRUE)) {
    stop("Job store helpers are not available.", call. = FALSE)
  }
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  config_path <- file.path(job_dir, "config.rds")
  if (!file.exists(config_path)) {
    stop("Job config is not available for job: ", job_id, call. = FALSE)
  }
  config <- readRDS(config_path)
  status <- tryCatch(ugplot_read_job_status(job_id, jobs_dir), error = function(e) list())
  result_path <- status$result_path %||% status$partial_result_path %||% ""
  result <- if (nzchar(result_path) && file.exists(result_path)) {
    tryCatch(readRDS(result_path), error = function(e) list())
  } else {
    list()
  }

  accession <- trimws(as.character(config$accession %||% result$accession %||% ""))
  source <- as.character(config$matrix_source %||% result$matrix_source %||% "processed")
  source <- if (identical(source, "raw_sesame")) "raw_sesame" else "processed"
  target_column <- trimws(as.character(config$target_column %||% result$target_column %||% ""))
  if (!nzchar(accession) || !nzchar(target_column)) {
    stop("The selected GEO job does not include accession/target metadata.", call. = FALSE)
  }
  threshold <- suppressWarnings(as.numeric(threshold %||% config$transcript_absrho_threshold %||% result$settings$transcript_absrho_threshold %||% 0.8))
  if (!is.finite(threshold)) {
    threshold <- 0.8
  }
  loaded_threshold <- suppressWarnings(as.numeric(config$transcript_absrho_threshold %||% result$settings$transcript_absrho_threshold %||% threshold))
  if (!is.finite(loaded_threshold)) {
    loaded_threshold <- threshold
  }
  spearman_min_samples_pct <- suppressWarnings(as.numeric(spearman_min_samples_pct %||% config$spearman_min_samples_pct %||% result$settings$spearman_min_samples_pct %||% 80))
  if (!is.finite(spearman_min_samples_pct)) {
    spearman_min_samples_pct <- 80
  }
  bin_width <- suppressWarnings(as.numeric(bin_width %||% 0.05))
  if (!is.finite(bin_width) || bin_width <= 0 || bin_width > 1) {
    bin_width <- 0.05
  }

  cache_dir <- as.character(result$cache_dir %||% ugplot_geo_cache_dir(accession))
  spearman_paths <- ugplot_geo_spearman_paths(cache_dir, target_column, source = source, create = FALSE)
  if (!file.exists(spearman_paths$raw)) {
    stop("Full cached Spearman file is not available on the server for this GEO job.", call. = FALSE)
  }
  spearman_results <- utils::read.csv(spearman_paths$raw, stringsAsFactors = FALSE, check.names = FALSE)
  filtered <- ugplot_geo_filter_spearman_min_samples_remote(spearman_results, spearman_min_samples_pct)
  absrho <- suppressWarnings(as.numeric(filtered$AbsRho))
  rho <- suppressWarnings(as.numeric(filtered$SpearmanRho))
  valid_absrho <- absrho[is.finite(absrho)]
  valid_rho <- rho[is.finite(rho)]
  breaks <- seq(0, 1, by = bin_width)
  if (utils::tail(breaks, 1) < 1) {
    breaks <- c(breaks, 1)
  }
  bins <- cut(pmax(0, pmin(1, valid_absrho)), breaks = breaks, include.lowest = TRUE, right = TRUE)
  histogram <- data.frame(
    BinMin = utils::head(breaks, -1),
    BinMax = utils::tail(breaks, -1),
    Count = as.integer(table(factor(bins, levels = levels(bins)))),
    stringsAsFactors = FALSE
  )
  histogram$Active <- histogram$BinMax >= threshold
  current_threshold_cpgs <- sum(is.finite(absrho) & absrho >= threshold, na.rm = TRUE)
  loaded_threshold_cpgs <- sum(is.finite(absrho) & absrho >= loaded_threshold, na.rm = TRUE)
  newly_included_cpgs <- if (threshold < loaded_threshold) {
    sum(is.finite(absrho) & absrho >= threshold & absrho < loaded_threshold, na.rm = TRUE)
  } else {
    0L
  }
  excluded_loaded_cpgs <- if (threshold > loaded_threshold) {
    sum(is.finite(absrho) & absrho >= loaded_threshold & absrho < threshold, na.rm = TRUE)
  } else {
    0L
  }
  list(
    kind = "geo_cpg_summary",
    job_id = job_id,
    accession = accession,
    source = source,
    target_column = target_column,
    threshold = threshold,
    loaded_threshold = loaded_threshold,
    spearman_min_samples_pct = spearman_min_samples_pct,
    bin_width = bin_width,
    spearman_total_cpgs = nrow(spearman_results),
    spearman_pass_filter_cpgs = nrow(filtered),
    threshold_cpgs = current_threshold_cpgs,
    loaded_threshold_cpgs = loaded_threshold_cpgs,
    newly_included_cpgs = newly_included_cpgs,
    excluded_loaded_cpgs = excluded_loaded_cpgs,
    threshold_delta_cpgs = current_threshold_cpgs - loaded_threshold_cpgs,
    positive_cpgs = sum(is.finite(rho) & rho >= threshold, na.rm = TRUE),
    negative_cpgs = sum(is.finite(rho) & rho <= -threshold, na.rm = TRUE),
    max_absrho = if (length(valid_absrho) > 0) max(valid_absrho) else NA_real_,
    max_rho = if (length(valid_rho) > 0) max(valid_rho) else NA_real_,
    min_rho = if (length(valid_rho) > 0) min(valid_rho) else NA_real_,
    histogram = histogram,
    spearman_path = spearman_paths$raw,
    cache_dir = cache_dir
  )
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
      c("GroupID", "Transcript", "EnsemblTranscript", "CpG", "Gene", "GeneRegion", "Chr", "Position", "SpearmanRho", "AbsRho", "PValue", "N", "CpGKeptForML", "DatasetPath", "KeptCpGs"),
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

ugplot_geo_paper_summary_remote <- function(summary, details = data.frame()) {
  if (!is.data.frame(summary) || nrow(summary) == 0 || !"GroupID" %in% names(summary)) {
    return(data.frame())
  }
  flatten_values <- function(values) {
    values <- unlist(values, recursive = TRUE, use.names = FALSE)
    if (is.null(values)) character(0) else values
  }
  trim_nonempty <- function(values) {
    values <- unique(trimws(as.character(stats::na.omit(flatten_values(values)))))
    values[nzchar(values)]
  }
  first_text <- function(row, columns, default = "") {
    hit <- intersect(columns, names(row))
    if (length(hit) == 0) return(default)
    values <- trim_nonempty(row[[hit[[1]]]][[1]])
    if (length(values) > 0) values[[1]] else default
  }
  first_number <- function(row, columns) {
    hit <- intersect(columns, names(row))
    if (length(hit) == 0) return(NA_real_)
    values <- suppressWarnings(as.numeric(flatten_values(row[[hit[[1]]]][[1]])))
    values <- values[is.finite(values)]
    if (length(values) > 0) values[[1]] else NA_real_
  }
  metric_vector <- function(df, columns) {
    hit <- intersect(columns, names(df))
    if (length(hit) == 0) return(rep(NA_real_, nrow(df)))
    vapply(seq_len(nrow(df)), function(i) {
      values <- suppressWarnings(as.numeric(flatten_values(df[[hit[[1]]]][[i]])))
      values <- values[is.finite(values)]
      if (length(values) > 0) values[[1]] else NA_real_
    }, numeric(1))
  }
  result_object_for_row <- function(row) {
    path <- first_text(row, c("StabilityResultPath", "ScreenResultPath", "ResultPath"))
    if (!nzchar(path) || !file.exists(path)) return(NULL)
    tryCatch(readRDS(path), error = function(e) NULL)
  }
  summary_number <- function(row, result, columns, summary_fields = character(0)) {
    value <- first_number(row, columns)
    if (is.finite(value)) return(value)
    if (is.list(result) && is.list(result$final_summary)) {
      for (field in summary_fields) {
        values <- suppressWarnings(as.numeric(flatten_values(result$final_summary[[field]] %||% NA_real_)))
        values <- values[is.finite(values)]
        if (length(values) > 0) return(values[[1]])
      }
    }
    NA_real_
  }
  cpg_label <- function(cpg, method, value) {
    if (!is.finite(value)) return(cpg)
    method_label <- tolower(as.character(method %||% ""))
    method_label <- if (identical(method_label, "pearson")) "P" else if (identical(method_label, "spearman")) "S" else toupper(substr(method_label, 1, 1))
    if (!nzchar(method_label)) method_label <- "R"
    paste0(cpg, "(", method_label, "=", sprintf("%.2f", value), ")")
  }
  best_cpgs_for_group <- function(group_id, row, limit = 5L) {
    group_details <- if (is.data.frame(details) && nrow(details) > 0 && "GroupID" %in% names(details)) {
      details[as.character(details$GroupID) == group_id, , drop = FALSE]
    } else {
      data.frame()
    }
    if (!is.data.frame(group_details) || nrow(group_details) == 0 || !"CpG" %in% names(group_details)) {
      cpg <- first_text(row, c("TriggerBestCpG", "BestCpG"))
      rho <- first_number(row, c("TriggerBestRho", "BestRho"))
      return(list(
        labels = if (nzchar(cpg)) cpg_label(cpg, "spearman", rho) else "",
        best_method = if (is.finite(rho)) "spearman" else "",
        best_value = abs(rho)
      ))
    }
    spearman <- metric_vector(group_details, c("SpearmanRho", "TriggerBestRho", "BestRho"))
    spearman_abs <- metric_vector(group_details, c("AbsRho", "TriggerMaxAbsRho", "MaxAbsRho"))
    spearman_abs[!is.finite(spearman_abs)] <- abs(spearman[!is.finite(spearman_abs)])
    pearson <- metric_vector(group_details, c("PearsonR", "PearsonRho", "PearsonCorrelation", "PearsonCorr", "Pearson"))
    pearson_abs <- metric_vector(group_details, c("AbsPearsonR", "PearsonAbsR", "AbsPearsonRho", "PearsonAbsRho"))
    pearson_abs[!is.finite(pearson_abs)] <- abs(pearson[!is.finite(pearson_abs)])
    use_pearson <- is.finite(pearson_abs) & (!is.finite(spearman_abs) | pearson_abs > spearman_abs)
    chosen_abs <- ifelse(use_pearson, pearson_abs, spearman_abs)
    chosen_value <- ifelse(use_pearson, pearson, spearman)
    chosen_method <- ifelse(use_pearson, "pearson", "spearman")
    valid <- is.finite(chosen_abs) & nzchar(as.character(group_details$CpG))
    if (!any(valid)) {
      cpgs <- trim_nonempty(group_details$CpG)
      return(list(labels = paste(utils::head(cpgs, limit), collapse = "\n"), best_method = "", best_value = NA_real_))
    }
    cpg_rows <- data.frame(
      CpG = as.character(group_details$CpG[valid]),
      Method = chosen_method[valid],
      Value = chosen_value[valid],
      AbsValue = chosen_abs[valid],
      stringsAsFactors = FALSE
    )
    cpg_rows <- cpg_rows[order(-cpg_rows$AbsValue, cpg_rows$CpG), , drop = FALSE]
    cpg_rows <- cpg_rows[!duplicated(cpg_rows$CpG), , drop = FALSE]
    top_rows <- utils::head(cpg_rows, limit)
    list(
      labels = paste(vapply(seq_len(nrow(top_rows)), function(i) {
        cpg_label(top_rows$CpG[[i]], top_rows$Method[[i]], top_rows$Value[[i]])
      }, character(1)), collapse = "\n"),
      best_method = top_rows$Method[[1]],
      best_value = top_rows$AbsValue[[1]]
    )
  }
  transcripts_for_group <- function(group_id, row) {
    group_details <- if (is.data.frame(details) && nrow(details) > 0 && "GroupID" %in% names(details)) {
      details[as.character(details$GroupID) == group_id, , drop = FALSE]
    } else {
      data.frame()
    }
    enst_columns <- intersect(c("EnsemblTranscript", "EnsemblTranscriptID", "TranscriptENST", "ENST", "ensembl_transcript_id", "Ensembl_Transcript_ID"), names(group_details))
    transcripts <- trim_nonempty(if (length(enst_columns) > 0) group_details[[enst_columns[[1]]]] else character(0))
    if (length(transcripts) == 0 && is.data.frame(group_details) && nrow(group_details) > 0 && "Transcript" %in% names(group_details)) {
      all_transcripts <- trim_nonempty(group_details$Transcript)
      enst_transcripts <- grep("^ENST", all_transcripts, value = TRUE)
      transcripts <- if (length(enst_transcripts) > 0) enst_transcripts else all_transcripts
    }
    if (length(transcripts) == 0) {
      all_transcripts <- trim_nonempty(c(
        first_text(row, c("PrincipalTranscript", "Transcript")),
        unlist(strsplit(first_text(row, c("ExtraTranscripts")), ";", fixed = TRUE), use.names = FALSE)
      ))
      enst_transcripts <- grep("^ENST", all_transcripts, value = TRUE)
      transcripts <- if (length(enst_transcripts) > 0) enst_transcripts else all_transcripts
    }
    paste(transcripts, collapse = "; ")
  }
  result_label_for_row <- function(row, cpg_info) {
    metric_name <- first_text(row, c("MetricName"), "model")
    model_metric <- first_number(row, c("MedianMetric", "MeanMetric", "BestMetric"))
    lower_is_better <- grepl("rmse|mae|mse|error|loss|deviance", metric_name, ignore.case = TRUE)
    candidates <- numeric(0)
    if (is.finite(model_metric) && !isTRUE(lower_is_better)) candidates[["ML"]] <- model_metric
    if (is.finite(cpg_info$best_value)) candidates[[paste0("CpG-", cpg_info$best_method %||% "spearman")]] <- cpg_info$best_value
    if (length(candidates) == 0) return("")
    best <- max(candidates, na.rm = TRUE)
    winners <- names(candidates)[abs(candidates - best) < 1e-12]
    if (all(grepl("^CpG-", winners))) "CpG" else if (all(winners == "ML")) "ML" else paste(winners, collapse = " + ")
  }
  rows <- lapply(seq_len(nrow(summary)), function(i) {
    row <- summary[i, , drop = FALSE]
    group_id <- first_text(row, c("GroupID"))
    cpg_info <- best_cpgs_for_group(group_id, row)
    result <- result_object_for_row(row)
    label <- result_label_for_row(row, cpg_info)
    data.frame(
      Result = label,
      Gene = first_text(row, c("Gene")),
      GroupID = group_id,
      Transcripts = transcripts_for_group(group_id, row),
      Correlation = cpg_info$labels,
      CpGs = first_number(row, c("Columns", "CpGCount", "CpGs", "NCpGs")),
      Samples = first_number(row, c("StratumSamples", "Samples", "N")),
      Model = first_text(row, c("BestModel")),
      MedianR2 = summary_number(row, result, c("MedianR2", "MedianMetric"), c("best_model_median")),
      MinR2 = summary_number(row, result, c("MinR2", "MinMetric"), c("best_model_min")),
      MaxR2 = summary_number(row, result, c("MaxR2", "MaxMetric", "BestMetric"), c("best_model_max")),
      MedianMAE = summary_number(row, result, c("MedianMAE", "MAEMedian"), c("best_model_mae_median")),
      WBR2 = summary_number(row, result, c("WBR2", "BloodAdjustedR2", "WithBloodR2", "R2WithBlood"), c("wb_r2", "blood_adjusted_r2")),
      ShuffleMaxR2 = summary_number(row, result, c("ShuffleMaxR2", "ScrambleMaxR2", "AgeShuffleMaxR2"), c("shuffle_max_r2", "scramble_max_r2")),
      BestSource = if (identical(label, "ML")) "model" else cpg_info$best_method,
      Source = first_text(row, c("Source")),
      Phase = first_text(row, c("Phase")),
      StratumColumn = first_text(row, c("StratumColumn")),
      StratumValue = first_text(row, c("StratumValue")),
      stringsAsFactors = FALSE
    )
  })
  final <- ugplot_geo_bind_rows(rows)
  if (!is.data.frame(final) || nrow(final) == 0) return(data.frame())
  key <- paste(final$Source, final$GroupID, final$StratumColumn, final$StratumValue, sep = "\r")
  stability_keys <- unique(key[as.character(final$Phase) == "stability"])
  final <- final[!(key %in% stability_keys & as.character(final$Phase) != "stability"), , drop = FALSE]
  phase_order <- ifelse(as.character(final$Phase) == "stability", 0L, 1L)
  final <- final[order(
    phase_order,
    final$Source,
    final$StratumColumn,
    final$StratumValue,
    -suppressWarnings(as.numeric(final$MedianR2)),
    final$GroupID
  ), , drop = FALSE]
  rownames(final) <- NULL
  final
}

ugplot_geo_collect_group_datasets_remote <- function(groups) {
  if (!is.data.frame(groups) || nrow(groups) == 0 || !"DatasetPath" %in% names(groups) || !"GroupID" %in% names(groups)) {
    return(list())
  }
  datasets <- list()
  for (row_i in seq_len(nrow(groups))) {
    group_id <- as.character(groups$GroupID[[row_i]] %||% "")
    dataset_path <- as.character(groups$DatasetPath[[row_i]] %||% "")
    if (is.na(group_id) || !nzchar(group_id) || is.na(dataset_path) || !nzchar(dataset_path) || !file.exists(dataset_path)) {
      next
    }
    dataset <- tryCatch(utils::read.csv(dataset_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    if (is.data.frame(dataset) && nrow(dataset) > 0) {
      datasets[[group_id]] <- dataset
    }
  }
  datasets
}

ugplot_geo_ml_rank_summary <- function(summary) {
  if (!is.data.frame(summary) || nrow(summary) == 0) {
    return(summary)
  }
  rank_one <- function(df) {
    metric <- suppressWarnings(as.numeric(df$MedianMetric %||% df$BestMetric))
    rho <- suppressWarnings(as.numeric(df$TriggerMaxAbsRho))
    metric_rank <- rank(-metric, ties.method = "min", na.last = "keep")
    rho_rank <- rank(-rho, ties.method = "min", na.last = "keep")
    combined <- metric_rank + rho_rank
    df$ModelRank <- metric_rank
    df$RhoRank <- rho_rank
    df$CombinedRank <- rank(combined, ties.method = "min", na.last = "keep")
    df[order(df$CombinedRank, df$ModelRank, df$RhoRank, df$PrincipalTranscript), , drop = FALSE]
  }
  has_strata <- all(c("StratumColumn", "StratumValue") %in% names(summary)) &&
    any(nzchar(as.character(summary$StratumColumn %||% "")) | nzchar(as.character(summary$StratumValue %||% "")))
  if (isTRUE(has_strata)) {
    summary$StratumColumn[is.na(summary$StratumColumn)] <- ""
    summary$StratumValue[is.na(summary$StratumValue)] <- ""
    ranked <- lapply(split(summary, paste(summary$StratumColumn, summary$StratumValue, sep = "\f"), drop = TRUE), rank_one)
    summary <- ugplot_geo_bind_rows(ranked)
    summary <- summary[order(summary$StratumColumn, summary$StratumValue, summary$CombinedRank, summary$PrincipalTranscript), , drop = FALSE]
  } else {
    summary <- rank_one(summary)
  }
  rownames(summary) <- NULL
  summary
}

ugplot_geo_ml_class_value <- function(values) {
  values <- as.character(values)
  values <- trimws(values)
  missing <- is.na(values) | !nzchar(values) | tolower(values) %in% c("na", "n/a", "nan", "null")
  values[missing] <- NA_character_
  values
}

ugplot_geo_ml_stability_strata <- function(metadata, column) {
  column <- as.character(column %||% "")
  if (!is.data.frame(metadata) || nrow(metadata) == 0 || !nzchar(column) || !column %in% names(metadata) || !"sample_id" %in% names(metadata)) {
    return(data.frame())
  }
  values <- ugplot_geo_ml_class_value(metadata[[column]])
  sample_ids <- as.character(metadata$sample_id)
  keep <- !is.na(values) & nzchar(values) & !is.na(sample_ids) & nzchar(sample_ids)
  values <- values[keep]
  sample_ids <- sample_ids[keep]
  if (length(values) == 0) {
    return(data.frame())
  }
  value_levels <- names(sort(table(values), decreasing = TRUE))
  rows <- lapply(value_levels, function(value) {
    ids <- unique(sample_ids[values == value])
    data.frame(
      StratumColumn = column,
      StratumValue = value,
      StratumSamples = length(ids),
      SampleIDs = paste(ids, collapse = "\r"),
      stringsAsFactors = FALSE
    )
  })
  strata <- do.call(rbind, rows)
  strata[order(-strata$StratumSamples, strata$StratumValue), , drop = FALSE]
}

ugplot_geo_ml_stability_task_key <- function(group_id, stratum_column = "", stratum_value = "") {
  paste(as.character(group_id), as.character(stratum_column %||% ""), as.character(stratum_value %||% ""), sep = "\f")
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

ugplot_geo_ml_group_dataset <- function(group, sample_ids = NULL, keep_sample_id = FALSE) {
  dataset_path_value <- group$DatasetPath %||% ""
  dataset_path <- as.character(dataset_path_value[[1]] %||% "")
  dataset <- data.frame()
  if (is.data.frame(group$dataset)) {
    dataset <- group$dataset
  } else if (is.data.frame(group$matrix)) {
    dataset <- group$matrix
  } else if (!is.na(dataset_path) && nzchar(dataset_path) && file.exists(dataset_path)) {
    dataset <- utils::read.csv(dataset_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Transcript group dataset file is missing: ", dataset_path, call. = FALSE)
  }
  if (!is.null(sample_ids)) {
    sample_ids <- unique(as.character(sample_ids))
    sample_ids <- sample_ids[nzchar(sample_ids) & !is.na(sample_ids)]
    if (!"sample_id" %in% names(dataset)) {
      stop("Transcript dataset has no sample_id column for class/group filtering.", call. = FALSE)
    }
    dataset <- dataset[as.character(dataset$sample_id) %in% sample_ids, , drop = FALSE]
    if (nrow(dataset) == 0) {
      stop("No transcript dataset samples matched the selected class/group.", call. = FALSE)
    }
  }
  if (!"target" %in% names(dataset)) {
    target_candidates <- setdiff(names(dataset), c("sample_id", grep("^cg", names(dataset), value = TRUE)))
    target_name <- target_candidates[[1]] %||% ""
    if (!nzchar(target_name)) {
      stop("Could not identify target column in transcript dataset.", call. = FALSE)
    }
    names(dataset)[names(dataset) == target_name] <- "target"
  }
  sample_count <- nrow(dataset)
  if (!isTRUE(keep_sample_id)) {
    dataset <- dataset[, setdiff(names(dataset), "sample_id"), drop = FALSE]
  }
  list(dataset = dataset, dataset_path = dataset_path, sample_count = sample_count)
}

ugplot_geo_ml_quick_models <- function(available) {
  available <- unique(as.character(available))
  available <- available[nzchar(available)]
  families <- list(
    linear = c("glmnet", "lm", "ridge", "lasso", "bayesglm", "leapSeq"),
    tree = c("rpart", "rf", "ranger", "treebag", "ctree"),
    boosting = c("xgbTree", "gbm", "blackboost", "ada", "bstTree"),
    neural = c("nnet", "avNNet", "mlp", "brnn")
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
  run_key <- as.character(config$geo_transcript_ml_run_key %||% "")
  pipeline_dir <- ugplot_geo_transcript_ml_dir(cache_dir, source, run_key)
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
    group_dir <- ugplot_geo_transcript_ml_group_dir(cache_dir, source, group_id, run_key)
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
      Columns = group$Columns[[1]],
      Samples = group$Samples[[1]],
      TranscriptCount = group$TranscriptCount[[1]],
      ExtraTranscripts = group$ExtraTranscripts[[1]] %||% "",
      CpGs = group$CpGs[[1]] %||% "",
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
  if (n < min_seeds) {
    return(list(stable = FALSE, reason = paste0("collecting minimum seeds: ", n, "/", min_seeds)))
  }
  if (n < (2 * window)) {
    return(list(stable = FALSE, reason = paste0("collecting two stability windows: ", n, "/", 2 * window)))
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
                                                       metadata = NULL,
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
  run_key <- as.character(config$geo_transcript_ml_run_key %||% "")
  pipeline_dir <- ugplot_geo_transcript_ml_dir(cache_dir, source, run_key)
  stratum_column <- as.character(config$geo_ml_stability_group_column %||% "")
  strata <- if (nzchar(stratum_column)) {
    ugplot_geo_ml_stability_strata(metadata, stratum_column)
  } else {
    data.frame(StratumColumn = "", StratumValue = "", StratumSamples = NA_integer_, SampleIDs = "", stringsAsFactors = FALSE)
  }
  if (!is.data.frame(strata) || nrow(strata) == 0) {
    strata <- data.frame(StratumColumn = "", StratumValue = "", StratumSamples = NA_integer_, SampleIDs = "", stringsAsFactors = FALSE)
    stratum_column <- ""
  }
  summary_path <- if (nzchar(stratum_column)) {
    file.path(pipeline_dir, paste0("summary_by_", ugplot_geo_safe_token(stratum_column), ".csv"))
  } else {
    file.path(pipeline_dir, "summary.csv")
  }
  summaries <- if (file.exists(summary_path)) {
    tryCatch(utils::read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  } else {
    data.frame()
  }
  processed <- if (is.data.frame(summaries) && "GroupID" %in% names(summaries)) {
    existing_col <- if ("StratumColumn" %in% names(summaries)) summaries$StratumColumn else rep("", nrow(summaries))
    existing_value <- if ("StratumValue" %in% names(summaries)) summaries$StratumValue else rep("", nrow(summaries))
    unique(ugplot_geo_ml_stability_task_key(summaries$GroupID, existing_col, existing_value))
  } else {
    character(0)
  }
  total_tasks <- nrow(screen_summary) * nrow(strata)
  task_i <- 0L
  for (stratum_i in seq_len(nrow(strata))) {
    stratum <- strata[stratum_i, , drop = FALSE]
    sample_ids <- if (nzchar(stratum$StratumColumn[[1]])) strsplit(as.character(stratum$SampleIDs[[1]] %||% ""), "\r", fixed = TRUE)[[1]] else NULL
    stratum_label <- if (nzchar(stratum$StratumColumn[[1]])) paste0(stratum$StratumColumn[[1]], "=", stratum$StratumValue[[1]]) else "all samples"
    for (row_i in seq_len(nrow(screen_summary))) {
      task_i <- task_i + 1L
      row <- screen_summary[row_i, , drop = FALSE]
      group_id <- as.character(row$GroupID[[1]])
      task_key <- ugplot_geo_ml_stability_task_key(group_id, stratum$StratumColumn[[1]], stratum$StratumValue[[1]])
      if (task_key %in% processed) {
        next
      }
      best_model <- as.character(row$BestModel[[1]] %||% "")
      if (!nzchar(best_model) || identical(best_model, "-")) {
        next
      }
      dataset_info <- ugplot_geo_ml_group_dataset(row, sample_ids = sample_ids)
      dataset <- dataset_info$dataset
      group_dir <- ugplot_geo_transcript_ml_group_dir(cache_dir, source, group_id, run_key)
      if (nzchar(stratum$StratumColumn[[1]])) {
        group_dir <- file.path(group_dir, "stability_by", ugplot_geo_safe_token(stratum$StratumColumn[[1]]), ugplot_geo_safe_token(stratum$StratumValue[[1]]))
        dir.create(group_dir, recursive = TRUE, showWarnings = FALSE)
      }
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
          progress_callback((task_i - 1) / total_tasks, paste0("Stability ", group_id, " / ", stratum_label, " with ", best_model, " to seed ", current_end))
        }
        stability_result <- ugplot_run_ml_job(
          dataset,
          stability_config,
          progress_callback = function(...) {
            args <- list(...)
            if (!is.null(progress_callback)) {
              progress_callback((task_i - 1) / total_tasks, paste0("Stability ", group_id, " / ", stratum_label, ": ", args$message %||% ""))
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
        if (nzchar(stratum$StratumColumn[[1]])) {
          importance$StratumColumn <- stratum$StratumColumn[[1]]
          importance$StratumValue <- stratum$StratumValue[[1]]
          importance$StratumSamples <- dataset_info$sample_count
        }
        utils::write.csv(importance, importance_path, row.names = FALSE)
      }
      summary_row <- row
      summary_row$Phase <- "stability"
      summary_row$StratumColumn <- stratum$StratumColumn[[1]]
      summary_row$StratumValue <- stratum$StratumValue[[1]]
      summary_row$StratumSamples <- if (nzchar(stratum$StratumColumn[[1]])) dataset_info$sample_count else NA_integer_
      summary_row$BestMetric <- suppressWarnings(as.numeric(stability_result$final_summary$metric_value %||% NA_real_))
      summary_row$MedianMetric <- if (length(metric_values) > 0) stats::median(metric_values) else NA_real_
      summary_row$MeanMetric <- if (length(metric_values) > 0) mean(metric_values) else NA_real_
      summary_row$MetricSE <- if (length(metric_values) > 1) stats::sd(metric_values) / sqrt(length(metric_values)) else NA_real_
      summary_row$SeedsRun <- length(metric_values)
      summary_row$Stable <- isTRUE(stable_state$stable)
      summary_row$StabilityDetail <- stable_state$reason
      summary_row$StabilityResultPath <- stability_path
      summary_row$ImportancePath <- if (file.exists(importance_path)) importance_path else ""
      summaries <- if (is.data.frame(summaries) && nrow(summaries) > 0) {
        existing_col <- if ("StratumColumn" %in% names(summaries)) summaries$StratumColumn else rep("", nrow(summaries))
        existing_value <- if ("StratumValue" %in% names(summaries)) summaries$StratumValue else rep("", nrow(summaries))
        keep <- ugplot_geo_ml_stability_task_key(summaries$GroupID, existing_col, existing_value) != task_key
        ugplot_geo_bind_rows(list(summaries[keep, , drop = FALSE], summary_row))
      } else {
        summary_row
      }
      summaries <- ugplot_geo_ml_rank_summary(summaries)
      utils::write.csv(summaries, summary_path, row.names = FALSE)
      processed <- union(processed, task_key)
    }
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
  resume_result_path <- as.character(config$resume_result_path %||% "")
  resume_mode <- nzchar(resume_result_path) ||
    (is.list(config$resume_result) && identical(config$resume_result$kind %||% "", "geo_pipeline")) ||
    isTRUE(config$resume_cached_geo) ||
    isTRUE(config$use_cached_geo)
  read_cached_csv <- function(path) {
    if (nzchar(path %||% "") && file.exists(path)) {
      tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    } else {
      data.frame()
    }
  }

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
      geo_ml_stability_tolerance = config$geo_ml_stability_tolerance %||% 0.01,
      geo_ml_stability_group_column = config$geo_ml_stability_group_column %||% ""
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
    beta_path <- ugplot_geo_sesame_beta_path(cache_dir)
    qc_path <- ugplot_geo_sesame_qc_path(cache_dir)
    if (isTRUE(resume_mode) && file.exists(beta_path) && file.exists(qc_path)) {
      publish(0.42, "Using cached sesame IDAT reprocessing for resume", force = TRUE)
      result$tables$idat_qc <- read_cached_csv(qc_path)
      result$paths$sesame_beta <- beta_path
      result$paths$sesame_qc <- qc_path
    } else {
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
    }
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
    spearman_paths <- ugplot_geo_spearman_paths(cache_dir, target_column, source = source)
    spearman_results <- if (isTRUE(resume_mode) && file.exists(spearman_paths$raw)) {
      publish(0.63, paste0("Using cached CpG Spearman scan for resume: ", target_column), force = TRUE)
      read_cached_csv(spearman_paths$raw)
    } else {
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
      scanned_results <- ugplot_geo_spearman_scan(
        matrix_files,
        metadata,
        target_column,
        max_cpgs = as.integer(config$spearman_max_cpgs %||% 0),
        min_matched_samples = min_matched,
        progress_callback = function(scanned) {
          publish(0.63, paste0("Scanned ", format(scanned, big.mark = ","), " CpGs"))
        }
      )
      utils::write.csv(scanned_results, spearman_paths$raw, row.names = FALSE)
      scanned_results
    }
    result$paths$spearman_raw <- spearman_paths$raw
    result$tables$spearman_preview <- utils::head(spearman_results, 100)

    publish(0.82, "Building/loading CpG annotation cache", force = TRUE)
    annotation_map <- ugplot_geo_build_annotation_cache(ugplot_geo_detect_platform(metadata))
    if (isTRUE(resume_mode) &&
        file.exists(spearman_paths$annotated) &&
        file.exists(spearman_paths$by_transcript) &&
        file.exists(spearman_paths$by_gene)) {
      annotated <- read_cached_csv(spearman_paths$annotated)
      transcript_summary <- read_cached_csv(spearman_paths$by_transcript)
      gene_summary <- read_cached_csv(spearman_paths$by_gene)
    } else {
      annotated <- ugplot_geo_join_spearman_annotation(spearman_results, annotation_map)
      utils::write.csv(annotated, spearman_paths$annotated, row.names = FALSE)
      transcript_summary <- ugplot_geo_group_spearman_annotation(annotated, "Transcript")
      gene_summary <- ugplot_geo_group_spearman_annotation(annotated, "Gene")
      utils::write.csv(transcript_summary, spearman_paths$by_transcript, row.names = FALSE)
      utils::write.csv(gene_summary, spearman_paths$by_gene, row.names = FALSE)
    }
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
    transcript_ml_run_key <- ugplot_geo_transcript_ml_run_key(target_column, threshold, min_transcript_samples)
    config$geo_transcript_ml_run_key <- transcript_ml_run_key
    transcript_ml_dir <- ugplot_geo_transcript_ml_dir(cache_dir, source, transcript_ml_run_key)
    result$settings <- c(result$settings %||% list(), list(
      transcript_ml_run_key = transcript_ml_run_key,
      resume_cached_geo = isTRUE(config$resume_cached_geo) || isTRUE(config$use_cached_geo)
    ))
    result$paths$transcript_ml_dir <- transcript_ml_dir
    publish(0.86, paste0("Building transcript ML datasets for |rho| >= ", threshold), force = TRUE)
    candidates_path <- file.path(
      ugplot_geo_analysis_dir(cache_dir, source),
      paste0("ugplot_geo_transcript_candidates_", ugplot_geo_safe_token(target_column), "_absrho_", ugplot_geo_safe_token(threshold), ".csv")
    )
    candidates <- if (isTRUE(resume_mode) && file.exists(candidates_path)) {
      read_cached_csv(candidates_path)
    } else {
      ugplot_geo_transcript_candidates(spearman_results, annotation_map, threshold)
    }
    result$tables$transcript_candidates_preview <- utils::head(candidates, 100)
    if (is.data.frame(candidates) && nrow(candidates) > 0) {
      utils::write.csv(candidates, candidates_path, row.names = FALSE)
      result$paths$transcript_candidates <- candidates_path
      group_paths <- ugplot_geo_transcript_group_paths(cache_dir, target_column, threshold, min_transcript_samples, source = source)
      group_result <- if (isTRUE(resume_mode) && file.exists(group_paths$summary) && file.exists(group_paths$details)) {
        publish(0.86, "Using cached transcript ML groups for resume", force = TRUE)
        list(
          summary = read_cached_csv(group_paths$summary),
          details = read_cached_csv(group_paths$details),
          paths = group_paths
        )
      } else {
        ugplot_geo_build_transcript_groups_remote(
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
      }
      result$paths$transcript_group_summary <- group_result$paths$summary
      result$paths$transcript_group_details <- group_result$paths$details
      result$tables$transcript_groups <- group_result$summary
      result$tables$transcript_group_details <- group_result$details
      result$tables$transcript_group_datasets <- ugplot_geo_collect_group_datasets_remote(group_result$summary)

      if (is.data.frame(group_result$summary) && nrow(group_result$summary) > 0) {
        publish(0.93, "Running remote transcript ML screening", force = TRUE)
        screen_summary <- ugplot_geo_run_transcript_ml_remote(
          groups = group_result$summary,
          cache_dir = cache_dir,
          source = source,
          config = config,
          progress_callback = function(value, message) publish(0.93 + 0.04 * value, message)
        )
        result$paths$transcript_ml_screening_summary <- file.path(transcript_ml_dir, "screening_summary.csv")
        result$tables$transcript_ml_screening <- screen_summary
        result$tables$transcript_ml_importance <- ugplot_geo_collect_ml_importance_remote(screen_summary)
        result$tables$transcript_ml_final <- ugplot_geo_paper_summary_remote(
          screen_summary,
          details = result$tables$transcript_group_details
        )

        if (is.data.frame(screen_summary) && nrow(screen_summary) > 0) {
          publish(0.97, "Running remote transcript ML stability", force = TRUE)
          stability_summary <- ugplot_geo_run_transcript_stability_remote(
            screen_summary = screen_summary,
            cache_dir = cache_dir,
            source = source,
            config = config,
            metadata = metadata,
            progress_callback = function(value, message) publish(0.97 + 0.02 * value, message)
          )
          stability_group_column <- as.character(config$geo_ml_stability_group_column %||% "")
          result$paths$transcript_ml_summary <- if (nzchar(stability_group_column)) {
            file.path(transcript_ml_dir, paste0("summary_by_", ugplot_geo_safe_token(stability_group_column), ".csv"))
          } else {
            file.path(transcript_ml_dir, "summary.csv")
          }
          result$tables$transcript_ml_summary <- stability_summary
          result$tables$transcript_ml_final <- ugplot_geo_paper_summary_remote(
            stability_summary,
            details = result$tables$transcript_group_details
          )
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
