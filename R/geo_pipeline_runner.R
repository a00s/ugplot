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
    annotated = file.path(analysis_dir, paste0("ugplot_geo_spearman_", safe_target, "_annotated_", ugplot_geo_annotation_cache_version(), ".csv")),
    by_transcript = file.path(analysis_dir, paste0("ugplot_geo_spearman_", safe_target, "_by_transcript_", ugplot_geo_annotation_cache_version(), ".csv")),
    by_gene = file.path(analysis_dir, paste0("ugplot_geo_spearman_", safe_target, "_by_gene_", ugplot_geo_annotation_cache_version(), ".csv"))
  )
}

ugplot_geo_transcript_cache_version <- function() {
  "reader_v5_members"
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
    ugplot_geo_transcript_cache_version(),
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

ugplot_geo_cpg_lookup_for_job <- function(job_id, jobs_dir, cpg,
                                          threshold = NULL,
                                          spearman_min_samples_pct = 80) {
  if (!exists("ugplot_job_dir", mode = "function", inherits = TRUE)) {
    stop("Job store helpers are not available.", call. = FALSE)
  }
  cpg <- trimws(as.character(cpg %||% ""))
  if (!nzchar(cpg)) {
    stop("CpG id is required.", call. = FALSE)
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
  spearman_min_samples_pct <- max(0, min(100, spearman_min_samples_pct))

  cache_dir <- as.character(result$cache_dir %||% ugplot_geo_cache_dir(accession))
  spearman_paths <- ugplot_geo_spearman_paths(cache_dir, target_column, source = source, create = FALSE)
  if (!file.exists(spearman_paths$raw)) {
    stop("Full cached Spearman file is not available on the server for this GEO job.", call. = FALSE)
  }

  match_cpg_rows <- function(path) {
    if (!nzchar(path %||% "") || !file.exists(path)) {
      return(data.frame())
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    if (!is.data.frame(df) || nrow(df) == 0 || !"CpG" %in% names(df)) {
      return(data.frame())
    }
    out <- df[tolower(as.character(df$CpG)) == tolower(cpg), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  spearman_results <- utils::read.csv(spearman_paths$raw, stringsAsFactors = FALSE, check.names = FALSE)
  raw <- if ("CpG" %in% names(spearman_results)) {
    spearman_results[tolower(as.character(spearman_results$CpG)) == tolower(cpg), , drop = FALSE]
  } else {
    data.frame()
  }
  rownames(raw) <- NULL
  min_samples <- NA_integer_
  if (nrow(spearman_results) > 0 && "N" %in% names(spearman_results)) {
    n_values <- suppressWarnings(as.numeric(spearman_results$N))
    max_n <- suppressWarnings(max(n_values, na.rm = TRUE))
    if (is.finite(max_n) && max_n > 0) {
      min_samples <- max(3L, ceiling(max_n * spearman_min_samples_pct / 100))
    }
  }
  if (nrow(raw) > 0) {
    raw_n <- suppressWarnings(as.numeric(raw$N %||% NA_real_))
    raw_absrho <- suppressWarnings(as.numeric(raw$AbsRho %||% NA_real_))
    raw$PassesSampleFilter <- is.finite(raw_n) & is.finite(min_samples) & raw_n >= min_samples
    raw$PassesCurrentThreshold <- is.finite(raw_absrho) & raw_absrho >= threshold
    raw$PassesLoadedThreshold <- is.finite(raw_absrho) & raw_absrho >= loaded_threshold
  }

  annotated <- match_cpg_rows(spearman_paths$annotated)
  group_details_path <- as.character(result$paths$transcript_group_details %||% "")
  group_details <- match_cpg_rows(group_details_path)

  genes <- character(0)
  transcripts <- character(0)
  if (is.data.frame(annotated) && nrow(annotated) > 0) {
    if ("Gene" %in% names(annotated)) {
      genes <- sort(unique(trimws(as.character(stats::na.omit(annotated$Gene)))))
      genes <- genes[nzchar(genes)]
    }
    if ("Transcript" %in% names(annotated)) {
      transcripts <- sort(unique(trimws(as.character(stats::na.omit(annotated$Transcript)))))
      transcripts <- transcripts[nzchar(transcripts)]
    }
  }

  gene_summary <- data.frame()
  if (file.exists(spearman_paths$by_gene) && length(genes) > 0) {
    by_gene <- utils::read.csv(spearman_paths$by_gene, stringsAsFactors = FALSE, check.names = FALSE)
    group_col <- if ("Group" %in% names(by_gene)) "Group" else if ("Gene" %in% names(by_gene)) "Gene" else ""
    if (nzchar(group_col)) {
      gene_summary <- by_gene[as.character(by_gene[[group_col]]) %in% genes, , drop = FALSE]
      rownames(gene_summary) <- NULL
    }
  }

  transcript_summary <- data.frame()
  if (file.exists(spearman_paths$by_transcript) && length(transcripts) > 0) {
    by_transcript <- utils::read.csv(spearman_paths$by_transcript, stringsAsFactors = FALSE, check.names = FALSE)
    group_col <- if ("Group" %in% names(by_transcript)) "Group" else if ("Transcript" %in% names(by_transcript)) "Transcript" else ""
    if (nzchar(group_col)) {
      transcript_summary <- by_transcript[as.character(by_transcript[[group_col]]) %in% transcripts, , drop = FALSE]
      rownames(transcript_summary) <- NULL
    }
  }

  transcript_min_samples <- suppressWarnings(as.numeric(
    config$transcript_min_samples %||%
      result$settings$transcript_min_samples %||%
      80
  ))
  if (!is.finite(transcript_min_samples)) {
    transcript_min_samples <- 80
  }
  candidate_rows <- data.frame()
  candidates_path <- as.character(result$paths$transcript_candidates %||% "")
  if (file.exists(candidates_path)) {
    candidates <- utils::read.csv(candidates_path, stringsAsFactors = FALSE, check.names = FALSE)
    if (is.data.frame(candidates) && nrow(candidates) > 0) {
      candidate_match <- rep(FALSE, nrow(candidates))
      if ("CpG" %in% names(candidates)) {
        candidate_match <- candidate_match |
          tolower(as.character(candidates$CpG)) == tolower(cpg)
      }
      if ("Transcript" %in% names(candidates) && length(transcripts) > 0) {
        candidate_match <- candidate_match |
          as.character(candidates$Transcript) %in% transcripts
      }
      candidate_rows <- candidates[candidate_match, , drop = FALSE]
      rownames(candidate_rows) <- NULL
    }
  }
  group_paths <- ugplot_geo_transcript_group_paths(
    cache_dir = cache_dir,
    target_column = target_column,
    threshold = loaded_threshold,
    min_samples_pct = transcript_min_samples,
    source = source
  )
  group_progress_path <- if (nzchar(group_details_path) &&
                             grepl("_details\\.csv$", group_details_path)) {
    sub("_details\\.csv$", "_progress.rds", group_details_path)
  } else {
    group_paths$progress
  }
  transcript_progress <- data.frame()
  if (file.exists(group_progress_path) && length(transcripts) > 0) {
    progress <- tryCatch(readRDS(group_progress_path), error = function(e) data.frame())
    if (is.data.frame(progress) && "Transcript" %in% names(progress)) {
      transcript_progress <- progress[
        as.character(progress$Transcript) %in% transcripts,
        ,
        drop = FALSE
      ]
      rownames(transcript_progress) <- NULL
    }
  }
  transcript_diagnostic <- if (nrow(candidate_rows) == 0) {
    "CpG/transcripts are absent from the cached transcript candidate file."
  } else if (nrow(transcript_progress) == 0) {
    "Candidate transcripts were not processed into transcript datasets."
  } else if (any(as.character(transcript_progress$Status) != "compatible")) {
    paste0(
      "Transcript dataset filter: ",
      paste(
        paste0(transcript_progress$Transcript, "=", transcript_progress$Status),
        collapse = "; "
      )
    )
  } else if (nrow(group_details) == 0) {
    "Transcript datasets are compatible, but the cached group details are stale or inconsistent."
  } else {
    "CpG is present in transcript group details."
  }

  list(
    kind = "geo_cpg_lookup",
    job_id = job_id,
    accession = accession,
    source = source,
    target_column = target_column,
    cpg = cpg,
    threshold = threshold,
    loaded_threshold = loaded_threshold,
    spearman_min_samples_pct = spearman_min_samples_pct,
    min_samples = min_samples,
    present_in_spearman = nrow(raw) > 0,
    present_in_annotation = nrow(annotated) > 0,
    present_in_transcript_groups = nrow(group_details) > 0,
    genes = genes,
    transcripts = transcripts,
    raw = raw,
    annotated = annotated,
    gene_summary = gene_summary,
    transcript_summary = transcript_summary,
    transcript_candidate_rows = candidate_rows,
    transcript_progress = transcript_progress,
    transcript_diagnostic = transcript_diagnostic,
    transcript_group_details = group_details,
    paths = list(
      spearman_raw = spearman_paths$raw,
      spearman_annotated = spearman_paths$annotated,
      spearman_by_gene = spearman_paths$by_gene,
      spearman_by_transcript = spearman_paths$by_transcript,
      transcript_candidates = candidates_path,
      transcript_group_details = group_details_path,
      transcript_group_progress = group_progress_path
    )
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
    transcript_members <- sort(unique(as.character(stats::na.omit(group_df$Transcript))))
    transcript_members <- transcript_members[nzchar(transcript_members)]
    gene_members <- sort(unique(unlist(strsplit(
      paste(as.character(stats::na.omit(group_df$Gene)), collapse = ";"),
      ";", fixed = TRUE
    ), use.names = FALSE)))
    gene_members <- trimws(gene_members)
    gene_members <- gene_members[nzchar(gene_members)]
    data.frame(
      GroupID = paste0("TG", group_index),
      PrincipalTranscript = principal$Transcript[[1]],
      Gene = principal$Gene[[1]],
      Columns = principal$Columns[[1]],
      Samples = principal$Samples[[1]],
      TranscriptCount = length(transcript_members),
      TranscriptMembers = paste(transcript_members, collapse = ";"),
      GeneMembers = paste(gene_members, collapse = ";"),
      ExtraTranscripts = paste(setdiff(transcript_members, principal$Transcript[[1]]), collapse = ";"),
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

ugplot_geo_build_transcript_group_progress_row <- function(transcript_id, candidates, matrix_files, metadata,
                                                           cache_dir, target_column, min_samples_pct,
                                                           source = "processed", candidate_dataset = NULL) {
  transcript_rows <- candidates[as.character(candidates$Transcript) == transcript_id, , drop = FALSE]
  transcript_cpgs <- unique(as.character(stats::na.omit(transcript_rows$CpG)))
  transcript_cpgs <- transcript_cpgs[nzchar(transcript_cpgs)]
  dataset_path <- ugplot_geo_transcript_dataset_path(cache_dir, transcript_id, target_column, source)
  raw_dataset_path <- ugplot_geo_transcript_dataset_path(cache_dir, transcript_id, target_column, source, raw = TRUE)
  transcript_dataset <- if (file.exists(raw_dataset_path)) {
    tryCatch(utils::read.csv(raw_dataset_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  } else if (is.data.frame(candidate_dataset) && nrow(candidate_dataset) > 0L) {
    available_cpgs <- intersect(transcript_cpgs, names(candidate_dataset))
    required_columns <- intersect(c("sample_id", target_column), names(candidate_dataset))
    if (length(available_cpgs) > 0L && length(required_columns) == 2L) {
      candidate_dataset[, c(required_columns, available_cpgs), drop = FALSE]
    } else {
      data.frame()
    }
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
  data.frame(
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
}

ugplot_geo_build_transcript_groups_remote <- function(candidates, matrix_files, metadata, cache_dir, target_column,
                                                      threshold, min_samples_pct, source = "processed",
                                                      progress_callback = NULL, cpu_limit = 1L,
                                                      parallel_enabled = FALSE) {
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
  total_transcripts <- length(transcripts)
  initial_completed <- nrow(progress_rows)
  group_count_from_progress <- function(rows) {
    if (!is.data.frame(rows) || nrow(rows) == 0L ||
        !all(c("Status", "CpGKey", "SampleKey") %in% names(rows))) return(0L)
    compatible <- rows[as.character(rows$Status) == "compatible", , drop = FALSE]
    if (nrow(compatible) == 0L) return(0L)
    length(unique(paste(compatible$CpGKey, compatible$SampleKey, sep = "\f")))
  }
  initial_groups <- group_count_from_progress(progress_rows)
  remaining <- setdiff(transcripts, processed)
  cpu_limit <- suppressWarnings(as.integer(cpu_limit %||% 1L))
  if (is.na(cpu_limit) || cpu_limit < 1L) {
    cpu_limit <- 1L
  }
  use_parallel <- isTRUE(parallel_enabled) && cpu_limit > 1L && .Platform$OS.type != "windows" && length(remaining) > 1L
  missing_raw <- remaining[!vapply(remaining, function(transcript_id) {
    file.exists(ugplot_geo_transcript_dataset_path(cache_dir, transcript_id, target_column, source, raw = TRUE))
  }, logical(1))]
  needed_cpgs <- if (length(missing_raw) > 0L) {
    unique(as.character(stats::na.omit(
      candidates$CpG[as.character(candidates$Transcript) %in% missing_raw]
    )))
  } else {
    character(0)
  }
  needed_cpgs <- needed_cpgs[nzchar(needed_cpgs)]
  candidate_dataset <- if (length(needed_cpgs) > 0L) {
    if (!is.null(progress_callback)) {
      progress_callback(
        min(1, initial_completed / max(1, total_transcripts)),
        paste0(
          "Preparing shared transcript matrix: reading ", length(needed_cpgs),
          " CpGs once for ", length(missing_raw), " remaining transcripts; ",
          initial_completed, "/", total_transcripts, " completed; ", initial_groups, " groups found"
        ),
        list(
          name = "transcript_datasets", phase = "preparing_matrix",
          completed = initial_completed, total = total_transcripts,
          remaining = max(0L, total_transcripts - initial_completed),
          groups = initial_groups, candidate_cpgs = length(needed_cpgs),
          rate_per_min = NA_real_, eta_seconds = NA_real_
        )
      )
    }
    matrix_started_at <- Sys.time()
    ugplot_geo_transcript_dataset(
      matrix_files = matrix_files,
      metadata = metadata,
      target_column = target_column,
      cpgs = needed_cpgs,
      progress_callback = function(scanned, found, total) {
        if (is.null(progress_callback)) return(invisible(NULL))
        elapsed <- max(0.001, as.numeric(difftime(Sys.time(), matrix_started_at, units = "secs")))
        rows_per_sec <- scanned / elapsed
        progress_callback(
          min(1, initial_completed / max(1, total_transcripts)),
          paste0(
            "Preparing shared transcript matrix: scanned ",
            format(scanned, big.mark = ",", scientific = FALSE),
            " matrix rows at ", format(round(rows_per_sec), big.mark = ",", scientific = FALSE),
            "/s; found ", found, "/", total, " required CpGs; ",
            initial_completed, "/", total_transcripts, " transcript datasets cached"
          ),
          list(
            name = "transcript_datasets", phase = "preparing_matrix",
            completed = initial_completed, total = total_transcripts,
            remaining = max(0L, total_transcripts - initial_completed),
            groups = initial_groups, candidate_cpgs = length(needed_cpgs),
            matrix_rows_scanned = scanned, matrix_cpgs_found = found,
            matrix_cpgs_total = total, matrix_rows_per_sec = rows_per_sec,
            rate_per_min = NA_real_, eta_seconds = NA_real_
          )
        )
        invisible(NULL)
      }
    )
  } else {
    NULL
  }
  processing_started_at <- Sys.time()
  batches <- if (use_parallel) {
    split(remaining, ceiling(seq_along(remaining) / cpu_limit))
  } else {
    as.list(remaining)
  }
  for (batch in batches) {
    progress_batch <- if (use_parallel) {
      parallel::mclapply(
        batch,
        ugplot_geo_build_transcript_group_progress_row,
        candidates = candidates,
        matrix_files = matrix_files,
        metadata = metadata,
        cache_dir = cache_dir,
        target_column = target_column,
        min_samples_pct = min_samples_pct,
        source = source,
        candidate_dataset = candidate_dataset,
        mc.cores = min(cpu_limit, length(batch)),
        mc.preschedule = FALSE
      )
    } else {
      list(ugplot_geo_build_transcript_group_progress_row(
        batch[[1]],
        candidates = candidates,
        matrix_files = matrix_files,
        metadata = metadata,
        cache_dir = cache_dir,
        target_column = target_column,
        min_samples_pct = min_samples_pct,
        source = source,
        candidate_dataset = candidate_dataset
      ))
    }
    progress_rows <- ugplot_geo_bind_rows(c(list(progress_rows), progress_batch))
    if (exists("ugplot_write_rds_atomic", mode = "function", inherits = TRUE)) {
      ugplot_write_rds_atomic(progress_rows, paths$progress)
    } else {
      saveRDS(progress_rows, paths$progress)
    }
    group_count <- group_count_from_progress(progress_rows)
    checkpoint_tables <- nrow(progress_rows) == length(transcripts) ||
      (nrow(progress_rows) %% max(100L, cpu_limit) < length(progress_batch))
    if (isTRUE(checkpoint_tables)) {
      tables <- ugplot_geo_build_group_tables_remote(progress_rows, candidates = candidates)
      utils::write.csv(tables$summary, paths$summary, row.names = FALSE)
      utils::write.csv(tables$details, paths$details, row.names = FALSE)
    }
    if (!is.null(progress_callback)) {
      completed <- nrow(progress_rows)
      remaining_count <- max(0L, total_transcripts - completed)
      elapsed_seconds <- max(0.001, as.numeric(difftime(Sys.time(), processing_started_at, units = "secs")))
      completed_this_run <- max(0L, completed - initial_completed)
      rate_per_min <- if (completed_this_run > 0L) 60 * completed_this_run / elapsed_seconds else NA_real_
      eta_seconds <- if (is.finite(rate_per_min) && rate_per_min > 0) 60 * remaining_count / rate_per_min else NA_real_
      eta_label <- if (is.finite(eta_seconds)) {
        if (eta_seconds < 120) paste0(round(eta_seconds), "s") else if (eta_seconds < 7200) paste0(round(eta_seconds / 60), "m") else paste0(round(eta_seconds / 3600, 1), "h")
      } else {
        "calculating"
      }
      progress_callback(
        min(1, completed / max(1, total_transcripts)),
        paste0(
          "Building transcript datasets: ", completed, "/", total_transcripts,
          " (", remaining_count, " remaining); ", group_count, " groups; ",
          if (is.finite(rate_per_min)) paste0(round(rate_per_min, 1), "/min") else "measuring rate",
          "; ETA ", eta_label
        ),
        list(
          name = "transcript_datasets", phase = "building_datasets",
          completed = completed, total = total_transcripts,
          remaining = remaining_count, groups = group_count,
          candidate_cpgs = length(needed_cpgs),
          rate_per_min = rate_per_min, eta_seconds = eta_seconds
        )
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

ugplot_geo_collect_model_timing <- function(transcript_ml_dir) {
  if (!nzchar(transcript_ml_dir %||% "") || !dir.exists(transcript_ml_dir)) {
    return(data.frame())
  }
  paths <- list.files(
    transcript_ml_dir,
    pattern = "^(screen_result|stability_result)\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(paths) == 0L) {
    return(data.frame())
  }
  rows <- lapply(paths, function(path) {
    result <- tryCatch(readRDS(path), error = function(e) NULL)
    table <- result$results_table %||% data.frame()
    if (!is.data.frame(table) || nrow(table) == 0L) {
      return(NULL)
    }
    relative <- substring(normalizePath(path, mustWork = FALSE), nchar(normalizePath(transcript_ml_dir, mustWork = FALSE)) + 2L)
    table$Analysis <- relative
    table
  })
  ugplot_model_timing_summary(Filter(Negate(is.null), rows))
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

ugplot_geo_stability_complete_groups <- function(screen_summary, stability_summary,
                                                  config = list(), metadata = NULL) {
  if (!is.data.frame(screen_summary) || nrow(screen_summary) == 0L || !"GroupID" %in% names(screen_summary)) {
    return(character(0))
  }
  column <- as.character(config$geo_ml_stability_group_column %||% "")
  strata <- if (nzchar(column)) ugplot_geo_ml_stability_strata(metadata, column) else {
    data.frame(StratumColumn = "", StratumValue = "", stringsAsFactors = FALSE)
  }
  if (!is.data.frame(strata) || nrow(strata) == 0L) {
    strata <- data.frame(StratumColumn = "", StratumValue = "", stringsAsFactors = FALSE)
  }
  existing_keys <- if (is.data.frame(stability_summary) && nrow(stability_summary) > 0L &&
                       "GroupID" %in% names(stability_summary)) {
    existing_column <- if ("StratumColumn" %in% names(stability_summary)) stability_summary$StratumColumn else rep("", nrow(stability_summary))
    existing_value <- if ("StratumValue" %in% names(stability_summary)) stability_summary$StratumValue else rep("", nrow(stability_summary))
    ugplot_geo_ml_stability_task_key(stability_summary$GroupID, existing_column, existing_value)
  } else character(0)
  group_ids <- unique(as.character(screen_summary$GroupID))
  group_ids[vapply(group_ids, function(group_id) {
    row <- screen_summary[as.character(screen_summary$GroupID) == group_id, , drop = FALSE]
    best_model <- as.character(row$BestModel[[1]] %||% "")
    if (!nzchar(best_model) || identical(best_model, "-")) return(TRUE)
    required <- ugplot_geo_ml_stability_task_key(
      rep(group_id, nrow(strata)), strata$StratumColumn, strata$StratumValue
    )
    all(required %in% existing_keys)
  }, logical(1))]
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
    skip_remaining_model_seeds_on_timeout = TRUE,
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
    dataset <- if (grepl("\\.rds$", dataset_path, ignore.case = TRUE)) {
      readRDS(dataset_path)
    } else {
      utils::read.csv(dataset_path, stringsAsFactors = FALSE, check.names = FALSE)
    }
  } else {
    stop("Transcript group dataset file is missing: ", dataset_path, call. = FALSE)
  }
  if (!is.data.frame(dataset)) {
    stop("Transcript group dataset must contain a data frame: ", dataset_path, call. = FALSE)
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

ugplot_geo_screen_group <- function(dataset, group, source, config, screen_path,
                                    importance_path, progress_callback = NULL,
                                    partial_callback = NULL) {
  if (!is.data.frame(dataset) || !is.data.frame(group) || nrow(group) != 1L) {
    stop("A single transcript group and its dataset are required.", call. = FALSE)
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
  ugplot_ensure_dir(dirname(screen_path))
  ugplot_ensure_dir(dirname(importance_path))
  screen_config <- ugplot_geo_ml_pipeline_config(
    models,
    screen_seeds,
    timeout,
    cpu_limit = cpu_limit,
    parallel_enabled = isTRUE(config$parallel_enabled),
    restart_parallel_each_model = isTRUE(config$restart_parallel_each_model %||% TRUE),
    retry_parallel_connection_errors = isTRUE(config$retry_parallel_connection_errors %||% TRUE)
  )
  screen_config$resume_result_path <- screen_path
  screen_config$model_log_dir <- file.path(dirname(screen_path), "logs", "screen")
  screen_result <- ugplot_run_ml_job(
    dataset,
    screen_config,
    progress_callback = progress_callback %||% function(...) NULL,
    partial_callback = function(partial) {
      ugplot_geo_write_checkpoint(partial, screen_path)
      if (is.function(partial_callback)) partial_callback(partial)
    }
  )
  ugplot_geo_write_checkpoint(screen_result, screen_path)
  metric_values <- ugplot_geo_ml_metric_values(screen_result)
  model_counts <- ugplot_geo_ml_model_run_counts(screen_result)
  importance <- ugplot_geo_ml_importance_table(screen_result$best_model, group, source, "screening")
  if (is.data.frame(importance) && nrow(importance) > 0) {
    utils::write.csv(importance, importance_path, row.names = FALSE)
  }
  summary_row <- data.frame(
    Source = source,
    Phase = "screening",
    GroupID = as.character(group$GroupID[[1]]),
    PrincipalTranscript = group$PrincipalTranscript[[1]],
    Gene = group$Gene[[1]],
    Columns = group$Columns[[1]],
    Samples = group$Samples[[1]],
    TranscriptCount = group$TranscriptCount[[1]],
    TranscriptMembers = as.character(group$TranscriptMembers[[1]] %||% group$PrincipalTranscript[[1]]),
    GeneMembers = as.character(group$GeneMembers[[1]] %||% group$Gene[[1]]),
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
    DatasetPath = as.character(config$coordinator_dataset_path %||% ""),
    ScreenResultPath = screen_path,
    ImportancePath = if (file.exists(importance_path)) importance_path else "",
    stringsAsFactors = FALSE
  )
  list(summary = summary_row, screen_result = screen_result, importance = importance)
}

ugplot_geo_complete_group_stability <- function(dataset, screen_summary, source, config,
                                                task_dir,
                                                progress_callback = NULL,
                                                partial_callback = NULL) {
  if (!is.data.frame(screen_summary) || nrow(screen_summary) != 1L) {
    stop("A single screening summary is required for group stability.", call. = FALSE)
  }
  local_cache <- file.path(task_dir, "complete-group-cache")
  dir.create(local_cache, recursive = TRUE, showWarnings = FALSE)
  dataset_path <- file.path(task_dir, "complete-group-dataset.rds")
  saveRDS(dataset, dataset_path)
  local_summary <- screen_summary
  local_summary$DatasetPath <- dataset_path
  stability <- ugplot_geo_run_transcript_stability_remote(
    screen_summary = local_summary,
    cache_dir = local_cache,
    source = source,
    config = config,
    metadata = config$geo_stability_metadata %||% NULL,
    progress_callback = progress_callback,
    partial_callback = partial_callback
  )
  artifacts <- if (is.data.frame(stability) && nrow(stability) > 0L) {
    unname(lapply(seq_len(nrow(stability)), function(i) {
      row <- stability[i, , drop = FALSE]
      result_path <- as.character(row$StabilityResultPath[[1]] %||% "")
      importance_path <- as.character(row$ImportancePath[[1]] %||% "")
      list(
        summary = row,
        result = if (nzchar(result_path) && file.exists(result_path)) readRDS(result_path) else NULL,
        importance = if (nzchar(importance_path) && file.exists(importance_path)) {
          utils::read.csv(importance_path, stringsAsFactors = FALSE, check.names = FALSE)
        } else data.frame()
      )
    }))
  } else list()
  list(summary = stability, artifacts = artifacts)
}

ugplot_run_geo_complete_group_job <- function(dataset, config = list(),
                                              progress_callback = function(...) NULL,
                                              partial_callback = NULL) {
  group <- config$distributed_group
  if (!is.data.frame(group) || nrow(group) != 1L) {
    stop("Distributed transcript analysis config is missing its group.", call. = FALSE)
  }
  task_dir <- as.character(config$job_dir %||% tempdir())
  group_id <- as.character(group$GroupID[[1]])
  resume_screen <- config$distributed_resume_screen %||% NULL
  can_resume_screen <- is.list(resume_screen) &&
    is.data.frame(resume_screen$summary) &&
    nrow(resume_screen$summary) == 1L &&
    "GroupID" %in% names(resume_screen$summary) &&
    identical(as.character(resume_screen$summary$GroupID[[1]]), group_id) &&
    !is.null(resume_screen$screen_result)
  result <- if (isTRUE(can_resume_screen)) {
    progress_callback(
      progress = 0.55,
      message = paste0("Reusing saved screening for ", group_id),
      phase = "screening_reused"
    )
    list(
      summary = resume_screen$summary,
      screen_result = resume_screen$screen_result,
      importance = if (is.data.frame(resume_screen$importance)) {
        resume_screen$importance
      } else {
        data.frame()
      }
    )
  } else {
    ugplot_geo_screen_group(
      dataset = dataset,
      group = group,
      source = as.character(config$matrix_source %||% "processed"),
      config = config,
      screen_path = file.path(task_dir, "worker-screen-result.rds"),
      importance_path = file.path(task_dir, "worker-screen-importance.csv"),
      progress_callback = function(...) {
        args <- list(...)
        screen_progress <- suppressWarnings(as.numeric(args$progress %||% 0))
        if (!is.finite(screen_progress)) screen_progress <- 0
        progress_callback(
          progress = 0.55 * max(0, min(1, screen_progress)),
          message = paste0("Screening ", group_id, ": ", args$message %||% ""),
          current_run = args$current_run %||% list(),
          phase = "screening"
        )
      },
      partial_callback = partial_callback
    )
  }
  stability <- ugplot_geo_complete_group_stability(
    dataset = dataset,
    screen_summary = result$summary,
    source = as.character(config$matrix_source %||% "processed"),
    config = config,
    task_dir = task_dir,
    progress_callback = function(value, message) {
      value <- suppressWarnings(as.numeric(value %||% 0))
      if (!is.finite(value)) value <- 0
      progress_callback(
        progress = 0.55 + 0.45 * max(0, min(1, value)),
        message = paste0("Stabilizing ", group_id, ": ", message %||% ""),
        phase = "stability"
      )
    },
    partial_callback = partial_callback
  )
  list(
    kind = "geo_complete_group",
    protocol_version = 2L,
    parent_job_id = as.character(config$parent_job_id %||% ""),
    worker_name = as.character(config$worker_name %||% ""),
    group_id = group_id,
    summary = result$summary,
    screen_result = result$screen_result,
    importance = result$importance,
    stability_summary = stability$summary,
    stability_artifacts = stability$artifacts
  )
}

ugplot_run_geo_screen_group_job <- function(dataset, config = list(),
                                            progress_callback = function(...) NULL,
                                            partial_callback = NULL) {
  ugplot_run_geo_complete_group_job(dataset, config, progress_callback, partial_callback)
}

ugplot_geo_distributed_workers <- function(config) {
  workers <- config$distributed_workers %||% list()
  if (is.data.frame(workers)) {
    workers <- lapply(seq_len(nrow(workers)), function(i) as.list(workers[i, , drop = FALSE]))
  }
  workers <- Filter(function(worker) {
    is.list(worker) &&
      nzchar(as.character(worker$name %||% "")) &&
      nzchar(as.character(worker$url %||% ""))
  }, workers)
  lapply(workers, function(worker) {
    cpu_limit <- suppressWarnings(as.integer(worker$cpu_limit %||% 1L))
    if (is.na(cpu_limit) || cpu_limit < 1L) {
      cpu_limit <- 1L
    }
    worker$cpu_limit <- cpu_limit
    worker
  })
}

ugplot_geo_distributed_manifest_path <- function(pipeline_dir) {
  file.path(pipeline_dir, "distributed-screening.rds")
}

ugplot_geo_write_distributed_manifest <- function(manifest, path) {
  if (exists("ugplot_write_rds_atomic", mode = "function", inherits = TRUE)) {
    ugplot_write_rds_atomic(manifest, path)
  } else {
    saveRDS(manifest, path)
  }
  invisible(path)
}

# ML checkpoints must survive an abrupt process or server stop. Writing to a
# temporary file first prevents a half-written RDS from replacing the last
# usable checkpoint.
ugplot_geo_write_checkpoint <- function(value, path) {
  if (exists("ugplot_write_rds_atomic", mode = "function", inherits = TRUE)) {
    ugplot_write_rds_atomic(value, path)
  } else {
    ugplot_ensure_dir(dirname(path))
    temporary_path <- paste0(path, ".tmp-", Sys.getpid())
    on.exit(unlink(temporary_path, force = TRUE), add = TRUE)
    saveRDS(value, temporary_path)
    if (!file.rename(temporary_path, path)) {
      stop("Could not write checkpoint: ", path, call. = FALSE)
    }
  }
  invisible(path)
}

ugplot_geo_can_resume_worker_task <- function(status, attempts, max_attempts, draining = FALSE) {
  attempts <- suppressWarnings(as.integer(attempts %||% 0L))
  max_attempts <- suppressWarnings(as.integer(max_attempts %||% 1L))
  if (is.na(attempts)) attempts <- 0L
  if (is.na(max_attempts) || max_attempts < 1L) max_attempts <- 1L
  !isTRUE(draining) && is.list(status) && isTRUE(status$resumable) && attempts < max_attempts
}

ugplot_geo_transcript_group_cache_complete <- function(candidates, group_paths) {
  if (!is.data.frame(candidates) || nrow(candidates) == 0 ||
      !"Transcript" %in% names(candidates) ||
      !file.exists(group_paths$summary) ||
      !file.exists(group_paths$details) ||
      !file.exists(group_paths$progress)) {
    return(FALSE)
  }
  candidate_transcripts <- unique(trimws(as.character(stats::na.omit(candidates$Transcript))))
  candidate_transcripts <- candidate_transcripts[nzchar(candidate_transcripts)]
  progress <- tryCatch(readRDS(group_paths$progress), error = function(e) data.frame())
  processed_transcripts <- if (is.data.frame(progress) && "Transcript" %in% names(progress)) {
    unique(trimws(as.character(stats::na.omit(progress$Transcript))))
  } else {
    character(0)
  }
  length(candidate_transcripts) > 0 &&
    all(candidate_transcripts %in% processed_transcripts)
}

ugplot_geo_retry_has_compatible_worker <- function(previous_worker, available_worker_names,
                                                   configured_worker_count) {
  previous_worker <- as.character(previous_worker %||% "")
  available_worker_names <- as.character(available_worker_names)
  !nzchar(previous_worker) ||
    configured_worker_count == 1L ||
    any(available_worker_names != previous_worker)
}

ugplot_geo_drain_ready <- function(busy_workers, collaboration_leased = logical(0)) {
  length(busy_workers) == 0L && !any(as.logical(collaboration_leased))
}

ugplot_geo_collaboration_group_from_task_id <- function(task_id, parent_job_id) {
  task_id <- as.character(task_id %||% "")
  parent_job_id <- as.character(parent_job_id %||% "")
  prefixes <- paste0(parent_job_id, c(":analyze:", ":screen:"))
  matching <- prefixes[startsWith(task_id, prefixes)]
  if (length(matching) == 0L) return("")
  substring(task_id, nchar(matching[[1]]) + 1L)
}

ugplot_geo_distributed_active_tasks <- function(manifest) {
  if (!is.data.frame(manifest) || nrow(manifest) == 0L) return(list())
  rows <- which(as.character(manifest$State) %in% c("dispatching", "submitted", "running"))
  lapply(rows, function(row_index) {
    progress <- suppressWarnings(as.numeric(manifest$Progress[[row_index]] %||% 0))
    if (length(progress) != 1L || !is.finite(progress)) progress <- 0
    list(
      worker = as.character(manifest$Worker[[row_index]] %||% ""),
      group = as.character(manifest$GroupID[[row_index]] %||% ""),
      job_id = as.character(manifest$JobID[[row_index]] %||% ""),
      state = as.character(manifest$State[[row_index]] %||% ""),
      progress = max(0, min(1, progress)),
      message = as.character(manifest$Message[[row_index]] %||% ""),
      error = as.character(manifest$Error[[row_index]] %||% ""),
      updated_at = as.character(manifest$UpdatedAt[[row_index]] %||% "")
    )
  })
}

ugplot_geo_distributed_resume_config <- function(config, group_id, summaries,
                                                  stability_summaries) {
  config$distributed_resume_screen <- NULL
  config$distributed_resume_stability_summary <- NULL
  group_id <- as.character(group_id %||% "")
  if (!nzchar(group_id)) return(config)

  screen_rows <- if (is.data.frame(summaries) && "GroupID" %in% names(summaries)) {
    summaries[as.character(summaries$GroupID) == group_id, , drop = FALSE]
  } else {
    data.frame()
  }
  if (nrow(screen_rows) > 0L) {
    screen_row <- screen_rows[1, , drop = FALSE]
    screen_path <- if ("ScreenResultPath" %in% names(screen_row)) {
      as.character(screen_row$ScreenResultPath[[1]] %||% "")
    } else {
      ""
    }
    screen_result <- if (!is.na(screen_path) && nzchar(screen_path) && file.exists(screen_path)) {
      tryCatch(readRDS(screen_path), error = function(e) NULL)
    } else {
      NULL
    }
    if (!is.null(screen_result)) {
      importance_path <- if ("ImportancePath" %in% names(screen_row)) {
        as.character(screen_row$ImportancePath[[1]] %||% "")
      } else {
        ""
      }
      importance <- if (!is.na(importance_path) && nzchar(importance_path) &&
                        file.exists(importance_path)) {
        tryCatch(
          utils::read.csv(importance_path, stringsAsFactors = FALSE, check.names = FALSE),
          error = function(e) data.frame()
        )
      } else {
        data.frame()
      }
      config$distributed_resume_screen <- list(
        summary = screen_row,
        screen_result = screen_result,
        importance = importance
      )
    }
  }

  if (is.data.frame(stability_summaries) && nrow(stability_summaries) > 0L &&
      "GroupID" %in% names(stability_summaries)) {
    rows <- stability_summaries[
      as.character(stability_summaries$GroupID) == group_id,
      ,
      drop = FALSE
    ]
    if (nrow(rows) > 0L) {
      config$distributed_resume_stability_summary <- rows
    }
  }
  config
}

ugplot_geo_run_transcript_ml_distributed <- function(eligible, summaries, summary_path,
                                                     pipeline_dir, cache_dir, source,
                                                     run_key, config, workers,
                                                     progress_callback = NULL) {
  manifest_path <- ugplot_geo_distributed_manifest_path(pipeline_dir)
  stability_column <- as.character(config$geo_ml_stability_group_column %||% "")
  stability_summary_path <- if (nzchar(stability_column)) {
    file.path(pipeline_dir, paste0("summary_by_", ugplot_geo_safe_token(stability_column), ".csv"))
  } else file.path(pipeline_dir, "summary.csv")
  stability_summaries <- if (file.exists(stability_summary_path)) {
    tryCatch(utils::read.csv(stability_summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  } else data.frame()
  stability_strata <- if (nzchar(stability_column)) {
    ugplot_geo_ml_stability_strata(config$geo_stability_metadata %||% NULL, stability_column)
  } else data.frame(StratumColumn = "", StratumValue = "", stringsAsFactors = FALSE)
  if (!is.data.frame(stability_strata) || nrow(stability_strata) == 0L) {
    stability_strata <- data.frame(StratumColumn = "", StratumValue = "", stringsAsFactors = FALSE)
  }
  stability_group_complete <- function(group_id, screen_rows = summaries) {
    screen_row <- screen_rows[as.character(screen_rows$GroupID) == as.character(group_id), , drop = FALSE]
    if (nrow(screen_row) == 0L) return(FALSE)
    best_model <- as.character(screen_row$BestModel[[1]] %||% "")
    if (!nzchar(best_model) || identical(best_model, "-")) return(TRUE)
    if (!is.data.frame(stability_summaries) || nrow(stability_summaries) == 0L ||
        !"GroupID" %in% names(stability_summaries)) return(FALSE)
    existing_column <- if ("StratumColumn" %in% names(stability_summaries)) stability_summaries$StratumColumn else rep("", nrow(stability_summaries))
    existing_value <- if ("StratumValue" %in% names(stability_summaries)) stability_summaries$StratumValue else rep("", nrow(stability_summaries))
    existing_keys <- ugplot_geo_ml_stability_task_key(stability_summaries$GroupID, existing_column, existing_value)
    required_keys <- ugplot_geo_ml_stability_task_key(
      rep(group_id, nrow(stability_strata)),
      stability_strata$StratumColumn,
      stability_strata$StratumValue
    )
    all(required_keys %in% existing_keys)
  }
  manifest <- if (file.exists(manifest_path)) {
    tryCatch(readRDS(manifest_path), error = function(e) data.frame())
  } else {
    data.frame()
  }
  required <- c(
    "GroupID", "Worker", "JobID", "State", "Progress", "Message", "UpdatedAt",
    "Attempts", "PollFailures", "Error"
  )
  if (!is.data.frame(manifest)) {
    manifest <- data.frame()
  }
  for (column_name in setdiff(required, names(manifest))) {
    manifest[[column_name]] <- switch(
      column_name,
      Progress = numeric(nrow(manifest)),
      Attempts = integer(nrow(manifest)),
      PollFailures = integer(nrow(manifest)),
      rep("", nrow(manifest))
    )
  }
  manifest <- manifest[, required, drop = FALSE]
  manifest$State[manifest$State == "dispatching"] <- "pending"
  eligible_ids <- as.character(eligible$GroupID)
  manifest <- manifest[as.character(manifest$GroupID) %in% eligible_ids, , drop = FALSE]
  missing_ids <- setdiff(eligible_ids, as.character(manifest$GroupID))
  if (length(missing_ids) > 0) {
    manifest <- rbind(
      manifest,
      data.frame(
        GroupID = missing_ids,
        Worker = "",
        JobID = "",
        State = "pending",
        Progress = 0,
        Message = "Waiting for worker",
        UpdatedAt = "",
        Attempts = 0L,
        PollFailures = 0L,
        Error = "",
        stringsAsFactors = FALSE
      )
    )
  }
  processed <- if (is.data.frame(summaries) && "GroupID" %in% names(summaries)) {
    candidate_ids <- unique(as.character(summaries$GroupID))
    candidate_ids[vapply(candidate_ids, stability_group_complete, logical(1))]
  } else {
    character(0)
  }
  manifest$State[manifest$State == "completed" & !manifest$GroupID %in% processed] <- "pending"
  manifest$State[manifest$GroupID %in% processed] <- "completed"
  retry_rows <- manifest$State == "pending"
  manifest$Attempts[retry_rows] <- 0L
  manifest$PollFailures[retry_rows] <- 0L
  manifest$JobID[retry_rows] <- ""
  ugplot_geo_write_distributed_manifest(manifest, manifest_path)

  worker_by_name <- function(name) {
    matches <- Filter(function(worker) identical(as.character(worker$name), as.character(name)), workers)
    if (length(matches) == 0) NULL else matches[[1]]
  }
  group_by_id <- function(group_id) {
    eligible[as.character(eligible$GroupID) == as.character(group_id), , drop = FALSE][1, , drop = FALSE]
  }
  save_completed <- function(row_index, remote_result) {
    group_id <- as.character(manifest$GroupID[[row_index]])
    if (!is.list(remote_result) ||
        !(as.character(remote_result$kind %||% "") %in% c("geo_screen_group", "geo_complete_group")) ||
        !identical(as.character(remote_result$group_id %||% ""), group_id) ||
        !is.data.frame(remote_result$summary) ||
        nrow(remote_result$summary) != 1L) {
      stop("Worker returned an invalid result for group ", group_id, ".", call. = FALSE)
    }
    group <- group_by_id(group_id)
    dataset_info <- ugplot_geo_ml_group_dataset(group)
    group_dir <- ugplot_geo_transcript_ml_group_dir(cache_dir, source, group_id, run_key)
    screen_path <- file.path(group_dir, "screen_result.rds")
    importance_path <- file.path(group_dir, "screen_importance.csv")
    ugplot_ensure_dir(group_dir)
    if (!is.null(remote_result$screen_result)) {
      saveRDS(remote_result$screen_result, screen_path)
    }
    if (is.data.frame(remote_result$importance) && nrow(remote_result$importance) > 0) {
      utils::write.csv(remote_result$importance, importance_path, row.names = FALSE)
    }
    summary_row <- remote_result$summary
    summary_row$DatasetPath <- dataset_info$dataset_path
    summary_row$ScreenResultPath <- if (file.exists(screen_path)) screen_path else ""
    summary_row$ImportancePath <- if (file.exists(importance_path)) importance_path else ""
    summaries <<- ugplot_geo_bind_rows(list(
      summaries[as.character(summaries$GroupID) != group_id, , drop = FALSE],
      summary_row
    ))
    summaries <<- ugplot_geo_ml_rank_summary(summaries)
    utils::write.csv(summaries, summary_path, row.names = FALSE)
    artifacts <- remote_result$stability_artifacts %||% list()
    if (identical(as.character(remote_result$kind %||% ""), "geo_complete_group") &&
        length(artifacts) > 0L) {
      for (artifact in artifacts) {
        if (!is.list(artifact) || !is.data.frame(artifact$summary) || nrow(artifact$summary) != 1L) next
        stability_row <- artifact$summary
        stratum_col <- as.character(stability_row$StratumColumn[[1]] %||% "")
        stratum_value <- as.character(stability_row$StratumValue[[1]] %||% "")
        target_dir <- group_dir
        if (nzchar(stratum_col)) {
          target_dir <- file.path(target_dir, "stability_by", ugplot_geo_safe_token(stratum_col), ugplot_geo_safe_token(stratum_value))
        }
        ugplot_ensure_dir(target_dir)
        target_result <- file.path(target_dir, "stability_result.rds")
        target_importance <- file.path(target_dir, "importance.csv")
        if (!is.null(artifact$result)) saveRDS(artifact$result, target_result)
        if (is.data.frame(artifact$importance) && nrow(artifact$importance) > 0L) {
          utils::write.csv(artifact$importance, target_importance, row.names = FALSE)
        }
        stability_row$DatasetPath <- dataset_info$dataset_path
        stability_row$StabilityResultPath <- if (file.exists(target_result)) target_result else ""
        stability_row$ImportancePath <- if (file.exists(target_importance)) target_importance else ""
        if (is.data.frame(stability_summaries) && nrow(stability_summaries) > 0L) {
          old_col <- if ("StratumColumn" %in% names(stability_summaries)) stability_summaries$StratumColumn else rep("", nrow(stability_summaries))
          old_value <- if ("StratumValue" %in% names(stability_summaries)) stability_summaries$StratumValue else rep("", nrow(stability_summaries))
          keep <- ugplot_geo_ml_stability_task_key(stability_summaries$GroupID, old_col, old_value) !=
            ugplot_geo_ml_stability_task_key(group_id, stratum_col, stratum_value)
          stability_summaries <<- ugplot_geo_bind_rows(list(stability_summaries[keep, , drop = FALSE], stability_row))
        } else stability_summaries <<- stability_row
      }
      stability_summaries <<- ugplot_geo_ml_rank_summary(stability_summaries)
      utils::write.csv(stability_summaries, stability_summary_path, row.names = FALSE)
    }
    if (!stability_group_complete(group_id)) {
      manifest$State[[row_index]] <<- "pending"
      manifest$JobID[[row_index]] <<- ""
      manifest$Progress[[row_index]] <<- 0.55
      manifest$Message[[row_index]] <<- "Screening recovered; complete stability still required"
      manifest$UpdatedAt[[row_index]] <<- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
      manifest$Attempts[[row_index]] <<- 0L
      ugplot_geo_write_distributed_manifest(manifest, manifest_path)
      return(invisible(FALSE))
    }
    manifest$State[[row_index]] <<- "completed"
    manifest$Progress[[row_index]] <<- 1
    manifest$Message[[row_index]] <<- "Completed"
    manifest$UpdatedAt[[row_index]] <<- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    manifest$PollFailures[[row_index]] <<- 0L
    manifest$Error[[row_index]] <<- ""
    collaboration_task_id <- paste(parent_job_id, "analyze", group_id, sep = ":")
    if (exists("ugplot_collaboration_cancel_task", mode = "function", inherits = TRUE)) {
      try(ugplot_collaboration_cancel_task(collaboration_task_id, jobs_dir = collaboration_jobs_dir), silent = TRUE)
    }
    ugplot_geo_write_distributed_manifest(manifest, manifest_path)
    invisible(TRUE)
  }
  report_progress <- function() {
    done <- sum(manifest$State == "completed")
    active_rows <- which(manifest$State %in% c("dispatching", "submitted", "running"))
    active_tasks <- ugplot_geo_distributed_active_tasks(manifest)
    active <- if (length(active_rows) > 0) {
      paste0(manifest$Worker[active_rows], ":", manifest$GroupID[active_rows], collapse = ", ")
    } else {
      "waiting"
    }
    if (!is.null(progress_callback)) {
      progress_callback(
        done / max(1L, nrow(manifest)),
        paste0(
          "Distributed complete analysis: ", done, "/", nrow(manifest),
          " group(s); active ", active
        ),
        distributed_state = list(
          phase = "complete_group_analysis",
          workers = unique(vapply(workers, function(worker) as.character(worker$name), character(1))),
          completed = done,
          total = nrow(manifest),
          active = length(active_rows),
          active_groups = if (length(active_rows) > 0L) {
            paste0(manifest$Worker[active_rows], ":", manifest$GroupID[active_rows])
          } else {
            character(0)
          },
          active_tasks = active_tasks
        )
      )
    }
  }
  max_attempts <- suppressWarnings(as.integer(config$distributed_max_attempts %||% 2L))
  if (is.na(max_attempts) || max_attempts < 1L) {
    max_attempts <- 2L
  }
  poll_seconds <- suppressWarnings(as.numeric(config$distributed_poll_seconds %||% 3))
  if (is.na(poll_seconds) || poll_seconds < 1) {
    poll_seconds <- 1
  }
  parent_job_id <- basename(as.character(config$job_dir %||% ""))
  collaboration_jobs_dir <- dirname(as.character(config$job_dir %||% ""))
  collaboration_enabled <- !identical(config$collaboration_enabled, FALSE) &&
    nzchar(parent_job_id) && dir.exists(collaboration_jobs_dir) &&
    exists("ugplot_collaboration_publish_task", mode = "function", inherits = TRUE)
  collaboration_queue_depth <- suppressWarnings(as.integer(config$collaboration_queue_depth %||% 8L))
  if (is.na(collaboration_queue_depth) || collaboration_queue_depth < 1L) collaboration_queue_depth <- 8L
  collaboration_task_id <- function(group_id) paste(parent_job_id, "analyze", group_id, sep = ":")
  legacy_collaboration_task_id <- function(group_id) paste(parent_job_id, "screen", group_id, sep = ":")
  collaboration_index_ids <- function(states) {
    if (!isTRUE(collaboration_enabled) ||
        !exists("ugplot_collaboration_task_ids", mode = "function", inherits = TRUE)) {
      return(character(0))
    }
    tryCatch(
      ugplot_collaboration_task_ids(
        collaboration_jobs_dir,
        states = states,
        parent_job_id = parent_job_id
      ),
      error = function(e) character(0)
    )
  }
  collaboration_group_id <- function(task_id) {
    ugplot_geo_collaboration_group_from_task_id(task_id, parent_job_id)
  }

  repeat {
    draining <- exists("ugplot_job_drain_requested", mode = "function", inherits = TRUE) &&
      ugplot_job_drain_requested(config$job_dir %||% "")
    if (isTRUE(collaboration_enabled)) {
      completed_task_ids <- collaboration_index_ids("completed")
      for (task_id in completed_task_ids) {
        group_id <- collaboration_group_id(task_id)
        row_index <- match(group_id, as.character(manifest$GroupID))
        if (is.na(row_index) || manifest$State[[row_index]] == "completed") next
        contributed <- tryCatch(
          ugplot_collaboration_take_result(
            task_id,
            collaboration_jobs_dir
          ),
          error = function(e) NULL
        )
        if (is.list(contributed) && !is.null(contributed$result)) {
          save_completed(row_index, contributed$result)
        }
      }

      offer_rows <- if (isTRUE(draining)) integer(0) else utils::head(which(manifest$State == "pending"), collaboration_queue_depth)
      for (row_index in offer_rows) {
        try(ugplot_collaboration_cancel_task(
          legacy_collaboration_task_id(manifest$GroupID[[row_index]]),
          reason = "replaced_by_complete_group_analysis",
          jobs_dir = collaboration_jobs_dir
        ), silent = TRUE)
        group <- group_by_id(manifest$GroupID[[row_index]])
        dataset_info <- ugplot_geo_ml_group_dataset(group)
        task_config <- config
        task_config$distributed_workers <- NULL
        task_config$resume_result <- NULL
        task_config$resume_result_path <- NULL
        task_config$resume_completed_keys <- NULL
        task_config$jobs_dir <- NULL
        task_config$job_dir <- NULL
        task_config$model_log_dir <- NULL
        task_config$runner <- "ugplot_run_geo_complete_group_job"
        task_config$type <- "geo_worker"
        task_config$distributed_group <- group
        task_config$matrix_source <- source
        task_config$coordinator_dataset_path <- dataset_info$dataset_path
        task_config$cpu_limit <- 1L
        task_config <- ugplot_geo_distributed_resume_config(
          task_config,
          manifest$GroupID[[row_index]],
          summaries,
          stability_summaries
        )
        task_config$collaboration_enabled <- FALSE
        task_config$request_id <- collaboration_task_id(manifest$GroupID[[row_index]])
        task_config$collaboration_required_models <- ugplot_collaboration_required_models(task_config)
        mission <- config$collaboration_mission %||% list(
          title = paste("Analyze", as.character(manifest$GroupID[[row_index]])),
          entity = list(id = as.character(manifest$GroupID[[row_index]])),
          dataset = list(rows = nrow(dataset_info$dataset), columns = ncol(dataset_info$dataset)),
          stages = c("dataset_received", "dataset_profiled", "experiment_started", "metric_updated", "validation_completed", "result_accepted")
        )
        try(
          ugplot_collaboration_publish_task(
            collaboration_task_id(manifest$GroupID[[row_index]]),
            parent_job_id,
            payload = list(dataset = dataset_info$dataset, config = task_config),
            requirements = list(models = task_config$collaboration_required_models, protocol_version = 2L),
            mission = mission,
            jobs_dir = collaboration_jobs_dir
          ),
          silent = TRUE
        )
      }
      if (isTRUE(draining)) {
        for (task_id in collaboration_index_ids("pending")) {
          try(ugplot_collaboration_close_pending_task(
            task_id,
            jobs_dir = collaboration_jobs_dir
          ), silent = TRUE)
        }
      }
    }

    if (all(manifest$State == "completed")) {
      break
    }

    active_rows <- which(manifest$State %in% c("submitted", "running"))
    for (row_index in active_rows) {
      worker <- worker_by_name(manifest$Worker[[row_index]])
      if (is.null(worker)) {
        manifest$State[[row_index]] <- "pending"
        manifest$JobID[[row_index]] <- ""
        manifest$Error[[row_index]] <- "Worker is no longer configured."
        next
      }
      status <- tryCatch(
        ugplot_remote_job_status(worker$url, manifest$JobID[[row_index]], worker$token %||% ""),
        error = function(e) e
      )
      if (inherits(status, "error")) {
        manifest$PollFailures[[row_index]] <- manifest$PollFailures[[row_index]] + 1L
        manifest$Error[[row_index]] <- conditionMessage(status)
        manifest$Message[[row_index]] <- paste0(
          "Waiting for ", as.character(worker$name), " status; checkpoint remains assigned (",
          manifest$PollFailures[[row_index]], " failed check(s))"
        )
        manifest$UpdatedAt[[row_index]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
        next
      }
      manifest$PollFailures[[row_index]] <- 0L
      remote_state <- as.character(status$state %||% "")
      remote_progress <- suppressWarnings(as.numeric(status$progress %||% 0))
      if (length(remote_progress) != 1L || !is.finite(remote_progress)) remote_progress <- 0
      manifest$Progress[[row_index]] <- max(0, min(1, remote_progress))
      manifest$Message[[row_index]] <- as.character(status$message %||% remote_state)
      manifest$UpdatedAt[[row_index]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
      if (identical(remote_state, "finished")) {
        remote_result <- ugplot_remote_get_result(
          worker$url,
          manifest$JobID[[row_index]],
          worker$token %||% ""
        )
        save_completed(row_index, remote_result)
        try(ugplot_remote_delete_job(
          worker$url,
          manifest$JobID[[row_index]],
          worker$token %||% ""
        ), silent = TRUE)
      } else if (remote_state %in% c("failed", "stopped")) {
        if (ugplot_geo_can_resume_worker_task(
          status, manifest$Attempts[[row_index]], max_attempts, draining
        )) {
          resumed <- tryCatch(
            ugplot_remote_resume_job(worker$url, manifest$JobID[[row_index]], worker$token %||% ""),
            error = function(e) NULL
          )
          if (!is.null(resumed)) {
            manifest$Attempts[[row_index]] <- manifest$Attempts[[row_index]] + 1L
            manifest$State[[row_index]] <- "running"
            manifest$Message[[row_index]] <- paste0(
              "Resuming ", manifest$GroupID[[row_index]], " from its saved model/seed checkpoint"
            )
            manifest$Error[[row_index]] <- ""
            manifest$UpdatedAt[[row_index]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
            next
          }
        }
        manifest$State[[row_index]] <- "pending"
        manifest$JobID[[row_index]] <- ""
        manifest$Error[[row_index]] <- as.character(status$error %||% status$message %||% "Worker task failed.")
      } else {
        manifest$State[[row_index]] <- "running"
      }
    }
    ugplot_geo_write_distributed_manifest(manifest, manifest_path)

    busy_workers <- unique(manifest$Worker[manifest$State %in% c("submitted", "running")])
    available_workers <- Filter(function(worker) !(as.character(worker$name) %in% busy_workers), workers)
    if (isTRUE(collaboration_enabled)) {
      for (task_id in collaboration_index_ids(c("pending", "leased"))) {
        group_id <- collaboration_group_id(task_id)
        row_index <- match(group_id, as.character(manifest$GroupID))
        if (is.na(row_index) || manifest$State[[row_index]] != "pending") next
        fallback_requested <- tryCatch(
          ugplot_collaboration_consume_fallback(
            task_id,
            collaboration_jobs_dir
          ),
          error = function(e) FALSE
        )
        if (isTRUE(fallback_requested)) manifest$Attempts[[row_index]] <- 0L
      }
    }
    collaboration_leased <- logical(0)
    collaboration_leased_groups <- character(0)
    if (isTRUE(collaboration_enabled)) {
      leased_task_ids <- collaboration_index_ids("leased")
      refreshed_tasks <- lapply(leased_task_ids, function(task_id) {
        tryCatch(
          ugplot_collaboration_refresh_task(task_id, collaboration_jobs_dir),
          error = function(e) NULL
        )
      })
      collaboration_leased <- vapply(refreshed_tasks, function(task) {
        is.list(task) && identical(task$state %||% "", "leased")
      }, logical(1))
      collaboration_leased_groups <- vapply(
        leased_task_ids[collaboration_leased],
        collaboration_group_id,
        character(1)
      )
    }
    if (isTRUE(draining) && ugplot_geo_drain_ready(busy_workers, collaboration_leased)) {
      report_progress()
      ugplot_signal_job_drained("Drained safely; active work collected and checkpoint ready")
    }
    pending_rows <- if (isTRUE(draining)) integer(0) else which(manifest$State == "pending" & manifest$Attempts < max_attempts)
    if (isTRUE(collaboration_enabled) && length(pending_rows) > 0L) {
      pending_rows <- pending_rows[!manifest$GroupID[pending_rows] %in% collaboration_leased_groups]
    }
    dispatch_count <- min(length(available_workers), length(pending_rows))
    if (dispatch_count > 0) {
      for (dispatch_i in seq_len(dispatch_count)) {
        worker_names <- vapply(available_workers, function(worker) as.character(worker$name), character(1))
        compatible_pending <- vapply(pending_rows, function(candidate_row) {
          ugplot_geo_retry_has_compatible_worker(
            manifest$Worker[[candidate_row]],
            worker_names,
            length(workers)
          )
        }, logical(1))
        if (!any(compatible_pending)) {
          break
        }
        pending_index <- which(compatible_pending)[[1]]
        row_index <- pending_rows[[pending_index]]
        previous_worker <- as.character(manifest$Worker[[row_index]] %||% "")
        worker_candidates <- which(worker_names != previous_worker)
        worker_index <- if (length(worker_candidates) > 0) worker_candidates[[1]] else 1L
        worker <- available_workers[[worker_index]]
        pending_rows <- pending_rows[-pending_index]
        available_workers <- available_workers[-worker_index]
        group <- group_by_id(manifest$GroupID[[row_index]])
        dataset_info <- ugplot_geo_ml_group_dataset(group)
        task_config <- config
        task_config$distributed_workers <- NULL
        task_config$resume_result <- NULL
        task_config$resume_result_path <- NULL
        task_config$resume_completed_keys <- NULL
        task_config$jobs_dir <- NULL
        task_config$job_dir <- NULL
        task_config$model_log_dir <- NULL
        task_config$runner <- "ugplot_run_geo_complete_group_job"
        task_config$type <- "geo_worker"
        task_config$job_name <- paste0("Worker ", manifest$GroupID[[row_index]], " for ", parent_job_id)
        task_config$internal_worker_task <- TRUE
        task_config$parent_job_id <- parent_job_id
        task_config$worker_name <- as.character(worker$name)
        task_config$distributed_group <- group
        task_config$matrix_source <- source
        task_config$coordinator_dataset_path <- dataset_info$dataset_path
        task_config$cpu_limit <- max(1L, suppressWarnings(as.integer(worker$cpu_limit %||% 1L)))
        task_config <- ugplot_geo_distributed_resume_config(
          task_config,
          manifest$GroupID[[row_index]],
          summaries,
          stability_summaries
        )
        task_config$request_id <- paste(parent_job_id, "analyze", manifest$GroupID[[row_index]], sep = ":")
        manifest$Worker[[row_index]] <- as.character(worker$name)
        manifest$State[[row_index]] <- "dispatching"
        manifest$Progress[[row_index]] <- 0
        manifest$Message[[row_index]] <- paste("Assigning to", as.character(worker$name))
        manifest$UpdatedAt[[row_index]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
        manifest$Attempts[[row_index]] <- manifest$Attempts[[row_index]] + 1L
        manifest$Error[[row_index]] <- ""
        ugplot_geo_write_distributed_manifest(manifest, manifest_path)
        started <- tryCatch(
          ugplot_remote_create_job(
            worker$url,
            dataset_info$dataset,
            task_config,
            worker$token %||% ""
          ),
          error = function(e) e
        )
        if (inherits(started, "error")) {
          manifest$State[[row_index]] <- "pending"
          manifest$Error[[row_index]] <- conditionMessage(started)
        } else {
          manifest$JobID[[row_index]] <- as.character(started$id %||% "")
          manifest$State[[row_index]] <- "submitted"
          if (isTRUE(collaboration_enabled)) {
            try(ugplot_collaboration_cancel_task(
              collaboration_task_id(manifest$GroupID[[row_index]]),
              reason = "assigned_to_fixed_worker",
              jobs_dir = collaboration_jobs_dir
            ), silent = TRUE)
          }
        }
        ugplot_geo_write_distributed_manifest(manifest, manifest_path)
      }
    }

    exhausted <- which(manifest$State != "completed" & manifest$Attempts >= max_attempts &
                         !manifest$State %in% c("submitted", "running"))
    if (isTRUE(collaboration_enabled) && length(exhausted) > 0L) {
      exhausted <- exhausted[!manifest$GroupID[exhausted] %in% collaboration_leased_groups]
    }
    if (length(exhausted) > 0) {
      failed <- paste0(manifest$GroupID[exhausted], ": ", manifest$Error[exhausted], collapse = "; ")
      stop("Distributed transcript analysis failed after retries: ", failed, call. = FALSE)
    }
    ugplot_geo_write_distributed_manifest(manifest, manifest_path)
    report_progress()
    Sys.sleep(poll_seconds)
  }
  report_progress()
  summaries
}

ugplot_geo_run_transcript_ml_remote <- function(groups, cache_dir, source = "processed", config = list(),
                                                metadata = NULL,
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
  stability_column <- as.character(config$geo_ml_stability_group_column %||% "")
  config$geo_stability_metadata <- if (
    nzchar(stability_column) && is.data.frame(metadata) &&
      all(c("sample_id", stability_column) %in% names(metadata))
  ) {
    metadata[, unique(c("sample_id", stability_column)), drop = FALSE]
  } else NULL
  run_key <- as.character(config$geo_transcript_ml_run_key %||% "")
  pipeline_dir <- ugplot_geo_transcript_ml_dir(cache_dir, source, run_key)
  summary_path <- file.path(pipeline_dir, "screening_summary.csv")
  summaries <- if (file.exists(summary_path)) {
    tryCatch(utils::read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  } else {
    data.frame()
  }
  workers <- ugplot_geo_distributed_workers(config)
  if (length(workers) > 0) {
    summaries <- ugplot_geo_run_transcript_ml_distributed(
      eligible = eligible,
      summaries = summaries,
      summary_path = summary_path,
      pipeline_dir = pipeline_dir,
      cache_dir = cache_dir,
      source = source,
      run_key = run_key,
      config = config,
      workers = workers,
      progress_callback = progress_callback
    )
    summaries <- ugplot_geo_enrich_ml_summary_remote(summaries, source = source, phase = "screening")
    summaries <- ugplot_geo_ml_rank_summary(summaries)
    if (is.data.frame(summaries) && nrow(summaries) > 0) {
      utils::write.csv(summaries, summary_path, row.names = FALSE)
    }
    return(summaries)
  }
  local_stability_path <- if (nzchar(stability_column)) {
    file.path(pipeline_dir, paste0("summary_by_", ugplot_geo_safe_token(stability_column), ".csv"))
  } else file.path(pipeline_dir, "summary.csv")
  local_stability <- if (file.exists(local_stability_path)) {
    tryCatch(utils::read.csv(local_stability_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  } else data.frame()
  processed_groups <- ugplot_geo_stability_complete_groups(
    summaries, local_stability, config, config$geo_stability_metadata
  )
  for (group_i in seq_len(nrow(eligible))) {
    group <- eligible[group_i, , drop = FALSE]
    group_id <- as.character(group$GroupID[[1]])
    if (group_id %in% processed_groups) {
      next
    }
    dataset_info <- ugplot_geo_ml_group_dataset(group)
    group_dir <- ugplot_geo_transcript_ml_group_dir(cache_dir, source, group_id, run_key)
    screen_path <- file.path(group_dir, "screen_result.rds")
    importance_path <- file.path(group_dir, "screen_importance.csv")
    if (!is.null(progress_callback)) {
      progress_callback((group_i - 1) / nrow(eligible), paste0("Screening ", group_id, " with ", length(models), " model(s)"))
    }
    group_config <- config
    group_config$models <- models
    group_config$geo_ml_screen_seeds <- screen_seeds
    group_config$geo_ml_timeout <- timeout
    group_config$cpu_limit <- cpu_limit
    group_config$parallel_enabled <- parallel_enabled
    group_config$restart_parallel_each_model <- restart_parallel_each_model
    group_config$retry_parallel_connection_errors <- retry_parallel_connection_errors
    group_config$coordinator_dataset_path <- dataset_info$dataset_path
    group_result <- ugplot_geo_screen_group(
      dataset = dataset_info$dataset,
      group = group,
      source = source,
      config = group_config,
      screen_path = screen_path,
      importance_path = importance_path,
      progress_callback = function(...) {
        args <- list(...)
        if (!is.null(progress_callback)) {
          value <- suppressWarnings(as.numeric(args$progress %||% 0))
          if (!is.finite(value)) value <- 0
          progress_callback(((group_i - 1) + 0.55 * max(0, min(1, value))) / nrow(eligible), paste0("Screening ", group_id, ": ", args$message %||% ""))
        }
      }
    )
    summary_row <- group_result$summary
    summaries <- ugplot_geo_bind_rows(list(summaries[as.character(summaries$GroupID) != group_id, , drop = FALSE], summary_row))
    summaries <- ugplot_geo_ml_rank_summary(summaries)
    utils::write.csv(summaries, summary_path, row.names = FALSE)
    ugplot_geo_run_transcript_stability_remote(
      screen_summary = summary_row,
      cache_dir = cache_dir,
      source = source,
      config = config,
      metadata = config$geo_stability_metadata,
      progress_callback = function(value, message) {
        if (!is.null(progress_callback)) {
          value <- suppressWarnings(as.numeric(value %||% 0))
          if (!is.finite(value)) value <- 0
          progress_callback(
            ((group_i - 1) + 0.55 + 0.45 * max(0, min(1, value))) / nrow(eligible),
            paste0("Completing ", group_id, ": ", message %||% "")
          )
        }
      }
    )
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
                                                       progress_callback = NULL,
                                                       partial_callback = NULL) {
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
  resume_summaries <- config$distributed_resume_stability_summary %||% data.frame()
  if (is.data.frame(resume_summaries) && nrow(resume_summaries) > 0L) {
    summaries <- ugplot_geo_bind_rows(list(resume_summaries, summaries))
    existing_col <- if ("StratumColumn" %in% names(summaries)) {
      summaries$StratumColumn
    } else {
      rep("", nrow(summaries))
    }
    existing_value <- if ("StratumValue" %in% names(summaries)) {
      summaries$StratumValue
    } else {
      rep("", nrow(summaries))
    }
    resume_keys <- ugplot_geo_ml_stability_task_key(
      summaries$GroupID,
      existing_col,
      existing_value
    )
    summaries <- summaries[!duplicated(resume_keys, fromLast = TRUE), , drop = FALSE]
    utils::write.csv(summaries, summary_path, row.names = FALSE)
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
          partial_callback = function(partial) {
            ugplot_geo_write_checkpoint(partial, stability_path)
            if (is.function(partial_callback)) partial_callback(partial)
          }
        )
        ugplot_geo_write_checkpoint(stability_result, stability_path)
        metric_values <- ugplot_geo_ml_metric_values(stability_result)
        if (length(metric_values) <= existing_n) {
          results_table <- stability_result$results_table %||% data.frame()
          statuses <- if (is.data.frame(results_table) && "Status" %in% names(results_table)) {
            status_counts <- table(as.character(results_table$Status), useNA = "ifany")
            paste(paste0(names(status_counts), "=", as.integer(status_counts)), collapse = ", ")
          } else {
            "no run status available"
          }
          errors <- if (is.data.frame(results_table) && "Error" %in% names(results_table)) {
            unique(trimws(as.character(results_table$Error)))
          } else {
            character(0)
          }
          errors <- errors[nzchar(errors) & !is.na(errors)]
          stop(
            paste0(
              "Stability ", group_id, " / ", stratum_label, " with ", best_model,
              " produced no ", if (existing_n > 0L) "additional " else "",
              "valid metrics in the seed batch ending at ", current_end,
              " (", statuses, ")",
              if (length(errors) > 0L) paste0(". First error: ", errors[[1]]) else ""
            ),
            call. = FALSE
          )
        }
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
  if (!is.null(progress_callback)) progress_callback(1, "Group stability complete")
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
      geo_ml_stability_group_column = config$geo_ml_stability_group_column %||% "",
      distributed_screening_workers = vapply(
        ugplot_geo_distributed_workers(config),
        function(worker) as.character(worker$name),
        character(1)
      )
    )
  )
  last_publish_progress <- -Inf
  last_publish_time <- Sys.time() - 60
  last_partial_time <- Sys.time() - 60
  publish <- function(progress, message, force = FALSE, distributed_state = NULL,
                      stage_progress = NULL) {
    result$stage <<- message
    result$updated_at <<- as.character(Sys.time())
    progress_value <- suppressWarnings(as.numeric(progress))
    if (is.list(stage_progress)) {
      result$stage_progress <<- stage_progress
    } else if (is.finite(progress_value) && progress_value > 0.92) {
      result$stage_progress <<- list()
    }
    now <- Sys.time()
    progress_delta <- progress_value - last_publish_progress
    publish_elapsed <- as.numeric(difftime(now, last_publish_time, units = "secs"))
    should_publish <- isTRUE(force) ||
      isTRUE(progress_delta >= 0.01) ||
      isTRUE(publish_elapsed >= 10) ||
      isTRUE(is.finite(progress_value) && progress_value >= 1)
    if (should_publish) {
      callback_args <- list(progress = progress, message = message)
      if (is.list(distributed_state)) {
        callback_args$distributed_state <- distributed_state
      }
      callback_args$stage_progress <- result$stage_progress %||% list()
      do.call(progress_callback, callback_args)
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
      transcript_min_samples = min_transcript_samples,
      transcript_annotation = ugplot_geo_annotation_cache_version()
    ))
    transcript_ml_run_key <- ugplot_geo_transcript_ml_run_key(target_column, threshold, min_transcript_samples)
    config$geo_transcript_ml_run_key <- transcript_ml_run_key
    transcript_ml_dir <- ugplot_geo_transcript_ml_dir(cache_dir, source, transcript_ml_run_key)
    result$settings <- c(result$settings %||% list(), list(
      transcript_ml_run_key = transcript_ml_run_key,
      resume_cached_geo = isTRUE(config$resume_cached_geo) || isTRUE(config$use_cached_geo)
    ))
    result$paths$transcript_ml_dir <- transcript_ml_dir
    last_model_timing_refresh <- Sys.time() - 60
    refresh_model_timing <- function(force = FALSE) {
      now <- Sys.time()
      elapsed <- as.numeric(difftime(now, last_model_timing_refresh, units = "secs"))
      if (!isTRUE(force) && is.finite(elapsed) && elapsed < 30) {
        return(invisible(FALSE))
      }
      timing <- ugplot_geo_collect_model_timing(transcript_ml_dir)
      if (is.data.frame(timing) && nrow(timing) > 0L) {
        result$tables$transcript_ml_model_timing <<- timing
      }
      last_model_timing_refresh <<- now
      invisible(TRUE)
    }
    publish(0.86, paste0("Building transcript ML datasets for |rho| >= ", threshold), force = TRUE)
    candidates_path <- file.path(
      ugplot_geo_analysis_dir(cache_dir, source),
      paste0("ugplot_geo_transcript_candidates_", ugplot_geo_safe_token(target_column), "_", ugplot_geo_annotation_cache_version(), "_absrho_", ugplot_geo_safe_token(threshold), ".csv")
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
      group_cache_complete <- isTRUE(resume_mode) &&
        ugplot_geo_transcript_group_cache_complete(candidates, group_paths)
      group_result <- if (isTRUE(group_cache_complete)) {
        publish(0.86, "Using cached transcript ML groups for resume", force = TRUE)
        list(
          summary = read_cached_csv(group_paths$summary),
          details = read_cached_csv(group_paths$details),
          paths = group_paths
        )
      } else {
        if (isTRUE(resume_mode) && file.exists(group_paths$progress)) {
          publish(0.86, "Completing transcript candidates missing from the cached group manifest", force = TRUE)
        }
        ugplot_geo_build_transcript_groups_remote(
          candidates = candidates,
          matrix_files = matrix_files,
          metadata = metadata,
          cache_dir = cache_dir,
          target_column = target_column,
          threshold = threshold,
          min_samples_pct = min_transcript_samples,
          source = source,
          cpu_limit = config$cpu_limit %||% 1L,
          parallel_enabled = isTRUE(config$parallel_enabled),
          progress_callback = function(value, message, stage_progress = NULL) {
            publish(
              0.86 + 0.06 * value,
              message,
              force = identical(as.character(stage_progress$phase %||% ""), "preparing_matrix"),
              stage_progress = stage_progress
            )
          }
        )
      }
      result$paths$transcript_group_summary <- group_result$paths$summary
      result$paths$transcript_group_details <- group_result$paths$details
      result$tables$transcript_groups <- group_result$summary
      result$tables$transcript_group_details <- group_result$details
      result$tables$transcript_group_datasets <- ugplot_geo_collect_group_datasets_remote(group_result$summary)

      if (is.data.frame(group_result$summary) && nrow(group_result$summary) > 0) {
        publish(0.93, "Running complete transcript analyses group by group", force = TRUE)
        screen_summary <- ugplot_geo_run_transcript_ml_remote(
          groups = group_result$summary,
          cache_dir = cache_dir,
          source = source,
          config = config,
          metadata = metadata,
          progress_callback = function(value, message, distributed_state = NULL) {
            refresh_model_timing()
            publish(
              0.93 + 0.04 * value,
              message,
              distributed_state = distributed_state
            )
          }
        )
        result$paths$transcript_ml_screening_summary <- file.path(transcript_ml_dir, "screening_summary.csv")
        result$tables$transcript_ml_screening <- screen_summary
        refresh_model_timing(force = TRUE)
        result$tables$transcript_ml_importance <- ugplot_geo_collect_ml_importance_remote(screen_summary)
        result$tables$transcript_ml_final <- ugplot_geo_paper_summary_remote(
          screen_summary,
          details = result$tables$transcript_group_details
        )

        if (is.data.frame(screen_summary) && nrow(screen_summary) > 0) {
          publish(0.97, "Finalizing any remaining transcript stability checkpoints", force = TRUE)
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
          refresh_model_timing(force = TRUE)
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
