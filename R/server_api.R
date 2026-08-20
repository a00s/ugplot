ugplot_check_token <- function(req, token) {
  if (!nzchar(token)) {
    return(TRUE)
  }
  header_token <- req$HTTP_AUTHORIZATION %||% ""
  header_token <- sub("^Bearer[[:space:]]+", "", header_token, ignore.case = TRUE)
  identical(header_token, token)
}

ugplot_request_dataset <- function(req) {
  json_body <- ugplot_request_json_body(req)
  if (!is.null(json_body$dataset_rds_base64)) {
    dataset <- ugplot_read_rds_base64(json_body$dataset_rds_base64)
    if (!is.data.frame(dataset)) {
      stop("Uploaded dataset must resolve to a data.frame.", call. = FALSE)
    }
    return(dataset)
  }

  upload <- req$files$dataset %||% NULL
  if (!is.null(upload) && !is.null(upload$datapath)) {
    ext <- tolower(tools::file_ext(upload$name %||% upload$datapath))
    if (identical(ext, "rds")) {
      dataset <- readRDS(upload$datapath)
    } else {
      dataset <- utils::read.csv(upload$datapath, stringsAsFactors = FALSE, check.names = FALSE)
    }
    if (!is.data.frame(dataset)) {
      stop("Uploaded dataset must resolve to a data.frame.", call. = FALSE)
    }
    return(dataset)
  }
  stop("Upload a dataset file using multipart field 'dataset'.", call. = FALSE)
}

ugplot_request_config <- function(req) {
  json_body <- ugplot_request_json_body(req)
  if (!is.null(json_body$config_rds_base64)) {
    config <- ugplot_read_rds_base64(json_body$config_rds_base64)
    if (!is.list(config)) {
      stop("Uploaded config must resolve to a list.", call. = FALSE)
    }
    return(config)
  }

  config_upload <- req$files$config %||% NULL
  if (!is.null(config_upload) && !is.null(config_upload$datapath)) {
    ext <- tolower(tools::file_ext(config_upload$name %||% config_upload$datapath))
    if (identical(ext, "rds")) {
      config <- readRDS(config_upload$datapath)
    } else if (requireNamespace("jsonlite", quietly = TRUE)) {
      config <- jsonlite::fromJSON(config_upload$datapath, simplifyVector = FALSE)
    } else {
      stop("Package 'jsonlite' is required to read JSON config uploads.", call. = FALSE)
    }
    if (!is.list(config)) {
      stop("Uploaded config must resolve to a list.", call. = FALSE)
    }
    return(config)
  }
  list()
}

ugplot_request_json_body <- function(req, max_bytes = Inf) {
  cached_body <- req$ugplot_json_body %||% NULL
  if (!is.null(cached_body)) {
    return(cached_body)
  }
  body <- req$postBody %||% ""
  body_bytes <- nchar(body, type = "bytes")
  if (length(body_bytes) != 1L || is.na(body_bytes) || body_bytes > max_bytes) {
    stop("Request body is too large.", call. = FALSE)
  }
  if (!nzchar(body)) {
    req$ugplot_json_body <- list()
    return(req$ugplot_json_body)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to read JSON job submissions.", call. = FALSE)
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) list()
  )
  if (!is.list(parsed)) {
    parsed <- list()
  }
  req$ugplot_json_body <- parsed
  parsed
}

ugplot_read_rds_base64 <- function(value) {
  raw_value <- base64enc::base64decode(value)
  tmp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_file), add = TRUE)
  writeBin(raw_value, tmp_file)
  readRDS(tmp_file)
}

ugplot_validate_remote_job_config <- function(config) {
  runner <- as.character(config$runner %||% "ugplot_run_placeholder_job")
  if (length(runner) != 1L || !nzchar(runner)) {
    stop("Remote job runner must be a single function name.", call. = FALSE)
  }
  public_runners <- c(
    "ugplot_run_placeholder_job",
    "ugplot_run_ml_job",
    "ugplot_run_geo_pipeline_job"
  )
  internal_runners <- c("ugplot_run_geo_complete_group_job", "ugplot_run_geo_screen_group_job")
  allowed <- runner %in% public_runners ||
    (runner %in% internal_runners && isTRUE(config$internal_worker_task))
  if (!isTRUE(allowed)) {
    stop("Remote job runner is not allowed: ", runner, call. = FALSE)
  }
  if (identical(runner, "ugplot_run_geo_pipeline_job")) {
    accession <- toupper(trimws(as.character(config$accession %||% "")))
    if (length(accession) != 1L || !grepl("^GSE[0-9]+$", accession)) {
      stop("Remote GEO jobs require an accession such as GSE87571.", call. = FALSE)
    }
    config$accession <- accession
  }
  server_owned_fields <- c(
    "jobs_dir",
    "job_dir",
    "model_log_dir",
    "resume_result",
    "resume_result_path",
    "resume_completed_keys"
  )
  for (field in server_owned_fields) {
    config[[field]] <- NULL
  }
  config$runner <- runner
  config
}

ugplot_job_discovery_report_paths <- function(job_id,
                                              jobs_dir = ugplot_default_jobs_dir()) {
  job_id <- ugplot_validate_job_id(job_id)
  config_path <- file.path(ugplot_job_dir(job_id, jobs_dir), "config.rds")
  if (!file.exists(config_path)) {
    stop("Job config is not available: ", job_id, call. = FALSE)
  }
  config <- readRDS(config_path)
  is_geo <- identical(as.character(config$type %||% ""), "geo") ||
    identical(as.character(config$runner %||% ""), "ugplot_run_geo_pipeline_job")
  if (!isTRUE(is_geo)) {
    stop("Incremental discovery reports are available for GEO jobs only.", call. = FALSE)
  }
  accession <- trimws(as.character(config$accession %||% ""))
  if (!grepl("^GSE[0-9]+$", accession, ignore.case = TRUE)) {
    stop("The GEO accession is not available for job: ", job_id, call. = FALSE)
  }
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
  pipeline_dir <- ugplot_geo_transcript_ml_dir(
    ugplot_geo_cache_dir(toupper(accession)), source, run_key
  )
  stratum_column <- as.character(config$geo_ml_stability_group_column %||% "")
  stability_path <- if (nzchar(stratum_column)) {
    file.path(pipeline_dir, paste0("summary_by_", ugplot_geo_safe_token(stratum_column), ".csv"))
  } else {
    file.path(pipeline_dir, "summary.csv")
  }
  group_paths <- ugplot_geo_transcript_group_paths(
    ugplot_geo_cache_dir(toupper(accession)), target, threshold, min_samples,
    source = source,
    metadata_numeric_predictors = config$geo_metadata_numeric_predictors %||% character(0),
    metadata_categorical_predictors = config$geo_metadata_categorical_predictors %||% character(0)
  )
  if (!file.exists(group_paths$summary)) {
    numeric_predictors <- config$geo_metadata_numeric_predictors %||% character(0)
    categorical_predictors <- config$geo_metadata_categorical_predictors %||% character(0)
    legacy_version <- if (length(numeric_predictors) == 0L && length(categorical_predictors) == 0L) {
      "reader_v5_members"
    } else {
      paste0(
        "reader_v6_metadata_",
        ugplot_geo_metadata_predictor_key(numeric_predictors, categorical_predictors)
      )
    }
    legacy_paths <- ugplot_geo_transcript_group_paths(
      ugplot_geo_cache_dir(toupper(accession)), target, threshold, min_samples,
      source = source,
      metadata_numeric_predictors = numeric_predictors,
      metadata_categorical_predictors = categorical_predictors,
      cache_version = legacy_version
    )
    if (file.exists(legacy_paths$summary)) group_paths <- legacy_paths
  }
  list(
    config = config,
    pipeline_dir = pipeline_dir,
    screening = file.path(pipeline_dir, "screening_summary.csv"),
    stability = stability_path,
    groups = group_paths$summary,
    manifest = ugplot_geo_distributed_manifest_path(pipeline_dir)
  )
}

ugplot_read_discovery_csv <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path) || !file.exists(path)) {
    return(data.frame())
  }
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )
}

ugplot_job_geo_group_datasets <- function(job_id,
                                          jobs_dir = ugplot_default_jobs_dir()) {
  paths <- ugplot_job_discovery_report_paths(job_id, jobs_dir)
  groups <- ugplot_read_discovery_csv(paths$groups)
  if (!is.data.frame(groups) || nrow(groups) == 0L ||
      !all(c("GroupID", "DatasetPath") %in% names(groups))) {
    return(data.frame())
  }
  dataset_paths <- as.character(groups$DatasetPath)
  available <- !is.na(dataset_paths) & nzchar(dataset_paths) & file.exists(dataset_paths)
  groups <- groups[available, , drop = FALSE]
  if (nrow(groups) == 0L) return(data.frame())
  field <- function(name, default = "") {
    if (name %in% names(groups)) groups[[name]] else rep(default, nrow(groups))
  }
  catalog <- data.frame(
    group_id = as.character(field("GroupID")),
    transcript = as.character(field("PrincipalTranscript")),
    gene = as.character(field("Gene")),
    transcripts = as.character(field("TranscriptMembers")),
    cpgs = suppressWarnings(as.integer(field("Columns", NA_integer_))),
    samples = suppressWarnings(as.integer(field("Samples", NA_integer_))),
    stringsAsFactors = FALSE
  )
  numeric_group <- suppressWarnings(as.integer(sub("^TG", "", catalog$group_id, ignore.case = TRUE)))
  catalog[order(is.na(numeric_group), numeric_group, catalog$group_id), , drop = FALSE]
}

ugplot_read_job_geo_group_dataset <- function(job_id, group_id,
                                              jobs_dir = ugplot_default_jobs_dir()) {
  group_id <- trimws(as.character(group_id %||% ""))
  if (length(group_id) != 1L || !grepl("^TG[0-9]+$", group_id, ignore.case = TRUE)) {
    stop("Invalid transcript group id.", call. = FALSE)
  }
  paths <- ugplot_job_discovery_report_paths(job_id, jobs_dir)
  groups <- ugplot_read_discovery_csv(paths$groups)
  if (!is.data.frame(groups) || nrow(groups) == 0L ||
      !all(c("GroupID", "DatasetPath") %in% names(groups))) {
    stop("Transcript group datasets are not available for this job yet.", call. = FALSE)
  }
  matched <- which(toupper(as.character(groups$GroupID)) == toupper(group_id))
  if (length(matched) == 0L) {
    stop("Transcript group dataset is not available: ", group_id, call. = FALSE)
  }
  group <- groups[matched[[1]], , drop = FALSE]
  dataset_path <- as.character(group$DatasetPath[[1]] %||% "")
  if (is.na(dataset_path) || !nzchar(dataset_path) || !file.exists(dataset_path)) {
    stop("Transcript group dataset file is not available: ", group_id, call. = FALSE)
  }
  dataset <- if (grepl("\\.rds$", dataset_path, ignore.case = TRUE)) {
    readRDS(dataset_path)
  } else {
    utils::read.csv(dataset_path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (!is.data.frame(dataset) || nrow(dataset) == 0L) {
    stop("Transcript group dataset is empty or invalid: ", group_id, call. = FALSE)
  }
  list(
    job_id = as.character(job_id),
    group_id = as.character(group$GroupID[[1]]),
    accession = as.character(paths$config$accession %||% "GEO"),
    target = as.character(paths$config$target_column %||% ""),
    transcript = as.character(group$PrincipalTranscript[[1]] %||% ""),
    gene = as.character(group$Gene[[1]] %||% ""),
    dataset = dataset
  )
}

.ugplot_discovery_metric_range_cache <- new.env(parent = emptyenv())
.ugplot_discovery_resolver_cache <- new.env(parent = emptyenv())

ugplot_discovery_result_metric_range <- function(path) {
  empty <- c(min = NA_real_, max = NA_real_)
  path <- as.character(path %||% "")
  if (length(path) != 1L || !nzchar(path) || !file.exists(path)) return(empty)
  info <- file.info(path)
  signature <- paste(info$size[[1]], as.numeric(info$mtime[[1]]), sep = ":")
  cache_key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  cached <- .ugplot_discovery_metric_range_cache[[cache_key]]
  if (is.list(cached) && identical(cached$signature, signature)) return(cached$value)
  result <- tryCatch(readRDS(path), error = function(e) NULL)
  values <- tryCatch(ugplot_geo_ml_metric_values(result), error = function(e) numeric(0))
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  value <- if (length(values) > 0L) {
    c(min = min(values), max = max(values))
  } else {
    summary <- result$final_summary %||% list()
    c(
      min = suppressWarnings(as.numeric(summary$best_model_min %||% NA_real_)),
      max = suppressWarnings(as.numeric(summary$best_model_max %||% NA_real_))
    )
  }
  value[!is.finite(value)] <- NA_real_
  .ugplot_discovery_metric_range_cache[[cache_key]] <- list(signature = signature, value = value)
  value
}

ugplot_discovery_resolver_map <- function(job_id, jobs_dir, manifest = data.frame()) {
  resolvers <- character(0)
  if (is.data.frame(manifest) && nrow(manifest) > 0L && "GroupID" %in% names(manifest)) {
    completed <- if ("State" %in% names(manifest)) {
      as.character(manifest$State) == "completed"
    } else rep(FALSE, nrow(manifest))
    resolved <- if ("ResolvedBy" %in% names(manifest)) as.character(manifest$ResolvedBy) else rep("", nrow(manifest))
    worker <- if ("Worker" %in% names(manifest)) as.character(manifest$Worker) else rep("", nrow(manifest))
    resolved[!nzchar(resolved)] <- worker[!nzchar(resolved)]
    keep <- completed & nzchar(resolved)
    resolvers[as.character(manifest$GroupID[keep])] <- resolved[keep]
  }
  index_path <- ugplot_collaboration_index_path(jobs_dir)
  if (!file.exists(index_path)) return(resolvers)
  info <- file.info(index_path)
  signature <- paste(info$size[[1]], as.numeric(info$mtime[[1]]), sep = ":")
  cache_key <- paste(normalizePath(index_path, winslash = "/", mustWork = FALSE), job_id, sep = "\r")
  cached <- .ugplot_discovery_resolver_cache[[cache_key]]
  if (!is.list(cached) || !identical(cached$signature, signature)) {
    index <- tryCatch(ugplot_collaboration_read_index(jobs_dir), error = function(e) data.frame())
    task_ids <- if (is.data.frame(index) && nrow(index) > 0L) {
      as.character(index$task_id[
        as.character(index$parent_job_id) == as.character(job_id) &
          as.character(index$state) == "completed"
      ])
    } else character(0)
    collaboration <- character(0)
    for (task_id in task_ids) {
      task <- ugplot_collaboration_read_task(task_id, jobs_dir)
      if (!is.list(task)) next
      resolver <- trimws(as.character(task$scientist_name %||% task$client_id %||% ""))
      group_id <- ugplot_geo_collaboration_group_from_task_id(task_id, job_id)
      if (nzchar(group_id) && nzchar(resolver)) collaboration[[group_id]] <- resolver
    }
    cached <- list(signature = signature, value = collaboration)
    .ugplot_discovery_resolver_cache[[cache_key]] <- cached
  }
  resolvers[names(cached$value)] <- cached$value
  resolvers
}

ugplot_public_report_text <- function(value, max_chars = 240L) {
  value <- paste(as.character(value %||% ""), collapse = " ")
  value <- gsub("[[:cntrl:]]", " ", value)
  value <- trimws(gsub("[[:space:]]+", " ", value))
  max_chars <- suppressWarnings(as.integer(max_chars))
  if (is.na(max_chars) || max_chars < 1L) max_chars <- 240L
  if (nchar(value, type = "chars") > max_chars) {
    value <- paste0(substr(value, 1L, max_chars - 1L), "…")
  }
  value
}

ugplot_reconcile_discovery_group_ids <- function(rows, groups) {
  if (!is.data.frame(rows) || nrow(rows) == 0L || !"GroupID" %in% names(rows) ||
      !is.data.frame(groups) || nrow(groups) == 0L || !"GroupID" %in% names(groups)) {
    return(rows)
  }
  text_column <- function(data, name) {
    value <- if (name %in% names(data)) as.character(data[[name]]) else rep("", nrow(data))
    value[is.na(value)] <- ""
    value
  }
  tokens <- function(value) {
    value <- as.character(value %||% "")
    value <- value[!is.na(value) & nzchar(value)]
    if (length(value) == 0L) return(character(0))
    value <- trimws(unlist(strsplit(paste(value, collapse = ";"), "[;,]"), use.names = FALSE))
    unique(toupper(value[nzchar(value)]))
  }
  cpg_key <- function(value) paste(sort(tokens(value)), collapse = "\r")
  same_number <- function(left, right) {
    left <- suppressWarnings(as.numeric(left))
    right <- suppressWarnings(as.numeric(right))
    is.finite(left) & is.finite(right) & left == right
  }

  group_keys <- text_column(groups, "GroupKey")
  group_cpg_keys <- vapply(text_column(groups, "CpGs"), cpg_key, character(1))
  group_best_cpgs <- text_column(groups, "TriggerBestCpG")
  group_transcripts <- Map(c, text_column(groups, "PrincipalTranscript"), text_column(groups, "TranscriptMembers"))
  group_genes <- Map(c, text_column(groups, "Gene"), text_column(groups, "GeneMembers"))
  group_paths <- text_column(groups, "DatasetPath")
  group_columns <- text_column(groups, "Columns")
  group_samples <- text_column(groups, "Samples")

  rows$OriginalGroupID <- text_column(rows, "GroupID")
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    row_key <- text_column(row, "GroupKey")[[1]]
    exact_key <- if (!is.na(row_key) && nzchar(row_key)) which(group_keys == row_key) else integer(0)
    if (length(exact_key) == 1L) {
      rows$GroupID[[i]] <- as.character(groups$GroupID[[exact_key]])
      next
    }

    score <- numeric(nrow(groups))
    row_path <- text_column(row, "DatasetPath")[[1]]
    if (!is.na(row_path) && nzchar(row_path)) score[group_paths == row_path] <- score[group_paths == row_path] + 80
    row_cpg_key <- cpg_key(text_column(row, "CpGs")[[1]])
    if (nzchar(row_cpg_key)) score[group_cpg_keys == row_cpg_key] <- score[group_cpg_keys == row_cpg_key] + 100
    row_best_cpg <- toupper(text_column(row, "TriggerBestCpG")[[1]])
    if (nzchar(row_best_cpg)) {
      contains_best <- vapply(text_column(groups, "CpGs"), function(value) row_best_cpg %in% tokens(value), logical(1))
      score[contains_best] <- score[contains_best] + 30
      score[toupper(group_best_cpgs) == row_best_cpg] <- score[toupper(group_best_cpgs) == row_best_cpg] + 5
    }
    row_transcripts <- tokens(c(text_column(row, "PrincipalTranscript"), text_column(row, "TranscriptMembers"), text_column(row, "Transcript")))
    if (length(row_transcripts) > 0L) {
      matches <- vapply(group_transcripts, function(value) length(intersect(row_transcripts, tokens(value))) > 0L, logical(1))
      score[matches] <- score[matches] + 40
    }
    row_genes <- tokens(c(text_column(row, "Gene"), text_column(row, "GeneMembers")))
    if (length(row_genes) > 0L) {
      matches <- vapply(group_genes, function(value) length(intersect(row_genes, tokens(value))) > 0L, logical(1))
      score[matches] <- score[matches] + 15
    }
    score[same_number(group_columns, text_column(row, "Columns")[[1]])] <-
      score[same_number(group_columns, text_column(row, "Columns")[[1]])] + 8
    score[same_number(group_samples, text_column(row, "Samples")[[1]])] <-
      score[same_number(group_samples, text_column(row, "Samples")[[1]])] + 8

    best <- which(score == max(score))
    if (length(best) == 1L && score[[best]] >= 30) {
      rows$GroupID[[i]] <- as.character(groups$GroupID[[best]])
    }
  }
  rows
}

ugplot_job_discovery_report <- function(job_id,
                                        jobs_dir = ugplot_default_jobs_dir()) {
  paths <- ugplot_job_discovery_report_paths(job_id, jobs_dir)
  status <- ugplot_read_job_status(job_id, jobs_dir)
  groups <- ugplot_read_discovery_csv(paths$groups)
  screening <- ugplot_read_discovery_csv(paths$screening)
  stability <- ugplot_read_discovery_csv(paths$stability)
  stability <- ugplot_geo_current_stability_rows(stability)
  screening <- ugplot_reconcile_discovery_group_ids(screening, groups)
  stability <- ugplot_reconcile_discovery_group_ids(stability, groups)
  manifest <- if (file.exists(paths$manifest)) {
    tryCatch(readRDS(paths$manifest), error = function(e) data.frame())
  } else data.frame()
  resolver_map <- ugplot_discovery_resolver_map(job_id, jobs_dir, manifest)

  text_value <- function(row, columns, default = "") {
    hit <- intersect(columns, names(row))
    if (length(hit) == 0L) return(default)
    for (column in hit) {
      value <- as.character(row[[column]][[1]] %||% default)
      if (length(value) > 0L && !is.na(value) && nzchar(value)) return(value)
    }
    default
  }
  number_value <- function(row, columns) {
    hit <- intersect(columns, names(row))
    if (length(hit) == 0L) return(NA_real_)
    for (column in hit) {
      value <- suppressWarnings(as.numeric(row[[column]][[1]]))
      if (length(value) > 0L && is.finite(value)) return(value)
    }
    NA_real_
  }
  logical_value <- function(row, columns, default = FALSE) {
    hit <- intersect(columns, names(row))
    if (length(hit) == 0L) return(default)
    value <- tolower(as.character(row[[hit[[1]]]][[1]] %||% ""))
    value %in% c("true", "1", "yes")
  }
  row_key <- function(row) {
    paste(
      text_value(row, "GroupID"),
      text_value(row, "StratumColumn"),
      text_value(row, "StratumValue"),
      sep = "\r"
    )
  }
  stable_keys <- if (is.data.frame(stability) && nrow(stability) > 0L) {
    vapply(seq_len(nrow(stability)), function(i) row_key(stability[i, , drop = FALSE]), character(1))
  } else {
    character(0)
  }
  if (length(stable_keys) > 0L && anyDuplicated(stable_keys)) {
    keep <- !duplicated(stable_keys, fromLast = TRUE)
    stability <- stability[keep, , drop = FALSE]
    stable_keys <- stable_keys[keep]
  }
  screened_keys <- if (is.data.frame(screening) && nrow(screening) > 0L) {
    vapply(seq_len(nrow(screening)), function(i) row_key(screening[i, , drop = FALSE]), character(1))
  } else {
    character(0)
  }
  report_rows <- list()
  if (is.data.frame(groups) && nrow(groups) > 0L) {
    for (i in seq_len(nrow(groups))) {
      row <- groups[i, , drop = FALSE]
      if (!(row_key(row) %in% c(screened_keys, stable_keys))) {
        row$Phase <- "awaiting_analysis"
        report_rows[[length(report_rows) + 1L]] <- row
      }
    }
  }
  if (is.data.frame(screening) && nrow(screening) > 0L) {
    for (i in seq_len(nrow(screening))) {
      row <- screening[i, , drop = FALSE]
      if (!(row_key(row) %in% stable_keys)) report_rows[[length(report_rows) + 1L]] <- row
    }
  }
  if (is.data.frame(stability) && nrow(stability) > 0L) {
    report_rows <- c(report_rows, lapply(seq_len(nrow(stability)), function(i) stability[i, , drop = FALSE]))
  }

  rows <- lapply(report_rows, function(row) {
    phase <- text_value(row, "Phase", "screening")
    stable <- identical(phase, "stability") && logical_value(row, "Stable", TRUE)
    model_r2 <- number_value(row, c("MedianR2", "MedianMetric", "MeanMetric", "BestMetric"))
    cpg_rho <- abs(number_value(row, c("TriggerBestRho", "TriggerMaxAbsRho")))
    discovery_type <- if (is.finite(model_r2) && is.finite(cpg_rho)) {
      if (abs(model_r2 - cpg_rho) < 0.01) "CpG & ML" else if (model_r2 > cpg_rho) "ML centered" else "CpG centered"
    } else if (is.finite(model_r2)) {
      "ML centered"
    } else {
      "CpG centered"
    }
    min_r2 <- number_value(row, c("MinR2", "MinMetric"))
    max_r2 <- number_value(row, c("MaxR2", "MaxMetric"))
    if (!is.finite(min_r2) || !is.finite(max_r2)) {
      result_path <- text_value(row, c("StabilityResultPath", "ScreenResultPath", "ResultPath"))
      metric_range <- ugplot_discovery_result_metric_range(result_path)
      if (!is.finite(min_r2)) min_r2 <- metric_range[["min"]]
      if (!is.finite(max_r2)) max_r2 <- metric_range[["max"]]
    }
    if (!is.finite(max_r2)) max_r2 <- number_value(row, "BestMetric")
    resolver_group <- text_value(row, c("OriginalGroupID", "GroupID"))
    resolver <- unname(resolver_map[resolver_group])
    if (length(resolver) == 0L || is.na(resolver[[1]])) resolver <- ""
    data.frame(
      status = if (identical(phase, "awaiting_analysis")) {
        "awaiting analysis"
      } else if (identical(phase, "stability")) {
        if (stable) "stabilized" else "stability complete"
      } else {
        "preliminary"
      },
      type = discovery_type,
      group = text_value(row, "GroupID"),
      gene = text_value(row, c("GeneMembers", "Gene")),
      transcript = text_value(row, c("TranscriptMembers", "PrincipalTranscript", "Transcript")),
      transcript_count = number_value(row, "TranscriptCount"),
      best_cpg = text_value(row, c("TriggerBestCpG", "BestCpG")),
      cpg_rho = cpg_rho,
      cpgs = number_value(row, c("Columns", "CpGCount", "NCpGs")),
      samples = number_value(row, c("StratumSamples", "Samples", "N")),
      model = text_value(row, "BestModel"),
      median_r2 = model_r2,
      min_r2 = min_r2,
      max_r2 = max_r2,
      metric_se = number_value(row, "MetricSE"),
      seeds = number_value(row, "SeedsRun"),
      stratum = paste0(text_value(row, "StratumColumn"), if (nzchar(text_value(row, "StratumValue"))) "=" else "", text_value(row, "StratumValue")),
      resolved_by = as.character(resolver[[1]]),
      stringsAsFactors = FALSE
    )
  })
  discoveries <- if (length(rows) > 0L) do.call(rbind, rows) else data.frame()
  if (is.data.frame(discoveries) && nrow(discoveries) > 0L) {
    discoveries <- discoveries[order(
      match(discoveries$status, c("stabilized", "stability complete", "preliminary", "awaiting analysis")),
      -suppressWarnings(as.numeric(discoveries$median_r2)),
      discoveries$gene,
      na.last = TRUE
    ), , drop = FALSE]
    rownames(discoveries) <- NULL
  }

  total <- 0L
  if (is.data.frame(manifest)) total <- nrow(manifest)
  if (total < nrow(screening)) total <- nrow(screening)
  contributor_rows <- tryCatch(
    ugplot_collaboration_active_contributors(job_id, jobs_dir),
    error = function(e) data.frame()
  )
  if (is.data.frame(manifest) && nrow(manifest) > 0L && "State" %in% names(manifest)) {
    active_index <- which(as.character(manifest$State) %in% c("dispatching", "submitted", "running"))
    active_group_ids <- ugplot_active_distributed_group_ids(status)
    if (length(active_index) > 0L && "GroupID" %in% names(manifest)) {
      active_index <- active_index[as.character(manifest$GroupID[active_index]) %in% active_group_ids]
    } else {
      active_index <- integer(0)
    }
    if (length(active_index) > 0L) {
      field <- function(name, default = "") {
        if (name %in% names(manifest)) as.character(manifest[[name]][active_index]) else rep(default, length(active_index))
      }
      progress_values <- if ("Progress" %in% names(manifest)) {
        suppressWarnings(as.numeric(manifest$Progress[active_index]))
      } else rep(0, length(active_index))
      progress_values[!is.finite(progress_values)] <- 0
      server_rows <- data.frame(
        group_id = field("GroupID"), executor = field("Worker", "ugPlot server"),
        executor_type = "server", progress = pmax(0, pmin(1, progress_values)),
        message = field("Message", "Processing scientific task"), candidate = "",
        stringsAsFactors = FALSE
      )
      contributor_rows <- if (is.data.frame(contributor_rows) && nrow(contributor_rows) > 0L) {
        rbind(contributor_rows, server_rows)
      } else server_rows
    }
  }
  contributors <- if (is.data.frame(contributor_rows) && nrow(contributor_rows) > 0L) {
    unname(lapply(seq_len(nrow(contributor_rows)), function(i) {
      row <- contributor_rows[i, , drop = FALSE]
      progress <- suppressWarnings(as.numeric(row$progress[[1]] %||% 0))
      if (!is.finite(progress)) progress <- 0
      list(
        scientist = ugplot_public_report_text(row$executor[[1]], 80L),
        kind = if (identical(as.character(row$executor_type[[1]]), "collaboration")) "public scientist" else "ugPlot server",
        group = ugplot_public_report_text(row$group_id[[1]], 60L),
        progress = max(0, min(1, progress)),
        activity = ugplot_public_report_text(row$message[[1]], 180L),
        candidate = ugplot_public_report_text(row$candidate[[1]], 80L)
      )
    }))
  } else list()
  list(
    protocol_version = 2L,
    job = list(
      id = as.character(job_id),
      name = as.character(status$name %||% ""),
      state = as.character(status$state %||% "unknown"),
      message = as.character(status$message %||% ""),
      accession = as.character(paths$config$accession %||% ""),
      target = as.character(paths$config$target_column %||% ""),
      updated_at = as.character(status$updated_at %||% "")
    ),
    progress = list(
      total = as.integer(total),
      screened = as.integer(nrow(screening)),
      stabilized = as.integer(nrow(stability))
    ),
    collaboration = list(
      active = as.integer(length(contributors)),
      contributors = contributors
    ),
    discoveries = if (is.data.frame(discoveries) && nrow(discoveries) > 0L) {
      unname(lapply(seq_len(nrow(discoveries)), function(i) as.list(discoveries[i, , drop = FALSE])))
    } else {
      list()
    }
  )
}

ugplot_job_discovery_snapshot_path <- function(job_id,
                                               jobs_dir = ugplot_default_jobs_dir()) {
  file.path(ugplot_job_dir(ugplot_validate_job_id(job_id), jobs_dir), "discovery-report.json")
}

ugplot_refresh_job_discovery_snapshot <- function(job_id,
                                                  jobs_dir = ugplot_default_jobs_dir()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(invisible(FALSE))
  report <- ugplot_job_discovery_report(job_id, jobs_dir)
  path <- ugplot_job_discovery_snapshot_path(job_id, jobs_dir)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".", Sys.getpid(), ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(report, tmp, auto_unbox = TRUE, na = "null", pretty = FALSE)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
  }
  invisible(file.exists(path))
}

ugplot_discovery_snapshot_live_status <- function(snapshot_json, job_id,
                                                   jobs_dir = ugplot_default_jobs_dir()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(snapshot_json)
  report <- tryCatch(
    jsonlite::fromJSON(snapshot_json, simplifyVector = FALSE),
    error = function(e) NULL
  )
  status <- tryCatch(
    ugplot_read_job_status_lightweight(job_id, jobs_dir),
    error = function(e) NULL
  )
  if (!is.list(report) || !is.list(status)) return(snapshot_json)
  report$job <- report$job %||% list()
  report$job$state <- as.character(status$state %||% "unknown")
  report$job$message <- as.character(status$message %||% "")
  report$job$updated_at <- as.character(status$updated_at %||% "")

  active_state <- as.character(status$state %||% "") %in% c("queued", "running", "draining")
  tasks <- status$distributed_state$active_tasks %||% list()
  if (is.data.frame(tasks)) {
    tasks <- lapply(seq_len(nrow(tasks)), function(i) as.list(tasks[i, , drop = FALSE]))
  } else if (is.list(tasks) && length(tasks) > 0L && any(
    c("worker", "group", "state", "progress", "message") %in% names(tasks)
  )) {
    tasks <- list(tasks)
  }
  scalar <- function(value, default = "") {
    if (is.null(value) || length(value) == 0L || is.na(value[[1]])) default else value[[1]]
  }
  server_contributors <- if (isTRUE(active_state) && is.list(tasks)) {
    Filter(Negate(is.null), lapply(tasks, function(task) {
      if (!is.list(task)) return(NULL)
      task_state <- as.character(scalar(task$state))
      if (!task_state %in% c("dispatching", "submitted", "running")) return(NULL)
      progress <- suppressWarnings(as.numeric(scalar(task$progress, 0)))
      if (!is.finite(progress)) progress <- 0
      list(
        scientist = ugplot_public_report_text(scalar(task$worker, "ugPlot server"), 80L),
        kind = "ugPlot server",
        group = ugplot_public_report_text(scalar(task$group), 60L),
        progress = max(0, min(1, progress)),
        activity = ugplot_public_report_text(scalar(task$message, "Processing scientific task"), 180L),
        candidate = ""
      )
    }))
  } else list()
  existing <- report$collaboration$contributors %||% list()
  public_contributors <- if (isTRUE(active_state) && is.list(existing)) {
    Filter(function(item) is.list(item) && identical(as.character(scalar(item$kind)), "public scientist"), existing)
  } else list()
  contributors <- c(public_contributors, server_contributors)
  report$collaboration <- list(active = length(contributors), contributors = contributors)
  jsonlite::toJSON(report, auto_unbox = TRUE, na = "null", pretty = FALSE)
}

ugplot_discovery_report_html <- function(job_id = "") {
  encoded_job <- if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::toJSON(as.character(job_id %||% ""), auto_unbox = TRUE)
  } else {
    paste0('"', gsub('"', '\\"', as.character(job_id %||% ""), fixed = TRUE), '"')
  }
  paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>ugPlot live discoveries</title><style>',
    ':root{--ink:#101a3a;--muted:#6f7b9d;--violet:#6d55ff;--cyan:#1dc7d5;--green:#22b982;--orange:#f59b32;--line:#e4e8f5}',
    '*{box-sizing:border-box}body{margin:0;font:15px/1.45 Inter,system-ui,sans-serif;color:var(--ink);background:radial-gradient(circle at 8% 12%,#e9e2ff 0,transparent 27%),radial-gradient(circle at 92% 15%,#d9fbff 0,transparent 28%),#f6f9ff}',
    '.shell{max-width:1500px;margin:auto;padding:28px}.hero,.panel{background:#fffdfdcc;border:1px solid #fff;border-radius:26px;box-shadow:0 18px 50px #28366d12}.hero{padding:28px 32px;margin-bottom:20px}.brand{display:flex;align-items:center;gap:22px}.brand img{width:min(260px,34vw);height:auto}.brand-copy{min-width:0}.eyebrow{color:var(--violet);font-weight:800;letter-spacing:.13em;font-size:12px}.hero h1{font-size:clamp(28px,4vw,48px);margin:6px 0}.hero p{color:var(--muted);margin:0}.controls{display:flex;align-items:center;gap:10px}.controls select,.controls input{height:46px;border:1px solid var(--line);border-radius:14px;background:#fff;color:var(--ink);font:inherit;outline:none;transition:border-color .2s,box-shadow .2s}.controls select{min-width:275px;padding:0 42px 0 15px;font-weight:750}.controls input{width:340px;padding:0 16px}.controls select:focus,.controls input:focus{border-color:#8875ff;box-shadow:0 0 0 4px #715cff18}.controls input::placeholder{color:#929ab1}',
    '.stats{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin:20px 0}.stat{padding:18px 20px;background:#ffffffd9;border:1px solid #fff;border-radius:18px}.stat b{display:block;font-size:28px}.stat span{color:var(--muted);font-size:12px;text-transform:uppercase;font-weight:800}.panel{padding:20px}.toolbar{display:flex;align-items:center;gap:18px;justify-content:space-between;margin-bottom:16px}.toolbar strong{font-size:17px}.live{color:var(--green);font-weight:800}.table-wrap{overflow:auto;max-height:70vh;border:1px solid var(--line);border-radius:16px}table{border-collapse:separate;border-spacing:0;width:100%;min-width:1280px}th{position:sticky;top:0;background:#f7f8fe;text-align:left;padding:13px 12px;color:#687392;font-size:11px;letter-spacing:.08em;text-transform:uppercase;z-index:1}th.sortable{cursor:pointer;user-select:none;transition:color .15s,background .15s}th.sortable:hover,th.sortable:focus{color:#4f3fd0;background:#eeecff;outline:none}th.sortable::after{content:"\u2195";display:inline-block;margin-left:7px;color:#b4bbce;font-size:10px}th.sortable[aria-sort="ascending"]::after{content:"\u2191";color:var(--violet)}th.sortable[aria-sort="descending"]::after{content:"\u2193";color:var(--violet)}td{padding:11px 12px;border-top:1px solid #edf0f8;white-space:nowrap}tr:hover td{background:#f7fbff}.badge{padding:5px 9px;border-radius:999px;font-size:11px;font-weight:800}.resolver{display:inline-block;padding:4px 8px;border-radius:999px;background:#ece8ff;color:#5945c8;font-size:11px;font-weight:800}.awaiting-analysis{background:#eef1f8;color:#69738d}.preliminary{background:#fff0dc;color:#a96000}.stabilized{background:#dcfaef;color:#087958}.stability-complete{background:#e8efff;color:#3159a5}.type-ml{background:#fff0e7;color:#d65b18}.type-cpg{background:#dff6ff;color:#0878a7}.type-both{background:#f5ddff;color:#8c189a}.empty{text-align:center;padding:60px;color:var(--muted)}.note{font-size:12px;color:var(--muted);margin-top:10px}',
    '.group-progress{margin-bottom:20px}.group-head{display:flex;align-items:flex-start;justify-content:space-between;gap:16px;margin-bottom:14px}.group-head strong{display:block;font-size:18px}.group-summary{color:var(--muted);font-size:13px;margin-top:2px}.group-legend{display:flex;gap:13px;flex-wrap:wrap;color:var(--muted);font-size:11px;font-weight:750}.group-legend span:before{content:"";display:inline-block;width:8px;height:8px;border-radius:50%;margin-right:5px}.group-legend .completed:before{background:var(--green)}.group-legend .processing:before{background:var(--violet)}.group-legend .screened:before{background:var(--orange)}.group-legend .waiting:before{background:#d9dfeb}.group-stripe{display:grid;width:100%;height:38px;gap:0;overflow:hidden;border-radius:9px;background:#edf0f6;box-shadow:inset 0 0 0 1px #3c4a6e14}.group-segment{display:block;min-width:1px;height:100%;cursor:help;transition:filter .15s,transform .15s}.group-segment:hover{filter:brightness(.88) saturate(1.2);transform:scaleY(1.08);z-index:2}.group-segment-completed{background:var(--green)}.group-segment-processing{background:linear-gradient(180deg,#8875ff,var(--cyan));animation:group-pulse 1.7s ease-in-out infinite alternate}.group-segment-screened{background:var(--orange)}.group-segment-waiting{background:#d9dfeb}.group-note{font-size:11px;color:var(--muted);margin-top:8px}@keyframes group-pulse{from{opacity:.68}to{opacity:1}}',
    '.collab{margin-bottom:20px}.collab-head{display:flex;align-items:center;justify-content:space-between;gap:15px;margin-bottom:14px}.collab-head strong{font-size:18px}.collab-head span{color:var(--green);font-weight:800}.contributors{display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:12px}.contributor{padding:15px;border:1px solid var(--line);border-radius:17px;background:linear-gradient(135deg,#fbfaff,#f3fdff)}.contributor-top{display:flex;align-items:flex-start;justify-content:space-between;gap:10px}.contributor-name{font-size:16px;font-weight:850}.contributor-kind{color:var(--violet);font-size:10px;text-transform:uppercase;font-weight:850;letter-spacing:.08em}.contributor-group{padding:4px 8px;border-radius:999px;background:#e8e2ff;color:#5640c9;font-size:11px;font-weight:800}.contributor-activity{color:var(--muted);font-size:13px;margin:8px 0}.contributor-track{height:7px;border-radius:999px;background:#e8ebf5;overflow:hidden}.contributor-fill{height:100%;border-radius:inherit;background:linear-gradient(90deg,var(--violet),var(--cyan))}.contributor-progress{display:block;text-align:right;color:var(--muted);font-size:11px;margin-top:4px}.collab-empty{color:var(--muted);padding:8px 2px}',
    '@media(max-width:900px){.shell{padding:12px}.brand{align-items:flex-start;flex-direction:column}.brand img{width:210px}.stats{grid-template-columns:1fr 1fr}.toolbar{align-items:stretch;flex-direction:column}.controls{align-items:stretch;flex-direction:column}.controls select,.controls input{width:100%;min-width:0}}</style></head><body>',
    '<main class="shell"><section class="hero"><div class="brand"><img src="/reports/assets/ugplot.png" alt="ugPlot"><div class="brand-copy"><div class="eyebrow">UGPLOT LIVE DISCOVERY REPORT</div><h1 id="title">Scientific discoveries as they emerge</h1><p id="subtitle">The report is loading the job identified by this URL.</p></div></div></section>',
    '<section class="stats"><div class="stat"><b id="total">&mdash;</b><span>Total groups</span></div><div class="stat"><b id="screened">&mdash;</b><span>Screened</span></div><div class="stat"><b id="stabilized">&mdash;</b><span>Stabilized</span></div><div class="stat"><b id="best">&mdash;</b><span>Best median R&sup2;</span></div></section>',
    '<section class="panel group-progress"><div class="group-head"><div><strong>Group completion map</strong><div class="group-summary" id="group-summary">Waiting for transcript groups.</div></div><div class="group-legend"><span class="completed">Completed</span><span class="processing">Processing</span><span class="screened">Screened</span><span class="waiting">Waiting</span></div></div><div class="group-stripe" id="group-stripe" role="img" aria-label="Transcript group completion map"></div><div class="group-note">Each segment is one transcript group. Hover it to see the group and its current state.</div></section>',
    '<section class="panel collab"><div class="collab-head"><strong>Science collaboration</strong><span id="collab-live">&#9679; checking contributors</span></div><div class="contributors" id="contributors"><div class="collab-empty">Checking who is helping this discovery.</div></div></section>',
    '<section class="panel"><div class="toolbar"><div><strong>Discovery table</strong> <span class="live" id="live">&#9679; loading snapshot</span></div><div class="controls"><select id="sort" aria-label="Order discoveries"><option value="combined">Best overall performance</option><option value="ml">Best ML R2</option><option value="cpg">Best CpG correlation</option><option value="column" hidden>Column sorting</option></select><input id="search" type="search" aria-label="Search discoveries" placeholder="Search gene, transcript, CpG or model"></div></div><div class="table-wrap"><table><thead><tr>',
    '<th class="sortable" tabindex="0" data-sort-key="status">Status</th><th class="sortable" tabindex="0" data-sort-key="type">Type</th><th class="sortable" tabindex="0" data-sort-key="gene">Gene</th><th class="sortable" tabindex="0" data-sort-key="transcript">Transcripts</th><th class="sortable" tabindex="0" data-sort-key="best_cpg">Best CpG</th><th class="sortable" tabindex="0" data-sort-key="cpgs" data-sort-type="number">CpGs</th><th class="sortable" tabindex="0" data-sort-key="cpg_rho" data-sort-type="number">CpG &rho;</th><th class="sortable" tabindex="0" data-sort-key="model">Model</th><th class="sortable" tabindex="0" data-sort-key="median_r2" data-sort-type="number">Median R&sup2;</th><th class="sortable" tabindex="0" data-sort-key="min_r2" data-sort-type="number">Min R&sup2;</th><th class="sortable" tabindex="0" data-sort-key="max_r2" data-sort-type="number">Max R&sup2;</th><th class="sortable" tabindex="0" data-sort-key="seeds" data-sort-type="number">Seeds</th><th class="sortable" tabindex="0" data-sort-key="samples" data-sort-type="number">Samples</th><th class="sortable" tabindex="0" data-sort-key="resolved_by">Resolved by</th><th class="sortable" tabindex="0" data-sort-key="group">Group</th></tr></thead><tbody id="rows"></tbody></table><div class="empty" id="empty">Connect to a job to see its discoveries.</div></div><div class="note">Click any column heading to sort it; click again to reverse the order. CpGs is the number of methylation probes evaluated in that computational group. A computational group may contain multiple biological transcripts only when their effective CpG and sample matrices are identical. Awaiting analysis means the group is known but model screening is not complete. Preliminary evidence comes from model screening. Stabilized evidence has completed the configured seed stability analysis.</div></section></main>',
    '<script>const initialJob=', encoded_job, ';const $=id=>document.getElementById(id);let all=[];const scalar=v=>Array.isArray(v)?v[0]:v;const normalize=o=>Object.fromEntries(Object.entries(o||{}).map(([k,v])=>[k,scalar(v)]));const fmt=v=>v===null||v===undefined||v===""||!Number.isFinite(Number(v))?"\\u2014":Number(v).toFixed(3);',
    'function badge(v){const c=v.replace(/ /g,"-");return `<span class="badge ${c}">${v}</span>`}function typeBadge(v){let c=v.startsWith("ML")?"type-ml":v.includes("&")?"type-both":"type-cpg";return `<span class="badge ${c}">${v}</span>`}function resolverBadge(v){return v?`<span class="resolver">${esc(v)}</span>`:"\u2014"}',
    'function renderGroupProgress(raw,contributors){const host=$("group-stripe");host.replaceChildren();const rows=(Array.isArray(raw)?raw:Object.values(raw||{})).map(normalize);const active=new Map((Array.isArray(contributors)?contributors:Object.values(contributors||{})).map(normalize).filter(x=>x.group).map(x=>[String(x.group),x]));const grouped=new Map();rows.forEach(row=>{const id=String(row.group||"");if(!id)return;if(!grouped.has(id))grouped.set(id,[]);grouped.get(id).push(row)});const groups=[...grouped].map(([id,items])=>{const worker=active.get(id);let state;if(worker)state="processing";else if(items.length&&items.every(x=>x.status==="stabilized"||x.status==="stability complete"))state="completed";else if(items.some(x=>x.status!=="awaiting analysis"))state="screened";else state="waiting";return{id,items,state,worker}}).sort((a,b)=>a.id.localeCompare(b.id,undefined,{numeric:true}));host.style.gridTemplateColumns=`repeat(${Math.max(1,groups.length)},minmax(1px,1fr))`;const counts=Object.fromEntries(["completed","processing","screened","waiting"].map(state=>[state,groups.filter(x=>x.state===state).length]));$("group-summary").textContent=groups.length?`${counts.completed} completed \u00b7 ${counts.processing} processing \u00b7 ${counts.screened} screened \u00b7 ${counts.waiting} waiting`:"No transcript groups are available yet.";groups.forEach(group=>{const segment=document.createElement("span");segment.className=`group-segment group-segment-${group.state}`;const label=group.state[0].toUpperCase()+group.state.slice(1);const detail=group.worker?` \u00b7 ${group.worker.scientist||group.worker.kind||"worker"} \u00b7 ${Math.round(Math.max(0,Math.min(1,Number(group.worker.progress)||0))*100)}%`:"";segment.title=`${group.id} \u00b7 ${label}${detail}`;segment.setAttribute("aria-label",segment.title);host.append(segment)})}',
    'function renderContributors(raw){const host=$("contributors");host.replaceChildren();const items=(Array.isArray(raw)?raw:Object.values(raw||{})).map(normalize);$("collab-live").textContent="\u25cf "+(items.length?items.length+" working now":"waiting for contributors");if(!items.length){const empty=document.createElement("div");empty.className="collab-empty";empty.textContent="No public scientist or worker server is active right now.";host.append(empty);return}items.forEach(row=>{const card=document.createElement("article");card.className="contributor";const top=document.createElement("div");top.className="contributor-top";const identity=document.createElement("div");const name=document.createElement("div");name.className="contributor-name";name.textContent=row.scientist||"Anonymous scientist";const kind=document.createElement("div");kind.className="contributor-kind";kind.textContent=row.kind||"contributor";identity.append(name,kind);const group=document.createElement("span");group.className="contributor-group";group.textContent=row.group||"scientific task";top.append(identity,group);const activity=document.createElement("div");activity.className="contributor-activity";activity.textContent=(row.activity||"Scientific computation in progress")+(row.candidate?" \u00b7 "+row.candidate:"");const track=document.createElement("div");track.className="contributor-track";const fill=document.createElement("div");fill.className="contributor-fill";const progress=Math.max(0,Math.min(1,Number(row.progress)||0));fill.style.width=(progress*100).toFixed(1)+"%";track.append(fill);const label=document.createElement("span");label.className="contributor-progress";label.textContent=Math.round(progress*100)+"% complete";card.append(top,activity,track,label);host.append(card)})}',
    'let columnSort=null;const sortableHeaders=[...document.querySelectorAll("th[data-sort-key]")];function updateHeaderSort(){sortableHeaders.forEach(th=>th.setAttribute("aria-sort",columnSort&&columnSort.key===th.dataset.sortKey?(columnSort.direction==="asc"?"ascending":"descending"):"none"))}function activateHeader(th){const key=th.dataset.sortKey;const type=th.dataset.sortType||"text";const direction=columnSort&&columnSort.key===key?(columnSort.direction==="desc"?"asc":"desc"):(type==="number"?"desc":"asc");columnSort={key,type,direction};const option=$("sort").querySelector("option[value=column]");option.textContent=`Column: ${th.textContent.trim()} ${direction==="asc"?"\u2191":"\u2193"}`;$("sort").value="column";updateHeaderSort();render()}sortableHeaders.forEach(th=>{th.onclick=()=>activateHeader(th);th.onkeydown=e=>{if(e.key==="Enter"||e.key===" "){e.preventDefault();activateHeader(th)}}});updateHeaderSort();',
    'function esc(v){const d=document.createElement("div");d.textContent=v??"";return d.innerHTML}function memberList(v){return esc(v).replaceAll(";","<br>")}function columnCompare(a,b){const av=a[columnSort.key],bv=b[columnSort.key];const aMissing=av===null||av===undefined||av===""||(columnSort.type==="number"&&!Number.isFinite(Number(av)));const bMissing=bv===null||bv===undefined||bv===""||(columnSort.type==="number"&&!Number.isFinite(Number(bv)));if(aMissing||bMissing)return aMissing===bMissing?0:(aMissing?1:-1);const compared=columnSort.type==="number"?Number(av)-Number(bv):String(av).localeCompare(String(bv),undefined,{numeric:true,sensitivity:"base"});return columnSort.direction==="desc"?-compared:compared}function render(){const q=$("search").value.toLowerCase();const mode=$("sort").value;const score=r=>{const ml=Number(r.median_r2),cpg=Math.abs(Number(r.cpg_rho));if(mode==="combined"){const values=[ml,cpg].filter(Number.isFinite);return values.length?Math.max(...values):-Infinity}const v=mode==="cpg"?cpg:ml;return Number.isFinite(v)?v:-Infinity};const data=all.filter(r=>Object.values(r).join(" ").toLowerCase().includes(q)).sort((a,b)=>columnSort?columnCompare(a,b):score(b)-score(a));$("rows").innerHTML=data.map(r=>`<tr><td>${badge(esc(r.status))}</td><td>${typeBadge(esc(r.type))}</td><td><b>${memberList(r.gene)}</b></td><td>${memberList(r.transcript)}</td><td>${esc(r.best_cpg)}</td><td>${fmt(r.cpgs).replace(".000","")}</td><td>${fmt(r.cpg_rho)}</td><td>${esc(r.model)}</td><td><b>${fmt(r.median_r2)}</b></td><td>${fmt(r.min_r2)}</td><td>${fmt(r.max_r2)}</td><td>${fmt(r.seeds).replace(".000","")}</td><td>${fmt(r.samples).replace(".000","")}</td><td>${resolverBadge(r.resolved_by)}</td><td>${esc(r.group)}</td></tr>`).join("");$("empty").style.display=data.length?"none":"block"}$("search").oninput=render;$("sort").onchange=()=>{columnSort=null;updateHeaderSort();render()};',
    'async function load(){if(!initialJob)return;try{const r=await fetch(`/reports/${encodeURIComponent(initialJob)}/data`,{cache:"no-store"});if(!r.ok)throw Error(await r.text());const d=await r.json();const job=normalize(d.job);const progress=normalize(d.progress);const collaboration=d.collaboration||{};const raw=Array.isArray(d.discoveries)?d.discoveries:Object.values(d.discoveries||{});all=raw.map(normalize);renderGroupProgress(raw,collaboration.contributors);renderContributors(collaboration.contributors);$("total").textContent=progress.total||"\\u2014";$("screened").textContent=progress.screened||0;$("stabilized").textContent=progress.stabilized||0;const vals=all.map(x=>Number(x.median_r2)).filter(Number.isFinite);$("best").textContent=vals.length?Math.max(...vals).toFixed(3):"\\u2014";$("title").textContent=(job.accession||"ugPlot")+" live discoveries";$("subtitle").textContent=(job.target?"Target: "+job.target+" \\u00b7 ":"")+(job.message||job.state);$("live").textContent="\\u25cf updated "+new Date().toLocaleTimeString();render()}catch(e){$("live").textContent="\\u25cf "+e.message;$("live").style.color="#d44"}}load();setInterval(load,10000);</script></body></html>')
}

#' Start a ugplot job server
#'
#' Starts an HTTP server that can receive datasets, run jobs in background R
#' processes, report progress, and return completed results.
#'
#' @param host Interface to bind. Use `"0.0.0.0"` for remote access.
#' @param port Port to listen on.
#' @param jobs_dir Directory used to persist datasets, status and results.
#' @param token Bearer token. Required when listening on a non-local interface.
#' @param name Local server handle name used by status/stop when the server is
#'   started directly.
#' @param register Whether to write a local state file for status/stop.
#' @param auto_resume_interval Seconds between automatic crashed-job resume
#'   checks. Use 0 to disable the background monitor.
#' @return The plumber server result.
#' @export
ugPlotServer <- function(host = "0.0.0.0", port = 8080,
                         jobs_dir = ugplot_default_jobs_dir(),
                         token = "", name = "default", register = TRUE,
                         auto_resume_interval = 30) {
  local_hosts <- c("127.0.0.1", "::1", "localhost")
  if (!(host %in% local_hosts) && !nzchar(token)) {
    stop("A bearer token is required when ugPlotServer listens on a non-local interface.", call. = FALSE)
  }
  # The token may be supplied as an R argument instead of an environment
  # variable. Background job and auto-resume processes inherit the environment,
  # so make the authoritative server token available to them as well. This is
  # required when a resumed distributed coordinator dispatches work back to the
  # same protected server.
  if (nzchar(token)) {
    Sys.setenv(UGPLOT_SERVER_TOKEN = token)
  }
  Sys.setenv(UGPLOT_SERVER_NAME = as.character(name))
  ugplot_assert_server_system_deps()
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Package 'plumber' is required to start ugPlotServer(). Run ugPlotInstallServerDeps().", call. = FALSE)
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required to start background jobs. Run ugPlotInstallServerDeps().", call. = FALSE)
  }

  ugplot_ensure_dir(jobs_dir)
  if (exists("ugplot_collaboration_read_index", mode = "function", inherits = TRUE)) {
    try(ugplot_collaboration_read_index(jobs_dir), silent = TRUE)
  }
  source_dir <- if (file.exists(file.path(getwd(), "R", "app.R"))) normalizePath(getwd(), mustWork = FALSE) else NULL
  auto_resume_process <- ugplot_start_auto_resume_monitor(
    jobs_dir = jobs_dir,
    interval = auto_resume_interval,
    source_dir = source_dir,
    lib_paths = .libPaths(),
    server_token = token
  )
  if (!is.null(auto_resume_process)) {
    on.exit({
      if (auto_resume_process$is_alive()) {
        try(auto_resume_process$kill_tree(), silent = TRUE)
        try(auto_resume_process$kill(), silent = TRUE)
      }
    }, add = TRUE)
  }
  if (isTRUE(register) && exists("ugplot_register_server_state", mode = "function", inherits = TRUE)) {
    ugplot_register_server_state(
      host = host,
      port = port,
      jobs_dir = jobs_dir,
      token = token,
      name = name,
      pid = Sys.getpid(),
      started_by = "ugPlotServer"
    )
    on.exit({
      if (exists("ugplot_mark_server_state_stopped", mode = "function", inherits = TRUE)) {
        ugplot_mark_server_state_stopped(name)
      }
    }, add = TRUE)
  }

  pr <- plumber::pr()

  pr$filter("auth", function(req, res) {
    request_path <- as.character(req$PATH_INFO %||% "")
    if (grepl("^/(collaboration|reports)(/|$)", request_path)) {
      if (grepl("^/collaboration(/|$)", request_path)) {
        max_bytes <- if (grepl("/complete$", request_path)) 10 * 1024^2 else 64 * 1024
        content_length <- suppressWarnings(as.numeric(req$HTTP_CONTENT_LENGTH %||% 0))
        body_bytes <- nchar(req$postBody %||% "", type = "bytes")
        if ((is.finite(content_length) && content_length > max_bytes) || body_bytes > max_bytes) {
          res$status <- 413
          return(list(error = "Request body is too large"))
        }
      }
      return(plumber::forward())
    }
    if (!ugplot_check_token(req, token)) {
      res$status <- 401
      return(list(error = "Unauthorized"))
    }
    plumber::forward()
  })

  pr$handle("GET", "/collaboration", function() {
    ugplot_collaboration_public_status(jobs_dir)
  })

  pr$handle("POST", "/collaboration/claim", function(req, res) {
    tryCatch({
      body <- ugplot_request_json_body(req, 64 * 1024)
      claimed <- ugplot_collaboration_claim_task(
        client_id = body$client_id %||% "",
        capabilities = body$capabilities %||% list(),
        lease_seconds = 120,
        jobs_dir = jobs_dir
      )
      if (is.null(claimed)) {
        return(list(task = NULL, message = "No compatible mission is waiting."))
      }
      public_task <- claimed$task
      public_task$payload_path <- NULL
      public_task$result_path <- NULL
      list(task = public_task, payload_rds_base64 = ugplot_collaboration_encode_rds(claimed$payload))
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("POST", "/collaboration/compatibility", function(req, res) {
    tryCatch({
      body <- ugplot_request_json_body(req, 64 * 1024)
      ugplot_collaboration_compatibility(body$capabilities %||% list(), jobs_dir)
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("POST", "/collaboration/<task_id>/heartbeat", function(task_id, req, res) {
    tryCatch({
      body <- ugplot_request_json_body(req, 64 * 1024)
      ugplot_collaboration_heartbeat(
        task_id, body$lease_id %||% "", body$client_id %||% "",
        lease_seconds = 120, telemetry = body$telemetry %||% list(), jobs_dir = jobs_dir
      )
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("POST", "/collaboration/<task_id>/release", function(task_id, req, res) {
    tryCatch({
      body <- ugplot_request_json_body(req, 64 * 1024)
      ugplot_collaboration_release_task(
        task_id, body$lease_id %||% "", body$client_id %||% "", jobs_dir
      )
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("POST", "/collaboration/<task_id>/complete", function(task_id, req, res) {
    tryCatch({
      body <- ugplot_request_json_body(req, 10 * 1024^2)
      if (!identical(suppressWarnings(as.integer(body$protocol_version %||% 0L)), 2L)) {
        stop("Science Collab result protocol version 2 is required.", call. = FALSE)
      }
      ugplot_collaboration_complete_task(
        task_id, body$lease_id %||% "", body$client_id %||% "", body$result %||% NULL,
        jobs_dir = jobs_dir
      )
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/health", function(req) {
    query <- req$argsQuery %||% list()
    include_resources <- !tolower(as.character(query$resources %||% "true")) %in% c("0", "false", "no")
    total_cpus <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
    if (is.na(total_cpus) || total_cpus < 1L) {
      total_cpus <- 1L
    }
    list(
      status = "ok",
      ugplot_build_version = ugplot_build_version(),
      jobs_dir = jobs_dir,
      cpus = as.integer(total_cpus),
      default_cpu_limit = max(1L, as.integer(total_cpus) - 1L),
      resources = ugplot_server_resource_snapshot(jobs_dir, include_jobs = include_resources),
      capabilities = list(
        delete_job = TRUE,
        resume_job = TRUE,
        drain_job = TRUE,
        auto_resume_monitor = !is.null(auto_resume_process),
        job_bundle = TRUE,
        job_preview = TRUE,
        job_model_timing = TRUE,
        job_model_diagnostics = TRUE,
        job_model_policy = TRUE,
        job_config_summary = TRUE,
        job_resource_monitor = !is.null(auto_resume_process),
        job_monitor_snapshot = TRUE,
        job_group_activity = TRUE,
        job_group_dataset = TRUE,
        job_discovery_report = TRUE,
        server_resources = TRUE,
        lightweight_health = TRUE,
        geo_pipeline = TRUE,
        geo_cpg_summary = TRUE,
        geo_cpg_lookup = TRUE,
        distributed_geo_screening = TRUE,
        distributed_protocol_version = 1L
      )
    )
  })

  pr$handle("GET", "/jobs", function(req) {
    # The overview is intentionally metadata-only. Full status/configuration
    # inspection remains available through the individual job endpoints.
    query <- req$argsQuery %||% list()
    include_internal <- tolower(as.character(query$include_internal %||% "false")) %in%
      c("1", "true", "yes")
    ugplot_list_jobs(jobs_dir, include_internal = include_internal, lightweight = TRUE)
  })

  pr$handle(
    "GET", "/reports", function(res) {
      res$setHeader("Cache-Control", "no-store")
      ugplot_discovery_report_html()
    },
    serializer = plumber::serializer_content_type("text/html; charset=UTF-8")
  )

  pr$handle(
    "GET", "/reports/assets/ugplot.png", function(res) {
      logo_path <- system.file("extdata", "ugplot.png", package = "ugplot")
      if (!nzchar(logo_path) || !file.exists(logo_path)) {
        logo_path <- file.path(getwd(), "inst", "extdata", "ugplot.png")
      }
      if (!file.exists(logo_path)) {
        res$status <- 404
        return(raw(0))
      }
      res$setHeader("Cache-Control", "public, max-age=86400")
      readBin(logo_path, what = "raw", n = file.info(logo_path)$size)
    },
    serializer = plumber::serializer_content_type("image/png")
  )

  pr$handle(
    "GET", "/reports/<job_id>", function(job_id, res) {
      ugplot_validate_job_id(job_id)
      res$setHeader("Cache-Control", "no-store")
      ugplot_discovery_report_html(job_id)
    },
    serializer = plumber::serializer_content_type("text/html; charset=UTF-8")
  )

  pr$handle(
    "GET", "/reports/<job_id>/data", function(job_id, res) {
      tryCatch({
        res$setHeader("Cache-Control", "no-store")
        snapshot_path <- ugplot_job_discovery_snapshot_path(job_id, jobs_dir)
        if (!file.exists(snapshot_path)) {
          ugplot_refresh_job_discovery_snapshot(job_id, jobs_dir)
        }
        snapshot <- paste(readLines(snapshot_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        snapshot_version <- tryCatch(
          suppressWarnings(as.integer(jsonlite::fromJSON(snapshot, simplifyVector = FALSE)$protocol_version %||% 0L)),
          error = function(e) 0L
        )
        if (is.na(snapshot_version) || snapshot_version < 2L) {
          ugplot_refresh_job_discovery_snapshot(job_id, jobs_dir)
          snapshot <- paste(readLines(snapshot_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        }
        ugplot_discovery_snapshot_live_status(snapshot, job_id, jobs_dir)
      }, error = function(e) {
        res$status <- 404
        jsonlite::toJSON(list(error = conditionMessage(e)), auto_unbox = TRUE)
      })
    },
    serializer = plumber::serializer_content_type("application/json; charset=UTF-8")
  )

  pr$handle("GET", "/models/dependencies", function() {
    ugplot_model_dependency_status()
  })

  pr$handle("GET", "/jobs/<job_id>/status", function(job_id, res) {
    tryCatch(
      {
        res$setHeader("Cache-Control", "no-store")
        ugplot_read_job_status_lightweight(job_id, jobs_dir)
      },
      error = function(e) {
        res$status <- 404
        list(error = conditionMessage(e))
      }
    )
  })

  pr$handle("GET", "/jobs/<job_id>", function(job_id, res) {
    tryCatch(
      {
        ugplot_read_job_status(job_id, jobs_dir)
      },
      error = function(e) {
        res$status <- 404
        list(error = conditionMessage(e))
      }
    )
  })

  pr$handle("GET", "/jobs/<job_id>/monitor", function(job_id, req, res) {
    tryCatch({
      query <- req$argsQuery %||% list()
      include_groups <- !tolower(as.character(query$groups %||% "true")) %in% c("0", "false", "no")
      resource_lines <- suppressWarnings(as.integer(query$resource_lines %||% 60L))
      if (is.na(resource_lines) || resource_lines < 1L) resource_lines <- 60L
      resource_lines <- min(resource_lines, 120L)
      res$setHeader("Cache-Control", "no-store")
      ugplot_job_monitor_snapshot(
        job_id = job_id,
        jobs_dir = jobs_dir,
        include_groups = include_groups,
        resource_lines = resource_lines
      )
    }, error = function(e) {
      res$status <- 404
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/jobs/<job_id>/log", function(job_id, req, res) {
    tryCatch({
      query <- req$argsQuery %||% list()
      max_lines <- suppressWarnings(as.integer(query$max_lines %||% 200L))
      if (is.na(max_lines) || max_lines < 1L) {
        max_lines <- 200L
      }
      list(log = ugplot_read_job_log(job_id, jobs_dir, max_lines = max_lines))
    }, error = function(e) {
      res$status <- 404
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/jobs/<job_id>/resources", function(job_id, req, res) {
    tryCatch({
      query <- req$argsQuery %||% list()
      max_lines <- suppressWarnings(as.integer(query$max_lines %||% 500L))
      if (is.na(max_lines) || max_lines < 1L) {
        max_lines <- 500L
      }
      list(resources = ugplot_read_job_resources(job_id, jobs_dir, max_lines = max_lines))
    }, error = function(e) {
      res$status <- 404
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/jobs/<job_id>/groups", function(job_id, res) {
    tryCatch(
      ugplot_collaboration_job_group_activity(job_id, jobs_dir),
      error = function(e) {
        res$status <- 404
        list(error = conditionMessage(e))
      }
    )
  })

  pr$handle("GET", "/jobs/<job_id>/group-datasets", function(job_id, res) {
    tryCatch(
      list(groups = ugplot_job_geo_group_datasets(job_id, jobs_dir)),
      error = function(e) {
        res$status <- 404
        list(error = conditionMessage(e))
      }
    )
  })

  pr$handle("GET", "/jobs/<job_id>/group-datasets/<group_id>/rds", function(job_id, group_id, res) {
    tryCatch({
      payload <- ugplot_read_job_geo_group_dataset(job_id, group_id, jobs_dir)
      payload_path <- tempfile(fileext = ".rds")
      on.exit(unlink(payload_path), add = TRUE)
      saveRDS(payload, payload_path)
      list(
        filename = paste0("ugplot-", payload$accession, "-", payload$group_id, "-dataset.rds"),
        content_base64 = base64enc::base64encode(payload_path)
      )
    }, error = function(e) {
      res$status <- 404
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/jobs/<job_id>/geo-cpg-summary", function(job_id, req, res) {
    tryCatch({
      query <- req$argsQuery %||% list()
      threshold <- suppressWarnings(as.numeric(query$threshold %||% NA_real_))
      spearman_min_samples_pct <- suppressWarnings(as.numeric(query$spearman_min_samples_pct %||% 80))
      bin_width <- suppressWarnings(as.numeric(query$bin_width %||% 0.05))
      ugplot_geo_cpg_summary_for_job(
        job_id = job_id,
        jobs_dir = jobs_dir,
        threshold = threshold,
        spearman_min_samples_pct = spearman_min_samples_pct,
        bin_width = bin_width
      )
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/jobs/<job_id>/geo-cpg-lookup", function(job_id, req, res) {
    tryCatch({
      query <- req$argsQuery %||% list()
      cpg <- as.character(query$cpg %||% "")
      threshold <- suppressWarnings(as.numeric(query$threshold %||% NA_real_))
      spearman_min_samples_pct <- suppressWarnings(as.numeric(query$spearman_min_samples_pct %||% 80))
      ugplot_geo_cpg_lookup_for_job(
        job_id = job_id,
        jobs_dir = jobs_dir,
        cpg = cpg,
        threshold = threshold,
        spearman_min_samples_pct = spearman_min_samples_pct
      )
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("POST", "/jobs/<job_id>/stop", function(job_id, res) {
    tryCatch(
      ugplot_stop_job(job_id, jobs_dir),
      error = function(e) {
        res$status <- 404
        list(error = conditionMessage(e))
      }
    )
  })

  pr$handle("POST", "/jobs/<job_id>/drain", function(job_id, res) {
    tryCatch(ugplot_request_job_drain(job_id, jobs_dir), error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("POST", "/jobs/<job_id>/workers", function(job_id, req, res) {
    tryCatch({
      body <- ugplot_request_json_body(req)
      workers <- ugplot_normalize_distributed_workers(body$workers %||% list())
      for (worker in workers) {
        loopback_worker <- grepl(
          "^https?://(localhost|127(?:[.][0-9]+){3}|\\[?::1\\]?)(:|/|$)",
          worker$url,
          ignore.case = TRUE
        )
        # Plumber handles this request in the same process that serves the
        # loopback health endpoint. Calling it synchronously would deadlock
        # until timeout; loopback authentication is enforced again when the
        # coordinator dispatches the worker job.
        if (isTRUE(loopback_worker)) next
        tryCatch(
          ugplot_remote_health(
            worker$url,
            worker$token,
            timeout_seconds = 10,
            include_resources = FALSE
          ),
          error = function(e) stop(
            "Worker ", worker$name, " failed authentication/health validation: ",
            conditionMessage(e),
            call. = FALSE
          )
        )
      }
      ugplot_replace_job_distributed_workers(job_id, workers, jobs_dir)
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/jobs/<job_id>/model-policy", function(job_id, res) {
    tryCatch(
      ugplot_read_job_model_policy(job_id, jobs_dir),
      error = function(e) {
        res$status <- 404
        list(error = conditionMessage(e))
      }
    )
  })

  pr$handle("POST", "/jobs/<job_id>/model-policy", function(job_id, req, res) {
    tryCatch({
      body <- ugplot_request_json_body(req)
      policy <- ugplot_set_job_model_enabled(
        job_id, body$model %||% "", isTRUE(body$enabled), jobs_dir
      )
      # Pending public-collaboration offers may already contain the old model
      # list. Close only unclaimed offers; leased/running work is untouched.
      if (exists("ugplot_collaboration_task_ids", mode = "function", inherits = TRUE) &&
          exists("ugplot_collaboration_close_pending_task", mode = "function", inherits = TRUE)) {
        pending <- tryCatch(
          ugplot_collaboration_task_ids(jobs_dir, states = "pending", parent_job_id = job_id),
          error = function(e) character(0)
        )
        for (task_id in pending) {
          try(ugplot_collaboration_close_pending_task(
            task_id, reason = "model_policy_changed", jobs_dir = jobs_dir
          ), silent = TRUE)
        }
      }
      policy
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("POST", "/jobs/<job_id>/resume", function(job_id, res) {
    tryCatch({
      started <- ugplot_resume_background_job(job_id, jobs_dir, startup_wait_seconds = 0)
      res$status <- 202
      started$job
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("DELETE", "/jobs/<job_id>", function(job_id, req, res) {
    tryCatch({
      query <- req$argsQuery %||% list()
      force <- tolower(as.character(query$force %||% "false")) %in% c("1", "true", "yes")
      ugplot_delete_job(job_id, jobs_dir, force = force)
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("POST", "/jobs", function(req, res) {
    tryCatch({
      dataset <- ugplot_request_dataset(req)
      config <- ugplot_request_config(req)
      config <- ugplot_validate_remote_job_config(config)
      started <- ugplot_start_background_job(dataset, config, jobs_dir, startup_wait_seconds = 0)
      res$status <- 202
      started$job
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/jobs/<job_id>/result", function(job_id, res) {
    tryCatch(
      ugplot_read_job_result(job_id, jobs_dir),
      error = function(e) {
        res$status <- 404
        list(error = conditionMessage(e))
      }
    )
  })

  pr$handle("GET", "/jobs/<job_id>/result-rds", function(job_id, res) {
    tryCatch({
      status <- ugplot_read_job_status(job_id, jobs_dir)
      result_path <- status$result_path %||% status$partial_result_path
      if (is.null(result_path) || !file.exists(result_path)) {
        stop("Result is not available for job: ", job_id, call. = FALSE)
      }
      list(
        filename = paste0("ugplot-job-", job_id, ".rds"),
        content_base64 = base64enc::base64encode(result_path)
      )
    }, error = function(e) {
      res$status <- 404
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/jobs/<job_id>/preview-rds", function(job_id, res) {
    tryCatch({
      preview <- ugplot_read_job_preview_result(job_id, jobs_dir)
      preview_path <- tempfile(fileext = ".rds")
      on.exit(unlink(preview_path), add = TRUE)
      saveRDS(preview, preview_path)
      list(
        filename = paste0("ugplot-job-", job_id, "-preview.rds"),
        content_base64 = base64enc::base64encode(preview_path)
      )
    }, error = function(e) {
      res$status <- 404
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/jobs/<job_id>/model-timing-rds", function(job_id, res) {
    tryCatch({
      timing <- ugplot_read_job_model_timing(job_id, jobs_dir)
      timing_path <- tempfile(fileext = ".rds")
      on.exit(unlink(timing_path), add = TRUE)
      saveRDS(timing, timing_path)
      list(
        filename = paste0("ugplot-job-", job_id, "-model-timing.rds"),
        content_base64 = base64enc::base64encode(timing_path)
      )
    }, error = function(e) {
      res$status <- 404
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/jobs/<job_id>/model-diagnostics-rds", function(job_id, req, res) {
    tryCatch({
      query <- req$argsQuery %||% list()
      diagnostics <- ugplot_read_job_model_diagnostics(
        job_id, as.character(query$model %||% ""), jobs_dir
      )
      diagnostics_path <- tempfile(fileext = ".rds")
      on.exit(unlink(diagnostics_path), add = TRUE)
      saveRDS(diagnostics, diagnostics_path)
      list(
        filename = paste0("ugplot-job-", job_id, "-model-diagnostics.rds"),
        content_base64 = base64enc::base64encode(diagnostics_path)
      )
    }, error = function(e) {
      res$status <- 404
      list(error = conditionMessage(e))
    })
  })

  pr$handle("GET", "/jobs/<job_id>/bundle-rds", function(job_id, res) {
    tryCatch({
      bundle <- ugplot_read_job_bundle(job_id, jobs_dir)
      bundle_path <- tempfile(fileext = ".rds")
      on.exit(unlink(bundle_path), add = TRUE)
      saveRDS(bundle, bundle_path)
      list(
        filename = paste0("ugplot-job-", job_id, "-bundle.rds"),
        content_base64 = base64enc::base64encode(bundle_path)
      )
    }, error = function(e) {
      if (grepl("while the job is active", conditionMessage(e), fixed = TRUE)) {
        res$status <- 409
      } else {
        res$status <- 404
      }
      list(error = conditionMessage(e))
    })
  })

  pr$run(host = host, port = port)
}
