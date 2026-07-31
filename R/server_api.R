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

ugplot_request_json_body <- function(req) {
  cached_body <- req$ugplot_json_body %||% NULL
  if (!is.null(cached_body)) {
    return(cached_body)
  }
  body <- req$postBody %||% ""
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
    run_key <- ugplot_geo_transcript_ml_run_key(target, threshold, min_samples)
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
    source = source
  )
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

ugplot_job_discovery_report <- function(job_id,
                                        jobs_dir = ugplot_default_jobs_dir()) {
  paths <- ugplot_job_discovery_report_paths(job_id, jobs_dir)
  status <- ugplot_read_job_status(job_id, jobs_dir)
  groups <- ugplot_read_discovery_csv(paths$groups)
  screening <- ugplot_read_discovery_csv(paths$screening)
  stability <- ugplot_read_discovery_csv(paths$stability)

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
      min_r2 = number_value(row, c("MinR2", "MinMetric")),
      max_r2 = number_value(row, c("MaxR2", "MaxMetric", "BestMetric")),
      metric_se = number_value(row, "MetricSE"),
      seeds = number_value(row, "SeedsRun"),
      stratum = paste0(text_value(row, "StratumColumn"), if (nzchar(text_value(row, "StratumValue"))) "=" else "", text_value(row, "StratumValue")),
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
  manifest <- data.frame()
  if (file.exists(paths$manifest)) {
    manifest <- tryCatch(readRDS(paths$manifest), error = function(e) data.frame())
    if (is.data.frame(manifest)) total <- nrow(manifest)
  }
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
    protocol_version = 1L,
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
    '.stats{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin:20px 0}.stat{padding:18px 20px;background:#ffffffd9;border:1px solid #fff;border-radius:18px}.stat b{display:block;font-size:28px}.stat span{color:var(--muted);font-size:12px;text-transform:uppercase;font-weight:800}.panel{padding:20px}.toolbar{display:flex;align-items:center;gap:18px;justify-content:space-between;margin-bottom:16px}.toolbar strong{font-size:17px}.live{color:var(--green);font-weight:800}.table-wrap{overflow:auto;max-height:70vh;border:1px solid var(--line);border-radius:16px}table{border-collapse:separate;border-spacing:0;width:100%;min-width:1200px}th{position:sticky;top:0;background:#f7f8fe;text-align:left;padding:13px 12px;color:#687392;font-size:11px;letter-spacing:.08em;text-transform:uppercase;z-index:1}td{padding:11px 12px;border-top:1px solid #edf0f8;white-space:nowrap}tr:hover td{background:#f7fbff}.badge{padding:5px 9px;border-radius:999px;font-size:11px;font-weight:800}.awaiting-analysis{background:#eef1f8;color:#69738d}.preliminary{background:#fff0dc;color:#a96000}.stabilized{background:#dcfaef;color:#087958}.stability-complete{background:#e8efff;color:#3159a5}.type-ml{background:#fff0e7;color:#d65b18}.type-cpg{background:#dff6ff;color:#0878a7}.type-both{background:#f5ddff;color:#8c189a}.empty{text-align:center;padding:60px;color:var(--muted)}.note{font-size:12px;color:var(--muted);margin-top:10px}',
    '.collab{margin-bottom:20px}.collab-head{display:flex;align-items:center;justify-content:space-between;gap:15px;margin-bottom:14px}.collab-head strong{font-size:18px}.collab-head span{color:var(--green);font-weight:800}.contributors{display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:12px}.contributor{padding:15px;border:1px solid var(--line);border-radius:17px;background:linear-gradient(135deg,#fbfaff,#f3fdff)}.contributor-top{display:flex;align-items:flex-start;justify-content:space-between;gap:10px}.contributor-name{font-size:16px;font-weight:850}.contributor-kind{color:var(--violet);font-size:10px;text-transform:uppercase;font-weight:850;letter-spacing:.08em}.contributor-group{padding:4px 8px;border-radius:999px;background:#e8e2ff;color:#5640c9;font-size:11px;font-weight:800}.contributor-activity{color:var(--muted);font-size:13px;margin:8px 0}.contributor-track{height:7px;border-radius:999px;background:#e8ebf5;overflow:hidden}.contributor-fill{height:100%;border-radius:inherit;background:linear-gradient(90deg,var(--violet),var(--cyan))}.contributor-progress{display:block;text-align:right;color:var(--muted);font-size:11px;margin-top:4px}.collab-empty{color:var(--muted);padding:8px 2px}',
    '@media(max-width:900px){.shell{padding:12px}.brand{align-items:flex-start;flex-direction:column}.brand img{width:210px}.stats{grid-template-columns:1fr 1fr}.toolbar{align-items:stretch;flex-direction:column}.controls{align-items:stretch;flex-direction:column}.controls select,.controls input{width:100%;min-width:0}}</style></head><body>',
    '<main class="shell"><section class="hero"><div class="brand"><img src="/reports/assets/ugplot.png" alt="ugPlot"><div class="brand-copy"><div class="eyebrow">UGPLOT LIVE DISCOVERY REPORT</div><h1 id="title">Scientific discoveries as they emerge</h1><p id="subtitle">The report is loading the job identified by this URL.</p></div></div></section>',
    '<section class="stats"><div class="stat"><b id="total">&mdash;</b><span>Total groups</span></div><div class="stat"><b id="screened">&mdash;</b><span>Screened</span></div><div class="stat"><b id="stabilized">&mdash;</b><span>Stabilized</span></div><div class="stat"><b id="best">&mdash;</b><span>Best median R&sup2;</span></div></section>',
    '<section class="panel collab"><div class="collab-head"><strong>Science collaboration</strong><span id="collab-live">&#9679; checking contributors</span></div><div class="contributors" id="contributors"><div class="collab-empty">Checking who is helping this discovery.</div></div></section>',
    '<section class="panel"><div class="toolbar"><div><strong>Discovery table</strong> <span class="live" id="live">&#9679; loading snapshot</span></div><div class="controls"><select id="sort" aria-label="Order discoveries"><option value="combined">Best overall performance</option><option value="ml">Best ML R2</option><option value="cpg">Best CpG correlation</option></select><input id="search" type="search" aria-label="Search discoveries" placeholder="Search gene, transcript, CpG or model"></div></div><div class="table-wrap"><table><thead><tr>',
    '<th>Status</th><th>Type</th><th>Gene</th><th>Transcripts</th><th>Best CpG</th><th>CpGs</th><th>CpG &rho;</th><th>Model</th><th>Median R&sup2;</th><th>Min R&sup2;</th><th>Max R&sup2;</th><th>Seeds</th><th>Samples</th><th>Group</th></tr></thead><tbody id="rows"></tbody></table><div class="empty" id="empty">Connect to a job to see its discoveries.</div></div><div class="note">CpGs is the number of methylation probes evaluated in that computational group. A computational group may contain multiple biological transcripts only when their effective CpG and sample matrices are identical. Awaiting analysis means the group is known but model screening is not complete. Preliminary evidence comes from model screening. Stabilized evidence has completed the configured seed stability analysis.</div></section></main>',
    '<script>const initialJob=', encoded_job, ';const $=id=>document.getElementById(id);let all=[];const scalar=v=>Array.isArray(v)?v[0]:v;const normalize=o=>Object.fromEntries(Object.entries(o||{}).map(([k,v])=>[k,scalar(v)]));const fmt=v=>v===null||v===undefined||v===""||!Number.isFinite(Number(v))?"\\u2014":Number(v).toFixed(3);',
    'function badge(v){const c=v.replace(/ /g,"-");return `<span class="badge ${c}">${v}</span>`}function typeBadge(v){let c=v.startsWith("ML")?"type-ml":v.includes("&")?"type-both":"type-cpg";return `<span class="badge ${c}">${v}</span>`}',
    'function renderContributors(raw){const host=$("contributors");host.replaceChildren();const items=(Array.isArray(raw)?raw:Object.values(raw||{})).map(normalize);$("collab-live").textContent="\u25cf "+(items.length?items.length+" working now":"waiting for contributors");if(!items.length){const empty=document.createElement("div");empty.className="collab-empty";empty.textContent="No public scientist or worker server is active right now.";host.append(empty);return}items.forEach(row=>{const card=document.createElement("article");card.className="contributor";const top=document.createElement("div");top.className="contributor-top";const identity=document.createElement("div");const name=document.createElement("div");name.className="contributor-name";name.textContent=row.scientist||"Anonymous scientist";const kind=document.createElement("div");kind.className="contributor-kind";kind.textContent=row.kind||"contributor";identity.append(name,kind);const group=document.createElement("span");group.className="contributor-group";group.textContent=row.group||"scientific task";top.append(identity,group);const activity=document.createElement("div");activity.className="contributor-activity";activity.textContent=(row.activity||"Scientific computation in progress")+(row.candidate?" \u00b7 "+row.candidate:"");const track=document.createElement("div");track.className="contributor-track";const fill=document.createElement("div");fill.className="contributor-fill";const progress=Math.max(0,Math.min(1,Number(row.progress)||0));fill.style.width=(progress*100).toFixed(1)+"%";track.append(fill);const label=document.createElement("span");label.className="contributor-progress";label.textContent=Math.round(progress*100)+"% complete";card.append(top,activity,track,label);host.append(card)})}',
    'function esc(v){const d=document.createElement("div");d.textContent=v??"";return d.innerHTML}function memberList(v){return esc(v).replaceAll(";","<br>")}function render(){const q=$("search").value.toLowerCase();const mode=$("sort").value;const score=r=>{const ml=Number(r.median_r2),cpg=Math.abs(Number(r.cpg_rho));if(mode==="combined")return Number.isFinite(ml)&&Number.isFinite(cpg)?(ml+cpg)/2:Number.isFinite(cpg)?cpg/2:-Infinity;const v=mode==="cpg"?cpg:ml;return Number.isFinite(v)?v:-Infinity};const data=all.filter(r=>Object.values(r).join(" ").toLowerCase().includes(q)).sort((a,b)=>score(b)-score(a));$("rows").innerHTML=data.map(r=>`<tr><td>${badge(esc(r.status))}</td><td>${typeBadge(esc(r.type))}</td><td><b>${memberList(r.gene)}</b></td><td>${memberList(r.transcript)}</td><td>${esc(r.best_cpg)}</td><td>${fmt(r.cpgs).replace(".000","")}</td><td>${fmt(r.cpg_rho)}</td><td>${esc(r.model)}</td><td><b>${fmt(r.median_r2)}</b></td><td>${fmt(r.min_r2)}</td><td>${fmt(r.max_r2)}</td><td>${fmt(r.seeds).replace(".000","")}</td><td>${fmt(r.samples).replace(".000","")}</td><td>${esc(r.group)}</td></tr>`).join("");$("empty").style.display=data.length?"none":"block"}$("search").oninput=render;$("sort").onchange=render;',
    'async function load(){if(!initialJob)return;try{const r=await fetch(`/reports/${encodeURIComponent(initialJob)}/data`,{cache:"no-store"});if(!r.ok)throw Error(await r.text());const d=await r.json();const job=normalize(d.job);const progress=normalize(d.progress);const collaboration=d.collaboration||{};const raw=Array.isArray(d.discoveries)?d.discoveries:Object.values(d.discoveries||{});all=raw.map(normalize);renderContributors(collaboration.contributors);$("total").textContent=progress.total||"\\u2014";$("screened").textContent=progress.screened||0;$("stabilized").textContent=progress.stabilized||0;const vals=all.map(x=>Number(x.median_r2)).filter(Number.isFinite);$("best").textContent=vals.length?Math.max(...vals).toFixed(3):"\\u2014";$("title").textContent=(job.accession||"ugPlot")+" live discoveries";$("subtitle").textContent=(job.target?"Target: "+job.target+" \\u00b7 ":"")+(job.message||job.state);$("live").textContent="\\u25cf updated "+new Date().toLocaleTimeString();render()}catch(e){$("live").textContent="\\u25cf "+e.message;$("live").style.color="#d44"}}load();setInterval(load,10000);</script></body></html>')
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
      body <- ugplot_request_json_body(req)
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
      body <- ugplot_request_json_body(req)
      ugplot_collaboration_compatibility(body$capabilities %||% list(), jobs_dir)
    }, error = function(e) {
      res$status <- 400
      list(error = conditionMessage(e))
    })
  })

  pr$handle("POST", "/collaboration/<task_id>/heartbeat", function(task_id, req, res) {
    tryCatch({
      body <- ugplot_request_json_body(req)
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
      body <- ugplot_request_json_body(req)
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
      body <- ugplot_request_json_body(req)
      result <- ugplot_read_rds_base64(body$result_rds_base64 %||% "")
      ugplot_collaboration_complete_task(
        task_id, body$lease_id %||% "", body$client_id %||% "", result,
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
        job_config_summary = TRUE,
        job_resource_monitor = !is.null(auto_resume_process),
        job_monitor_snapshot = TRUE,
        job_group_activity = TRUE,
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

  pr$handle("GET", "/jobs", function() {
    # The overview is intentionally metadata-only. Full status/configuration
    # inspection remains available through the individual job endpoints.
    ugplot_list_jobs(jobs_dir, lightweight = TRUE)
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
        paste(readLines(snapshot_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
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

  pr$handle("POST", "/jobs/<job_id>/resume", function(job_id, res) {
    tryCatch({
      started <- ugplot_resume_background_job(job_id, jobs_dir)
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
      started <- ugplot_start_background_job(dataset, config, jobs_dir)
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
