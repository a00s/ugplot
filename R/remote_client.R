ugplot_remote_url <- function(server_url, path = "") {
  server_url <- sub("/+$", "", server_url)
  path <- sub("^/+", "", path)
  if (!nzchar(path)) {
    return(server_url)
  }
  paste(server_url, path, sep = "/")
}

ugplot_remote_request <- function(server_url, path, token = "") {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for remote ugplot jobs. Run ugPlotInstallServerDeps().", call. = FALSE)
  }
  request_url <- ugplot_remote_url(server_url, path)
  headers <- if (nzchar(token)) httr::add_headers(Authorization = paste("Bearer", token)) else NULL
  list(url = request_url, headers = headers)
}

ugplot_remote_parse <- function(response) {
  if (httr::http_error(response)) {
    body <- tryCatch(httr::content(response, as = "parsed", simplifyVector = FALSE), error = function(e) NULL)
    message <- if (is.list(body) && !is.null(body$error)) body$error else httr::http_status(response)$message
    stop(message, call. = FALSE)
  }
  httr::content(response, as = "parsed", simplifyVector = TRUE)
}

ugplot_remote_create_job <- function(server_url, dataset, config, token = "", timeout_seconds = 600) {
  request <- ugplot_remote_request(server_url, "jobs", token)
  dataset_file <- tempfile(fileext = ".rds")
  config_file <- tempfile(fileext = ".rds")
  saveRDS(dataset, dataset_file)
  saveRDS(config, config_file)
  on.exit(unlink(c(dataset_file, config_file)), add = TRUE)
  response <- httr::POST(
    request$url,
    request$headers,
    httr::timeout(timeout_seconds),
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(
        dataset_rds_base64 = base64enc::base64encode(dataset_file),
        config_rds_base64 = base64enc::base64encode(config_file)
      ),
      auto_unbox = TRUE
    ),
    encode = "raw"
  )
  ugplot_remote_parse(response)
}

ugplot_remote_health <- function(server_url, token = "", timeout_seconds = 15,
                                 include_resources = TRUE) {
  health_path <- if (isTRUE(include_resources)) "health" else "health?resources=false"
  request <- ugplot_remote_request(server_url, health_path, token)
  response <- httr::GET(request$url, request$headers, httr::timeout(timeout_seconds))
  ugplot_remote_parse(response)
}

ugplot_collaboration_post_json <- function(server_url, path, body, timeout_seconds = 60) {
  request <- ugplot_remote_request(server_url, path, token = "")
  response <- httr::POST(
    request$url,
    httr::timeout(timeout_seconds),
    httr::content_type_json(),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
    encode = "raw"
  )
  ugplot_remote_parse(response)
}

ugplot_remote_decode_rds_base64 <- function(value) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  writeBin(base64enc::base64decode(value), path)
  readRDS(path)
}

ugplot_remote_store_rds_base64 <- function(value, path = tempfile(fileext = ".rds")) {
  writeBin(base64enc::base64decode(value), path)
  path
}

ugplot_remote_collaboration_status <- function(server_url, timeout_seconds = 15) {
  request <- ugplot_remote_request(server_url, "collaboration", token = "")
  ugplot_remote_parse(httr::GET(request$url, httr::timeout(timeout_seconds)))
}

ugplot_remote_collaboration_claim <- function(server_url, client_id, capabilities, timeout_seconds = 60) {
  parsed <- ugplot_collaboration_post_json(
    server_url, "collaboration/claim",
    list(client_id = client_id, capabilities = capabilities), timeout_seconds
  )
  if (is.null(parsed$task) || is.null(parsed$payload_rds_base64)) return(NULL)
  list(
    task = parsed$task,
    payload_path = ugplot_remote_store_rds_base64(parsed$payload_rds_base64)
  )
}

ugplot_remote_collaboration_compatibility <- function(server_url, capabilities, timeout_seconds = 30) {
  ugplot_collaboration_post_json(
    server_url, "collaboration/compatibility",
    list(capabilities = capabilities), timeout_seconds
  )
}

ugplot_remote_collaboration_heartbeat <- function(server_url, task_id, lease_id, client_id,
                                                  telemetry = list()) {
  ugplot_collaboration_post_json(
    server_url, paste0("collaboration/", task_id, "/heartbeat"),
    list(lease_id = lease_id, client_id = client_id, telemetry = telemetry)
  )
}

ugplot_remote_collaboration_release <- function(server_url, task_id, lease_id, client_id) {
  ugplot_collaboration_post_json(
    server_url, paste0("collaboration/", task_id, "/release"),
    list(lease_id = lease_id, client_id = client_id)
  )
}

ugplot_remote_collaboration_complete <- function(server_url, task_id, lease_id, client_id, result,
                                                 timeout_seconds = 600) {
  result_path <- tempfile(fileext = ".rds")
  on.exit(unlink(result_path), add = TRUE)
  saveRDS(result, result_path)
  ugplot_collaboration_post_json(
    server_url, paste0("collaboration/", task_id, "/complete"),
    list(
      lease_id = lease_id,
      client_id = client_id,
      result_rds_base64 = base64enc::base64encode(result_path)
    ),
    timeout_seconds
  )
}

ugplot_remote_list_jobs <- function(server_url, token = "") {
  request <- ugplot_remote_request(server_url, "jobs", token)
  response <- httr::GET(request$url, request$headers)
  parsed <- ugplot_remote_parse(response)
  if (is.data.frame(parsed)) {
    return(parsed)
  }
  if (is.list(parsed) && length(parsed) > 0) {
    return(as.data.frame(parsed, stringsAsFactors = FALSE))
  }
  data.frame()
}

ugplot_remote_model_deps <- function(server_url, token = "") {
  request <- ugplot_remote_request(server_url, "models/dependencies", token)
  response <- httr::GET(request$url, request$headers)
  ugplot_remote_parse(response)
}

ugplot_remote_job_status <- function(server_url, job_id, token = "", timeout_seconds = 15) {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id), token)
  response <- httr::GET(request$url, request$headers, httr::timeout(timeout_seconds))
  ugplot_remote_parse(response)
}

ugplot_remote_job_monitor <- function(server_url, job_id, token = "", include_groups = TRUE,
                                      resource_lines = 60L, timeout_seconds = 6) {
  request <- ugplot_remote_request(
    server_url,
    paste0(
      "jobs/", job_id, "/monitor?groups=", if (isTRUE(include_groups)) "true" else "false",
      "&resource_lines=", as.integer(resource_lines)
    ),
    token
  )
  parsed <- ugplot_remote_parse(
    httr::GET(request$url, request$headers, httr::timeout(timeout_seconds))
  )
  resources <- parsed$resources %||% data.frame()
  if (is.list(resources) && !is.data.frame(resources) && length(resources) > 0L) {
    resources <- as.data.frame(resources, stringsAsFactors = FALSE)
  }
  parsed$resources <- resources
  groups <- parsed$group_activity$groups %||% data.frame()
  if (is.list(groups) && !is.data.frame(groups) && length(groups) > 0L) {
    groups <- as.data.frame(groups, stringsAsFactors = FALSE)
  }
  parsed$group_activity <- parsed$group_activity %||% list()
  parsed$group_activity$groups <- groups
  parsed
}

ugplot_remote_distributed_summary <- function(status) {
  status <- status %||% list()
  distributed <- status$distributed_state %||% list()
  number <- function(value, default = NA_real_) {
    value <- suppressWarnings(as.numeric(value %||% default))
    if (length(value) == 0L || !is.finite(value[[1]])) default else value[[1]]
  }
  completed <- number(distributed$completed, NA_real_)
  total <- number(distributed$total, NA_real_)
  processing <- number(distributed$active, NA_real_)
  active_groups <- as.character(distributed$active_groups %||% character(0))
  active_groups <- trimws(active_groups[nzchar(trimws(active_groups))])

  message <- as.character(status$message %||% "")
  if ((!is.finite(completed) || !is.finite(total)) && nzchar(message)) {
    match <- regexec(
      "Distributed screening: *([0-9]+)/([0-9]+) group\\(s\\); *active *(.*)$",
      message,
      perl = TRUE,
      ignore.case = TRUE
    )
    pieces <- regmatches(message, match)[[1]]
    if (length(pieces) >= 4L) {
      completed <- suppressWarnings(as.numeric(pieces[[2]]))
      total <- suppressWarnings(as.numeric(pieces[[3]]))
      active_text <- trimws(pieces[[4]])
      if (!tolower(active_text) %in% c("", "waiting", "none")) {
        active_groups <- trimws(strsplit(active_text, ",", fixed = TRUE)[[1]])
        active_groups <- active_groups[nzchar(active_groups)]
      }
    }
  }
  if (!is.finite(processing)) processing <- length(active_groups)
  pending <- if (is.finite(total) && is.finite(completed)) {
    max(0, total - completed - processing)
  } else {
    NA_real_
  }
  list(
    completed = completed,
    total = total,
    processing = processing,
    pending = pending,
    active_groups = active_groups
  )
}

ugplot_remote_job_log <- function(server_url, job_id, token = "", max_lines = 200L) {
  request <- ugplot_remote_request(
    server_url,
    paste0("jobs/", job_id, "/log?max_lines=", as.integer(max_lines)),
    token
  )
  response <- httr::GET(request$url, request$headers)
  parsed <- ugplot_remote_parse(response)
  as.character(parsed$log %||% "")
}

ugplot_remote_job_resources <- function(server_url, job_id, token = "", max_lines = 500L) {
  request <- ugplot_remote_request(
    server_url,
    paste0("jobs/", job_id, "/resources?max_lines=", as.integer(max_lines)),
    token
  )
  response <- httr::GET(request$url, request$headers)
  parsed <- ugplot_remote_parse(response)
  resources <- parsed$resources %||% data.frame()
  if (is.data.frame(resources)) {
    return(resources)
  }
  if (is.list(resources) && length(resources) > 0) {
    return(as.data.frame(resources, stringsAsFactors = FALSE))
  }
  data.frame()
}

ugplot_remote_job_groups <- function(server_url, job_id, token = "", timeout_seconds = 15) {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id, "/groups"), token)
  parsed <- ugplot_remote_parse(httr::GET(request$url, request$headers, httr::timeout(timeout_seconds)))
  groups <- parsed$groups %||% data.frame()
  if (is.list(groups) && !is.data.frame(groups) && length(groups) > 0L) {
    groups <- as.data.frame(groups, stringsAsFactors = FALSE)
  }
  parsed$groups <- groups
  parsed
}

ugplot_remote_job_discoveries <- function(server_url, job_id, timeout_seconds = 15) {
  request <- ugplot_remote_request(server_url, paste0("reports/", job_id, "/data"), token = "")
  ugplot_remote_parse(httr::GET(request$url, httr::timeout(timeout_seconds)))
}

ugplot_remote_geo_cpg_summary <- function(server_url, job_id, threshold,
                                          spearman_min_samples_pct = 80,
                                          bin_width = 0.05,
                                          token = "") {
  query <- paste0(
    "threshold=", utils::URLencode(as.character(threshold), reserved = TRUE),
    "&spearman_min_samples_pct=", utils::URLencode(as.character(spearman_min_samples_pct), reserved = TRUE),
    "&bin_width=", utils::URLencode(as.character(bin_width), reserved = TRUE)
  )
  request <- ugplot_remote_request(
    server_url,
    paste0("jobs/", job_id, "/geo-cpg-summary?", query),
    token
  )
  response <- httr::GET(request$url, request$headers)
  ugplot_remote_parse(response)
}

ugplot_remote_geo_cpg_lookup <- function(server_url, job_id, cpg, threshold,
                                         spearman_min_samples_pct = 80,
                                         token = "") {
  query <- paste0(
    "cpg=", utils::URLencode(as.character(cpg), reserved = TRUE),
    "&threshold=", utils::URLencode(as.character(threshold), reserved = TRUE),
    "&spearman_min_samples_pct=", utils::URLencode(as.character(spearman_min_samples_pct), reserved = TRUE)
  )
  request <- ugplot_remote_request(
    server_url,
    paste0("jobs/", job_id, "/geo-cpg-lookup?", query),
    token
  )
  response <- httr::GET(request$url, request$headers)
  ugplot_remote_parse(response)
}

ugplot_remote_stop_job <- function(server_url, job_id, token = "") {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id, "/stop"), token)
  response <- httr::POST(request$url, request$headers)
  ugplot_remote_parse(response)
}

ugplot_remote_drain_job <- function(server_url, job_id, token = "") {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id, "/drain"), token)
  response <- httr::POST(request$url, request$headers)
  ugplot_remote_parse(response)
}

ugplot_remote_resume_job <- function(server_url, job_id, token = "", timeout_seconds = 30) {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id, "/resume"), token)
  response <- httr::POST(request$url, request$headers, httr::timeout(timeout_seconds))
  ugplot_remote_parse(response)
}

ugplot_remote_delete_job <- function(server_url, job_id, token = "", force = FALSE,
                                     timeout_seconds = 30) {
  path <- paste0("jobs/", job_id)
  if (isTRUE(force)) {
    path <- paste0(path, "?force=true")
  }
  request <- ugplot_remote_request(server_url, path, token)
  response <- httr::DELETE(request$url, request$headers, httr::timeout(timeout_seconds))
  ugplot_remote_parse(response)
}

ugplot_remote_get_result <- function(server_url, job_id, token = "", timeout_seconds = 600) {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id, "/result-rds"), token)
  response <- httr::GET(request$url, request$headers, httr::timeout(timeout_seconds))
  parsed <- ugplot_remote_parse(response)
  content_base64 <- parsed$content_base64
  content_base64 <- as.character(unlist(content_base64, use.names = FALSE))
  content_base64 <- content_base64[nzchar(content_base64)]
  if (length(content_base64) == 0) {
    stop("Remote result response did not include RDS content.", call. = FALSE)
  }
  result_file <- tempfile(fileext = ".rds")
  raw_result <- base64enc::base64decode(content_base64[[1]])
  writeBin(raw_result, result_file)
  on.exit(unlink(result_file), add = TRUE)
  readRDS(result_file)
}

ugplot_remote_get_job_preview <- function(server_url, job_id, token = "", timeout_seconds = 60) {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id, "/preview-rds"), token)
  response <- httr::GET(request$url, request$headers, httr::timeout(timeout_seconds))
  parsed <- ugplot_remote_parse(response)
  content_base64 <- parsed$content_base64
  content_base64 <- as.character(unlist(content_base64, use.names = FALSE))
  content_base64 <- content_base64[nzchar(content_base64)]
  if (length(content_base64) == 0) {
    stop("Remote preview response did not include RDS content.", call. = FALSE)
  }
  preview_file <- tempfile(fileext = ".rds")
  raw_preview <- base64enc::base64decode(content_base64[[1]])
  writeBin(raw_preview, preview_file)
  on.exit(unlink(preview_file), add = TRUE)
  readRDS(preview_file)
}

ugplot_remote_get_job_bundle <- function(server_url, job_id, token = "") {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id, "/bundle-rds"), token)
  response <- httr::GET(request$url, request$headers)
  parsed <- ugplot_remote_parse(response)
  content_base64 <- parsed$content_base64
  content_base64 <- as.character(unlist(content_base64, use.names = FALSE))
  content_base64 <- content_base64[nzchar(content_base64)]
  if (length(content_base64) == 0) {
    stop("Remote job bundle response did not include RDS content.", call. = FALSE)
  }
  bundle_file <- tempfile(fileext = ".rds")
  raw_bundle <- base64enc::base64decode(content_base64[[1]])
  writeBin(raw_bundle, bundle_file)
  on.exit(unlink(bundle_file), add = TRUE)
  readRDS(bundle_file)
}
