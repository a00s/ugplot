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

ugplot_remote_create_job <- function(server_url, dataset, config, token = "") {
  request <- ugplot_remote_request(server_url, "jobs", token)
  dataset_file <- tempfile(fileext = ".rds")
  config_file <- tempfile(fileext = ".rds")
  saveRDS(dataset, dataset_file)
  saveRDS(config, config_file)
  on.exit(unlink(c(dataset_file, config_file)), add = TRUE)
  response <- httr::POST(
    request$url,
    request$headers,
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

ugplot_remote_health <- function(server_url, token = "") {
  request <- ugplot_remote_request(server_url, "health", token)
  response <- httr::GET(request$url, request$headers)
  ugplot_remote_parse(response)
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

ugplot_remote_job_status <- function(server_url, job_id, token = "") {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id), token)
  response <- httr::GET(request$url, request$headers)
  ugplot_remote_parse(response)
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

ugplot_remote_geo_threshold_summary <- function(server_url, job_id, threshold,
                                                transcript_min_samples = 80,
                                                spearman_min_samples_pct = 80,
                                                token = "") {
  query <- paste0(
    "threshold=", utils::URLencode(as.character(threshold), reserved = TRUE),
    "&transcript_min_samples=", utils::URLencode(as.character(transcript_min_samples), reserved = TRUE),
    "&spearman_min_samples_pct=", utils::URLencode(as.character(spearman_min_samples_pct), reserved = TRUE)
  )
  request <- ugplot_remote_request(
    server_url,
    paste0("jobs/", job_id, "/geo-threshold-summary?", query),
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

ugplot_remote_resume_job <- function(server_url, job_id, token = "") {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id, "/resume"), token)
  response <- httr::POST(request$url, request$headers)
  ugplot_remote_parse(response)
}

ugplot_remote_delete_job <- function(server_url, job_id, token = "", force = FALSE) {
  path <- paste0("jobs/", job_id)
  if (isTRUE(force)) {
    path <- paste0(path, "?force=true")
  }
  request <- ugplot_remote_request(server_url, path, token)
  response <- httr::DELETE(request$url, request$headers)
  ugplot_remote_parse(response)
}

ugplot_remote_get_result <- function(server_url, job_id, token = "") {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id, "/result-rds"), token)
  response <- httr::GET(request$url, request$headers)
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

ugplot_remote_get_job_preview <- function(server_url, job_id, token = "") {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id, "/preview-rds"), token)
  response <- httr::GET(request$url, request$headers)
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
