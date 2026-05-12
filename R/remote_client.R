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
  httr::content(response, as = "parsed", simplifyVector = FALSE)
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
    body = list(
      dataset = httr::upload_file(dataset_file, type = "application/octet-stream"),
      config = httr::upload_file(config_file, type = "application/octet-stream")
    ),
    encode = "multipart"
  )
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

ugplot_remote_job_status <- function(server_url, job_id, token = "") {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id), token)
  response <- httr::GET(request$url, request$headers)
  ugplot_remote_parse(response)
}

ugplot_remote_get_result <- function(server_url, job_id, token = "") {
  request <- ugplot_remote_request(server_url, paste0("jobs/", job_id, "/result-rds"), token)
  response <- httr::GET(request$url, request$headers)
  parsed <- ugplot_remote_parse(response)
  if (is.null(parsed$content_base64)) {
    stop("Remote result response did not include RDS content.", call. = FALSE)
  }
  result_file <- tempfile(fileext = ".rds")
  raw_result <- base64enc::base64decode(parsed$content_base64)
  writeBin(raw_result, result_file)
  on.exit(unlink(result_file), add = TRUE)
  readRDS(result_file)
}
