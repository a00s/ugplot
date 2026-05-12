ugplot_check_token <- function(req, token) {
  if (!nzchar(token)) {
    return(TRUE)
  }
  header_token <- req$HTTP_AUTHORIZATION %||% ""
  header_token <- sub("^Bearer[[:space:]]+", "", header_token, ignore.case = TRUE)
  identical(header_token, token)
}

ugplot_request_dataset <- function(req) {
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

#' Start a ugplot job server
#'
#' Starts an HTTP server that can receive datasets, run jobs in background R
#' processes, report progress, and return completed results.
#'
#' @param host Interface to bind. Use `"0.0.0.0"` for remote access.
#' @param port Port to listen on.
#' @param jobs_dir Directory used to persist datasets, status and results.
#' @param token Optional bearer token. If empty, requests are unauthenticated.
#' @return The plumber server result.
#' @export
ugPlotServer <- function(host = "127.0.0.1", port = 8080,
                         jobs_dir = ugplot_default_jobs_dir(),
                         token = Sys.getenv("UGPLOT_SERVER_TOKEN", unset = "")) {
  ugplot_assert_server_system_deps()
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Package 'plumber' is required to start ugPlotServer(). Run ugPlotInstallServerDeps().", call. = FALSE)
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required to start background jobs. Run ugPlotInstallServerDeps().", call. = FALSE)
  }

  ugplot_ensure_dir(jobs_dir)
  pr <- plumber::pr()

  pr$filter("auth", function(req, res) {
    if (!ugplot_check_token(req, token)) {
      res$status <- 401
      return(list(error = "Unauthorized"))
    }
    plumber::forward()
  })

  pr$handle("GET", "/health", function() {
    list(status = "ok", jobs_dir = jobs_dir)
  })

  pr$handle("GET", "/jobs", function() {
    ugplot_list_jobs(jobs_dir)
  })

  pr$handle("GET", "/jobs/<job_id>", function(job_id, res) {
    tryCatch(
      ugplot_read_job_status(job_id, jobs_dir),
      error = function(e) {
        res$status <- 404
        list(error = conditionMessage(e))
      }
    )
  })

  pr$handle("POST", "/jobs", function(req, res) {
    tryCatch({
      dataset <- ugplot_request_dataset(req)
      config <- ugplot_request_config(req)
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

  pr$run(host = host, port = port)
}
