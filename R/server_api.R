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
  internal_runners <- c("ugplot_run_geo_screen_group_job")
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
  ugplot_assert_server_system_deps()
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Package 'plumber' is required to start ugPlotServer(). Run ugPlotInstallServerDeps().", call. = FALSE)
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required to start background jobs. Run ugPlotInstallServerDeps().", call. = FALSE)
  }

  ugplot_ensure_dir(jobs_dir)
  source_dir <- if (file.exists(file.path(getwd(), "R", "app.R"))) normalizePath(getwd(), mustWork = FALSE) else NULL
  auto_resume_process <- ugplot_start_auto_resume_monitor(
    jobs_dir = jobs_dir,
    interval = auto_resume_interval,
    source_dir = source_dir,
    lib_paths = .libPaths()
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
    if (grepl("^/collaboration(/|$)", request_path)) {
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

  pr$handle("POST", "/collaboration/<task_id>/heartbeat", function(task_id, req, res) {
    tryCatch({
      body <- ugplot_request_json_body(req)
      ugplot_collaboration_heartbeat(
        task_id, body$lease_id %||% "", body$client_id %||% "",
        lease_seconds = 120, jobs_dir = jobs_dir
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

  pr$handle("GET", "/health", function() {
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
      resources = ugplot_server_resource_snapshot(jobs_dir),
      capabilities = list(
        delete_job = TRUE,
        resume_job = TRUE,
        auto_resume_monitor = !is.null(auto_resume_process),
        job_bundle = TRUE,
        job_preview = TRUE,
        job_config_summary = TRUE,
        job_resource_monitor = !is.null(auto_resume_process),
        server_resources = TRUE,
        geo_pipeline = TRUE,
        geo_cpg_summary = TRUE,
        geo_cpg_lookup = TRUE,
        distributed_geo_screening = TRUE,
        distributed_protocol_version = 1L
      )
    )
  })

  pr$handle("GET", "/jobs", function() {
    ugplot_auto_resume_crashed_jobs(jobs_dir)
    ugplot_list_jobs(jobs_dir)
  })

  pr$handle("GET", "/models/dependencies", function() {
    ugplot_model_dependency_status()
  })

  pr$handle("GET", "/jobs/<job_id>", function(job_id, res) {
    tryCatch(
      {
        ugplot_auto_resume_crashed_jobs(jobs_dir)
        ugplot_read_job_status(job_id, jobs_dir)
      },
      error = function(e) {
        res$status <- 404
        list(error = conditionMessage(e))
      }
    )
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
