ugplot_format_model_search_signal <- function(values, completed_runs = NULL, total_runs = NULL,
                                               metric_name = "R2") {
  values <- suppressWarnings(as.numeric(unlist(values, use.names = FALSE)))
  valid_runs <- sum(is.finite(values))

  completed_runs <- suppressWarnings(as.integer(completed_runs)[1])
  total_runs <- suppressWarnings(as.integer(total_runs)[1])
  if (!is.finite(completed_runs) || completed_runs < 0L) {
    completed_runs <- valid_runs
  }
  completed_runs <- max(completed_runs, valid_runs)

  attempt_label <- if (is.finite(total_runs) && total_runs > 0L) {
    paste0("model attempt ", completed_runs, "/", total_runs)
  } else {
    paste0(completed_runs, " model attempts processed")
  }
  result_label <- if (valid_runs == 1L) "valid result" else "valid results"

  paste0(
    "Model search: ", valid_runs, " ", result_label,
    " for ", metric_name, " | ", attempt_label
  )
}

ugplot_ml_safe_num <- function(value, fallback) {
  if (is.null(value) || length(value) == 0) {
    return(fallback)
  }
  parsed <- suppressWarnings(as.numeric(value))
  if (length(parsed) == 0 || is.na(parsed[[1]])) fallback else parsed[[1]]
}

ugplot_ml_seed_values <- function(start_value, end_value) {
  start_value <- suppressWarnings(as.integer(start_value))
  end_value <- suppressWarnings(as.integer(end_value))
  if (is.na(start_value) || is.na(end_value)) {
    return(1L)
  }
  seq(start_value, end_value)
}

ugplot_ml_cv_settings <- function(config) {
  performance_mode <- config$performance_mode %||% "default"
  switch(performance_mode,
    "high_effort" = list(method = "repeatedcv", number = 10, repeats = 3, tune_length = 10),
    "custom" = list(
      method = config$cv_method %||% "cv",
      number = max(2, ugplot_ml_safe_num(config$cv_folds, 10)),
      repeats = max(1, ugplot_ml_safe_num(config$cv_repeats, 1)),
      tune_length = max(1, ugplot_ml_safe_num(config$tune_length, 3))
    ),
    list(method = "cv", number = 10, repeats = 1, tune_length = 3)
  )
}

ugplot_ml_append_row <- function(results, row) {
  row <- as.data.frame(row, stringsAsFactors = FALSE)
  if (!is.data.frame(results) || nrow(results) == 0) {
    return(row)
  }
  all_columns <- union(names(results), names(row))
  for (column_name in setdiff(all_columns, names(results))) {
    results[[column_name]] <- NA
  }
  for (column_name in setdiff(all_columns, names(row))) {
    row[[column_name]] <- NA
  }
  rbind(results[, all_columns, drop = FALSE], row[, all_columns, drop = FALSE])
}

ugplot_ml_classify_error <- function(error_message) {
  if (grepl("wrong model type for (regression|classification)", error_message, ignore.case = TRUE)) {
    "INCOMPATIBLE"
  } else if (identical(error_message, "Stopping")) {
    "INVALID_METRICS"
  } else {
    "ERROR"
  }
}

ugplot_ml_process_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (length(pid) != 1L || is.na(pid) || pid <= 0L) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    output <- tryCatch(
      suppressWarnings(
        system2(
          "tasklist",
          c("/FI", shQuote(paste0("PID eq ", pid)), "/NH"),
          stdout = TRUE,
          stderr = FALSE
        )
      ),
      error = function(e) character(0)
    )
    return(any(grepl(paste0("\\b", pid, "\\b"), output)))
  }
  isTRUE(tryCatch(tools::pskill(pid, signal = 0), error = function(e) FALSE))
}

ugplot_ml_terminate_process <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (length(pid) != 1L || is.na(pid) || pid <= 0L || identical(pid, Sys.getpid())) {
    return(invisible(FALSE))
  }
  if (.Platform$OS.type == "windows") {
    try(
      system2(
        "taskkill", c("/PID", as.character(pid), "/T", "/F"),
        stdout = FALSE, stderr = FALSE
      ),
      silent = TRUE
    )
    return(invisible(TRUE))
  }
  try(tools::pskill(pid, signal = tools::SIGTERM), silent = TRUE)
  for (attempt in seq_len(10L)) {
    if (!ugplot_ml_process_alive(pid)) break
    Sys.sleep(0.05)
  }
  if (ugplot_ml_process_alive(pid)) {
    try(tools::pskill(pid, signal = tools::SIGKILL), silent = TRUE)
  }
  invisible(TRUE)
}

ugplot_ml_worker_registry_pids <- function(path) {
  if (is.null(path) || !nzchar(as.character(path)) || !file.exists(path)) {
    return(integer(0))
  }
  registry <- tryCatch(readRDS(path), error = function(e) NULL)
  pids <- if (is.list(registry)) registry$worker_pids else registry
  pids <- unique(suppressWarnings(as.integer(unlist(pids, use.names = FALSE))))
  pids[!is.na(pids) & pids > 0L]
}

ugplot_ml_process_has_worker_token <- function(pid, token) {
  token <- as.character(token %||% "")
  if (!nzchar(token)) return(FALSE)
  environ_path <- file.path("/proc", as.character(as.integer(pid)), "environ")
  if (!file.exists(environ_path)) {
    return(.Platform$OS.type != "unix")
  }
  bytes <- tryCatch(
    readBin(environ_path, what = "raw", n = 1024L * 1024L),
    error = function(e) raw(0)
  )
  if (length(bytes) == 0L) return(FALSE)
  boundaries <- c(0L, which(bytes == as.raw(0)), length(bytes) + 1L)
  entries <- vapply(seq_len(length(boundaries) - 1L), function(i) {
    start <- boundaries[[i]] + 1L
    end <- boundaries[[i + 1L]] - 1L
    if (end < start) "" else rawToChar(bytes[start:end])
  }, character(1))
  paste0("UGPLOT_WORKER_TOKEN=", token) %in% entries
}

ugplot_ml_cleanup_worker_registry <- function(path) {
  registry <- if (!is.null(path) && nzchar(as.character(path)) && file.exists(path)) {
    tryCatch(readRDS(path), error = function(e) NULL)
  } else {
    NULL
  }
  pids <- ugplot_ml_worker_registry_pids(path)
  token <- if (is.list(registry)) as.character(registry$worker_token %||% "") else ""
  verified <- pids[
    vapply(pids, function(pid) {
      ugplot_ml_process_alive(pid) &&
        ugplot_ml_process_has_worker_token(pid, token)
    }, logical(1))
  ]
  for (pid in verified) {
    ugplot_ml_terminate_process(pid)
  }
  if (!is.null(path) && nzchar(as.character(path))) {
    unlink(path, force = TRUE)
  }
  invisible(verified)
}

ugplot_ml_cleanup_worker_registries <- function(model_log_dir) {
  if (is.null(model_log_dir) || !nzchar(as.character(model_log_dir)) ||
      !dir.exists(model_log_dir)) {
    return(invisible(integer(0)))
  }
  paths <- list.files(
    model_log_dir,
    pattern = "[.]workers[.]rds$",
    full.names = TRUE,
    all.files = TRUE
  )
  cleaned <- unlist(lapply(paths, ugplot_ml_cleanup_worker_registry), use.names = FALSE)
  invisible(unique(suppressWarnings(as.integer(cleaned))))
}

ugplot_ml_train_with_timeout <- function(train_set, target_name, model_name, ctrl,
                                         tune_length, timeout, model_libraries,
                                         parallel_enabled = FALSE, cpu_limit = 1L,
                                         lib_paths = .libPaths(),
                                         heartbeat_callback = function(...) NULL,
                                         heartbeat_interval = 30,
                                         model_log_dir = NULL,
                                         run_key = NULL) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required to enforce remote model timeouts.", call. = FALSE)
  }

  heartbeat_callback(0, "starting isolated trainer")
  safe_run_name <- gsub("[^A-Za-z0-9_.-]+", "_", paste(c(model_name, run_key), collapse = "_"))
  stdout_path <- NULL
  stderr_path <- NULL
  worker_token <- paste(
    Sys.getpid(),
    format(Sys.time(), "%Y%m%d%H%M%OS6"),
    sample.int(.Machine$integer.max, 1L),
    sep = "-"
  )
  worker_registry_path <- tempfile(paste0("ugplot-", safe_run_name, "-"), fileext = ".workers.rds")
  if (!is.null(model_log_dir) && nzchar(as.character(model_log_dir))) {
    dir.create(model_log_dir, recursive = TRUE, showWarnings = FALSE)
    ugplot_ml_cleanup_worker_registries(model_log_dir)
    stdout_path <- file.path(model_log_dir, paste0(safe_run_name, ".stdout.log"))
    stderr_path <- file.path(model_log_dir, paste0(safe_run_name, ".stderr.log"))
    worker_registry_path <- file.path(model_log_dir, paste0(".", safe_run_name, ".workers.rds"))
    ugplot_ml_cleanup_worker_registry(worker_registry_path)
  }
  process <- callr::r_bg(
    func = function(train_set, target_name, model_name, ctrl, tune_length,
                    model_libraries, parallel_enabled, cpu_limit, lib_paths,
                    worker_registry_path, worker_token) {
      .libPaths(lib_paths)
      Sys.setenv(
        OMP_NUM_THREADS = cpu_limit,
        MKL_NUM_THREADS = cpu_limit,
        OPENBLAS_NUM_THREADS = cpu_limit,
        VECLIB_MAXIMUM_THREADS = cpu_limit,
        NUMEXPR_NUM_THREADS = cpu_limit,
        UGPLOT_CPU_LIMIT = cpu_limit,
        UGPLOT_WORKER_TOKEN = worker_token
      )
      for (lib in model_libraries) {
        suppressPackageStartupMessages(library(lib, character.only = TRUE))
      }

      cl <- NULL
      if (isTRUE(parallel_enabled) && cpu_limit > 1L) {
        cl <- parallel::makeCluster(cpu_limit)
        doParallel::registerDoParallel(cl)
        worker_pids <- unique(suppressWarnings(as.integer(unlist(
          parallel::clusterCall(cl, Sys.getpid),
          use.names = FALSE
        ))))
        saveRDS(
          list(
            trainer_pid = Sys.getpid(),
            worker_pids = worker_pids,
            worker_token = worker_token,
            registered_at = Sys.time()
          ),
          worker_registry_path
        )
      }
      on.exit({
        if (!is.null(cl)) {
          try(parallel::stopCluster(cl), silent = TRUE)
        }
        foreach::registerDoSEQ()
      }, add = TRUE)

      ctrl$allowParallel <- isTRUE(parallel_enabled)
      caret::train(
        stats::as.formula(paste(target_name, "~ .")),
        data = train_set,
        method = model_name,
        trControl = ctrl,
        tuneLength = tune_length
      )
    },
    args = list(
      train_set = train_set,
      target_name = target_name,
      model_name = model_name,
      ctrl = ctrl,
      tune_length = tune_length,
      model_libraries = model_libraries,
      parallel_enabled = isTRUE(parallel_enabled),
      cpu_limit = max(1L, as.integer(cpu_limit)),
      lib_paths = lib_paths,
      worker_registry_path = worker_registry_path,
      worker_token = worker_token
    ),
    stdout = stdout_path %||% NULL,
    stderr = stderr_path %||% NULL,
    poll_connection = FALSE,
    supervise = TRUE
  )
  heartbeat_callback(0, paste(
    "isolated trainer pid", process$get_pid(),
    if (!is.null(stdout_path)) paste("| stdout", basename(stdout_path)) else "",
    if (!is.null(stderr_path)) paste("| stderr", basename(stderr_path)) else ""
  ))
  on.exit({
    if (process$is_alive()) {
      try(process$kill_tree(), silent = TRUE)
      try(process$kill(), silent = TRUE)
    }
    ugplot_ml_cleanup_worker_registry(worker_registry_path)
  }, add = TRUE)

  started_at <- proc.time()[["elapsed"]]
  last_heartbeat <- started_at
  poll_interval <- 0.25
  repeat {
    if (!process$is_alive()) {
      return(tryCatch(process$get_result(), error = function(e) {
        log_tail <- function(path) {
          if (is.null(path) || !file.exists(path)) {
            return("")
          }
          lines <- tryCatch(utils::tail(readLines(path, warn = FALSE), 40), error = function(err) character(0))
          paste(lines, collapse = "\n")
        }
        stdout_tail <- log_tail(stdout_path)
        stderr_tail <- log_tail(stderr_path)
        detail <- paste(
          conditionMessage(e),
          if (nzchar(stdout_tail)) paste0("\n--- isolated stdout tail ---\n", stdout_tail) else "",
          if (nzchar(stderr_tail)) paste0("\n--- isolated stderr tail ---\n", stderr_tail) else ""
        )
        stop(simpleError(detail))
      }))
    }
    elapsed <- proc.time()[["elapsed"]] - started_at
    if (is.finite(elapsed) && (proc.time()[["elapsed"]] - last_heartbeat) >= heartbeat_interval) {
      heartbeat_callback(elapsed, "isolated trainer still running")
      last_heartbeat <- proc.time()[["elapsed"]]
    }
    if (is.finite(elapsed) && elapsed >= timeout) {
      try(process$kill_tree(), silent = TRUE)
      try(process$kill(), silent = TRUE)
      condition <- simpleError(paste0("Timed out after ", timeout, " seconds"))
      class(condition) <- c("callr_timeout_error", class(condition))
      stop(condition)
    }
    Sys.sleep(poll_interval)
  }
}

ugplot_ml_train_direct <- function(train_set, target_name, model_name, ctrl,
                                   tune_length, model_libraries,
                                   parallel_enabled = FALSE, cpu_limit = 1L) {
  cpu_limit <- suppressWarnings(as.integer(cpu_limit %||% 1L))
  if (is.na(cpu_limit) || cpu_limit < 1L) {
    cpu_limit <- 1L
  }
  Sys.setenv(
    OMP_NUM_THREADS = cpu_limit,
    MKL_NUM_THREADS = cpu_limit,
    OPENBLAS_NUM_THREADS = cpu_limit,
    VECLIB_MAXIMUM_THREADS = cpu_limit,
    NUMEXPR_NUM_THREADS = cpu_limit,
    UGPLOT_CPU_LIMIT = cpu_limit
  )
  for (lib in model_libraries) {
    suppressPackageStartupMessages(library(lib, character.only = TRUE))
  }
  cl <- NULL
  if (isTRUE(parallel_enabled) && cpu_limit > 1L) {
    cl <- tryCatch(
      parallel::makeCluster(cpu_limit),
      error = function(e) {
        warning("Could not start parallel workers: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (!is.null(cl)) {
      doParallel::registerDoParallel(cl)
    }
  }
  on.exit({
    if (!is.null(cl)) {
      try(parallel::stopCluster(cl), silent = TRUE)
    }
    foreach::registerDoSEQ()
  }, add = TRUE)

  ctrl$allowParallel <- !is.null(cl)
  caret::train(
    stats::as.formula(paste(target_name, "~ .")),
    data = train_set,
    method = model_name,
    trControl = ctrl,
    tuneLength = tune_length
  )
}

#' Run a ugplot machine learning job
#'
#' Executes a caret model search from a dataset and plain list configuration.
#' This runner is designed for remote/background jobs and avoids Shiny
#' dependencies so progress can be persisted by the job server.
#'
#' @param dataset Data frame containing target and predictor columns.
#' @param config List with target, models, seeds and preprocessing options.
#' @param progress_callback Function called with progress and message.
#' @param partial_callback Function called with recoverable partial results.
#' @return A list containing results, summary, best model and predictions.
ugplot_run_ml_job <- function(dataset, config = list(), progress_callback = function(...) NULL,
                              partial_callback = function(...) NULL) {
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data.frame.", call. = FALSE)
  }
  target_name <- config$target %||% config$target_name
  if (is.null(target_name) || !nzchar(target_name) || !(target_name %in% names(dataset))) {
    stop("config$target must name a column in dataset.", call. = FALSE)
  }

  cpu_limit <- max(1L, as.integer(config$cpu_limit %||% 1L))
  apply_runtime_thread_limit(cpu_limit)
  parallel_enabled <- isTRUE(config$parallel_enabled)
  use_callr_timeout <- !identical(config$use_callr_timeout, FALSE)
  restart_parallel_each_model <- isTRUE(config$restart_parallel_each_model)
  retry_parallel_connection_errors <- isTRUE(config$retry_parallel_connection_errors)
  timeout <- max(1, ugplot_ml_safe_num(config$timeout, 1200))
  skip_remaining_model_seeds_on_timeout <- !identical(
    config$skip_remaining_model_seeds_on_timeout,
    FALSE
  )

  models <- config$models %||% config$model_names %||% "lm"
  models <- unique(as.character(models))
  models <- models[nzchar(models)]
  if (length(models) == 0) {
    stop("config$models must contain at least one caret model.", call. = FALSE)
  }

  X_base <- dataset
  category_columns <- intersect(config$category_columns %||% character(0), names(X_base))
  for (column_name in category_columns) {
    X_base[[column_name]] <- as.factor(X_base[[column_name]])
    if (length(levels(X_base[[column_name]])) == 1) {
      X_base[[column_name]] <- as.numeric(rep(1, nrow(X_base)))
    }
  }
  if (target_name %in% category_columns) {
    X_base[[target_name]] <- as.factor(X_base[[target_name]])
    freq_table <- table(X_base[[target_name]])
    single_item_levels <- names(freq_table[freq_table <= 2])
    if (length(single_item_levels) > 0) {
      X_base <- X_base[!(X_base[[target_name]] %in% single_item_levels), , drop = FALSE]
      X_base[[target_name]] <- droplevels(X_base[[target_name]])
    }
  }

  dataset_seed_values <- ugplot_ml_seed_values(config$dataset_seed_start %||% 1, config$dataset_seed_end %||% 1)
  training_seed_values <- ugplot_ml_seed_values(config$training_seed_start %||% 1, config$training_seed_end %||% 1)
  total_runs <- max(1L, length(dataset_seed_values) * length(models) * length(training_seed_values))
  completed_runs <- 0L
  start_time <- proc.time()[["elapsed"]]

  best_result <- -Inf
  best_model_name <- "-"
  best_model_label <- "-"
  best_dataset_seed <- NA_integer_
  best_training_seed <- NA_integer_
  best_model_object <- NULL
  best_preprocess <- NULL
  best_mae <- NA_real_
  best_rmse <- NA_real_
  worst_result <- Inf
  worst_model <- "-"
  results <- data.frame()
  predictions <- list()
  metric_values <- list()
  mae_values <- list()
  rmse_values <- list()
  completed_run_keys <- character(0)
  timeout_skipped_models <- character(0)

  run_key <- function(model_name, dataset_seed, training_seed) {
    paste(as.character(model_name), as.character(dataset_seed), as.character(training_seed), sep = "\r")
  }

  restore_resume_result <- function(resume_result) {
    if (!is.list(resume_result) || !is.data.frame(resume_result$results_table) || nrow(resume_result$results_table) == 0) {
      return(invisible(FALSE))
    }
    resumed_results <- resume_result$results_table
    if (!all(c("Model", "dataset_seed", "training_seed") %in% names(resumed_results))) {
      return(invisible(FALSE))
    }

    resumed_results <- resumed_results[
      as.character(resumed_results$Model) %in% models &
        suppressWarnings(as.integer(resumed_results$dataset_seed)) %in% dataset_seed_values &
        suppressWarnings(as.integer(resumed_results$training_seed)) %in% training_seed_values,
      ,
      drop = FALSE
    ]
    if (nrow(resumed_results) == 0) {
      return(invisible(FALSE))
    }

    resumed_results$.resume_key <- run_key(
      resumed_results$Model,
      suppressWarnings(as.integer(resumed_results$dataset_seed)),
      suppressWarnings(as.integer(resumed_results$training_seed))
    )
    resumed_results <- resumed_results[!duplicated(resumed_results$.resume_key), , drop = FALSE]
    completed_run_keys <<- resumed_results$.resume_key
    resumed_results$.resume_key <- NULL
    results <<- resumed_results
    completed_runs <<- min(length(completed_run_keys), total_runs)

    if (isTRUE(skip_remaining_model_seeds_on_timeout) && "Status" %in% names(results)) {
      timed_out_rows <- as.character(results$Status) == "TIMEOUT"
      timeout_skipped_models <<- unique(as.character(results$Model[timed_out_rows]))
      timeout_skipped_models <<- timeout_skipped_models[
        nzchar(timeout_skipped_models) & !is.na(timeout_skipped_models)
      ]
    }

    ok_rows <- if ("Status" %in% names(results)) as.character(results$Status) == "OK" else rep(TRUE, nrow(results))
    for (row_index in which(ok_rows)) {
      model_name <- as.character(results$Model[[row_index]])
      metric <- if ("R2" %in% names(results)) {
        suppressWarnings(as.numeric(results$R2[[row_index]]))
      } else if ("Accuracy" %in% names(results)) {
        suppressWarnings(as.numeric(results$Accuracy[[row_index]]))
      } else {
        NA_real_
      }
      if (is.finite(metric)) {
        metric_values[[model_name]] <<- c(metric_values[[model_name]], metric)
        if ("MAE" %in% names(results)) {
          mae <- suppressWarnings(as.numeric(results$MAE[[row_index]]))
          if (is.finite(mae)) {
            mae_values[[model_name]] <<- c(mae_values[[model_name]], mae)
          }
        }
        if ("RMSE" %in% names(results)) {
          rmse <- suppressWarnings(as.numeric(results$RMSE[[row_index]]))
          if (is.finite(rmse)) {
            rmse_values[[model_name]] <<- c(rmse_values[[model_name]], rmse)
          }
        }
        if (metric > best_result) {
          best_result <<- metric
          best_model_name <<- model_name
          best_dataset_seed <<- suppressWarnings(as.integer(results$dataset_seed[[row_index]]))
          best_training_seed <<- suppressWarnings(as.integer(results$training_seed[[row_index]]))
          best_model_label <<- paste(model_name, "(", best_dataset_seed, ":", best_training_seed, ")")
          if ("MAE" %in% names(results)) {
            best_mae <<- suppressWarnings(as.numeric(results$MAE[[row_index]]))
          }
          if ("RMSE" %in% names(results)) {
            best_rmse <<- suppressWarnings(as.numeric(results$RMSE[[row_index]]))
          }
        }
        if (metric < worst_result) {
          worst_result <<- metric
          worst_model <<- paste(model_name, "(", results$dataset_seed[[row_index]], ":", results$training_seed[[row_index]], ")")
        }
      }
    }

    best_model_object <<- resume_result$best_model %||% NULL
    best_preprocess <<- resume_result$best_model_preprocess %||% NULL
    if (is.list(resume_result$predictions)) {
      predictions <<- resume_result$predictions
    }
    invisible(TRUE)
  }

  current_result <- function(partial = FALSE) {
    metric_name <- if (is.factor(X_base[[target_name]])) "Accuracy" else "R2"
    robust_stats_rows <- lapply(models, function(model_name) {
      values <- metric_values[[model_name]]
      values <- values[is.finite(values)]
      data.frame(
        Model = model_name,
        MeanMetric = if (length(values) > 0) round(mean(values), 4) else NA_real_,
        MedianMetric = if (length(values) > 0) round(stats::median(values), 4) else NA_real_,
        IQRMetric = if (length(values) > 1) round(stats::IQR(values), 4) else NA_real_,
        MinMetric = if (length(values) > 0) round(min(values), 4) else NA_real_,
        MaxMetric = if (length(values) > 0) round(max(values), 4) else NA_real_,
        RangeMetric = if (length(values) > 1) round(diff(range(values)), 4) else NA_real_,
        stringsAsFactors = FALSE
      )
    })
    robust_stats <- do.call(rbind, robust_stats_rows)
    if (is.data.frame(robust_stats) && nrow(robust_stats) > 0) {
      robust_stats <- robust_stats[order(-robust_stats$MedianMetric, robust_stats$Model), , drop = FALSE]
    }

    best_model_metrics <- metric_values[[best_model_name]]
    best_model_metrics <- best_model_metrics[is.finite(best_model_metrics)]
    best_model_mae <- mae_values[[best_model_name]]
    best_model_mae <- best_model_mae[is.finite(best_model_mae)]
    best_model_rmse <- rmse_values[[best_model_name]]
    best_model_rmse <- best_model_rmse[is.finite(best_model_rmse)]
    status_values <- if ("Status" %in% names(results)) as.character(results$Status) else character(0)
    final_summary <- list(
      best_model = best_model_name,
      best_model_label = best_model_label,
      dataset_seed = if (!is.na(best_dataset_seed)) best_dataset_seed else "N/A",
      training_seed = if (!is.na(best_training_seed)) best_training_seed else "N/A",
      metric_name = metric_name,
      metric_value = if (is.finite(best_result)) best_result else NA_real_,
      best_model_min = if (length(best_model_metrics) > 0) round(min(best_model_metrics), 4) else "N/A",
      best_model_max = if (length(best_model_metrics) > 0) round(max(best_model_metrics), 4) else "N/A",
      best_model_mean = if (length(best_model_metrics) > 0) round(mean(best_model_metrics), 4) else "N/A",
      best_model_median = if (length(best_model_metrics) > 0) round(stats::median(best_model_metrics), 4) else "N/A",
      best_model_iqr = if (length(best_model_metrics) > 1) round(stats::IQR(best_model_metrics), 4) else "N/A",
      best_model_range = if (length(best_model_metrics) > 1) round(diff(range(best_model_metrics)), 4) else "N/A",
      best_model_mae_median = if (length(best_model_mae) > 0) round(stats::median(best_model_mae), 4) else "N/A",
      best_model_mae_iqr = if (length(best_model_mae) > 1) round(stats::IQR(best_model_mae), 4) else "N/A",
      best_model_rmse_median = if (length(best_model_rmse) > 0) round(stats::median(best_model_rmse), 4) else "N/A",
      best_model_rmse_iqr = if (length(best_model_rmse) > 1) round(stats::IQR(best_model_rmse), 4) else "N/A",
      mae = if (identical(metric_name, "R2")) best_mae else NA_real_,
      rmse = if (identical(metric_name, "R2")) best_rmse else NA_real_,
      total_elapsed_seconds = round(proc.time()[["elapsed"]] - start_time, 3),
      completed_runs = completed_runs,
      total_runs = total_runs,
      ok_runs = sum(status_values == "OK", na.rm = TRUE),
      timeout_runs = sum(status_values == "TIMEOUT", na.rm = TRUE),
      skipped_timeout_runs = sum(status_values == "SKIPPED_TIMEOUT", na.rm = TRUE),
      incompatible_runs = sum(status_values == "INCOMPATIBLE", na.rm = TRUE),
      invalid_metric_runs = sum(status_values == "INVALID_METRICS", na.rm = TRUE),
      error_runs = sum(status_values == "ERROR", na.rm = TRUE),
      model_robust_stats = robust_stats
    )

    list(
      results_table = results,
      final_summary = final_summary,
      best_model = best_model_object,
      best_model_name = best_model_name,
      best_model_preprocess = best_preprocess,
      predictions = predictions,
      partial = isTRUE(partial),
      updated_at = as.character(Sys.time())
    )
  }

  resume_result <- config$resume_result %||% NULL
  resume_result_path <- config$resume_result_path %||% ""
  if (is.null(resume_result) && nzchar(resume_result_path) && file.exists(resume_result_path)) {
    resume_result <- tryCatch(readRDS(resume_result_path), error = function(e) NULL)
  }
  resume_completed_keys <- unique(as.character(config$resume_completed_keys %||% character(0)))
  resume_completed_keys <- resume_completed_keys[nzchar(resume_completed_keys) & !is.na(resume_completed_keys)]
  restored_resume <- restore_resume_result(resume_result)
  if (!isTRUE(restored_resume) && length(resume_completed_keys) > 0) {
    completed_run_keys <- resume_completed_keys
    completed_runs <- min(length(completed_run_keys), total_runs)
    progress_callback(
      progress = completed_runs / total_runs,
      message = paste("Resuming after", completed_runs, "completed run keys")
    )
  }
  if (isTRUE(restored_resume)) {
    progress_callback(
      progress = completed_runs / total_runs,
      message = paste("Resuming after", completed_runs, "completed runs")
    )
    partial_callback(current_result(partial = TRUE))
  }
  cv_settings <- ugplot_ml_cv_settings(config)
  missing_definition <- config$missing_definition %||% c("empty", "na")
  zero_exceptions <- config$zero_exceptions %||% character(0)
  missing_strategy <- config$missing_strategy %||% "none"
  imputation_scope <- config$imputation_scope %||% "split_separate"
  missing_filter_order <- config$missing_filter_order %||% "cols_first"
  missing_filter_order <- normalize_missing_filter_order(missing_filter_order, allow_auto = TRUE)
  complete_case_min_samples <- suppressWarnings(as.numeric(config$complete_case_min_samples %||% 80)) / 100
  if (!is.finite(complete_case_min_samples)) {
    complete_case_min_samples <- 0.8
  }
  threshold_scope <- "full_before_split"

  for (dataset_position in seq_along(dataset_seed_values)) {
    dataset_seed <- dataset_seed_values[[dataset_position]]
    set.seed(dataset_seed)
    X <- X_base
    Y <- X[[target_name]]
    preprocess_meta <- NULL

    predictors_all <- X[, setdiff(colnames(X), target_name), drop = FALSE]
    filtered_all <- apply_missing_filters_resolved(
      predictors = predictors_all,
      missing_definition = missing_definition,
      zero_exceptions = zero_exceptions,
      threshold_cols = config$missing_threshold_cols %||% 100,
      threshold_rows = config$missing_threshold_rows %||% 100,
      filter_order = missing_filter_order,
      min_rows_retained = complete_case_min_samples,
      mode = if (identical(missing_strategy, "none")) "complete_case" else "balanced"
    )
    X <- cbind(X[filtered_all$keep_rows, target_name, drop = FALSE], filtered_all$filtered_predictors)
    names(X)[1] <- target_name
    Y <- X[[target_name]]

    if (identical(imputation_scope, "full_once") && !identical(missing_strategy, "none")) {
      preprocessed_full <- apply_missing_strategy(
        trainSet = X,
        testSet = X[0, , drop = FALSE],
        target_name = target_name,
        strategy = missing_strategy,
        missing_definition = missing_definition,
        zero_exceptions = zero_exceptions,
        threshold_cols = config$missing_threshold_cols %||% 100,
        threshold_rows = config$missing_threshold_rows %||% 100,
        threshold_scope = "full_before_split",
        filter_order = missing_filter_order,
        min_rows_retained = complete_case_min_samples
      )
      X <- preprocessed_full$train_set
      Y <- X[[target_name]]
      preprocess_meta <- preprocessed_full$preprocess_meta
    }

    if (nrow(X) < 5 || length(unique(Y)) < 2) {
      results <- ugplot_ml_append_row(results, data.frame(
        Model = paste(models, collapse = ","),
        Status = "ERROR",
        Error = "Not enough data after preprocessing",
        dataset_seed = dataset_seed,
        training_seed = NA_integer_,
        threshold_scope = threshold_scope,
        imputation_scope = imputation_scope
      ))
      completed_runs <- completed_runs + length(models) * length(training_seed_values)
      progress_callback(progress = completed_runs / total_runs, message = "Not enough data after preprocessing")
      next
    }

    train_index <- caret::createDataPartition(Y, p = .8, list = FALSE, times = 1)
    train_set <- X[train_index, , drop = FALSE]
    test_set <- X[-train_index, , drop = FALSE]
    strategy_after_split <- if (identical(imputation_scope, "full_once")) "none" else missing_strategy
    if (!identical(strategy_after_split, "none")) {
      processed_data <- apply_missing_strategy(
        trainSet = train_set,
        testSet = test_set,
        target_name = target_name,
        strategy = strategy_after_split,
        missing_definition = missing_definition,
        zero_exceptions = zero_exceptions,
        threshold_cols = config$missing_threshold_cols %||% 100,
        threshold_rows = config$missing_threshold_rows %||% 100,
        threshold_scope = threshold_scope,
        filter_order = missing_filter_order,
        min_rows_retained = complete_case_min_samples
      )
      train_set <- processed_data$train_set
      test_set <- processed_data$test_set
      preprocess_meta <- processed_data$preprocess_meta
    }

    for (model_index in seq_along(models)) {
      model_name <- models[[model_index]]
      model_info <- tryCatch(caret::getModelInfo(model_name, regex = FALSE)[[model_name]], error = function(e) NULL)
      if (is.null(model_info)) {
        results <- ugplot_ml_append_row(results, data.frame(
          Model = model_name,
          Status = "ERROR",
          Error = "Unknown caret model",
          dataset_seed = dataset_seed,
          training_seed = NA_integer_,
          threshold_scope = threshold_scope,
          imputation_scope = imputation_scope
        ))
        completed_runs <- completed_runs + length(training_seed_values)
        progress_callback(progress = completed_runs / total_runs, message = paste("Unknown caret model:", model_name))
        next
      }
      for (lib in model_info$library) {
        suppressPackageStartupMessages(library(lib, character.only = TRUE))
      }
      ctrl <- if (identical(cv_settings$method, "repeatedcv")) {
        caret::trainControl(method = "repeatedcv", number = cv_settings$number, repeats = cv_settings$repeats)
      } else {
        caret::trainControl(method = "cv", number = cv_settings$number)
      }
      ctrl$allowParallel <- parallel_enabled

      for (seed_position in seq_along(training_seed_values)) {
        training_seed <- training_seed_values[[seed_position]]
        current_run_key <- run_key(model_name, dataset_seed, training_seed)
        if (current_run_key %in% completed_run_keys) {
          progress_callback(
            progress = completed_runs / total_runs,
            message = paste("Skipping completed", model_name, "dataset seed", dataset_seed, "training seed", training_seed)
          )
          next
        }
        if (isTRUE(skip_remaining_model_seeds_on_timeout) && model_name %in% timeout_skipped_models) {
          results <- ugplot_ml_append_row(results, data.frame(
            Model = model_name,
            Status = "SKIPPED_TIMEOUT",
            elapsed_seconds = 0,
            Error = "Skipped because another seed for this model timed out",
            dataset_seed = dataset_seed,
            training_seed = training_seed,
            threshold_scope = threshold_scope,
            imputation_scope = imputation_scope
          ))
          completed_runs <- completed_runs + 1L
          completed_run_keys <- c(completed_run_keys, current_run_key)
          progress_callback(
            progress = completed_runs / total_runs,
            message = paste(
              "Skipping", model_name,
              "dataset seed", dataset_seed,
              "training seed", training_seed,
              "because an earlier seed timed out"
            )
          )
          partial_callback(current_result(partial = TRUE))
          next
        }
        set.seed(training_seed)
        attempt_start <- proc.time()[["elapsed"]]
        progress_callback(
          progress = completed_runs / total_runs,
          message = paste("Running", model_name, "dataset seed", dataset_seed, "training seed", training_seed),
          current_run = list(
            key = current_run_key,
            model = model_name,
            dataset_seed = dataset_seed,
            training_seed = training_seed,
            timeout_seconds = timeout,
            started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
          )
        )

        run_status <- "OK"
        run_error <- ""
        model <- tryCatch({
          train_once <- function() {
            if (isTRUE(use_callr_timeout)) {
              ugplot_ml_train_with_timeout(
                train_set = train_set,
                target_name = target_name,
                model_name = model_name,
                ctrl = ctrl,
                tune_length = cv_settings$tune_length,
                timeout = timeout,
                model_libraries = model_info$library,
                parallel_enabled = parallel_enabled,
                cpu_limit = cpu_limit,
                lib_paths = .libPaths(),
                model_log_dir = config$model_log_dir %||% NULL,
                run_key = current_run_key,
                heartbeat_callback = function(elapsed, phase = "isolated trainer still running") {
                  progress_callback(
                    progress = completed_runs / total_runs,
                    message = paste(
                      "Running", model_name,
                      "dataset seed", dataset_seed,
                      "training seed", training_seed,
                      "-", phase,
                      "- elapsed", round(elapsed),
                      "/ timeout", timeout, "seconds"
                    )
                  )
                }
              )
            } else {
              ugplot_ml_train_direct(
                train_set = train_set,
                target_name = target_name,
                model_name = model_name,
                ctrl = ctrl,
                tune_length = cv_settings$tune_length,
                model_libraries = model_info$library,
                parallel_enabled = parallel_enabled,
                cpu_limit = cpu_limit
              )
            }
          }
          tryCatch(train_once(), error = function(e) {
            connection_error <- grepl("error (writing|reading) to connection|serialize|unserialize|SOCK", conditionMessage(e), ignore.case = TRUE)
            if (parallel_enabled && retry_parallel_connection_errors && connection_error) {
              return(train_once())
            }
            stop(e)
          })
        }, callr_timeout_error = function(e) {
          run_status <<- "TIMEOUT"
          run_error <<- paste0("Timed out after ", timeout, " seconds")
          NULL
        }, TimeoutException = function(e) {
          run_status <<- "TIMEOUT"
          run_error <<- paste0("Timed out after ", timeout, " seconds")
          NULL
        }, error = function(e) {
          run_status <<- ugplot_ml_classify_error(conditionMessage(e))
          run_error <<- conditionMessage(e)
          NULL
        })

        if (!is.null(model)) {
          pred <- tryCatch(stats::predict(model, newdata = test_set), error = function(e) {
            run_status <<- "ERROR"
            run_error <<- conditionMessage(e)
            NULL
          })
          if (!is.null(pred)) {
            actual <- test_set[[target_name]]
            if (is.factor(actual)) {
              metric <- sum(pred == actual) / length(pred)
              metric_values[[model_name]] <- c(metric_values[[model_name]], metric)
              results <- ugplot_ml_append_row(results, data.frame(
                Model = model_name,
                Status = "OK",
                elapsed_seconds = round(proc.time()[["elapsed"]] - attempt_start, 3),
                Accuracy = metric,
                Error = "",
                dataset_seed = dataset_seed,
                training_seed = training_seed,
                threshold_scope = threshold_scope,
                imputation_scope = imputation_scope
              ))
            } else {
              sampled <- caret::postResample(pred, actual)
              metric <- unname(sampled["Rsquared"])
              mae <- unname(sampled["MAE"])
              rmse <- unname(sampled["RMSE"])
              if (is.na(metric) || is.na(mae) || is.na(rmse)) {
                run_status <- "INVALID_METRICS"
                run_error <- "Regression metrics returned NA"
                metric <- NA_real_
              } else {
                metric_values[[model_name]] <- c(metric_values[[model_name]], metric)
                mae_values[[model_name]] <- c(mae_values[[model_name]], mae)
                rmse_values[[model_name]] <- c(rmse_values[[model_name]], rmse)
                results <- ugplot_ml_append_row(results, data.frame(
                  Model = model_name,
                  Status = "OK",
                  elapsed_seconds = round(proc.time()[["elapsed"]] - attempt_start, 3),
                  R2 = metric,
                  MAE = mae,
                  RMSE = rmse,
                  Error = "",
                  dataset_seed = dataset_seed,
                  training_seed = training_seed,
                  threshold_scope = threshold_scope,
                  imputation_scope = imputation_scope
                ))
              }
            }
            if (is.finite(metric) && metric > best_result) {
              best_result <- metric
              best_model_name <- model_name
              best_model_label <- paste(model_name, "(", dataset_seed, ":", training_seed, ")")
              best_dataset_seed <- dataset_seed
              best_training_seed <- training_seed
              best_model_object <- model
              best_preprocess <- preprocess_meta
              if (exists("mae", inherits = FALSE)) {
                best_mae <- mae
              }
              if (exists("rmse", inherits = FALSE)) {
                best_rmse <- rmse
              }
            }
            if (is.finite(metric) && metric < worst_result) {
              worst_result <- metric
              worst_model <- paste(model_name, "(", dataset_seed, ":", training_seed, ")")
            }
            predictions[[model_name]] <- data.frame(Actual = actual, Predicted = pred)
          }
        }

        if (is.null(model) || !identical(run_status, "OK")) {
          results <- ugplot_ml_append_row(results, data.frame(
            Model = model_name,
            Status = run_status,
            elapsed_seconds = round(proc.time()[["elapsed"]] - attempt_start, 3),
            Error = run_error,
            dataset_seed = dataset_seed,
            training_seed = training_seed,
            threshold_scope = threshold_scope,
            imputation_scope = imputation_scope
          ))
        }
        if (identical(run_status, "TIMEOUT") && isTRUE(skip_remaining_model_seeds_on_timeout)) {
          timeout_skipped_models <- union(timeout_skipped_models, model_name)
        }
        completed_runs <- completed_runs + 1L
        completed_run_keys <- c(completed_run_keys, current_run_key)
        progress_callback(progress = completed_runs / total_runs, message = paste("Finished", model_name))
        partial_callback(current_result(partial = TRUE))
        progress_callback(current_run = list(clear = TRUE))
        rm(model)
        gc(verbose = FALSE)
      }
    }
  }

  result <- current_result(partial = FALSE)
  result$finished_at <- as.character(Sys.time())
  result
}
