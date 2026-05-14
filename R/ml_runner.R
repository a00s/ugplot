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
  restart_parallel_each_model <- isTRUE(config$restart_parallel_each_model)
  retry_parallel_connection_errors <- isTRUE(config$retry_parallel_connection_errors)
  timeout <- max(1, ugplot_ml_safe_num(config$timeout, 1200))

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

  cl <- NULL
  start_cluster <- function() {
    stop_cluster()
    cl <<- parallel::makeCluster(cpu_limit)
    doParallel::registerDoParallel(cl)
    invisible(cl)
  }
  stop_cluster <- function() {
    if (!is.null(cl)) {
      try(parallel::stopCluster(cl), silent = TRUE)
      cl <<- NULL
    }
    foreach::registerDoSEQ()
    invisible(NULL)
  }
  if (parallel_enabled && !restart_parallel_each_model) {
    start_cluster()
  }
  on.exit(stop_cluster(), add = TRUE)

  cv_settings <- ugplot_ml_cv_settings(config)
  missing_definition <- config$missing_definition %||% c("empty", "na")
  zero_exceptions <- config$zero_exceptions %||% character(0)
  missing_strategy <- config$missing_strategy %||% "none"
  imputation_scope <- config$imputation_scope %||% "split_separate"
  threshold_scope <- "full_before_split"

  for (dataset_position in seq_along(dataset_seed_values)) {
    dataset_seed <- dataset_seed_values[[dataset_position]]
    set.seed(dataset_seed)
    X <- X_base
    Y <- X[[target_name]]
    preprocess_meta <- NULL

    predictors_all <- X[, setdiff(colnames(X), target_name), drop = FALSE]
    filtered_all <- apply_missing_filters(
      predictors = predictors_all,
      missing_definition = missing_definition,
      zero_exceptions = zero_exceptions,
      threshold_cols = config$missing_threshold_cols %||% 100,
      threshold_rows = config$missing_threshold_rows %||% 100
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
        threshold_scope = "full_before_split"
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
        threshold_scope = threshold_scope
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
        set.seed(training_seed)
        attempt_start <- proc.time()[["elapsed"]]
        progress_callback(
          progress = completed_runs / total_runs,
          message = paste("Running", model_name, "dataset seed", dataset_seed, "training seed", training_seed)
        )

        run_status <- "OK"
        run_error <- ""
        model <- tryCatch({
          train_once <- function() {
            if (parallel_enabled && restart_parallel_each_model && isTRUE(ctrl$allowParallel)) {
              start_cluster()
              on.exit(stop_cluster(), add = TRUE)
            }
            R.utils::withTimeout(
              caret::train(
                stats::as.formula(paste(target_name, "~ .")),
                data = train_set,
                method = model_name,
                trControl = ctrl,
                tuneLength = cv_settings$tune_length
              ),
              timeout = timeout,
              onTimeout = "error"
            )
          }
          tryCatch(train_once(), error = function(e) {
            connection_error <- grepl("error (writing|reading) to connection|serialize|unserialize|SOCK", conditionMessage(e), ignore.case = TRUE)
            if (parallel_enabled && retry_parallel_connection_errors && connection_error) {
              stop_cluster()
              if (!restart_parallel_each_model) {
                start_cluster()
              }
              return(train_once())
            }
            stop(e)
          })
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
        completed_runs <- completed_runs + 1L
        progress_callback(progress = completed_runs / total_runs, message = paste("Finished", model_name))
        partial_callback(current_result(partial = TRUE))
      }
    }
  }

  result <- current_result(partial = FALSE)
  result$finished_at <- as.character(Sys.time())
  result
}
