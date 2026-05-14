local_ml_runner_env <- function() {
  local_env <- new.env(parent = globalenv())
  local_env$`%||%` <- function(lhs, rhs) {
    if (is.null(lhs)) rhs else lhs
  }
  local_env$apply_runtime_thread_limit <- function(cpu_limit) invisible(cpu_limit)
  local_env$apply_missing_filters <- function(predictors, ...) {
    list(
      keep_rows = rep(TRUE, nrow(predictors)),
      filtered_predictors = predictors
    )
  }
  local_env$apply_missing_strategy <- function(trainSet, testSet, ...) {
    list(
      train_set = trainSet,
      test_set = testSet,
      preprocess_meta = list(strategy = "none")
    )
  }
  ml_runner_path <- file.path("R", "ml_runner.R")
  if (!file.exists(ml_runner_path)) {
    ml_runner_path <- file.path("..", "..", "R", "ml_runner.R")
  }
  sys.source(ml_runner_path, envir = local_env)
  local_env
}

sample_ml_data <- function() {
  sample_path <- file.path("inst", "extdata", "sample.csv")
  if (!file.exists(sample_path)) {
    sample_path <- system.file("extdata", "sample.csv", package = "ugplot")
  }
  data <- utils::read.csv(sample_path, check.names = FALSE)
  rownames(data) <- data[[1]]
  data <- data[, -1, drop = FALSE]
  utils::head(data, 40)
}

test_that("remote ML runner trains a model on sample data", {
  local_env <- local_ml_runner_env()
  result <- local_env$ugplot_run_ml_job(
    dataset = sample_ml_data(),
    config = list(
      target = "age",
      models = "lm",
      dataset_seed_start = 1,
      dataset_seed_end = 1,
      training_seed_start = 1,
      training_seed_end = 1,
      timeout = 30,
      performance_mode = "custom",
      cv_method = "cv",
      cv_folds = 2,
      tune_length = 1,
      cpu_limit = 1
    )
  )

  expect_s3_class(result$best_model, "train")
  expect_equal(result$final_summary$total_runs, 1)
  expect_equal(result$final_summary$ok_runs, 1)
  expect_equal(result$results_table$Status, "OK")
})

test_that("remote ML runner timeout skips one model and continues", {
  local_env <- local_ml_runner_env()
  timed_out <- FALSE
  local_env$ugplot_ml_train_with_timeout <- function(train_set, target_name, model_name,
                                                     ctrl, tune_length, timeout,
                                                     model_libraries, ...) {
    if (identical(model_name, "lm") && !timed_out) {
      timed_out <<- TRUE
      condition <- simpleError("callr timed out")
      class(condition) <- c("callr_timeout_error", class(condition))
      stop(condition)
    }
    suppressWarnings(
      caret::train(
        stats::as.formula(paste(target_name, "~ .")),
        data = train_set,
        method = model_name,
        trControl = ctrl,
        tuneLength = tune_length
      )
    )
  }

  result <- local_env$ugplot_run_ml_job(
    dataset = sample_ml_data(),
    config = list(
      target = "age",
      models = c("lm", "glm"),
      dataset_seed_start = 1,
      dataset_seed_end = 1,
      training_seed_start = 1,
      training_seed_end = 1,
      timeout = 1,
      performance_mode = "custom",
      cv_method = "cv",
      cv_folds = 2,
      tune_length = 1,
      cpu_limit = 1
    )
  )

  expect_true("TIMEOUT" %in% result$results_table$Status)
  expect_true("OK" %in% result$results_table$Status)
  expect_equal(result$final_summary$timeout_runs, 1)
  expect_equal(result$final_summary$ok_runs, 1)
})
