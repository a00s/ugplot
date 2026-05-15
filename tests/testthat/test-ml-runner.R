local_ml_runner_env <- function() {
  list(
    ugplot_run_ml_job = ugplot_test_internal("ugplot_run_ml_job")
  )
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
  ugplot_test_local_namespace_binding("ugplot_ml_train_with_timeout", function(train_set, target_name, model_name,
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
  })

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
