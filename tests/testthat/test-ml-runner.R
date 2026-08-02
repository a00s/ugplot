local_ml_runner_env <- function() {
  list(
    ugplot_run_ml_job = ugplot_test_internal("ugplot_run_ml_job")
  )
}

test_that("model search progress is not presented as seed stability", {
  format_signal <- ugplot_test_internal("ugplot_format_model_search_signal")

  signal <- format_signal(
    values = c(0.12, NA, 0.24, Inf),
    completed_runs = 16,
    total_runs = 199,
    metric_name = "R2"
  )

  expect_equal(
    signal,
    "Model search: 2 valid results for R2 | model attempt 16/199"
  )
  expect_false(grepl("Stability|seed", signal, ignore.case = TRUE))
})

test_that("ML worker registries terminate residual processes", {
  skip_on_os("windows")
  skip_if_not(dir.exists("/proc/self"))
  skip_if_not_installed("callr")
  cleanup_registry <- ugplot_test_internal("ugplot_ml_cleanup_worker_registry")
  registry_path <- tempfile("ugplot-worker-registry-", fileext = ".workers.rds")
  worker_token <- paste0("test-worker-", Sys.getpid())
  worker <- callr::r_bg(
    function() Sys.sleep(60),
    supervise = FALSE,
    cleanup = FALSE,
    env = c(UGPLOT_WORKER_TOKEN = worker_token)
  )
  withr::defer({
    if (worker$is_alive()) worker$kill()
    unlink(registry_path, force = TRUE)
  })
  saveRDS(
    list(
      trainer_pid = Sys.getpid(),
      worker_pids = worker$get_pid(),
      worker_token = worker_token,
      registered_at = Sys.time()
    ),
    registry_path
  )

  cleaned <- cleanup_registry(registry_path)
  worker$wait(timeout = 3000)

  expect_equal(cleaned, worker$get_pid())
  expect_false(worker$is_alive())
  expect_false(file.exists(registry_path))
})

test_that("ML worker cleanup does not terminate a reused or unrelated PID", {
  skip_on_os("windows")
  skip_if_not(dir.exists("/proc/self"))
  skip_if_not_installed("callr")
  cleanup_registry <- ugplot_test_internal("ugplot_ml_cleanup_worker_registry")
  registry_path <- tempfile("ugplot-unrelated-registry-", fileext = ".workers.rds")
  worker <- callr::r_bg(
    function() Sys.sleep(60),
    supervise = FALSE,
    cleanup = FALSE,
    env = c(UGPLOT_WORKER_TOKEN = "actual-token")
  )
  withr::defer({
    if (worker$is_alive()) worker$kill()
    unlink(registry_path, force = TRUE)
  })
  saveRDS(
    list(
      worker_pids = worker$get_pid(),
      worker_token = "stale-token",
      registered_at = Sys.time()
    ),
    registry_path
  )

  expect_length(cleanup_registry(registry_path), 0L)
  expect_true(worker$is_alive())
  expect_false(file.exists(registry_path))
})

test_that("ML worker registry cleanup removes stale and invalid registries", {
  cleanup_registries <- ugplot_test_internal("ugplot_ml_cleanup_worker_registries")
  model_log_dir <- tempfile("ugplot-model-logs-")
  dir.create(model_log_dir)
  withr::defer(unlink(model_log_dir, recursive = TRUE, force = TRUE))
  saveRDS(
    list(worker_pids = c(NA_integer_, -1L)),
    file.path(model_log_dir, ".finished.workers.rds")
  )
  writeLines(
    "interrupted registry write",
    file.path(model_log_dir, ".invalid.workers.rds")
  )

  expect_length(cleanup_registries(model_log_dir), 0L)
  expect_length(
    list.files(model_log_dir, pattern = "[.]workers[.]rds$", all.files = TRUE),
    0L
  )
})

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
  skip_on_os("windows")
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

test_that("class metadata predictors participate in numeric-target prediction", {
  skip_on_os("windows")
  local_env <- local_ml_runner_env()
  set.seed(7)
  stage <- rep(c("II", "III", "IV", "II"), 10)
  cpg <- stats::runif(40)
  dataset <- data.frame(
    target = 20 + 5 * (stage == "III") + 11 * (stage == "IV") + 3 * cpg,
    stage = stage,
    cg1 = cpg,
    stringsAsFactors = FALSE
  )

  result <- local_env$ugplot_run_ml_job(
    dataset = dataset,
    config = list(
      target = "target", category_columns = "stage", models = "lm",
      dataset_seed_start = 1, dataset_seed_end = 1,
      training_seed_start = 1, training_seed_end = 1,
      timeout = 30, performance_mode = "custom",
      cv_method = "cv", cv_folds = 2, tune_length = 1, cpu_limit = 1
    )
  )

  expect_equal(result$results_table$Status, "OK")
  expect_true(is.factor(result$best_model$trainingData$stage))
  expect_gt(result$results_table$R2, 0.9)
})

test_that("remote ML runner timeout skips remaining seeds for that model and continues", {
  skip_on_os("windows")
  local_env <- local_ml_runner_env()
  timed_out <- FALSE
  train_calls <- character(0)
  ugplot_test_local_namespace_binding("ugplot_ml_train_with_timeout", function(train_set, target_name, model_name,
                                                                               ctrl, tune_length, timeout,
                                                                               model_libraries, ...) {
    train_calls <<- c(train_calls, model_name)
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
      training_seed_end = 3,
      timeout = 1,
      performance_mode = "custom",
      cv_method = "cv",
      cv_folds = 2,
      tune_length = 1,
      cpu_limit = 1
    )
  )

  expect_true("TIMEOUT" %in% result$results_table$Status)
  expect_true("SKIPPED_TIMEOUT" %in% result$results_table$Status)
  expect_true("OK" %in% result$results_table$Status)
  expect_equal(result$final_summary$timeout_runs, 1)
  expect_equal(result$final_summary$skipped_timeout_runs, 2)
  expect_equal(result$final_summary$ok_runs, 3)
  expect_equal(sum(train_calls == "lm"), 1)
  expect_equal(sum(train_calls == "glm"), 3)
  expect_equal(result$final_summary$completed_runs, 6)
})

test_that("remote ML runner preserves timeout model skips on resume", {
  skip_on_os("windows")
  local_env <- local_ml_runner_env()
  train_calls <- character(0)
  ugplot_test_local_namespace_binding("ugplot_ml_train_direct", function(train_set, target_name, model_name,
                                                                         ctrl, tune_length, model_libraries, ...) {
    train_calls <<- c(train_calls, model_name)
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
      training_seed_end = 3,
      timeout = 1,
      performance_mode = "custom",
      cv_method = "cv",
      cv_folds = 2,
      tune_length = 1,
      cpu_limit = 1,
      use_callr_timeout = FALSE,
      resume_result = list(
        results_table = data.frame(
          Model = "lm",
          Status = "TIMEOUT",
          elapsed_seconds = 1,
          Error = "Timed out after 1 seconds",
          dataset_seed = 1L,
          training_seed = 1L,
          threshold_scope = "full_before_split",
          imputation_scope = "split_separate",
          stringsAsFactors = FALSE
        )
      )
    )
  )

  expect_equal(sum(train_calls == "lm"), 0)
  expect_equal(sum(train_calls == "glm"), 3)
  expect_equal(result$final_summary$timeout_runs, 1)
  expect_equal(result$final_summary$skipped_timeout_runs, 2)
  expect_equal(result$final_summary$ok_runs, 3)
  expect_equal(result$final_summary$completed_runs, 6)
})

test_that("remote ML runner resumes after completed partial rows", {
  skip_on_os("windows")
  local_env <- local_ml_runner_env()
  train_calls <- 0L
  ugplot_test_local_namespace_binding("ugplot_ml_train_direct", function(train_set, target_name, model_name,
                                                                         ctrl, tune_length, model_libraries, ...) {
    train_calls <<- train_calls + 1L
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
      models = "lm",
      dataset_seed_start = 1,
      dataset_seed_end = 2,
      training_seed_start = 1,
      training_seed_end = 1,
      timeout = 30,
      performance_mode = "custom",
      cv_method = "cv",
      cv_folds = 2,
      tune_length = 1,
      cpu_limit = 1,
      use_callr_timeout = FALSE,
      resume_result = list(
        results_table = data.frame(
          Model = "lm",
          Status = "OK",
          elapsed_seconds = 0.1,
          R2 = 0.1,
          MAE = 1,
          RMSE = 1,
          Error = "",
          dataset_seed = 1L,
          training_seed = 1L,
          threshold_scope = "full_before_split",
          imputation_scope = "split_separate",
          stringsAsFactors = FALSE
        )
      )
    )
  )

  expect_equal(train_calls, 1L)
  expect_equal(nrow(result$results_table), 2L)
  expect_equal(result$final_summary$completed_runs, 2L)
})

test_that("remote ML runner can resume from completed run keys", {
  skip_on_os("windows")
  local_env <- local_ml_runner_env()
  train_calls <- 0L
  ugplot_test_local_namespace_binding("ugplot_ml_train_direct", function(train_set, target_name, model_name,
                                                                         ctrl, tune_length, model_libraries, ...) {
    train_calls <<- train_calls + 1L
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
      models = "lm",
      dataset_seed_start = 1,
      dataset_seed_end = 2,
      training_seed_start = 1,
      training_seed_end = 1,
      timeout = 30,
      performance_mode = "custom",
      cv_method = "cv",
      cv_folds = 2,
      tune_length = 1,
      cpu_limit = 1,
      use_callr_timeout = FALSE,
      resume_completed_keys = paste("lm", "1", "1", sep = "\r")
    )
  )

  expect_equal(train_calls, 1L)
  expect_equal(nrow(result$results_table), 1L)
  expect_equal(result$final_summary$completed_runs, 2L)
})
