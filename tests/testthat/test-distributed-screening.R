test_that("remote runner allowlist blocks arbitrary functions", {
  validate <- ugplot_test_internal("ugplot_validate_remote_job_config")

  expect_equal(
    validate(list(
      runner = "ugplot_run_geo_pipeline_job",
      accession = "GSE87571",
      job_dir = "/tmp/attacker",
      resume_result_path = "/etc/secret.rds"
    ))$runner,
    "ugplot_run_geo_pipeline_job"
  )
  sanitized <- validate(list(
    runner = "ugplot_run_geo_pipeline_job",
    accession = "gse87571",
    job_dir = "/tmp/attacker",
    resume_result_path = "/etc/secret.rds"
  ))
  expect_equal(sanitized$accession, "GSE87571")
  expect_null(sanitized$job_dir)
  expect_null(sanitized$resume_result_path)
  expect_equal(
    validate(list(
      runner = "ugplot_run_geo_complete_group_job",
      internal_worker_task = TRUE
    ))$runner,
    "ugplot_run_geo_complete_group_job"
  )
  expect_equal(
    validate(list(
      runner = "ugplot_run_geo_screen_group_job",
      internal_worker_task = TRUE
    ))$runner,
    "ugplot_run_geo_screen_group_job"
  )
  expect_error(
    validate(list(runner = "system")),
    "not allowed"
  )
  expect_error(
    validate(list(runner = "ugplot_run_geo_screen_group_job")),
    "not allowed"
  )
  expect_error(
    validate(list(runner = "ugplot_run_geo_pipeline_job", accession = "https://attacker.invalid")),
    "require an accession"
  )
})

test_that("distributed telemetry exposes the last status from every active server", {
  summarize_tasks <- ugplot_test_internal("ugplot_geo_distributed_active_tasks")
  group_from_task <- ugplot_test_internal("ugplot_geo_collaboration_group_from_task_id")
  manifest <- data.frame(
    GroupID = c("TG1", "TG2", "TG3"),
    Worker = c("Fy2", "Fy3", ""),
    JobID = c("worker-1", "worker-2", ""),
    State = c("running", "submitted", "pending"),
    Progress = c(0.37, 0.08, 0),
    Message = c("Training ranger", "Downloading mission", "Waiting for worker"),
    UpdatedAt = c("now", "now", ""),
    Attempts = c(1L, 1L, 0L),
    PollFailures = c(0L, 0L, 0L),
    Error = c("", "", ""),
    stringsAsFactors = FALSE
  )
  tasks <- summarize_tasks(manifest)
  expect_length(tasks, 2L)
  expect_equal(vapply(tasks, `[[`, character(1), "worker"), c("Fy2", "Fy3"))
  expect_equal(vapply(tasks, `[[`, numeric(1), "progress"), c(0.37, 0.08))
  expect_equal(vapply(tasks, `[[`, character(1), "message"), c("Training ranger", "Downloading mission"))
  expect_equal(group_from_task("parent:analyze:TG17", "parent"), "TG17")
  expect_equal(group_from_task("parent:screen:TG18", "parent"), "TG18")
  expect_equal(group_from_task("another:analyze:TG1", "parent"), "")
})

test_that("local transcript analysis completes stability before the next group", {
  root <- tempfile("local-complete-groups-")
  dir.create(root)
  dataset_paths <- vapply(c("TG1", "TG2"), function(group_id) {
    path <- file.path(root, paste0(group_id, ".csv"))
    utils::write.csv(
      data.frame(sample_id = c("S1", "S2"), target = c(1, 2), cg1 = c(0.2, 0.8)),
      path, row.names = FALSE
    )
    path
  }, character(1))
  groups <- data.frame(
    GroupID = c("TG1", "TG2"), TriggerMaxAbsRho = c(0.9, 0.8),
    DatasetPath = unname(dataset_paths), stringsAsFactors = FALSE
  )
  order <- character(0)
  ugplot_test_local_namespace_binding("ugplot_geo_screen_group", function(
      dataset, group, source, config, screen_path, importance_path,
      progress_callback = NULL, partial_callback = NULL) {
    group_id <- as.character(group$GroupID[[1]])
    order <<- c(order, paste0("screen-", group_id))
    list(
      summary = data.frame(
        GroupID = group_id, BestModel = "lm", Phase = "screening",
        PrincipalTranscript = paste0("ENST-", group_id),
        TriggerMaxAbsRho = as.numeric(group$TriggerMaxAbsRho[[1]]),
        BestMetric = 0.8, MedianMetric = 0.8,
        DatasetPath = config$coordinator_dataset_path,
        ScreenResultPath = screen_path, ImportancePath = "",
        stringsAsFactors = FALSE
      ),
      screen_result = list(best_model_name = "lm"), importance = data.frame()
    )
  })
  ugplot_test_local_namespace_binding("ugplot_geo_run_transcript_stability_remote", function(
      screen_summary, cache_dir, source = "processed", config = list(), metadata = NULL,
      progress_callback = NULL, partial_callback = NULL) {
    group_id <- as.character(screen_summary$GroupID[[1]])
    order <<- c(order, paste0("stability-", group_id))
    data.frame(GroupID = group_id, Phase = "stability", Stable = TRUE)
  })
  ugplot_test_local_namespace_binding("ugplot_geo_enrich_ml_summary_remote", function(summary, ...) summary)

  run_local <- ugplot_test_internal("ugplot_geo_run_transcript_ml_remote")
  result <- run_local(
    groups, cache_dir = root, source = "raw_sesame",
    config = list(models = "lm", geo_ml_screen_seeds = 1L)
  )

  expect_equal(result$GroupID, c("TG1", "TG2"))
  expect_equal(order, c("screen-TG1", "stability-TG1", "screen-TG2", "stability-TG2"))
})

test_that("transcript group datasets load RDS checkpoints without CSV corruption", {
  load_group <- ugplot_test_internal("ugplot_geo_ml_group_dataset")
  dataset_path <- tempfile("transcript-group-", fileext = ".rds")
  dataset <- data.frame(
    sample_id = c("S1", "S2"),
    target = c(10, 20),
    cg00000001 = c(0.1, 0.9),
    check.names = FALSE
  )
  saveRDS(dataset, dataset_path)

  loaded <- load_group(
    data.frame(DatasetPath = dataset_path, stringsAsFactors = FALSE),
    keep_sample_id = TRUE
  )

  expect_identical(loaded$dataset, dataset)
  expect_equal(loaded$sample_count, 2L)
})

test_that("stability stops after a seed batch with no valid metrics", {
  run_stability <- ugplot_test_internal("ugplot_geo_run_transcript_stability_remote")
  cache_dir <- tempfile("stability-no-valid-")
  dir.create(cache_dir)
  dataset_path <- file.path(cache_dir, "TG1.rds")
  saveRDS(
    data.frame(target = 1:6, cg00000001 = seq(0.1, 0.6, by = 0.1)),
    dataset_path
  )
  calls <- 0L
  ugplot_test_local_namespace_binding("ugplot_run_ml_job", function(...) {
    calls <<- calls + 1L
    list(
      results_table = data.frame(
        Model = rep("gbm", 2),
        dataset_seed = rep(1L, 2),
        training_seed = 1:2,
        Status = rep("ERROR", 2),
        Error = rep("Please use column names for x", 2),
        stringsAsFactors = FALSE
      ),
      final_summary = list(metric_value = NA_real_),
      best_model = NULL
    )
  })

  expect_error(
    run_stability(
      screen_summary = data.frame(
        GroupID = "TG1",
        BestModel = "gbm",
        DatasetPath = dataset_path,
        stringsAsFactors = FALSE
      ),
      cache_dir = cache_dir,
      config = list(
        geo_ml_min_stability_seeds = 2L,
        geo_ml_max_stability_seeds = 4L,
        geo_ml_stability_window = 2L
      )
    ),
    "produced no valid metrics.*ERROR=2.*Please use column names for x"
  )
  expect_equal(calls, 1L)
})

test_that("distributed GEO checkpoints produce a durable model timing summary", {
  collect <- ugplot_test_internal("ugplot_geo_collect_model_timing")
  pipeline_dir <- tempfile("model-timing-")
  dir.create(file.path(pipeline_dir, "TG1"), recursive = TRUE)
  dir.create(file.path(pipeline_dir, "TG2", "stability_by", "sex", "F"), recursive = TRUE)
  saveRDS(
    list(results_table = data.frame(
      Model = c("gbm", "glmnet"),
      Status = c("TIMEOUT", "OK"),
      elapsed_seconds = c(1200, 18),
      stringsAsFactors = FALSE
    )),
    file.path(pipeline_dir, "TG1", "screen_result.rds")
  )
  saveRDS(
    list(results_table = data.frame(
      Model = c("gbm", "gbm"),
      Status = c("TIMEOUT", "SKIPPED_TIMEOUT"),
      elapsed_seconds = c(1200, 0),
      stringsAsFactors = FALSE
    )),
    file.path(pipeline_dir, "TG2", "stability_by", "sex", "F", "stability_result.rds")
  )

  timing <- collect(pipeline_dir)
  gbm <- timing[timing$Model == "gbm", , drop = FALSE]
  expect_equal(gbm$Analyses, 2)
  expect_equal(gbm$Attempts, 2)
  expect_equal(gbm$Timeouts, 2)
  expect_equal(gbm$Skipped, 1)
  expect_equal(gbm$Signal, "Frequent timeout")
  expect_equal(timing$Completed[timing$Model == "glmnet"], 1)
})

test_that("stability completion requires every configured stratum", {
  complete <- ugplot_test_internal("ugplot_geo_stability_complete_groups")
  screen <- data.frame(GroupID = "TG1", BestModel = "lm", stringsAsFactors = FALSE)
  metadata <- data.frame(sample_id = c("S1", "S2"), sex = c("F", "M"), stringsAsFactors = FALSE)
  partial <- data.frame(
    GroupID = "TG1", StratumColumn = "sex", StratumValue = "F",
    stringsAsFactors = FALSE
  )
  finished <- rbind(partial, data.frame(GroupID = "TG1", StratumColumn = "sex", StratumValue = "M"))

  expect_length(complete(screen, partial, list(geo_ml_stability_group_column = "sex"), metadata), 0L)
  expect_equal(complete(screen, finished, list(geo_ml_stability_group_column = "sex"), metadata), "TG1")
})

test_that("public job servers require authentication", {
  server <- ugplot_test_internal("ugPlotServer")
  expect_error(
    server(host = "0.0.0.0", port = 18080, token = "", register = FALSE),
    "bearer token is required"
  )
})

test_that("protected server token is inherited by background work", {
  skip_if_not_installed("plumber")
  skip_if_not_installed("callr")
  server <- ugplot_test_internal("ugPlotServer")
  observed_token <- NULL
  ugplot_test_local_namespace_binding("ugplot_assert_server_system_deps", function() {
    observed_token <<- Sys.getenv("UGPLOT_SERVER_TOKEN", unset = "")
    stop("stop after environment check", call. = FALSE)
  })
  withr::local_envvar(UGPLOT_SERVER_TOKEN = NA)

  expect_error(
    server(host = "0.0.0.0", port = 18080, token = "inherited-token", register = FALSE),
    "stop after environment check"
  )
  expect_equal(observed_token, "inherited-token")
})

test_that("distributed worker config is normalized safely", {
  normalize <- ugplot_test_internal("ugplot_geo_distributed_workers")
  workers <- normalize(list(distributed_workers = list(
    list(name = "Fy2", url = "http://fy2:8080", token = "a", cpu_limit = 8L),
    list(name = "Fy3", url = "http://fy3:8080", token = "b", cpu_limit = NA_integer_),
    list(name = "", url = "http://invalid")
  )))

  expect_length(workers, 2L)
  expect_equal(vapply(workers, `[[`, character(1), "name"), c("Fy2", "Fy3"))
  expect_equal(vapply(workers, `[[`, integer(1), "cpu_limit"), c(8L, 1L))
})

test_that("stability task keys normalize CSV missing values", {
  task_key <- ugplot_test_internal("ugplot_geo_ml_stability_task_key")

  expect_equal(task_key("TG1", NA_character_, NA_character_), task_key("TG1", "", ""))
})

test_that("loopback distributed worker uses the running server token", {
  normalize <- ugplot_test_internal("ugplot_geo_distributed_workers")
  withr::local_envvar(UGPLOT_SERVER_TOKEN = "current-server-token")

  workers <- normalize(list(distributed_workers = list(
    list(
      name = "Local 8080",
      url = "http://127.0.0.1:8080",
      token = "stale-token",
      cpu_limit = 6L
    ),
    list(
      name = "Fy3",
      url = "http://fy3.a00s.com:8080",
      token = "remote-token",
      cpu_limit = 5L
    )
  )))

  expect_equal(workers[[1]]$token, "current-server-token")
  expect_equal(workers[[2]]$token, "remote-token")
})

test_that("distributed retry waits for a different busy worker", {
  compatible <- ugplot_test_internal("ugplot_geo_retry_has_compatible_worker")

  expect_false(compatible("Fy3", "Fy3", 2L))
  expect_true(compatible("Fy3", c("Fy3", "Fy2"), 2L))
  expect_true(compatible("Fy3", "Fy3", 1L))
  expect_true(compatible("", "Fy3", 2L))
})

test_that("collaboration offers only fill unused queue slots", {
  offer_rows <- ugplot_test_internal("ugplot_geo_collaboration_offer_rows")
  manifest <- data.frame(
    GroupID = paste0("TG", 1:6),
    State = c("pending", "pending", "pending", "running", "pending", "completed"),
    stringsAsFactors = FALSE
  )

  expect_equal(offer_rows(manifest, c("TG1", "TG2"), queue_depth = 3L), 3L)
  expect_length(offer_rows(manifest, c("TG1", "TG2", "TG3"), queue_depth = 3L), 0L)
  expect_equal(offer_rows(manifest, character(0), queue_depth = 2L), c(1L, 2L))
  expect_length(offer_rows(manifest, queue_depth = 3L, draining = TRUE), 0L)
})

test_that("superseded internal jobs are selected before redispatch", {
  superseded <- ugplot_test_internal("ugplot_geo_superseded_worker_job_ids")
  jobs <- data.frame(
    id = c("old-running", "keep-running", "other-group", "top-level", "old-finished"),
    name = c(
      "Worker TG10 for parent", "Worker TG10 for parent",
      "Worker TG11 for parent", "Coordinator", "Worker TG10 for parent"
    ),
    state = c("running", "running", "running", "running", "finished"),
    internal_worker_task = c(TRUE, TRUE, TRUE, FALSE, TRUE),
    parent_job_id = c("parent", "parent", "parent", "", "parent"),
    group_id = c("TG10", "TG10", "TG11", "", "TG10"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    superseded(jobs, "parent", "TG10", keep_job_ids = "keep-running"),
    "old-running"
  )
  jobs$group_id[[1]] <- ""
  expect_equal(superseded(jobs, "parent", "TG10"), c("old-running", "keep-running"))
})

test_that("incomplete legacy worker results get a new idempotency revision", {
  request_id <- ugplot_test_internal("ugplot_geo_distributed_request_id")

  expect_equal(request_id("parent", "TG10"), "parent:analyze:TG10")
  expect_equal(
    request_id("parent", "TG10", "revision-2"),
    "parent:analyze:TG10:retry:revision-2"
  )
})

test_that("failed worker tasks resume the same checkpoint while attempts remain", {
  can_resume <- ugplot_test_internal("ugplot_geo_can_resume_worker_task")
  status <- list(state = "failed", resumable = TRUE, message = "Any worker failure")

  expect_true(can_resume(status, attempts = 1L, max_attempts = 3L))
  expect_false(can_resume(status, attempts = 3L, max_attempts = 3L))
  expect_false(can_resume(status, attempts = 1L, max_attempts = 3L, draining = TRUE))
  expect_false(can_resume(list(state = "failed", resumable = FALSE), 1L, 3L))
})

test_that("GEO checkpoints are written as readable atomic files", {
  write_checkpoint <- ugplot_test_internal("ugplot_geo_write_checkpoint")
  path <- file.path(tempfile("geo-checkpoint-"), "screen-result.rds")
  checkpoint <- list(results_table = data.frame(Model = "lm", dataset_seed = 1L, training_seed = 1L))

  write_checkpoint(checkpoint, path)
  updated_checkpoint <- checkpoint
  updated_checkpoint$results_table$Model <- "rpart"
  write_checkpoint(updated_checkpoint, path)

  expect_true(file.exists(path))
  expect_equal(readRDS(path), updated_checkpoint)
  expect_length(Sys.glob(paste0(path, ".tmp-*")), 0L)
})

test_that("smooth drain waits for every fixed and collaborative worker", {
  drain_ready <- ugplot_test_internal("ugplot_geo_drain_ready")

  expect_false(drain_ready("Fy2", FALSE))
  expect_false(drain_ready(character(0), TRUE))
  expect_true(drain_ready(character(0), logical(0)))
})

test_that("transcript group cache is complete only when every candidate was processed", {
  cache_complete <- ugplot_test_internal("ugplot_geo_transcript_group_cache_complete")
  root <- tempfile("transcript-group-cache-")
  dir.create(root)
  paths <- list(
    summary = file.path(root, "summary.csv"),
    details = file.path(root, "details.csv"),
    progress = file.path(root, "progress.rds")
  )
  candidates <- data.frame(
    Transcript = c("NM_001098623", "NM_052843"),
    CpG = c("cg04193160", "cg04193160"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(data.frame(GroupID = "TG1"), paths$summary, row.names = FALSE)
  utils::write.csv(data.frame(GroupID = "TG1"), paths$details, row.names = FALSE)
  saveRDS(data.frame(Transcript = "NM_001098623"), paths$progress)

  expect_false(cache_complete(candidates, paths))

  saveRDS(data.frame(Transcript = c("NM_001098623", "NM_052843")), paths$progress)
  expect_true(cache_complete(candidates, paths))
})

test_that("worker screening runner returns a portable group result", {
  ugplot_test_local_namespace_binding("ugplot_geo_screen_group", function(
      dataset, group, source, config, screen_path, importance_path,
      progress_callback = NULL, partial_callback = NULL) {
    list(
      summary = data.frame(
        GroupID = group$GroupID,
        DatasetPath = config$coordinator_dataset_path,
        ScreenResultPath = screen_path,
        ImportancePath = importance_path,
        stringsAsFactors = FALSE
      ),
      screen_result = list(best_model_name = "lm"),
      importance = data.frame(CpG = "cg1", Overall = 1)
    )
  })
  ugplot_test_local_namespace_binding("ugplot_geo_complete_group_stability", function(...) {
    list(
      summary = data.frame(GroupID = "TG1", Phase = "stability", Stable = TRUE),
      artifacts = list(list(
        summary = data.frame(
          GroupID = "TG1", Phase = "stability", StratumColumn = "", StratumValue = "",
          StabilityResultPath = "", ImportancePath = "", stringsAsFactors = FALSE
        ),
        result = list(best_model_name = "lm"),
        importance = data.frame(CpG = "cg1", Overall = 1)
      ))
    )
  })
  run_worker <- ugplot_test_internal("ugplot_run_geo_complete_group_job")
  result <- run_worker(
    data.frame(target = 1:3, cg1 = 3:1),
    list(
      job_dir = tempfile("worker-task-"),
      distributed_group = data.frame(GroupID = "TG1", stringsAsFactors = FALSE),
      matrix_source = "raw_sesame",
      parent_job_id = "parent-1",
      worker_name = "Fy3",
      coordinator_dataset_path = "/coordinator/TG1.csv"
    )
  )

  expect_equal(result$kind, "geo_complete_group")
  expect_equal(result$protocol_version, 2L)
  expect_equal(result$group_id, "TG1")
  expect_equal(result$worker_name, "Fy3")
  expect_equal(result$summary$DatasetPath, "/coordinator/TG1.csv")
  expect_length(result$stability_artifacts, 1L)
})

test_that("complete group worker reuses a saved screening checkpoint", {
  screening_calls <- 0L
  progress_phases <- character(0)
  resumed_stability <- NULL
  saved_screen <- list(best_model_name = "lm", results_table = data.frame(Model = "lm"))
  saved_summary <- data.frame(
    GroupID = "TG1", BestModel = "lm", Phase = "screening",
    DatasetPath = "/coordinator/TG1.csv",
    stringsAsFactors = FALSE
  )
  saved_stability <- data.frame(
    GroupID = "TG1", Phase = "stability",
    StratumColumn = "sex", StratumValue = "F",
    stringsAsFactors = FALSE
  )

  ugplot_test_local_namespace_binding("ugplot_geo_screen_group", function(...) {
    screening_calls <<- screening_calls + 1L
    stop("screening should not run")
  })
  ugplot_test_local_namespace_binding("ugplot_geo_complete_group_stability", function(
      dataset, screen_summary, source, config, task_dir,
      progress_callback = NULL, partial_callback = NULL) {
    resumed_stability <<- config$distributed_resume_stability_summary
    list(summary = saved_stability, artifacts = list())
  })

  run_worker <- ugplot_test_internal("ugplot_run_geo_complete_group_job")
  result <- run_worker(
    data.frame(target = 1:3, cg1 = 3:1),
    list(
      job_dir = tempfile("worker-resume-"),
      distributed_group = data.frame(GroupID = "TG1", stringsAsFactors = FALSE),
      matrix_source = "processed",
      distributed_resume_screen = list(
        summary = saved_summary,
        screen_result = saved_screen,
        importance = data.frame(CpG = "cg1", Overall = 1)
      ),
      distributed_resume_stability_summary = saved_stability
    ),
    progress_callback = function(...) {
      args <- list(...)
      progress_phases <<- c(progress_phases, as.character(args$phase %||% ""))
    }
  )

  expect_equal(screening_calls, 0L)
  expect_equal(result$screen_result, saved_screen)
  expect_equal(result$summary, saved_summary)
  expect_equal(resumed_stability, saved_stability)
  expect_true("screening_reused" %in% progress_phases)
})

test_that("stability resume skips strata already completed elsewhere", {
  model_calls <- 0L
  saved_stability <- data.frame(
    GroupID = "TG1", BestModel = "lm", Phase = "stability",
    StratumColumn = "sex", StratumValue = "F",
    stringsAsFactors = FALSE
  )
  screen_summary <- data.frame(
    GroupID = "TG1", BestModel = "lm", Phase = "screening",
    DatasetPath = "/unused/when-stratum-is-complete.rds",
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    sample_id = c("S1", "S2"), sex = c("F", "F"),
    stringsAsFactors = FALSE
  )
  ugplot_test_local_namespace_binding("ugplot_run_ml_job", function(...) {
    model_calls <<- model_calls + 1L
    stop("completed stability stratum should not run")
  })
  ugplot_test_local_namespace_binding(
    "ugplot_geo_enrich_ml_summary_remote",
    function(summary, ...) summary
  )
  ugplot_test_local_namespace_binding(
    "ugplot_geo_ml_rank_summary",
    function(summary, ...) summary
  )

  run_stability <- ugplot_test_internal("ugplot_geo_run_transcript_stability_remote")
  result <- run_stability(
    screen_summary,
    cache_dir = tempfile("stability-resume-"),
    config = list(
      geo_ml_stability_group_column = "sex",
      distributed_resume_stability_summary = saved_stability
    ),
    metadata = metadata
  )

  expect_equal(model_calls, 0L)
  expect_equal(result$GroupID, "TG1")
  expect_equal(result$StratumValue, "F")
})

test_that("distributed resume config carries coordinator checkpoints", {
  root <- tempfile("distributed-resume-config-")
  dir.create(root)
  screen_path <- file.path(root, "screen.rds")
  importance_path <- file.path(root, "importance.csv")
  screen_result <- list(best_model_name = "lm")
  saveRDS(screen_result, screen_path)
  utils::write.csv(
    data.frame(CpG = "cg1", Overall = 1),
    importance_path,
    row.names = FALSE
  )
  summaries <- data.frame(
    GroupID = c("TG1", "TG2"),
    BestModel = c("lm", "rpart"),
    ScreenResultPath = c(screen_path, ""),
    ImportancePath = c(importance_path, ""),
    stringsAsFactors = FALSE
  )
  stability <- data.frame(
    GroupID = c("TG1", "TG2"),
    StratumColumn = c("sex", "sex"),
    StratumValue = c("F", "M"),
    stringsAsFactors = FALSE
  )

  build_resume <- ugplot_test_internal("ugplot_geo_distributed_resume_config")
  config <- build_resume(list(models = "lm"), "TG1", summaries, stability)

  expect_equal(config$distributed_resume_screen$screen_result, screen_result)
  expect_equal(config$distributed_resume_screen$summary$GroupID, "TG1")
  expect_equal(config$distributed_resume_screen$importance$CpG, "cg1")
  expect_equal(config$distributed_resume_stability_summary$GroupID, "TG1")
  expect_equal(config$distributed_resume_stability_summary$StratumValue, "F")
})

test_that("distributed scheduler checkpoints worker results in coordinator cache", {
  root <- tempfile("distributed-screen-")
  dir.create(root)
  pipeline_dir <- file.path(root, "pipeline")
  dir.create(pipeline_dir)
  dataset_path <- file.path(root, "TG1.csv")
  utils::write.csv(
    data.frame(sample_id = c("S1", "S2"), target = c(1, 2), cg1 = c(0.2, 0.8)),
    dataset_path,
    row.names = FALSE
  )
  eligible <- data.frame(
    GroupID = "TG1",
    PrincipalTranscript = "ENST000001",
    Gene = "GENE1",
    Columns = 2L,
    Samples = 2L,
    TranscriptCount = 1L,
    ExtraTranscripts = "",
    CpGs = "cg1",
    TriggerMaxAbsRho = 0.8,
    TriggerBestCpG = "cg1",
    TriggerBestRho = 0.8,
    DatasetPath = dataset_path,
    stringsAsFactors = FALSE
  )
  worker_summary <- data.frame(
    Source = "raw_sesame",
    Phase = "screening",
    GroupID = "TG1",
    PrincipalTranscript = "ENST000001",
    Gene = "GENE1",
    Columns = 2L,
    Samples = 2L,
    TranscriptCount = 1L,
    ExtraTranscripts = "",
    CpGs = "cg1",
    TriggerMaxAbsRho = 0.8,
    TriggerBestCpG = "cg1",
    TriggerBestRho = 0.8,
    BestModel = "lm",
    MetricName = "R2",
    BestMetric = 0.7,
    MedianMetric = 0.7,
    MeanMetric = 0.7,
    SeedsRun = 3L,
    ModelsRun = 1L,
    ModelsOK = 1L,
    DatasetPath = "/worker/TG1.csv",
    ScreenResultPath = "/worker/result.rds",
    ImportancePath = "/worker/importance.csv",
    stringsAsFactors = FALSE
  )
  stability_summary <- worker_summary
  stability_summary$Phase <- "stability"
  stability_summary$StratumColumn <- ""
  stability_summary$StratumValue <- ""
  stability_summary$Stable <- TRUE
  stability_summary$StabilityResultPath <- "/worker/stability.rds"
  ugplot_test_local_namespace_binding("ugplot_remote_create_job", function(...) list(id = "worker-job-1"))
  ugplot_test_local_namespace_binding("ugplot_remote_job_status", function(...) {
    list(state = "finished", resumable = FALSE)
  })
  ugplot_test_local_namespace_binding("ugplot_remote_get_result", function(...) {
    list(
      kind = "geo_complete_group",
      group_id = "TG1",
      summary = worker_summary,
      screen_result = list(best_model_name = "lm"),
      importance = data.frame(CpG = "cg1", Overall = 1),
      stability_summary = stability_summary,
      stability_artifacts = list(list(
        summary = stability_summary,
        result = list(best_model_name = "lm", final_summary = list(metric_value = 0.72)),
        importance = data.frame(CpG = "cg1", Overall = 0.9)
      ))
    )
  })
  ugplot_test_local_namespace_binding("ugplot_remote_delete_job", function(...) list(deleted = TRUE))

  run_distributed <- ugplot_test_internal("ugplot_geo_run_transcript_ml_distributed")
  summary_path <- file.path(pipeline_dir, "screening_summary.csv")
  saveRDS(
    data.frame(
      GroupID = "TG1",
      Worker = "Fy3",
      JobID = "",
      State = "pending",
      Attempts = 2L,
      PollFailures = 0L,
      Error = "Connection timeout",
      stringsAsFactors = FALSE
    ),
    file.path(pipeline_dir, "distributed-screening.rds")
  )
  result <- run_distributed(
    eligible = eligible,
    summaries = data.frame(),
    summary_path = summary_path,
    pipeline_dir = pipeline_dir,
    cache_dir = root,
    source = "raw_sesame",
    run_key = "test",
    config = list(
      job_dir = file.path(root, "parent-job"),
      distributed_poll_seconds = 0,
      distributed_max_attempts = 2L
    ),
    workers = list(list(
      name = "Fy3",
      url = "http://fy3:8080",
      token = "secret",
      cpu_limit = 2L
    ))
  )

  manifest <- readRDS(file.path(pipeline_dir, "distributed-screening.rds"))
  expect_equal(result$GroupID, "TG1")
  expect_equal(result$DatasetPath, dataset_path)
  expect_true(file.exists(result$ScreenResultPath))
  expect_true(file.exists(result$ImportancePath))
  final_summary <- utils::read.csv(file.path(pipeline_dir, "summary.csv"), stringsAsFactors = FALSE)
  expect_equal(final_summary$Phase, "stability")
  expect_true(file.exists(final_summary$StabilityResultPath))
  expect_equal(manifest$State, "completed")
  expect_equal(manifest$Attempts, 1L)
})
