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

test_that("public job servers require authentication", {
  server <- ugplot_test_internal("ugPlotServer")
  expect_error(
    server(host = "0.0.0.0", port = 18080, token = "", register = FALSE),
    "bearer token is required"
  )
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

test_that("distributed retry waits for a different busy worker", {
  compatible <- ugplot_test_internal("ugplot_geo_retry_has_compatible_worker")

  expect_false(compatible("Fy3", "Fy3", 2L))
  expect_true(compatible("Fy3", c("Fy3", "Fy2"), 2L))
  expect_true(compatible("Fy3", "Fy3", 1L))
  expect_true(compatible("", "Fy3", 2L))
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
      progress_callback = NULL) {
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
  run_worker <- ugplot_test_internal("ugplot_run_geo_screen_group_job")
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

  expect_equal(result$kind, "geo_screen_group")
  expect_equal(result$group_id, "TG1")
  expect_equal(result$worker_name, "Fy3")
  expect_equal(result$summary$DatasetPath, "/coordinator/TG1.csv")
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
  ugplot_test_local_namespace_binding("ugplot_remote_create_job", function(...) list(id = "worker-job-1"))
  ugplot_test_local_namespace_binding("ugplot_remote_job_status", function(...) {
    list(state = "finished", resumable = FALSE)
  })
  ugplot_test_local_namespace_binding("ugplot_remote_get_result", function(...) {
    list(
      kind = "geo_screen_group",
      group_id = "TG1",
      summary = worker_summary,
      screen_result = list(best_model_name = "lm"),
      importance = data.frame(CpG = "cg1", Overall = 1)
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
  expect_equal(manifest$State, "completed")
  expect_equal(manifest$Attempts, 1L)
})
