test_that("incremental discovery report upgrades screened groups with stability", {
  jobs_dir <- tempfile("ugplot-report-jobs-")
  cache_dir <- tempfile("ugplot-report-cache-")
  dir.create(jobs_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE)

  create_job <- ugplot_test_internal("ugplot_create_job")
  report_paths <- ugplot_test_internal("ugplot_job_discovery_report_paths")
  report_job <- ugplot_test_internal("ugplot_job_discovery_report")
  write_atomic <- ugplot_test_internal("ugplot_write_rds_atomic")

  ugplot_test_local_namespace_binding("ugplot_geo_cache_dir", function(accession) cache_dir)
  status <- create_job(
    data.frame(x = 1),
    config = list(
      runner = "ugplot_run_geo_pipeline_job",
      accession = "GSE87571",
      matrix_source = "processed",
      target_column = "age",
      transcript_absrho_threshold = 0.8,
      transcript_min_samples = 80
    ),
    jobs_dir = jobs_dir,
    type = "geo"
  )
  paths <- report_paths(status$id, jobs_dir)
  dir.create(paths$pipeline_dir, recursive = TRUE, showWarnings = FALSE)

  screening <- data.frame(
    Source = "processed", Phase = "screening", GroupID = c("TG1", "TG2"),
    PrincipalTranscript = c("ENST1", "ENST2"), Gene = c("GENE1", "GENE2"),
    Columns = c(12, 9), Samples = c(714, 700), TriggerBestCpG = c("cg1", "cg2"),
    TriggerBestRho = c(0.71, 0.82), BestModel = c("glmnet", "ranger"),
    MedianMetric = c(0.84, 0.77), BestMetric = c(0.86, 0.79), SeedsRun = c(3, 3),
    stringsAsFactors = FALSE
  )
  stability <- screening[1, , drop = FALSE]
  stability$Phase <- "stability"
  stability$MedianMetric <- 0.82
  stability$MetricSE <- 0.004
  stability$SeedsRun <- 90
  stability$Stable <- TRUE
  utils::write.csv(screening, paths$screening, row.names = FALSE)
  utils::write.csv(stability, paths$stability, row.names = FALSE)
  group_candidates <- screening
  group_candidates$Phase <- NULL
  group_candidates$BestModel <- NULL
  group_candidates$MedianMetric <- NULL
  group_candidates$BestMetric <- NULL
  group_candidates$SeedsRun <- NULL
  group_candidates <- rbind(
    group_candidates,
    transform(group_candidates[1, , drop = FALSE],
      GroupID = "TG3", PrincipalTranscript = "ENST3", Gene = "GENE3",
      TriggerBestCpG = "cg3", TriggerBestRho = 0.74
    )
  )
  dir.create(dirname(paths$groups), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(group_candidates, paths$groups, row.names = FALSE)
  write_atomic(data.frame(GroupID = c("TG1", "TG2", "TG3")), paths$manifest)

  report <- report_job(status$id, jobs_dir)
  expect_equal(report$progress$total, 3L)
  expect_equal(report$progress$screened, 2L)
  expect_equal(report$progress$stabilized, 1L)
  expect_length(report$discoveries, 3L)
  expect_equal(report$discoveries[[1]]$status, "stabilized")
  expect_equal(report$discoveries[[1]]$gene, "GENE1")
  expect_equal(report$discoveries[[1]]$median_r2, 0.82)
  expect_equal(report$discoveries[[2]]$status, "preliminary")
  expect_equal(report$discoveries[[3]]$status, "awaiting analysis")
  expect_equal(report$discoveries[[3]]$best_cpg, "cg3")
  expect_false(any(grepl("Path$", names(report$discoveries[[1]]))))
})

test_that("discovery report HTML accepts a direct job link", {
  report_html <- ugplot_test_internal("ugplot_discovery_report_html")("job-123")
  expect_match(report_html, "UGPLOT LIVE DISCOVERY REPORT", fixed = TRUE)
  expect_match(report_html, "job-123", fixed = TRUE)
  expect_match(report_html, "Preliminary evidence", fixed = TRUE)
  expect_match(report_html, "raw.map(normalize)", fixed = TRUE)
  expect_false(grepl('id="server"', report_html, fixed = TRUE))
  expect_false(grepl('id="job"', report_html, fixed = TRUE))
  expect_false(grepl("Open report", report_html, fixed = TRUE))
  expect_match(report_html, "/reports/assets/ugplot.png", fixed = TRUE)
  expect_match(report_html, "Best CpG correlation", fixed = TRUE)
  expect_match(report_html, "Best overall performance", fixed = TRUE)
  expect_match(report_html, "(ml+cpg)/2", fixed = TRUE)
  expect_match(report_html, 'class="controls"', fixed = TRUE)
})

test_that("discovery report snapshot is a reusable static JSON artifact", {
  jobs_dir <- tempfile("ugplot-report-snapshot-")
  cache_dir <- tempfile("ugplot-report-cache-")
  dir.create(jobs_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  create_job <- ugplot_test_internal("ugplot_create_job")
  refresh <- ugplot_test_internal("ugplot_refresh_job_discovery_snapshot")
  snapshot_path <- ugplot_test_internal("ugplot_job_discovery_snapshot_path")
  ugplot_test_local_namespace_binding("ugplot_geo_cache_dir", function(accession) cache_dir)
  status <- create_job(
    data.frame(x = 1),
    config = list(
      runner = "ugplot_run_geo_pipeline_job", accession = "GSE87571",
      matrix_source = "processed", target_column = "age",
      transcript_absrho_threshold = 0.8, transcript_min_samples = 80
    ),
    jobs_dir = jobs_dir, type = "geo"
  )
  expect_true(refresh(status$id, jobs_dir))
  path <- snapshot_path(status$id, jobs_dir)
  expect_true(file.exists(path))
  snapshot <- jsonlite::read_json(path, simplifyVector = FALSE)
  expect_equal(snapshot$job$id, status$id)
  expect_identical(snapshot$discoveries, list())
})
