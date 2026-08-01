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
    TranscriptMembers = c("ENST1;ENST1_ALT", "ENST2"),
    GeneMembers = c("GENE1", "GENE2"), TranscriptCount = c(2L, 1L),
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
  utils::write.csv(rbind(stability, stability), paths$stability, row.names = FALSE)
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
  write_atomic(data.frame(
    GroupID = c("TG1", "TG2", "TG3"),
    Worker = c("", "Fy2", ""),
    State = c("completed", "running", "pending"),
    Progress = c(1, 0.37, 0),
    Message = c("Completed", "Comparing candidate models", "Waiting"),
    stringsAsFactors = FALSE
  ), paths$manifest)
  ugplot_test_internal("ugplot_update_job_status")(
    status$id,
    jobs_dir,
    state = "running",
    message = "Distributed complete analysis: 1/3 group(s); active Fy2:TG2",
    distributed_state = list(
      completed = 1L, total = 3L, active = 1L,
      active_groups = "Fy2:TG2"
    )
  )

  report <- report_job(status$id, jobs_dir)
  expect_equal(report$progress$total, 3L)
  expect_equal(report$progress$screened, 2L)
  expect_equal(report$progress$stabilized, 1L)
  expect_length(report$discoveries, 3L)
  expect_equal(report$discoveries[[1]]$status, "stabilized")
  expect_equal(report$discoveries[[1]]$gene, "GENE1")
  expect_equal(report$discoveries[[1]]$transcript, "ENST1;ENST1_ALT")
  expect_equal(report$discoveries[[1]]$transcript_count, 2)
  expect_equal(report$discoveries[[1]]$cpgs, 12)
  expect_equal(report$discoveries[[1]]$median_r2, 0.82)
  expect_equal(report$discoveries[[2]]$status, "preliminary")
  expect_equal(report$discoveries[[3]]$status, "awaiting analysis")
  expect_equal(report$discoveries[[3]]$best_cpg, "cg3")
  expect_false(any(grepl("Path$", names(report$discoveries[[1]]))))
  expect_equal(report$collaboration$active, 1L)
  expect_equal(report$collaboration$contributors[[1]]$scientist, "Fy2")
  expect_equal(report$collaboration$contributors[[1]]$kind, "ugPlot server")
  expect_equal(report$collaboration$contributors[[1]]$group, "TG2")
  expect_equal(report$collaboration$contributors[[1]]$progress, 0.37)
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
  expect_match(report_html, "<th>CpGs</th>", fixed = TRUE)
  expect_match(report_html, "fmt(r.cpgs)", fixed = TRUE)
  expect_match(report_html, "Best overall performance", fixed = TRUE)
  expect_match(report_html, "computational group may contain multiple biological transcripts", fixed = TRUE)
  expect_match(report_html, "(ml+cpg)/2", fixed = TRUE)
  expect_match(report_html, 'class="controls"', fixed = TRUE)
  expect_match(report_html, "Science collaboration", fixed = TRUE)
  expect_match(report_html, 'name.textContent=row.scientist', fixed = TRUE)
  expect_match(report_html, 'activity.textContent=', fixed = TRUE)
  expect_false(grepl('innerHTML=row.scientist', report_html, fixed = TRUE))
})

test_that("public collaboration text is bounded and cannot become report markup", {
  public_text <- ugplot_test_internal("ugplot_public_report_text")
  malicious <- paste0("<img src=x onerror=alert(1)>", "\n", paste(rep("x", 200), collapse = ""))
  cleaned <- public_text(malicious, 40L)
  expect_false(grepl("\n", cleaned, fixed = TRUE))
  expect_lte(nchar(cleaned), 40L)
  expect_match(cleaned, "<img src=x", fixed = TRUE)

  html <- ugplot_test_internal("ugplot_discovery_report_html")("job-safe")
  expect_match(html, 'name.textContent=row.scientist', fixed = TRUE)
  expect_false(grepl(malicious, html, fixed = TRUE))
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
  expect_equal(snapshot$collaboration$active, 0L)
  expect_identical(snapshot$discoveries, list())
})

test_that("live report overlay clears stale workers after stop", {
  jobs_dir <- tempfile("ugplot-report-live-")
  dir.create(jobs_dir, recursive = TRUE)
  create_job <- ugplot_test_internal("ugplot_create_job")
  update_status <- ugplot_test_internal("ugplot_update_job_status")
  overlay <- ugplot_test_internal("ugplot_discovery_snapshot_live_status")
  status <- create_job(data.frame(x = 1), jobs_dir = jobs_dir, type = "geo")
  update_status(
    status$id, jobs_dir, state = "stopped", message = "Stopped safely",
    distributed_state = list(active = 0L, active_tasks = list())
  )
  stale <- jsonlite::toJSON(list(
    job = list(id = status$id, state = "running", message = "Running"),
    progress = list(total = 2L, screened = 1L, stabilized = 1L),
    collaboration = list(
      active = 1L,
      contributors = list(list(scientist = "Fy2", kind = "ugPlot server", group = "TG1"))
    ),
    discoveries = list()
  ), auto_unbox = TRUE)

  live <- jsonlite::fromJSON(overlay(stale, status$id, jobs_dir), simplifyVector = FALSE)
  expect_equal(live$job$state, "stopped")
  expect_equal(live$job$message, "Stopped safely")
  expect_equal(live$collaboration$active, 0L)
  expect_identical(live$collaboration$contributors, list())
})
