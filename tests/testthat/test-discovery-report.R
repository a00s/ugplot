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
  tg1_dataset_path <- file.path(cache_dir, "TG1.csv")
  utils::write.csv(
    data.frame(sample_id = paste0("S", 1:5), age = 1:5, cg1 = c(1, 2, 3, 5, 4)),
    tg1_dataset_path, row.names = FALSE
  )
  screening$DatasetPath <- c(tg1_dataset_path, "")
  stability <- screening[1, , drop = FALSE]
  stability$Phase <- "stability"
  stability$MedianMetric <- 0.82
  stability$MetricSE <- 0.004
  stability$SeedsRun <- 90
  stability$Stable <- TRUE
  stability$SeedStrategy <- "dataset_partition_v1"
  stability_result_path <- file.path(paths$pipeline_dir, "TG1-stability-result.rds")
  saveRDS(
    list(results_table = data.frame(R2 = c(0.71, 0.82, 0.91), Status = "OK")),
    stability_result_path
  )
  stability$StabilityResultPath <- stability_result_path
  utils::write.csv(screening, paths$screening, row.names = FALSE)
  legacy_stability <- stability
  legacy_stability$GroupID <- "TG2"
  legacy_stability$SeedStrategy <- ""
  utils::write.csv(rbind(stability, stability, legacy_stability), paths$stability, row.names = FALSE)
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
    Worker = c("Fy3", "Fy2", ""),
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
  write_task <- ugplot_test_internal("ugplot_collaboration_write_task")
  task_id <- paste(status$id, "analyze", "TG1", sep = ":")
  write_task(list(
    task_id = task_id, parent_job_id = status$id, state = "completed",
    scientist_name = "Adyl", client_id = "public-client",
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ), jobs_dir = jobs_dir)

  report <- report_job(status$id, jobs_dir)
  expect_equal(report$progress$total, 3L)
  expect_equal(report$progress$screened, 2L)
  expect_equal(report$progress$stabilized, 1L)
  expect_equal(report$protocol_version, 3L)
  expect_length(report$discoveries, 3L)
  expect_equal(report$discoveries[[1]]$status, "stabilized")
  expect_equal(report$discoveries[[1]]$gene, "GENE1")
  expect_equal(report$discoveries[[1]]$transcript, "ENST1;ENST1_ALT")
  expect_equal(report$discoveries[[1]]$transcript_count, 2)
  expect_equal(report$discoveries[[1]]$cpgs, 12)
  expect_equal(report$discoveries[[1]]$cpg_rho_original, 0.71)
  expect_equal(report$discoveries[[1]]$cpg_rho_ml, 0.9)
  expect_equal(report$discoveries[[1]]$cpg_rho_ml_n, 5)
  expect_equal(report$discoveries[[1]]$cpg_rho, 0.9)
  expect_equal(report$discoveries[[1]]$median_r2, 0.82)
  expect_equal(report$discoveries[[1]]$min_r2, 0.71)
  expect_equal(report$discoveries[[1]]$max_r2, 0.91)
  expect_equal(report$discoveries[[1]]$resolved_by, "Adyl")
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

test_that("discovery rows are reconciled to the current biological group", {
  reconcile <- ugplot_test_internal("ugplot_reconcile_discovery_group_ids")
  groups <- data.frame(
    GroupID = c("TG10", "TG32", "TG33"),
    PrincipalTranscript = c("ENST_TRIM59", "ENST_ZYG11A_A", "ENST_ZYG11A_B"),
    Gene = c("TRIM59", "ZYG11A", "ZYG11A"),
    Columns = c(17L, 13L, 12L), Samples = c(649L, 699L, 699L),
    CpGs = c("cg_a;cg_b", "cg16015712;cg_z2", "cg16015712;cg_z3"),
    GroupKey = c("trim-key", "zyg-a-key", "zyg-b-key"),
    DatasetPath = c("trim.csv", "zyg-a.csv", "zyg-b.csv"),
    stringsAsFactors = FALSE
  )
  old_result <- data.frame(
    GroupID = "TG10", PrincipalTranscript = "NM_001004339", Gene = "ZYG11A",
    Columns = 13L, Samples = 699L, CpGs = "cg16015712;cg_z2",
    TriggerBestCpG = "cg16015712", stringsAsFactors = FALSE
  )

  fixed <- reconcile(old_result, groups)
  expect_equal(fixed$OriginalGroupID, "TG10")
  expect_equal(fixed$GroupID, "TG32")
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
  expect_match(report_html, 'data-sort-key="cpg_rho_original"', fixed = TRUE)
  expect_match(report_html, 'data-sort-key="cpg_rho_ml"', fixed = TRUE)
  expect_match(report_html, "How ranking and discovery types are defined", fixed = TRUE)
  expect_match(report_html, "They measure different properties", fixed = TRUE)
  expect_match(report_html, 'data-sort-key="cpgs"', fixed = TRUE)
  expect_match(report_html, 'data-sort-key="resolved_by"', fixed = TRUE)
  expect_match(report_html, "resolverBadge(r.resolved_by)", fixed = TRUE)
  expect_match(report_html, "fmt(r.cpgs)", fixed = TRUE)
  expect_match(report_html, "Best overall performance", fixed = TRUE)
  expect_match(report_html, 'id="type-filter"', fixed = TRUE)
  expect_match(report_html, "All types", fixed = TRUE)
  expect_match(report_html, "CpG centered", fixed = TRUE)
  expect_match(report_html, '!selectedType||r.type===selectedType', fixed = TRUE)
  expect_match(report_html, 'data-sort-key="median_r2"', fixed = TRUE)
  expect_false(grepl('data-sort-key="min_r2"', report_html, fixed = TRUE))
  expect_false(grepl('data-sort-key="max_r2"', report_html, fixed = TRUE))
  expect_match(report_html, 'class="data-row"', fixed = TRUE)
  expect_match(report_html, "detailHtml(r,key)", fixed = TRUE)
  expect_match(report_html, "Min R²", fixed = TRUE)
  expect_match(report_html, "Max R²", fixed = TRUE)
  expect_match(report_html, "drawTrack(host,payload)", fixed = TRUE)
  expect_match(report_html, 'className="track-transcripts"', fixed = TRUE)
  expect_match(report_html, "new Map(source.map", fixed = TRUE)
  expect_match(report_html, 'className="track-tooltip"', fixed = TRUE)
  expect_match(report_html, "Spearman ρ", fixed = TRUE)
  expect_match(report_html, "Best CpG |ρ| · ML dataset", fixed = TRUE)
  expect_match(report_html, "Best CpG |ρ| · original", fixed = TRUE)
  expect_match(report_html, "Sample group", fixed = TRUE)
  expect_match(report_html, "All samples", fixed = TRUE)
  expect_match(report_html, "/groups/${encodeURIComponent(group)}/track", fixed = TRUE)
  expect_match(report_html, 'data-sort-type="number"', fixed = TRUE)
  expect_match(report_html, "activateHeader(th)", fixed = TRUE)
  expect_match(report_html, "columnCompare(a,b)", fixed = TRUE)
  expect_match(report_html, 'setAttribute("aria-sort"', fixed = TRUE)
  expect_match(report_html, "Click any column heading to sort it", fixed = TRUE)
  expect_match(report_html, "computational group may contain multiple biological transcripts", fixed = TRUE)
  expect_match(report_html, "Math.max(...values)", fixed = TRUE)
  expect_false(grepl("(ml+cpg)/2", report_html, fixed = TRUE))
  expect_false(grepl("max-height:70vh", report_html, fixed = TRUE))
  expect_match(report_html, "overflow-y:visible", fixed = TRUE)
  expect_match(report_html, '<button type="button" class="expand-icon"', fixed = TRUE)
  expect_false(grepl("row.onclick=toggle", report_html, fixed = TRUE))
  expect_match(report_html, 'class="controls"', fixed = TRUE)
  expect_match(report_html, "Science collaboration", fixed = TRUE)
  expect_match(report_html, "Group completion map", fixed = TRUE)
  expect_match(report_html, 'id="group-stripe"', fixed = TRUE)
  expect_match(report_html, "renderGroupProgress(raw,collaboration.contributors)", fixed = TRUE)
  expect_match(report_html, "group-segment-completed", fixed = TRUE)
  expect_match(report_html, ".group-stripe{display:grid;width:100%;height:38px;gap:0", fixed = TRUE)
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
  expect_match(html, "How to read this track", fixed = TRUE)
  expect_match(html, "Point colour", fixed = TRUE)
  expect_match(html, "Background annotation", fixed = TRUE)
  expect_match(html, 'viewBox","0 0 1120 320', fixed = TRUE)
  expect_match(html, 'label:"+0.5"', fixed = TRUE)
  expect_match(html, ".table-wrap{max-width:100%;overflow-x:hidden}", fixed = TRUE)
  expect_match(html, ".table-wrap table{width:100%;min-width:0;table-layout:fixed}", fixed = TRUE)
  expect_match(html, ".track-plot{width:100%;overflow:hidden}", fixed = TRUE)
  expect_match(html, ".track-svg{display:block;width:100%;height:auto}", fixed = TRUE)
  expect_match(html, "270-266*x} 84% ${34+18*x}", fixed = TRUE)
  expect_match(html, "hsl(4 84% 52%)", fixed = TRUE)
  expect_match(html, "@media(max-width:1050px){.track-layout{grid-template-columns:minmax(0,1fr)}", fixed = TRUE)
  expect_match(html, ".shell{width:calc(100% - 24px);max-width:1920px", fixed = TRUE)
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

test_that("missing discovery snapshots are prepared outside the HTTP request", {
  jobs_dir <- tempfile("ugplot-report-async-")
  dir.create(jobs_dir, recursive = TRUE)
  status <- ugplot_test_internal("ugplot_create_job")(
    data.frame(x = 1), jobs_dir = jobs_dir, type = "geo"
  )
  starts <- 0L
  ugplot_test_local_namespace_binding(
    "ugplot_start_job_discovery_snapshot_refresh",
    function(job_id, requested_jobs_dir) {
      expect_equal(job_id, status$id)
      expect_equal(requested_jobs_dir, jobs_dir)
      starts <<- starts + 1L
      invisible(NULL)
    }
  )
  ugplot_test_local_namespace_binding(
    "ugplot_refresh_job_discovery_snapshot",
    function(...) stop("synchronous refresh must not run")
  )

  response <- ugplot_test_internal("ugplot_discovery_snapshot_response")(status$id, jobs_dir)
  payload <- jsonlite::fromJSON(response$body, simplifyVector = FALSE)
  expect_equal(response$status, 202L)
  expect_true(response$refreshing)
  expect_equal(starts, 1L)
  expect_equal(payload$protocol_version, 3L)
  expect_true(payload$refreshing)
})

test_that("old discovery snapshots remain available while upgrading asynchronously", {
  jobs_dir <- tempfile("ugplot-report-upgrade-")
  dir.create(jobs_dir, recursive = TRUE)
  status <- ugplot_test_internal("ugplot_create_job")(
    data.frame(x = 1), jobs_dir = jobs_dir, type = "geo"
  )
  snapshot_path <- ugplot_test_internal("ugplot_job_discovery_snapshot_path")(status$id, jobs_dir)
  jsonlite::write_json(list(
    protocol_version = 2L,
    job = list(id = status$id, state = "queued", message = "Cached"),
    progress = list(total = 3L, screened = 1L, stabilized = 0L),
    collaboration = list(active = 0L, contributors = list()),
    discoveries = list()
  ), snapshot_path, auto_unbox = TRUE)
  starts <- 0L
  ugplot_test_local_namespace_binding(
    "ugplot_start_job_discovery_snapshot_refresh",
    function(...) {
      starts <<- starts + 1L
      invisible(NULL)
    }
  )

  response <- ugplot_test_internal("ugplot_discovery_snapshot_response")(status$id, jobs_dir)
  payload <- jsonlite::fromJSON(response$body, simplifyVector = FALSE)
  expect_equal(response$status, 200L)
  expect_true(response$refreshing)
  expect_equal(starts, 1L)
  expect_equal(payload$protocol_version, 2L)
  expect_equal(payload$progress$total, 3L)
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

test_that("one cached GEO group dataset can be listed and read while its job is active", {
  jobs_dir <- tempfile("ugplot-group-dataset-jobs-")
  cache_dir <- tempfile("ugplot-group-dataset-cache-")
  dir.create(jobs_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  ugplot_test_local_namespace_binding("ugplot_geo_cache_dir", function(accession) cache_dir)

  status <- ugplot_test_internal("ugplot_create_job")(
    data.frame(x = 1),
    config = list(
      runner = "ugplot_run_geo_pipeline_job", accession = "GSE87571",
      matrix_source = "processed", target_column = "age",
      transcript_absrho_threshold = 0.8, transcript_min_samples = 80
    ),
    jobs_dir = jobs_dir, type = "geo"
  )
  ugplot_test_internal("ugplot_update_job_status")(
    status$id, jobs_dir, state = "running", message = "Distributed analysis is running"
  )
  paths <- ugplot_test_internal("ugplot_job_discovery_report_paths")(status$id, jobs_dir)
  dir.create(dirname(paths$groups), recursive = TRUE, showWarnings = FALSE)
  tg1_path <- file.path(cache_dir, "TG1.csv")
  tg2_path <- file.path(cache_dir, "TG2.csv")
  expected <- data.frame(sample_id = c("S1", "S2"), age = c(40, 60), cg1 = c(0.2, 0.8))
  utils::write.csv(expected, tg1_path, row.names = FALSE)
  utils::write.csv(data.frame(sample_id = "S3", age = 50, cg2 = 0.5), tg2_path, row.names = FALSE)
  utils::write.csv(data.frame(
    GroupID = c("TG2", "TG1", "TG3"),
    PrincipalTranscript = c("ENST2", "ENST1", "ENST3"),
    Gene = c("GENE2", "GENE1", "GENE3"),
    TranscriptMembers = c("ENST2", "ENST1;ENST1_ALT", "ENST3"),
    Columns = c(1L, 1L, 1L), Samples = c(1L, 2L, 1L),
    DatasetPath = c(tg2_path, tg1_path, file.path(cache_dir, "missing.csv")),
    stringsAsFactors = FALSE
  ), paths$groups, row.names = FALSE)
  expected_details <- data.frame(
    GroupID = c("TG1", "TG2"), Transcript = c("ENST1", "ENST2"),
    Gene = c("GENE1", "GENE2"), CpG = c("cg1", "cg2"),
    GeneRegion = c("Body", "TSS1500"), Chr = c("chr1", "chr2"),
    Position = c(101L, 202L), SpearmanRho = c(0.8, -0.7), AbsRho = c(0.8, 0.7),
    stringsAsFactors = FALSE
  )
  utils::write.csv(expected_details, paths$group_details, row.names = FALSE)
  importance_path <- file.path(cache_dir, "TG1-importance.csv")
  expected_importance <- data.frame(CpG = "cg1", Importance = 0.75, ImportanceRank = 1L)
  utils::write.csv(expected_importance, importance_path, row.names = FALSE)
  dir.create(dirname(paths$screening), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(
    GroupID = "TG1", ImportancePath = importance_path, stringsAsFactors = FALSE
  ), paths$screening, row.names = FALSE)

  catalog <- ugplot_test_internal("ugplot_job_geo_group_datasets")(status$id, jobs_dir)
  expect_equal(catalog$group_id, c("TG1", "TG2"))
  expect_equal(catalog$gene, c("GENE1", "GENE2"))
  expect_false("DatasetPath" %in% names(catalog))

  payload <- ugplot_test_internal("ugplot_read_job_geo_group_dataset")(status$id, "tg1", jobs_dir)
  expect_equal(payload$group_id, "TG1")
  expect_equal(payload$accession, "GSE87571")
  expect_equal(payload$target, "age")
  expect_equal(payload$dataset, expected)
  expect_equal(payload$details, expected_details[1, , drop = FALSE])
  expect_equal(payload$importance, expected_importance)
  public_track <- ugplot_test_internal("ugplot_job_discovery_group_track")(status$id, "TG1", jobs_dir)
  expect_equal(public_track$group, "TG1")
  expect_length(public_track$points, 1L)
  expect_equal(public_track$points[[1]]$cpg, "cg1")
  expect_equal(public_track$points[[1]]$position, 101)
  expect_equal(public_track$points[[1]]$region, "Body")
  expect_equal(public_track$points[[1]]$rho, 0.8)
  expect_equal(ugplot_test_internal("ugplot_read_job_status")(status$id, jobs_dir)$state, "running")
  expect_error(
    ugplot_test_internal("ugplot_read_job_geo_group_dataset")(status$id, "../TG1", jobs_dir),
    "Invalid transcript group id"
  )
})
