ugplot_test_active_collaboration_parent <- function(root, parent_job_id = "parent",
                                                    state = "running") {
  parent_dir <- file.path(root, parent_job_id)
  dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    list(
      id = parent_job_id,
      state = state,
      pid = if (identical(state, "running")) Sys.getpid() else NA_integer_,
      timeout = NA_real_
    ),
    file.path(parent_dir, "status.rds")
  )
}

test_that("Science Collab accepts a bare coordinator IP or hostname", {
  normalize <- ugplot_test_internal("ugplot_science_collab_url")

  expect_equal(normalize("192.168.1.20"), "http://192.168.1.20:8080")
  expect_equal(normalize("worker.example:9090"), "http://worker.example:9090")
  expect_equal(
    normalize("https://collab.example.org/"),
    "https://collab.example.org"
  )
  expect_error(normalize(""), "coordinator")
  expect_error(normalize("ftp://example.org"), "HTTP")
})

test_that("headless Science Collab declares its runtime dependencies", {
  packages <- ugplot_test_internal("ugplot_science_collab_client_packages")()
  expect_setequal(packages, c("httr", "jsonlite", "processx"))
})

test_that("a direct Science Collab coordinator excludes configured servers", {
  candidates <- ugplot_test_internal("ugplot_collaboration_coordinator_candidates")
  servers <- data.frame(
    name = c("Fy1", "Fy2"),
    url = c("http://fy1:8080", "http://fy2:8080"),
    token = c("one", "two"),
    stringsAsFactors = FALSE
  )

  direct <- candidates(servers, direct_url = "192.168.1.20")
  expect_equal(nrow(direct), 1L)
  expect_equal(direct$name, "Direct coordinator")
  expect_equal(direct$url, "http://192.168.1.20:8080")
  expect_false("token" %in% names(direct))

  configured <- candidates(servers, selected = "Fy2")
  expect_equal(configured$name, c("Fy2", "Fy1"))
})

test_that("collaboration recovers a lock left by a terminated process", {
  root <- tempfile("collaboration-")
  task_dir <- file.path(root, "collaboration", "orphaned-task")
  lock_dir <- file.path(task_dir, ".lock")
  dir.create(lock_dir, recursive = TRUE)
  saveRDS(
    list(pid = 999999999L, acquired_at = Sys.time() - 3600),
    file.path(lock_dir, "owner.rds")
  )
  with_lock <- ugplot_test_internal("ugplot_collaboration_with_lock")

  expect_equal(with_lock(task_dir, "recovered", timeout_seconds = 0.2), "recovered")
  expect_false(dir.exists(lock_dir))
})

test_that("collaboration recovers a legacy lock without owner metadata", {
  root <- tempfile("collaboration-")
  task_dir <- file.path(root, "collaboration", "legacy-task")
  lock_dir <- file.path(task_dir, ".lock")
  dir.create(lock_dir, recursive = TRUE)
  Sys.setFileTime(lock_dir, Sys.time() - 3600)
  with_lock <- ugplot_test_internal("ugplot_collaboration_with_lock")

  expect_equal(
    with_lock(
      task_dir, "recovered", timeout_seconds = 0.2,
      legacy_stale_seconds = 1
    ),
    "recovered"
  )
  expect_false(dir.exists(lock_dir))
})

test_that("collaboration does not steal a lock from a live process", {
  root <- tempfile("collaboration-")
  task_dir <- file.path(root, "collaboration", "live-task")
  lock_dir <- file.path(task_dir, ".lock")
  dir.create(lock_dir, recursive = TRUE)
  saveRDS(
    list(pid = Sys.getpid(), acquired_at = Sys.time()),
    file.path(lock_dir, "owner.rds")
  )
  with_lock <- ugplot_test_internal("ugplot_collaboration_with_lock")

  expect_error(
    with_lock(task_dir, "stolen", timeout_seconds = 0.1),
    "Collaboration task is busy",
    fixed = TRUE
  )
})

test_that("collaboration leases expire without blocking a task", {
  root <- tempfile("collaboration-")
  dir.create(root)
  ugplot_test_active_collaboration_parent(root)
  publish <- ugplot_test_internal("ugplot_collaboration_publish_task")
  claim <- ugplot_test_internal("ugplot_collaboration_claim_task")
  read_task <- ugplot_test_internal("ugplot_collaboration_read_task")
  write_task <- ugplot_test_internal("ugplot_collaboration_write_task")
  consume_fallback <- ugplot_test_internal("ugplot_collaboration_consume_fallback")
  release <- ugplot_test_internal("ugplot_collaboration_release_task")

  publish(
    "parent-TG1",
    "parent",
    payload = list(dataset = data.frame(x = 1:2)),
    requirements = list(models = "lm"),
    jobs_dir = root
  )
  first <- claim("client-a", list(models = "lm"), jobs_dir = root)
  expect_equal(first$task$state, "leased")
  expect_null(claim("client-b", list(models = "lm"), jobs_dir = root))

  expired <- read_task("parent-TG1", root)
  expired$lease_expires_at <- Sys.time() - 1
  write_task(expired, root)
  expect_true(consume_fallback("parent-TG1", root))
  expect_false(consume_fallback("parent-TG1", root))
  second <- claim("client-b", list(models = "lm"), jobs_dir = root)
  expect_equal(second$task$client_id, "client-b")
  expect_false(identical(first$task$lease_id, second$task$lease_id))
  expect_true(release("parent-TG1", second$task$lease_id, "client-b", root)$released)
  expect_equal(claim("client-c", list(models = "lm"), jobs_dir = root)$task$client_id, "client-c")
})

test_that("republishing an unchanged pending mission preserves its payload", {
  root <- tempfile("collaboration-")
  dir.create(root)
  publish <- ugplot_test_internal("ugplot_collaboration_publish_task")

  requirements <- list(models = "lm")
  mission <- list(title = "Stable mission")
  first <- publish(
    "stable-task", "parent", list(dataset = data.frame(x = 1)),
    requirements = requirements, mission = mission, jobs_dir = root
  )
  payload_time <- file.info(first$payload_path)$mtime
  Sys.sleep(0.02)
  second <- publish(
    "stable-task", "parent", list(dataset = data.frame(x = 999)),
    requirements = requirements, mission = mission, jobs_dir = root
  )

  expect_equal(file.info(second$payload_path)$mtime, payload_time)
  expect_equal(readRDS(second$payload_path)$dataset$x, 1)
})

test_that("collaboration accepts only the active lease and first result", {
  root <- tempfile("collaboration-")
  dir.create(root)
  ugplot_test_active_collaboration_parent(root)
  publish <- ugplot_test_internal("ugplot_collaboration_publish_task")
  claim <- ugplot_test_internal("ugplot_collaboration_claim_task")
  heartbeat <- ugplot_test_internal("ugplot_collaboration_heartbeat")
  release <- ugplot_test_internal("ugplot_collaboration_release_task")
  complete <- ugplot_test_internal("ugplot_collaboration_complete_task")
  take_result <- ugplot_test_internal("ugplot_collaboration_take_result")

  publish("task-1", "parent", list(value = 1), requirements = list(models = "lm"), jobs_dir = root)
  expect_null(claim("incompatible", list(models = "rf"), jobs_dir = root))
  lease <- claim("scientist", list(models = c("lm", "rf")), jobs_dir = root)$task
  expect_true(heartbeat("task-1", lease$lease_id, "scientist", jobs_dir = root)$accepted)
  expect_false(release("task-1", "wrong", "scientist", jobs_dir = root)$released)
  expect_false(complete("task-1", "wrong", "scientist", list(answer = 0), jobs_dir = root)$accepted)
  expect_true(complete("task-1", lease$lease_id, "scientist", list(answer = 42), jobs_dir = root)$accepted)
  expect_false(complete("task-1", lease$lease_id, "scientist", list(answer = 99), jobs_dir = root)$accepted)
  expect_equal(take_result("task-1", root)$result$answer, 42)
})

test_that("claim skips unavailable tasks and continues through the queue", {
  root <- tempfile("collaboration-")
  dir.create(root)
  ugplot_test_active_collaboration_parent(root)
  publish <- ugplot_test_internal("ugplot_collaboration_publish_task")
  claim <- ugplot_test_internal("ugplot_collaboration_claim_task")
  complete <- ugplot_test_internal("ugplot_collaboration_complete_task")

  first <- publish(
    "a-unavailable", "parent", list(value = 1),
    requirements = list(models = "lm"), jobs_dir = root
  )
  first_lease <- claim("first-client", list(models = "lm"), jobs_dir = root)$task
  expect_true(complete(
    first$task_id, first_lease$lease_id, "first-client", list(answer = 1), jobs_dir = root
  )$accepted)

  publish(
    "b-compatible", "parent", list(value = 2),
    requirements = list(models = "lm"), jobs_dir = root
  )
  claimed <- claim("second-client", list(models = "lm"), jobs_dir = root)

  expect_equal(claimed$task$task_id, "b-compatible")
  expect_equal(claimed$payload$value, 2)
})

test_that("collaboration runner emits structured scientific events", {
  run_payload <- ugplot_test_internal("ugplot_collaboration_run_payload")
  event_path <- tempfile(fileext = ".rds")
  ugplot_test_local_namespace_binding("ugplot_run_geo_complete_group_job", function(
      dataset, config, progress_callback = function(...) NULL, partial_callback = NULL) {
    progress_callback(
      progress = 0.25,
      message = "Running candidate",
      current_run = list(model = "lm", dataset_seed = 1L, training_seed = 2L)
    )
    partial_callback(list(results_table = data.frame(
      Model = "lm", Status = "OK", R2 = 0.72, RMSE = 1.4,
      stringsAsFactors = FALSE
    )))
    list(
      kind = "geo_complete_group",
      group_id = "example",
      screen_result = list(best_model_name = "lm")
    )
  })

  result <- run_payload(
    list(
      dataset = data.frame(target = 1:5, feature = 5:1),
      config = list(
        runner = "ugplot_run_geo_complete_group_job", target_column = "age",
        distributed_group = data.frame(GroupID = "TG1", Gene = "GENE1", stringsAsFactors = FALSE)
      )
    ),
    event_path = event_path
  )
  events <- readRDS(event_path)
  event_types <- vapply(events, `[[`, character(1), "type")

  expect_equal(result$group_id, "example")
  expect_true(all(c(
    "mission_received", "dataset_profiled", "experiment_started",
    "metric_updated", "validation_completed"
  ) %in% event_types))
  metric_event <- events[[which(event_types == "metric_updated")[[1]]]]
  profile_event <- events[[which(event_types == "dataset_profiled")[[1]]]]
  expect_equal(metric_event$data$metrics$R2, 0.72)
  expect_equal(profile_event$data$total_values, 10)
  expect_equal(profile_event$data$variable_names, c("target", "feature"))
  expect_equal(profile_event$data$target_label, "age")
  expect_equal(profile_event$data$metadata$Gene, "GENE1")
  expect_true(length(profile_event$data$target_distribution$counts) > 0L)
})

test_that("pending collaboration missions refresh their real requirements", {
  root <- tempfile("collaboration-")
  dir.create(root)
  publish <- ugplot_test_internal("ugplot_collaboration_publish_task")
  read_task <- ugplot_test_internal("ugplot_collaboration_read_task")
  required_models <- ugplot_test_internal("ugplot_collaboration_required_models")

  publish("task-refresh", "parent", list(version = 1), list(models = c("lm", "missing")), jobs_dir = root)
  publish("task-refresh", "parent", list(version = 2), list(models = "lm"), jobs_dir = root)
  refreshed <- read_task("task-refresh", root)

  expect_equal(refreshed$requirements$models, "lm")
  expect_equal(readRDS(refreshed$payload_path)$version, 2)
  expect_equal(required_models(list(models = c("lm", "rpart"))), c("lm", "rpart"))
})

test_that("public compatibility explains missing mission models", {
  root <- tempfile("collaboration-")
  dir.create(root)
  ugplot_test_active_collaboration_parent(root)
  publish <- ugplot_test_internal("ugplot_collaboration_publish_task")
  compatibility <- ugplot_test_internal("ugplot_collaboration_compatibility")

  publish(
    "task-explain", "parent", list(value = 1),
    requirements = list(models = c("lm", "ranger")),
    mission = list(title = "Example mission"), jobs_dir = root
  )
  result <- compatibility(list(models = "lm"), root)

  expect_equal(result$pending, 1L)
  expect_equal(result$compatible, 0L)
  expect_equal(result$missions[[1]]$missing_models, "ranger")
  expect_equal(result$missions[[1]]$title, "Example mission")
})

test_that("collaboration only offers tasks from a running parent job", {
  root <- tempfile("collaboration-")
  dir.create(root)
  ugplot_test_active_collaboration_parent(root, "finished-parent", "finished")
  ugplot_test_active_collaboration_parent(root, "running-parent", "running")
  publish <- ugplot_test_internal("ugplot_collaboration_publish_task")
  claim <- ugplot_test_internal("ugplot_collaboration_claim_task")
  compatibility <- ugplot_test_internal("ugplot_collaboration_compatibility")
  public_status <- ugplot_test_internal("ugplot_collaboration_public_status")

  publish(
    "a-old-task", "finished-parent", list(value = "old"),
    requirements = list(models = "lm"), jobs_dir = root
  )
  publish(
    "b-active-task", "running-parent", list(value = "active"),
    requirements = list(models = "lm"), jobs_dir = root
  )

  status <- public_status(root)
  expect_equal(status$pending, 1L)
  expect_equal(status$inactive_pending, 1L)
  report <- compatibility(list(models = "lm"), root)
  expect_equal(report$pending, 1L)
  expect_equal(report$inactive_pending, 1L)
  expect_equal(report$missions[[1]]$parent_job_id, "running-parent")
  expect_equal(report$inactive_missions[[1]]$parent_state, "finished")

  claimed <- claim("scientist", list(models = "lm"), jobs_dir = root)
  expect_equal(claimed$task$parent_job_id, "running-parent")
  expect_equal(claimed$payload$value, "active")
})

test_that("collaboration parent checks do not refresh the full job configuration", {
  root <- tempfile("collaboration-")
  dir.create(root)
  ugplot_test_active_collaboration_parent(root)
  parent_status <- ugplot_test_internal("ugplot_collaboration_parent_job_status")
  ugplot_test_local_namespace_binding(
    "ugplot_read_job_status",
    function(...) stop("full status refresh should not be called")
  )

  result <- parent_status(list(parent_job_id = "parent"), root)

  expect_true(result$active)
  expect_equal(result$state, "running")
})

test_that("job group activity identifies collaborative and server executors", {
  root <- tempfile("collaboration-")
  cache_root <- tempfile("geo-cache-")
  dir.create(root)
  dir.create(cache_root)
  job_id <- "parent-job"
  ugplot_test_active_collaboration_parent(root, job_id)
  saveRDS(
    list(
      type = "geo", runner = "ugplot_run_geo_pipeline_job", accession = "GSE1",
      matrix_source = "processed", target_column = "age",
      transcript_absrho_threshold = 0.7, transcript_min_samples = 80
    ),
    file.path(root, job_id, "config.rds")
  )
  ugplot_test_local_namespace_binding("ugplot_geo_cache_dir", function(accession) cache_root)
  run_key <- ugplot_test_internal("ugplot_geo_transcript_ml_run_key")("age", 0.7, 80)
  pipeline_dir <- ugplot_test_internal("ugplot_geo_transcript_ml_dir")(cache_root, "processed", run_key)
  saveRDS(
    data.frame(
      GroupID = c("TG1", "TG2", "TG3"), Worker = c("Fy2", "", "Fy3"),
      JobID = c("done", "", "worker-job"), State = c("completed", "pending", "running"),
      Progress = c(1, 0, 0.35), Message = c("Completed", "Waiting", "Training model"),
      stringsAsFactors = FALSE
    ),
    file.path(pipeline_dir, "distributed-screening.rds")
  )
  publish <- ugplot_test_internal("ugplot_collaboration_publish_task")
  claim <- ugplot_test_internal("ugplot_collaboration_claim_task")
  heartbeat <- ugplot_test_internal("ugplot_collaboration_heartbeat")
  activity <- ugplot_test_internal("ugplot_collaboration_job_group_activity")
  active_contributors <- ugplot_test_internal("ugplot_collaboration_active_contributors")
  publish(
    paste(job_id, "screen", "TG2", sep = ":"), job_id, list(value = 1),
    requirements = list(models = "lm"), jobs_dir = root
  )
  leased <- claim(
    "scientist-client", list(models = "lm", scientist_name = "Alice"), jobs_dir = root
  )$task
  expect_true(heartbeat(
    leased$task_id, leased$lease_id, "scientist-client",
    telemetry = list(progress = 0.42, message = "Comparing models", candidate = "ranger"),
    jobs_dir = root
  )$accepted)

  result <- activity(job_id, root)
  collaborative <- result$groups[result$groups$group_id == "TG2", , drop = FALSE]
  fixed <- result$groups[result$groups$group_id == "TG3", , drop = FALSE]

  expect_equal(result$completed, 1L)
  expect_equal(result$processing, 2L)
  expect_equal(collaborative$executor, "Alice")
  expect_equal(collaborative$executor_type, "collaboration")
  expect_equal(collaborative$progress, 0.42)
  expect_match(collaborative$message, "ranger")
  expect_equal(fixed$executor, "Fy3")
  expect_equal(fixed$progress, 0.35)

  active <- active_contributors(job_id, root)
  expect_equal(nrow(active), 1L)
  expect_equal(active$executor, "Alice")
  expect_equal(active$group_id, "TG2")
  expect_equal(active$progress, 0.42)
  expect_match(active$message, "Comparing models", fixed = TRUE)
})

test_that("collaboration payload can be staged without loading it in the Shiny process", {
  payload <- list(dataset = data.frame(x = 1:4), config = list(runner = "worker"))
  source_path <- tempfile(fileext = ".rds")
  saveRDS(payload, source_path)
  encoded <- base64enc::base64encode(source_path)
  unlink(source_path)

  staged_path <- ugplot_test_internal("ugplot_remote_store_rds_base64")(encoded)
  on.exit(unlink(staged_path), add = TRUE)

  expect_true(file.exists(staged_path))
  expect_identical(readRDS(staged_path), payload)
})
