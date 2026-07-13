test_that("collaboration leases expire without blocking a task", {
  root <- tempfile("collaboration-")
  dir.create(root)
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
  ugplot_test_local_namespace_binding("ugplot_run_geo_screen_group_job", function(
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
      kind = "geo_screen_group",
      group_id = "example",
      screen_result = list(best_model_name = "lm")
    )
  })

  result <- run_payload(
    list(
      dataset = data.frame(target = 1:5, feature = 5:1),
      config = list(runner = "ugplot_run_geo_screen_group_job")
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
  expect_equal(metric_event$data$metrics$R2, 0.72)
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
