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
