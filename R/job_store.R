ugplot_default_jobs_dir <- function() {
  configured_dir <- Sys.getenv("UGPLOT_JOBS_DIR", unset = "")
  if (nzchar(configured_dir)) {
    return(normalizePath(configured_dir, mustWork = FALSE))
  }
  normalizePath(file.path(path.expand("~"), ".ugplot", "jobs"), mustWork = FALSE)
}

ugplot_new_job_id <- function() {
  random_part <- paste(sample(c(letters, LETTERS, 0:9), 12, replace = TRUE), collapse = "")
  paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", random_part)
}

ugplot_ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Could not create directory: ", path, call. = FALSE)
  }
  invisible(path)
}

ugplot_validate_job_id <- function(job_id) {
  if (!is.character(job_id) || length(job_id) != 1 ||
      job_id %in% c(".", "..") ||
      !grepl("^[A-Za-z0-9._-]+$", job_id)) {
    stop("Invalid job id.", call. = FALSE)
  }
  job_id
}

ugplot_job_dir <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  job_id <- ugplot_validate_job_id(job_id)
  file.path(jobs_dir, job_id)
}

ugplot_status_path <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  file.path(ugplot_job_dir(job_id, jobs_dir), "status.rds")
}

ugplot_result_path <- function(job_id, jobs_dir = ugplot_default_jobs_dir(), partial = FALSE) {
  file.path(ugplot_job_dir(job_id, jobs_dir), if (isTRUE(partial)) "partial-result.rds" else "result.rds")
}

ugplot_preview_result_path <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  file.path(ugplot_job_dir(job_id, jobs_dir), "preview-result.rds")
}

ugplot_best_model_path <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  file.path(ugplot_job_dir(job_id, jobs_dir), "best-model.rds")
}

ugplot_resources_path <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  file.path(ugplot_job_dir(job_id, jobs_dir), "resources.tsv")
}

ugplot_drain_request_path <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  file.path(ugplot_job_dir(job_id, jobs_dir), "drain-request.rds")
}

ugplot_job_drain_requested <- function(job_dir) {
  file.exists(file.path(as.character(job_dir %||% ""), "drain-request.rds"))
}

ugplot_request_job_drain <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  if (is.null(status)) stop("Job not found: ", job_id, call. = FALSE)
  if (!(status$state %||% "") %in% c("queued", "running", "draining")) {
    return(ugplot_read_job_status(job_id, jobs_dir))
  }
  ugplot_write_rds_atomic(
    list(requested_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    ugplot_drain_request_path(job_id, jobs_dir)
  )
  ugplot_update_job_status(
    job_id, jobs_dir,
    state = "draining",
    message = "Draining: finishing active work before stopping",
    drain_requested_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
  ugplot_read_job_status(job_id, jobs_dir)
}

ugplot_signal_job_drained <- function(message = "Drained safely; checkpoint is ready") {
  condition <- structure(list(message = message, call = NULL), class = c("ugplot_job_drained", "condition"))
  stop(condition)
}

ugplot_read_key_value_file <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  matches <- regexec("^([^:[:space:]]+):?[[:space:]]+(.+)$", lines)
  parts <- regmatches(lines, matches)
  parts <- parts[lengths(parts) >= 3L]
  if (length(parts) == 0) {
    return(list())
  }
  stats::setNames(
    lapply(parts, function(part) part[[3]]),
    vapply(parts, function(part) part[[2]], character(1))
  )
}

ugplot_linux_numeric_value <- function(value, divisor = 1) {
  value <- as.character(value %||% "")
  parsed <- suppressWarnings(as.numeric(sub("[[:space:]].*$", "", value)))
  if (length(parsed) == 0 || !is.finite(parsed[[1]])) NA_real_ else parsed[[1]] / divisor
}

ugplot_linux_process_table <- function() {
  if (.Platform$OS.type == "windows" || !dir.exists("/proc")) {
    return(data.frame())
  }
  proc_dirs <- list.files("/proc", pattern = "^[0-9]+$", full.names = TRUE)
  rows <- lapply(proc_dirs, function(proc_dir) {
    line <- tryCatch(readLines(file.path(proc_dir, "stat"), n = 1L, warn = FALSE), error = function(e) character(0))
    if (length(line) == 0) {
      return(NULL)
    }
    suffix <- sub("^.*\\) ", "", line[[1]])
    fields <- strsplit(suffix, " ", fixed = TRUE)[[1]]
    fields <- fields[nzchar(fields)]
    if (length(fields) < 13L) {
      return(NULL)
    }
    pid <- suppressWarnings(as.integer(basename(proc_dir)))
    ppid <- suppressWarnings(as.integer(fields[[2]]))
    cpu_ticks <- sum(suppressWarnings(as.numeric(fields[c(12L, 13L)])), na.rm = TRUE)
    if (is.na(pid) || is.na(ppid)) {
      return(NULL)
    }
    data.frame(pid = pid, ppid = ppid, cpu_ticks = cpu_ticks, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

ugplot_linux_process_tree_metrics <- function(pid, process_table = ugplot_linux_process_table()) {
  pid <- suppressWarnings(as.integer(pid))
  empty <- list(alive = FALSE, process_count = 0L, rss_mb = NA_real_, swap_mb = NA_real_,
                peak_mb = NA_real_, threads = NA_integer_, cpu_ticks = NA_real_)
  if (is.na(pid) || pid <= 0 || !is.data.frame(process_table) || nrow(process_table) == 0 ||
      !(pid %in% process_table$pid)) {
    return(empty)
  }
  tree_pids <- pid
  repeat {
    children <- process_table$pid[process_table$ppid %in% tree_pids]
    expanded <- unique(c(tree_pids, children))
    if (length(expanded) == length(tree_pids)) {
      break
    }
    tree_pids <- expanded
  }
  process_rows <- process_table[process_table$pid %in% tree_pids, , drop = FALSE]
  statuses <- lapply(tree_pids, function(tree_pid) {
    ugplot_read_key_value_file(file.path("/proc", tree_pid, "status"))
  })
  sum_status <- function(key, divisor = 1024) {
    values <- vapply(statuses, function(status) ugplot_linux_numeric_value(status[[key]], divisor), numeric(1))
    if (all(is.na(values))) NA_real_ else sum(values, na.rm = TRUE)
  }
  list(
    alive = TRUE,
    process_count = length(tree_pids),
    rss_mb = sum_status("VmRSS"),
    swap_mb = sum_status("VmSwap"),
    peak_mb = sum_status("VmPeak"),
    threads = as.integer(round(sum_status("Threads", divisor = 1))),
    cpu_ticks = sum(process_rows$cpu_ticks, na.rm = TRUE)
  )
}

ugplot_linux_psi_avg10 <- function(path, kind) {
  if (!file.exists(path)) {
    return(NA_real_)
  }
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  line <- lines[startsWith(lines, paste0(kind, " "))]
  if (length(line) == 0) {
    return(NA_real_)
  }
  match <- regexec("avg10=([0-9.]+)", line[[1]])
  parts <- regmatches(line[[1]], match)[[1]]
  if (length(parts) < 2L) NA_real_ else suppressWarnings(as.numeric(parts[[2]]))
}

ugplot_linux_cgroup_memory <- function() {
  empty <- list(current_mb = NA_real_, max_mb = NA_real_, swap_current_mb = NA_real_,
                swap_max_mb = NA_real_, high = NA_real_, max_events = NA_real_,
                oom = NA_real_, oom_kill = NA_real_)
  if (.Platform$OS.type == "windows" || !file.exists("/proc/self/cgroup")) {
    return(empty)
  }
  lines <- tryCatch(readLines("/proc/self/cgroup", warn = FALSE), error = function(e) character(0))
  unified <- lines[startsWith(lines, "0::")]
  if (length(unified) == 0) {
    return(empty)
  }
  relative <- sub("^0::", "", unified[[1]])
  base <- file.path("/sys/fs/cgroup", sub("^/", "", relative))
  read_number <- function(name, divisor = 1) {
    path <- file.path(base, name)
    value <- if (file.exists(path)) tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) "") else ""
    value <- if (length(value) == 0) "" else value[[1]]
    parsed <- suppressWarnings(as.numeric(value))
    if (!is.finite(parsed)) NA_real_ else parsed / divisor
  }
  events <- ugplot_read_key_value_file(file.path(base, "memory.events"))
  list(
    current_mb = read_number("memory.current", 1024^2),
    max_mb = read_number("memory.max", 1024^2),
    swap_current_mb = read_number("memory.swap.current", 1024^2),
    swap_max_mb = read_number("memory.swap.max", 1024^2),
    high = ugplot_linux_numeric_value(events$high, 1),
    max_events = ugplot_linux_numeric_value(events$max, 1),
    oom = ugplot_linux_numeric_value(events$oom, 1),
    oom_kill = ugplot_linux_numeric_value(events$oom_kill, 1)
  )
}

ugplot_linux_system_resources <- function() {
  mem <- ugplot_read_key_value_file("/proc/meminfo")
  vm <- ugplot_read_key_value_file("/proc/vmstat")
  load_line <- if (file.exists("/proc/loadavg")) tryCatch(readLines("/proc/loadavg", n = 1L, warn = FALSE), error = function(e) "") else ""
  load_line <- if (length(load_line) == 0) "" else load_line[[1]]
  load_fields <- strsplit(load_line, " ", fixed = TRUE)[[1]]
  load1 <- suppressWarnings(as.numeric(if (length(load_fields) > 0) load_fields[[1]] else NA_character_))
  cpu_line <- if (file.exists("/proc/stat")) tryCatch(readLines("/proc/stat", n = 1L, warn = FALSE), error = function(e) "") else ""
  cpu_line <- if (length(cpu_line) == 0) "" else cpu_line[[1]]
  cpu_fields <- suppressWarnings(as.numeric(strsplit(trimws(sub("^cpu", "", cpu_line)), " +")[[1]]))
  total_cpu_ticks <- if (length(cpu_fields) == 0 || all(is.na(cpu_fields))) NA_real_ else sum(cpu_fields, na.rm = TRUE)
  mem_total <- ugplot_linux_numeric_value(mem$MemTotal, 1024)
  mem_available <- ugplot_linux_numeric_value(mem$MemAvailable, 1024)
  swap_total <- ugplot_linux_numeric_value(mem$SwapTotal, 1024)
  swap_free <- ugplot_linux_numeric_value(mem$SwapFree, 1024)
  list(
    load1 = load1,
    mem_total_mb = mem_total,
    mem_available_mb = mem_available,
    mem_used_pct = if (is.finite(mem_total) && mem_total > 0) 100 * (mem_total - mem_available) / mem_total else NA_real_,
    swap_total_mb = swap_total,
    swap_free_mb = swap_free,
    swap_used_pct = if (is.finite(swap_total) && swap_total > 0) 100 * (swap_total - swap_free) / swap_total else 0,
    psi_some_avg10 = ugplot_linux_psi_avg10("/proc/pressure/memory", "some"),
    psi_full_avg10 = ugplot_linux_psi_avg10("/proc/pressure/memory", "full"),
    vm_oom_kill = ugplot_linux_numeric_value(vm$oom_kill, 1),
    total_cpu_ticks = total_cpu_ticks,
    cpus = {
      detected <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
      if (is.na(detected) || detected < 1L) 1L else detected
    },
    cgroup = ugplot_linux_cgroup_memory()
  )
}

ugplot_disk_resources <- function(path) {
  empty <- list(total_mb = NA_real_, available_mb = NA_real_, used_pct = NA_real_)
  if (.Platform$OS.type == "windows" || !nzchar(Sys.which("df"))) {
    return(empty)
  }
  output <- tryCatch(
    suppressWarnings(system2("df", c("-Pk", path), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  if (length(output) < 2L) {
    return(empty)
  }
  fields <- strsplit(trimws(utils::tail(output, 1L)), "[[:space:]]+")[[1]]
  if (length(fields) < 6L) {
    return(empty)
  }
  total_kb <- suppressWarnings(as.numeric(fields[[2]]))
  available_kb <- suppressWarnings(as.numeric(fields[[4]]))
  used_pct <- suppressWarnings(as.numeric(sub("%$", "", fields[[5]])))
  list(
    total_mb = if (is.finite(total_kb)) total_kb / 1024 else NA_real_,
    available_mb = if (is.finite(available_kb)) available_kb / 1024 else NA_real_,
    used_pct = if (is.finite(used_pct)) used_pct else NA_real_
  )
}

ugplot_resource_delta <- function(current, previous) {
  if (!is.finite(current) || !is.finite(previous)) NA_real_ else current - previous
}

ugplot_sample_job_resources <- function(status, previous = NULL, jobs_dir = ugplot_default_jobs_dir()) {
  pid <- suppressWarnings(as.integer(status$pid %||% NA_integer_))
  process <- ugplot_linux_process_tree_metrics(pid)
  system <- ugplot_linux_system_resources()
  disk <- ugplot_disk_resources(jobs_dir)
  same_process <- is.list(previous) && identical(suppressWarnings(as.integer(previous$pid)), pid)
  process_delta <- if (same_process) ugplot_resource_delta(process$cpu_ticks, previous$process_cpu_ticks) else NA_real_
  system_delta <- if (same_process) ugplot_resource_delta(system$total_cpu_ticks, previous$system_cpu_ticks) else NA_real_
  cpu_pct <- if (is.finite(process_delta) && process_delta >= 0 && is.finite(system_delta) && system_delta > 0) {
    100 * system$cpus * process_delta / system_delta
  } else {
    NA_real_
  }
  data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    pid = pid,
    alive = isTRUE(process$alive),
    process_count = process$process_count,
    process_rss_mb = round(process$rss_mb, 2),
    process_swap_mb = round(process$swap_mb, 2),
    process_peak_mb = round(process$peak_mb, 2),
    process_threads = process$threads,
    process_cpu_ticks = process$cpu_ticks,
    process_cpu_pct = round(cpu_pct, 2),
    host_cpu_count = system$cpus,
    host_load1 = round(system$load1, 2),
    host_mem_total_mb = round(system$mem_total_mb, 2),
    host_mem_available_mb = round(system$mem_available_mb, 2),
    host_mem_used_pct = round(system$mem_used_pct, 2),
    host_swap_total_mb = round(system$swap_total_mb, 2),
    host_swap_free_mb = round(system$swap_free_mb, 2),
    host_swap_used_pct = round(system$swap_used_pct, 2),
    disk_total_mb = round(disk$total_mb, 2),
    disk_available_mb = round(disk$available_mb, 2),
    disk_used_pct = round(disk$used_pct, 2),
    memory_psi_some_avg10 = system$psi_some_avg10,
    memory_psi_full_avg10 = system$psi_full_avg10,
    vm_oom_kill = system$vm_oom_kill,
    system_cpu_ticks = system$total_cpu_ticks,
    vm_oom_kill_delta = ugplot_resource_delta(system$vm_oom_kill, previous$vm_oom_kill %||% NA_real_),
    cgroup_mem_current_mb = round(system$cgroup$current_mb, 2),
    cgroup_mem_max_mb = round(system$cgroup$max_mb, 2),
    cgroup_swap_current_mb = round(system$cgroup$swap_current_mb, 2),
    cgroup_swap_max_mb = round(system$cgroup$swap_max_mb, 2),
    cgroup_high = system$cgroup$high,
    cgroup_max_events = system$cgroup$max_events,
    cgroup_oom = system$cgroup$oom,
    cgroup_oom_kill = system$cgroup$oom_kill,
    cgroup_oom_kill_delta = ugplot_resource_delta(system$cgroup$oom_kill, previous$cgroup_oom_kill %||% NA_real_),
    current_model = as.character(status$current_model %||% ""),
    current_message = as.character(status$message %||% ""),
    stringsAsFactors = FALSE
  )
}

ugplot_append_job_resources <- function(job_id, sample, jobs_dir = ugplot_default_jobs_dir()) {
  path <- ugplot_resources_path(job_id, jobs_dir)
  utils::write.table(
    sample,
    file = path,
    sep = "\t",
    row.names = FALSE,
    col.names = !file.exists(path) || file.info(path)$size == 0,
    quote = FALSE,
    append = file.exists(path) && file.info(path)$size > 0,
    na = "NA"
  )
  invisible(path)
}

ugplot_read_job_resources <- function(job_id, jobs_dir = ugplot_default_jobs_dir(), max_lines = 500L) {
  path <- ugplot_resources_path(job_id, jobs_dir)
  if (!file.exists(path)) {
    return(data.frame())
  }
  max_lines <- suppressWarnings(as.integer(max_lines))
  if (is.na(max_lines) || max_lines < 1L) {
    max_lines <- 500L
  }
  header <- readLines(path, n = 1L, warn = FALSE)
  if (length(header) == 0L) {
    return(data.frame())
  }
  lines <- if (.Platform$OS.type != "windows" && nzchar(Sys.which("tail"))) {
    tryCatch(
      system2("tail", c("-n", as.character(max_lines), path), stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    )
  } else {
    readLines(path, warn = FALSE)
  }
  lines <- lines[nzchar(lines) & lines != header[[1]]]
  if (length(lines) == 0L) {
    return(data.frame())
  }
  selected <- c(header[[1]], utils::tail(lines, max_lines))
  utils::read.delim(text = selected, stringsAsFactors = FALSE, check.names = FALSE)
}

ugplot_job_monitor_snapshot <- function(job_id, jobs_dir = ugplot_default_jobs_dir(),
                                        include_groups = TRUE, resource_lines = 60L) {
  ugplot_validate_job_id(job_id)
  # This endpoint is polled by the focused monitor and must stay cheap.  A full
  # status refresh also opens config.rds, which can contain a very large GEO
  # configuration.  The background job is the owner of status.rds and resource
  # telemetry already tells the monitor whether that process is alive.
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  if (!is.list(status)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }
  resources <- ugplot_read_job_resources(job_id, jobs_dir, max_lines = resource_lines)
  group_activity <- list(groups = data.frame())
  if (isTRUE(include_groups) && identical(as.character(status$type %||% ""), "geo")) {
    group_activity <- tryCatch(
      ugplot_collaboration_job_group_activity(job_id, jobs_dir, inspect_collaboration = FALSE),
      error = function(e) list(groups = data.frame(), error = conditionMessage(e))
    )
  }
  list(
    protocol_version = 1L,
    checked_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    status = status,
    resources = resources,
    group_activity = group_activity
  )
}

ugplot_server_resource_snapshot <- function(jobs_dir = ugplot_default_jobs_dir(), include_jobs = TRUE) {
  system <- ugplot_linux_system_resources()
  disk <- ugplot_disk_resources(jobs_dir)
  job_ids <- if (isTRUE(include_jobs) && dir.exists(jobs_dir)) {
    basename(list.dirs(jobs_dir, full.names = TRUE, recursive = FALSE))
  } else {
    character(0)
  }
  statuses <- if (length(job_ids) > 0L) {
    Filter(Negate(is.null), lapply(job_ids, function(job_id) {
      status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
      if (is.list(status) && (status$state %||% "") %in% c("running", "draining")) status else NULL
    }))
  } else {
    list()
  }
  samples <- Filter(function(value) is.data.frame(value) && nrow(value) > 0L, lapply(statuses, function(status) {
    ugplot_read_job_resources(status$id, jobs_dir, max_lines = 1L)
  }))
  latest <- if (length(samples) > 0L) do.call(rbind, samples) else data.frame()
  numeric_sum <- function(name) {
    if (!name %in% names(latest)) return(NA_real_)
    values <- suppressWarnings(as.numeric(latest[[name]]))
    if (all(!is.finite(values))) NA_real_ else sum(values[is.finite(values)])
  }
  tasks <- unique(vapply(statuses, function(status) {
    worker <- as.character(status$worker_name %||% "")
    message <- as.character(status$message %||% "")
    model <- as.character(status$current_model %||% "")
    detail <- if (nzchar(model)) paste0("model ", model) else message
    if (nzchar(worker)) paste0(worker, ": ", detail) else detail
  }, character(1)))
  tasks <- tasks[nzchar(tasks)]
  list(
    sampled_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    active_processes = length(statuses),
    process_cpu_pct = round(numeric_sum("process_cpu_pct"), 2),
    process_rss_mb = round(numeric_sum("process_rss_mb"), 2),
    host_cpu_count = system$cpus,
    host_load1 = round(system$load1, 2),
    host_mem_total_mb = round(system$mem_total_mb, 2),
    host_mem_available_mb = round(system$mem_available_mb, 2),
    host_mem_used_pct = round(system$mem_used_pct, 2),
    disk_available_mb = round(disk$available_mb, 2),
    disk_used_pct = round(disk$used_pct, 2),
    tasks = utils::head(tasks, 4L)
  )
}

ugplot_job_result_preview <- function(result) {
  if (!is.list(result)) {
    return(result)
  }
  if (identical(result$kind %||% "", "geo_pipeline")) {
    preview <- result
    if (is.list(preview$tables) && is.list(preview$tables$transcript_group_datasets)) {
      preview$tables$transcript_group_datasets <- NULL
      preview$tables$transcript_group_datasets_omitted <- TRUE
    }
    return(preview)
  }
  preview <- list(
    results_table = result$results_table %||% data.frame(),
    final_summary = result$final_summary %||% NULL,
    partial = isTRUE(result$partial),
    updated_at = result$updated_at %||% as.character(Sys.time())
  )
  if (is.data.frame(preview$results_table)) {
    keep_columns <- intersect(
      c("Model", "R2", "Accuracy", "MAE", "RMSE", "dataset_seed", "training_seed", "Status", "Error"),
      names(preview$results_table)
    )
    preview$results_table <- preview$results_table[, keep_columns, drop = FALSE]
  }
  preview
}

ugplot_job_partial_result <- function(result) {
  if (!is.list(result)) {
    return(result)
  }
  partial_result <- result
  partial_result$best_model <- NULL
  partial_result$predictions <- NULL
  partial_result$partial_model_omitted <- TRUE
  partial_result
}

ugplot_attach_job_best_model <- function(result, status) {
  if (!is.list(result)) {
    return(result)
  }
  best_model_path <- status$best_model_path %||% ""
  if (nzchar(best_model_path) && file.exists(best_model_path)) {
    best_model <- tryCatch(readRDS(best_model_path), error = function(e) NULL)
    if (!is.null(best_model)) {
      result$best_model <- best_model
    }
  }
  result
}

ugplot_job_completed_run_keys <- function(result) {
  if (!is.list(result) || !is.data.frame(result$results_table)) {
    return(character(0))
  }
  rows <- result$results_table
  required_columns <- c("Model", "dataset_seed", "training_seed")
  if (!all(required_columns %in% names(rows))) {
    return(character(0))
  }
  keys <- paste(
    as.character(rows$Model),
    as.character(suppressWarnings(as.integer(rows$dataset_seed))),
    as.character(suppressWarnings(as.integer(rows$training_seed))),
    sep = "\r"
  )
  unique(keys[nzchar(keys) & !is.na(keys)])
}

ugplot_write_rds_atomic <- function(object, path) {
  ugplot_ensure_dir(dirname(path))
  tmp_path <- paste0(path, ".tmp-", Sys.getpid(), "-", as.integer(stats::runif(1, 1, 1e9)))
  saveRDS(object, tmp_path)
  try(Sys.chmod(tmp_path, mode = "0600"), silent = TRUE)
  replaced <- if (.Platform$OS.type == "windows" && file.exists(path)) {
    backup_path <- paste0(path, ".backup-", Sys.getpid())
    unlink(backup_path, force = TRUE)
    backed_up <- file.rename(path, backup_path)
    installed <- isTRUE(backed_up) && file.rename(tmp_path, path)
    if (!isTRUE(installed) && isTRUE(backed_up)) {
      file.rename(backup_path, path)
    }
    if (isTRUE(installed)) unlink(backup_path, force = TRUE)
    installed
  } else {
    file.rename(tmp_path, path)
  }
  if (!isTRUE(replaced)) {
    unlink(tmp_path)
    stop("Could not write file: ", path, call. = FALSE)
  }
  try(Sys.chmod(path, mode = "0600"), silent = TRUE)
  invisible(path)
}

ugplot_read_rds_or_null <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path)
}

ugplot_windows_tasklist_args <- function(pid) {
  c("/FI", shQuote(paste0("PID eq ", as.integer(pid))), "/NH")
}

ugplot_process_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (is.na(pid) || pid <= 0) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    output <- tryCatch(
      suppressWarnings(system2("tasklist", ugplot_windows_tasklist_args(pid), stdout = TRUE, stderr = FALSE)),
      error = function(e) character()
    )
    return(any(grepl(paste0("\\b", pid, "\\b"), output)))
  }
  result <- tryCatch(tools::pskill(pid, signal = 0), error = function(e) FALSE)
  isTRUE(result)
}

ugplot_terminate_process <- function(pid) {
  if (.Platform$OS.type == "windows") {
    system2("taskkill", c("/PID", as.character(as.integer(pid)), "/T", "/F"), stdout = FALSE, stderr = FALSE)
    return(invisible(TRUE))
  }
  tools::pskill(as.integer(pid), signal = tools::SIGTERM)
  Sys.sleep(0.5)
  if (ugplot_process_alive(pid)) {
    tools::pskill(as.integer(pid), signal = tools::SIGKILL)
  }
  invisible(TRUE)
}

ugplot_status_time <- function(value) {
  parsed <- tryCatch(
    as.POSIXct(value, format = "%Y-%m-%d %H:%M:%S %z"),
    error = function(e) as.POSIXct(NA)
  )
  if (is.na(parsed)) {
    parsed <- tryCatch(as.POSIXct(value), error = function(e) as.POSIXct(NA))
  }
  parsed
}

ugplot_job_timeout_seconds <- function(status) {
  timeout <- suppressWarnings(as.numeric(status$timeout %||% NA_real_))
  if (length(timeout) == 0 || is.na(timeout) || timeout <= 0) {
    return(NA_real_)
  }
  max(1, timeout)
}

ugplot_find_job_by_request_id <- function(request_id, jobs_dir = ugplot_default_jobs_dir()) {
  request_id <- trimws(as.character(request_id %||% ""))
  if (!nzchar(request_id) || !dir.exists(jobs_dir)) {
    return(NULL)
  }
  job_dirs <- list.dirs(jobs_dir, full.names = TRUE, recursive = FALSE)
  for (job_dir in job_dirs) {
    config_path <- file.path(job_dir, "config.rds")
    if (!file.exists(config_path)) {
      next
    }
    config <- tryCatch(readRDS(config_path), error = function(e) NULL)
    if (is.list(config) && identical(as.character(config$request_id %||% ""), request_id)) {
      return(tryCatch(ugplot_read_job_status(basename(job_dir), jobs_dir), error = function(e) NULL))
    }
  }
  NULL
}

ugplot_running_job_timed_out <- function(status) {
  if (!identical(status$state %||% "", "running")) {
    return(FALSE)
  }
  timeout <- ugplot_job_timeout_seconds(status)
  if (is.na(timeout)) {
    return(FALSE)
  }
  updated_at <- ugplot_status_time(status$updated_at %||% NA_character_)
  if (is.na(updated_at)) {
    return(FALSE)
  }
  watchdog_multiplier <- suppressWarnings(as.numeric(status$watchdog_timeout_multiplier %||% NA_real_))
  if (is.na(watchdog_multiplier) || watchdog_multiplier < 1) {
    watchdog_multiplier <- 3
  }
  grace <- max(300, min(1800, timeout * 0.5))
  age <- as.numeric(difftime(Sys.time(), updated_at, units = "secs"))
  is.finite(age) && age > ((timeout * watchdog_multiplier) + grace)
}

ugplot_create_job <- function(dataset, config = list(), jobs_dir = ugplot_default_jobs_dir(), type = "ml") {
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data.frame.", call. = FALSE)
  }
  if (!is.list(config)) {
    stop("config must be a list.", call. = FALSE)
  }

  ugplot_ensure_dir(jobs_dir)
  job_id <- ugplot_new_job_id()
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  ugplot_ensure_dir(job_dir)
  try(Sys.chmod(job_dir, mode = "0700"), silent = TRUE)

  dataset_path <- file.path(job_dir, "dataset.rds")
  config_path <- file.path(job_dir, "config.rds")
  saveRDS(dataset, dataset_path)
  saveRDS(config, config_path)
  try(Sys.chmod(c(dataset_path, config_path), mode = "0600"), silent = TRUE)

  status <- list(
    id = job_id,
    name = config$job_name %||% "",
    type = type,
    state = "queued",
    progress = 0,
    message = "Queued",
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    pid = NA_integer_,
    error = NULL,
    result_path = NULL,
    partial_result_path = NULL,
    config_summary = ugplot_config_summary(config),
    timeout = suppressWarnings(as.numeric(config$timeout %||% NA_real_)),
    watchdog_timeout_multiplier = suppressWarnings(as.numeric(config$watchdog_timeout_multiplier %||% 3))
  )
  status$internal_worker_task <- isTRUE(config$internal_worker_task)
  status$parent_job_id <- as.character(config$parent_job_id %||% "")
  status$worker_name <- as.character(config$worker_name %||% "")
  status$request_id <- as.character(config$request_id %||% "")
  ugplot_write_job_status(job_id, status, jobs_dir)
  status
}

ugplot_read_job_status <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  if (is.null(status)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }
  ugplot_refresh_job_status(status, jobs_dir)
}

# Return the progress already persisted by a worker without loading its job
# configuration or rebuilding any detailed monitoring data. Distributed
# coordinators poll this frequently, so it must stay inexpensive even while a
# worker is using all available CPU. A dead or timed-out process still goes
# through the regular refresh once so retry/resume semantics remain intact.
ugplot_read_job_status_lightweight <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  if (is.null(status)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }

  state <- as.character(status$state %||% "")
  pid <- suppressWarnings(as.integer(status$pid %||% NA_integer_))
  should_check <- state %in% c("running", "draining") && !is.na(pid)
  if (isTRUE(should_check)) {
    alive <- ugplot_process_alive(pid)
    timed_out <- isTRUE(alive) && ugplot_running_job_timed_out(status)
    if (!isTRUE(alive) || isTRUE(timed_out)) {
      return(ugplot_refresh_job_status(status, jobs_dir))
    }
  }

  status$resumable <- ugplot_job_resumable(status, jobs_dir)
  status$config_summary <- status$config_summary %||% list(target = "", models = "")
  status
}

ugplot_job_resumable <- function(status, jobs_dir = ugplot_default_jobs_dir()) {
  if (!is.list(status) || is.null(status$id)) {
    return(FALSE)
  }
  state <- status$state %||% ""
  if (state %in% c("queued", "running", "draining", "finished")) {
    return(FALSE)
  }
  job_dir <- ugplot_job_dir(status$id, jobs_dir)
  file.exists(file.path(job_dir, "dataset.rds")) && file.exists(file.path(job_dir, "config.rds"))
}

ugplot_job_config_summary <- function(status, jobs_dir = ugplot_default_jobs_dir()) {
  empty_summary <- list(target = "", models = "")
  if (!is.list(status) || is.null(status$id)) {
    return(empty_summary)
  }
  config_path <- file.path(ugplot_job_dir(status$id, jobs_dir), "config.rds")
  if (!file.exists(config_path)) {
    return(empty_summary)
  }
  config <- tryCatch(readRDS(config_path), error = function(e) list())
  ugplot_config_summary(config)
}

ugplot_config_summary <- function(config) {
  empty_summary <- list(target = "", models = "")
  if (!is.list(config)) {
    return(empty_summary)
  }
  models <- config$models %||% config$model_names %||% character(0)
  if (identical(config$type %||% "", "geo") || identical(config$runner %||% "", "ugplot_run_geo_pipeline_job")) {
    return(list(
      target = as.character(config$accession %||% ""),
      models = paste(c(config$matrix_source %||% "", config$target_column %||% ""), collapse = " / ")
    ))
  }
  list(
    target = as.character(config$target %||% config$target_name %||% ""),
    models = paste(as.character(models), collapse = ", ")
  )
}

ugplot_write_job_status <- function(job_id, status, jobs_dir = ugplot_default_jobs_dir()) {
  status$id <- job_id
  status$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ugplot_write_rds_atomic(status, ugplot_status_path(job_id, jobs_dir))
  invisible(status)
}

ugplot_update_job_status <- function(job_id, jobs_dir = ugplot_default_jobs_dir(), ...) {
  status <- ugplot_read_job_status(job_id, jobs_dir)
  updates <- list(...)
  for (name in names(updates)) {
    status[[name]] <- updates[[name]]
  }
  ugplot_write_job_status(job_id, status, jobs_dir)
}

ugplot_write_job_partial_result <- function(job_id, result, jobs_dir = ugplot_default_jobs_dir()) {
  partial_path <- ugplot_result_path(job_id, jobs_dir, partial = TRUE)
  preview_path <- ugplot_preview_result_path(job_id, jobs_dir)
  best_model_path <- ugplot_best_model_path(job_id, jobs_dir)
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  completed_keys <- unique(c(
    status$resume_completed_keys %||% character(0),
    ugplot_job_completed_run_keys(result)
  ))
  best_model_signature <- paste(
    result$best_model_name %||% "",
    result$final_summary$dataset_seed %||% "",
    result$final_summary$training_seed %||% "",
    sep = "\r"
  )
  best_model_updates <- list()
  if (!is.null(result$best_model) &&
      nzchar(best_model_signature) &&
      !identical(status$best_model_signature %||% "", best_model_signature)) {
    ugplot_write_rds_atomic(result$best_model, best_model_path)
    best_model_updates$best_model_path <- best_model_path
    best_model_updates$best_model_signature <- best_model_signature
  }
  ugplot_write_rds_atomic(ugplot_job_partial_result(result), partial_path)
  ugplot_write_rds_atomic(ugplot_job_result_preview(result), preview_path)
  status_updates <- c(
    list(
      partial_result_path = partial_path,
      preview_result_path = preview_path,
      resume_completed_keys = completed_keys,
      partial_saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ),
    best_model_updates
  )
  do.call(ugplot_update_job_status, c(list(job_id = job_id, jobs_dir = jobs_dir), status_updates))
  invisible(partial_path)
}

ugplot_stop_job <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  if (is.null(status)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }
  state <- status$state %||% ""
  pid <- suppressWarnings(as.integer(status$pid %||% NA_integer_))

  if (state %in% c("finished", "failed", "stopped")) {
    return(status)
  }

  if (!is.na(pid) && ugplot_process_alive(pid)) {
    ugplot_terminate_process(pid)
  }

  partial_path <- status$partial_result_path %||% ugplot_result_path(job_id, jobs_dir, partial = TRUE)
  has_partial <- !is.null(partial_path) && file.exists(partial_path)
  distributed_active <- is.list(status$distributed_state) &&
    suppressWarnings(as.integer(status$distributed_state$active %||% 0L)) > 0L
  stop_message <- if (isTRUE(distributed_active)) {
    "Coordinator stopped; active worker tasks may finish and will be collected on Resume"
  } else if (has_partial) {
    "Stopped; partial result is available"
  } else {
    "Stopped"
  }
  ugplot_update_job_status(
    job_id,
    jobs_dir,
    state = "stopped",
    message = stop_message,
    error = NULL,
    result_path = if (has_partial) partial_path else status$result_path
  )
  ugplot_read_job_status(job_id, jobs_dir)
}

ugplot_delete_job <- function(job_id, jobs_dir = ugplot_default_jobs_dir(), force = FALSE) {
  job_id <- ugplot_validate_job_id(job_id)
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  if (!dir.exists(job_dir)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }

  status <- ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir))
  state <- status$state %||% ""
  pid <- suppressWarnings(as.integer(status$pid %||% NA_integer_))
  is_active <- state %in% c("queued", "running", "draining") && !is.na(pid) && ugplot_process_alive(pid)
  if (is_active && !isTRUE(force)) {
    stop("Stop the job before deleting it.", call. = FALSE)
  }
  if (is_active && isTRUE(force)) {
    ugplot_terminate_process(pid)
  }

  removed <- unlink(job_dir, recursive = TRUE, force = TRUE)
  if (!identical(removed, 0L) || dir.exists(job_dir)) {
    stop("Could not delete job: ", job_id, call. = FALSE)
  }
  list(id = job_id, deleted = TRUE)
}

ugplot_refresh_job_status <- function(status, jobs_dir = ugplot_default_jobs_dir()) {
  state <- status$state %||% ""
  pid <- status$pid %||% NA_integer_
  should_check_pid <- state %in% c("queued", "running", "draining") && !is.na(suppressWarnings(as.integer(pid)))
  if (!should_check_pid) {
    status$resumable <- ugplot_job_resumable(status, jobs_dir)
    status$config_summary <- ugplot_job_config_summary(status, jobs_dir)
    return(status)
  }

  if (ugplot_process_alive(pid)) {
    if (!ugplot_running_job_timed_out(status)) {
      status$resumable <- FALSE
      status$config_summary <- ugplot_job_config_summary(status, jobs_dir)
      return(status)
    }
    ugplot_terminate_process(pid)
    partial_path <- status$partial_result_path %||% ugplot_result_path(status$id, jobs_dir, partial = TRUE)
    has_partial <- !is.null(partial_path) && file.exists(partial_path)
    status$state <- if (has_partial) "stopped" else "failed"
    status$message <- if (has_partial) "Timed out; partial result is available" else "Timed out"
    status$error <- paste0("The job process exceeded the configured timeout without a progress update.")
    if (has_partial) {
      status$result_path <- partial_path
    }
    ugplot_write_job_status(status$id, status, jobs_dir)
    status$resumable <- ugplot_job_resumable(status, jobs_dir)
    status$config_summary <- ugplot_job_config_summary(status, jobs_dir)
    return(status)
  }

  status$state <- "failed"
  status$message <- "Background process stopped before finishing"
  status$error <- "The job process is no longer running. The server may have restarted or crashed."
  status$progress <- status$progress %||% 0
  ugplot_write_job_status(status$id, status, jobs_dir)
  status$resumable <- ugplot_job_resumable(status, jobs_dir)
  status$config_summary <- ugplot_job_config_summary(status, jobs_dir)
  status
}

ugplot_list_jobs <- function(jobs_dir = ugplot_default_jobs_dir(), include_internal = FALSE,
                             lightweight = FALSE) {
  if (!dir.exists(jobs_dir)) {
    return(data.frame())
  }
  job_ids <- basename(list.dirs(jobs_dir, full.names = TRUE, recursive = FALSE))
  statuses <- lapply(job_ids, function(job_id) {
    status <- tryCatch(
      ugplot_read_rds_or_null(ugplot_status_path(job_id, jobs_dir)),
      error = function(e) NULL
    )
    if (!is.list(status) || (!isTRUE(include_internal) && isTRUE(status$internal_worker_task))) {
      return(NULL)
    }
    if (isTRUE(lightweight)) {
      # `resumable` may have been persisted as FALSE while the process was
      # running. Re-evaluate it from the current state and the two small file
      # existence checks so a server restart immediately exposes Resume.
      status$resumable <- ugplot_job_resumable(status, jobs_dir)
      status$config_summary <- status$config_summary %||% list(target = "", models = "")
      return(status)
    }
    tryCatch(ugplot_refresh_job_status(status, jobs_dir), error = function(e) NULL)
  })
  statuses <- Filter(Negate(is.null), statuses)
  if (length(statuses) == 0) {
    return(data.frame())
  }
  rows <- lapply(statuses, function(status) {
    data.frame(
      id = status$id %||% NA_character_,
      name = status$name %||% NA_character_,
      type = status$type %||% NA_character_,
      state = status$state %||% NA_character_,
      progress = status$progress %||% NA_real_,
      message = status$message %||% NA_character_,
      target = status$config_summary$target %||% NA_character_,
      models = status$config_summary$models %||% NA_character_,
      created_at = status$created_at %||% NA_character_,
      updated_at = status$updated_at %||% NA_character_,
      pid = status$pid %||% NA_integer_,
      execution = paste(status$distributed_state$workers %||% character(0), collapse = " + "),
      tasks = if (is.list(status$distributed_state) &&
                  is.finite(suppressWarnings(as.numeric(status$distributed_state$total %||% NA_real_)))) {
        paste0(status$distributed_state$completed %||% 0L, "/", status$distributed_state$total)
      } else {
        ""
      },
      resumable = isTRUE(status$resumable %||% ugplot_job_resumable(status, jobs_dir)),
      stringsAsFactors = FALSE
    )
  })
  jobs <- do.call(rbind, rows)
  jobs[order(jobs$created_at, decreasing = TRUE), , drop = FALSE]
}

ugplot_append_job_log <- function(job_id, message, jobs_dir = ugplot_default_jobs_dir()) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), " ", message)
  cat(line, "\n", file = file.path(ugplot_job_dir(job_id, jobs_dir), "log.txt"), append = TRUE)
  invisible(line)
}

ugplot_read_job_log <- function(job_id, jobs_dir = ugplot_default_jobs_dir(), max_lines = 200L) {
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  log_paths <- c(
    file.path(job_dir, "log.txt"),
    file.path(job_dir, "stdout.log"),
    file.path(job_dir, "stderr.log"),
    utils::tail(sort(list.files(file.path(job_dir, "model-logs"), pattern = "\\.log$", full.names = TRUE)), 12)
  )
  sections <- lapply(log_paths[file.exists(log_paths)], function(path) {
    lines <- readLines(path, warn = FALSE)
    lines <- utils::tail(lines, max(1L, as.integer(max_lines)))
    c(paste0("== ", basename(path), " =="), lines)
  })
  paste(unlist(sections, use.names = FALSE), collapse = "\n")
}

ugplot_read_job_result <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_job_status(job_id, jobs_dir)
  result_path <- status$result_path %||% status$partial_result_path
  if (is.null(result_path) || !file.exists(result_path)) {
    stop("Result is not available for job: ", job_id, call. = FALSE)
  }
  ugplot_attach_job_best_model(readRDS(result_path), status)
}

ugplot_read_job_preview_result <- function(job_id, jobs_dir = ugplot_default_jobs_dir()) {
  status <- ugplot_read_job_status(job_id, jobs_dir)
  preview_path <- status$preview_result_path %||% ugplot_preview_result_path(job_id, jobs_dir)
  if (!is.null(preview_path) && file.exists(preview_path)) {
    return(readRDS(preview_path))
  }
  ugplot_job_result_preview(ugplot_read_job_result(job_id, jobs_dir))
}

ugplot_redact_job_config <- function(config) {
  if (!is.list(config)) {
    return(config)
  }
  if (is.list(config$distributed_workers)) {
    config$distributed_workers <- lapply(config$distributed_workers, function(worker) {
      if (is.list(worker) && "token" %in% names(worker)) {
        worker$token <- ""
      }
      worker
    })
  }
  config
}

ugplot_read_job_bundle <- function(job_id, jobs_dir = ugplot_default_jobs_dir(), allow_active = FALSE) {
  job_dir <- ugplot_job_dir(job_id, jobs_dir)
  if (!dir.exists(job_dir)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }
  status <- ugplot_read_job_status(job_id, jobs_dir)
  if (!isTRUE(allow_active) && status$state %in% c("queued", "running", "draining")) {
    stop("Full job bundle is not available while the job is active. Use preview, or stop/wait before Load.", call. = FALSE)
  }
  dataset_path <- file.path(job_dir, "dataset.rds")
  config_path <- file.path(job_dir, "config.rds")
  if (!file.exists(dataset_path) || !file.exists(config_path)) {
    stop("Job dataset/config is not available for job: ", job_id, call. = FALSE)
  }
  list(
    id = job_id,
    status = status,
    dataset = readRDS(dataset_path),
    config = ugplot_redact_job_config(readRDS(config_path)),
    result = tryCatch(ugplot_read_job_result(job_id, jobs_dir), error = function(e) NULL)
  )
}
