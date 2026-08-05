ugplot_science_collab_url <- function(coordinator, default_port = 8080L) {
  coordinator <- trimws(as.character(coordinator %||% ""))
  if (length(coordinator) != 1L || !nzchar(coordinator)) {
    stop("Provide the Science Collab coordinator IP or URL.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z][A-Za-z0-9+.-]*://", coordinator)) {
    coordinator <- paste0("http://", coordinator)
    authority <- sub("^http://", "", coordinator)
    authority <- sub("/.*$", "", authority)
    if (!grepl(":[0-9]+$", authority)) {
      coordinator <- sub(
        "^http://([^/]+)",
        paste0("http://\\1:", as.integer(default_port)),
        coordinator
      )
    }
  }
  if (!grepl("^https?://[^/[:space:]]+", coordinator, ignore.case = TRUE)) {
    stop("The coordinator must be an HTTP(S) URL or a server IP/hostname.", call. = FALSE)
  }
  sub("/+$", "", coordinator)
}

ugplot_science_collab_client_packages <- function() {
  c("httr", "jsonlite", "processx")
}

ugplot_science_collab_state_dir <- function() {
  configured <- trimws(Sys.getenv("UGPLOT_SCIENCE_COLLAB_STATE_DIR", unset = ""))
  state_dir <- if (nzchar(configured)) configured else file.path(path.expand("~"), ".ugplot", "science-collab")
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
  state_dir
}

ugplot_science_collab_state_path <- function(name = "default") {
  safe_name <- gsub("[^A-Za-z0-9._-]+", "_", as.character(name))
  file.path(ugplot_science_collab_state_dir(), paste0(safe_name, ".rds"))
}

ugplot_read_science_collab_state <- function(name = "default") {
  path <- ugplot_science_collab_state_path(name)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

ugplot_write_science_collab_state <- function(state, name = "default") {
  saveRDS(state, ugplot_science_collab_state_path(name))
  invisible(state)
}

ugplot_install_science_collab_client_deps <- function(install = TRUE, dependencies = TRUE) {
  packages <- ugplot_science_collab_client_packages()
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (isTRUE(install) && length(missing) > 0L) {
    message("Installing Science Collab client packages: ", paste(missing, collapse = ", "))
    utils::install.packages(missing, dependencies = dependencies)
    missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  }
  if (length(missing) > 0L) {
    stop(
      "Missing Science Collab client packages: ", paste(missing, collapse = ", "),
      ". Install them and try again.",
      call. = FALSE
    )
  }
  invisible(packages)
}

ugplot_science_collab_worker <- function(payload_path, cpu_limit) {
  event_path <- tempfile("ugplot-collab-events-", fileext = ".rds")
  result_path <- tempfile("ugplot-collab-result-", fileext = ".rds")
  launcher_path <- tempfile("ugplot-collab-launcher-", fileext = ".R")
  lib_paths_path <- tempfile("ugplot-collab-libs-", fileext = ".rds")
  stdout_path <- tempfile("ugplot-collab-worker-", fileext = ".stdout.log")
  stderr_path <- tempfile("ugplot-collab-worker-", fileext = ".stderr.log")
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    ".libPaths(readRDS(args[[5]]))",
    "library(ugplot)",
    "runner <- get('ugplot_collaboration_run_payload', envir = asNamespace('ugplot'))",
    "payload <- readRDS(args[[1]])",
    "result <- runner(payload, cpu_limit = as.integer(args[[2]]), event_path = args[[3]])",
    "saveRDS(result, args[[4]])"
  ), launcher_path, useBytes = TRUE)
  saveRDS(.libPaths(), lib_paths_path)
  process <- processx::process$new(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(
      "--vanilla", launcher_path, payload_path, as.character(cpu_limit),
      event_path, result_path, lib_paths_path
    ),
    stdout = stdout_path,
    stderr = stderr_path,
    cleanup = TRUE,
    cleanup_tree = TRUE,
    windows_hide_window = TRUE
  )
  list(
    process = process,
    event_path = event_path,
    result_path = result_path,
    files = c(
      payload_path, event_path, result_path, launcher_path, lib_paths_path,
      stdout_path, stderr_path
    ),
    stdout_path = stdout_path,
    stderr_path = stderr_path
  )
}

ugplot_science_collab_latest_telemetry <- function(event_path) {
  events <- if (file.exists(event_path)) {
    tryCatch(readRDS(event_path), error = function(e) list())
  } else {
    list()
  }
  latest <- function(types) {
    matches <- Filter(function(event) as.character(event$type %||% "") %in% types, events)
    if (length(matches) == 0L) list() else utils::tail(matches, 1L)[[1]]$data %||% list()
  }
  progress <- latest(c("progress_updated", "experiment_started"))
  metric <- latest("metric_updated")
  list(
    progress = progress$progress %||% 0,
    message = progress$message %||% "Collaborative experiment running",
    candidate = progress$candidate %||% metric$candidate %||% "",
    completed = metric$completed %||% 0L
  )
}

ugplot_science_collab_run_mission <- function(claimed, server_url, client_id, cpu_limit) {
  task <- claimed$task
  task_id <- as.character(task$task_id %||% "")
  lease_id <- as.character(task$lease_id %||% "")
  worker <- NULL
  delivered <- FALSE
  on.exit({
    if (!is.null(worker)) {
      if (worker$process$is_alive()) try(worker$process$kill_tree(), silent = TRUE)
      unlink(worker$files, force = TRUE)
    } else {
      unlink(claimed$payload_path %||% "", force = TRUE)
    }
    if (!isTRUE(delivered) && nzchar(task_id) && nzchar(lease_id)) {
      try(
        ugplot_remote_collaboration_release(
          server_url, task_id, lease_id, client_id
        ),
        silent = TRUE
      )
    }
  }, add = TRUE)

  required_models <- unique(as.character(task$requirements$models %||% character()))
  available_now <- ugplot_model_dependency_status(models = required_models)
  missing <- unique(c(available_now$models_missing, available_now$unknown_models))
  if (length(missing) > 0L) {
    message("Releasing incompatible mission; missing models: ", paste(missing, collapse = ", "))
    ugplot_remote_collaboration_release(server_url, task_id, lease_id, client_id)
    delivered <- TRUE
    return(FALSE)
  }

  message("Mission ", task_id, " received; starting computation.")
  worker <- ugplot_science_collab_worker(claimed$payload_path, cpu_limit)
  last_heartbeat <- as.POSIXct(NA)
  while (worker$process$is_alive()) {
    if (is.na(last_heartbeat) || difftime(Sys.time(), last_heartbeat, units = "secs") >= 25) {
      heartbeat <- ugplot_remote_collaboration_heartbeat(
        server_url, task_id, lease_id, client_id,
        telemetry = ugplot_science_collab_latest_telemetry(worker$event_path)
      )
      if (!isTRUE(heartbeat$accepted)) {
        stop("The coordinator no longer accepts this mission lease.", call. = FALSE)
      }
      last_heartbeat <- Sys.time()
    }
    Sys.sleep(2)
  }

  exit_status <- worker$process$get_exit_status()
  if (!identical(exit_status, 0L) || !file.exists(worker$result_path)) {
    error_lines <- if (file.exists(worker$stderr_path)) {
      utils::tail(readLines(worker$stderr_path, warn = FALSE), 20L)
    } else {
      character()
    }
    stop(
      paste(c("Science Collab mission failed.", error_lines), collapse = "\n"),
      call. = FALSE
    )
  }
  response <- ugplot_remote_collaboration_complete(
    server_url, task_id, lease_id, client_id, readRDS(worker$result_path)
  )
  delivered <- TRUE
  message(
    "Mission ", task_id,
    if (isTRUE(response$accepted)) " accepted." else " was already completed elsewhere."
  )
  isTRUE(response$accepted)
}

#' Run a headless Science Collab client
#'
#' Connects this machine to the public collaboration endpoint of an ugPlot
#' coordinator. The client can claim and complete published Science Collab
#' missions, but it does not receive or use the coordinator's administrative
#' bearer token and therefore cannot submit arbitrary remote jobs.
#'
#' @param coordinator Coordinator IP, hostname, or HTTP(S) URL. Bare hosts use
#'   port 8080.
#' @param scientist_name Public name shown for this contributor.
#' @param cpu_limit Maximum CPU cores offered to a mission.
#' @param install_model_deps Whether to attempt installation of dependencies for
#'   every missing caret model before connecting.
#' @param install_client_deps Whether to install the small set of packages needed
#'   by the headless client.
#' @param poll_seconds Seconds between attempts when no compatible mission is
#'   waiting.
#' @param max_missions Maximum missions to complete before returning. The
#'   default keeps the client running until interrupted.
#' @return Invisibly returns contribution counters when the client stops.
#' @export
ugPlotScienceCollab <- function(coordinator, scientist_name,
                                cpu_limit = max(1L, parallel::detectCores() - 1L),
                                install_model_deps = TRUE,
                                install_client_deps = TRUE,
                                poll_seconds = 6,
                                max_missions = Inf) {
  server_url <- ugplot_science_collab_url(coordinator)
  scientist_name <- trimws(as.character(scientist_name %||% ""))
  if (length(scientist_name) != 1L || !nzchar(scientist_name)) {
    stop("Provide the scientist name shown by Science Collab.", call. = FALSE)
  }
  cpu_limit <- suppressWarnings(as.integer(cpu_limit))
  if (length(cpu_limit) != 1L || is.na(cpu_limit) || cpu_limit < 1L) {
    stop("cpu_limit must be a positive integer.", call. = FALSE)
  }
  poll_seconds <- max(1, suppressWarnings(as.numeric(poll_seconds)))
  max_missions <- suppressWarnings(as.numeric(max_missions))
  if (length(max_missions) != 1L || is.na(max_missions) || max_missions < 0) {
    stop("max_missions must be zero, a positive number, or Inf.", call. = FALSE)
  }

  ugplot_install_science_collab_client_deps(
    install = install_client_deps,
    dependencies = TRUE
  )
  if (isTRUE(install_model_deps)) {
    message("Checking and attempting to install dependencies for all caret models...")
    tryCatch(
      ugPlotInstallModelDeps(),
      error = function(e) warning(
        "Some model dependencies could not be installed: ", conditionMessage(e),
        call. = FALSE
      )
    )
  }
  model_status <- ugplot_model_dependency_status()
  client_id <- paste0(
    "headless-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
    Sys.getpid(), "-", sample(1000:9999, 1L)
  )
  capabilities <- list(
    models = unique(as.character(model_status$models_installed)),
    cpu_limit = cpu_limit,
    protocol_version = 1L,
    scientist_name = scientist_name
  )
  counters <- list(completed = 0L, accepted = 0L, server_url = server_url)
  message(
    "Science Collab client ready at ", server_url, " as ", scientist_name,
    " (", cpu_limit, " CPU core", if (cpu_limit == 1L) "" else "s", ")."
  )
  message("Waiting for public missions. Press Ctrl+C to stop safely.")

  while (counters$completed < max_missions) {
    claimed <- tryCatch(
      ugplot_remote_collaboration_claim(server_url, client_id, capabilities),
      error = function(e) {
        message("Coordinator unavailable: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(claimed)) {
      Sys.sleep(poll_seconds)
      next
    }
    accepted <- ugplot_science_collab_run_mission(
      claimed, server_url, client_id, cpu_limit
    )
    counters$completed <- counters$completed + 1L
    if (isTRUE(accepted)) counters$accepted <- counters$accepted + 1L
  }
  invisible(counters)
}

#' Start a Science Collab client in the background
#'
#' Starts code{ugPlotScienceCollab()} in a detached R process and keeps its
#' PID, configuration, and log location under code{~/.ugplot/science-collab}.
#'
#' @inheritParams ugPlotScienceCollab
#' @param name Local handle used by the status and stop functions.
#' @return Invisibly returns the background client state.
#' @export
ugPlotScienceCollabStart <- function(coordinator, scientist_name,
                                     cpu_limit = max(1L, parallel::detectCores() - 1L),
                                     install_model_deps = TRUE,
                                     install_client_deps = TRUE,
                                     poll_seconds = 6,
                                     name = "default") {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required to start Science Collab in the background.", call. = FALSE)
  }
  server_url <- ugplot_science_collab_url(coordinator)
  scientist_name <- trimws(as.character(scientist_name %||% ""))
  if (length(scientist_name) != 1L || !nzchar(scientist_name)) {
    stop("Provide the scientist name shown by Science Collab.", call. = FALSE)
  }
  cpu_limit <- suppressWarnings(as.integer(cpu_limit))
  if (length(cpu_limit) != 1L || is.na(cpu_limit) || cpu_limit < 1L) {
    stop("cpu_limit must be a positive integer.", call. = FALSE)
  }
  poll_seconds <- suppressWarnings(as.numeric(poll_seconds))
  if (length(poll_seconds) != 1L || !is.finite(poll_seconds) || poll_seconds < 1) {
    stop("poll_seconds must be at least one second.", call. = FALSE)
  }
  name <- trimws(as.character(name %||% "default"))
  if (length(name) != 1L || !nzchar(name)) stop("name must not be empty.", call. = FALSE)

  current <- ugplot_read_science_collab_state(name)
  if (is.list(current) && ugplot_process_alive(current$pid %||% NA_integer_)) {
    message(
      "Science Collab client is already running as ", name,
      " (pid ", current$pid, ")."
    )
    return(invisible(current))
  }

  state_dir <- ugplot_science_collab_state_dir()
  safe_name <- gsub("[^A-Za-z0-9._-]+", "_", name)
  log_file <- file.path(state_dir, paste0(safe_name, ".log"))
  lib_paths <- .libPaths()
  process <- callr::r_bg(
    func = function(server_url, scientist_name, cpu_limit,
                    install_model_deps, install_client_deps,
                    poll_seconds, lib_paths) {
      .libPaths(lib_paths)
      library(ugplot)
      ugPlotScienceCollab(
        coordinator = server_url,
        scientist_name = scientist_name,
        cpu_limit = cpu_limit,
        install_model_deps = install_model_deps,
        install_client_deps = install_client_deps,
        poll_seconds = poll_seconds
      )
    },
    args = list(
      server_url = server_url,
      scientist_name = scientist_name,
      cpu_limit = cpu_limit,
      install_model_deps = isTRUE(install_model_deps),
      install_client_deps = isTRUE(install_client_deps),
      poll_seconds = poll_seconds,
      lib_paths = lib_paths
    ),
    stdout = log_file,
    stderr = log_file,
    supervise = FALSE,
    cleanup = FALSE,
    poll_connection = FALSE
  )
  Sys.sleep(0.25)
  if (!process$is_alive()) {
    log_lines <- if (file.exists(log_file)) utils::tail(readLines(log_file, warn = FALSE), 30L) else character(0)
    stop(
      paste(c("Science Collab client stopped during startup.", log_lines), collapse = "\n"),
      call. = FALSE
    )
  }
  state <- list(
    name = name,
    pid = process$get_pid(),
    running = TRUE,
    coordinator = server_url,
    scientist_name = scientist_name,
    cpu_limit = cpu_limit,
    poll_seconds = poll_seconds,
    log_file = log_file,
    started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
  ugplot_write_science_collab_state(state, name)
  message(
    "Science Collab client started as ", name, " (pid ", state$pid,
    "). Log: ", log_file
  )
  invisible(state)
}

#' Get background Science Collab client status
#'
#' @param name Local handle supplied to `ugPlotScienceCollabStart()`.
#' @return Invisibly returns the background client state.
#' @export
ugPlotScienceCollabStatus <- function(name = "default") {
  state <- ugplot_read_science_collab_state(name)
  if (is.null(state)) {
    state <- list(name = name, running = FALSE, message = "No background Science Collab client state found.")
  } else {
    state$running <- ugplot_process_alive(state$pid %||% NA_integer_)
    if (!isTRUE(state$running)) state$message <- "Science Collab client is not running."
  }
  print(state)
  invisible(state)
}

#' Stop a background Science Collab client
#'
#' @param name Local handle supplied to `ugPlotScienceCollabStart()`.
#' @return Invisibly returns TRUE when a client was stopped.
#' @export
ugPlotScienceCollabStop <- function(name = "default") {
  state <- ugplot_read_science_collab_state(name)
  if (is.null(state) || !ugplot_process_alive(state$pid %||% NA_integer_)) {
    message("Science Collab client is not running.")
    return(invisible(FALSE))
  }
  pid <- as.integer(state$pid)
  if (.Platform$OS.type != "windows") {
    # SIGINT lets an active mission unwind its lease before the process tree is
    # forcibly terminated below.
    try(tools::pskill(pid, signal = tools::SIGINT), silent = TRUE)
    deadline <- Sys.time() + 5
    while (ugplot_process_alive(pid) && Sys.time() < deadline) Sys.sleep(0.1)
  }
  if (ugplot_process_alive(pid)) ugplot_terminate_process(pid)
  state$running <- FALSE
  state$stopped_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ugplot_write_science_collab_state(state, name)
  message("Science Collab client stopped (pid ", pid, ").")
  invisible(TRUE)
}
