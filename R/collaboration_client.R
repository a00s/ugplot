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

ugplot_science_collab_work_dir <- function() {
  work_dir <- file.path(ugplot_science_collab_state_dir(), "work")
  if (!dir.exists(work_dir)) {
    created <- dir.create(
      work_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700"
    )
    if (!isTRUE(created) && !dir.exists(work_dir)) {
      stop(
        "Could not create the Science Collab work directory: ", work_dir,
        call. = FALSE
      )
    }
  }
  work_dir
}

ugplot_science_collab_tempfile <- function(pattern = "file", fileext = "") {
  tempfile(
    pattern = pattern,
    tmpdir = ugplot_science_collab_work_dir(),
    fileext = fileext
  )
}

ugplot_science_collab_spool_dir <- function(path = NULL) {
  path <- trimws(as.character(path %||% ""))
  if (length(path) != 1L || !nzchar(path)) {
    path <- file.path(ugplot_science_collab_work_dir(), "pending-delivery")
  }
  if (!dir.exists(path)) {
    created <- dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    if (!isTRUE(created) && !dir.exists(path)) {
      stop("Could not create the Science Collab delivery spool: ", path, call. = FALSE)
    }
  }
  normalizePath(path, mustWork = FALSE)
}

ugplot_science_collab_delivery_path <- function(task_id, lease_id, spool_dir = NULL) {
  safe <- gsub("[^A-Za-z0-9._-]+", "_", paste(task_id, lease_id, sep = "--"))
  file.path(ugplot_science_collab_spool_dir(spool_dir), paste0(safe, ".rds"))
}

ugplot_science_collab_store_delivery <- function(record, spool_dir = NULL) {
  path <- ugplot_science_collab_delivery_path(
    record$task_id %||% "mission", record$lease_id %||% "lease", spool_dir
  )
  ugplot_write_rds_atomic(record, path)
  try(Sys.chmod(path, mode = "0600"), silent = TRUE)
  path
}

ugplot_science_collab_attempt_delivery <- function(path, timeout_seconds = 60) {
  record <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(record)) {
    unlink(path, force = TRUE)
    return(list(done = TRUE, accepted = FALSE, reason = "invalid_spool_record"))
  }
  response <- tryCatch(
    ugplot_remote_collaboration_complete(
      record$server_url, record$task_id, record$lease_id, record$client_id,
      record$result, timeout_seconds = timeout_seconds
    ),
    error = function(e) e
  )
  if (inherits(response, "error")) {
    return(list(done = FALSE, accepted = FALSE, error = conditionMessage(response)))
  }
  accepted <- isTRUE(response$accepted)
  reason <- as.character(response$reason %||% "")
  terminal <- accepted || reason %in% c(
    "already_completed", "lease_expired", "lease_not_active", "task_not_found"
  )
  if (isTRUE(terminal)) unlink(path, force = TRUE)
  list(done = terminal, accepted = accepted, reason = reason)
}

ugplot_science_collab_flush_deliveries <- function(spool_dir = NULL) {
  spool_dir <- ugplot_science_collab_spool_dir(spool_dir)
  paths <- list.files(spool_dir, pattern = "[.]rds$", full.names = TRUE)
  if (length(paths) == 0L) {
    return(list(pending = 0L, completed = 0L, accepted = 0L))
  }
  outcomes <- lapply(paths, ugplot_science_collab_attempt_delivery)
  list(
    pending = sum(!vapply(outcomes, function(item) isTRUE(item$done), logical(1))),
    completed = sum(vapply(outcomes, function(item) isTRUE(item$done), logical(1))),
    accepted = sum(vapply(outcomes, function(item) isTRUE(item$accepted), logical(1)))
  )
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
    utils::install.packages(
      missing,
      dependencies = dependencies,
      repos = ugplot_install_repositories()
    )
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

ugplot_science_collab_preflight <- function(coordinator_status, client_version,
                                             protocol_version = 2L) {
  if (is.null(coordinator_status)) {
    return(list(coordinator_version = "", protocol_version = NA_integer_))
  }
  coordinator_protocol <- suppressWarnings(as.integer(
    coordinator_status$protocol_version %||% NA_integer_
  ))
  if (length(coordinator_protocol) != 1L || is.na(coordinator_protocol) ||
      !identical(coordinator_protocol, as.integer(protocol_version))) {
    stop(
      "Science Collab protocol mismatch: client ", protocol_version,
      " | coordinator ",
      if (length(coordinator_protocol) == 1L && !is.na(coordinator_protocol)) {
        coordinator_protocol
      } else {
        "not reported"
      },
      ". Install a version that supports protocol ", protocol_version, ".",
      call. = FALSE
    )
  }
  coordinator_version <- trimws(as.character(
    coordinator_status$ugplot_build_version %||% ""
  ))
  if (!nzchar(coordinator_version)) {
    warning(
      "Science Collab coordinator did not report its build; protocol ",
      protocol_version, " is compatible, continuing.",
      call. = FALSE
    )
  } else if (!identical(
    ugplot_compare_build_versions(client_version, coordinator_version), 0L
  )) {
    warning(
      "Science Collab build differs: client ", client_version,
      " | coordinator ", coordinator_version,
      ". Protocol ", protocol_version, " is compatible; continuing.",
      call. = FALSE
    )
  }
  list(
    coordinator_version = coordinator_version,
    protocol_version = coordinator_protocol
  )
}

ugplot_science_collab_compatibility_models <- function(report, field) {
  if (!is.list(report) || !field %in% c("required_models", "missing_models")) {
    return(character(0))
  }
  missions <- report$missions %||% list()
  values <- if (is.data.frame(missions)) {
    if (field %in% names(missions)) missions[[field]] else character(0)
  } else {
    lapply(missions, function(mission) {
      if (is.list(mission)) mission[[field]] %||% character(0) else character(0)
    })
  }
  values <- unique(as.character(unlist(values, use.names = FALSE)))
  values[nzchar(values)]
}

ugplot_science_collab_compatibility_count <- function(report, field) {
  value <- suppressWarnings(as.integer(report[[field]] %||% 0L))
  if (length(value) == 0L || is.na(value[[1L]])) 0L else value[[1L]]
}

ugplot_science_collab_compatibility <- function(server_url, model_status) {
  capabilities <- list(
    models = unique(as.character(model_status$models_installed %||% character(0))),
    protocol_version = 2L
  )
  ugplot_remote_collaboration_compatibility(server_url, capabilities)
}

ugplot_science_collab_assert_compatible <- function(report) {
  pending <- ugplot_science_collab_compatibility_count(report, "pending")
  compatible <- ugplot_science_collab_compatibility_count(report, "compatible")
  if (pending == 0L || compatible > 0L) return(invisible(TRUE))

  missing <- ugplot_science_collab_compatibility_models(report, "missing_models")
  detail <- if (length(missing) > 0L) {
    paste0(" Missing caret models: ", paste(missing, collapse = ", "), ".")
  } else {
    " The coordinator did not report the missing model names."
  }
  stop(
    "Science Collab client is incompatible with all ", pending,
    " waiting mission", if (pending == 1L) "" else "s", ".", detail,
    " Install their dependencies and start the client again.",
    call. = FALSE
  )
}

ugplot_science_collab_worker <- function(payload_path, cpu_limit) {
  event_path <- ugplot_science_collab_tempfile("ugplot-collab-events-", ".rds")
  result_path <- ugplot_science_collab_tempfile("ugplot-collab-result-", ".rds")
  launcher_path <- ugplot_science_collab_tempfile("ugplot-collab-launcher-", ".R")
  stdout_path <- ugplot_science_collab_tempfile("ugplot-collab-worker-", ".stdout.log")
  stderr_path <- ugplot_science_collab_tempfile("ugplot-collab-worker-", ".stderr.log")
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    ".libPaths(args[-seq_len(4L)])",
    "library(ugplot)",
    "runner <- get('ugplot_collaboration_run_payload', envir = asNamespace('ugplot'))",
    "payload <- readRDS(args[[1]])",
    "result <- runner(payload, cpu_limit = as.integer(args[[2]]), event_path = args[[3]])",
    "saveRDS(result, args[[4]])"
  ), launcher_path, useBytes = TRUE)
  process <- processx::process$new(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(
      "--vanilla", launcher_path, payload_path, as.character(cpu_limit),
      event_path, result_path, .libPaths()
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
      payload_path, event_path, result_path, launcher_path, stdout_path,
      stderr_path
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

ugplot_science_collab_failure_delay <- function(attempt, poll_seconds) {
  attempt <- suppressWarnings(as.integer(attempt %||% 1L))
  if (length(attempt) != 1L || is.na(attempt) || attempt < 1L) attempt <- 1L
  poll_seconds <- suppressWarnings(as.numeric(poll_seconds %||% 6))
  if (length(poll_seconds) != 1L || !is.finite(poll_seconds) || poll_seconds < 1) {
    poll_seconds <- 6
  }
  min(300, max(15, poll_seconds) * 2^(min(attempt, 6L) - 1L))
}

ugplot_science_collab_run_mission <- function(claimed, server_url, client_id, cpu_limit,
                                              spool_dir = NULL, poll_seconds = 6) {
  task <- claimed$task
  task_id <- as.character(task$task_id %||% "")
  lease_id <- as.character(task$lease_id %||% "")
  offline_delivery <- isTRUE(task$offline_delivery)
  worker <- NULL
  delivered <- FALSE
  delivery_stored <- FALSE
  on.exit({
    if (!is.null(worker)) {
      if (worker$process$is_alive()) try(worker$process$kill_tree(), silent = TRUE)
      unlink(worker$files, force = TRUE)
    } else {
      unlink(claimed$payload_path %||% "", force = TRUE)
    }
    if (!isTRUE(delivered) && !isTRUE(delivery_stored) &&
        nzchar(task_id) && nzchar(lease_id)) {
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
  coordinator_offline <- FALSE
  lease_inactive <- FALSE
  while (worker$process$is_alive()) {
    if (is.na(last_heartbeat) || difftime(Sys.time(), last_heartbeat, units = "secs") >= 25) {
      heartbeat <- tryCatch(
        ugplot_remote_collaboration_heartbeat(
          server_url, task_id, lease_id, client_id,
          telemetry = ugplot_science_collab_latest_telemetry(worker$event_path)
        ),
        error = function(e) e
      )
      if (inherits(heartbeat, "error")) {
        if (!isTRUE(offline_delivery)) stop(heartbeat)
        if (!isTRUE(coordinator_offline)) {
          message(
            "Coordinator unavailable during mission ", task_id,
            "; computation continues offline: ", conditionMessage(heartbeat)
          )
        }
        coordinator_offline <- TRUE
      } else if (!isTRUE(heartbeat$accepted)) {
        if (!isTRUE(offline_delivery)) {
          stop("The coordinator no longer accepts this mission lease.", call. = FALSE)
        }
        heartbeat_reason <- as.character(heartbeat$reason %||% "lease_not_active")
        if (heartbeat_reason %in% c(
          "already_completed", "task_cancelled", "task_not_found"
        )) {
          message(
            "Mission ", task_id, " no longer needs a result (",
            heartbeat_reason, "); stopping local computation."
          )
          delivered <- TRUE
          return(FALSE)
        }
        if (!isTRUE(lease_inactive)) {
          message(
            "Mission ", task_id,
            " lease is no longer active; finishing locally for late delivery."
          )
        }
        lease_inactive <- TRUE
        coordinator_offline <- FALSE
      } else {
        if (isTRUE(coordinator_offline)) {
          message("Coordinator connection restored during mission ", task_id, ".")
        }
        coordinator_offline <- FALSE
        lease_inactive <- FALSE
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
  delivery_path <- ugplot_science_collab_store_delivery(
    list(
      server_url = server_url,
      task_id = task_id,
      lease_id = lease_id,
      client_id = client_id,
      result = readRDS(worker$result_path),
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ),
    spool_dir = spool_dir
  )
  delivery_stored <- TRUE
  attempt <- 0L
  response <- NULL
  last_delivery_heartbeat <- as.POSIXct(NA)
  repeat {
    attempt <- attempt + 1L
    response <- ugplot_science_collab_attempt_delivery(delivery_path)
    if (isTRUE(response$done)) break
    # Delivery may be delayed by a coordinator outage or by a rolling schema
    # update. Keep the completed mission visible and its lease fresh while the
    # locally saved result is retried.
    if (is.na(last_delivery_heartbeat) ||
        difftime(Sys.time(), last_delivery_heartbeat, units = "secs") >= 25) {
      try(
        ugplot_remote_collaboration_heartbeat(
          server_url, task_id, lease_id, client_id,
          telemetry = list(
            progress = 1,
            message = "Result saved locally; waiting to deliver",
            candidate = "",
            completed = 1L
          )
        ),
        silent = TRUE
      )
      last_delivery_heartbeat <- Sys.time()
    }
    if (attempt == 1L || attempt %% 10L == 0L) {
      message(
        "Mission ", task_id,
        " finished and saved locally; waiting to deliver: ",
        response$error %||% "coordinator unavailable"
      )
    }
    Sys.sleep(max(1, suppressWarnings(as.numeric(poll_seconds %||% 6))))
  }
  delivered <- TRUE
  delivery_stored <- FALSE
  outcome_message <- if (isTRUE(response$accepted)) {
    " accepted."
  } else if (identical(response$reason %||% "", "already_completed")) {
    " was already completed elsewhere."
  } else {
    paste0(" was not accepted (", response$reason %||% "delivery rejected", ").")
  }
  message("Mission ", task_id, outcome_message)
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
#' @param install_model_deps Whether to inspect the waiting missions and attempt
#'   installation of dependencies for the caret models they require before
#'   connecting.
#' @param install_client_deps Whether to install the small set of packages needed
#'   by the headless client.
#' @param poll_seconds Seconds between attempts when no compatible mission is
#'   waiting.
#' @param delivery_grace_seconds Maximum time a completed mission may be
#'   delivered after its regular lease expires while the coordinator is
#'   unavailable. The default is 24 hours.
#' @param spool_dir Persistent directory used to retain results that are waiting
#'   for the coordinator. The default uses the Science Collab state directory.
#' @param max_missions Maximum missions to complete before returning. The
#'   default keeps the client running until interrupted.
#' @return Invisibly returns contribution counters when the client stops.
#' @export
ugPlotScienceCollab <- function(coordinator, scientist_name,
                                cpu_limit = max(1L, parallel::detectCores() - 1L),
                                install_model_deps = TRUE,
                                install_client_deps = TRUE,
                                poll_seconds = 6,
                                delivery_grace_seconds = 86400,
                                spool_dir = NULL,
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
  delivery_grace_seconds <- suppressWarnings(as.numeric(delivery_grace_seconds))
  if (length(delivery_grace_seconds) != 1L || !is.finite(delivery_grace_seconds) ||
      delivery_grace_seconds < 300 || delivery_grace_seconds > 7 * 86400) {
    stop("delivery_grace_seconds must be between 300 seconds and 7 days.", call. = FALSE)
  }
  spool_dir <- ugplot_science_collab_spool_dir(spool_dir)
  max_missions <- suppressWarnings(as.numeric(max_missions))
  if (length(max_missions) != 1L || is.na(max_missions) || max_missions < 0) {
    stop("max_missions must be zero, a positive number, or Inf.", call. = FALSE)
  }

  ugplot_install_science_collab_client_deps(
    install = install_client_deps,
    dependencies = TRUE
  )
  client_version <- ugplot_build_version()
  coordinator_status <- tryCatch(
    ugplot_remote_collaboration_status(server_url),
    error = function(e) {
      message(
        "Science Collab preflight could not reach ", server_url, ": ",
        conditionMessage(e)
      )
      NULL
    }
  )
  preflight <- ugplot_science_collab_preflight(
    coordinator_status, client_version, protocol_version = 2L
  )
  coordinator_version <- preflight$coordinator_version
  message(
    "Science Collab version check: client ", client_version,
    " | coordinator ", if (nzchar(coordinator_version)) coordinator_version else if (is.null(coordinator_status)) "unavailable" else "not reported",
    " | protocol ", if (is.na(preflight$protocol_version)) "unavailable" else preflight$protocol_version
  )
  model_status <- ugplot_model_dependency_status()
  compatibility <- tryCatch(
    ugplot_science_collab_compatibility(server_url, model_status),
    error = function(e) {
      warning(
        "Science Collab could not inspect mission compatibility: ",
        conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )
  required_models <- ugplot_science_collab_compatibility_models(
    compatibility, "required_models"
  )
  if (isTRUE(install_model_deps) && length(required_models) > 0L) {
    message(
      "Checking and attempting to install dependencies for ",
      length(required_models), " caret model",
      if (length(required_models) == 1L) "" else "s",
      " required by the waiting missions..."
    )
    tryCatch(
      ugPlotInstallModelDeps(models = required_models),
      error = function(e) warning(
        "Some required model dependencies could not be installed: ",
        conditionMessage(e),
        call. = FALSE
      )
    )
    model_status <- ugplot_model_dependency_status()
    compatibility <- tryCatch(
      ugplot_science_collab_compatibility(server_url, model_status),
      error = function(e) {
        warning(
          "Science Collab could not recheck mission compatibility: ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
  }
  if (!is.null(compatibility)) {
    ugplot_science_collab_assert_compatible(compatibility)
    pending_missions <- ugplot_science_collab_compatibility_count(
      compatibility, "pending"
    )
    compatible_missions <- ugplot_science_collab_compatibility_count(
      compatibility, "compatible"
    )
    if (pending_missions > 0L) {
      message(
        "Science Collab compatibility: ", compatible_missions, "/",
        pending_missions, " waiting mission",
        if (pending_missions == 1L) " is" else "s are",
        " compatible with this client."
      )
    } else {
      message("Science Collab compatibility: no mission is currently waiting.")
    }
  }
  client_id <- paste0(
    "headless-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
    Sys.getpid(), "-", sample(1000:9999, 1L)
  )
  capabilities <- list(
    models = unique(as.character(model_status$models_installed)),
    cpu_limit = cpu_limit,
    protocol_version = 2L,
    scientist_name = scientist_name,
    offline_delivery = TRUE,
    delivery_grace_seconds = delivery_grace_seconds
  )
  counters <- list(completed = 0L, accepted = 0L, server_url = server_url)
  failed_attempts <- new.env(parent = emptyenv())
  message(
    "Science Collab client ready at ", server_url, " as ", scientist_name,
    " (", cpu_limit, " CPU core", if (cpu_limit == 1L) "" else "s",
    ", build ", client_version, ")."
  )
  message("Waiting for public missions. Press Ctrl+C to stop safely.")

  pending_delivery_notice <- FALSE
  while (counters$completed < max_missions) {
    flushed <- ugplot_science_collab_flush_deliveries(spool_dir)
    if (flushed$pending > 0L) {
      if (!isTRUE(pending_delivery_notice)) {
        message(
          flushed$pending, " completed mission(s) saved locally; ",
          "waiting for the coordinator before claiming more work."
        )
      }
      pending_delivery_notice <- TRUE
      Sys.sleep(poll_seconds)
      next
    }
    if (isTRUE(pending_delivery_notice)) {
      message("All locally saved Science Collab results were delivered or resolved.")
    }
    pending_delivery_notice <- FALSE
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
    mission_error <- NULL
    accepted <- tryCatch(
      ugplot_science_collab_run_mission(
        claimed, server_url, client_id, cpu_limit,
        spool_dir = spool_dir, poll_seconds = poll_seconds
      ),
      error = function(e) {
        mission_error <<- e
        FALSE
      }
    )
    task_id <- as.character(claimed$task$task_id %||% "unknown")
    if (!is.null(mission_error)) {
      attempt <- if (exists(task_id, envir = failed_attempts, inherits = FALSE)) {
        get(task_id, envir = failed_attempts, inherits = FALSE) + 1L
      } else 1L
      assign(task_id, attempt, envir = failed_attempts)
      retry_delay <- ugplot_science_collab_failure_delay(attempt, poll_seconds)
      message(
        "Mission ", task_id, " failed [client ", client_version,
        " | coordinator ", coordinator_version, "]: ",
        conditionMessage(mission_error),
        ". Mission released; returning to the public queue in ",
        retry_delay, " second", if (retry_delay == 1) "" else "s", "."
      )
      Sys.sleep(retry_delay)
      next
    }
    if (exists(task_id, envir = failed_attempts, inherits = FALSE)) {
      rm(list = task_id, envir = failed_attempts)
    }
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
                                     delivery_grace_seconds = 86400,
                                     name = "default") {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required to start Science Collab in the background.", call. = FALSE)
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
  delivery_grace_seconds <- suppressWarnings(as.numeric(delivery_grace_seconds))
  if (length(delivery_grace_seconds) != 1L || !is.finite(delivery_grace_seconds) ||
      delivery_grace_seconds < 300 || delivery_grace_seconds > 7 * 86400) {
    stop("delivery_grace_seconds must be between 300 seconds and 7 days.", call. = FALSE)
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
  runtime_dir <- file.path(ugplot_science_collab_work_dir(), paste0("client-", safe_name))
  ugplot_ensure_dir(runtime_dir)
  launcher_path <- file.path(runtime_dir, "launcher.R")
  config_path <- file.path(runtime_dir, "config.rds")
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "config <- readRDS(args[[1]])",
    ".libPaths(config$lib_paths)",
    "library(ugplot)",
    "ugPlotScienceCollab(",
    "  coordinator = config$server_url,",
    "  scientist_name = config$scientist_name,",
    "  cpu_limit = config$cpu_limit,",
    "  install_model_deps = config$install_model_deps,",
    "  install_client_deps = config$install_client_deps,",
    "  poll_seconds = config$poll_seconds,",
    "  delivery_grace_seconds = config$delivery_grace_seconds,",
    "  spool_dir = config$spool_dir",
    ")"
  ), launcher_path, useBytes = TRUE)
  saveRDS(list(
    server_url = server_url,
    scientist_name = scientist_name,
    cpu_limit = cpu_limit,
    install_model_deps = isTRUE(install_model_deps),
    install_client_deps = isTRUE(install_client_deps),
    poll_seconds = poll_seconds,
    delivery_grace_seconds = delivery_grace_seconds,
    spool_dir = file.path(runtime_dir, "pending-delivery"),
    lib_paths = .libPaths()
  ), config_path)
  try(Sys.chmod(c(launcher_path, config_path), mode = "0600"), silent = TRUE)
  process <- processx::process$new(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", launcher_path, config_path),
    stdout = log_file,
    stderr = log_file,
    supervise = FALSE,
    cleanup = FALSE,
    cleanup_tree = FALSE,
    poll_connection = FALSE,
    env = c(TMPDIR = runtime_dir, TMP = runtime_dir, TEMP = runtime_dir),
    windows_hide_window = TRUE
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
    delivery_grace_seconds = delivery_grace_seconds,
    log_file = log_file,
    runtime_dir = runtime_dir,
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
    spool_dir <- file.path(as.character(state$runtime_dir %||% ""), "pending-delivery")
    state$pending_deliveries <- if (dir.exists(spool_dir)) {
      length(list.files(spool_dir, pattern = "[.]rds$"))
    } else {
      0L
    }
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
