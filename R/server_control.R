ugplot_server_state_dir <- function() {
  state_dir <- file.path(path.expand("~"), ".ugplot", "server")
  ugplot_ensure_dir(state_dir)
  state_dir
}

ugplot_first_non_null <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value)) {
      return(value)
    }
  }
  NULL
}

ugplot_server_state_path <- function(name = "default") {
  safe_name <- gsub("[^A-Za-z0-9._-]+", "_", name)
  file.path(ugplot_server_state_dir(), paste0(safe_name, ".rds"))
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

ugplot_port_listener_pids <- function(port) {
  port <- suppressWarnings(as.integer(port))
  if (is.na(port) || port <= 0) {
    return(integer())
  }

  pids <- integer()

  if (.Platform$OS.type == "unix") {
    if (nzchar(Sys.which("lsof"))) {
      output <- tryCatch(
        suppressWarnings(
          system2("lsof", c("-nP", paste0("-iTCP:", port), "-sTCP:LISTEN", "-t"),
            stdout = TRUE, stderr = FALSE
          )
        ),
        error = function(e) character()
      )
      pids <- c(pids, suppressWarnings(as.integer(output)))
    }

    if (length(pids) == 0 && nzchar(Sys.which("ss"))) {
      output <- tryCatch(
        suppressWarnings(system2("ss", c("-ltnp", paste0("sport = :", port)), stdout = TRUE, stderr = FALSE)),
        error = function(e) character()
      )
      pid_matches <- regmatches(output, gregexpr("pid=[0-9]+", output))
      pid_values <- sub("^pid=", "", unlist(pid_matches, use.names = FALSE))
      pids <- c(pids, suppressWarnings(as.integer(pid_values)))
    }

    if (length(pids) == 0 && nzchar(Sys.which("fuser"))) {
      output <- tryCatch(
        suppressWarnings(system2("fuser", c("-n", "tcp", as.character(port)), stdout = TRUE, stderr = FALSE)),
        error = function(e) character()
      )
      pids <- c(pids, suppressWarnings(as.integer(strsplit(paste(output, collapse = " "), "[[:space:]]+")[[1]])))
    }
  }

  pids <- unique(pids[!is.na(pids) & pids > 0])
  pids[vapply(pids, ugplot_process_alive, logical(1))]
}

ugplot_server_state_from_port <- function(port, name = "default") {
  pids <- ugplot_port_listener_pids(port)
  if (length(pids) == 0) {
    return(NULL)
  }
  list(
    name = name,
    pid = pids[[1]],
    pids = pids,
    host = NA_character_,
    port = as.integer(port),
    url = paste0("http://127.0.0.1:", as.integer(port)),
    discovered_by = "port",
    running = TRUE
  )
}

ugplot_read_server_state <- function(name = "default") {
  state_path <- ugplot_server_state_path(name)
  if (!file.exists(state_path)) {
    return(NULL)
  }
  readRDS(state_path)
}

ugplot_write_server_state <- function(state, name = "default") {
  saveRDS(state, ugplot_server_state_path(name))
  invisible(state)
}

ugplot_register_server_state <- function(host = "0.0.0.0", port = 8080,
                                         jobs_dir = ugplot_default_jobs_dir(),
                                         token = "", name = "default",
                                         pid = Sys.getpid(), log_file = NA_character_,
                                         started_by = "ugPlotServer") {
  state <- list(
    name = name,
    pid = as.integer(pid),
    host = host,
    port = as.integer(port),
    url = paste0("http://", host, ":", as.integer(port)),
    jobs_dir = jobs_dir,
    token_set = nzchar(token),
    log_file = log_file,
    started_by = started_by,
    started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
  ugplot_write_server_state(state, name)
}

ugplot_mark_server_state_stopped <- function(name = "default") {
  state <- ugplot_read_server_state(name)
  if (is.null(state)) {
    return(invisible(FALSE))
  }
  state$running <- FALSE
  state$stopped_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ugplot_write_server_state(state, name)
  invisible(TRUE)
}

#' Start a ugplot job server in the background
#'
#' Starts \code{ugPlotServer()} in a detached R process and immediately returns
#' control to the console.
#'
#' @param host Interface to bind.
#' @param port Port to listen on.
#' @param jobs_dir Directory used to persist datasets, status and results.
#' @param token Bearer token. Required when listening on a non-local interface.
#' @param name Local server handle name used by status/stop.
#' @return Invisibly returns the server state.
#' @export
ugPlotServerStart <- function(host = "0.0.0.0", port = 8080,
                              jobs_dir = ugplot_default_jobs_dir(),
                              token = "", name = "default") {
  if (!(host %in% c("127.0.0.1", "::1", "localhost")) && !nzchar(token)) {
    stop("A bearer token is required when ugPlotServer listens on a non-local interface.", call. = FALSE)
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required to start ugPlotServer in background.", call. = FALSE)
  }

  current <- ugplot_read_server_state(name)
  if (!is.null(current) && ugplot_process_alive(current$pid)) {
    message("ugPlotServer is already running at ", current$url, " (pid ", current$pid, ").")
    return(invisible(current))
  }

  lib_paths <- .libPaths()
  source_dir <- if (file.exists(file.path(getwd(), "R", "app.R"))) normalizePath(getwd(), mustWork = FALSE) else NULL
  log_file <- file.path(ugplot_server_state_dir(), paste0(gsub("[^A-Za-z0-9._-]+", "_", name), ".log"))
  process <- callr::r_bg(
    func = function(host, port, jobs_dir, token, name, lib_paths, source_dir) {
      .libPaths(lib_paths)
      if (!is.null(source_dir) && file.exists(file.path(source_dir, "R", "server_api.R"))) {
        source(file.path(source_dir, "R", "00_version.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "app.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "job_store.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "job_process.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "ml_runner.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "server_deps.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "server_api.R"), local = .GlobalEnv)
        ugPlotServer(host = host, port = port, jobs_dir = jobs_dir, token = token, name = name)
      } else {
        library(ugplot)
        ugPlotServer(host = host, port = port, jobs_dir = jobs_dir, token = token, name = name)
      }
    },
    args = list(
      host = host,
      port = port,
      jobs_dir = jobs_dir,
      token = token,
      name = name,
      lib_paths = lib_paths,
      source_dir = source_dir
    ),
    stdout = log_file,
    stderr = log_file,
    supervise = FALSE,
    cleanup = FALSE
  )

  state <- list(
    name = name,
    pid = process$get_pid(),
    host = host,
    port = port,
    url = paste0("http://", host, ":", port),
    jobs_dir = jobs_dir,
    log_file = log_file,
    started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
  ugplot_write_server_state(state, name)

  started <- FALSE
  for (attempt in seq_len(50)) {
    if (!process$is_alive()) {
      log_lines <- if (file.exists(log_file)) utils::tail(readLines(log_file, warn = FALSE), 20) else character(0)
      stop(
        paste(c("ugPlotServer background process stopped before opening the port.", log_lines), collapse = "\n"),
        call. = FALSE
      )
    }
    if (length(ugplot_port_listener_pids(port)) > 0) {
      started <- TRUE
      break
    }
    Sys.sleep(0.2)
  }
  if (!isTRUE(started)) {
    warning("ugPlotServer process started, but port ", port, " was not listening yet.", call. = FALSE)
  }

  message("ugPlotServer started at ", state$url, " (pid ", state$pid, ").")
  invisible(state)
}

#' Get background ugplot job server status
#'
#' @param name Local server handle name.
#' @param port Optional port used to discover a server when no live state file is
#'   available. Defaults to the saved state port, or 8080 when there is no state.
#' @return A list with server metadata and running state.
#' @export
ugPlotServerStatus <- function(name = "default", port = NULL) {
  state <- ugplot_read_server_state(name)
  if (is.null(state)) {
    lookup_port <- ugplot_first_non_null(port, 8080L)
    state <- ugplot_server_state_from_port(lookup_port, name)
    if (is.null(state)) {
      state <- list(name = name, port = lookup_port, running = FALSE, message = "No background server state found.")
    } else {
      state$message <- "Server process discovered by listening port; state file was missing."
    }
  } else {
    state$running <- ugplot_process_alive(state$pid)
    lookup_port <- ugplot_first_non_null(port, state$port, 8080L)
    if (!isTRUE(state$running)) {
      discovered <- ugplot_server_state_from_port(lookup_port, name)
      if (!is.null(discovered)) {
        state$running <- TRUE
        state$pid <- discovered$pid
        state$pids <- discovered$pids
        state$discovered_by <- discovered$discovered_by
        state$message <- "Saved PID was not running; server process discovered by listening port."
      }
    }
  }
  print(state)
  invisible(state)
}

#' Stop a background ugplot job server
#'
#' @param name Local server handle name.
#' @param port Optional port used to discover and stop a server when no live
#'   state file is available. Defaults to the saved state port, or 8080 when
#'   there is no state.
#' @return Invisibly returns TRUE when a process was stopped.
#' @export
ugPlotServerStop <- function(name = "default", port = NULL) {
  state <- ugplot_read_server_state(name)
  lookup_port <- ugplot_first_non_null(port, if (!is.null(state)) state$port else NULL, 8080L)
  if (is.null(state)) {
    state <- ugplot_server_state_from_port(lookup_port, name)
    if (is.null(state)) {
      message("No background ugPlotServer state found.")
      return(invisible(FALSE))
    }
  }
  pids <- unique(c(ugplot_first_non_null(state$pid, integer()), ugplot_first_non_null(state$pids, integer())))
  pids <- pids[vapply(pids, ugplot_process_alive, logical(1))]
  if (length(pids) == 0) {
    discovered <- ugplot_server_state_from_port(lookup_port, name)
    pids <- ugplot_first_non_null(discovered$pids, integer())
  }
  pids <- unique(pids[vapply(pids, ugplot_process_alive, logical(1))])
  if (length(pids) == 0) {
    message("ugPlotServer is not running.")
    return(invisible(FALSE))
  }
  for (pid in pids) {
    tools::pskill(as.integer(pid), signal = tools::SIGTERM)
  }
  Sys.sleep(0.5)
  still_running <- pids[vapply(pids, ugplot_process_alive, logical(1))]
  for (pid in still_running) {
    tools::pskill(as.integer(pid), signal = tools::SIGKILL)
  }
  ugplot_mark_server_state_stopped(name)
  message("ugPlotServer stopped (pid ", paste(pids, collapse = ", "), ").")
  invisible(TRUE)
}
