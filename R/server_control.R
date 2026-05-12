ugplot_server_state_dir <- function() {
  state_dir <- file.path(path.expand("~"), ".ugplot", "server")
  ugplot_ensure_dir(state_dir)
  state_dir
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
  result <- tryCatch(tools::pskill(pid, signal = 0), error = function(e) FALSE)
  isTRUE(result)
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

#' Start a ugplot job server in the background
#'
#' Starts \code{ugPlotServer()} in a detached R process and immediately returns
#' control to the console.
#'
#' @param host Interface to bind.
#' @param port Port to listen on.
#' @param jobs_dir Directory used to persist datasets, status and results.
#' @param token Optional bearer token. Defaults to no authentication.
#' @param name Local server handle name used by status/stop.
#' @return Invisibly returns the server state.
#' @export
ugPlotServerStart <- function(host = "127.0.0.1", port = 8080,
                              jobs_dir = ugplot_default_jobs_dir(),
                              token = "", name = "default") {
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
    func = function(host, port, jobs_dir, token, lib_paths, source_dir) {
      .libPaths(lib_paths)
      if (!is.null(source_dir) && file.exists(file.path(source_dir, "R", "server_api.R"))) {
        source(file.path(source_dir, "R", "app.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "job_store.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "job_process.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "ml_runner.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "server_deps.R"), local = .GlobalEnv)
        source(file.path(source_dir, "R", "server_api.R"), local = .GlobalEnv)
        ugPlotServer(host = host, port = port, jobs_dir = jobs_dir, token = token)
      } else {
        library(ugplot)
        ugPlotServer(host = host, port = port, jobs_dir = jobs_dir, token = token)
      }
    },
    args = list(
      host = host,
      port = port,
      jobs_dir = jobs_dir,
      token = token,
      lib_paths = lib_paths,
      source_dir = source_dir
    ),
    stdout = log_file,
    stderr = log_file,
    supervise = TRUE
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
  message("ugPlotServer started at ", state$url, " (pid ", state$pid, ").")
  invisible(state)
}

#' Get background ugplot job server status
#'
#' @param name Local server handle name.
#' @return A list with server metadata and running state.
#' @export
ugPlotServerStatus <- function(name = "default") {
  state <- ugplot_read_server_state(name)
  if (is.null(state)) {
    state <- list(name = name, running = FALSE, message = "No background server state found.")
  } else {
    state$running <- ugplot_process_alive(state$pid)
  }
  print(state)
  invisible(state)
}

#' Stop a background ugplot job server
#'
#' @param name Local server handle name.
#' @return Invisibly returns TRUE when a process was stopped.
#' @export
ugPlotServerStop <- function(name = "default") {
  state <- ugplot_read_server_state(name)
  if (is.null(state)) {
    message("No background ugPlotServer state found.")
    return(invisible(FALSE))
  }
  if (!ugplot_process_alive(state$pid)) {
    message("ugPlotServer is not running.")
    return(invisible(FALSE))
  }
  tools::pskill(as.integer(state$pid), signal = tools::SIGTERM)
  Sys.sleep(0.5)
  if (ugplot_process_alive(state$pid)) {
    tools::pskill(as.integer(state$pid), signal = tools::SIGKILL)
  }
  message("ugPlotServer stopped (pid ", state$pid, ").")
  invisible(TRUE)
}
