ugplot_collaboration_tab_ui <- function(id, total_system_cpus = 1L) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    "SCIENCE COLLAB",
    shiny::fluidPage(
      shiny::tags$div(
        class = "collab-lab",
        shiny::tags$div(
          class = "collab-hero",
          shiny::tags$div(class = "collab-hero-orb", shiny::tags$span(class = "collab-orb-core")),
          shiny::tags$div(
            shiny::tags$div(class = "collab-eyebrow", "OPEN SCIENCE COMPUTE NETWORK"),
            shiny::tags$h2("Turn your computer into a scientific workstation"),
            shiny::tags$p("Follow a real experiment as your machine explores data, compares candidates, and returns a validated contribution.")
          ),
          shiny::uiOutput(ns("connection_badge"))
        ),
        shiny::tags$div(
          class = "collab-layout",
          shiny::tags$aside(
            class = "collab-control-card",
            shiny::tags$div(class = "collab-card-kicker", "YOUR WORKSTATION"),
            shiny::uiOutput(ns("server_picker")),
            shiny::textInput(ns("scientist_name"), "Scientist name", value = paste0("Scientist-", sample(1000:9999, 1))),
            shiny::sliderInput(
              ns("cpu_limit"), "CPU cores to contribute",
              min = 1, max = max(1, total_system_cpus - 1L),
              value = max(1, min(2, total_system_cpus - 1L)), step = 1
            ),
            shiny::tags$div(class = "collab-compatibility", shiny::uiOutput(ns("compatibility"))),
            shiny::actionButton(ns("toggle"), "Start contributing", class = "collab-start-button", icon = shiny::icon("flask")),
            shiny::tags$p(class = "collab-privacy-note", shiny::icon("globe"), " Public collaboration: only the assigned mission payload is downloaded.")
          ),
          shiny::tags$main(
            class = "collab-stage",
            shiny::uiOutput(ns("journey")),
            shiny::tags$div(
              class = "collab-dashboard-grid",
              shiny::tags$section(class = "collab-panel collab-mission-panel", shiny::uiOutput(ns("mission"))),
              shiny::tags$section(
                class = "collab-panel collab-resource-panel",
                shiny::tags$div(class = "collab-panel-heading", shiny::tags$span("Workstation pulse"), shiny::tags$span(class = "collab-live-dot", "LIVE")),
                plotly::plotlyOutput(ns("resource_plot"), height = "190px")
              ),
              shiny::tags$section(
                class = "collab-panel collab-dataset-panel",
                shiny::tags$div(class = "collab-panel-heading", "Dataset"),
                shiny::tags$div(
                  class = "collab-dataset-grid",
                  shiny::uiOutput(ns("variable_explorer")),
                  plotly::plotlyOutput(ns("dataset_plot"), height = "235px")
                )
              ),
              shiny::tags$section(
                class = "collab-panel collab-experiment-panel",
                shiny::tags$div(class = "collab-panel-heading", "Experiment arena"),
                shiny::uiOutput(ns("current_experiment")),
                plotly::plotlyOutput(ns("metric_plot"), height = "285px")
              ),
              shiny::tags$section(
                class = "collab-panel collab-discovery-panel",
                shiny::tags$div(class = "collab-panel-heading", "Discovery emerging"),
                shiny::uiOutput(ns("discovery"))
              ),
              shiny::tags$section(
                class = "collab-panel collab-community-panel",
                shiny::tags$div(class = "collab-panel-heading", "Your scientific impact"),
                shiny::uiOutput(ns("impact"))
              )
            )
          )
        )
      )
    )
  )
}

ugplot_collaboration_coordinator_candidates <- function(servers, direct_url = "",
                                                        selected = "") {
  direct_url <- trimws(as.character(direct_url %||% ""))
  if (nzchar(direct_url)) {
    return(data.frame(
      name = "Direct coordinator",
      url = ugplot_science_collab_url(direct_url),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(servers) || nrow(servers) == 0L) return(servers)
  servers[order(as.character(servers$name) != as.character(selected %||% "")), , drop = FALSE]
}

ugplot_collaboration_tab_server <- function(id, remote_servers, total_system_cpus = 1L) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- shiny::reactiveVal("idle")
    enabled <- shiny::reactiveVal(FALSE)
    mission <- shiny::reactiveVal(list())
    events <- shiny::reactiveVal(list())
    events_signature <- shiny::reactiveVal("")
    lease <- shiny::reactiveVal(NULL)
    lease_server_url <- shiny::reactiveVal("")
    process <- shiny::reactiveVal(NULL)
    launch_pending <- shiny::reactiveVal(FALSE)
    process_files <- shiny::reactiveVal(list())
    compatible_models <- shiny::reactiveVal(character(0))
    claim_models <- shiny::reactiveVal(character(0))
    network_note <- shiny::reactiveVal("")
    last_claim_at <- shiny::reactiveVal(as.POSIXct(NA))
    last_heartbeat_at <- shiny::reactiveVal(as.POSIXct(NA))
    resource_previous <- shiny::reactiveVal(NULL)
    resource_history <- shiny::reactiveVal(data.frame())
    impact <- shiny::reactiveVal(list(experiments = 0L, compute_seconds = 0, accepted = 0L))
    client_id <- paste0("scientist-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample(1000:9999, 1))

    selected_server <- function() {
      direct_url <- trimws(as.character(input$server_url %||% ""))
      if (nzchar(direct_url)) {
        return(data.frame(
          name = "Direct coordinator",
          url = ugplot_science_collab_url(direct_url),
          stringsAsFactors = FALSE
        ))
      }
      servers <- remote_servers()
      selected <- as.character(input$server_name %||% "")
      row <- servers[as.character(servers$name) == selected, , drop = FALSE]
      if (nrow(row) != 1L) {
        stop("Enter a coordinator IP/URL or choose a configured server.", call. = FALSE)
      }
      row
    }
    available_coordinators <- function() {
      ugplot_collaboration_coordinator_candidates(
        remote_servers(),
        direct_url = input$server_url %||% "",
        selected = input$server_name %||% ""
      )
    }
    append_local_event <- function(type, data = list()) {
      current <- events()
      current[[length(current) + 1L]] <- list(
        sequence = length(current) + 1L, type = type,
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), data = data
      )
      events(current)
    }
    latest_event <- function(type) {
      matches <- Filter(function(event) identical(event$type %||% "", type), events())
      if (length(matches) == 0L) NULL else utils::tail(matches, 1L)[[1]]
    }
    empty_plot <- function(message) {
      plotly::plot_ly(
        x = numeric(0), y = numeric(0), type = "scatter", mode = "markers"
      ) %>% plotly::layout(
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(text = message, showarrow = FALSE))
      )
    }

    output$server_picker <- shiny::renderUI({
      servers <- remote_servers()
      shiny::tagList(
        shiny::textInput(
          session$ns("server_url"), "Coordinator IP or URL",
          value = "", placeholder = "192.168.1.20 or https://collab.example.org"
        ),
        if (is.data.frame(servers) && nrow(servers) > 0L) {
          shiny::selectInput(
            session$ns("server_name"), "Or use a configured coordinator",
            choices = stats::setNames(as.character(servers$name), as.character(servers$name)),
            selected = as.character(servers$name[[1]])
          )
        } else {
          shiny::tags$p(
            class = "collab-empty-visual",
            "No saved coordinator is needed: enter its IP or URL above."
          )
        },
        shiny::tags$small(
          class = "collab-privacy-note",
          "Science Collab uses the public mission channel and does not request the server job token."
        )
      )
    })

    shiny::observe({
      remote_servers()
      direct_url <- trimws(as.character(input$server_url %||% ""))
      shinyjs::toggleState("server_name", condition = !nzchar(direct_url))
    })

    output$connection_badge <- shiny::renderUI({
      labels <- c(
        idle = "○ READY TO CONNECT", connecting = "◌ CONNECTING", waiting = "◉ WAITING FOR MISSION",
        preparing = "◌ PREPARING EXPERIMENT",
        computing = "● EXPERIMENT RUNNING", uploading = "↑ RETURNING DISCOVERY",
        accepted = "✓ CONTRIBUTION ACCEPTED", error = "! ATTENTION NEEDED"
      )
      current <- state()
      shiny::tags$div(
        class = paste("collab-connection-badge", if (current %in% c("waiting", "preparing", "computing", "uploading", "accepted")) "active" else ""),
        labels[[current]] %||% toupper(current)
      )
    })

    output$compatibility <- shiny::renderUI({
      models <- compatible_models()
      if (state() == "idle") {
        return(shiny::tags$span(shiny::icon("wand-magic-sparkles"), " Compatibility will be checked automatically."))
      }
      if (length(models) == 0L) {
        return(shiny::tags$span(shiny::icon("spinner", class = "fa-spin"), " Inspecting the local scientific toolkit..."))
      }
      shiny::tags$div(
        class = "collab-compat-ok", shiny::icon("circle-check"), " Workstation ready",
        shiny::tags$div(style = "margin-top:4px; font-weight:400; color:#6d7790;", paste(length(models), "compatible ML candidates available")),
        if (nzchar(network_note())) shiny::tags$div(style = "margin-top:4px; font-weight:500; color:#55627f;", network_note())
      )
    })

    shiny::observe({
      active <- enabled()
      shiny::updateActionButton(
        session, "toggle", label = if (active) "Stop after current mission" else "Start contributing",
        icon = if (active) shiny::icon("stop") else shiny::icon("flask")
      )
      if (active) shinyjs::addClass("toggle", "stop") else shinyjs::removeClass("toggle", "stop")
    })

    shiny::observeEvent(input$toggle, {
      if (isTRUE(enabled())) {
        enabled(FALSE)
        launch_pending(FALSE)
        worker <- process()
        if (!is.null(worker) && worker$is_alive()) try(worker$kill(), silent = TRUE)
        files <- process_files()
        active_lease <- lease()
        if (is.list(active_lease) && nzchar(lease_server_url())) {
          try(ugplot_remote_collaboration_release(
            lease_server_url(), active_lease$task_id, active_lease$lease_id, client_id
          ), silent = TRUE)
        }
        process(NULL)
        if (length(files) > 0L) {
          unlink(unlist(files[c("events", "result", "payload", "launcher", "stdout", "stderr", "lib_paths")]), force = TRUE)
        }
        process_files(list())
        lease(NULL)
        lease_server_url("")
        state("idle")
        return()
      }
      state("connecting")
      servers <- available_coordinators()
      statuses <- if (is.data.frame(servers) && nrow(servers) > 0L) lapply(seq_len(nrow(servers)), function(i) {
        tryCatch(ugplot_remote_collaboration_status(as.character(servers$url[[i]])), error = function(e) e)
      }) else list()
      open_servers <- vapply(statuses, function(status) {
        is.list(status) && !inherits(status, "error") && identical(as.character(status$status %||% ""), "open")
      }, logical(1))
      if (!any(open_servers)) {
        state("error")
        shiny::showNotification(
          "No configured server is accepting public collaboration yet.",
          type = "error"
        )
        return()
      }
      dependency_status <- tryCatch(ugplot_model_dependency_status(), error = function(e) NULL)
      models <- dependency_status$models_installed %||% character(0)
      compatible_models(unique(as.character(models)))
      claim_models(unique(as.character(models)))
      enabled(TRUE)
      state("waiting")
      last_claim_at(as.POSIXct(NA))
    })

    collaboration_timer <- shiny::reactiveTimer(2000, session = session)
    shiny::observe({
      collaboration_timer()
      if (!isTRUE(enabled())) return()
      worker <- process()
      active_lease <- lease()
      server_url <- if (is.list(active_lease) && nzchar(lease_server_url())) {
        lease_server_url()
      } else {
        preferred <- tryCatch(selected_server(), error = function(e) NULL)
        if (is.null(preferred)) return() else as.character(preferred$url[[1]])
      }

      if (!is.null(worker)) {
        files <- process_files()
        if (nzchar(files$events %||% "") && file.exists(files$events)) {
          new_events <- tryCatch(readRDS(files$events), error = function(e) events())
          if (is.list(new_events)) {
            last_event <- if (length(new_events) > 0L) utils::tail(new_events, 1L)[[1]] else list()
            signature <- paste(
              length(new_events),
              as.character(last_event$sequence %||% ""),
              as.character(last_event$type %||% ""),
              as.character(last_event$timestamp %||% ""),
              sep = ":"
            )
            if (!identical(signature, events_signature())) {
              events_signature(signature)
              events(new_events)
            }
          }
        }
        if (worker$is_alive()) {
          sample <- tryCatch(
            ugplot_sample_job_resources(
              list(pid = worker$get_pid(), message = "Collaborative experiment"),
              shiny::isolate(resource_previous())
            ),
            error = function(e) data.frame()
          )
          if (is.data.frame(sample) && nrow(sample) > 0L) {
            resource_previous(as.list(sample[nrow(sample), , drop = FALSE]))
            history <- shiny::isolate(resource_history())
            resource_history(utils::tail(rbind(history, sample), 120L))
          }
          heartbeat_at <- last_heartbeat_at()
          if (is.list(active_lease) && (is.na(heartbeat_at) || difftime(Sys.time(), heartbeat_at, units = "secs") >= 25)) {
            progress_event <- latest_event("progress_updated") %||% latest_event("experiment_started")
            metric_event <- latest_event("metric_updated")
            telemetry_data <- progress_event$data %||% list()
            metric_data <- metric_event$data %||% list()
            heartbeat <- tryCatch(
              ugplot_remote_collaboration_heartbeat(
                server_url, active_lease$task_id, active_lease$lease_id, client_id,
                telemetry = list(
                  progress = telemetry_data$progress %||% 0,
                  message = telemetry_data$message %||% "Experiment running",
                  candidate = telemetry_data$candidate %||% metric_data$candidate %||% "",
                  completed = metric_data$completed %||% 0L
                )
              ),
              error = function(e) NULL
            )
            if (is.list(heartbeat) && isTRUE(heartbeat$accepted)) last_heartbeat_at(Sys.time())
          }
          return()
        }

        exit_status <- tryCatch(worker$get_exit_status(), error = function(e) 1L)
        if (identical(exit_status, 0L) && file.exists(files$result %||% "") && is.list(active_lease)) {
          state("uploading")
          accepted <- tryCatch(
            ugplot_remote_collaboration_complete(
              server_url, active_lease$task_id, active_lease$lease_id, client_id, readRDS(files$result)
            ),
            error = function(e) list(accepted = FALSE, reason = conditionMessage(e))
          )
          append_local_event(if (isTRUE(accepted$accepted)) "result_accepted" else "result_not_needed", accepted)
          if (isTRUE(accepted$accepted)) {
            totals <- impact()
            totals$accepted <- totals$accepted + 1L
            totals$experiments <- totals$experiments + sum(vapply(events(), function(event) identical(event$type, "metric_updated"), logical(1)))
            totals$compute_seconds <- totals$compute_seconds + as.numeric(difftime(Sys.time(), files$started_at, units = "secs"))
            impact(totals)
            state("accepted")
          } else state("waiting")
        } else state("waiting")
        unlink(unlist(files[c("events", "result", "payload", "launcher", "stdout", "stderr", "lib_paths")]), force = TRUE)
        process(NULL)
        process_files(list())
        lease(NULL)
        lease_server_url("")
        resource_previous(NULL)
        last_claim_at(Sys.time())
        return()
      }
      if (isTRUE(launch_pending())) return()

      claimed_at <- last_claim_at()
      if (!is.na(claimed_at) && difftime(Sys.time(), claimed_at, units = "secs") < 6) return()
      last_claim_at(Sys.time())
      capabilities <- list(
        models = claim_models(), cpu_limit = as.integer(input$cpu_limit %||% 1L),
        protocol_version = 2L,
        scientist_name = trimws(input$scientist_name %||% "Anonymous scientist")
      )
      coordinators <- available_coordinators()
      claimed <- NULL
      claimed_server_url <- ""
      claim_errors <- character(0)
      if (is.data.frame(coordinators) && nrow(coordinators) > 0L) {
        queue_sizes <- vapply(seq_len(nrow(coordinators)), function(i) {
          status <- tryCatch(
            ugplot_remote_collaboration_status(as.character(coordinators$url[[i]])),
            error = function(e) NULL
          )
          suppressWarnings(as.integer(status$pending %||% 0L))
        }, integer(1))
        total_waiting <- sum(queue_sizes, na.rm = TRUE)
        network_note(if (total_waiting > 0L) paste(total_waiting, "mission(s) found; matching requirements") else "No mission is currently queued")
        coordinators <- coordinators[order(queue_sizes, decreasing = TRUE), , drop = FALSE]
        for (i in seq_len(nrow(coordinators))) {
          candidate_url <- as.character(coordinators$url[[i]])
          claimed <- tryCatch(
            ugplot_remote_collaboration_claim(candidate_url, client_id, capabilities),
            error = function(e) {
              claim_errors <<- c(claim_errors, paste0(candidate_url, ": ", conditionMessage(e)))
              NULL
            }
          )
          if (!is.null(claimed)) {
            claimed_server_url <- candidate_url
            break
          }
        }
      }
      if (is.null(claimed)) {
        diagnostics <- if (is.data.frame(coordinators) && nrow(coordinators) > 0L) lapply(seq_len(nrow(coordinators)), function(i) {
          tryCatch(
            ugplot_remote_collaboration_compatibility(as.character(coordinators$url[[i]]), capabilities),
            error = function(e) e
          )
        }) else list()
        supported <- Filter(function(value) is.list(value) && !inherits(value, "error"), diagnostics)
        compatible_count <- sum(vapply(supported, function(value) {
          suppressWarnings(as.integer(value$compatible %||% 0L))
        }, integer(1)), na.rm = TRUE)
        pending_count <- sum(vapply(supported, function(value) {
          suppressWarnings(as.integer(value$pending %||% 0L))
        }, integer(1)), na.rm = TRUE)
        inactive_count <- sum(vapply(supported, function(value) {
          suppressWarnings(as.integer(value$inactive_pending %||% 0L))
        }, integer(1)), na.rm = TRUE)
        missing_models <- unique(unlist(lapply(supported, function(value) {
          missions <- value$missions %||% list()
          if (is.data.frame(missions) && "missing_models" %in% names(missions)) {
            return(unlist(missions$missing_models, use.names = FALSE))
          }
          unlist(lapply(missions, function(item) item$missing_models %||% character(0)), use.names = FALSE)
        }), use.names = FALSE))
        missing_models <- missing_models[nzchar(as.character(missing_models))]
        if (length(claim_errors) > 0L) {
          network_note(paste("Could not reserve mission:", paste(unique(claim_errors), collapse = " | ")))
        } else if (compatible_count > 0L) {
          network_note(paste0(
            compatible_count, " compatible mission(s) found, but the coordinator did not reserve one. ",
            "The queue may be changing; retry shortly or update the coordinator."
          ))
        } else if (length(missing_models) > 0L) {
          preview <- paste(utils::head(missing_models, 6L), collapse = ", ")
          suffix <- if (length(missing_models) > 6L) paste0(" +", length(missing_models) - 6L, " more") else ""
          network_note(paste0("Incompatible mission: missing ", preview, suffix))
        } else if (length(supported) == 0L && grepl("mission(s) found", network_note(), fixed = TRUE)) {
          network_note("Legacy missions found; update the coordinator to display exact incompatibilities")
        } else if (pending_count > 0L) {
          network_note(paste(pending_count, "mission(s) are waiting, but none match this workstation"))
        } else if (inactive_count > 0L) {
          network_note(paste(
            inactive_count,
            "old mission(s) ignored because their parent job is not running"
          ))
        } else {
          network_note("No mission is currently queued by the coordinators")
        }
        state("waiting")
        return()
      }
      actual_models <- unique(as.character(claimed$task$requirements$models %||% character(0)))
      actual_status <- tryCatch(
        ugplot_model_dependency_status(models = actual_models),
        error = function(e) NULL
      )
      missing_actual <- unique(c(actual_status$models_missing %||% character(0), actual_status$unknown_models %||% character(0)))
      if (length(missing_actual) > 0L) {
        try(ugplot_remote_collaboration_release(
          claimed_server_url, claimed$task$task_id, claimed$task$lease_id, client_id
        ), silent = TRUE)
        unlink(claimed$payload_path %||% "", force = TRUE)
        network_note(paste("Mission requires missing models:", paste(missing_actual, collapse = ", ")))
        state("waiting")
        return()
      }
      network_note(paste("Mission accepted from", claimed_server_url))
      event_path <- tempfile("ugplot-collab-events-", fileext = ".rds")
      result_path <- tempfile("ugplot-collab-result-", fileext = ".rds")
      payload_path <- as.character(claimed$payload_path %||% "")
      launcher_path <- tempfile("ugplot-collab-launcher-", fileext = ".R")
      stdout_path <- tempfile("ugplot-collab-worker-", fileext = ".stdout.log")
      stderr_path <- tempfile("ugplot-collab-worker-", fileext = ".stderr.log")
      cpu_limit <- as.integer(input$cpu_limit %||% 1L)
      mission(claimed$task$mission %||% list())
      events(list())
      events_signature("")
      append_local_event("mission_received", list(message = "Mission secured; preparing the scientific workspace"))
      lease(claimed$task)
      lease_server_url(claimed_server_url)
      resource_history(data.frame())
      last_heartbeat_at(as.POSIXct(NA))
      process_files(list(
        events = event_path, result = result_path, payload = payload_path,
        launcher = launcher_path, stdout = stdout_path, stderr = stderr_path,
        started_at = Sys.time()
      ))
      launch_pending(TRUE)
      state("preparing")
      scheduled_task_id <- as.character(claimed$task$task_id %||% "")
      session$onFlushed(function() {
        active <- shiny::isolate(enabled())
        active_lease <- shiny::isolate(lease())
        still_reserved <- isTRUE(active) && is.list(active_lease) &&
          identical(as.character(active_lease$task_id %||% ""), scheduled_task_id)
        if (!still_reserved) {
          launch_pending(FALSE)
          unlink(c(event_path, result_path, payload_path, launcher_path, stdout_path, stderr_path), force = TRUE)
          process_files(list())
          return()
        }
        worker <- tryCatch({
          if (!requireNamespace("processx", quietly = TRUE)) {
            stop("Package 'processx' is required to start a collaborative experiment.", call. = FALSE)
          }
          writeLines(c(
            "args <- commandArgs(trailingOnly = TRUE)",
            ".libPaths(readRDS(args[[5]]))",
            "library(ugplot)",
            "runner <- get('ugplot_collaboration_run_payload', envir = asNamespace('ugplot'))",
            "payload <- readRDS(args[[1]])",
            "result <- runner(payload, cpu_limit = as.integer(args[[2]]), event_path = args[[3]])",
            "saveRDS(result, args[[4]])"
          ), launcher_path, useBytes = TRUE)
          lib_paths_path <- tempfile("ugplot-collab-libs-", fileext = ".rds")
          saveRDS(.libPaths(), lib_paths_path)
          current_files <- shiny::isolate(process_files())
          current_files$lib_paths <- lib_paths_path
          process_files(current_files)
          processx::process$new(
            command = file.path(R.home("bin"), "Rscript"),
            args = c("--vanilla", launcher_path, payload_path, as.character(cpu_limit), event_path, result_path, lib_paths_path),
            stdout = stdout_path,
            stderr = stderr_path,
            cleanup = TRUE,
            cleanup_tree = TRUE,
            windows_hide_window = TRUE
          )
        }, error = function(e) e)
        if (inherits(worker, "error")) {
          try(ugplot_remote_collaboration_release(
            claimed_server_url, claimed$task$task_id, claimed$task$lease_id, client_id
          ), silent = TRUE)
          launch_pending(FALSE)
          lease(NULL)
          lease_server_url("")
          network_note(paste("Could not start experiment:", conditionMessage(worker)))
          state("error")
          files <- shiny::isolate(process_files())
          unlink(unlist(files[c("events", "result", "payload", "launcher", "stdout", "stderr", "lib_paths")]), force = TRUE)
          process_files(list())
          return()
        }
        process(worker)
        launch_pending(FALSE)
        state("computing")
      }, once = TRUE)
    })

    event_types <- shiny::reactive(vapply(events(), function(event) as.character(event$type %||% ""), character(1)))

    output$journey <- shiny::renderUI({
      steps <- c(
        mission_received = "Mission", dataset_profiled = "Explore", experiment_started = "Experiment",
        metric_updated = "Compare", validation_completed = "Validate", result_accepted = "Contribute"
      )
      reached <- vapply(names(steps), function(type) type %in% event_types(), logical(1))
      if (isTRUE(reached[["metric_updated"]])) reached[["experiment_started"]] <- TRUE
      if (isTRUE(reached[["validation_completed"]])) reached[c("experiment_started", "metric_updated")] <- TRUE
      active_index <- if (any(!reached)) which(!reached)[[1]] else length(steps)
      shiny::tags$div(
        class = "collab-journey",
        lapply(seq_along(steps), function(i) shiny::tags$div(
          class = paste("collab-journey-step", if (reached[[i]]) "done" else if (i == active_index && enabled()) "active" else ""),
          unname(steps[[i]])
        ))
      )
    })

    output$mission <- shiny::renderUI({
      current <- mission()
      profile_event <- latest_event("dataset_profiled")
      profile <- profile_event$data %||% list()
      shiny::tags$div(
        shiny::tags$div(class = "collab-panel-heading", "Current mission"),
        shiny::tags$div(
          class = "collab-mission-title",
          htmltools::htmlEscape(as.character(current$title %||% "Waiting for the next scientific mission"))
        ),
        shiny::tags$p(
          class = "collab-mission-copy",
          htmltools::htmlEscape(as.character(current$description %||% "When a compatible mission becomes available, its real scientific context will appear here."))
        ),
        shiny::tags$div(
          class = "collab-stat-row",
          shiny::tags$span(class = "collab-stat-pill", paste(profile$rows %||% "—", "observations")),
          shiny::tags$span(class = "collab-stat-pill", paste(profile$columns %||% "—", "variables")),
          if (!is.null(profile$total_values)) shiny::tags$span(
            class = "collab-stat-pill",
            paste(format(as.numeric(profile$total_values), big.mark = ",", scientific = FALSE), "values")
          ),
          shiny::tags$span(class = "collab-stat-pill", paste0(round(as.numeric(profile$missing_pct %||% 0), 2), "% missing")),
          shiny::tags$span(class = "collab-stat-pill", paste(input$cpu_limit %||% 1L, "CPU cores"))
        )
      )
    })

    output$variable_explorer <- shiny::renderUI({
      profile <- (latest_event("dataset_profiled") %||% list())$data %||% list()
      variable_names <- as.character(unlist(profile$variable_names %||% character(0), use.names = FALSE))
      variable_types <- as.character(unlist(profile$variable_types %||% character(0), use.names = FALSE))
      target_name <- as.character(profile$target_name %||% "")
      target_label <- as.character(profile$target_label %||% target_name)
      metadata <- profile$metadata %||% list()
      target_summary <- profile$target_summary %||% list()
      if (length(variable_names) == 0L) {
        return(shiny::tags$div(class = "collab-empty-visual", "Profiling variables..."))
      }
      if (length(variable_types) < length(variable_names)) {
        variable_types <- c(variable_types, rep("value", length(variable_names) - length(variable_types)))
      }
      metadata_cards <- if (is.list(metadata) && length(metadata) > 0L) {
        lapply(names(metadata), function(name) {
          label <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", name)
          shiny::tags$div(
            class = "collab-metadata-item",
            shiny::tags$span(htmltools::htmlEscape(label)),
            shiny::tags$strong(htmltools::htmlEscape(as.character(metadata[[name]])))
          )
        })
      } else NULL
      summary_items <- Filter(Negate(is.null), list(
        if (!is.null(target_summary$minimum)) c("Minimum", signif(as.numeric(target_summary$minimum), 5)),
        if (!is.null(target_summary$median)) c("Median", signif(as.numeric(target_summary$median), 5)),
        if (!is.null(target_summary$maximum)) c("Maximum", signif(as.numeric(target_summary$maximum), 5)),
        if (!is.null(target_summary$distinct)) c("Distinct", as.character(target_summary$distinct))
      ))
      shiny::tags$div(
        class = "collab-variable-explorer",
        shiny::tags$div(
          class = "collab-dataset-headline",
          shiny::tags$span("Target"),
          shiny::tags$strong(htmltools::htmlEscape(target_label)),
          shiny::tags$small(paste(format(as.numeric(profile$total_values %||% 0), big.mark = ",", scientific = FALSE), "measured values"))
        ),
        if (length(metadata_cards) > 0L) shiny::tags$div(class = "collab-metadata-grid", metadata_cards),
        if (length(summary_items) > 0L) shiny::tags$div(
          class = "collab-target-summary",
          lapply(summary_items, function(item) shiny::tags$span(shiny::tags$small(item[[1]]), shiny::tags$strong(item[[2]])))
        ),
        shiny::tags$div(
          class = "collab-variable-cloud",
          lapply(seq_along(variable_names), function(i) shiny::tags$span(
            class = paste("collab-variable-chip", if (identical(variable_names[[i]], target_name)) "target" else ""),
            shiny::tags$strong(htmltools::htmlEscape(variable_names[[i]])),
            shiny::tags$small(htmltools::htmlEscape(variable_types[[i]]))
          ))
        )
      )
    })

    output$dataset_plot <- plotly::renderPlotly({
      profile <- (latest_event("dataset_profiled") %||% list())$data %||% list()
      distribution <- profile$target_distribution %||% list()
      labels <- unlist(distribution$labels %||% character(0), use.names = FALSE)
      counts <- suppressWarnings(as.numeric(unlist(distribution$counts %||% numeric(0), use.names = FALSE)))
      valid <- is.finite(counts)
      labels <- labels[valid]
      counts <- counts[valid]
      if (length(counts) == 0L) return(empty_plot("Target distribution will appear here"))
      target_label <- as.character(profile$target_label %||% profile$target_name %||% "Target")
      plotly::plot_ly(
        x = labels, y = counts, type = "bar",
        marker = list(color = counts, colorscale = list(c(0, "#7557ff"), c(1, "#16c7d9")), showscale = FALSE),
        hovertemplate = "%{x}<br>%{y} observations<extra></extra>"
      ) %>% plotly::layout(
        title = list(text = paste("Distribution of", target_label), font = list(size = 14)),
        margin = list(l = 45, r = 15, t = 42, b = 48),
        xaxis = list(title = target_label, gridcolor = "#f0f2f8"),
        yaxis = list(title = "Observations", gridcolor = "#f0f2f8")
      )
    })

    output$current_experiment <- shiny::renderUI({
      progress_events <- Filter(function(event) {
        identical(event$type %||% "", "progress_updated")
      }, events())
      progress_event <- if (length(progress_events) > 0L) utils::tail(progress_events, 1L)[[1]] else NULL
      current <- latest_event("experiment_started") %||% progress_event
      metric_event <- latest_event("metric_updated")
      if (is.null(current) && is.null(metric_event)) {
        if (identical(state(), "preparing")) {
          return(shiny::tags$div(
            class = "collab-candidate",
            shiny::icon("flask"),
            "Preparing the scientific workspace and dataset"
          ))
        }
        return(shiny::tags$div(
          class = "collab-empty-visual",
          shiny::tags$div(shiny::tags$span(class = "collab-empty-icon", shiny::icon("atom")), "Candidates will appear here as the experiment begins.")
        ))
      }
      data <- current$data %||% list()
      candidate <- as.character(data$candidate %||% "")
      message <- as.character(data$message %||% "")
      if (!nzchar(candidate) && grepl("Running[[:space:]]+[^[:space:]]+", message)) {
        candidate <- sub(".*Running[[:space:]]+([^[:space:]]+).*", "\\1", message)
      }
      if (!nzchar(candidate)) candidate <- as.character((metric_event$data %||% list())$candidate %||% "Preparing candidate")
      progress <- suppressWarnings(as.numeric(data$progress %||% NA_real_))
      shiny::tags$div(
        class = "collab-current-experiment",
        shiny::tags$div(
          class = "collab-candidate", shiny::icon("microchip"),
          htmltools::htmlEscape(candidate),
          if (is.finite(progress)) shiny::tags$small(paste0(round(100 * progress, 1), "%")),
          if (!is.null(data$training_seed)) shiny::tags$small(paste("seed", data$training_seed))
        ),
        if (nzchar(message)) shiny::tags$p(class = "collab-experiment-message", htmltools::htmlEscape(message)),
        if (is.finite(progress)) shiny::tags$div(
          class = "collab-experiment-progress",
          shiny::tags$span(style = paste0("width:", max(0, min(100, 100 * progress)), "%"))
        )
      )
    })

    output$metric_plot <- plotly::renderPlotly({
      metric_events <- Filter(function(event) identical(event$type, "metric_updated"), events())
      rows <- Filter(Negate(is.null), lapply(seq_along(metric_events), function(i) {
        event <- metric_events[[i]]
        metrics <- event$data$metrics %||% list()
        if (length(metrics) == 0L) return(NULL)
        completed <- suppressWarnings(as.numeric(event$data$completed %||% i))
        if (!is.finite(completed)) completed <- i
        data.frame(
          experiment = completed,
          candidate = as.character(event$data$candidate %||% paste("Candidate", i)),
          metric = names(metrics), value = as.numeric(unlist(metrics, use.names = FALSE)),
          stringsAsFactors = FALSE
        )
      }))
      if (length(rows) == 0L) return(empty_plot("Real metrics will grow here"))
      data <- do.call(rbind, rows)
      data <- data[is.finite(data$value), , drop = FALSE]
      if (nrow(data) == 0L) return(empty_plot("Real metrics will grow here"))
      metric_names <- unique(as.character(data$metric))
      primary_metric <- grepl("^(R2|R\\^2|R²)$", toupper(metric_names))
      ordered_metrics <- c(metric_names[primary_metric], metric_names[!primary_metric])
      named_colors <- c(R2 = "#35a326", "R^2" = "#35a326", "R²" = "#35a326", MAE = "#e56883", RMSE = "#109bd3")
      fallback_colors <- grDevices::hcl.colors(max(3L, length(ordered_metrics)), palette = "Dark 3")
      plot <- NULL
      for (i in seq_along(ordered_metrics)) {
        metric_name <- ordered_metrics[[i]]
        metric_data <- data[data$metric == metric_name, , drop = FALSE]
        color_key <- toupper(metric_name)
        color <- if (color_key %in% names(named_colors)) unname(named_colors[[color_key]]) else fallback_colors[[i]]
        trace <- list(
          x = metric_data$experiment, y = metric_data$value,
          name = metric_name, type = "scattergl", mode = "lines+markers",
          yaxis = if (grepl("^(R2|R\\^2|R²)$", color_key)) "y" else "y2",
          marker = list(size = 7, color = color), line = list(width = 3, color = color),
          text = paste(metric_data$candidate, "·", metric_name),
          hovertemplate = "%{text}<br>Experiment %{x}<br>%{y:.4f}<extra></extra>"
        )
        plot <- if (is.null(plot)) {
          do.call(plotly::plot_ly, trace)
        } else {
          do.call(plotly::add_trace, c(list(p = plot), trace))
        }
      }
      plot %>% plotly::layout(
        margin = list(l = 55, r = 58, t = 10, b = 42),
        xaxis = list(title = "Completed experiments", gridcolor = "#eef0f7"),
        yaxis = list(title = "R²", range = c(0, 1), dtick = 0.1, fixedrange = FALSE, gridcolor = "#eef0f7", zeroline = TRUE),
        yaxis2 = list(title = "MAE / RMSE", overlaying = "y", side = "right", rangemode = "tozero", showgrid = FALSE),
        legend = list(orientation = "h", y = -0.25)
      )
    })

    output$resource_plot <- plotly::renderPlotly({
      history <- resource_history()
      if (!is.data.frame(history) || nrow(history) == 0L) return(empty_plot("Workstation at rest"))
      history$sample <- seq_len(nrow(history))
      cpu_count <- suppressWarnings(as.numeric(utils::tail(history$host_cpu_count, 1L)))
      history$cpu_share <- suppressWarnings(as.numeric(history$process_cpu_pct)) / max(1, cpu_count)
      history$memory_gb <- suppressWarnings(as.numeric(history$process_rss_mb)) / 1024
      plotly::plot_ly(history, x = ~sample) %>%
        plotly::add_lines(y = ~cpu_share, name = "CPU %", line = list(color = "#7557ff", width = 3), fill = "tozeroy", fillcolor = "rgba(117,87,255,.10)") %>%
        plotly::add_lines(y = ~memory_gb, name = "Memory GB", yaxis = "y2", line = list(color = "#16c7d9", width = 3)) %>%
        plotly::layout(
          margin = list(l = 42, r = 42, t = 5, b = 28), legend = list(orientation = "h", y = 1.15),
          xaxis = list(title = "", showticklabels = FALSE, gridcolor = "#f0f2f8"),
          yaxis = list(title = "CPU %", rangemode = "tozero", gridcolor = "#f0f2f8"),
          yaxis2 = list(title = "GB", overlaying = "y", side = "right", rangemode = "tozero")
        )
    })

    output$discovery <- shiny::renderUI({
      metric_events <- Filter(function(event) {
        identical(event$type %||% "", "metric_updated") && length((event$data %||% list())$metrics %||% list()) > 0L
      }, events())
      if (length(metric_events) == 0L) {
        return(shiny::tags$div(
          class = "collab-empty-visual",
          shiny::tags$div(shiny::tags$span(class = "collab-empty-icon", shiny::icon("gem")), "A result card will emerge from the evidence.")
        ))
      }
      r2_values <- vapply(metric_events, function(event) {
        metrics <- (event$data %||% list())$metrics %||% list()
        names_upper <- toupper(names(metrics))
        index <- which(names_upper %in% c("R2", "R^2", "R²"))
        if (length(index) == 0L) return(NA_real_)
        suppressWarnings(as.numeric(metrics[[index[[1]]]]))
      }, numeric(1))
      if (any(is.finite(r2_values))) {
        metric_event <- metric_events[[which.max(replace(r2_values, !is.finite(r2_values), -Inf))]]
        data <- metric_event$data %||% list()
        metric_name <- "Best R²"
        metric_value <- round(max(r2_values, na.rm = TRUE), 4)
        evidence_label <- "Strongest evidence from"
      } else {
        metric_event <- utils::tail(metric_events, 1L)[[1]]
        data <- metric_event$data %||% list()
        metrics <- data$metrics %||% list()
        metric_name <- names(metrics)[[1]]
        metric_value <- round(as.numeric(metrics[[1]]), 4)
        evidence_label <- "Latest evidence from"
      }
      shiny::tags$div(
        shiny::tags$div(class = "collab-discovery-score", metric_value),
        shiny::tags$strong(htmltools::htmlEscape(metric_name)),
        shiny::tags$p(style = "color:#7d8498; margin-top:8px;", paste(evidence_label, data$candidate %||% "the current candidate")),
        if (!is.null(latest_event("validation_completed"))) shiny::tags$div(class = "collab-compat-ok", shiny::icon("circle-check"), " Validation completed")
      )
    })

    output$impact <- shiny::renderUI({
      totals <- impact()
      worker <- process()
      resource_history()
      live_experiments <- if (!is.null(worker)) {
        sum(vapply(events(), function(event) identical(event$type %||% "", "metric_updated"), logical(1)))
      } else 0L
      files <- process_files()
      live_seconds <- if (!is.null(worker) && !is.null(files$started_at)) {
        max(0, as.numeric(difftime(Sys.time(), files$started_at, units = "secs")))
      } else 0
      shiny::tags$div(
        class = "collab-impact-grid",
        shiny::tags$div(class = "collab-impact-item", shiny::tags$div(class = "collab-impact-value", totals$accepted), shiny::tags$div(class = "collab-impact-label", "Accepted contributions")),
        shiny::tags$div(class = "collab-impact-item", shiny::tags$div(class = "collab-impact-value", totals$experiments + live_experiments), shiny::tags$div(class = "collab-impact-label", "Experiments conducted")),
        shiny::tags$div(class = "collab-impact-item", shiny::tags$div(class = "collab-impact-value", sprintf("%.2fh", (totals$compute_seconds + live_seconds) / 3600)), shiny::tags$div(class = "collab-impact-label", "Compute donated")),
        shiny::tags$div(class = "collab-impact-item", shiny::tags$div(class = "collab-impact-value", if (enabled()) "ONLINE" else "READY"), shiny::tags$div(class = "collab-impact-label", "Laboratory status"))
      )
    })

    session$onSessionEnded(function() {
      worker <- shiny::isolate(process())
      if (!is.null(worker) && worker$is_alive()) try(worker$kill(), silent = TRUE)
      files <- shiny::isolate(process_files())
      if (length(files) > 0L) {
        unlink(unlist(files[c("events", "result", "payload", "launcher", "stdout", "stderr", "lib_paths")]), force = TRUE)
      }
    })
  })
}
