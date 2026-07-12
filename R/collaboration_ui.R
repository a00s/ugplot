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

ugplot_collaboration_tab_server <- function(id, remote_servers, total_system_cpus = 1L) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- shiny::reactiveVal("idle")
    enabled <- shiny::reactiveVal(FALSE)
    mission <- shiny::reactiveVal(list())
    events <- shiny::reactiveVal(list())
    lease <- shiny::reactiveVal(NULL)
    lease_server_url <- shiny::reactiveVal("")
    process <- shiny::reactiveVal(NULL)
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
      servers <- remote_servers()
      selected <- as.character(input$server_name %||% "")
      row <- servers[as.character(servers$name) == selected, , drop = FALSE]
      if (nrow(row) != 1L) stop("Choose a collaboration coordinator.", call. = FALSE)
      row
    }
    available_coordinators <- function() {
      servers <- remote_servers()
      if (!is.data.frame(servers) || nrow(servers) == 0L) return(servers)
      selected <- as.character(input$server_name %||% "")
      servers[order(as.character(servers$name) != selected), , drop = FALSE]
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
      plotly::plot_ly() %>% plotly::layout(
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(text = message, showarrow = FALSE))
      )
    }

    output$server_picker <- shiny::renderUI({
      servers <- remote_servers()
      if (!is.data.frame(servers) || nrow(servers) == 0L) {
        return(shiny::tags$p(class = "collab-empty-visual", "Configure a remote server first."))
      }
      shiny::selectInput(
        session$ns("server_name"), "Preferred coordinator",
        choices = stats::setNames(as.character(servers$name), as.character(servers$name)),
        selected = as.character(servers$name[[1]])
      )
    })

    output$connection_badge <- shiny::renderUI({
      labels <- c(
        idle = "○ READY TO CONNECT", connecting = "◌ CONNECTING", waiting = "◉ WAITING FOR MISSION",
        computing = "● EXPERIMENT RUNNING", uploading = "↑ RETURNING DISCOVERY",
        accepted = "✓ CONTRIBUTION ACCEPTED", error = "! ATTENTION NEEDED"
      )
      current <- state()
      shiny::tags$div(
        class = paste("collab-connection-badge", if (current %in% c("waiting", "computing", "uploading", "accepted")) "active" else ""),
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
        worker <- process()
        if (!is.null(worker) && worker$is_alive()) try(worker$kill(), silent = TRUE)
        active_lease <- lease()
        if (is.list(active_lease) && nzchar(lease_server_url())) {
          try(ugplot_remote_collaboration_release(
            lease_server_url(), active_lease$task_id, active_lease$lease_id, client_id
          ), silent = TRUE)
        }
        process(NULL)
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
          if (is.list(new_events)) events(new_events)
        }
        if (worker$is_alive()) {
          sample <- tryCatch(
            ugplot_sample_job_resources(
              list(pid = worker$get_pid(), message = "Collaborative experiment"),
              resource_previous()
            ),
            error = function(e) data.frame()
          )
          if (is.data.frame(sample) && nrow(sample) > 0L) {
            resource_previous(as.list(sample[nrow(sample), , drop = FALSE]))
            resource_history(utils::tail(rbind(resource_history(), sample), 120L))
          }
          heartbeat_at <- last_heartbeat_at()
          if (is.list(active_lease) && (is.na(heartbeat_at) || difftime(Sys.time(), heartbeat_at, units = "secs") >= 25)) {
            heartbeat <- tryCatch(
              ugplot_remote_collaboration_heartbeat(server_url, active_lease$task_id, active_lease$lease_id, client_id),
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
        unlink(unlist(files[c("events", "result")]), force = TRUE)
        process(NULL)
        process_files(list())
        lease(NULL)
        lease_server_url("")
        resource_previous(NULL)
        last_claim_at(Sys.time())
        return()
      }

      claimed_at <- last_claim_at()
      if (!is.na(claimed_at) && difftime(Sys.time(), claimed_at, units = "secs") < 6) return()
      last_claim_at(Sys.time())
      capabilities <- list(
        models = claim_models(), cpu_limit = as.integer(input$cpu_limit %||% 1L),
        protocol_version = 1L,
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
        } else if (length(missing_models) > 0L) {
          preview <- paste(utils::head(missing_models, 6L), collapse = ", ")
          suffix <- if (length(missing_models) > 6L) paste0(" +", length(missing_models) - 6L, " more") else ""
          network_note(paste0("Incompatible mission: missing ", preview, suffix))
        } else if (length(supported) == 0L && grepl("mission(s) found", network_note(), fixed = TRUE)) {
          network_note("Legacy missions found; update the coordinator to display exact incompatibilities")
        } else {
          network_note("No compatible mission is currently available")
        }
        state("waiting")
        return()
      }
      actual_models <- ugplot_collaboration_required_models(claimed$payload$config %||% list())
      actual_status <- tryCatch(
        ugplot_model_dependency_status(models = actual_models),
        error = function(e) NULL
      )
      missing_actual <- unique(c(actual_status$models_missing %||% character(0), actual_status$unknown_models %||% character(0)))
      if (length(missing_actual) > 0L) {
        try(ugplot_remote_collaboration_release(
          claimed_server_url, claimed$task$task_id, claimed$task$lease_id, client_id
        ), silent = TRUE)
        network_note(paste("Mission requires missing models:", paste(missing_actual, collapse = ", ")))
        state("waiting")
        return()
      }
      network_note(paste("Mission accepted from", claimed_server_url))
      event_path <- tempfile("ugplot-collab-events-", fileext = ".rds")
      result_path <- tempfile("ugplot-collab-result-", fileext = ".rds")
      cpu_limit <- as.integer(input$cpu_limit %||% 1L)
      worker <- callr::r_bg(
        func = function(payload, cpu_limit, event_path, result_path, lib_paths) {
          .libPaths(lib_paths)
          library(ugplot)
          runner <- get("ugplot_collaboration_run_payload", envir = asNamespace("ugplot"))
          saveRDS(runner(payload, cpu_limit = cpu_limit, event_path = event_path), result_path)
        },
        args = list(
          payload = claimed$payload, cpu_limit = cpu_limit, event_path = event_path,
          result_path = result_path, lib_paths = .libPaths()
        ),
        supervise = TRUE, cleanup = FALSE, poll_connection = FALSE
      )
      mission(claimed$task$mission %||% list())
      events(list())
      lease(claimed$task)
      lease_server_url(claimed_server_url)
      process_files(list(events = event_path, result = result_path, started_at = Sys.time()))
      process(worker)
      resource_history(data.frame())
      last_heartbeat_at(as.POSIXct(NA))
      state("computing")
    })

    event_types <- shiny::reactive(vapply(events(), function(event) as.character(event$type %||% ""), character(1)))

    output$journey <- shiny::renderUI({
      steps <- c(
        mission_received = "Mission", dataset_profiled = "Explore", experiment_started = "Experiment",
        metric_updated = "Compare", validation_completed = "Validate", result_accepted = "Contribute"
      )
      reached <- vapply(names(steps), function(type) type %in% event_types(), logical(1))
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
          shiny::tags$span(class = "collab-stat-pill", paste0(round(as.numeric(profile$missing_pct %||% 0), 2), "% missing")),
          shiny::tags$span(class = "collab-stat-pill", paste(input$cpu_limit %||% 1L, "CPU cores"))
        )
      )
    })

    output$current_experiment <- shiny::renderUI({
      current <- latest_event("experiment_started")
      if (is.null(current) && is.null(latest_event("metric_updated"))) {
        return(shiny::tags$div(
          class = "collab-empty-visual",
          shiny::tags$div(shiny::tags$span(class = "collab-empty-icon", shiny::icon("atom")), "Candidates will appear here as the experiment begins.")
        ))
      }
      data <- current$data %||% list()
      shiny::tags$div(
        class = "collab-candidate", shiny::icon("microchip"),
        htmltools::htmlEscape(as.character(data$candidate %||% "Preparing candidate")),
        if (!is.null(data$training_seed)) shiny::tags$small(paste("seed", data$training_seed))
      )
    })

    output$metric_plot <- plotly::renderPlotly({
      metric_events <- Filter(function(event) identical(event$type, "metric_updated"), events())
      rows <- Filter(Negate(is.null), lapply(seq_along(metric_events), function(i) {
        event <- metric_events[[i]]
        metrics <- event$data$metrics %||% list()
        if (length(metrics) == 0L) return(NULL)
        data.frame(
          experiment = i,
          candidate = as.character(event$data$candidate %||% paste("Candidate", i)),
          metric = names(metrics), value = as.numeric(unlist(metrics, use.names = FALSE)),
          stringsAsFactors = FALSE
        )
      }))
      if (length(rows) == 0L) return(empty_plot("Real metrics will grow here"))
      data <- do.call(rbind, rows)
      plotly::plot_ly(
        data, x = ~experiment, y = ~value, color = ~candidate, split = ~metric,
        type = "scatter", mode = "lines+markers", marker = list(size = 8), line = list(width = 3),
        hovertemplate = "%{text}<br>%{y:.4f}<extra></extra>", text = ~paste(candidate, metric)
      ) %>% plotly::layout(
        margin = list(l = 45, r = 15, t = 10, b = 38),
        xaxis = list(title = "Completed experiments", gridcolor = "#eef0f7"),
        yaxis = list(title = "Metric", gridcolor = "#eef0f7"),
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
        plotly::add_lines(y = ~cpu_share, name = "CPU %", line = list(color = "#087f83", width = 3), fill = "tozeroy", fillcolor = "rgba(16,200,194,.12)") %>%
        plotly::add_lines(y = ~memory_gb, name = "Memory GB", yaxis = "y2", line = list(color = "#7557ff", width = 3)) %>%
        plotly::layout(
          margin = list(l = 42, r = 42, t = 5, b = 28), legend = list(orientation = "h", y = 1.15),
          xaxis = list(title = "", showticklabels = FALSE, gridcolor = "#f0f2f8"),
          yaxis = list(title = "CPU %", rangemode = "tozero", gridcolor = "#f0f2f8"),
          yaxis2 = list(title = "GB", overlaying = "y", side = "right", rangemode = "tozero")
        )
    })

    output$discovery <- shiny::renderUI({
      metric_event <- latest_event("metric_updated")
      if (is.null(metric_event)) {
        return(shiny::tags$div(
          class = "collab-empty-visual",
          shiny::tags$div(shiny::tags$span(class = "collab-empty-icon", shiny::icon("gem")), "A result card will emerge from the evidence.")
        ))
      }
      data <- metric_event$data %||% list()
      metrics <- data$metrics %||% list()
      metric_name <- if (length(metrics) > 0L) names(metrics)[[1]] else "Experiments"
      metric_value <- if (length(metrics) > 0L) round(as.numeric(metrics[[1]]), 4) else data$completed %||% 0L
      shiny::tags$div(
        shiny::tags$div(class = "collab-discovery-score", metric_value),
        shiny::tags$strong(htmltools::htmlEscape(metric_name)),
        shiny::tags$p(style = "color:#7d8498; margin-top:8px;", paste("Latest evidence from", data$candidate %||% "the current candidate")),
        if (!is.null(latest_event("validation_completed"))) shiny::tags$div(class = "collab-compat-ok", shiny::icon("circle-check"), " Validation completed")
      )
    })

    output$impact <- shiny::renderUI({
      totals <- impact()
      shiny::tags$div(
        class = "collab-impact-grid",
        shiny::tags$div(class = "collab-impact-item", shiny::tags$div(class = "collab-impact-value", totals$accepted), shiny::tags$div(class = "collab-impact-label", "Accepted contributions")),
        shiny::tags$div(class = "collab-impact-item", shiny::tags$div(class = "collab-impact-value", totals$experiments), shiny::tags$div(class = "collab-impact-label", "Experiments conducted")),
        shiny::tags$div(class = "collab-impact-item", shiny::tags$div(class = "collab-impact-value", sprintf("%.1fh", totals$compute_seconds / 3600)), shiny::tags$div(class = "collab-impact-label", "Compute donated")),
        shiny::tags$div(class = "collab-impact-item", shiny::tags$div(class = "collab-impact-value", if (enabled()) "ONLINE" else "READY"), shiny::tags$div(class = "collab-impact-label", "Laboratory status"))
      )
    })

    session$onSessionEnded(function() {
      worker <- shiny::isolate(process())
      if (!is.null(worker) && worker$is_alive()) try(worker$kill(), silent = TRUE)
    })
  })
}
