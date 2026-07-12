# app.R

# Load required libraries
library(shiny)
library(shinyWidgets)
library(shinybusy)
library(ggplot2)
library(heatmap3)
library(DT)
library(gplots)
library(viridis)
library(RColorBrewer)
library(dendextend)
library(pheatmap)
library(glmnet)
library(plotly)
library(tidyr)
library(caret)
library(base64enc)
library(shinyjs)
library(ggExtra)
library(gridExtra)
library(randomForest)  # Required for Windows
library(doParallel)
library(R.utils)
library(ConsensusClusterPlus)
library(gam)

log_file_path <- "ugplot.log"

write_checkpoint_log <- function(last_model = "-", results_table = NULL, context = list(), max_rows = 20) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  context_value <- function(name, default = "N/A") {
    value <- context[[name]]
    if (is.null(value) || length(value) == 0 || is.na(value)) default else as.character(value)
  }
  header_lines <- c(
    paste0("ugplot checkpoint log (last update: ", timestamp, ")"),
    paste0("Last model analyzed: ", last_model),
    paste0("Phase: ", context_value("phase", "unknown")),
    paste0("Current model: ", context_value("current_model", last_model)),
    paste0("Dataset seed: ", context_value("dataset_seed")),
    paste0("Training seed: ", context_value("training_seed")),
    paste0("Run progress: ", context_value("current_run"), "/", context_value("total_runs")),
    paste0("Model progress: ", context_value("model_position"), "/", context_value("total_models")),
    paste0("Best model so far: ", context_value("best_model")),
    paste0("Best metric so far: ", context_value("best_metric")),
    ""
  )

  if (is.null(results_table) || !is.data.frame(results_table) || nrow(results_table) == 0) {
    body_lines <- "No results recorded yet."
  } else {
    status_lines <- character(0)
    if ("Status" %in% names(results_table)) {
      status_counts <- table(results_table$Status, useNA = "ifany")
      status_lines <- c(
        "Status counts:",
        paste0("  ", names(status_counts), ": ", as.integer(status_counts)),
        ""
      )
    }
    metric_col <- if ("Accuracy" %in% names(results_table)) {
      "Accuracy"
    } else if ("R2" %in% names(results_table)) {
      "R2"
    } else {
      NULL
    }
    ordered_results <- results_table
    if (!is.null(metric_col)) {
      metric_values <- suppressWarnings(as.numeric(as.character(ordered_results[[metric_col]])))
      ordered_idx <- order(-metric_values, na.last = TRUE)
      ordered_results <- ordered_results[ordered_idx, , drop = FALSE]
    }
    priority_columns <- c(
      "Model", "R2", "Accuracy", "MAE", "RMSE",
      "dataset_seed", "training_seed", "threshold_scope", "imputation_scope",
      "elapsed_seconds", "Status", "Error"
    )
    ordered_columns <- c(intersect(priority_columns, names(ordered_results)), setdiff(names(ordered_results), priority_columns))
    ordered_results <- ordered_results[, ordered_columns, drop = FALSE]
    for (metric_name in intersect(c("R2", "Accuracy", "MAE", "RMSE", "elapsed_seconds"), names(ordered_results))) {
      metric_values <- suppressWarnings(as.numeric(ordered_results[[metric_name]]))
      ordered_results[[metric_name]] <- ifelse(is.finite(metric_values), round(metric_values, 4), NA)
    }
    ordered_results <- utils::head(ordered_results, max_rows)
    body_lines <- c(
      status_lines,
      paste0("Latest results (top ", nrow(ordered_results), "):"),
      capture.output(print(ordered_results, row.names = FALSE))
    )
  }

  writeLines(c(header_lines, body_lines), con = log_file_path, useBytes = TRUE)
}

format_running_metric_distribution <- function(values, metric_name = "R2", bins = 8, width = 18) {
  values <- suppressWarnings(as.numeric(unlist(values, use.names = FALSE)))
  values <- values[is.finite(values)]
  bar_char <- "\u2588"

  if (length(values) == 0) {
    return(paste0("DISTRIBUTION : waiting for ", metric_name, " results"))
  }

  if (length(unique(values)) == 1) {
    bar <- paste(rep(bar_char, min(width, max(1, length(values)))), collapse = "")
    return(paste0(
      "DISTRIBUTION : ", metric_name, " (n=", length(values), ")\n",
      sprintf("%.4f | %s", values[[1]], bar)
    ))
  }

  breaks <- pretty(range(values), n = bins)
  if (length(breaks) < 2) {
    breaks <- seq(min(values), max(values), length.out = bins + 1)
  }
  counts <- hist(values, breaks = breaks, plot = FALSE, include.lowest = TRUE)$counts
  max_count <- max(counts)
  bars <- vapply(counts, function(count) {
    bar_size <- if (max_count > 0) round((count / max_count) * width) else 0
    if (count > 0 && bar_size == 0) {
      bar_size <- 1
    }
    paste(rep(bar_char, bar_size), collapse = "")
  }, character(1))

  interval_labels <- paste0(
    sprintf("%.2f", utils::head(breaks, -1)),
    "-",
    sprintf("%.2f", utils::tail(breaks, -1))
  )
  lines <- paste0(interval_labels, " | ", bars, " ", counts)
  paste(c(paste0("DISTRIBUTION : ", metric_name, " (n=", length(values), ")"), lines), collapse = "\n")
}

format_running_stability_signal <- function(values, metric_name = "R2") {
  values <- suppressWarnings(as.numeric(unlist(values, use.names = FALSE)))
  values <- values[is.finite(values)]
  n_values <- length(values)

  if (n_values < 30) {
    return(paste0(
      "Stability: collecting data (n=", n_values, "/30)"
    ))
  }

  recent_n <- min(100, max(20, floor(n_values * 0.25)))
  recent_values <- utils::tail(values, recent_n)
  previous_values <- utils::head(values, n_values - recent_n)
  reference_values <- if (length(previous_values) >= 10) previous_values else values

  mean_shift <- abs(mean(recent_values) - mean(reference_values))
  median_shift <- abs(stats::median(recent_values) - stats::median(reference_values))
  metric_se <- stats::sd(values) / sqrt(n_values)
  metric_se <- if (is.finite(metric_se)) metric_se else 0

  stable_green <- n_values >= 100 &&
    mean_shift <= 0.005 &&
    median_shift <= 0.005 &&
    metric_se <= 0.01
  stable_yellow <- n_values >= 50 &&
    mean_shift <= 0.015 &&
    median_shift <= 0.015 &&
    metric_se <= 0.02

  status <- if (stable_green) {
    "stable"
  } else if (stable_yellow) {
    "getting stable"
  } else {
    "still moving"
  }

  paste0(
    "Stability: ", status,
    " | n=", n_values,
    " | delta mean ", round(mean_shift, 4),
    " | delta median ", round(median_shift, 4),
    " | SE ", round(metric_se, 4)
  )
}
# Optional: set maximum number of threads
# Sys.setenv(OMP_NUM_THREADS = 2)
# Sys.setenv(MKL_NUM_THREADS = 2)
# Sys.setenv(OPENBLAS_NUM_THREADS = 2)

options(shiny.maxRequestSize = 800 * 1024 * 1024)


`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

source_local_helper <- function(file_name, function_name = NULL, always_reload = FALSE) {
  if (!always_reload && !is.null(function_name) && exists(function_name, mode = "function", inherits = TRUE)) {
    return(invisible(TRUE))
  }
  target_env <- parent.frame()
  candidate_paths <- c(
    file.path("R", file_name),
    file_name
  )
  for (candidate_path in candidate_paths) {
    if (file.exists(candidate_path)) {
      source(candidate_path, local = target_env)
      return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}

source_local_helper("00_version.R", "ugplot_build_version", always_reload = TRUE)
source_local_helper("job_store.R", "ugplot_ensure_dir", always_reload = TRUE)
source_local_helper("collaboration.R", "ugplot_collaboration_claim_task", always_reload = TRUE)
source_local_helper("collaboration_ui.R", "ugplot_collaboration_tab_ui", always_reload = TRUE)
source_local_helper("server_deps.R", "ugPlotInstallModelDeps", always_reload = TRUE)
source_local_helper("remote_client.R", "ugplot_remote_create_job", always_reload = TRUE)
source_local_helper("remote_servers.R", "ugplot_read_remote_servers", always_reload = TRUE)
source_local_helper("geo_import.R", "ugplot_geo_cache_dir", always_reload = TRUE)
source_local_helper("ml_runner.R", "ugplot_run_ml_job", always_reload = TRUE)
source_local_helper("geo_pipeline_runner.R", "ugplot_run_geo_pipeline_job", always_reload = TRUE)

ugplot_cleanup_global_session_objects <- function() {
  objects_to_remove <- c(
    "dff",
    "changed_table",
    "ml_available",
    "ml_not_available",
    "ml_prediction",
    "df_pre",
    "source_local_helper",
    "ugplot_cleanup_global_session_objects",
    "ugplot_default_jobs_dir",
    "ugplot_new_job_id",
    "ugplot_ensure_dir",
    "ugplot_validate_job_id",
    "ugplot_job_dir",
    "ugplot_status_path",
    "ugplot_result_path",
    "ugplot_preview_result_path",
    "ugplot_best_model_path",
    "ugplot_job_result_preview",
    "ugplot_job_partial_result",
    "ugplot_attach_job_best_model",
    "ugplot_job_completed_run_keys",
    "ugplot_write_rds_atomic",
    "ugplot_read_rds_or_null",
    "ugplot_process_alive",
    "ugplot_terminate_process",
    "ugplot_status_time",
    "ugplot_job_timeout_seconds",
    "ugplot_running_job_timed_out",
    "ugplot_create_job",
    "ugplot_read_job_status",
    "ugplot_write_job_status",
    "ugplot_update_job_status",
    "ugplot_write_job_partial_result",
    "ugplot_stop_job",
    "ugplot_delete_job",
    "ugplot_job_resumable",
    "ugplot_refresh_job_status",
    "ugplot_list_jobs",
    "ugplot_append_job_log",
    "ugplot_read_job_log",
    "ugplot_read_job_result",
    "ugplot_read_job_preview_result",
    "ugplot_read_job_bundle",
    "ugplot_launch_background_job",
    "ugplot_resume_background_job",
    "ugplot_server_r_packages",
    "ugplot_installed_r_packages",
    "ugplot_model_dependency_status",
    "ugplot_print_model_dependency_status",
    "ugPlotCheckModelDeps",
    "ugPlotInstallModelDeps",
    "ugplot_command_exists",
    "ugplot_has_header",
    "ugplot_has_pkg_config_package",
    "ugplot_detect_linux_package_manager",
    "ugplot_server_system_dependency_commands",
    "ugplot_missing_server_system_deps",
    "ugplot_assert_server_system_deps",
    "ugPlotInstallServerDeps",
    "ugplot_remote_url",
    "ugplot_remote_request",
    "ugplot_remote_parse",
    "ugplot_remote_create_job",
    "ugplot_remote_health",
    "ugplot_remote_list_jobs",
    "ugplot_remote_model_deps",
    "ugplot_remote_job_status",
    "ugplot_remote_job_log",
    "ugplot_remote_job_resources",
    "ugplot_remote_geo_cpg_summary",
    "ugplot_remote_geo_cpg_lookup",
    "ugplot_remote_stop_job",
    "ugplot_remote_resume_job",
    "ugplot_remote_delete_job",
    "ugplot_remote_get_result",
    "ugplot_remote_get_job_preview",
    "ugplot_remote_get_job_bundle",
    "ugplot_remote_servers_path",
    "ugplot_default_remote_servers",
    "ugplot_read_remote_servers",
    "ugplot_write_remote_servers",
    "ugplot_upsert_remote_server",
    "ugplot_remove_remote_server"
  )
  existing_objects <- objects_to_remove[
    vapply(objects_to_remove, exists, logical(1), envir = globalenv(), inherits = FALSE)
  ]
  if (length(existing_objects) > 0) {
    rm(list = existing_objects, envir = globalenv())
  }
  invisible(existing_objects)
}

detect_total_cpus <- function() {
  cpu_count <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
  if (is.na(cpu_count) || cpu_count < 1) 1L else as.integer(cpu_count)
}

default_cpu_limit <- function(total_cpus) {
  max(1L, as.integer(total_cpus) - 1L)
}

apply_runtime_thread_limit <- function(cpu_limit) {
  cpu_limit <- max(1L, as.integer(cpu_limit))
  Sys.setenv(
    OMP_NUM_THREADS = cpu_limit,
    MKL_NUM_THREADS = cpu_limit,
    OPENBLAS_NUM_THREADS = cpu_limit,
    VECLIB_MAXIMUM_THREADS = cpu_limit,
    NUMEXPR_NUM_THREADS = cpu_limit,
    UGPlot_CPU_LIMIT = cpu_limit
  )
  if (requireNamespace("torch", quietly = TRUE)) {
    try(torch::torch_set_num_threads(cpu_limit), silent = TRUE)
    try(torch::torch_set_num_interop_threads(max(1L, min(2L, cpu_limit))), silent = TRUE)
  }
  invisible(cpu_limit)
}

total_system_cpus <- detect_total_cpus()
default_system_cpu_limit <- default_cpu_limit(total_system_cpus)

# Auxiliary functions to load example files, palettes, and CSS
resolve_extdata <- function(filename) {
  package_path <- system.file("extdata", filename, package = "ugplot")
  local_inst_path <- file.path("inst", "extdata", filename)
  local_path <- file.path("extdata", filename)

  candidate_paths <- unique(c(package_path, local_inst_path, local_path))
  candidate_paths <- candidate_paths[nzchar(candidate_paths)]
  existing_paths <- candidate_paths[file.exists(candidate_paths)]

  if (length(existing_paths) > 0) {
    return(existing_paths[[1]])
  }

  stop(
    paste0(
      "File '", filename, "' was not found in extdata. ",
      "Install the package with devtools::install() / R CMD INSTALL ",
      "or run the app from a local project structure containing inst/extdata."
    ),
    call. = FALSE
  )
}

read_extdata_lines <- function(filename) {
  resolved_path <- resolve_extdata(filename)

  if (!nzchar(resolved_path) || !file.exists(resolved_path)) {
    stop(
      paste0(
        "Unable to read '", filename, "'. ",
        "Install the package with devtools::install() / R CMD INSTALL ",
        "or use a local project structure with inst/extdata."
      ),
      call. = FALSE
    )
  }

  readLines(resolved_path)
}

path_to_2dplotlist <- function() {
  resolve_extdata("2dplotlist.csv")
}
lines <- read_extdata_lines("2dplotlist.csv")
lines <- lines[!startsWith(trimws(lines), "#")]
plotlist2d <- read.csv(text = lines, sep = ";", header = TRUE)

path_to_plotlist <- function() {
  resolve_extdata("plotlist.csv")
}
lines <- read_extdata_lines("plotlist.csv")
lines <- lines[!startsWith(trimws(lines), "#")]
plotlist <- read.csv(text = lines, sep = ";", header = TRUE)

path_to_palette <- function() {
  resolve_extdata("palette.csv")
}
lines <- read_extdata_lines("palette.csv")
lines <- lines[!startsWith(trimws(lines), "#")]
palettelist <- read.csv(text = lines, sep = ";", header = TRUE)

path_to_css <- function() {
  resolve_extdata("styles.css")
}

path_to_sample_data <- function() {
  resolve_extdata("sample.csv")
}
lines <- read_extdata_lines("sample.csv")
sample_data <- read.csv(text = lines, sep = ",", header = TRUE)
row.names(sample_data) <- sample_data[, 1]
sample_data <- sample_data[, -1]

slow_models <- c(
  'bam', 'ANFIS', 'DENFIS', 'FH.GBML', 'FIR.DM', 'FS.HGD',
  'gam', 'GFS.LT.RS', 'GFS.FR.MOGUL', 'GFS.THRIFT', 'HYFIS',
  'gaussprRadial', 'gaussprLinear', 'rbf', 'randomGLM', 'Rborist', 'null'
)
slow_models_text <- paste("Slow or problematic models automatically removed:",
  paste(slow_models, collapse = ", "))

# Global variables (seguindo o padrao utilizado)
df_pre <<- ""
dff <<- ""
ml_available <<- list()
ml_not_available <<- list()
ml_prediction <<- list()
best_model_object <- reactiveVal(NULL)
best_model_preprocess <- reactiveVal(NULL)

getImage <- function(fileName) {
  image_path <- resolve_extdata(fileName)
  dataURI(file = image_path, mime = "image/png")
}

# Define the UI of the application
ui <- fluidPage(
  tags$script("
    function filterCheckboxGroup(inputSelector, groupSelector) {
      var query = ($(inputSelector).val() || '').toLowerCase().trim();
      $(groupSelector + ' .checkbox').each(function() {
        var label = $(this).text().toLowerCase();
        $(this).toggle(query === '' || label.indexOf(query) !== -1);
      });
    }

    function setupTableListFilters() {
      var filters = [
        ['#filter_columns', '#column_checkbox_group'],
        ['#filter_rows', '#row_checkbox_group'],
        ['#filter_categories', '#checkbox_group_categories'],
        ['#filter_ml_models', '#ml_checkbox_group'],
        ['#filter_ml_missing_models', '#ml_missing_checkbox_group']
      ];
      filters.forEach(function(pair) {
        var inputSelector = pair[0];
        var groupSelector = pair[1];
        $(document)
          .off('input.ugplotFilter', inputSelector)
          .on('input.ugplotFilter', inputSelector, function() {
            filterCheckboxGroup(inputSelector, groupSelector);
          });
        filterCheckboxGroup(inputSelector, groupSelector);
      });
    }

    $(document).on('shiny:sessioninitialized', function(event) {
      setInterval(function() {
        Shiny.setInputValue('keepAlive', Math.random());
      }, 60000);
      setInterval(setupTableListFilters, 500);
    });

    Shiny.addCustomMessageHandler('geoProgress', function(x) {
      var pct = Math.max(0, Math.min(100, Number(x.percent || 0)));
      $('#geoProgressBar').css('width', pct + '%').attr('aria-valuenow', pct);
      $('#geoProgressText').text(Math.round(pct) + '%');
      $('#geoProgressFile').text(x.file || 'Waiting');
      $('#geoProgressDetail').text(x.detail || '');
    });

    Shiny.addCustomMessageHandler('geoMlProgress', function(x) {
      var pct = Math.max(0, Math.min(100, Number(x.percent || 0)));
      var panel = $('#geoMlLiveProgress');
      panel.toggleClass('active', x.active !== false);
      $('#geoMlLiveTitle').text(x.title || 'Transcript ML progress');
      $('#geoMlLiveBar').css('width', pct + '%').attr('aria-valuenow', pct);
      $('#geoMlLivePct').text(Math.round(pct) + '%');
      $('#geoMlLiveTask').text(x.task || '');
      $('#geoMlLiveMessage').text(x.message || '');
      $('#geoMlLiveStability').text(x.stability || '');
      $('#geoMlLiveCache').text(x.cache || '');
      var values = Array.isArray(x.values) ? x.values.map(Number).filter(Number.isFinite) : [];
      var svg = $('#geoMlLiveSpark');
      svg.empty();
      if (values.length > 1) {
        var width = 260, height = 58, pad = 4;
        var min = Math.min.apply(null, values);
        var max = Math.max.apply(null, values);
        var span = max - min || 1;
        var points = values.map(function(v, i) {
          var px = pad + i * ((width - 2 * pad) / Math.max(1, values.length - 1));
          var py = height - pad - ((v - min) / span) * (height - 2 * pad);
          return px.toFixed(1) + ',' + py.toFixed(1);
        }).join(' ');
        svg.attr('viewBox', '0 0 ' + width + ' ' + height);
        svg.append(document.createElementNS('http://www.w3.org/2000/svg', 'polyline'));
        svg.find('polyline').attr({ points: points, fill: 'none', stroke: '#2563eb', 'stroke-width': 2, 'stroke-linecap': 'round', 'stroke-linejoin': 'round' });
      }
    });
  "),
  uiOutput("geo_ml_live_progress_ui"),
  includeCSS(path_to_css()),
  add_busy_spinner(spin = "fading-circle"),
  useShinyjs(),
  titlePanel(tags$img(
    src = getImage("ugplot.png"), height = "50px",
    tags$span(paste("version", ugplot_build_version()), style = "color: gray; font-size: 11px;")
  )),
  tabsetPanel(
    id = "tabs",
    tabPanel("LOAD DATA",
      tags$div(
        style = "display: inline-block; vertical-align: top;",
        class = "small-input",
        numericInput("startfromline", "Start at line", value = 1, min = 1, step = 1)
      ),
      tags$div(
        style = "display: inline-block; vertical-align: top;",
        class = "small-input",
        selectInput(inputId = "separator",
          label = "Separator",
          choices = c("space" = " ", "tab" = "\t", ";", ",", "|"),
          selected = ",")
      ),
      tags$div(
        style = "display: inline-block; vertical-align: top;",
        fileInput("file1", "Choose a CSV file", multiple = FALSE,
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      tags$div(
        style = "display: inline-block; vertical-align: top;",
        tags$div(
          tags$span(style = "font-size: 17px; color: white;", ".")
        ),
        tags$div(
          actionButton("process_table_content", "Load data")
        )
      ),
      conditionalPanel(
        condition = "input.textarea_columns != '' || input.textarea_rows != ''",
        tags$div(
          style = "display: inline-block; text-align: center; vertical-align: top;",
          textAreaInput("textarea_columns", label = "", rows = 20, cols = 50),
          actionButton("add_all_columns", "Add all"),
          actionButton("remove_all_columns", "Remove all"),
          actionButton("merge_all_columns", "Join columns")
        ),
        tags$div(
          style = "display: inline-block; text-align: center; vertical-align: top;",
          textAreaInput("textarea_rows", label = "", rows = 20, cols = 50),
          actionButton("add_all_rows", "Add all"),
          actionButton("remove_all_rows", "Remove all"),
          actionButton("merge_all_rows", "Join columns")
        )
      ),
      tags$div(
        br(),
        actionButton("load_sample", "Click here to load an example")
      )
    ),
    tabPanel("GEO IMPORT",
      fluidPage(
        tags$h4("GEO methylation import"),
        uiOutput("geo_stage_status"),
        uiOutput("geo_workflow_ui"),
        tags$details(class = "geo-debug-log geo-table-section",
          tags$summary("Technical log"),
          tags$div(class = "dl-status-box", verbatimTextOutput("geo_import_status"))
        )
      )
    ),
    tabPanel("TABLE",
      div(
        style = "width: 100%; overflow-x: auto;",
        column(
          width = 4,
          tags$h4("Columns", style = "margin-top: 10px;"),
          tags$div(class = "table-list-filter",
            textInput("filter_columns", NULL, placeholder = "Search columns", width = "100%")),
          div(class = "scrollable-table",
            div(id = "dynamic_columns")),
          actionButton("uncheck_all_columns", "Uncheck all"),
          actionButton("check_all_columns", "Check all"),
          br(),
          tags$div(
            style = "display: inline-block; vertical-align: top;",
            class = "small-input",
            numericInput("minvariability", NULL, value = 10, min = 0.1, step = 0.1)
          ),
          actionButton("remove_columns_variability", "Uncheck variability"),
          br(),
          tags$div(
            class = "scramble-description",
            "Shuffle only one selected column across samples (keeps the same values in random order)."
          ),
          tags$div(
            class = "scramble-controls",
            tags$span("Scramble:", class = "scramble-label"),
            tags$div(
              class = "scramble-select",
              selectInput("scramble_column", NULL, choices = character(0), width = "100%")
            ),
            actionButton("scramble_column_button", "Scramble column"),
            actionButton("reset_scramble_columns", "Restore")
          ),
          br()
        ),
        column(
          width = 4,
          tags$h4("Rows", style = "margin-top: 10px;"),
          tags$div(class = "table-list-filter",
            textInput("filter_rows", NULL, placeholder = "Search rows", width = "100%")),
          div(class = "scrollable-table",
            div(id = "dynamic_rows")),
          actionButton("uncheck_all_rows", "Uncheck all"),
          actionButton("check_all_rows", "Check all"),
          br(), br()
        ),
        column(
          width = 4,
          tags$h4("Categories", style = "margin-top: 10px;"),
          tags$div(class = "table-list-filter",
            textInput("filter_categories", NULL, placeholder = "Search categories", width = "100%")),
          div(class = "scrollable-table",
            style = "background-color: #f7f8fa; overflow-y: auto; max-height: 200px;",
            div(id = "dynamic_columns_categories")),
          actionButton("transpose_table", "Transpose table", icon = icon("retweet")),
          downloadButton("downloadData", "Download"),
          br(), br()
        ),
        uiOutput("table_cleaning_message"), br(),
        uiOutput("table_message"), br(),
        DT::DTOutput("contents")
      )
    ),
    tabPanel("HEATMAP PLOT",
      br(),
      fluidRow(
        column(
          width = 3,
          class = "sidebar-panel-custom",
          selectInput(inputId = "plot_xy", label = NULL, choices = c("ROW x COL", "COL x COL", "ROW x ROW")),
          div(class = "rowplotlist",
            lapply(1:nrow(plotlist), function(i) {
              bname <- paste0("buttonplot", i)
              imgname <- paste0("img/", plotlist$img[i])
              fluidRow(actionButton(bname, tags$img(src = getImage(imgname), height = "130px", width = "130px", class = "image-button")))
            })),
          br(),
          div(class = "rowpalettelist",
            lapply(1:nrow(palettelist), function(i) {
              bname <- paste0("buttonpalette", i)
              imgname <- paste0("img/", palettelist$img[i])
              if (imgname != "img/NA") {
                fluidRow(actionButton(bname, tags$img(src = getImage(imgname), height = "20px", width = "130px", class = "image-button")))
              }
            }))
        ),
        column(
          width = 9,
          class = "plotheatmap",
          tags$div(
            style = "display: flex; width: 100%; align-items: flex-start;",
            tags$div(
              actionButton("run_code_plot", label = tags$i(class = "fa fa-play")),
              style = "flex: none; width: 40px; margin-right: 10px;"
            ),
            tags$div(
              textAreaInput("textarea_code_plot", label = NULL, row = 1, width = '100%'),
              style = "flex-grow: 1;"
            ),
            tags$div(
              downloadButton("downloadHeatmapPlotTiffTop", "Download plot (TIFF)", icon = icon("download")),
              style = "flex: none; margin-left: 10px;"
            )
          ),
          plotOutput("plot", height = "90%"),
          br(),
          downloadButton("downloadHeatmapPlotTiff", "Download plot (TIFF)")
        )
      )
    ),
    tabPanel("2D PLOT",
      class = "sidebar-layout",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar-panel-custom2d",
          div(
            class = "rowplotlist",
            selectInput(
              inputId = "plot2d_column_filter",
              label = "Filter correlations by column",
              choices = c("All columns" = ""),
              selected = ""
            ),
            selectInput(inputId = "correlation", label = NULL, choices = c("pearson", "spearman", "kendall")),
            sliderInput(inputId = "correlation_threshhold", label = "Spearman Correlation >= x", min = 0, max = 1, value = 0.7, step = 0.01),
            sliderInput(inputId = "correlation_threshhold_negative", label = "Negative correlation <= x", min = -1, max = 0, value = -0.7, step = 0.01),
            lapply(1:nrow(plotlist2d), function(i) {
              bname <- paste0("buttonplot2d", i)
              imgname <- paste0("img/", plotlist2d$img[i])
              fluidRow(
                tags$img(src = getImage(imgname), width = 130, height = 130),
                actionButton(bname, plotlist2d$name[i])
              )
            })
          )
        ),
        mainPanel(
          br(),
          uiOutput("plotLoadingIndicator"),
          div(style = "width: 100%; overflow-x: auto; margin-bottom: 20px;", DT::DTOutput("plot2d_results_table")),
          uiOutput("plots")
        )
      )
    ),
    tabPanel("MACHINE LEARNING",
      tags$div(
        style = "display: block; width: 100%;",
        selectizeInput("ml_target", "Target column (healthy, cancer, ...)", choices = ""),
        conditionalPanel(
          condition = "input.ml_target != ''",
          actionButton("ml_toggle_seeds", "\u25b8 Seeds", class = "ml-section-toggle"),
          conditionalPanel(
            condition = "input.ml_toggle_seeds % 2 == 1",
            div(
              class = "ml-section-panel",
              fluidRow(
                column(3, numericInput("ml_dataset_seedi", "Initial Dataset Seed:", step = 1, value = 1)),
                column(3, numericInput("ml_dataset_seedf", "Final Dataset Seed:", step = 1, value = 1)),
                column(3, numericInput("ml_seedi", "Initial Training Seed:", step = 1, value = 1)),
                column(3, numericInput("ml_seedf", "Final Training Seed:", step = 1, value = 1))
              )
            )
          ),
          div(
            class = "ml-threshold-input",
            numericInput("ml_timeout", "Timeout (s):", step = 1, value = 1200)
          ),
          div(
            class = "ml-skip-controls",
            div(
              class = "ml-skip-checkbox",
              checkboxInput(
                "ml_auto_skip_bad_models",
                "Auto-skip models in next rounds (timeout or low R2)",
                value = FALSE
              )
            ),
            div(
              class = "ml-threshold-input ml-skip-threshold",
              numericInput("ml_min_r2_skip", "Min R2 (0-1)", value = 0, min = 0, max = 1, step = 0.01)
            )
          ),
          selectInput(
            "ml_performance_mode",
            "Training effort profile",
            choices = c(
              "System default (faster)" = "default",
              "High effort (slower, can improve results)" = "high_effort",
              "Custom" = "custom"
            ),
            selected = "default"
          ),
          conditionalPanel(
            condition = "input.ml_performance_mode == 'custom'",
            fluidRow(
              column(4, selectInput(
                "ml_cv_method",
                "CV method",
                choices = c("Cross-validation" = "cv", "Repeated cross-validation" = "repeatedcv"),
                selected = "cv"
              )),
              column(4, numericInput("ml_cv_folds", "CV folds", value = 10, min = 2, step = 1)),
              column(4, numericInput("ml_cv_repeats", "CV repeats", value = 1, min = 1, step = 1))
            ),
            numericInput("ml_tune_length", "Hyperparameter search depth (tuneLength)", value = 3, min = 1, step = 1)
          )
        ),
        conditionalPanel(
          condition = "input.ml_target != ''",
          tags$div(
            actionButton("ml_toggle_missing", "\u25b8 Missing Data Strategy", class = "ml-section-toggle"),
            conditionalPanel(
              condition = "input.ml_toggle_missing % 2 == 1",
              div(
                class = "ml-section-panel",
                div(
                  class = "ml-missing-stack",
                  checkboxGroupInput(
                    "ml_missing_definition",
                    "Consider as missing:",
                    choices = c("Empty string" = "empty", "NA" = "na", "Zero (0, 0.0, 0.0000)" = "zero"),
                    selected = c("empty", "na")
                  ),
                  uiOutput("ml_missing_definition_stats"),
                  conditionalPanel(
                    condition = "input.ml_missing_definition && input.ml_missing_definition.indexOf('zero') !== -1",
                    selectizeInput(
                      "ml_zero_exceptions",
                      "Zero rule exceptions (columns where 0 is valid):",
                      choices = NULL,
                      selected = character(0),
                      multiple = TRUE,
                      options = list(
                        plugins = list("remove_button"),
                        placeholder = "Select columns to ignore zero-as-missing"
                      )
                    )
                  ),
                  selectInput(
                    "ml_missing_strategy",
                    "How to handle missing values:",
                    choices = c(
                      "Do nothing" = "none",
                      "Replace with zero" = "replace_zero",
                      "KNN imputation" = "knn",
                      "Mean imputation" = "mean",
                      "missForest imputation" = "missforest",
                      "methyLImp2 imputation" = "methylimp2"
                    ),
                    selected = "none"
                  ),
                  selectInput(
                    "ml_imputation_scope",
                    "Imputation scope",
                    choices = c(
                      "Impute train and test separately" = "split_separate",
                      "Impute all data once (preprocessing)" = "full_once"
                    ),
                    selected = "split_separate"
                  ),
                  div(
                    class = "ml-threshold-input",
                    numericInput(
                      "ml_missing_threshold_cols",
                      "Remove columns when missingness is above (%)",
                      min = 0, max = 100, value = 100, step = 1
                    )
                  ),
                  div(
                    class = "ml-threshold-input",
                    numericInput(
                      "ml_missing_threshold_rows",
                      "Remove samples when missingness is above (%)",
                      min = 0, max = 100, value = 100, step = 1
                    )
                  ),
                  div(
                    class = "ml-threshold-input",
                    numericInput(
                      "ml_complete_case_min_samples",
                      "Complete-case scan: keep at least this many samples (%)",
                      min = 0, max = 100, value = 80, step = 1
                    )
                  ),
                  actionButton("ml_run_threshold_scan", "Find complete-case thresholds"),
                  tags$div(style = "margin-top: 8px;", textOutput("ml_threshold_scan_status"))
                ),
                htmlOutput("ml_missing_summary"),
                htmlOutput("ml_threshold_scan_summary"),
                downloadButton("downloadMissingScanBestDataset", "Download dataset with current thresholds (CSV)"),
                fluidRow(
                  column(6, plotOutput("ml_target_plot_original", height = "220px")),
                  column(6, plotOutput("ml_target_plot_filtered", height = "220px"))
                ),
                fluidRow(
                  column(12, plotOutput("ml_target_plot_removed", height = "220px"))
                )
              )
            ),
            verbatimTextOutput("console_output"),
            tags$div(
              class = "ml-model-source-controls",
              radioButtons(
                "ml_run_target",
                "Model source / run target",
                choices = c("Local" = "local", "Remote server" = "remote"),
                selected = "local",
                inline = TRUE
              ),
              conditionalPanel(
                condition = "input.ml_run_target == 'remote'",
                selectInput("remote_server_name", "Remote server", choices = NULL),
                textInput("remote_job_name", "Job name", value = "")
              ),
              textOutput("ml_model_source_status")
            ),
            column(
              width = 6,
              tags$h4("Models installed", style = "margin-top: 10px;"),
              tags$div(class = "table-list-filter",
                textInput("filter_ml_models", NULL, placeholder = "Search models", width = "100%")),
              div(class = "scrollable-table", div(id = "dynamic_machine_learning")),
              actionButton("uncheck_all_ml", "Uncheck all"),
              actionButton("check_all_ml", "Check all"),
              actionButton("play_search_best_model_caret", "RUN"),
              uiOutput("downloadModelUI"),
              tags$br(),
              tags$p(slow_models_text, style = "color: gray; font-size: 11px;")
            ),
            column(
              width = 6,
              tags$h4("Models missing", style = "margin-top: 10px;"),
              tags$div(class = "table-list-filter",
                textInput("filter_ml_missing_models", NULL, placeholder = "Search missing models", width = "100%")),
              div(class = "scrollable-table", div(id = "dynamic_machine_learning_missing")),
              actionButton("uncheck_all_ml_missing", "Uncheck all"),
              actionButton("check_all_ml_missing", "Check all"),
              conditionalPanel(
                condition = "input.ml_run_target == 'local'",
                actionButton("install_missing_modules", "Install libraries")
              ),
              conditionalPanel(
                condition = "input.ml_run_target == 'remote'",
                tags$p(
                  class = "ml-remote-install-note",
                  "Install missing model libraries on the selected server, then refresh the model source."
                )
              )
            ),
            div(style = "width: 100%; overflow-x: auto;", uiOutput("ml_error_message")),
            div(style = "overflow-x: auto; width: 100%;", uiOutput("dynamic_ml_plot")),
            div(style = "width: 100%; overflow-x: auto;", uiOutput("ml_final_status")),
            div(style = "width: 100%; overflow-x: auto;", DT::DTOutput("ml_table_results_output")),
            verbatimTextOutput("ml_row_details"),
            div(style = "width: 100%; overflow-x: auto;", DT::DTOutput("ml_table"))
          )
        )
      )
    ),
    tabPanel("JOBS",
      fluidPage(
        tags$h4("Remote jobs", class = "section-title"),
        tags$div(
          class = "jobs-toolbar",
          style = "display: flex; align-items: stretch; gap: 8px; flex-wrap: wrap; margin-bottom: 8px;",
          actionButton("remote_refresh_jobs", "Refresh jobs", icon = icon("refresh")),
          uiOutput("remote_server_connection_status"),
          tags$div(style = "display: none;",
            textInput("remote_job_id", "Job ID", value = "", width = "360px"),
            downloadButton("downloadRemoteJobResult", "Download result (RDS)")
          )
        ),
        DT::DTOutput("remote_jobs_table"),
        uiOutput("remote_job_status_panel"),
        uiOutput("remote_job_geo_progress_report"),
        uiOutput("remote_job_resources"),
        uiOutput("remote_job_metric_panel"),
        verbatimTextOutput("remote_job_running_details"),
        uiOutput("remote_job_log_panel")
      )
    ),
    ugplot_collaboration_tab_ui("collaboration", total_system_cpus),
    # MODEL ANALYSIS (vertical layout)
    tabPanel("MODEL ANALYSIS",
      fluidPage(
        # File input and model details display
        fileInput("model_file", "Load RDS Model", accept = c(".rds")),
        verbatimTextOutput("model_details"),
        uiOutput("model_preprocess_ui"),
        ## NOVO: mostrar variavel alvo do modelo
        uiOutput("model_target_var_ui"),
        uiOutput("model_analysis_missing_features_ui"),

        ## NOVO: escolher no dataset qual coluna e o ground truth
        selectInput("dataset_response_col", "Target column:",
                                                   choices = NULL,
                                                   selected = NULL),
        checkboxGroupInput(
          "model_analysis_missing_definition",
          "Consider as missing:",
          choices = c(
            "Empty string" = "empty",
            "NA" = "na",
            "Zero (0, 0.0, 0.0000)" = "zero"
          ),
          selected = c("empty", "na", "zero")
        ),
        numericInput(
          "model_analysis_missing_threshold_rows",
          "Remove samples when missingness is above (%)",
          value = 100,
          min = 0,
          max = 100,
          step = 1
        ),

        # Input for confidence threshold
        numericInput("confidence_threshold", "Confidence Threshold", value = 0.8, min = 0, max = 1, step = 0.01),
        actionButton("run_model_analysis", "Run Analysis"),
        br(), br(),
        # Extra metrics will be displayed here (before the table)
        uiOutput("model_analysis_missing_summary"),
        br(),
        verbatimTextOutput("model_analysis_accuracy"),
        verbatimTextOutput("model_analysis_plot_metrics"),
        br(),
        plotOutput("model_analysis_correlation_plot", height = "520px", width = "520px"),
        br(),
        downloadButton("downloadModelAnalysisPlotTiff", "Download plot (TIFF)"),
        br(),
        downloadButton("downloadModelAnalysisTable", "Download analysis table (CSV)"),
        br(), br(),
        DT::DTOutput("model_analysis_table")
      )
    ),
    tabPanel("DEEP LEARNING",
      fluidPage(
        tags$h4("Deep Learning (torch)"),
        fluidRow(
          column(
            4,
            selectInput("dl_target", "Target column:", choices = NULL),
            selectInput(
              "dl_task",
              "Task type:",
              choices = c("Auto-detect" = "auto", "Classification" = "classification", "Regression" = "regression"),
              selected = "regression"
            ),
            sliderInput("dl_test_split", "Test split (%):", min = 10, max = 40, value = 20, step = 5),
            numericInput("dl_seed", "Random seed:", value = 1, min = 1, step = 1),
            numericInput("dl_epochs", "Epochs:", value = 200, min = 5, step = 5),
            numericInput("dl_batch_size", "Batch size:", value = 32, min = 4, step = 4),
            numericInput("dl_hidden_layers", "Number of hidden layers:", value = 2, min = 1, step = 1),
            uiOutput("dl_hidden_units_ui"),
            numericInput("dl_learning_rate", "Learning rate:", value = 0.0005, min = 0.0001, step = 0.0001),
            numericInput("dl_weight_decay", "Weight decay (L2):", value = 0.001, min = 0, step = 0.0001),
            uiOutput("dl_dropout_ui"),
            checkboxInput("dl_scale_target", "Scale numeric target (regression)", value = TRUE),
            checkboxInput("dl_auto_arch", "Auto adjust hidden layer sizes", value = FALSE),
            checkboxInput("dl_auto_tune", "Auto tune parameters", value = FALSE),
            conditionalPanel(
              condition = "input.dl_auto_tune == true",
              numericInput("dl_tune_trials", "Auto tune trials:", value = 20, min = 2, max = 100, step = 1),
              sliderInput("dl_validation_split", "Validation split inside training (%):", min = 10, max = 40, value = 20, step = 5)
            ),
            actionButton("dl_run_training", "Train Deep Learning model")
          ),
          column(
            8,
            tags$div(class = "dl-status-box", verbatimTextOutput("dl_training_log")),
            uiOutput("dl_loss_panel"),
            uiOutput("dl_metric_panel"),
            uiOutput("dl_tuning_tips"),
            tags$div(
              class = "dl-panel",
              tags$h4("Network view"),
              tags$div(class = "dl-model-shape", textOutput("dl_model_shape")),
              plotlyOutput("dl_path_plot", height = "420px"),
              tags$div(class = "dl-path-table-wrap", DT::DTOutput("dl_path_table")),
              plotOutput("dl_weight_plot", height = "250px")
            ),
            DT::DTOutput("dl_metrics_table"),
            DT::DTOutput("dl_tune_table"),
            DT::DTOutput("dl_predictions_table")
          )
        )
      )
    ),
    tabPanel("GRAPH MODELS",
      fluidPage(
        tags$h4("Graph Models"),
        fluidRow(
          column(
            4,
            selectInput("gm_target", "Target column (optional):", choices = NULL, selected = NULL),
            sliderInput("gm_max_nodes", "Max nodes (top variable columns):", min = 10, max = 200, value = 60, step = 5),
            sliderInput("gm_corr_threshold", "Edge threshold |correlation|:", min = 0.2, max = 0.95, value = 0.6, step = 0.05),
            numericInput("gm_min_degree", "Minimum degree to keep node:", value = 1, min = 0, step = 1),
            selectInput("gm_layout", "Layout:", choices = c("MDS (correlation distance)" = "mds", "Circular" = "circular"), selected = "mds"),
            checkboxInput("gm_use_3d", "Render in 3D (plotly)", value = TRUE),
            actionButton("gm_build_graph", "Build graph"),
            tags$hr(),
            downloadButton("gm_download_nodes", "Download node metrics (CSV)"),
            downloadButton("gm_download_edges", "Download edges (CSV)")
          ),
          column(
            8,
            uiOutput("gm_summary"),
            plotlyOutput("gm_network_plot_3d", height = "560px"),
            plotOutput("gm_network_plot", height = "520px"),
            plotOutput("gm_degree_plot", height = "240px"),
            DT::DTOutput("gm_nodes_table"),
            DT::DTOutput("gm_edges_table")
          )
        )
      )
    ),
    tabPanel("CONFIGURATIONS",
      fluidPage(
        tags$h4("Resource limits"),
        fluidRow(
          column(
            6,
            sliderInput(
              "config_cpu_count",
              paste0("CPUs to use. Available: ", total_system_cpus),
              min = 1,
              max = total_system_cpus,
              value = default_system_cpu_limit,
              step = 1
            ),
            textOutput("config_cpu_summary")
          )
        ),
        tags$hr(),
        tags$h4("Remote servers"),
        tags$div(
          class = "remote-server-toolbar",
          actionButton("config_remote_add", "Add server", icon = icon("plus"))
        ),
        DT::DTOutput("config_remote_servers_table"),
        tags$hr(),
        checkboxInput(
          "config_parallel_cubist_models",
          "Use parallel processing",
          value = TRUE
        ),
        checkboxInput(
          "config_restart_parallel_each_model",
          "Restart parallel workers for each model",
          value = TRUE
        ),
        checkboxInput(
          "config_retry_parallel_connection_errors",
          "Retry once if parallel workers fail",
          value = TRUE
        ),
        tags$p(
          "Use these controls to limit how much CPU ugPlot can use during Machine Learning. ",
          "The default value does not use the whole computer, so the operating system stays responsive."
        ),
        tags$hr(),
        tags$h4("GEO storage"),
        tags$p(
          "Local GEO downloads, sesame reprocessing outputs, and per-source analysis caches are kept separately."
        ),
        tags$div(
          class = "remote-server-toolbar",
          actionButton("config_geo_storage_refresh", "Refresh GEO storage", icon = icon("refresh"))
        ),
        uiOutput("config_geo_storage_summary"),
        DT::DTOutput("config_geo_storage_table")
      )
    )
  )
)

# --- Helper functions (defined globally) ---

load_ml_list <- function(model_deps = NULL) {
  if (is.null(model_deps)) {
    model_deps <- ugplot_model_dependency_status()
  }
  ml_available <<- setdiff(model_deps$models_installed, slow_models)
  ml_not_available <<- model_deps$models_missing
  removeUI(selector = "#ml_checkbox_group")
  insertUI(
    selector = "#dynamic_machine_learning",
    where = "afterEnd",
    ui = checkboxGroupInput(inputId = "ml_checkbox_group", label = NULL, choices = ml_available, selected = ml_available)
  )
  removeUI(selector = "#ml_missing_checkbox_group")
  insertUI(
    selector = "#dynamic_machine_learning_missing",
    where = "afterEnd",
    ui = checkboxGroupInput(inputId = "ml_missing_checkbox_group", label = NULL, choices = ml_not_available)
  )
}

load_file_into_table <- function(textarea_columns, textarea_rows, localsession) {
  column_names <- strsplit(textarea_columns, "\n")[[1]]
  rown_names <- strsplit(textarea_rows, "\n")[[1]]
  dff <<- df_pre[rown_names, column_names, drop = FALSE]
  empty_columns <- sapply(dff, function(column) all(is.na(column)))
  removed_columns <- names(dff)[empty_columns]
  if (any(empty_columns)) {
    dff <<- dff[, !empty_columns, drop = FALSE]
    table_cleaning_message_text(paste("Those columns have been removed because they are empty: ", paste(removed_columns, collapse = ", ")))
  } else {
    table_cleaning_message_text("")
  }
  changed_table <<- dff
  load_checkbox_group()
  updateTabsetPanel(localsession, "tabs", selected = "TABLE")
  enable("merge_all_columns")
  enable("merge_all_rows")
  showTab(inputId = "tabs", target = "TABLE")
  showTab(inputId = "tabs", target = "HEATMAP PLOT")
  showTab(inputId = "tabs", target = "2D PLOT")
  showTab(inputId = "tabs", target = "MACHINE LEARNING")
  showTab(inputId = "tabs", target = "MODEL ANALYSIS")
  showTab(inputId = "tabs", target = "DEEP LEARNING")
  showTab(inputId = "tabs", target = "GRAPH MODELS")
  showTab(inputId = "tabs", target = "JOBS")
  showTab(inputId = "tabs", target = "CONFIGURATIONS")
}

build_missing_mask <- function(df, missing_definition = c("empty", "na"), zero_exceptions = character(0)) {
  mask <- matrix(FALSE, nrow = nrow(df), ncol = ncol(df))
  colnames(mask) <- colnames(df)
  rownames(mask) <- rownames(df)
  for (j in seq_along(df)) {
    col_data <- df[[j]]
    missing_col <- rep(FALSE, length(col_data))
    normalized_text <- trimws(as.character(col_data))
    if ("na" %in% missing_definition) {
      missing_col <- missing_col | is.na(col_data) | (!is.na(col_data) & toupper(normalized_text) == "NA")
    }
    if ("empty" %in% missing_definition) {
      missing_col <- missing_col | (!is.na(col_data) & normalized_text == "")
    }
    if ("zero" %in% missing_definition && !(colnames(df)[j] %in% zero_exceptions)) {
      suppressWarnings({
        numeric_col <- as.numeric(as.character(col_data))
      })
      missing_col <- missing_col | (!is.na(numeric_col) & numeric_col == 0)
    }
    mask[, j] <- missing_col
  }
  mask
}

missing_definition_counts <- function(df, zero_exceptions = character(0)) {
  if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(data.frame(
      Rule = c("Empty string", "NA / NA-like text", "Zero"),
      Cells = 0L,
      Columns = 0L,
      stringsAsFactors = FALSE
    ))
  }

  count_rule <- function(rule) {
    mask <- build_missing_mask(df, missing_definition = rule, zero_exceptions = zero_exceptions)
    data.frame(
      Cells = as.integer(sum(mask)),
      Columns = as.integer(sum(colSums(mask) > 0)),
      stringsAsFactors = FALSE
    )
  }

  counts <- rbind(
    cbind(Rule = "Empty string", count_rule("empty")),
    cbind(Rule = "NA / NA-like text", count_rule("na")),
    cbind(Rule = "Zero", count_rule("zero"))
  )
  rownames(counts) <- NULL
  counts
}

apply_missing_filters_with_order <- function(predictors, missing_definition,
                                             zero_exceptions = character(0),
                                             threshold_cols = 100, threshold_rows = 100,
                                             order = c("cols_first", "rows_first")) {
  order <- match.arg(order)
  original_cols <- colnames(predictors)
  original_rows <- seq_len(nrow(predictors))
  filtered_predictors <- predictors
  filtered_mask <- build_missing_mask(filtered_predictors, missing_definition, zero_exceptions)
  keep_cols <- colnames(filtered_predictors)
  keep_rows <- seq_len(nrow(filtered_predictors))

  if (order == "cols_first") {
    if (ncol(filtered_predictors) > 0) {
      col_missing_pct <- colMeans(filtered_mask) * 100
      col_missing_pct[!is.finite(col_missing_pct)] <- 100
      keep_cols <- names(col_missing_pct[col_missing_pct <= threshold_cols])
      filtered_predictors <- filtered_predictors[, keep_cols, drop = FALSE]
      filtered_mask <- build_missing_mask(filtered_predictors, missing_definition, zero_exceptions)
    }

    if (ncol(filtered_predictors) > 0) {
      row_missing_pct <- rowMeans(filtered_mask) * 100
      keep_rows <- which(row_missing_pct <= threshold_rows)
      filtered_predictors <- filtered_predictors[keep_rows, , drop = FALSE]
      filtered_mask <- filtered_mask[keep_rows, , drop = FALSE]
    }
  } else {
    if (ncol(filtered_predictors) > 0) {
      row_missing_pct <- rowMeans(filtered_mask) * 100
      keep_rows <- which(row_missing_pct <= threshold_rows)
      filtered_predictors <- filtered_predictors[keep_rows, , drop = FALSE]
      filtered_mask <- filtered_mask[keep_rows, , drop = FALSE]
    }
    if (ncol(filtered_predictors) > 0) {
      col_missing_pct <- colMeans(filtered_mask) * 100
      col_missing_pct[!is.finite(col_missing_pct)] <- 100
      keep_cols <- names(col_missing_pct[col_missing_pct <= threshold_cols])
      filtered_predictors <- filtered_predictors[, keep_cols, drop = FALSE]
      filtered_mask <- build_missing_mask(filtered_predictors, missing_definition, zero_exceptions)
    }
  }

  list(
    filtered_predictors = filtered_predictors,
    filtered_mask = filtered_mask,
    keep_cols = keep_cols,
    keep_rows = keep_rows,
    removed_cols = setdiff(original_cols, keep_cols),
    removed_rows = setdiff(original_rows, keep_rows)
  )
}

apply_missing_filters <- function(predictors, missing_definition,
                                  zero_exceptions = character(0),
                                  threshold_cols = 100, threshold_rows = 100) {
  apply_missing_filters_with_order(
    predictors = predictors,
    missing_definition = missing_definition,
    zero_exceptions = zero_exceptions,
    threshold_cols = threshold_cols,
    threshold_rows = threshold_rows,
    order = "cols_first"
  )
}

normalize_missing_filter_order <- function(filter_order, allow_auto = TRUE) {
  filter_order <- as.character(filter_order)
  valid_orders <- if (isTRUE(allow_auto)) {
    c("auto", "cols_first", "rows_first")
  } else {
    c("cols_first", "rows_first")
  }
  if (length(filter_order) == 0 || !filter_order[[1]] %in% valid_orders) {
    return(if (isTRUE(allow_auto)) "auto" else "cols_first")
  }
  filter_order[[1]]
}

rank_threshold_scan_results <- function(results, min_rows_retained = 0.8,
                                        mode = c("complete_case", "balanced")) {
  mode <- match.arg(mode)
  if (is.null(results) || nrow(results) == 0) {
    return(results)
  }

  min_rows_retained <- suppressWarnings(as.numeric(min_rows_retained))
  if (!is.finite(min_rows_retained)) {
    min_rows_retained <- 0
  }
  min_rows_retained <- max(0, min(1, min_rows_retained))

  results$complete_case <- results$missing_cells_after == 0 &
    results$n_cols_after > 0 & results$n_rows_after > 0
  results$meets_min_samples <- results$rows_retained >= min_rows_retained

  if (identical(mode, "complete_case")) {
    recommendation_group <- ifelse(results$complete_case & results$meets_min_samples, 0,
      ifelse(results$complete_case, 1, 2))
    primary <- ifelse(recommendation_group == 0, -results$n_cols_after,
      ifelse(recommendation_group == 1, -results$n_rows_after, -results$tradeoff_score))
    secondary <- ifelse(recommendation_group == 0, -results$n_rows_after,
      ifelse(recommendation_group == 1, -results$n_cols_after, results$missing_pct_after))
    tertiary <- ifelse(recommendation_group == 2, -results$filled_cells, results$thr_row)
    results$recommendation_group <- recommendation_group
    return(results[order(
      recommendation_group, primary, secondary,
      -results$cross_point, -results$pareto, tertiary, results$thr_col
    ), , drop = FALSE])
  }

  results$recommendation_group <- 2
  results[order(-results$cross_point, -results$pareto, -results$tradeoff_score,
    results$missing_pct_after, -results$filled_cells), , drop = FALSE]
}

missing_filter_metrics <- function(filtered, original_rows, original_cols,
                                   thr_col, thr_row, scan_order) {
  filtered_mask <- filtered$filtered_mask
  n_cols_after <- ncol(filtered_mask)
  n_rows_after <- nrow(filtered_mask)
  missing_after <- if (length(filtered_mask) > 0) sum(filtered_mask) else 0
  total_after <- n_cols_after * n_rows_after
  missing_pct_after <- if (total_after > 0) (100 * missing_after / total_after) else 0
  filled_cells <- total_after - missing_after
  rows_retained <- if (original_rows > 0) n_rows_after / original_rows else 0
  cols_retained <- if (original_cols > 0) n_cols_after / original_cols else 0
  data.frame(
    thr_col = thr_col, thr_row = thr_row, scan_order = scan_order,
    n_cols_after = n_cols_after, n_rows_after = n_rows_after,
    total_cells_after = total_after, missing_cells_after = missing_after,
    filled_cells = filled_cells, missing_pct_after = round(missing_pct_after, 2),
    rows_retained = rows_retained, cols_retained = cols_retained,
    tradeoff_score = ((rows_retained + cols_retained) / 2) - (missing_pct_after / 100),
    cross_point = FALSE, pareto = TRUE
  )
}

apply_missing_filters_resolved <- function(predictors, missing_definition,
                                           zero_exceptions = character(0),
                                           threshold_cols = 100, threshold_rows = 100,
                                           filter_order = "auto",
                                           min_rows_retained = 0.8,
                                           mode = c("complete_case", "balanced")) {
  mode <- match.arg(mode)
  filter_order <- normalize_missing_filter_order(filter_order, allow_auto = TRUE)
  if (!identical(filter_order, "auto")) {
    filtered <- apply_missing_filters_with_order(
      predictors = predictors,
      missing_definition = missing_definition,
      zero_exceptions = zero_exceptions,
      threshold_cols = threshold_cols,
      threshold_rows = threshold_rows,
      order = filter_order
    )
    filtered$resolved_order <- filter_order
    return(filtered)
  }

  candidates <- lapply(c("cols_first", "rows_first"), function(order) {
    filtered <- apply_missing_filters_with_order(
      predictors = predictors,
      missing_definition = missing_definition,
      zero_exceptions = zero_exceptions,
      threshold_cols = threshold_cols,
      threshold_rows = threshold_rows,
      order = order
    )
    metrics <- missing_filter_metrics(
      filtered, nrow(predictors), ncol(predictors),
      threshold_cols, threshold_rows, order
    )
    filtered$resolved_order <- order
    list(filtered = filtered, metrics = metrics)
  })
  metrics <- do.call(rbind, lapply(candidates, `[[`, "metrics"))
  cross_key <- paste(metrics$n_cols_after, metrics$n_rows_after,
    metrics$missing_pct_after, sep = "|")
  metrics$cross_point <- as.logical(table(cross_key)[cross_key] >= 2)
  ranked <- rank_threshold_scan_results(metrics, min_rows_retained = min_rows_retained, mode = mode)
  best_order <- ranked$scan_order[[1]]
  candidates[[match(best_order, c("cols_first", "rows_first"))]]$filtered
}

compute_exhaustive_threshold_scan <- function(predictors, missing_definition,
                                              zero_exceptions = character(0),
                                              min_rows_retained = 0.8,
                                              mode = c("complete_case", "balanced"),
                                              progress_callback = NULL, status_callback = NULL) {
  mode <- match.arg(mode)
  original_rows <- nrow(predictors)
  original_cols <- ncol(predictors)
  full_mask <- build_missing_mask(predictors, missing_definition, zero_exceptions)

  scan_one_order <- function(scan_order = c("cols_first", "rows_first"), phase_start = 0, phase_width = 0.5) {
    scan_order <- match.arg(scan_order)
    metrics_list <- list()
    idx <- 0

    if (scan_order == "cols_first") {
      col_missing_pct <- if (ncol(full_mask) > 0) colMeans(full_mask) * 100 else numeric(0)
      outer_thresholds <- sort(unique(pmin(100, pmax(0, ceiling(c(0, 100, col_missing_pct))))))
      if (length(outer_thresholds) == 0) outer_thresholds <- c(0, 100)
      for (thr_col in outer_thresholds) {
        if (ncol(full_mask) > 0) {
          keep_cols <- names(col_missing_pct[col_missing_pct <= thr_col])
          filtered_mask_outer <- full_mask[, keep_cols, drop = FALSE]
        } else {
          filtered_mask_outer <- full_mask
        }
        if (ncol(filtered_mask_outer) > 0) {
          row_missing_pct <- rowMeans(filtered_mask_outer) * 100
          inner_thresholds <- sort(unique(pmin(100, pmax(0, ceiling(c(0, 100, row_missing_pct))))))
        } else {
          row_missing_pct <- numeric(0)
          inner_thresholds <- c(0, 100)
        }
        for (thr_row in inner_thresholds) {
          idx <- idx + 1
          filtered <- apply_missing_filters_with_order(
            predictors = predictors,
            missing_definition = missing_definition,
            zero_exceptions = zero_exceptions,
            threshold_cols = thr_col,
            threshold_rows = thr_row,
            order = "cols_first"
          )
          filtered_mask <- filtered$filtered_mask
          n_cols_after <- ncol(filtered_mask)
          n_rows_after <- nrow(filtered_mask)
          missing_after <- if (length(filtered_mask) > 0) sum(filtered_mask) else 0
          total_after <- n_cols_after * n_rows_after
          missing_pct_after <- if (total_after > 0) (100 * missing_after / total_after) else 0
          filled_cells <- total_after - missing_after
          rows_retained <- if (original_rows > 0) n_rows_after / original_rows else 0
          cols_retained <- if (original_cols > 0) n_cols_after / original_cols else 0
          tradeoff_score <- ((rows_retained + cols_retained) / 2) - (missing_pct_after / 100)
          metrics_list[[idx]] <- data.frame(
            thr_col = thr_col, thr_row = thr_row, scan_order = "cols_first",
            n_cols_after = n_cols_after, n_rows_after = n_rows_after,
            total_cells_after = total_after, missing_cells_after = missing_after,
            filled_cells = filled_cells, missing_pct_after = round(missing_pct_after, 2),
            rows_retained = rows_retained, cols_retained = cols_retained, tradeoff_score = tradeoff_score
          )
        }
        if (!is.null(progress_callback)) {
          local_progress <- which(outer_thresholds == thr_col)[1] / length(outer_thresholds)
          progress_callback(phase_start + phase_width * local_progress)
        }
        if (!is.null(status_callback)) {
          status_callback(sprintf("Scanning (cols->rows)... column threshold %d%%", thr_col))
        }
      }
    } else {
      row_missing_pct <- if (ncol(full_mask) > 0) rowMeans(full_mask) * 100 else numeric(0)
      outer_thresholds <- sort(unique(pmin(100, pmax(0, ceiling(c(0, 100, row_missing_pct))))))
      if (length(outer_thresholds) == 0) outer_thresholds <- c(0, 100)
      for (thr_row in outer_thresholds) {
        if (ncol(full_mask) > 0) {
          keep_rows <- which(row_missing_pct <= thr_row)
          filtered_mask_outer <- full_mask[keep_rows, , drop = FALSE]
        } else {
          filtered_mask_outer <- full_mask
        }
        if (ncol(filtered_mask_outer) > 0) {
          col_missing_pct <- colMeans(filtered_mask_outer) * 100
          col_missing_pct <- col_missing_pct[is.finite(col_missing_pct)]
          inner_thresholds <- sort(unique(pmin(100, pmax(0, ceiling(c(0, 100, col_missing_pct))))))
        } else {
          inner_thresholds <- c(0, 100)
        }
        for (thr_col in inner_thresholds) {
          idx <- idx + 1
          filtered <- apply_missing_filters_with_order(
            predictors = predictors,
            missing_definition = missing_definition,
            zero_exceptions = zero_exceptions,
            threshold_cols = thr_col,
            threshold_rows = thr_row,
            order = "rows_first"
          )
          filtered_mask <- filtered$filtered_mask
          n_cols_after <- ncol(filtered_mask)
          n_rows_after <- nrow(filtered_mask)
          missing_after <- if (length(filtered_mask) > 0) sum(filtered_mask) else 0
          total_after <- n_cols_after * n_rows_after
          missing_pct_after <- if (total_after > 0) (100 * missing_after / total_after) else 0
          filled_cells <- total_after - missing_after
          rows_retained <- if (original_rows > 0) n_rows_after / original_rows else 0
          cols_retained <- if (original_cols > 0) n_cols_after / original_cols else 0
          tradeoff_score <- ((rows_retained + cols_retained) / 2) - (missing_pct_after / 100)
          metrics_list[[idx]] <- data.frame(
            thr_col = thr_col, thr_row = thr_row, scan_order = "rows_first",
            n_cols_after = n_cols_after, n_rows_after = n_rows_after,
            total_cells_after = total_after, missing_cells_after = missing_after,
            filled_cells = filled_cells, missing_pct_after = round(missing_pct_after, 2),
            rows_retained = rows_retained, cols_retained = cols_retained, tradeoff_score = tradeoff_score
          )
        }
        if (!is.null(progress_callback)) {
          local_progress <- which(outer_thresholds == thr_row)[1] / length(outer_thresholds)
          progress_callback(phase_start + phase_width * local_progress)
        }
        if (!is.null(status_callback)) {
          status_callback(sprintf("Scanning (rows->cols)... row threshold %d%%", thr_row))
        }
      }
    }
    if (length(metrics_list) == 0) {
      return(data.frame())
    }
    do.call(rbind, metrics_list)
  }

  results_cols_first <- scan_one_order("cols_first", phase_start = 0, phase_width = 0.5)
  results_rows_first <- scan_one_order("rows_first", phase_start = 0.5, phase_width = 0.5)
  results <- rbind(results_cols_first, results_rows_first)
  if (nrow(results) == 0) {
    return(results)
  }

  cross_key <- paste(results$thr_col, results$thr_row, results$n_cols_after, results$n_rows_after,
    results$missing_pct_after, sep = "|")
  cross_counts <- table(cross_key)
  results$cross_point <- as.logical(cross_counts[cross_key] >= 2)

  dominated <- rep(FALSE, nrow(results))
  for (i in seq_len(nrow(results))) {
    candidate <- results[i, ]
    better_or_equal <- (results$n_rows_after >= candidate$n_rows_after) &
      (results$n_cols_after >= candidate$n_cols_after) &
      (results$missing_pct_after <= candidate$missing_pct_after)
    strictly_better <- (results$n_rows_after > candidate$n_rows_after) |
      (results$n_cols_after > candidate$n_cols_after) |
      (results$missing_pct_after < candidate$missing_pct_after)
    dominated[i] <- any(better_or_equal & strictly_better)
  }
  results$pareto <- !dominated
  rank_threshold_scan_results(results, min_rows_retained = min_rows_retained, mode = mode)
}

run_methylimp2 <- function(data_with_na) {
  if (!requireNamespace("methyLImp2", quietly = TRUE)) {
    stop("The 'methyLImp2' package is not installed. Install it with BiocManager::install('methyLImp2').")
  }
  methyl_matrix <- as.matrix(data_with_na)
  suppressWarnings(storage.mode(methyl_matrix) <- "numeric")
  if (any(!is.finite(methyl_matrix) & !is.na(methyl_matrix))) {
    stop("Invalid non-finite values detected while preparing data for methyLImp2.")
  }
  imputed_matrix <- methyLImp2::methyLImp2(methyl_matrix)
  as.data.frame(imputed_matrix, stringsAsFactors = FALSE)
}

apply_saved_preprocess <- function(df, preprocess_meta) {
  if (is.null(preprocess_meta) || is.null(preprocess_meta$strategy)) {
    return(df)
  }

  if (identical(preprocess_meta$strategy, "knn") && !is.null(preprocess_meta$pp)) {
    num_cols <- preprocess_meta$num_cols
    num_cols <- intersect(num_cols, colnames(df))
    if (length(num_cols) > 0) {
      knn_data <- df[, num_cols, drop = FALSE]
      suppressWarnings(storage.mode(knn_data) <- "numeric")
      knn_data <- predict(preprocess_meta$pp, knn_data)
      df[, num_cols] <- knn_data[, num_cols, drop = FALSE]
    }
  }

  df
}

apply_missing_strategy <- function(trainSet, testSet, target_name, strategy, missing_definition,
                                   zero_exceptions = character(0),
                                   threshold_cols = 50, threshold_rows = 50,
                                   threshold_scope = "train_only",
                                   filter_order = "auto",
                                   min_rows_retained = 0.8) {
  filter_order <- normalize_missing_filter_order(filter_order, allow_auto = TRUE)
  train_set <- as.data.frame(trainSet)
  test_set <- as.data.frame(testSet)

  predictors_train <- train_set[, setdiff(colnames(train_set), target_name), drop = FALSE]
  predictors_test <- test_set[, setdiff(colnames(test_set), target_name), drop = FALSE]
  preprocess_meta <- list(strategy = strategy)

  train_missing <- build_missing_mask(predictors_train, missing_definition, zero_exceptions)
  test_missing <- build_missing_mask(predictors_test, missing_definition, zero_exceptions)

  if (identical(threshold_scope, "full_before_split")) {
    filtered_train <- list(
      filtered_predictors = predictors_train,
      filtered_mask = train_missing,
      keep_cols = colnames(predictors_train),
      keep_rows = seq_len(nrow(predictors_train))
    )
  } else {
    filtered_train <- apply_missing_filters_resolved(
      predictors = predictors_train,
      missing_definition = missing_definition,
      zero_exceptions = zero_exceptions,
      threshold_cols = threshold_cols,
      threshold_rows = threshold_rows,
      filter_order = filter_order,
      min_rows_retained = min_rows_retained,
      mode = if (identical(strategy, "none")) "complete_case" else "balanced"
    )
  }
  predictors_train <- filtered_train$filtered_predictors
  train_missing <- filtered_train$filtered_mask

  if (ncol(predictors_test) > 0) {
    predictors_test <- predictors_test[, filtered_train$keep_cols, drop = FALSE]
    test_missing <- build_missing_mask(predictors_test, missing_definition, zero_exceptions)
  }

  if (ncol(predictors_train) > 0) {
    keep_rows <- filtered_train$keep_rows
    train_set <- train_set[keep_rows, , drop = FALSE]
  }

  if (identical(threshold_scope, "train_and_test_self_rows") && ncol(predictors_test) > 0) {
    test_row_missing_pct <- rowMeans(test_missing) * 100
    keep_test_rows <- which(test_row_missing_pct <= threshold_rows)
    predictors_test <- predictors_test[keep_test_rows, , drop = FALSE]
    test_set <- test_set[keep_test_rows, , drop = FALSE]
    test_missing <- build_missing_mask(predictors_test, missing_definition, zero_exceptions)
  }

  if (strategy == "replace_zero") {
    predictors_train[train_missing] <- 0
    predictors_test[test_missing] <- 0
  }

  if (strategy == "mean") {
    num_cols <- names(predictors_train)[sapply(predictors_train, is.numeric)]
    for (col_name in num_cols) {
      mean_value <- mean(predictors_train[[col_name]][!train_missing[, col_name]], na.rm = TRUE)
      if (is.nan(mean_value)) mean_value <- 0
      col_missing_train <- train_missing[, col_name]
      col_missing_test <- test_missing[, col_name]
      predictors_train[[col_name]][col_missing_train] <- mean_value
      predictors_test[[col_name]][col_missing_test] <- mean_value
    }
  }

  if (strategy == "knn") {
    num_cols <- names(predictors_train)[sapply(predictors_train, is.numeric)]
    if (length(num_cols) > 0) {
      knn_train <- predictors_train[, num_cols, drop = FALSE]
      knn_test <- predictors_test[, num_cols, drop = FALSE]
      knn_train[train_missing[, num_cols, drop = FALSE]] <- NA
      knn_test[test_missing[, num_cols, drop = FALSE]] <- NA
      pp <- caret::preProcess(knn_train, method = "knnImpute")
      knn_train_imputed <- predict(pp, knn_train)
      knn_test_imputed <- predict(pp, knn_test)
      preprocess_meta <- list(strategy = "knn", pp = pp, num_cols = num_cols)
      predictors_train[, num_cols] <- knn_train_imputed[, num_cols, drop = FALSE]
      predictors_test[, num_cols] <- knn_test_imputed[, num_cols, drop = FALSE]
    }
  }

  if (strategy == "missforest") {
    if (!requireNamespace("missForest", quietly = TRUE)) {
      stop("The 'missForest' package is not installed. Install it with install.packages('missForest').")
    }
    train_for_impute <- predictors_train
    test_for_impute <- predictors_test
    train_for_impute[train_missing] <- NA
    test_for_impute[test_missing] <- NA

    predictors_train <- missForest::missForest(train_for_impute, verbose = FALSE)$ximp
    predictors_train <- as.data.frame(predictors_train, stringsAsFactors = FALSE)
    if (nrow(test_for_impute) > 0) {
      predictors_test <- missForest::missForest(test_for_impute, verbose = FALSE)$ximp
      predictors_test <- as.data.frame(predictors_test, stringsAsFactors = FALSE)
    } else {
      predictors_test <- predictors_test[0, , drop = FALSE]
    }
  }

  if (strategy == "methylimp2") {
    train_for_impute <- predictors_train
    test_for_impute <- predictors_test
    train_for_impute[train_missing] <- NA
    test_for_impute[test_missing] <- NA

    predictors_train <- run_methylimp2(train_for_impute)
    if (nrow(test_for_impute) > 0) {
      predictors_test <- run_methylimp2(test_for_impute)
    } else {
      predictors_test <- predictors_test[0, , drop = FALSE]
    }
  }

  if (strategy == "none") {
    predictors_train[train_missing] <- NA
    predictors_test[test_missing] <- NA
  }

  train_set <- cbind(train_set[, target_name, drop = FALSE], predictors_train)
  test_set <- cbind(test_set[, target_name, drop = FALSE], predictors_test)
  names(train_set)[1] <- target_name
  names(test_set)[1] <- target_name

  list(train_set = train_set, test_set = test_set, preprocess_meta = preprocess_meta)
}

load_dataset_into_table <- function(localsession) {
  if (exists("dff") && is.data.frame(dff) && nrow(dff) > 0) {
    changed_table <<- dff
    load_checkbox_group()
    updateTabsetPanel(localsession, "tabs", selected = "TABLE")
    enable("merge_all_columns")
    enable("merge_all_rows")
    showTab(inputId = "tabs", target = "TABLE")
    showTab(inputId = "tabs", target = "HEATMAP PLOT")
    showTab(inputId = "tabs", target = "2D PLOT")
    showTab(inputId = "tabs", target = "MACHINE LEARNING")
    showTab(inputId = "tabs", target = "MODEL ANALYSIS")
    showTab(inputId = "tabs", target = "DEEP LEARNING")
    showTab(inputId = "tabs", target = "GRAPH MODELS")
    showTab(inputId = "tabs", target = "JOBS")
    showTab(inputId = "tabs", target = "CONFIGURATIONS")
  }
}

generate_annotation_colors <- function(annotation_df) {
  color_list <- list()
  for (colname in names(annotation_df)) {
    unique_vals <- unique(annotation_df[[colname]])
    colors <- rainbow(length(unique_vals))
    color_list[[colname]] <- setNames(colors, unique_vals)
  }
  return(color_list)
}

load_checkbox_group <- function() {
  removeUI(selector = "#column_checkbox_group")
  removeUI(selector = "#row_checkbox_group")
  removeUI(selector = "#checkbox_group_categories")
  insertUI(
    selector = "#dynamic_columns",
    where = "afterEnd",
    ui = checkboxGroupInput(inputId = "column_checkbox_group", label = NULL, choices = names(dff), selected = names(dff))
  )
  insertUI(
    selector = "#dynamic_rows",
    where = "afterEnd",
    ui = checkboxGroupInput(inputId = "row_checkbox_group", label = NULL, choices = rownames(dff), selected = rownames(dff))
  )
  insertUI(
    selector = "#dynamic_columns_categories",
    where = "afterEnd",
    ui = checkboxGroupInput(inputId = "checkbox_group_categories", label = NULL, choices = names(dff))
  )
}

ugPlot <- function(dataset = data.frame()) {
  if (nrow(dataset) > 0) {
    dff <<- dataset
  }
  shinyApp(ui = ui, server = server)
}

# --- End of helper functions ---

# Define the server function
server <- function(input, output, session) {
  # Define reactive to store the loaded model
  loaded_model <- reactiveVal(NULL)
  remote_servers <- reactiveVal(ugplot_read_remote_servers())
  config_remote_editing_name <- reactiveVal(NULL)
  config_remote_test_status <- reactiveVal("")
  config_remote_detected_cpus <- reactiveVal(NA_integer_)
  config_geo_storage_refresh <- reactiveVal(0L)
  config_geo_delete_request <- reactiveVal(NULL)
  ml_model_source_status_text <- reactiveVal("")

  hideTab(inputId = "tabs", target = "TABLE")
  hideTab(inputId = "tabs", target = "HEATMAP PLOT")
  hideTab(inputId = "tabs", target = "2D PLOT")
  hideTab(inputId = "tabs", target = "MACHINE LEARNING")
  hideTab(inputId = "tabs", target = "MODEL ANALYSIS")
  hideTab(inputId = "tabs", target = "DEEP LEARNING")
  hideTab(inputId = "tabs", target = "GRAPH MODELS")

  disable("merge_all_columns")
  disable("merge_all_rows")
  disable("process_table_content")
  session$allowReconnect(TRUE)
  ugplot_collaboration_tab_server("collaboration", remote_servers, total_system_cpus)

  configured_cpu_limit <- reactive({
    cpu_limit <- suppressWarnings(as.integer(input$config_cpu_count %||% default_system_cpu_limit))
    if (is.na(cpu_limit)) {
      cpu_limit <- default_system_cpu_limit
    }
    max(1L, min(total_system_cpus, cpu_limit))
  })

  observe({
    apply_runtime_thread_limit(configured_cpu_limit())
  })

  observe({
    has_pending_table <- nzchar(trimws(input$textarea_columns %||% "")) &&
      nzchar(trimws(input$textarea_rows %||% ""))
    if (has_pending_table) {
      enable("process_table_content")
    } else {
      disable("process_table_content")
    }
  })

  output$config_cpu_summary <- renderText({
    paste0(
      "Parallel jobs will use up to ", configured_cpu_limit(), " of ",
      total_system_cpus, " CPU threads."
    )
  })

  output$config_remote_test_status <- renderUI({
    status_text <- config_remote_test_status()
    if (!nzchar(status_text)) {
      return(NULL)
    }
    if (grepl("Version mismatch", status_text, fixed = TRUE)) {
      return(tags$span(style = "color: #8a5a00; font-weight: 600;", htmltools::htmlEscape(status_text)))
    }
    tags$span(htmltools::htmlEscape(status_text))
  })

  remote_server_choices <- function() {
    servers <- remote_servers()
    if (!is.data.frame(servers) || nrow(servers) == 0) {
      servers <- ugplot_default_remote_servers()
    }
    stats::setNames(servers$name, servers$name)
  }

  selected_remote_server <- function() {
    servers <- remote_servers()
    if (!is.data.frame(servers) || nrow(servers) == 0) {
      servers <- ugplot_default_remote_servers()
    }
    selected_name <- input$remote_server_name %||% servers$name[[1]]
    server <- servers[servers$name == selected_name, , drop = FALSE]
    if (nrow(server) == 0) {
      server <- servers[1, , drop = FALSE]
    }
    server
  }

  selected_geo_remote_server <- function() {
    servers <- remote_servers()
    if (!is.data.frame(servers) || nrow(servers) == 0) {
      servers <- ugplot_default_remote_servers()
    }
    selected_name <- input$geo_remote_server_name %||% input$remote_server_name %||% servers$name[[1]]
    server <- servers[servers$name == selected_name, , drop = FALSE]
    if (nrow(server) == 0) {
      server <- servers[1, , drop = FALSE]
    }
    server
  }

  selected_remote_cpu_limit <- function(server = selected_remote_server()) {
    cpu_limit <- suppressWarnings(as.integer(server$cpu_limit[[1]] %||% configured_cpu_limit()))
    if (is.na(cpu_limit) || cpu_limit < 1L) {
      cpu_limit <- configured_cpu_limit()
    }
    cpu_limit
  }

  refresh_remote_server_inputs <- function(selected = NULL) {
    servers <- remote_servers()
    if (!is.data.frame(servers) || nrow(servers) == 0) {
      servers <- ugplot_default_remote_servers()
      remote_servers(servers)
    }
    choices <- stats::setNames(servers$name, servers$name)
    selected <- selected %||% isolate(input$remote_server_name) %||% servers$name[[1]]
    if (!(selected %in% servers$name)) {
      selected <- servers$name[[1]]
    }
    updateSelectInput(session, "remote_server_name", choices = choices, selected = selected)
  }

  refresh_geo_remote_server_inputs <- function(selected = NULL) {
    servers <- remote_servers()
    if (!is.data.frame(servers) || nrow(servers) == 0) {
      servers <- ugplot_default_remote_servers()
      remote_servers(servers)
    }
    choices <- stats::setNames(servers$name, servers$name)
    selected <- selected %||% isolate(input$geo_remote_server_name) %||% isolate(input$remote_server_name) %||% servers$name[[1]]
    if (!(selected %in% servers$name)) {
      selected <- servers$name[[1]]
    }
    updateSelectInput(session, "geo_remote_server_name", choices = choices, selected = selected)
  }

  observe({
    refresh_remote_server_inputs()
    refresh_geo_remote_server_inputs()
  })

  load_selected_ml_list <- function() {
    if (identical(input$ml_run_target %||% "local", "remote")) {
      server <- selected_remote_server()
      model_deps <- ugplot_remote_model_deps(
        server_url = server$url,
        token = server$token %||% ""
      )
      load_ml_list(model_deps)
      ml_model_source_status_text(paste("Models loaded from remote server:", server$name))
      return(invisible(model_deps))
    }

    model_deps <- ugplot_model_dependency_status()
    load_ml_list(model_deps)
    ml_model_source_status_text("Models loaded from this R session.")
    invisible(model_deps)
  }

  output$ml_model_source_status <- renderText({
    ml_model_source_status_text()
  })

  output$geo_remote_execution_status <- renderUI({
    run_target <- input$geo_run_target %||% "local"
    if (!identical(run_target, "remote")) {
      return(tags$p(class = "geo-step-note", "GEO processing will run in this R session."))
    }
    server <- selected_geo_remote_server()
    server_url <- as.character(server$url[[1]] %||% "")
    server_name <- as.character(server$name[[1]] %||% "")
    tags$div(
      tags$p(class = "geo-step-note",
        paste0("Remote GEO server selected: ", server_name),
        if (nzchar(server_url)) paste0(" (", server_url, ").") else "."
      ),
      tags$p(class = "geo-step-note",
        "Start a remote GEO pipeline to keep downloads, matrix preparation, CpG scan, and saved outputs on the selected server."
      ),
      tags$p(class = "geo-step-note", htmltools::htmlEscape(geo_remote_pipeline_status()))
    )
  })

  observeEvent(input$ml_run_target, {
    tryCatch({
      load_selected_ml_list()
    }, error = function(e) {
      ml_model_source_status_text(paste("Could not load model list:", conditionMessage(e)))
    })
  }, ignoreInit = TRUE)

  observeEvent(input$remote_server_name, {
    if (!identical(input$ml_run_target %||% "local", "remote")) {
      return()
    }
    tryCatch({
      load_selected_ml_list()
    }, error = function(e) {
      ml_model_source_status_text(paste("Could not load remote model list:", conditionMessage(e)))
    })
  }, ignoreInit = TRUE)

  show_remote_server_modal <- function(server = NULL) {
    is_edit <- is.data.frame(server) && nrow(server) == 1
    config_remote_editing_name(if (is_edit) server$name[[1]] else NULL)
    config_remote_test_status("")
    current_cpu_limit <- suppressWarnings(as.integer(if (is_edit && "cpu_limit" %in% names(server)) server$cpu_limit[[1]] else default_system_cpu_limit))
    if (is.na(current_cpu_limit) || current_cpu_limit < 1L) {
      current_cpu_limit <- default_system_cpu_limit
    }
    current_cpu_max <- suppressWarnings(as.integer(if (is_edit && "cpu_max" %in% names(server)) server$cpu_max[[1]] else total_system_cpus))
    if (is.na(current_cpu_max) || current_cpu_max < 1L) {
      current_cpu_max <- max(total_system_cpus, current_cpu_limit)
    }
    current_cpu_max <- max(current_cpu_max, current_cpu_limit)
    config_remote_detected_cpus(current_cpu_max)
    cpu_slider_max <- current_cpu_max
    showModal(modalDialog(
      title = if (is_edit) "Edit remote server" else "Add remote server",
      textInput("config_remote_name", "Server name", value = if (is_edit) server$name[[1]] else ""),
      textInput("config_remote_url", "Server URL", value = if (is_edit) server$url[[1]] else "http://127.0.0.1:8080"),
      passwordInput("config_remote_token", "Token", value = if (is_edit) server$token[[1]] else ""),
      sliderInput(
        "config_remote_cpu_limit",
        "CPUs to use on this server",
        min = 1,
        max = cpu_slider_max,
        value = min(current_cpu_limit, cpu_slider_max),
        step = 1
      ),
      tags$div(
        class = "remote-server-test-row",
        actionButton("config_remote_test", "Test connection", icon = icon("plug")),
        uiOutput("config_remote_test_status", inline = TRUE)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("config_remote_save", if (is_edit) "Save changes" else "Add server", icon = icon("save"))
      ),
      easyClose = TRUE
    ))
  }

  remote_server_action_button <- function(action, name, label, icon_name, class_name) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      return("")
    }
    payload <- jsonlite::toJSON(
      list(action = action, name = name),
      auto_unbox = TRUE
    )
    payload <- htmltools::htmlEscape(payload, attribute = TRUE)
    sprintf(
      paste0(
        "<button type=\"button\" class=\"btn btn-default btn-sm remote-server-action %s\" ",
        "onclick=\"Shiny.setInputValue('config_remote_action', Object.assign(%s, {nonce: Math.random()}), {priority: 'event'})\">",
        "<i class=\"fa fa-%s\"></i> %s</button>"
      ),
      class_name,
      payload,
      icon_name,
      label
    )
  }

  remote_servers_table_data <- function() {
    servers <- remote_servers()
    if (!is.data.frame(servers) || nrow(servers) == 0) {
      servers <- data.frame(name = character(0), url = character(0), token = character(0), cpu_limit = integer(0), cpu_max = integer(0), stringsAsFactors = FALSE)
    }
    token_status <- ifelse(nzchar(servers$token %||% ""), "Set", "")
    cpu_text <- paste0(servers$cpu_limit, " / ", servers$cpu_max)
    actions <- vapply(servers$name, function(server_name) {
      paste(
        remote_server_action_button("edit", server_name, "Edit", "pencil", "remote-server-edit"),
        remote_server_action_button("remove", server_name, "Remove", "trash", "remote-server-remove")
      )
    }, character(1))
    data.frame(
      name = servers$name,
      url = servers$url,
      token = token_status,
      cpus = cpu_text,
      actions = actions,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  observeEvent(input$config_remote_add, {
    show_remote_server_modal()
  })

  observeEvent(input$config_remote_test, {
    tryCatch({
      health <- ugplot_remote_health(
        server_url = input$config_remote_url,
        token = input$config_remote_token %||% ""
      )
      cpus <- suppressWarnings(as.integer(health$cpus %||% NA_integer_))
      if (is.na(cpus) || cpus < 1L) {
        stop("Server did not return CPU information.", call. = FALSE)
      }
      default_limit <- suppressWarnings(as.integer(health$default_cpu_limit %||% max(1L, cpus - 1L)))
      if (is.na(default_limit) || default_limit < 1L) {
        default_limit <- max(1L, cpus - 1L)
      }
      remote_version <- as.character(health$ugplot_build_version %||% "")
      local_version <- ugplot_build_version()
      version_message <- if (identical(ugplot_compare_build_versions(local_version, remote_version), 0L)) {
        ""
      } else {
        paste0(" ", ugplot_version_mismatch_message(local_version, remote_version))
      }
      config_remote_detected_cpus(cpus)
      updateSliderInput(
        session,
        "config_remote_cpu_limit",
        max = cpus,
        value = min(default_limit, cpus)
      )
      config_remote_test_status(paste0("Connected. Available CPUs: ", cpus, ". Suggested: ", min(default_limit, cpus), ".", version_message))
    }, error = function(e) {
      config_remote_test_status(paste("Connection failed:", conditionMessage(e)))
    })
  })

  observeEvent(input$config_remote_action, {
    action <- input$config_remote_action$action %||% ""
    server_name <- input$config_remote_action$name %||% ""
    servers <- remote_servers()
    server <- servers[servers$name == server_name, , drop = FALSE]
    if (nrow(server) != 1) {
      showModal(modalDialog(title = "Remote server error", "Server not found.", easyClose = TRUE))
      return()
    }
    if (identical(action, "edit")) {
      show_remote_server_modal(server)
      return()
    }
    if (identical(action, "remove")) {
      showModal(modalDialog(
        title = "Remove remote server",
        paste("Remove", server_name, "from configured servers?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("config_remote_confirm_remove", "Remove", icon = icon("trash"), class = "btn-danger")
        ),
        easyClose = TRUE
      ))
      config_remote_editing_name(server_name)
    }
  })

  observeEvent(input$config_remote_save, {
    tryCatch({
      previous_name <- config_remote_editing_name()
      if (!is.null(previous_name) && !identical(previous_name, input$config_remote_name)) {
        ugplot_remove_remote_server(previous_name)
      }
      servers <- ugplot_upsert_remote_server(
        name = input$config_remote_name,
        url = input$config_remote_url,
        token = input$config_remote_token %||% "",
        cpu_limit = input$config_remote_cpu_limit %||% 1L,
        cpu_max = config_remote_detected_cpus() %||% input$config_remote_cpu_limit %||% 1L
      )
      remote_servers(servers)
      refresh_remote_server_inputs(input$config_remote_name)
      config_remote_editing_name(NULL)
      removeModal()
    }, error = function(e) {
      showModal(modalDialog(title = "Remote server error", e$message, easyClose = TRUE))
    })
  })

  observeEvent(input$config_remote_confirm_remove, {
    server_name <- config_remote_editing_name()
    req(nzchar(server_name %||% ""))
    servers <- ugplot_remove_remote_server(server_name)
    remote_servers(servers)
    refresh_remote_server_inputs()
    config_remote_editing_name(NULL)
    removeModal()
  })

  output$config_remote_servers_table <- DT::renderDT({
    DT::datatable(
      remote_servers_table_data(),
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        lengthChange = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = 4, orderable = FALSE, searchable = FALSE, className = "remote-server-actions")
        )
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })

  geo_storage_dir_size <- function(path) {
    if (!nzchar(path %||% "") || !dir.exists(path)) {
      return(0)
    }
    files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    files <- files[file.exists(files) & !dir.exists(files)]
    if (length(files) == 0) {
      return(0)
    }
    sum(file.info(files)$size, na.rm = TRUE)
  }

  geo_storage_file_size <- function(paths) {
    paths <- paths[nzchar(paths %||% "") & file.exists(paths) & !dir.exists(paths)]
    if (length(paths) == 0) {
      return(0)
    }
    sum(file.info(paths)$size, na.rm = TRUE)
  }

  geo_storage_accessions <- function() {
    root <- ugplot_geo_cache_root("downloads")
    if (!dir.exists(root)) {
      return(character(0))
    }
    dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
    dirs[file.exists(file.path(dirs, "ugplot_geo_manifest.rds")) |
      grepl("^GSE[0-9]+$", basename(dirs), ignore.case = TRUE)]
  }

  geo_storage_report <- reactive({
    config_geo_storage_refresh()
    accession_dirs <- geo_storage_accessions()
    if (length(accession_dirs) == 0) {
      return(data.frame())
    }
    rows <- lapply(accession_dirs, function(cache_dir) {
      accession <- basename(cache_dir)
      processed_files <- ugplot_geo_matrix_files(cache_dir, source = "processed")
      raw_top_files <- list.files(cache_dir, full.names = TRUE, recursive = FALSE)
      raw_top_files <- raw_top_files[vapply(raw_top_files, function(path) {
        grepl("\\.idat(\\.gz)?$|\\.(tar|tar\\.gz|tgz|zip)$", basename(path), ignore.case = TRUE)
      }, logical(1))]
      legacy_analysis_files <- list.files(cache_dir, full.names = TRUE, recursive = FALSE)
      legacy_analysis_files <- legacy_analysis_files[grepl("^ugplot_geo_spearman_|^ugplot_geo_transcript_", basename(legacy_analysis_files))]
      processed_analysis_dir <- file.path(cache_dir, "analysis", "processed")
      raw_analysis_dir <- file.path(cache_dir, "analysis", "raw_sesame")
      data.frame(
        accession = accession,
        total_bytes = geo_storage_dir_size(cache_dir),
        processed_matrix_bytes = geo_storage_file_size(processed_files),
        raw_idat_bytes = geo_storage_dir_size(ugplot_geo_raw_idat_dir(cache_dir)) + geo_storage_file_size(raw_top_files),
        sesame_bytes = geo_storage_dir_size(ugplot_geo_sesame_dir(cache_dir)),
        processed_analysis_bytes = geo_storage_dir_size(processed_analysis_dir) +
          geo_storage_file_size(legacy_analysis_files) +
          geo_storage_dir_size(file.path(cache_dir, "transcript_datasets")),
        raw_analysis_bytes = geo_storage_dir_size(raw_analysis_dir),
        path = cache_dir,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })

  geo_storage_action_button <- function(action, accession, label, icon_name, class_name = "btn-default") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      return("")
    }
    payload <- jsonlite::toJSON(list(action = action, accession = accession), auto_unbox = TRUE)
    payload <- htmltools::htmlEscape(payload, attribute = TRUE)
    sprintf(
      paste0(
        "<button type=\"button\" class=\"btn %s btn-xs\" style=\"margin-right: 4px; margin-bottom: 4px;\" ",
        "onclick=\"Shiny.setInputValue('config_geo_storage_action', Object.assign(%s, {nonce: Math.random()}), {priority: 'event'})\">",
        "<i class=\"fa fa-%s\"></i> %s</button>"
      ),
      class_name, payload, icon_name, label
    )
  }

  output$config_geo_storage_summary <- renderUI({
    report <- geo_storage_report()
    root <- ugplot_geo_cache_root("downloads")
    if (!is.data.frame(report) || nrow(report) == 0) {
      return(tags$p(paste0("No local GEO cache found in ", root, ".")))
    }
    total_bytes <- sum(report$total_bytes, na.rm = TRUE)
    tags$div(class = "geo-status-card",
      tags$p(tags$strong("GEO cache root: "), root),
      tags$p(tags$strong("Accessions: "), nrow(report), tags$span(" | "),
        tags$strong("Total size: "), ugplot_format_bytes(total_bytes)
      )
    )
  })

  output$config_geo_storage_table <- DT::renderDT({
    report <- geo_storage_report()
    if (!is.data.frame(report) || nrow(report) == 0) {
      return(DT::datatable(data.frame(), options = list(dom = "t"), rownames = FALSE))
    }
    display <- data.frame(
      Accession = report$accession,
      Total = vapply(report$total_bytes, ugplot_format_bytes, character(1)),
      `Processed matrices` = vapply(report$processed_matrix_bytes, ugplot_format_bytes, character(1)),
      `Raw IDAT` = vapply(report$raw_idat_bytes, ugplot_format_bytes, character(1)),
      Sesame = vapply(report$sesame_bytes, ugplot_format_bytes, character(1)),
      `Processed analysis` = vapply(report$processed_analysis_bytes, ugplot_format_bytes, character(1)),
      `IDAT analysis` = vapply(report$raw_analysis_bytes, ugplot_format_bytes, character(1)),
      Path = report$path,
      Actions = vapply(report$accession, function(accession) {
        paste(
          geo_storage_action_button("delete_processed_analysis", accession, "Delete processed analysis", "trash"),
          geo_storage_action_button("delete_raw_analysis", accession, "Delete IDAT analysis", "trash"),
          geo_storage_action_button("delete_idat_sesame", accession, "Delete IDAT/sesame files", "trash"),
          geo_storage_action_button("delete_all", accession, "Delete all", "trash", "btn-danger")
        )
      }, character(1)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    DT::datatable(
      display,
      options = list(pageLength = 10, scrollX = TRUE, columnDefs = list(list(targets = 8, orderable = FALSE, searchable = FALSE))),
      rownames = FALSE,
      escape = which(names(display) != "Actions")
    )
  })

  observeEvent(input$config_geo_storage_refresh, {
    config_geo_storage_refresh(config_geo_storage_refresh() + 1L)
  }, ignoreInit = TRUE)

  geo_storage_delete_targets <- function(accession, action) {
    cache_dir <- ugplot_geo_cache_dir(accession)
    if (identical(action, "delete_all")) {
      return(list(paths = cache_dir, label = "all GEO cache content"))
    }
    if (identical(action, "delete_processed_analysis")) {
      legacy_files <- list.files(cache_dir, full.names = TRUE, recursive = FALSE)
      legacy_files <- legacy_files[grepl("^ugplot_geo_spearman_|^ugplot_geo_transcript_", basename(legacy_files))]
      return(list(
        paths = c(file.path(cache_dir, "analysis", "processed"), file.path(cache_dir, "transcript_datasets"), legacy_files),
        label = "processed matrix analysis outputs"
      ))
    }
    if (identical(action, "delete_raw_analysis")) {
      return(list(paths = file.path(cache_dir, "analysis", "raw_sesame"), label = "IDAT/sesame analysis outputs"))
    }
    if (identical(action, "delete_idat_sesame")) {
      raw_top_files <- list.files(cache_dir, full.names = TRUE, recursive = FALSE)
      raw_top_files <- raw_top_files[grepl("\\.idat(\\.gz)?$|\\.(tar|tar\\.gz|tgz|zip)$", basename(raw_top_files), ignore.case = TRUE)]
      return(list(
        paths = c(ugplot_geo_raw_idat_dir(cache_dir), ugplot_geo_sesame_dir(cache_dir), raw_top_files),
        label = "raw IDAT downloads and sesame beta/QC outputs"
      ))
    }
    list(paths = character(0), label = "")
  }

  observeEvent(input$config_geo_storage_action, {
    action <- input$config_geo_storage_action$action %||% ""
    accession <- input$config_geo_storage_action$accession %||% ""
    if (!nzchar(accession) || !nzchar(action)) {
      return()
    }
    targets <- geo_storage_delete_targets(accession, action)
    existing_paths <- targets$paths[file.exists(targets$paths)]
    if (length(existing_paths) == 0) {
      showModal(modalDialog(title = "GEO storage", "Nothing to delete for this selection.", easyClose = TRUE))
      return()
    }
    config_geo_delete_request(list(action = action, accession = accession, paths = existing_paths, label = targets$label))
    showModal(modalDialog(
      title = paste("Delete GEO storage for", accession),
      tags$p(paste0("This will delete ", targets$label, ".")),
      tags$p(tags$strong("Paths:")),
      tags$pre(paste(existing_paths, collapse = "\n")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("config_geo_storage_confirm_delete", "Delete", icon = icon("trash"), class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$config_geo_storage_confirm_delete, {
    request <- config_geo_delete_request()
    req(is.list(request), length(request$paths) > 0)
    root <- normalizePath(ugplot_geo_cache_root("downloads"), mustWork = FALSE)
    paths <- normalizePath(request$paths, mustWork = FALSE)
    inside_root <- startsWith(paths, paste0(root, .Platform$file.sep)) | paths == root
    if (!all(inside_root)) {
      removeModal()
      showModal(modalDialog(title = "GEO storage delete blocked", "Delete target is outside the GEO cache root.", easyClose = TRUE))
      return()
    }
    unlink(paths, recursive = TRUE, force = TRUE)
    config_geo_delete_request(NULL)
    config_geo_storage_refresh(config_geo_storage_refresh() + 1L)
    removeModal()
    showModal(modalDialog(
      title = "GEO storage deleted",
      paste0("Deleted ", request$label, " for ", request$accession, "."),
      easyClose = TRUE
    ))
  })

  ml_data_table <- reactiveVal(data.frame())
  ml_table_results <- reactiveVal(data.frame())
  ml_final_summary <- reactiveVal(NULL)
  ml_plot_importance <- reactiveVal()
  num_rows <- reactiveVal(0)
  num_cols <- reactiveVal(0)
  text_result_ml <- reactiveVal(0)
  changed_table <<- ""
  numeric_table <- ""
  changed_palette <- 0
  annotation_row <- ""

  max_table_columns <- 50
  table_message_text <- reactiveVal("")
  table_cleaning_message_text <<- reactiveVal("")
  ml_error_message_text <- reactiveVal("")
  remote_jobs <- reactiveVal(data.frame())
  remote_job_status_text <- reactiveVal("")
  remote_job_log_text <- reactiveVal("")
  remote_job_resources_data <- reactiveVal(data.frame())
  remote_job_preview_status <- reactiveVal(NULL)
  remote_job_preview_result <- reactiveVal(NULL)
  remote_job_progress_estimates <- reactiveVal(list())
  remote_job_loading <- reactiveVal(FALSE)
  remote_geo_result_applying <- reactiveVal(FALSE)
  remote_selected_job <- reactiveVal(list(id = "", server = ""))
  remote_server_capabilities <- reactiveVal(list())
  remote_server_connection_state <- reactiveVal(data.frame())
  remote_result_cache <- reactiveVal(NULL)
  remote_result_cache_job_id <- reactiveVal("")
  defaultpalette <- reactiveVal(colorRampPalette(c("red", "yellow", "green"))(256))
  transpose_table2 <- reactiveVal(0)
  refresh_counter <- reactiveVal(0)
  scrambled_columns <- reactiveVal(character(0))
  scramble_original_columns <- reactiveVal(list())

  tab_separator <- reactiveVal(",")
  file_click_count <- reactiveVal(0)
  last_file_click_count <- 0
  original_dataset_filename <- reactiveVal("model_analysis_results")
  model_analysis_results_data <- reactiveVal(data.frame())
  heatmap_recorded_plot <- reactiveVal(NULL)
  model_analysis_recorded_plot <- reactiveVal(NULL)
  model_analysis_metrics_report <- reactiveVal("")
  gm_nodes_metrics <- reactiveVal(data.frame())
  gm_edges_metrics <- reactiveVal(data.frame())
  pending_duplicate_row_names_upload <- reactiveVal(NULL)
  geo_files <- reactiveVal(data.frame())
  geo_remote_files <- reactiveVal(data.frame())
  geo_sample_metadata <- reactiveVal(data.frame())
  geo_cpg_annotation <- reactiveVal(data.frame())
  geo_pending_annotation_platform <- reactiveVal(NULL)
  geo_spearman_results <- reactiveVal(data.frame())
  geo_spearman_raw_results <- reactiveVal(data.frame())
  geo_transcript_candidates <- reactiveVal(data.frame())
  geo_transcript_groups <- reactiveVal(data.frame())
  geo_transcript_group_details <- reactiveVal(data.frame())
  geo_transcript_build_progress <- reactiveVal(list(
    phase = "idle",
    message = "Transcript CSV build has not started.",
    processed = 0L,
    total = 0L,
    compatible = 0L,
    excluded = 0L,
    current = "",
    cache = ""
  ))
  geo_transcript_ml_progress <- reactiveVal(list(
    phase = "idle",
    message = "Transcript ML pipeline has not started.",
    processed = 0L,
    total = 0L,
    current = "",
    cache = ""
  ))
  geo_transcript_ml_results <- reactiveVal(data.frame())
  geo_transcript_ml_focus_group <- reactiveVal("")
  geo_transcript_ml_focus_stratum <- reactiveVal(list(column = "", value = ""))
  geo_remote_cpg_summary <- reactiveVal(list(key = "", data = NULL, status = "idle", message = ""))
  geo_cpg_lookup_state <- reactiveVal(list(key = "", data = NULL, status = "idle", message = ""))
  geo_idat_qc_report <- reactiveVal(data.frame())
  geo_idat_qc_progress <- reactiveVal(list(
    phase = "idle",
    message = "Raw IDAT reprocessing has not started.",
    processed = 0L,
    total = 0L,
    current = "",
    beta_path = "",
    qc_path = ""
  ))
  geo_status <- reactiveVal("Waiting for GEO accession.")
  geo_stage <- reactiveVal(list(
    step = "Step 1",
    title = "Inspect a GEO accession",
    message = "Enter a GEO accession and inspect the available supplementary files."
  ))
  geo_run_target_state <- reactiveVal("local")
  geo_remote_pipeline_job_id <- reactiveVal("")
  geo_remote_pipeline_status <- reactiveVal("")
  geo_download_progress <- reactiveVal(list(
    type = "download",
    percent = 0,
    file = "Waiting",
    detail = "No download running.",
    folder = ""
  ))
  geo_preview_data <- reactiveVal(data.frame())
  shinyjs::disable("geo_fetch_files")
  shinyjs::disable("geo_extract_files")
  shinyjs::disable("geo_fetch_metadata")

  finish_uploaded_dataset_load <- function(data) {
    df_pre <<- data
    reset_missing_strategy_ui()
    updateTextAreaInput(session, "textarea_columns", value = paste(names(df_pre), collapse = "\n"))
    updateTextAreaInput(session, "textarea_rows", value = paste(rownames(df_pre), collapse = "\n"))
  }

  output$geo_import_status <- renderText({
    geo_status()
  })

  output$geo_stage_status <- renderUI({
    stage <- geo_stage()
    tags$div(class = "geo-status-card",
      tags$div(class = "geo-status-title", paste(stage$step, "-", stage$title)),
      tags$div(stage$message)
    )
  })

  render_geo_progress <- function(progress) {
    tags$div(class = "geo-progress-card",
      tags$div(class = "geo-progress-shell",
        tags$div(
          id = "geoProgressBar",
          class = "geo-progress-bar",
          role = "progressbar",
          `aria-valuemin` = "0",
          `aria-valuemax` = "100",
          `aria-valuenow` = round(progress$percent %||% 0),
          style = paste0("width: ", round(progress$percent %||% 0), "%;")
        )
      ),
      tags$div(class = "geo-progress-meta",
        tags$strong(id = "geoProgressText", paste0(round(progress$percent %||% 0), "%")),
        tags$span(" - "),
        tags$span(id = "geoProgressFile", progress$file %||% "Waiting"),
        tags$br(),
        tags$span(id = "geoProgressDetail", progress$detail %||% ""),
        if (nzchar(progress$folder %||% "")) tags$div(paste0("Folder: ", progress$folder)) else NULL
      )
    )
  }

  output$geo_download_progress_ui <- renderUI({
    progress <- geo_download_progress()
    if (identical(progress$type %||% "download", "extract")) {
      return(NULL)
    }
    render_geo_progress(progress)
  })

  output$geo_extract_progress_ui <- renderUI({
    progress <- geo_download_progress()
    if (!identical(progress$type %||% "download", "extract")) {
      return(NULL)
    }
    render_geo_progress(progress)
  })

  render_geo_step_card <- function(number, title, done = FALSE, body = NULL) {
    tags$div(
      class = paste("geo-step", if (isTRUE(done)) "geo-step-done" else "geo-step-pending"),
      tags$div(class = "geo-step-title",
        tags$span(paste0(number, ". ", title)),
        tags$span(class = paste("geo-step-badge", if (isTRUE(done)) "geo-step-badge-done" else "geo-step-badge-pending"),
          if (isTRUE(done)) "done" else "pending"
        )
      ),
      body
    )
  }

  render_geo_table_details <- function(summary, title_output, table_output, open = FALSE, extra = NULL,
                                       class_name = "geo-table-section geo-step-table") {
    tags$details(class = class_name, open = if (isTRUE(open)) TRUE else NULL,
      tags$summary(summary),
      title_output,
      table_output,
      extra
    )
  }

  geo_download_selection <- function(remote_files, source = "processed") {
    if (!is.data.frame(remote_files) || nrow(remote_files) == 0) {
      return(remote_files[0, , drop = FALSE])
    }
    source <- source %||% "processed"
    if (identical(source, "raw_sesame")) {
      return(remote_files[remote_files$Type %in% c("IDAT", "archive"), , drop = FALSE])
    }
    remote_files[remote_files$Loadable, , drop = FALSE]
  }

  output$geo_workflow_ui <- renderUI({
    accession_value <- isolate(input$geo_accession %||% "")
    current_geo_run_target <- geo_run_target_state()
    run_remote <- identical(current_geo_run_target, "remote")
    remote_geo_result <- remote_job_preview_result()
    remote_geo_loaded <- run_remote && is.list(remote_geo_result) && identical(remote_geo_result$kind %||% "", "geo_pipeline")
    source_value <- if (remote_geo_loaded && nzchar(remote_geo_result$matrix_source %||% "")) {
      remote_geo_result$matrix_source
    } else {
      input$geo_matrix_source %||% "processed"
    }
    source_value <- geo_matrix_source_value(source_value)
    remote_settings <- if (remote_geo_loaded && is.list(remote_geo_result$settings)) remote_geo_result$settings else list()
    threshold_value <- isolate(input$geo_transcript_absrho_threshold %||% remote_settings$transcript_absrho_threshold %||% 0.8)
    threshold_value <- suppressWarnings(as.numeric(threshold_value))
    if (!is.finite(threshold_value)) {
      threshold_value <- 0.8
    }
    max_cpgs_value <- isolate(input$geo_spearman_max_cpgs %||% remote_settings$spearman_max_cpgs %||% 0)
    min_spearman_samples_value <- isolate(input$geo_spearman_min_samples %||% remote_settings$spearman_min_samples_pct %||% 80)
    metadata <- geo_sample_metadata()
    remote_files <- geo_remote_files()
    local_files <- geo_files()
    annotation_map <- geo_cpg_annotation()
    spearman_results <- geo_spearman_raw_results()
    transcript_table <- geo_transcript_candidates()
    transcript_groups <- geo_transcript_groups()
    transcript_progress <- geo_transcript_build_progress()
    transcript_ml_progress <- geo_transcript_ml_progress()
    transcript_ml_results <- geo_transcript_ml_results()
	    idat_progress <- geo_idat_qc_progress()
	    idat_qc <- geo_idat_qc_report()
	    preview <- geo_preview_data()
	    remote_matrix_files <- if (remote_geo_loaded && identical(remote_geo_result$matrix_source %||% source_value, source_value)) {
	      as.character(unlist(remote_geo_result$paths$matrix_files %||% character(0), use.names = FALSE))
	    } else {
	      character(0)
	    }
	    remote_matrix_files <- remote_matrix_files[nzchar(remote_matrix_files)]
	    if (remote_geo_loaded) {
	      if ((!is.data.frame(transcript_table) || nrow(transcript_table) == 0) &&
	          is.data.frame(remote_geo_result$tables$transcript_candidates_preview)) {
	        transcript_table <- remote_geo_result$tables$transcript_candidates_preview
	      }
	      if ((!is.data.frame(transcript_groups) || nrow(transcript_groups) == 0) &&
	          is.data.frame(remote_geo_result$tables$transcript_groups)) {
	        transcript_groups <- remote_geo_result$tables$transcript_groups
	      }
	      remote_ml_summary <- remote_geo_result$tables$transcript_ml_summary
	      if (!is.data.frame(remote_ml_summary) || nrow(remote_ml_summary) == 0) {
	        remote_ml_summary <- remote_geo_result$tables$transcript_ml_screening
	      }
	      if ((!is.data.frame(transcript_ml_results) || nrow(transcript_ml_results) == 0) &&
	          is.data.frame(remote_ml_summary)) {
	        transcript_ml_results <- remote_ml_summary
	      }
	    }

    metadata_done <- is.data.frame(metadata) && nrow(metadata) > 0
    files_seen <- (is.data.frame(remote_files) && nrow(remote_files) > 0) || (is.data.frame(local_files) && nrow(local_files) > 0)
    files_done <- FALSE
    selected_download_done <- FALSE
    needs_extract <- FALSE
    source_is_raw <- identical(source_value, "raw_sesame")
    if (is.data.frame(remote_files) && nrow(remote_files) > 0) {
      processed_files <- remote_files[remote_files$Loadable, , drop = FALSE]
      files_done <- nrow(processed_files) > 0 && !any(processed_files$NeedsDownload %||% TRUE)
      selected_files <- geo_download_selection(remote_files, source_value)
      selected_download_done <- nrow(selected_files) > 0 && !any(selected_files$NeedsDownload %||% TRUE)
      needs_extract <- any(processed_files$LocalStatus == "downloaded" & grepl("\\.gz$", processed_files$File, ignore.case = TRUE))
    }
    matrix_files <- if (nzchar(trimws(accession_value))) ugplot_geo_matrix_files(ugplot_geo_cache_dir(trimws(accession_value)), source = source_value) else character(0)
    extract_done <- length(matrix_files) > 0 || length(remote_matrix_files) > 0
    if (remote_geo_loaded && length(remote_matrix_files) > 0) {
      selected_download_done <- TRUE
      files_done <- TRUE
      needs_extract <- FALSE
    }
    annotation_done <- is.data.frame(annotation_map) && nrow(annotation_map) > 0
    if (!annotation_done && metadata_done) {
      platform_info <- ugplot_geo_platform_annotation_package(ugplot_geo_detect_platform(metadata))
      if (!is.null(platform_info)) {
        annotation_done <- file.exists(ugplot_geo_annotation_cache_path(platform_info$platform, "rds"))
      }
    }
    spearman_done <- is.data.frame(spearman_results) && nrow(spearman_results) > 0 &&
      (length(matrix_files) > 0 || length(remote_matrix_files) > 0 || remote_geo_loaded)
    transcript_needs_rebuild <- identical(transcript_progress$phase %||% "", "needs rebuild")
    transcript_groups_loaded <- is.data.frame(transcript_groups) && nrow(transcript_groups) > 0
    transcript_done <- !transcript_needs_rebuild && (
      (transcript_progress$phase %||% "") %in% c("complete", "loaded from cache", "loaded from remote") ||
        transcript_groups_loaded
    ) && spearman_done
    transcript_ml_ready <- transcript_done && transcript_groups_loaded
    transcript_ml_done <- transcript_ml_ready && (
      (transcript_ml_progress$phase %||% "") %in% c("complete", "loaded from cache", "loaded from remote", "already complete") ||
        (is.data.frame(transcript_ml_results) && nrow(transcript_ml_results) > 0)
    )
    transcript_ml_stability_done <- transcript_ml_ready &&
      is.data.frame(transcript_ml_results) &&
      nrow(transcript_ml_results) > 0 &&
      "Phase" %in% names(transcript_ml_results) &&
      any(as.character(transcript_ml_results$Phase) == "stability")
    remote_idat_done <- remote_geo_loaded &&
      (nzchar(remote_geo_result$paths$sesame_beta %||% "") || nzchar(remote_geo_result$paths$sesame_qc %||% ""))
    idat_done <- remote_idat_done ||
      (idat_progress$phase %||% "") %in% c("complete", "loaded from cache", "loaded from remote") ||
      (is.data.frame(idat_qc) && nrow(idat_qc) > 0 && nzchar(idat_progress$beta_path %||% "") && (file.exists(idat_progress$beta_path %||% "") || remote_geo_loaded))
	    preview_done <- is.data.frame(preview) && nrow(preview) > 0

	    tagList(
	      tags$div(class = "geo-workflow geo-workflow-top",
	        render_geo_step_card(1, "Inspect GEO accession", files_seen || metadata_done,
	          tags$div(
            textInput("geo_accession", "GEO accession:", value = accession_value, placeholder = "GSE87571"),
	            actionButton("geo_inspect_files", if (files_seen || metadata_done) "Refresh GEO status" else "Inspect files")
          )
        ),
        render_geo_step_card(2, "Sample metadata", metadata_done,
          tags$div(
            tags$p(class = "geo-step-note", "Sample metadata contains age, sex, tissue, disease, treatment, response, and other phenotype fields when GEO provides them."),
            uiOutput("geo_metadata_summary"),
	            if (!metadata_done) actionButton("geo_fetch_metadata", "Fetch sample metadata") else NULL
          )
        ),
        tags$div(class = "geo-run-target-panel",
          radioButtons(
            "geo_run_target",
            "GEO processing location:",
            choices = c("Local" = "local", "Remote server" = "remote"),
            selected = current_geo_run_target,
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.geo_run_target == 'remote'",
            selectInput(
              "geo_remote_server_name",
              "Remote server:",
              choices = remote_server_choices(),
              selected = isolate(input$geo_remote_server_name %||% input$remote_server_name)
            ),
            tags$div(class = "remote-server-toolbar",
              actionButton("geo_start_remote_pipeline", "Start remote GEO pipeline", icon = icon("play")),
              actionButton("geo_refresh_remote_pipeline", "Refresh status", icon = icon("refresh")),
              actionButton("geo_load_remote_pipeline_result", "Load remote result", icon = icon("download"))
            )
          ),
          uiOutput("geo_remote_execution_status")
        ),
        render_geo_step_card(3, "Matrix files", selected_download_done,
          tags$div(
            tags$p(class = "geo-step-note", "Choose whether this GEO run uses processed matrices or raw IDAT files reprocessed with sesame."),
            selectInput(
              "geo_matrix_source",
              "Matrix source:",
              choices = c("Use GEO processed matrix" = "processed", "Recalculate from raw IDAT with sesame" = "raw_sesame"),
              selected = source_value
            ),
            uiOutput("geo_source_status_summary"),
            uiOutput("geo_download_summary"),
	            if (!run_remote && !selected_download_done) actionButton("geo_fetch_files", if (source_is_raw) "Download raw IDAT files" else "Download processed matrices") else NULL
          )
        ),
        render_geo_step_card(4, "Download progress", selected_download_done,
          tags$div(
            if (selected_download_done) tags$p(class = "geo-step-note", if (source_is_raw) "Selected raw files are already local." else "Selected processed matrices are already local.") else NULL,
            uiOutput("geo_download_progress_ui")
          )
        ),
        render_geo_step_card(5, if (source_is_raw) "Recalculate beta matrix" else "Extract matrix files", if (source_is_raw) idat_done else extract_done,
          tags$div(
            if (source_is_raw) {
              tags$div(
                tags$p(class = "geo-step-note", if (idat_done) "Sesame beta matrix is available for Spearman." else "Download raw IDAT files, then run sesame QC/reprocessing here before Spearman."),
                numericInput("geo_idat_detection_p", "Probe detection p-value cutoff:", value = 0.05, min = 0.0001, max = 0.2, step = 0.001),
                numericInput("geo_idat_max_failed_fraction", "Maximum failed pOOBAH probe fraction per sample:", value = 0.05, min = 0, max = 1, step = 0.01),
                textInput("geo_idat_sesame_prep", "Sesame prep code:", value = "QCDPB"),
                uiOutput("geo_idat_qc_summary"),
	                if (!run_remote) uiOutput("geo_idat_action_ui") else NULL,
                uiOutput("geo_idat_qc_progress_ui")
              )
            } else if (needs_extract) {
              tags$div(
                tags$p(class = "geo-step-note", "Large .gz matrices must be extracted before preprocessing. They are still too large to load directly into ugPlot."),
	                if (!run_remote) actionButton("geo_extract_files", "Extract downloaded .gz files") else NULL,
                uiOutput("geo_extract_progress_ui")
              )
            } else {
              tags$div(
                tags$p(class = "geo-step-note", if (extract_done) "Extracted matrix files are available locally." else "No compressed matrix is waiting for extraction."),
                uiOutput("geo_extract_progress_ui")
              )
            }
          )
        ),
        render_geo_step_card(6, "Analyze CpGs", spearman_done,
          tags$div(
            uiOutput("geo_target_selector"),
            tags$p(class = "geo-step-note", "Spearman scan uses the selected numeric metadata field and saves CpG-level correlations for the active matrix source."),
            numericInput("geo_spearman_max_cpgs", "Max CpGs to scan (0 = all):", value = max_cpgs_value, min = 0, step = 10000),
            numericInput("geo_spearman_min_samples", "Minimum samples per CpG for Spearman (%):", value = min_spearman_samples_value, min = 0, max = 100, step = 1),
            numericInput("geo_transcript_absrho_threshold", "Transcript CpG threshold |rho|:", value = threshold_value, min = 0, max = 1, step = 0.01),
            uiOutput("geo_spearman_summary"),
            tags$div(
              style = "margin-top: 10px; padding: 10px 12px; border: 1px solid #dbe7f3; background: #f8fbff; border-radius: 4px;",
              tags$p(style = "margin: 0 0 8px 0; font-weight: 700;", "CpG lookup"),
              textInput("geo_cpg_lookup_id", "CpG id:", value = "", placeholder = "cg04193160"),
              actionButton("geo_cpg_lookup_run", "Lookup CpG", class = "btn btn-default btn-sm"),
              uiOutput("geo_cpg_lookup_ui")
            ),
            if (length(matrix_files) > 0) {
	              if (!run_remote) actionButton("geo_run_spearman", if (spearman_done) "Re-run CpG Spearman scan" else "Run CpG Spearman scan") else NULL
            } else {
              tags$p(class = "geo-step-note", if (source_is_raw) "Run sesame IDAT QC first to create the beta matrix." else "Download/extract processed matrices before running Spearman.")
            }
          )
        ),
        render_geo_step_card(7, "Load CpG annotation", annotation_done,
          tags$div(
            tags$p(class = "geo-step-note", "Build or load the CpG-to-gene/transcript map for the current GEO platform."),
            uiOutput("geo_annotation_summary"),
	            if (!run_remote && !annotation_done) actionButton("geo_build_annotation", "Build/load CpG annotation cache") else NULL
          )
        ),
        render_geo_step_card(8, "Build transcript ML datasets", transcript_done,
          tags$div(
            tags$p(class = "geo-step-note", "Build complete-case transcript CSVs and group transcripts that produce identical ML datasets."),
            numericInput("geo_transcript_min_samples", "Transcript complete-case minimum samples (%):", value = isolate(input$geo_transcript_min_samples %||% remote_settings$transcript_min_samples %||% 80), min = 0, max = 100, step = 1),
            tags$p(class = "geo-step-note", "Transcript complete-case treats empty strings, NA/na text, true NA, and zero as missing."),
            if (spearman_done && annotation_done) {
	              if (!run_remote) actionButton("geo_build_transcript_groups", "Build/continue transcript CSVs") else NULL
            } else {
              tags$p(class = "geo-step-note", "Run Spearman and load annotation before building transcript datasets.")
            },
            uiOutput("geo_transcript_build_progress_ui")
          )
        ),
        render_geo_step_card(9, "Screen transcript ML models", transcript_ml_done,
          tags$div(
            tags$p(class = "geo-step-note", "Screen all installed caret models per transcript group for the active matrix source."),
            numericInput("geo_ml_min_absrho", "Run transcript groups with trigger |rho| >=:", value = 0.7, min = 0, max = 1, step = 0.01),
            numericInput("geo_ml_rank_limit", "Limit to top Spearman-ranked groups (blank = all):", value = NA, min = 1, step = 1),
            checkboxInput("geo_ml_quick_models", "Use one representative model from four ML families", value = FALSE),
            numericInput("geo_ml_screen_seeds", "Screening seeds per model:", value = 3, min = 1, step = 1),
            numericInput("geo_ml_timeout", "Timeout per model/seed (s):", value = 1200, min = 1, step = 1),
            uiOutput("geo_ml_model_summary"),
	            if (run_remote) {
	              tags$div(
	                uiOutput("geo_distributed_screening_ui"),
	                tags$p(class = "geo-step-note", "Screening group checkpoints remain with the coordinator. Stability continues on the coordinator after distributed screening.")
	              )
	            } else if (transcript_ml_ready) {
	              actionButton("geo_run_transcript_ml", "Start/resume model screening")
            } else if (transcript_needs_rebuild) {
              tags$p(class = "geo-step-note", "Rebuild transcript ML datasets before screening models.")
            } else {
              tags$p(class = "geo-step-note", "Build transcript ML datasets before screening models.")
            }
          )
        ),
        render_geo_step_card(10, "Stabilize best transcript ML", transcript_ml_stability_done,
          tags$div(
            tags$p(class = "geo-step-note", "Use each transcript group's best screened model and run seed batches until the metric stabilizes."),
            numericInput("geo_ml_min_stability_seeds", "Minimum stability seeds:", value = 30, min = 2, step = 1),
            numericInput("geo_ml_max_stability_seeds", "Maximum stability seeds:", value = 4000, min = 2, step = 10),
            numericInput("geo_ml_stability_window", "Seeds compared for stability:", value = 30, min = 2, step = 1),
            numericInput("geo_ml_stability_tolerance", "Max metric change to stop:", value = 0.01, min = 0, max = 1, step = 0.001),
            uiOutput("geo_ml_stability_group_selector"),
            uiOutput("geo_ml_stability_group_summary"),
	            if (run_remote) {
	              tags$p(class = "geo-step-note", "Remote mode keeps stability jobs on the selected server.")
	            } else if (transcript_ml_done) {
              actionButton("geo_run_transcript_ml_stability", "Start/resume stability seeds")
            } else if (transcript_ml_ready) {
              tags$p(class = "geo-step-note", "Run or load transcript ML screening before stability seeds.")
            } else if (transcript_needs_rebuild) {
              tags$p(class = "geo-step-note", "Rebuild transcript ML datasets before running transcript ML.")
            } else {
              tags$p(class = "geo-step-note", "Build transcript ML datasets before running transcript ML.")
            },
            uiOutput("geo_transcript_ml_progress_ui")
          )
        )
      ),
      tags$div(class = "geo-table-stack geo-workflow-tables",
        if (metadata_done) render_geo_table_details("Open sample metadata table", uiOutput("geo_metadata_table_title"), DT::DTOutput("geo_metadata_table"), open = FALSE) else NULL,
        if (files_seen) render_geo_table_details("Open GEO files table", uiOutput("geo_files_table_title"), DT::DTOutput("geo_files_table"), open = FALSE) else NULL,
        if (spearman_done) render_geo_table_details(
          "Open CpG Spearman table",
          uiOutput("geo_spearman_table_title"),
          DT::DTOutput("geo_spearman_table"),
          open = FALSE,
          class_name = "geo-table-section geo-step-table"
        ) else NULL,
        if (source_is_raw && is.data.frame(idat_qc) && nrow(idat_qc) > 0) {
          render_geo_table_details("Open sesame IDAT QC report", uiOutput("geo_idat_qc_table_title"), DT::DTOutput("geo_idat_qc_table"), open = FALSE)
        } else NULL,
        if (transcript_done) {
          render_geo_table_details(
            "Open transcript ML candidate groups",
            uiOutput("geo_transcript_candidates_table_title"),
            tagList(
              DT::DTOutput("geo_transcript_groups_table"),
              uiOutput("geo_transcript_group_details_title"),
              plotlyOutput("geo_transcript_group_track", height = "520px"),
              DT::DTOutput("geo_transcript_group_details_table")
            ),
            open = FALSE
          )
        } else NULL,
        if (preview_done) render_geo_table_details("Open loaded ugPlot preview", uiOutput("geo_preview_table_title"), DT::DTOutput("geo_preview_table"), open = FALSE) else NULL
        ,
        render_geo_table_details(
          "Open transcript ML results",
          uiOutput("geo_transcript_ml_table_title"),
          tagList(
            uiOutput("geo_transcript_ml_final_table_title"),
            DT::DTOutput("geo_transcript_ml_final_table"),
            uiOutput("geo_transcript_ml_class_compare_title"),
            uiOutput("geo_transcript_ml_class_order_control"),
            tabsetPanel(
              tabPanel(
                "R2",
                plotlyOutput("geo_transcript_ml_class_rank_plot", height = "420px"),
                DT::DTOutput("geo_transcript_ml_class_compare_table"),
                uiOutput("geo_transcript_ml_class_change_controls_r2"),
                uiOutput("geo_transcript_ml_class_change_title"),
                DT::DTOutput("geo_transcript_ml_class_change_table")
              ),
              tabPanel(
                "Spearman",
                plotlyOutput("geo_transcript_ml_class_spearman_plot", height = "420px"),
                DT::DTOutput("geo_transcript_ml_class_spearman_table"),
                uiOutput("geo_transcript_ml_class_change_controls_spearman"),
                uiOutput("geo_transcript_ml_class_spearman_change_title"),
                DT::DTOutput("geo_transcript_ml_class_spearman_change_table")
              ),
              tabPanel(
                "Combined",
                plotlyOutput("geo_transcript_ml_class_combined_plot", height = "420px"),
                DT::DTOutput("geo_transcript_ml_class_combined_table"),
                uiOutput("geo_transcript_ml_class_change_controls_combined"),
                uiOutput("geo_transcript_ml_class_combined_change_title"),
                DT::DTOutput("geo_transcript_ml_class_combined_change_table")
              )
            ),
            uiOutput("geo_transcript_ml_epigenetic_story_title"),
            DT::DTOutput("geo_transcript_ml_epigenetic_story_table"),
            uiOutput("geo_transcript_ml_epigenetic_cpg_change_title"),
            DT::DTOutput("geo_transcript_ml_epigenetic_cpg_change_table"),
            DT::DTOutput("geo_transcript_ml_table"),
            uiOutput("geo_transcript_ml_selected_title"),
            plotlyOutput("geo_transcript_ml_importance_track", height = "420px"),
            plotlyOutput("geo_transcript_ml_rho_importance_plot", height = "420px")
          ),
          open = FALSE
        )
      )
    )
  })

  output$geo_files_table <- DT::renderDT({
    files <- geo_files()
    remote_files <- geo_remote_files()
    if (is.data.frame(remote_files) && nrow(remote_files) > 0) {
      if (!all(c("LocalStatus", "LocalSize", "NeedsDownload") %in% names(remote_files))) {
        remote_files <- ugplot_geo_annotate_remote_files(remote_files, ugplot_geo_cache_dir(trimws(input$geo_accession %||% "GEO")))
      }
      display <- remote_files[, c("File", "Size", "Type", "MethylationHint", "Loadable", "LocalStatus", "LocalSize"), drop = FALSE]
      display$Action <- ifelse(
        display$LocalStatus == "extracted",
        "Extracted",
        ifelse(display$LocalStatus == "downloaded", "Ready to extract",
        ifelse(display$LocalStatus %in% c("deleted_corrupt", "deleted_corrupt_partial"), "Deleted invalid local copy; download again",
        ifelse(display$LocalStatus == "partial", "Resume", ifelse(display$Loadable, "Download/load", "Skip unless raw preprocessing is needed")))
        )
      )
      return(DT::datatable(display, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE))
    }
    req(is.data.frame(files), nrow(files) > 0)
    display <- files[, c("File", "SizeMB", "Type", "MethylationHint", "Loadable", "Path"), drop = FALSE]
    display$Size <- vapply(display$SizeMB * 1024^2, ugplot_format_bytes, character(1))
    display <- display[, c("File", "Size", "Type", "MethylationHint", "Loadable", "Path"), drop = FALSE]
    DT::datatable(display, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$geo_download_summary <- renderUI({
    remote_files <- geo_remote_files()
    local_files <- geo_files()
    if (!is.data.frame(remote_files) || nrow(remote_files) == 0) {
      if (is.data.frame(local_files) && nrow(local_files) > 0) {
        return(tags$div(
          tags$p(paste0("Local files: ", nrow(local_files), " file(s), ", ugplot_format_bytes(sum(local_files$SizeMB, na.rm = TRUE) * 1024^2))),
          tags$p(paste0("Loadable tables: ", sum(local_files$Loadable))),
          tags$p(paste0("Folder: ", unique(dirname(local_files$Path))[1]))
        ))
      }
      return(tags$p("Inspect a GEO accession to preview supplementary files before downloading."))
    }
    if (!"NeedsDownload" %in% names(remote_files)) {
      remote_files$NeedsDownload <- TRUE
    }
    known_size <- sum(ugplot_geo_size_bytes(remote_files), na.rm = TRUE)
    unknown_size_n <- sum(is.na(ugplot_geo_size_bytes(remote_files)))
    source <- input$geo_matrix_source %||% "processed"
    processed_files <- remote_files[remote_files$Loadable, , drop = FALSE]
    processed_pending <- processed_files[processed_files$NeedsDownload, , drop = FALSE]
    raw_files <- remote_files[remote_files$Type %in% c("IDAT", "archive"), , drop = FALSE]
    raw_pending <- raw_files[raw_files$NeedsDownload, , drop = FALSE]
    selected_files <- geo_download_selection(remote_files, source)
    selected_pending <- selected_files[selected_files$NeedsDownload, , drop = FALSE]
    tags$div(
      tags$p(paste0("Found: ", nrow(remote_files), " file(s), ", ugplot_format_bytes(known_size), if (unknown_size_n > 0) paste0(" + ", unknown_size_n, " unknown-size file(s)") else "")),
      if (identical(source, "raw_sesame")) {
        tags$p(paste0("Raw IDAT/reprocessing files selected: ", nrow(raw_files), " file(s), ", ugplot_format_bytes(sum(ugplot_geo_size_bytes(raw_files), na.rm = TRUE)), "."))
      } else {
        tags$p(paste0("Processed matrices selected: ", nrow(processed_files), " file(s), ", ugplot_format_bytes(sum(ugplot_geo_size_bytes(processed_files), na.rm = TRUE)), "."))
      },
      tags$p(paste0("Still needed for selected workflow: ", nrow(selected_pending), " file(s), ", ugplot_format_bytes(sum(ugplot_geo_size_bytes(selected_pending), na.rm = TRUE)), ".")),
      if (identical(source, "raw_sesame") && nrow(raw_files) == 0) {
        tags$p("No raw IDAT/archive supplementary file was detected for this GEO accession.")
      } else NULL,
      tags$p(paste0("Folder: ", ugplot_geo_cache_dir(trimws(input$geo_accession %||% "GEO"))))
    )
  })

  output$geo_source_status_summary <- renderUI({
    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      return(NULL)
    }
    cache_dir <- ugplot_geo_cache_dir(accession)
    target_column <- input$geo_target_column %||% ""
    source <- input$geo_matrix_source %||% "processed"
    remote_result <- remote_job_preview_result()
    remote_loaded <- geo_remote_mode_active() && is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline")
    remote_paths <- if (remote_loaded) remote_result$paths else list()
    remote_matrix_files <- as.character(unlist(remote_paths$matrix_files %||% character(0), use.names = FALSE))
    remote_matrix_files <- remote_matrix_files[nzchar(remote_matrix_files)]
    remote_source <- remote_result$matrix_source %||% source
    source_rows <- lapply(c("processed", "raw_sesame"), function(source_i) {
      matrix_files <- ugplot_geo_matrix_files(cache_dir, source = source_i)
      remote_matrix_count <- if (remote_loaded && identical(remote_source, source_i)) length(remote_matrix_files) else 0L
      spearman_done <- FALSE
      transcript_done <- FALSE
      cache_path <- ""
      if (nzchar(target_column)) {
        paths <- geo_spearman_cache_paths(cache_dir, target_column, source = source_i, create = FALSE)
        spearman_done <- file.exists(paths$raw)
        threshold <- suppressWarnings(as.numeric(input$geo_transcript_absrho_threshold %||% 0.8))
        min_samples <- suppressWarnings(as.numeric(input$geo_transcript_min_samples %||% 80))
        group_paths <- geo_transcript_group_cache_paths(cache_dir, target_column, threshold, min_samples, source = source_i, create = FALSE)
        transcript_done <- file.exists(group_paths$summary)
        cache_path <- geo_analysis_cache_dir(cache_dir, source_i, create = FALSE)
      }
      status <- paste0(
        geo_matrix_source_label(source_i), ": ",
        length(matrix_files), " local matrix file(s)",
        if (remote_matrix_count > 0) paste0("; ", remote_matrix_count, " remote matrix file(s)") else "",
        "; ",
        if (spearman_done) "Spearman ready" else "Spearman pending",
        "; ",
        if (transcript_done) "transcripts ready" else "transcripts pending"
      )
      tags$p(
        style = if (identical(source_i, source)) "font-weight: 700; margin: 0 0 3px 0;" else "margin: 0 0 3px 0;",
        status,
        if (nzchar(cache_path) && dir.exists(cache_path)) tags$span(class = "geo-step-note", paste0(" (", cache_path, ")")) else NULL
      )
    })
    tags$div(class = "geo-status-card",
      tags$p(style = "margin: 0 0 5px 0;", tags$strong("Active path: "), geo_matrix_source_label(source)),
      source_rows,
      if (remote_loaded) {
        tags$div(
          style = "margin-top: 8px;",
          tags$p(style = "margin: 0 0 3px 0;", tags$strong("Remote cache: "), remote_result$cache_dir %||% ""),
          if (length(remote_matrix_files) > 0) {
            tags$p(style = "margin: 0 0 3px 0;", paste0("Remote matrix file: ", paste(basename(remote_matrix_files), collapse = ", ")))
          } else {
            tags$p(style = "margin: 0 0 3px 0;", "No remote matrix file is recorded in the loaded remote result.")
          },
          tags$p(style = "margin: 0;", paste0("Remote stage: ", remote_result$stage %||% "unknown"))
        )
      } else NULL
    )
  })

  output$geo_metadata_summary <- renderUI({
    metadata <- geo_sample_metadata()
    accession <- trimws(input$geo_accession %||% "GEO")
    cache_dir <- ugplot_geo_cache_dir(accession)
    if (!is.data.frame(metadata) || nrow(metadata) == 0) {
      cached_path <- ugplot_geo_sample_metadata_path(cache_dir, "rds")
      if (file.exists(cached_path)) {
        return(tags$div(
          tags$p("Cached sample metadata is available locally."),
          tags$p(paste0("File: ", cached_path))
        ))
      }
      return(tags$p("Fetch sample metadata before building an analysis table."))
    }
    likely_targets <- grep("age|sex|gender|disease|status|treatment|response|case|control|group|phenotype", names(metadata), value = TRUE, ignore.case = TRUE)
    tags$div(
      tags$p(paste0("Samples: ", nrow(metadata), "; metadata columns: ", ncol(metadata), ".")),
      if (length(likely_targets) > 0) {
        tags$p(paste0("Likely analysis fields: ", paste(utils::head(likely_targets, 8), collapse = ", "), if (length(likely_targets) > 8) "..." else ""))
      } else {
        tags$p("No obvious phenotype field detected yet; inspect the table.")
      },
      tags$p(paste0("Saved in: ", cache_dir))
    )
  })

  output$geo_file_selector <- renderUI({
    files <- geo_files()
    if (!is.data.frame(files) || nrow(files) == 0) {
      return(tags$p("No GEO files loaded yet."))
    }
    loadable <- files[files$Loadable, , drop = FALSE]
    if (nrow(loadable) == 0) {
      return(tags$p("No directly loadable processed table found. IDAT files require a separate preprocessing pipeline."))
    }
    large_loadable <- loadable[loadable$SizeMB > 500, , drop = FALSE]
    if (nrow(large_loadable) > 0) {
      return(tags$div(
        tags$p("Downloaded/extracted GEO matrices are too large to load directly into ugPlot."),
        tags$p("Use the extraction step first; the next ugPlot step should summarize/subset these matrices before loading.")
      ))
    }
    choices <- stats::setNames(seq_len(nrow(loadable)), paste0(loadable$File, " (", loadable$SizeMB, " MB)"))
    selectInput("geo_selected_file", "Processed methylation table:", choices = choices, selected = choices[[1]])
  })

  render_geo_table_title <- function(title, subtitle = NULL) {
    tags$div(class = "geo-table-title",
      tags$h4(title),
      if (!is.null(subtitle) && nzchar(subtitle)) tags$p(class = "geo-step-note", subtitle) else NULL
    )
  }

  output$geo_metadata_table_title <- renderUI({
    metadata <- geo_sample_metadata()
    req(is.data.frame(metadata), nrow(metadata) > 0)
    render_geo_table_title(
      "Sample metadata",
      "Phenotype/sample columns from GEO. These fields define targets such as age, sex, tissue, treatment, or response."
    )
  })

  output$geo_files_table_title <- renderUI({
    remote_files <- geo_remote_files()
    files <- geo_files()
    req((is.data.frame(remote_files) && nrow(remote_files) > 0) || (is.data.frame(files) && nrow(files) > 0))
    render_geo_table_title(
      "GEO files",
      "Supplementary files detected for the GEO accession, including local download/extraction status."
    )
  })

  output$geo_annotation_table_title <- renderUI({
    annotation_map <- geo_cpg_annotation()
    req(is.data.frame(annotation_map), nrow(annotation_map) > 0)
    render_geo_table_title(
      "CpG annotation map",
      "Many-to-many mapping from CpG probes to genes and transcripts using the Illumina annotation package."
    )
  })

  output$geo_spearman_table_title <- renderUI({
    results <- geo_spearman_results()
    req(is.data.frame(results), nrow(results) > 0)
    render_geo_table_title(
      "CpG Spearman results",
      "CpG-level correlation against the selected metadata field. If annotation is loaded, CpGs may appear in multiple rows because one CpG can map to multiple transcripts."
    )
  })

  geo_cpg_distribution_ui <- function(absrho = NULL, threshold, histogram = NULL) {
    if (is.data.frame(histogram) && nrow(histogram) > 0 && all(c("BinMin", "BinMax", "Count") %in% names(histogram))) {
      bin_min <- suppressWarnings(as.numeric(histogram$BinMin))
      bin_max <- suppressWarnings(as.numeric(histogram$BinMax))
      counts <- suppressWarnings(as.integer(histogram$Count))
    } else {
      absrho <- suppressWarnings(as.numeric(absrho))
      absrho <- absrho[is.finite(absrho)]
      if (length(absrho) == 0) {
        return(NULL)
      }
      breaks <- seq(0, 1, by = 0.05)
      bins <- cut(pmax(0, pmin(1, absrho)), breaks = breaks, include.lowest = TRUE, right = TRUE)
      counts <- as.integer(table(factor(bins, levels = levels(bins))))
      bin_min <- utils::head(breaks, -1)
      bin_max <- utils::tail(breaks, -1)
    }
    max_count <- max(counts, na.rm = TRUE)
    if (!is.finite(max_count) || max_count <= 0) {
      max_count <- 1L
    }
    threshold <- suppressWarnings(as.numeric(threshold))
    if (!is.finite(threshold)) {
      threshold <- 0.8
    }
    bars <- lapply(seq_along(counts), function(i) {
      current_min <- bin_min[[i]]
      current_max <- bin_max[[i]]
      active <- current_max >= threshold
      height <- max(4, round(58 * counts[[i]] / max_count))
      tags$div(
        title = paste0(sprintf("%.2f", current_min), "-", sprintf("%.2f", current_max), ": ", counts[[i]], " CpG(s)"),
        style = "display: flex; flex-direction: column; align-items: center; justify-content: flex-end; flex: 1 1 0; min-width: 8px;",
        tags$div(style = paste0(
          "width: 100%; height: ", height, "px; border-radius: 3px 3px 0 0; ",
          "background: ", if (isTRUE(active)) "#2e9d4d" else "#b8c7d9", ";"
        )),
        if (abs(round(current_min * 10) - current_min * 10) < 1e-8) {
          tags$span(style = "font-size: 9px; color: #5d6b7a; margin-top: 3px;", sprintf("%.1f", current_min))
        } else {
          tags$span(style = "font-size: 9px; color: transparent; margin-top: 3px;", ".")
        }
      )
    })
    tags$div(
      style = "margin: 10px 0 12px 0; padding: 8px 10px; border: 1px solid #dbe7f3; background: #f8fbff; border-radius: 4px;",
      tags$p(style = "margin: 0 0 6px 0; font-weight: 700;", "CpG |rho| distribution"),
      tags$div(style = "height: 82px; display: flex; align-items: flex-end; gap: 4px;", bars),
      tags$p(style = "margin: 4px 0 0 0; font-size: 12px; color: #5d6b7a;", "Bin width is 0.05. Bars at or above the selected threshold are highlighted.")
    )
  }

  remote_geo_cpg_summary_context <- function(threshold = NULL) {
    job_id <- trimws(as.character(geo_remote_pipeline_job_id() %||% input$remote_job_id %||% ""))
    threshold <- suppressWarnings(as.numeric(threshold %||% input$geo_transcript_absrho_threshold %||% 0.8))
    if (!is.finite(threshold)) {
      threshold <- 0.8
    }
    spearman_min_samples <- suppressWarnings(as.numeric(input$geo_spearman_min_samples %||% 80))
    if (!is.finite(spearman_min_samples)) {
      spearman_min_samples <- 80
    }
    spearman_min_samples <- max(0, min(100, spearman_min_samples))
    server <- selected_geo_remote_server()
    server_url <- as.character(server$url[[1]] %||% "")
    list(
      key = paste(server_url, job_id, threshold, spearman_min_samples, sep = "\r"),
      job_id = job_id,
      threshold = threshold,
      spearman_min_samples = spearman_min_samples,
      server = server
    )
  }

  remote_geo_cpg_summary_key <- reactive({
    remote_result <- remote_job_preview_result()
    remote_loaded <- identical(geo_run_target_state(), "remote") &&
      is.list(remote_result) &&
      identical(remote_result$kind %||% "", "geo_pipeline")
    if (!isTRUE(remote_loaded)) {
      return("")
    }
    context <- remote_geo_cpg_summary_context()
    if (!nzchar(context$job_id)) {
      return("")
    }
    context$key
  })

  remote_geo_cpg_summary_key_debounced <- shiny::debounce(remote_geo_cpg_summary_key, 800)

  observeEvent(remote_geo_cpg_summary_key_debounced(), {
    key <- remote_geo_cpg_summary_key_debounced()
    if (!nzchar(key)) {
      return()
    }
    state <- geo_remote_cpg_summary()
    if (is.list(state) &&
        identical(state$key %||% "", key) &&
        ((state$status %||% "") %in% c("loading", "complete", "failed"))) {
      return()
    }
    context <- remote_geo_cpg_summary_context()
    if (!nzchar(context$job_id)) {
      return()
    }
    geo_remote_cpg_summary(list(
      key = key,
      data = NULL,
      status = "loading",
      message = "Loading full server CpG counts..."
    ))
    tryCatch({
      summary <- ugplot_remote_geo_cpg_summary(
        server_url = context$server$url,
        job_id = context$job_id,
        threshold = context$threshold,
        spearman_min_samples_pct = context$spearman_min_samples,
        bin_width = 0.05,
        token = context$server$token %||% ""
      )
      geo_remote_cpg_summary(list(
        key = key,
        data = summary,
        status = "complete",
        message = "Full server CpG counts loaded."
      ))
    }, error = function(e) {
      geo_remote_cpg_summary(list(
        key = key,
        data = NULL,
        status = "failed",
        message = conditionMessage(e)
      ))
    })
  }, ignoreInit = FALSE)

  geo_lookup_rows <- function(value) {
    if (is.data.frame(value)) {
      return(value)
    }
    if (is.list(value) && length(value) > 0) {
      out <- tryCatch(as.data.frame(value, stringsAsFactors = FALSE), error = function(e) data.frame())
      return(out)
    }
    data.frame()
  }

  geo_local_cpg_lookup <- function(cpg, threshold) {
    results <- geo_spearman_raw_results()
    annotation_map <- geo_cpg_annotation()
    cpg <- trimws(as.character(cpg %||% ""))
    raw <- if (is.data.frame(results) && nrow(results) > 0 && "CpG" %in% names(results)) {
      results[tolower(as.character(results$CpG)) == tolower(cpg), , drop = FALSE]
    } else {
      data.frame()
    }
    rownames(raw) <- NULL
    min_samples <- NA_integer_
    if (is.data.frame(results) && nrow(results) > 0 && "N" %in% names(results)) {
      n_values <- suppressWarnings(as.numeric(results$N))
      max_n <- suppressWarnings(max(n_values, na.rm = TRUE))
      if (is.finite(max_n) && max_n > 0) {
        min_samples <- max(3L, ceiling(max_n * geo_spearman_min_samples_pct() / 100))
      }
    }
    if (nrow(raw) > 0) {
      raw_n <- suppressWarnings(as.numeric(raw$N %||% NA_real_))
      raw_absrho <- suppressWarnings(as.numeric(raw$AbsRho %||% NA_real_))
      raw$PassesSampleFilter <- is.finite(raw_n) & is.finite(min_samples) & raw_n >= min_samples
      raw$PassesCurrentThreshold <- is.finite(raw_absrho) & raw_absrho >= threshold
      raw$PassesLoadedThreshold <- raw$PassesCurrentThreshold
    }
    annotated <- if (is.data.frame(annotation_map) && nrow(annotation_map) > 0 && "CpG" %in% names(annotation_map)) {
      annotation_map[tolower(as.character(annotation_map$CpG)) == tolower(cpg), , drop = FALSE]
    } else {
      data.frame()
    }
    rownames(annotated) <- NULL
    genes <- if (is.data.frame(annotated) && nrow(annotated) > 0 && "Gene" %in% names(annotated)) {
      sort(unique(trimws(as.character(stats::na.omit(annotated$Gene)))))
    } else {
      character(0)
    }
    genes <- genes[nzchar(genes)]
    transcripts <- if (is.data.frame(annotated) && nrow(annotated) > 0 && "Transcript" %in% names(annotated)) {
      sort(unique(trimws(as.character(stats::na.omit(annotated$Transcript)))))
    } else {
      character(0)
    }
    transcripts <- transcripts[nzchar(transcripts)]
    list(
      kind = "geo_cpg_lookup",
      cpg = cpg,
      threshold = threshold,
      loaded_threshold = threshold,
      min_samples = min_samples,
      present_in_spearman = nrow(raw) > 0,
      present_in_annotation = nrow(annotated) > 0,
      present_in_transcript_groups = FALSE,
      genes = genes,
      transcripts = transcripts,
      raw = raw,
      annotated = annotated,
      transcript_group_details = data.frame()
    )
  }

  observeEvent(input$geo_cpg_lookup_run, {
    cpg <- trimws(as.character(input$geo_cpg_lookup_id %||% ""))
    if (!nzchar(cpg)) {
      geo_cpg_lookup_state(list(key = "", data = NULL, status = "failed", message = "Enter a CpG id."))
      return()
    }
    threshold <- suppressWarnings(as.numeric(input$geo_transcript_absrho_threshold %||% 0.8))
    if (!is.finite(threshold)) {
      threshold <- 0.8
    }
    spearman_min_samples <- suppressWarnings(as.numeric(input$geo_spearman_min_samples %||% 80))
    if (!is.finite(spearman_min_samples)) {
      spearman_min_samples <- 80
    }
    remote_result <- remote_job_preview_result()
    remote_loaded <- identical(geo_run_target_state(), "remote") &&
      is.list(remote_result) &&
      identical(remote_result$kind %||% "", "geo_pipeline")
    if (isTRUE(remote_loaded)) {
      context <- remote_geo_cpg_summary_context(threshold)
      key <- paste(context$key, tolower(cpg), sep = "\r")
      geo_cpg_lookup_state(list(key = key, data = NULL, status = "loading", message = "Loading CpG lookup..."))
      tryCatch({
        lookup <- ugplot_remote_geo_cpg_lookup(
          server_url = context$server$url,
          job_id = context$job_id,
          cpg = cpg,
          threshold = threshold,
          spearman_min_samples_pct = spearman_min_samples,
          token = context$server$token %||% ""
        )
        geo_cpg_lookup_state(list(key = key, data = lookup, status = "complete", message = "CpG lookup loaded."))
      }, error = function(e) {
        geo_cpg_lookup_state(list(key = key, data = NULL, status = "failed", message = conditionMessage(e)))
      })
    } else {
      lookup <- geo_local_cpg_lookup(cpg, threshold)
      geo_cpg_lookup_state(list(key = paste("local", tolower(cpg), threshold, sep = "\r"), data = lookup, status = "complete", message = "CpG lookup loaded."))
    }
  }, ignoreInit = TRUE)

  output$geo_cpg_lookup_ui <- renderUI({
    state <- geo_cpg_lookup_state()
    status <- state$status %||% "idle"
    if (identical(status, "idle")) {
      return(tags$p(class = "geo-step-note", "Enter a CpG id to inspect its Spearman value, annotation, and transcript-group status."))
    }
    if (identical(status, "loading")) {
      return(tags$p(class = "geo-step-note", state$message %||% "Loading CpG lookup..."))
    }
    if (identical(status, "failed")) {
      return(tags$p(class = "geo-step-note", paste0("CpG lookup failed: ", state$message %||% "unknown error")))
    }
    lookup <- state$data
    if (!is.list(lookup)) {
      return(tags$p(class = "geo-step-note", "CpG lookup returned no data."))
    }
    raw <- geo_lookup_rows(lookup$raw)
    annotated <- geo_lookup_rows(lookup$annotated)
    group_details <- geo_lookup_rows(lookup$transcript_group_details)
    transcript_progress <- geo_lookup_rows(lookup$transcript_progress)
    genes <- as.character(unlist(lookup$genes %||% character(0), use.names = FALSE))
    genes <- genes[nzchar(genes)]
    transcripts <- as.character(unlist(lookup$transcripts %||% character(0), use.names = FALSE))
    transcripts <- transcripts[nzchar(transcripts)]
    raw_line <- if (nrow(raw) > 0) {
      raw_value <- function(column, default = NA) {
        if (column %in% names(raw) && length(raw[[column]]) > 0) raw[[column]][[1]] else default
      }
      rho <- suppressWarnings(as.numeric(raw_value("SpearmanRho", NA_real_)))
      absrho <- suppressWarnings(as.numeric(raw_value("AbsRho", NA_real_)))
      n_value <- suppressWarnings(as.numeric(raw_value("N", NA_real_)))
      paste0(
        "Spearman rho=", if (is.finite(rho)) signif(rho, 5) else "NA",
        "; |rho|=", if (is.finite(absrho)) signif(absrho, 5) else "NA",
        "; N=", if (is.finite(n_value)) format(n_value, big.mark = ",") else "NA",
        "; sample filter=", if (isTRUE(raw_value("PassesSampleFilter", FALSE))) "pass" else "fail",
        "; current threshold=", if (isTRUE(raw_value("PassesCurrentThreshold", FALSE))) "pass" else "fail",
        "; loaded threshold=", if (isTRUE(raw_value("PassesLoadedThreshold", FALSE))) "pass" else "fail",
        "."
      )
    } else {
      "CpG was not found in the loaded Spearman cache."
    }
    group_line <- if (nrow(group_details) > 0) {
      group_ids <- if ("GroupID" %in% names(group_details)) unique(as.character(group_details$GroupID)) else character(0)
      group_ids <- group_ids[nzchar(group_ids)]
      if (length(group_ids) > 0) {
        paste0("Present in transcript group details: ", paste(group_ids, collapse = ", "), ".")
      } else {
        "Present in transcript group details."
      }
    } else {
      "Not present in the loaded transcript group details."
    }
    tags$div(
      style = "margin-top: 8px;",
      tags$p(style = "margin: 0 0 4px 0;", tags$strong(as.character(lookup$cpg %||% "")), ": ", raw_line),
      tags$p(style = "margin: 0 0 4px 0;", "Genes: ", if (length(genes) > 0) paste(utils::head(genes, 12), collapse = "; ") else "none in loaded annotation"),
      tags$p(style = "margin: 0 0 4px 0;", "Transcripts: ", if (length(transcripts) > 0) paste(utils::head(transcripts, 12), collapse = "; ") else "none in loaded annotation"),
      tags$p(style = "margin: 0 0 4px 0;", group_line),
      if (nzchar(as.character(lookup$transcript_diagnostic %||% ""))) {
        tags$p(
          style = "margin: 0 0 4px 0; font-weight: 600;",
          "Transcript pipeline: ",
          as.character(lookup$transcript_diagnostic)
        )
      } else {
        NULL
      },
      if (nrow(transcript_progress) > 0) {
        tags$p(
          class = "geo-step-note",
          paste(
            apply(transcript_progress, 1, function(row) {
              paste0(
                row[["Transcript"]] %||% "",
                ": status=", row[["Status"]] %||% "",
                "; retained samples=", row[["Samples"]] %||% 0,
                "; retained CpGs=", row[["Columns"]] %||% 0
              )
            }),
            collapse = " | "
          )
        )
      } else {
        NULL
      },
      if (nrow(annotated) > 0) {
        tags$p(class = "geo-step-note", paste0("Annotation rows for this CpG: ", nrow(annotated), "."))
      } else {
        NULL
      }
    )
  })

  output$geo_spearman_summary <- renderUI({
    results <- geo_spearman_raw_results()
    if (!is.data.frame(results) || nrow(results) == 0) {
      return(tags$p(class = "geo-step-note", "No CpG Spearman result is loaded yet."))
    }
    filtered <- geo_filter_spearman_min_samples(results)
    threshold <- suppressWarnings(as.numeric(input$geo_transcript_absrho_threshold %||% 0.8))
    if (!is.finite(threshold)) {
      threshold <- 0.8
    }
    absrho <- suppressWarnings(as.numeric(filtered$AbsRho))
    rho <- suppressWarnings(as.numeric(filtered$SpearmanRho))
    trigger_rows <- filtered[is.finite(absrho) & absrho >= threshold, , drop = FALSE]
    positive_rows <- filtered[is.finite(rho) & rho >= threshold, , drop = FALSE]
    negative_rows <- filtered[is.finite(rho) & rho <= -threshold, , drop = FALSE]
    annotation_map <- geo_cpg_annotation()
    annotation_line <- NULL
    annotated <- data.frame()
    run_remote <- identical(geo_run_target_state(), "remote")
    remote_result <- remote_job_preview_result()
    remote_loaded <- isTRUE(run_remote) &&
      is.list(remote_result) &&
      identical(remote_result$kind %||% "", "geo_pipeline")
    remote_spearman_preview <- isTRUE(remote_loaded) &&
      is.data.frame(remote_result$tables$spearman_preview) &&
      nrow(remote_result$tables$spearman_preview) == nrow(results)
    cpg_summary_context <- if (isTRUE(remote_loaded)) remote_geo_cpg_summary_context(threshold) else list(key = "")
    cpg_summary_state <- geo_remote_cpg_summary()
    cpg_summary_status <- if (is.list(cpg_summary_state) && identical(cpg_summary_state$key %||% "", cpg_summary_context$key)) {
      cpg_summary_state$status %||% "idle"
    } else {
      "idle"
    }
    cpg_summary <- if (identical(cpg_summary_status, "complete") && is.list(cpg_summary_state$data)) {
      cpg_summary_state$data
    } else {
      NULL
    }
    has_full_cpg_summary <- is.list(cpg_summary) && identical(cpg_summary$kind %||% "", "geo_cpg_summary")
    cpg_summary_count <- function(field, fallback) {
      value <- suppressWarnings(as.numeric(cpg_summary[[field]] %||% NA_real_))
      if (isTRUE(has_full_cpg_summary) && length(value) > 0 && is.finite(value[[1]])) {
        return(as.integer(round(value[[1]])))
      }
      fallback
    }
    cpg_summary_number <- function(field, fallback) {
      value <- suppressWarnings(as.numeric(cpg_summary[[field]] %||% NA_real_))
      if (isTRUE(has_full_cpg_summary) && length(value) > 0 && is.finite(value[[1]])) {
        return(value[[1]])
      }
      fallback
    }
    scanned_count <- cpg_summary_count("spearman_total_cpgs", nrow(results))
    filtered_count <- cpg_summary_count("spearman_pass_filter_cpgs", nrow(filtered))
    trigger_count <- cpg_summary_count("threshold_cpgs", nrow(trigger_rows))
    positive_count <- cpg_summary_count("positive_cpgs", nrow(positive_rows))
    negative_count <- cpg_summary_count("negative_cpgs", nrow(negative_rows))
    max_absrho <- cpg_summary_number("max_absrho", suppressWarnings(max(absrho, na.rm = TRUE)))
    max_text <- if (is.finite(max_absrho)) signif(max_absrho, 4) else "NA"
    max_pos <- cpg_summary_number("max_rho", suppressWarnings(max(rho, na.rm = TRUE)))
    min_neg <- cpg_summary_number("min_rho", suppressWarnings(min(rho, na.rm = TRUE)))
    max_pos_text <- if (is.finite(max_pos)) signif(max_pos, 4) else "NA"
    min_neg_text <- if (is.finite(min_neg)) signif(min_neg, 4) else "NA"
    cpg_summary_note <- NULL
    if (identical(cpg_summary_status, "loading")) {
      cpg_summary_note <- tags$p(class = "geo-step-note", "Loading full server CpG counts...")
    } else if (identical(cpg_summary_status, "failed")) {
      cpg_summary_note <- tags$p(
        class = "geo-step-note",
        paste0("Full server CpG counts unavailable: ", cpg_summary_state$message %||% "unknown error", ". Showing loaded preview.")
      )
    }
    remote_threshold <- if (is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline")) {
      suppressWarnings(as.numeric(remote_result$settings$transcript_absrho_threshold %||% NA_real_))
    } else {
      NA_real_
    }
    loaded_threshold <- cpg_summary_number("loaded_threshold", remote_threshold)
    threshold_changed <- is.finite(loaded_threshold) &&
      is.finite(threshold) &&
      !isTRUE(all.equal(loaded_threshold, threshold, tolerance = 1e-8))
    loaded_threshold_count <- cpg_summary_count("loaded_threshold_cpgs", if (isTRUE(threshold_changed)) NA_integer_ else trigger_count)
    newly_included_count <- cpg_summary_count("newly_included_cpgs", NA_integer_)
    excluded_loaded_count <- cpg_summary_count("excluded_loaded_cpgs", NA_integer_)
    threshold_delta_note <- NULL
    if (isTRUE(has_full_cpg_summary) && is.finite(loaded_threshold) && is.finite(loaded_threshold_count)) {
      if (isTRUE(threshold_changed) && threshold < loaded_threshold) {
        threshold_delta_note <- tags$p(paste0(
          "Loaded threshold |rho| >= ", loaded_threshold, " covers ",
          format(loaded_threshold_count, big.mark = ","), " CpG(s); current threshold |rho| >= ",
          threshold, " contains ", format(trigger_count, big.mark = ","),
          " CpG(s), adding ", format(newly_included_count, big.mark = ","), " new CpG(s)."
        ))
      } else if (isTRUE(threshold_changed) && threshold > loaded_threshold) {
        threshold_delta_note <- tags$p(paste0(
          "Loaded threshold |rho| >= ", loaded_threshold, " covers ",
          format(loaded_threshold_count, big.mark = ","), " CpG(s); current threshold |rho| >= ",
          threshold, " keeps ", format(trigger_count, big.mark = ","),
          " CpG(s), excluding ", format(excluded_loaded_count, big.mark = ","), " CpG(s) from the loaded threshold."
        ))
      } else {
        threshold_delta_note <- tags$p(paste0(
          "Loaded threshold |rho| >= ", loaded_threshold, " covers ",
          format(loaded_threshold_count, big.mark = ","), " CpG(s)."
        ))
      }
    }
    preview_prefix <- if (isTRUE(remote_spearman_preview)) "In the loaded Spearman preview" else "At current threshold"
    if (is.data.frame(annotation_map) && nrow(annotation_map) > 0 && nrow(trigger_rows) > 0) {
      annotated <- annotation_map[
        annotation_map$CpG %in% unique(trigger_rows$CpG) &
          !is.na(annotation_map$Transcript) &
          nzchar(as.character(annotation_map$Transcript)),
        ,
        drop = FALSE
      ]
      annotation_line <- tags$p(paste0(
        preview_prefix, ": ", length(unique(annotated$CpG)), " annotated CpG(s), ",
        nrow(annotated), " CpG-transcript link(s), ",
        length(unique(annotated$Transcript)), " transcript(s)."
      ))
    } else if (is.data.frame(annotation_map) && nrow(annotation_map) > 0) {
      annotation_line <- tags$p(paste0(preview_prefix, ": 0 annotated CpGs/transcripts."))
    }
    continue_action <- NULL
    transcript_candidate_count <- if (isTRUE(has_full_cpg_summary)) {
      trigger_count
    } else if (is.data.frame(annotation_map) && nrow(annotation_map) > 0) {
      length(unique(as.character(annotated$Transcript %||% character(0))))
    } else {
      trigger_count
    }
    if (isTRUE(run_remote) && transcript_candidate_count > 0) {
      action_title <- if (isTRUE(threshold_changed)) "Start cached threshold run" else "Ready to continue transcript pipeline"
      count_label <- if (isTRUE(has_full_cpg_summary)) {
        if (isTRUE(threshold_changed) && threshold < loaded_threshold && is.finite(loaded_threshold_count)) {
          paste0(
            "the loaded threshold covers ", format(loaded_threshold_count, big.mark = ","),
            " CpG(s); the current threshold contains ", format(trigger_count, big.mark = ","),
            " CpG(s), adding ", format(newly_included_count, big.mark = ","), " new CpG(s)."
          )
        } else if (isTRUE(threshold_changed) && threshold > loaded_threshold && is.finite(loaded_threshold_count)) {
          paste0(
            "the loaded threshold covers ", format(loaded_threshold_count, big.mark = ","),
            " CpG(s); the current threshold keeps ", format(trigger_count, big.mark = ","),
            " CpG(s), excluding ", format(excluded_loaded_count, big.mark = ","), " CpG(s)."
          )
        } else {
          paste0(
            "the full server cache contains ", format(trigger_count, big.mark = ","),
            " CpG(s) at this threshold."
          )
        }
      } else if (isTRUE(remote_spearman_preview)) {
        paste0(
          "the loaded preview contains ", format(trigger_count, big.mark = ","),
          " CpG(s) at this threshold. The new remote run will use the full cached Spearman file."
        )
      } else {
        paste0(
          "keeps ", format(trigger_count, big.mark = ","),
          " CpG(s)."
        )
      }
      action_text <- if (isTRUE(threshold_changed)) {
        paste0(
          "Loaded job used |rho| >= ", loaded_threshold,
          "; current threshold is |rho| >= ", threshold,
          " and ", count_label,
          " This will create a new remote job and leave the loaded job unchanged."
        )
      } else {
        paste0(
          "Current threshold |rho| >= ", threshold,
          if (isTRUE(remote_spearman_preview)) "; " else " ",
          count_label,
          " Continue will reuse the remote GEO cache where available."
        )
      }
      continue_action <- tags$div(
        style = paste0(
          "margin: 10px 0; padding: 10px 12px; border-left: 5px solid #2e9d4d;",
          "background: #e8f7eb; color: #1f6f37; border-radius: 4px;"
        ),
        tags$p(style = "margin: 0 0 8px 0; font-weight: 700;", action_title),
        tags$p(style = "margin: 0 0 8px 0;", action_text),
        tags$button(
          type = "button",
          id = "geo_continue_remote_pipeline",
          class = "btn btn-success btn-sm",
          onclick = "if (window.Shiny) Shiny.setInputValue('geo_continue_remote_pipeline_click', Date.now(), {priority: 'event'});",
          if (isTRUE(threshold_changed)) "Start cached remote run" else "Continue remote pipeline"
        )
      )
    }
    threshold_warning <- NULL
    if (filtered_count > 0 && transcript_candidate_count == 0) {
      no_candidate_message <- if (trigger_count == 0) {
        paste0(
          "Current threshold |rho| >= ", threshold,
          " is above the observed range. Max |rho| is ", max_text,
          ", so no transcript candidates can be built."
        )
      } else {
        paste0(
          "Current threshold |rho| >= ", threshold,
          " keeps CpGs, but none map to an annotated transcript candidate."
        )
      }
      threshold_warning <- tags$div(
        style = paste0(
          "margin: 10px 0; padding: 10px 12px; border-left: 5px solid #d9534f;",
          "background: #fdecec; color: #7f1d1d; border-radius: 4px;"
        ),
        tags$p(style = "margin: 0 0 6px 0; font-weight: 700;", "Transcript pipeline stopped at Step 6"),
        tags$p(style = "margin: 0 0 8px 0;", no_candidate_message),
        tags$p(style = "margin: 0 0 8px 0;",
          paste0("Lower the threshold. The continue button appears when the current value produces transcript candidates.")
        )
      )
    }
    totals_label <- if (isTRUE(has_full_cpg_summary)) {
      "Spearman full cache: "
    } else if (isTRUE(remote_spearman_preview)) {
      "Spearman preview: "
    } else {
      "Spearman totals: "
    }
    totals_suffix <- if (isTRUE(has_full_cpg_summary)) {
      ""
    } else if (isTRUE(remote_spearman_preview)) {
      " shown from the loaded remote result; full cached Spearman is evaluated on the remote server when you continue."
    } else {
      ""
    }
    tags$div(class = "geo-step-status",
      tags$p(tags$strong(totals_label), paste0(
        format(scanned_count, big.mark = ","), " CpG(s) scanned; ",
        format(filtered_count, big.mark = ","), " pass the sample filter; max |rho| ", max_text, ".",
        totals_suffix
      )),
      cpg_summary_note,
      threshold_delta_note,
      geo_cpg_distribution_ui(absrho, threshold, histogram = cpg_summary$histogram %||% NULL),
      threshold_warning,
      continue_action,
      tags$p(paste0(
        "|rho| >= ", threshold, ": ",
        format(trigger_count, big.mark = ","), " CpG(s)."
      )),
      tags$p(paste0(
        "rho >= +", threshold, ": ", format(positive_count, big.mark = ","),
        " CpG(s); rho <= -", threshold, ": ", format(negative_count, big.mark = ","),
        " CpG(s). Range: ", min_neg_text, " to +", max_pos_text, "."
      )),
      annotation_line
    )
  })

  output$geo_transcript_candidates_table_title <- renderUI({
    groups <- geo_transcript_groups()
    req(is.data.frame(groups), nrow(groups) > 0)
    threshold <- suppressWarnings(as.numeric(input$geo_transcript_absrho_threshold %||% 0.8))
    min_samples <- suppressWarnings(as.numeric(input$geo_transcript_min_samples %||% 80))
    render_geo_table_title(
      "Transcript ML candidate groups",
      paste0(
        "Transcripts with at least one CpG |rho| >= ", threshold,
        " and a complete-case dataset retaining at least ", min_samples,
        "% samples. Transcripts with identical final CpGs and samples are grouped to avoid repeated ML runs."
      )
    )
  })

  output$geo_idat_qc_summary <- renderUI({
    accession <- trimws(input$geo_accession %||% "")
    remote_result <- remote_job_preview_result()
    remote_loaded <- geo_remote_mode_active() && is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline")
    if (remote_loaded && (nzchar(remote_result$paths$sesame_beta %||% "") || nzchar(remote_result$paths$sesame_qc %||% ""))) {
      qc_n <- if (is.data.frame(remote_result$tables$idat_qc)) nrow(remote_result$tables$idat_qc) else 0L
      return(tags$div(
        tags$p(tags$strong("Sesame IDAT status: "), "loaded from remote"),
        tags$p(paste0("Remote sesame beta matrix is available for Spearman", if (qc_n > 0) paste0("; QC rows: ", qc_n) else "", ".")),
        tags$p(class = "geo-step-note", paste0("Remote beta: ", remote_result$paths$sesame_beta %||% "")),
        tags$p(class = "geo-step-note", paste0("Remote QC: ", remote_result$paths$sesame_qc %||% ""))
      ))
    }
    if (!nzchar(accession)) {
      return(tags$p(class = "geo-step-note", "Inspect a GEO accession before scanning raw IDAT availability."))
    }
    cache_dir <- ugplot_geo_cache_dir(accession)
    pairs <- ugplot_geo_idat_pairs(cache_dir)
    raw_archives <- character(0)
    if (dir.exists(cache_dir)) {
      raw_archives <- list.files(cache_dir, full.names = TRUE, recursive = FALSE)
      raw_archives <- raw_archives[vapply(raw_archives, ugplot_geo_is_raw_archive, logical(1))]
    }
    complete_pairs <- if (is.data.frame(pairs)) sum(pairs$Complete) else 0L
    qc_path <- ugplot_geo_sesame_qc_path(cache_dir)
    beta_path <- ugplot_geo_sesame_beta_path(cache_dir)
    tags$div(
      tags$p(paste0("Complete Red/Grn IDAT pairs: ", complete_pairs, ".")),
      if (length(raw_archives) > 0) tags$p(paste0("Raw archives available for extraction: ", length(raw_archives), ".")) else NULL,
      tags$p(paste0("Sesame installed: ", if (requireNamespace("sesame", quietly = TRUE)) "yes" else "no", ".")),
      if (file.exists(beta_path)) tags$p(paste0("Cached beta matrix: ", beta_path)) else NULL,
      if (file.exists(qc_path)) tags$p(paste0("Cached QC report: ", qc_path)) else NULL
    )
  })

  output$geo_idat_action_ui <- renderUI({
    progress <- geo_idat_qc_progress()
    idat_done <- (progress$phase %||% "") %in% c("complete", "loaded from cache")
    if (!requireNamespace("sesame", quietly = TRUE)) {
      return(tags$div(
        actionButton("geo_install_sesame", "Install sesame"),
        tags$p(class = "geo-step-note", "Sesame is required before raw Red/Grn IDAT QC can run.")
      ))
    }
    actionButton("geo_run_sesame_idat", if (idat_done) "Re-run sesame IDAT QC" else "Run sesame IDAT QC")
  })

  output$geo_idat_qc_progress_ui <- renderUI({
    progress <- geo_idat_qc_progress()
    remote_result <- remote_job_preview_result()
    remote_loaded <- geo_remote_mode_active() && is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline")
    if (remote_loaded && (nzchar(remote_result$paths$sesame_beta %||% "") || nzchar(remote_result$paths$sesame_qc %||% "")) &&
        !((progress$phase %||% "") %in% c("complete", "loaded from cache", "loaded from remote"))) {
      qc_n <- if (is.data.frame(remote_result$tables$idat_qc)) nrow(remote_result$tables$idat_qc) else 0L
      progress <- list(
        phase = "loaded from remote",
        message = "Loaded remote sesame paths from the selected GEO job.",
        processed = qc_n,
        total = qc_n,
        current = "",
        beta_path = remote_result$paths$sesame_beta %||% "",
        qc_path = remote_result$paths$sesame_qc %||% ""
      )
    }
    tags$div(class = "geo-status-card",
      tags$p(tags$strong("Sesame IDAT status: "), progress$phase %||% "idle"),
      tags$p(progress$message %||% ""),
      tags$p(paste0(
        "Processed ", progress$processed %||% 0L, " / ", progress$total %||% 0L,
        if (nzchar(progress$current %||% "")) paste0("; current: ", progress$current) else ""
      )),
      if (nzchar(progress$beta_path %||% "")) tags$p(paste0("Beta matrix: ", progress$beta_path)) else NULL,
      if (nzchar(progress$qc_path %||% "")) tags$p(paste0("QC report: ", progress$qc_path)) else NULL
    )
  })

  output$geo_idat_qc_table_title <- renderUI({
    qc <- geo_idat_qc_report()
    req(is.data.frame(qc), nrow(qc) > 0)
    passed <- if ("PassedQC" %in% names(qc)) sum(as.logical(qc$PassedQC), na.rm = TRUE) else NA_integer_
    render_geo_table_title(
      "Sesame IDAT QC report",
      paste0("Sample-level raw Red/Grn reprocessing QC. Passed samples: ", passed, " / ", nrow(qc), ".")
    )
  })

  output$geo_idat_qc_table <- DT::renderDT({
    qc <- geo_idat_qc_report()
    req(is.data.frame(qc), nrow(qc) > 0)
    display <- qc
    for (metric_col in intersect(c("FailedProbeFraction", "MissingBetaFraction", "DetectionPThreshold"), names(display))) {
      display[[metric_col]] <- signif(suppressWarnings(as.numeric(display[[metric_col]])), 4)
    }
    DT::datatable(display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, selection = "single")
  })

  output$geo_preview_table_title <- renderUI({
    preview <- geo_preview_data()
    req(is.data.frame(preview), nrow(preview) > 0)
    render_geo_table_title(
      "Loaded ugPlot preview",
      "Preview of the small table loaded directly into ugPlot."
    )
  })

  output$geo_preview_table <- DT::renderDT({
    preview <- geo_preview_data()
    req(is.data.frame(preview), nrow(preview) > 0)
    DT::datatable(utils::head(preview, 10), options = list(pageLength = 5, scrollX = TRUE), rownames = TRUE)
  })

  output$geo_metadata_table <- DT::renderDT({
    metadata <- geo_sample_metadata()
    req(is.data.frame(metadata), nrow(metadata) > 0)
    display_cols <- c(
      intersect(c("sample_id", "title", "geo_accession", "source_name_ch1", "organism_ch1"), names(metadata)),
      setdiff(names(metadata), c("sample_id", "title", "geo_accession", "source_name_ch1", "organism_ch1"))
    )
    DT::datatable(metadata[, display_cols, drop = FALSE], options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$geo_target_selector <- renderUI({
    metadata <- geo_sample_metadata()
    if (!is.data.frame(metadata) || nrow(metadata) == 0) {
      return(tags$p("Fetch sample metadata first to choose a field."))
    }
    candidates <- ugplot_geo_target_candidates(metadata)
    if (length(candidates) == 0) {
      return(tags$p("No usable metadata field was detected. Inspect the sample metadata table."))
    }
    current <- input$geo_target_column %||% ""
    remote_result <- remote_job_preview_result()
    remote_target <- if (is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline")) {
      as.character(remote_result$target_column %||% "")
    } else {
      ""
    }
    selected <- if (nzchar(current) && current %in% candidates) {
      current
    } else if (nzchar(remote_target) && remote_target %in% candidates) {
      remote_target
    } else {
      ""
    }
    choices <- c("Choose metadata field" = "", stats::setNames(candidates, candidates))
    selectInput("geo_target_column", "Metadata field to predict/correlate:", choices = choices, selected = selected)
  })

  output$geo_annotation_summary <- renderUI({
    metadata <- geo_sample_metadata()
    if (!is.data.frame(metadata) || nrow(metadata) == 0) {
      return(tags$p("Fetch sample metadata first to detect the methylation platform."))
    }
    platform_id <- ugplot_geo_detect_platform(metadata)
    if (!nzchar(platform_id %||% "")) {
      return(tags$p("No GEO platform_id was found in sample metadata."))
    }
    platform_info <- ugplot_geo_platform_annotation_package(platform_id)
    if (is.null(platform_info)) {
      return(tags$p(paste0("No built-in CpG annotation cache is configured for ", platform_id, ".")))
    }
    annotation_map <- geo_cpg_annotation()
    cache_path <- ugplot_geo_annotation_cache_path(platform_info$platform, "rds")
    if (is.data.frame(annotation_map) && nrow(annotation_map) > 0) {
      return(tags$div(
        tags$p(paste0(
          "Annotation loaded: ", nrow(annotation_map), " CpG-gene/transcript links; ",
          length(unique(annotation_map$CpG)), " CpGs; ",
          length(unique(stats::na.omit(annotation_map$Gene))), " genes; ",
          length(unique(stats::na.omit(annotation_map$Transcript))), " transcripts."
        )),
        tags$p(paste0("Platform: ", platform_info$platform, " (", platform_info$array, ")."))
      ))
    }
    if (file.exists(cache_path)) {
      return(tags$div(
        tags$p(paste0("CpG annotation cache is available locally for ", platform_info$platform, ".")),
        tags$p(paste0("Cache: ", cache_path))
      ))
    }
    missing_packages <- ugplot_geo_missing_annotation_packages(platform_info)
    if (length(missing_packages) == 0) {
      return(tags$div(
        tags$p(paste0("Annotation packages are installed for ", platform_info$platform, ", but the cache has not been built yet.")),
        tags$p("Click Build/load CpG annotation cache to create the local CpG-to-gene/transcript map.")
      ))
    }
    tags$div(
      tags$p(paste0("CpG annotation cache is not built yet for ", platform_info$platform, " (", platform_info$array, ").")),
      tags$p(paste0("Missing Bioconductor package(s): ", paste(missing_packages, collapse = ", "), "."))
    )
  })

  output$geo_annotation_table <- DT::renderDT({
    annotation_map <- geo_cpg_annotation()
    req(is.data.frame(annotation_map), nrow(annotation_map) > 0)
    display_cols <- intersect(
      c("CpG", "Gene", "Transcript", "GeneRegion", "Chr", "Position", "CpGIslandRelation", "RegulatoryFeature", "ProbeType", "Platform", "Genome"),
      names(annotation_map)
    )
    DT::datatable(annotation_map[, display_cols, drop = FALSE], options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  geo_safe_cache_token <- function(value) {
    gsub("[^A-Za-z0-9_.-]+", "_", as.character(value))
  }

  geo_matrix_source_value <- function(source = NULL) {
    source <- source %||% isolate(input$geo_matrix_source %||% "processed")
    if (!source %in% c("processed", "raw_sesame")) {
      source <- "processed"
    }
    source
  }

  geo_matrix_source_label <- function(source = NULL) {
    source <- geo_matrix_source_value(source)
    if (identical(source, "raw_sesame")) {
      "raw IDAT / sesame"
    } else {
      "GEO processed matrix"
    }
  }

  geo_analysis_cache_dir <- function(cache_dir, source = NULL, create = TRUE) {
    source <- geo_matrix_source_value(source)
    path <- file.path(cache_dir, "analysis", geo_safe_cache_token(source))
    if (isTRUE(create) && !dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
    path
  }

  geo_spearman_cache_paths <- function(cache_dir, target_column, source = NULL, create = TRUE) {
    analysis_dir <- geo_analysis_cache_dir(cache_dir, source, create = create)
    safe_target <- geo_safe_cache_token(target_column)
    prefix <- file.path(analysis_dir, paste0("ugplot_geo_spearman_", safe_target))
    list(
      raw = paste0(prefix, ".csv"),
      annotated = paste0(prefix, "_annotated.csv"),
      by_transcript = paste0(prefix, "_by_transcript.csv"),
      by_gene = paste0(prefix, "_by_gene.csv")
    )
  }

  geo_spearman_min_samples_pct <- function(default = 80) {
    value <- suppressWarnings(as.numeric(isolate(input$geo_spearman_min_samples %||% default)))
    if (!is.finite(value)) {
      value <- default
    }
    max(0, min(100, value))
  }

  geo_filter_spearman_min_samples <- function(results, min_samples = NULL) {
    if (!is.data.frame(results) || nrow(results) == 0 || !"N" %in% names(results)) {
      return(results)
    }
    n_values <- suppressWarnings(as.numeric(results$N))
    max_n <- suppressWarnings(max(n_values, na.rm = TRUE))
    if (!is.finite(max_n) || max_n <= 0) {
      return(results)
    }
    if (is.null(min_samples)) {
      min_samples <- ceiling(max(3, max_n * geo_spearman_min_samples_pct() / 100))
    }
    min_samples <- suppressWarnings(as.integer(min_samples))
    if (!is.finite(min_samples) || min_samples < 3) {
      min_samples <- 3L
    }
    filtered <- results[n_values >= min_samples, , drop = FALSE]
    rownames(filtered) <- NULL
    filtered
  }

  geo_transcript_cache_version <- function() {
    "reader_v4"
  }

  geo_transcript_group_cache_paths <- function(cache_dir, target_column, threshold, min_samples_pct, source = NULL, create = TRUE) {
    safe_target <- geo_safe_cache_token(target_column)
    safe_threshold <- geo_safe_cache_token(format(threshold, trim = TRUE, scientific = FALSE))
    safe_min_samples <- geo_safe_cache_token(format(min_samples_pct, trim = TRUE, scientific = FALSE))
    safe_missing <- geo_safe_cache_token(paste(geo_transcript_missing_definition(), collapse = "_"))
    prefix <- file.path(geo_analysis_cache_dir(cache_dir, source, create = create), paste0(
      "ugplot_geo_transcript_ml_groups_", safe_target,
      "_", geo_transcript_cache_version(),
      "_absrho_", safe_threshold,
      "_minsamples_", safe_min_samples,
      "_missing_", safe_missing
    ))
    list(
      summary = paste0(prefix, "_summary.csv"),
      details = paste0(prefix, "_details.csv"),
      progress = paste0(prefix, "_progress.rds")
    )
  }

  geo_group_key <- function(values) {
    paste(sort(unique(as.character(values))), collapse = "\r")
  }

  geo_transcript_missing_definition <- function() {
    c("empty", "na", "zero")
  }

  geo_transcript_candidates_for_id <- function(candidates, transcript_id) {
    transcript_id <- as.character(transcript_id %||% "")
    if (!nzchar(transcript_id) || !is.data.frame(candidates) || nrow(candidates) == 0) {
      return(data.frame())
    }
    annotation_map <- attr(candidates, "annotation_map", exact = TRUE)
    raw_results <- attr(candidates, "raw_results", exact = TRUE)
    threshold <- attr(candidates, "threshold", exact = TRUE)
    if (is.data.frame(annotation_map) && nrow(annotation_map) > 0 &&
        is.data.frame(raw_results) && nrow(raw_results) > 0) {
      transcript_cpgs <- annotation_map[
        !is.na(annotation_map$Transcript) &
          as.character(annotation_map$Transcript) == transcript_id,
        ,
        drop = FALSE
      ]
      if (nrow(transcript_cpgs) == 0) {
        return(data.frame())
      }
      transcript_cpgs <- unique(transcript_cpgs)
      required_result_cols <- intersect(c("CpG", "SpearmanRho", "PValue", "N", "AbsRho"), names(raw_results))
      transcript_candidates <- merge(
        transcript_cpgs,
        unique(raw_results[, required_result_cols, drop = FALSE]),
        by = "CpG",
        all.x = TRUE,
        sort = FALSE
      )
      transcript_candidates$CpGInSpearmanScan <- !is.na(transcript_candidates$AbsRho)
      trigger_rows <- candidates[as.character(candidates$Transcript) == transcript_id, , drop = FALSE]
      trigger_rows <- trigger_rows[order(-suppressWarnings(as.numeric(trigger_rows$AbsRho)), suppressWarnings(as.numeric(trigger_rows$PValue))), , drop = FALSE]
      if (nrow(trigger_rows) > 0) {
        transcript_candidates$TriggerCpGs <- paste(unique(trigger_rows$CpG), collapse = ";")
        transcript_candidates$TriggerGenes <- paste(unique(stats::na.omit(trigger_rows$Gene)), collapse = ";")
        transcript_candidates$TriggerMaxAbsRho <- max(suppressWarnings(as.numeric(trigger_rows$AbsRho)), na.rm = TRUE)
        transcript_candidates$TriggerBestCpG <- trigger_rows$CpG[[1]]
        transcript_candidates$TriggerBestRho <- trigger_rows$SpearmanRho[[1]]
        transcript_candidates$ThresholdAbsRho <- threshold %||% NA_real_
      }
      rownames(transcript_candidates) <- NULL
      return(transcript_candidates)
    }
    candidates[as.character(candidates$Transcript) == transcript_id, , drop = FALSE]
  }

  geo_limit_transcript_detail_rows <- function(details, kept_cpgs, max_rows = 300L) {
    if (!is.data.frame(details) || nrow(details) <= max_rows || !"CpG" %in% names(details)) {
      return(details)
    }
    details$CpGKeptForML <- as.character(details$CpG) %in% kept_cpgs
    absrho <- if ("AbsRho" %in% names(details)) suppressWarnings(as.numeric(details$AbsRho)) else rep(NA_real_, nrow(details))
    order_idx <- order(!details$CpGKeptForML, -absrho, as.character(details$CpG), na.last = TRUE)
    details <- details[order_idx, , drop = FALSE]
    kept_rows <- details[details$CpGKeptForML, , drop = FALSE]
    remaining_rows <- details[!details$CpGKeptForML, , drop = FALSE]
    limited <- rbind(
      utils::head(kept_rows, max_rows),
      utils::head(remaining_rows, max(0L, max_rows - min(nrow(kept_rows), max_rows)))
    )
    limited$DetailRowsShown <- nrow(limited)
    limited$DetailRowsTotal <- nrow(details)
    limited$DetailRowsTruncated <- nrow(details) > nrow(limited)
    limited
  }

  geo_build_group_tables <- function(progress_rows, candidates) {
    compatible <- progress_rows[progress_rows$Status == "compatible", , drop = FALSE]
    if (nrow(compatible) == 0) {
      return(list(summary = data.frame(), details = data.frame()))
    }

    compatible$GroupKey <- paste(compatible$CpGKey, compatible$SampleKey, sep = "\f")
    group_keys <- unique(compatible$GroupKey)
    summary_rows <- lapply(seq_along(group_keys), function(group_index) {
      group_df <- compatible[compatible$GroupKey == group_keys[[group_index]], , drop = FALSE]
      group_df <- group_df[order(-group_df$TriggerMaxAbsRho, -group_df$Columns, -group_df$Samples, group_df$Transcript), , drop = FALSE]
      principal <- group_df[1, , drop = FALSE]
      data.frame(
        GroupID = paste0("TG", group_index),
        PrincipalTranscript = principal$Transcript[[1]],
        Gene = principal$Gene[[1]],
        Columns = principal$Columns[[1]],
        Samples = principal$Samples[[1]],
        TranscriptCount = nrow(group_df),
        ExtraTranscripts = paste(setdiff(group_df$Transcript, principal$Transcript[[1]]), collapse = ";"),
        CpGs = principal$KeptCpGs[[1]],
        TriggerMaxAbsRho = principal$TriggerMaxAbsRho[[1]],
        TriggerBestCpG = if ("TriggerBestCpG" %in% names(principal)) principal$TriggerBestCpG[[1]] else "",
        TriggerBestRho = if ("TriggerBestRho" %in% names(principal)) principal$TriggerBestRho[[1]] else NA_real_,
        DatasetPath = principal$DatasetPath[[1]],
        GroupKey = group_keys[[group_index]],
        stringsAsFactors = FALSE
      )
    })
    summary <- do.call(rbind, summary_rows)
    summary <- summary[order(-summary$TriggerMaxAbsRho, -summary$Columns, -summary$Samples, summary$PrincipalTranscript), , drop = FALSE]
    summary$GroupID <- paste0("TG", seq_len(nrow(summary)))

    group_lookup <- stats::setNames(summary$GroupID, summary$GroupKey)
    detail_rows <- lapply(seq_len(nrow(compatible)), function(i) {
      transcript_row <- compatible[i, , drop = FALSE]
      transcript_candidates <- geo_transcript_candidates_for_id(candidates, transcript_row$Transcript[[1]])
      kept_cpgs <- strsplit(transcript_row$KeptCpGs[[1]], ";", fixed = TRUE)[[1]]
      transcript_candidates$GroupID <- unname(group_lookup[[transcript_row$GroupKey[[1]]]])
      transcript_candidates$PrincipalTranscript <- summary$PrincipalTranscript[match(transcript_candidates$GroupID, summary$GroupID)]
      transcript_candidates$CpGKeptForML <- as.character(transcript_candidates$CpG) %in% kept_cpgs
      geo_limit_transcript_detail_rows(transcript_candidates, kept_cpgs)
    })
    details <- unique(do.call(rbind, detail_rows))
    rownames(summary) <- NULL
    rownames(details) <- NULL
    list(summary = summary, details = details)
  }

  write_geo_transcript_group_cache <- function(paths, tables, progress_rows) {
    if (is.data.frame(tables$summary)) {
      utils::write.csv(tables$summary, paths$summary, row.names = FALSE)
    }
    if (is.data.frame(tables$details)) {
      utils::write.csv(tables$details, paths$details, row.names = FALSE)
    }
    saveRDS(progress_rows, paths$progress)
  }

  update_geo_transcript_build_progress <- function(phase = NULL, message = NULL,
                                                   processed = NULL, total = NULL,
                                                   compatible = NULL, excluded = NULL,
                                                   current = NULL, cache = NULL,
                                                   detail = NULL) {
    progress <- geo_transcript_build_progress()
    if (!is.null(phase)) progress$phase <- phase
    if (!is.null(message)) progress$message <- message
    if (!is.null(processed)) progress$processed <- processed
    if (!is.null(total)) progress$total <- total
    if (!is.null(compatible)) progress$compatible <- compatible
    if (!is.null(excluded)) progress$excluded <- excluded
    if (!is.null(current)) progress$current <- current
    if (!is.null(cache)) progress$cache <- cache
    if (!is.null(detail)) progress$detail <- detail
    geo_transcript_build_progress(progress)
    invisible(progress)
  }

  output$geo_transcript_build_progress_ui <- renderUI({
    progress <- geo_transcript_build_progress()
    remote_result <- remote_job_preview_result()
    remote_loaded <- geo_remote_mode_active() && is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline")
    if (remote_loaded && is.data.frame(remote_result$tables$transcript_groups) && nrow(remote_result$tables$transcript_groups) > 0 &&
        !((progress$phase %||% "") %in% c("complete", "loaded from cache", "loaded from remote"))) {
      detail_rows <- if (is.data.frame(remote_result$tables$transcript_group_details)) nrow(remote_result$tables$transcript_group_details) else 0L
      progress <- list(
        phase = "loaded from remote",
        message = paste0("Loaded remote transcript ML groups: ", nrow(remote_result$tables$transcript_groups), " group(s). Large artifacts remain on the remote server."),
        processed = nrow(remote_result$tables$transcript_groups),
        total = nrow(remote_result$tables$transcript_groups),
        compatible = nrow(remote_result$tables$transcript_groups),
        excluded = 0L,
        current = "",
        cache = remote_result$paths$transcript_group_summary %||% "",
        detail = if (detail_rows > 0) paste0(detail_rows, " transcript detail row(s) loaded from remote.") else NULL
      )
    }
    total <- suppressWarnings(as.integer(progress$total %||% 0L))
    processed <- suppressWarnings(as.integer(progress$processed %||% 0L))
    compatible <- suppressWarnings(as.integer(progress$compatible %||% 0L))
    excluded <- suppressWarnings(as.integer(progress$excluded %||% 0L))
    percent <- if (is.finite(total) && total > 0) round(100 * processed / total, 1) else 0
    tags$div(
      style = "margin-top: 10px; padding: 10px; border: 1px solid #dbe7f3; background: #f8fbff; border-radius: 4px;",
      tags$p(style = "margin: 0 0 4px 0;", tags$strong("Transcript build status: "), progress$phase %||% "idle"),
      tags$p(style = "margin: 0 0 4px 0;", progress$message %||% ""),
      tags$p(style = "margin: 0 0 4px 0;", paste0(
        "Processed ", processed, " / ", total,
        " (", percent, "%); remaining ", max(0L, total - processed),
        "; compatible ", compatible,
        "; excluded ", excluded, "."
      )),
      if (nzchar(progress$detail %||% "")) tags$p(style = "margin: 0 0 4px 0;", progress$detail) else NULL,
      if (nzchar(progress$current %||% "")) tags$p(style = "margin: 0 0 4px 0;", paste0("Current transcript: ", progress$current)) else NULL,
      if (nzchar(progress$cache %||% "")) tags$p(style = "margin: 0; font-size: 12px; color: #596273;", paste0("Cache: ", progress$cache)) else NULL
    )
  })

  geo_transcript_ml_run_key <- function(target_column, threshold, min_samples_pct) {
    safe_target <- geo_safe_cache_token(target_column)
    safe_threshold <- geo_safe_cache_token(format(threshold, trim = TRUE, scientific = FALSE))
    safe_min_samples <- geo_safe_cache_token(format(min_samples_pct, trim = TRUE, scientific = FALSE))
    safe_missing <- geo_safe_cache_token(paste(geo_transcript_missing_definition(), collapse = "_"))
    paste0(
      "target_", safe_target,
      "_", geo_transcript_cache_version(),
      "_absrho_", safe_threshold,
      "_minsamples_", safe_min_samples,
      "_missing_", safe_missing
    )
  }

  geo_current_transcript_ml_run_key <- function() {
    threshold <- suppressWarnings(as.numeric(input$geo_transcript_absrho_threshold %||% 0.8))
    if (!is.finite(threshold)) threshold <- 0.8
    min_samples <- suppressWarnings(as.numeric(input$geo_transcript_min_samples %||% 80))
    if (!is.finite(min_samples)) min_samples <- 80
    geo_transcript_ml_run_key(input$geo_target_column %||% "", threshold, min_samples)
  }

  geo_transcript_ml_dir <- function(cache_dir, source = NULL, run_key = NULL) {
    path <- file.path(geo_analysis_cache_dir(cache_dir, source), "transcript_ml_pipeline")
    run_key <- as.character(run_key %||% "")
    if (nzchar(run_key)) {
      path <- file.path(path, geo_safe_cache_token(run_key))
    }
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
    path
  }

  geo_transcript_ml_group_dir <- function(cache_dir, source, group_id, run_key = NULL) {
    path <- file.path(geo_transcript_ml_dir(cache_dir, source, run_key), geo_safe_cache_token(group_id))
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
    path
  }

  geo_ml_safe_num <- function(value, fallback, min_value = NULL, max_value = NULL) {
    parsed <- suppressWarnings(as.numeric(value))
    if (length(parsed) == 0 || !is.finite(parsed[[1]])) {
      parsed <- fallback
    } else {
      parsed <- parsed[[1]]
    }
    if (!is.null(min_value)) parsed <- max(min_value, parsed)
    if (!is.null(max_value)) parsed <- min(max_value, parsed)
    parsed
  }

  geo_ml_stability_state <- function(values, min_seeds, window, tolerance) {
    ugplot_geo_stability_state(values, min_seeds, window, tolerance)
  }

  geo_ml_importance_table <- function(model, group, source, phase) {
    ugplot_geo_ml_importance_table(model, group, source, phase)
  }

  geo_ml_result_metric_values <- function(result) {
    ugplot_geo_ml_metric_values(result)
  }

  geo_ml_rank_summary <- function(summary) {
    ugplot_geo_ml_rank_summary(summary)
  }

  bind_summary_rows <- function(rows) {
    ugplot_geo_bind_rows(rows)
  }

  geo_ml_load_screening_summary <- function(pipeline_dir, write_back = TRUE) {
    summary_path <- file.path(pipeline_dir, "screening_summary.csv")
    csv_summary <- if (file.exists(summary_path)) {
      tryCatch(utils::read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    } else {
      data.frame()
    }
    group_summary_paths <- if (dir.exists(pipeline_dir)) {
      list.files(pipeline_dir, pattern = "^screen_summary[.]rds$", recursive = TRUE, full.names = TRUE)
    } else {
      character(0)
    }
    group_summaries <- lapply(group_summary_paths, function(path) {
      tryCatch(readRDS(path), error = function(e) data.frame())
    })
    if (length(group_summaries) > 0) {
      cached_summary <- bind_summary_rows(group_summaries)
      combined <- if (is.data.frame(csv_summary) && nrow(csv_summary) > 0) {
        bind_summary_rows(list(csv_summary, cached_summary))
      } else {
        cached_summary
      }
      if ("GroupID" %in% names(combined)) {
        combined <- combined[rev(seq_len(nrow(combined))), , drop = FALSE]
        combined <- combined[!duplicated(as.character(combined$GroupID)), , drop = FALSE]
        combined <- combined[rev(seq_len(nrow(combined))), , drop = FALSE]
      }
      if (!"ModelsRun" %in% names(combined)) combined$ModelsRun <- NA_integer_
      if (!"ModelsOK" %in% names(combined)) combined$ModelsOK <- NA_integer_
      if ("ScreenResultPath" %in% names(combined)) {
        for (row_i in seq_len(nrow(combined))) {
          if (is.na(combined$ModelsRun[[row_i]]) || is.na(combined$ModelsOK[[row_i]])) {
            result_path <- as.character(combined$ScreenResultPath[[row_i]])
            if (nzchar(result_path) && file.exists(result_path)) {
              result <- tryCatch(readRDS(result_path), error = function(e) NULL)
              counts <- geo_ml_model_run_counts(result)
              combined$ModelsRun[[row_i]] <- counts[["ModelsRun"]]
              combined$ModelsOK[[row_i]] <- counts[["ModelsOK"]]
            }
          }
        }
      }
      csv_summary <- geo_ml_rank_summary(combined)
      if (isTRUE(write_back) && is.data.frame(csv_summary) && nrow(csv_summary) > 0) {
        dir.create(pipeline_dir, recursive = TRUE, showWarnings = FALSE)
        utils::write.csv(csv_summary, summary_path, row.names = FALSE)
      }
    } else {
      csv_summary <- geo_ml_rank_summary(csv_summary)
    }
    csv_summary
  }

  geo_ml_model_run_counts <- function(result) {
    ugplot_geo_ml_model_run_counts(result)
  }

  geo_ml_quick_models <- function(available) {
    ugplot_geo_ml_quick_models(available)
  }

  geo_ml_class_value <- function(values) {
    ugplot_geo_ml_class_value(values)
  }

  geo_ml_stability_group_candidates <- function(metadata) {
    if (!is.data.frame(metadata) || nrow(metadata) == 0) {
      return(character(0))
    }
    ignored <- c("sample_id", "geo_accession", "title", "description", "supplementary_file")
    candidates <- setdiff(names(metadata), ignored)
    candidates <- candidates[vapply(candidates, function(column) {
      values <- geo_ml_class_value(metadata[[column]])
      unique_values <- unique(stats::na.omit(values))
      length(unique_values) >= 2 && length(unique_values) <= 80
    }, logical(1))]
    likely <- grep("disease|status|case|control|group|class|phenotype|condition|treatment|response|sex|gender", candidates, value = TRUE, ignore.case = TRUE)
    unique(c(likely, setdiff(candidates, likely)))
  }

  geo_ml_stability_strata <- function(metadata, column) {
    ugplot_geo_ml_stability_strata(metadata, column)
  }

  geo_ml_stability_task_key <- function(group_id, stratum_column = "", stratum_value = "") {
    ugplot_geo_ml_stability_task_key(group_id, stratum_column, stratum_value)
  }

  geo_ml_clean_runner_message <- function(message, model = "") {
    message <- as.character(message %||% "")
    model <- as.character(model %||% "")
    dataset_seed <- regmatches(message, regexpr("dataset seed [0-9]+", message, ignore.case = TRUE))
    training_seed <- regmatches(message, regexpr("training seed [0-9]+", message, ignore.case = TRUE))
    elapsed <- regmatches(message, regexpr("elapsed [0-9.]+ seconds", message, ignore.case = TRUE))
    pieces <- c(
      if (nzchar(model)) paste0("model ", model) else character(0),
      if (length(dataset_seed) > 0 && nzchar(dataset_seed)) dataset_seed else character(0),
      if (length(training_seed) > 0 && nzchar(training_seed)) training_seed else character(0),
      if (length(elapsed) > 0 && nzchar(elapsed)) elapsed else character(0)
    )
    if (length(pieces) > 0) {
      return(paste(pieces, collapse = " | "))
    }
    if (grepl("running", message, ignore.case = TRUE)) {
      return(paste(c(if (nzchar(model)) paste0("model ", model), "running trainer"), collapse = " | "))
    }
    paste(c(if (nzchar(model)) paste0("model ", model), "running"), collapse = " | ")
  }

  geo_ml_stability_progress_text <- function(task_detail, completed, total, runner_message = "",
                                             stability_text = "", distribution_text = "") {
    sections <- c(
      "CURRENT",
      paste0("  ", task_detail),
      paste0("  completed tasks: ", completed, " / ", total),
      "",
      "RUNNING",
      paste0("  ", runner_message %||% "waiting for trainer update")
    )
    if (nzchar(stability_text %||% "")) {
      sections <- c(sections, "", "STABILITY", paste0("  ", stability_text))
    }
    if (nzchar(distribution_text %||% "")) {
      distribution_lines <- strsplit(distribution_text, "\n", fixed = TRUE)[[1]]
      sections <- c(sections, "", "METRIC DISTRIBUTION", paste0("  ", distribution_lines))
    }
    paste(sections, collapse = "\n")
  }

  geo_ml_pipeline_config <- function(models, screen_seeds, seed_end, timeout, best_only_model = NULL) {
    cpu_limit <- configured_cpu_limit()
    list(
      target = "target",
      models = if (is.null(best_only_model)) models else best_only_model,
      dataset_seed_start = 1,
      dataset_seed_end = 1,
      training_seed_start = 1,
      training_seed_end = seed_end,
      timeout = timeout,
      performance_mode = "default",
      missing_definition = geo_transcript_missing_definition(),
      missing_strategy = "none",
      missing_threshold_cols = 100,
      missing_threshold_rows = 100,
      complete_case_min_samples = 0,
      imputation_scope = "split_separate",
      cpu_limit = cpu_limit,
      parallel_enabled = isTRUE(input$config_parallel_cubist_models) && cpu_limit > 1L,
      use_callr_timeout = TRUE,
      restart_parallel_each_model = isTRUE(input$config_restart_parallel_each_model),
      retry_parallel_connection_errors = isTRUE(input$config_retry_parallel_connection_errors),
      screen_seeds = screen_seeds
    )
  }

  geo_ml_group_dataset <- function(group, sample_ids = NULL, keep_sample_id = FALSE) {
    ugplot_geo_ml_group_dataset(group, sample_ids = sample_ids, keep_sample_id = keep_sample_id)
  }

  geo_ml_run_group_screen <- function(group, source, models, settings, run_key = NULL, progress_callback = NULL) {
    dataset_info <- geo_ml_group_dataset(group)
    dataset <- dataset_info$dataset
    dataset_path <- dataset_info$dataset_path
    cache_dir <- ugplot_geo_cache_dir(trimws(input$geo_accession %||% "GEO"))
    group_dir <- geo_transcript_ml_group_dir(cache_dir, source, group$GroupID[[1]], run_key)
    screen_path <- file.path(group_dir, "screen_result.rds")
    summary_path <- file.path(group_dir, "screen_summary.rds")
    importance_path <- file.path(group_dir, "screen_importance.csv")

    screen_config <- geo_ml_pipeline_config(models, settings$screen_seeds, settings$screen_seeds, settings$timeout)
    screen_config$resume_result_path <- screen_path
    screen_config$model_log_dir <- file.path(group_dir, "logs", "screen")
    screen_result <- ugplot_run_ml_job(
      dataset,
      screen_config,
      progress_callback = function(...) {
        args <- list(...)
        if (!is.null(progress_callback)) {
          progress_callback(paste0("Screening ", group$GroupID[[1]], ": ", args$message %||% ""))
        }
      },
      partial_callback = function(partial) saveRDS(partial, screen_path)
    )
    saveRDS(screen_result, screen_path)

    best_model <- screen_result$best_model_name %||% ""
    if (!nzchar(best_model) || identical(best_model, "-")) {
      stop("No best model was found during screening.")
    }
    importance <- geo_ml_importance_table(screen_result$best_model, group, source, "screening")
    if (is.data.frame(importance) && nrow(importance) > 0) {
      utils::write.csv(importance, importance_path, row.names = FALSE)
    }
    metric_values <- geo_ml_result_metric_values(screen_result)
    model_counts <- geo_ml_model_run_counts(screen_result)
    metric_name <- screen_result$final_summary$metric_name %||% "R2"
    summary <- data.frame(
      Source = source,
      Phase = "screening",
      GroupID = group$GroupID[[1]],
      PrincipalTranscript = group$PrincipalTranscript[[1]],
      Gene = group$Gene[[1]],
      Columns = group$Columns[[1]],
      Samples = group$Samples[[1]],
      TranscriptCount = group$TranscriptCount[[1]],
      ExtraTranscripts = group$ExtraTranscripts[[1]] %||% "",
      CpGs = group$CpGs[[1]] %||% "",
      TriggerMaxAbsRho = suppressWarnings(as.numeric(group$TriggerMaxAbsRho[[1]])),
      TriggerBestCpG = if ("TriggerBestCpG" %in% names(group)) as.character(group$TriggerBestCpG[[1]]) else "",
      TriggerBestRho = if ("TriggerBestRho" %in% names(group)) suppressWarnings(as.numeric(group$TriggerBestRho[[1]])) else NA_real_,
      BestModel = best_model,
      MetricName = metric_name,
      BestMetric = suppressWarnings(as.numeric(screen_result$final_summary$metric_value %||% NA_real_)),
      MedianMetric = if (length(metric_values) > 0) stats::median(metric_values) else NA_real_,
      MeanMetric = if (length(metric_values) > 0) mean(metric_values) else NA_real_,
      SeedsRun = length(metric_values),
      ModelsRun = model_counts[["ModelsRun"]],
      ModelsOK = model_counts[["ModelsOK"]],
      DatasetPath = dataset_path,
      ScreenResultPath = screen_path,
      ImportancePath = if (file.exists(importance_path)) importance_path else "",
      stringsAsFactors = FALSE
    )
    saveRDS(summary, summary_path)
    summary
  }

  geo_ml_run_group_stability <- function(group, source, settings, run_key = NULL, progress_callback = NULL, stratum = NULL) {
    stratum_column <- as.character(stratum$StratumColumn %||% "")
    stratum_value <- as.character(stratum$StratumValue %||% "")
    stratum_label <- if (nzchar(stratum_column)) paste0(stratum_column, "=", stratum_value) else ""
    sample_ids <- if (nzchar(stratum_column)) strsplit(as.character(stratum$SampleIDs %||% ""), "\r", fixed = TRUE)[[1]] else NULL
    dataset_info <- geo_ml_group_dataset(group, sample_ids = sample_ids)
    dataset <- dataset_info$dataset
    dataset_path <- dataset_info$dataset_path
    cache_dir <- ugplot_geo_cache_dir(trimws(input$geo_accession %||% "GEO"))
    base_group_dir <- geo_transcript_ml_group_dir(cache_dir, source, group$GroupID[[1]], run_key)
    group_dir <- base_group_dir
    if (nzchar(stratum_column)) {
      group_dir <- file.path(base_group_dir, "stability_by", geo_safe_cache_token(stratum_column), geo_safe_cache_token(stratum_value))
      dir.create(group_dir, recursive = TRUE, showWarnings = FALSE)
    }
    screen_path <- file.path(group_dir, "screen_result.rds")
    stability_path <- file.path(group_dir, "stability_result.rds")
    summary_path <- file.path(group_dir, "summary.rds")
    importance_path <- file.path(group_dir, "importance.csv")
    if (nzchar(stratum_column)) {
      screen_path <- file.path(base_group_dir, "screen_result.rds")
    }
    if (!file.exists(screen_path)) {
      stop("Screening result is missing for ", group$GroupID[[1]], ". Run Step 9 first.")
    }
    screen_result <- tryCatch(readRDS(screen_path), error = function(e) NULL)
    best_model <- screen_result$best_model_name %||% ""
    if (!nzchar(best_model) || identical(best_model, "-")) {
      stop("No best model was found in the screening cache for ", group$GroupID[[1]], ".")
    }

    current_end <- settings$min_stability_seeds
    stability_result <- if (file.exists(stability_path)) tryCatch(readRDS(stability_path), error = function(e) NULL) else NULL
    stable_state <- list(stable = FALSE, reason = "not started")
    stability_progress_detail <- function(result = NULL, source = "cache") {
      if (is.null(result) && file.exists(stability_path)) {
        result <- tryCatch(readRDS(stability_path), error = function(e) NULL)
      }
      metric_values <- geo_ml_result_metric_values(result)
      metric_name <- result$final_summary$metric_name %||% "R2"
      if (length(metric_values) == 0) {
        return(list(stability = "", distribution = "", values = numeric(0), source = source))
      }
      list(
        stability = format_running_stability_signal(metric_values, metric_name = metric_name),
        distribution = format_running_metric_distribution(metric_values, metric_name = metric_name, bins = 8, width = 14),
        values = metric_values,
        source = source
      )
    }
    repeat {
      existing_n <- length(geo_ml_result_metric_values(stability_result))
      current_end <- max(current_end, min(settings$max_stability_seeds, existing_n + settings$window))
      stability_config <- geo_ml_pipeline_config(best_model, settings$screen_seeds, current_end, settings$timeout, best_only_model = best_model)
      stability_config$resume_result_path <- stability_path
      stability_config$model_log_dir <- file.path(group_dir, "logs", "stability")
      stability_result <- ugplot_run_ml_job(
        dataset,
        stability_config,
        progress_callback = function(...) {
          args <- list(...)
          if (!is.null(progress_callback)) {
            progress_callback(
              geo_ml_clean_runner_message(args$message %||% "", best_model),
              stability_progress_detail(source = "runner")
            )
          }
        },
        partial_callback = function(partial) {
          saveRDS(partial, stability_path)
          detail <- stability_progress_detail(partial, source = "partial")
          if (!is.null(progress_callback)) {
            progress_callback(
              paste0("Model ", best_model, ": ", length(detail$values), " seed result(s) collected."),
              detail
            )
          }
        }
      )
      saveRDS(stability_result, stability_path)
      metric_values <- geo_ml_result_metric_values(stability_result)
      stable_state <- geo_ml_stability_state(metric_values, settings$min_stability_seeds, settings$window, settings$tolerance)
      if (isTRUE(stable_state$stable) || length(metric_values) >= settings$max_stability_seeds) {
        break
      }
      current_end <- min(settings$max_stability_seeds, length(metric_values) + settings$window)
      if (current_end <= length(metric_values)) {
        break
      }
    }

    importance <- geo_ml_importance_table(stability_result$best_model, group, source, "stability")
    if (is.data.frame(importance) && nrow(importance) > 0) {
      if (nzchar(stratum_column)) {
        importance$StratumColumn <- stratum_column
        importance$StratumValue <- stratum_value
        importance$StratumSamples <- dataset_info$sample_count
      }
      utils::write.csv(importance, importance_path, row.names = FALSE)
    }

    metric_values <- geo_ml_result_metric_values(stability_result)
    metric_name <- stability_result$final_summary$metric_name %||% "R2"
    summary <- data.frame(
      Source = source,
      Phase = "stability",
      StratumColumn = stratum_column,
      StratumValue = stratum_value,
      StratumSamples = if (nzchar(stratum_column)) dataset_info$sample_count else NA_integer_,
      GroupID = group$GroupID[[1]],
      PrincipalTranscript = group$PrincipalTranscript[[1]],
      Gene = group$Gene[[1]],
      Columns = group$Columns[[1]],
      Samples = group$Samples[[1]],
      TranscriptCount = group$TranscriptCount[[1]],
      ExtraTranscripts = group$ExtraTranscripts[[1]] %||% "",
      CpGs = group$CpGs[[1]] %||% "",
      TriggerMaxAbsRho = suppressWarnings(as.numeric(group$TriggerMaxAbsRho[[1]])),
      TriggerBestCpG = if ("TriggerBestCpG" %in% names(group)) as.character(group$TriggerBestCpG[[1]]) else "",
      TriggerBestRho = if ("TriggerBestRho" %in% names(group)) suppressWarnings(as.numeric(group$TriggerBestRho[[1]])) else NA_real_,
      BestModel = best_model,
      MetricName = metric_name,
      BestMetric = suppressWarnings(as.numeric(stability_result$final_summary$metric_value %||% NA_real_)),
      MedianMetric = if (length(metric_values) > 0) stats::median(metric_values) else NA_real_,
      MeanMetric = if (length(metric_values) > 0) mean(metric_values) else NA_real_,
      MetricSE = if (length(metric_values) > 1) stats::sd(metric_values) / sqrt(length(metric_values)) else NA_real_,
      SeedsRun = length(metric_values),
      Stable = isTRUE(stable_state$stable),
      StabilityDetail = stable_state$reason,
      DatasetPath = dataset_path,
      ScreenResultPath = screen_path,
      StabilityResultPath = stability_path,
      ImportancePath = if (file.exists(importance_path)) importance_path else "",
      stringsAsFactors = FALSE
    )
    saveRDS(summary, summary_path)
    summary
  }

  update_geo_transcript_ml_progress <- function(phase = NULL, message = NULL,
                                                processed = NULL, total = NULL,
                                                current = NULL, cache = NULL,
                                                detail = NULL, stability = NULL,
                                                values = NULL, active = TRUE) {
    progress <- geo_transcript_ml_progress()
    if (!is.null(phase)) progress$phase <- phase
    if (!is.null(message)) progress$message <- message
    if (!is.null(processed)) progress$processed <- processed
    if (!is.null(total)) progress$total <- total
    if (!is.null(current)) progress$current <- current
    if (!is.null(cache)) progress$cache <- cache
    if (!is.null(detail)) progress$detail <- detail
    if (!is.null(stability)) progress$stability <- stability
    if (!is.null(values)) progress$values <- values
    progress$live_active <- isTRUE(active)
    geo_transcript_ml_progress(progress)
    total_value <- suppressWarnings(as.numeric(progress$total %||% 0))
    processed_value <- suppressWarnings(as.numeric(progress$processed %||% 0))
    percent <- if (is.finite(total_value) && total_value > 0) 100 * processed_value / total_value else 0
    if (identical(progress$phase %||% "", "running") && nzchar(progress$current %||% "")) {
      percent <- max(percent, min(99, percent))
    }
    session$sendCustomMessage("geoMlProgress", list(
      active = isTRUE(active) && (progress$phase %||% "") %in% c("running", "failed"),
      title = paste0("Transcript ML: ", progress$phase %||% "idle"),
      percent = percent,
      task = progress$current %||% "",
      message = progress$detail %||% progress$message %||% "",
      stability = progress$stability %||% "",
      values = utils::tail(suppressWarnings(as.numeric(progress$values %||% numeric(0))), 80),
      cache = progress$cache %||% ""
    ))
    flush_react <- get("flushReact", asNamespace("shiny"))
    if (is.function(flush_react)) {
      flush_react()
    }
    invisible(progress)
  }

  output$geo_ml_live_progress_ui <- renderUI({
    progress <- geo_transcript_ml_progress()
    active <- isTRUE(progress$live_active %||% FALSE) &&
      (progress$phase %||% "") %in% c("running", "failed")
    if (!isTRUE(active)) {
      return(NULL)
    }
    total <- suppressWarnings(as.numeric(progress$total %||% 0))
    processed <- suppressWarnings(as.numeric(progress$processed %||% 0))
    percent <- if (is.finite(total) && total > 0) 100 * processed / total else 0
    if (identical(progress$phase %||% "", "running") && nzchar(progress$current %||% "")) {
      percent <- max(percent, min(99, percent))
    }
    percent <- max(0, min(100, percent))
    values <- utils::tail(suppressWarnings(as.numeric(progress$values %||% numeric(0))), 80)
    values <- values[is.finite(values)]
    spark <- NULL
    if (length(values) > 1) {
      width <- 260
      height <- 58
      pad <- 4
      value_min <- min(values)
      value_max <- max(values)
      span <- value_max - value_min
      if (!is.finite(span) || span == 0) span <- 1
      x <- pad + seq_along(values) - 1
      x <- pad + (seq_along(values) - 1) * ((width - 2 * pad) / max(1, length(values) - 1))
      y <- height - pad - ((values - value_min) / span) * (height - 2 * pad)
      points <- paste(paste0(round(x, 1), ",", round(y, 1)), collapse = " ")
      spark <- tags$svg(class = "geo-ml-live-spark", viewBox = paste("0 0", width, height),
        tags$polyline(points = points, fill = "none", stroke = "#2563eb", `stroke-width` = 2, `stroke-linecap` = "round", `stroke-linejoin` = "round")
      )
    } else {
      spark <- tags$svg(class = "geo-ml-live-spark")
    }
    tags$div(
      id = "geoMlLiveProgress",
      class = "geo-ml-live-progress active",
      tags$div(class = "geo-ml-live-head",
        tags$strong(paste0("Transcript ML: ", progress$phase %||% "idle")),
        tags$span(paste0(round(percent), "%"))
      ),
      tags$div(class = "geo-ml-live-bar-track", tags$div(class = "geo-ml-live-bar", style = paste0("width: ", percent, "%;"))),
      if (nzchar(progress$current %||% "")) tags$p(class = "geo-ml-live-task", progress$current) else NULL,
      tags$p(class = "geo-ml-live-message", progress$detail %||% progress$message %||% ""),
      spark,
      if (nzchar(progress$stability %||% "")) tags$pre(class = "geo-ml-live-stability", progress$stability) else NULL,
      if (nzchar(progress$cache %||% "")) tags$p(class = "geo-ml-live-cache", progress$cache) else NULL
    )
  })

  output$geo_ml_model_summary <- renderUI({
    available <- unique(as.character(ml_available))
    available <- available[nzchar(available)]
    if (length(available) == 0) {
      return(tags$p(class = "geo-step-note", "No installed caret models are currently available."))
    }
    selected <- if (isTRUE(input$geo_ml_quick_models)) geo_ml_quick_models(available) else available
    selected <- selected[nzchar(selected)]
    tags$div(
      tags$p(
        tags$strong("Models for transcript ML: "),
        if (isTRUE(input$geo_ml_quick_models)) {
          paste0(length(selected), " representative model(s) will be screened.")
        } else {
          paste0(length(available), " installed model(s) will be screened.")
        }
      ),
      tags$p(class = "geo-step-note", paste(utils::head(selected, 12), collapse = ", "), if (length(selected) > 12) "..." else "")
    )
  })

  output$geo_distributed_screening_ui <- renderUI({
    states <- remote_server_connection_state()
    capabilities <- remote_server_capabilities()
    servers <- remote_servers()
    if (!is.data.frame(states) || nrow(states) == 0) {
      return(tags$p(class = "geo-step-note", "Refresh remote jobs to discover compatible screening workers."))
    }
    compatible_names <- as.character(states$server[
      states$state %in% c("idle", "active") &
        vapply(as.character(states$server), function(server_name) {
          isTRUE(capabilities[[server_name]]$distributed_geo_screening %||% FALSE) &&
            identical(as.integer(capabilities[[server_name]]$distributed_protocol_version %||% 0L), 1L)
        }, logical(1))
    ])
    compatible_names <- intersect(compatible_names, as.character(servers$name))
    if (length(compatible_names) == 0) {
      return(tags$div(
        checkboxInput("geo_distributed_screening", "Distribute screening between servers", value = FALSE),
        tags$p(class = "geo-step-note", "No compatible workers are available. Update the remote servers to this ugPlot version.")
      ))
    }
    current <- isolate(input$geo_distributed_worker_names %||% character(0))
    selected <- intersect(as.character(current), compatible_names)
    if (length(selected) == 0) {
      selected <- compatible_names
    }
    insecure_workers <- as.character(servers$name[
      servers$name %in% compatible_names &
        grepl("^http://", as.character(servers$url), ignore.case = TRUE)
    ])
    tags$div(
      checkboxInput("geo_distributed_screening", "Distribute screening between servers", value = TRUE),
      conditionalPanel(
        condition = "input.geo_distributed_screening",
        checkboxGroupInput(
          "geo_distributed_worker_names",
          "Screening workers:",
          choices = compatible_names,
          selected = selected,
          inline = TRUE
        )
      ),
      if (length(insecure_workers) > 0) {
        tags$p(
          class = "geo-step-note text-danger",
          paste0(
            "Unencrypted worker connection: ",
            paste(insecure_workers, collapse = ", "),
            ". Use HTTPS or a private VPN before sending sensitive datasets."
          )
        )
      } else {
        NULL
      }
    )
  })

  output$geo_ml_stability_group_selector <- renderUI({
    metadata <- geo_sample_metadata()
    choices <- geo_ml_stability_group_candidates(metadata)
    labels <- if (length(choices) > 0) {
      stats::setNames(choices, vapply(choices, function(column) {
        values <- geo_ml_class_value(metadata[[column]])
        paste0(column, " (", length(unique(stats::na.omit(values))), " classes)")
      }, character(1)))
    } else {
      character(0)
    }
    selectInput(
      "geo_ml_stability_group_column",
      "Optional class/group column for stability seeds:",
      choices = c("All samples together" = "", labels),
      selected = isolate(input$geo_ml_stability_group_column %||% "")
    )
  })

  output$geo_ml_stability_group_summary <- renderUI({
    metadata <- geo_sample_metadata()
    column <- input$geo_ml_stability_group_column %||% ""
    if (!is.data.frame(metadata) || nrow(metadata) == 0) {
      return(tags$p(class = "geo-step-note", "Sample metadata is needed to stratify stability seeds."))
    }
    if (!nzchar(column)) {
      return(tags$p(class = "geo-step-note", "Stability seeds will use all samples together unless a class/group column is selected."))
    }
    strata <- geo_ml_stability_strata(metadata, column)
    if (!is.data.frame(strata) || nrow(strata) == 0) {
      return(tags$p(class = "geo-step-note", "Selected class/group column has no usable sample groups."))
    }
    shown <- utils::head(strata, 12)
    tags$div(class = "geo-step-status",
      tags$p(tags$strong("Class counts: "), paste0(nrow(strata), " class(es) in ", column, ".")),
      tags$p(paste(paste0(shown$StratumValue, "=", shown$StratumSamples), collapse = "; "), if (nrow(strata) > nrow(shown)) " ..." else "")
    )
  })

  output$geo_transcript_ml_progress_ui <- renderUI({
    progress <- geo_transcript_ml_progress()
    remote_result <- remote_job_preview_result()
    remote_loaded <- geo_remote_mode_active() && is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline")
    remote_ml_summary <- if (remote_loaded) remote_result$tables$transcript_ml_summary else data.frame()
    if (!is.data.frame(remote_ml_summary) || nrow(remote_ml_summary) == 0) {
      remote_ml_summary <- if (remote_loaded) remote_result$tables$transcript_ml_screening else data.frame()
    }
    if (remote_loaded && is.data.frame(remote_ml_summary) && nrow(remote_ml_summary) > 0 &&
        !((progress$phase %||% "") %in% c("complete", "loaded from cache", "loaded from remote", "already complete"))) {
      progress <- list(
        phase = "loaded from remote",
        message = paste0("Loaded remote transcript ML summary: ", nrow(remote_ml_summary), " row(s). Large artifacts remain on the remote server."),
        processed = nrow(remote_ml_summary),
        total = nrow(remote_ml_summary),
        current = "",
        cache = remote_result$paths$transcript_ml_summary %||% remote_result$paths$transcript_ml_screening_summary %||% ""
      )
    }
    total <- suppressWarnings(as.integer(progress$total %||% 0L))
    processed <- suppressWarnings(as.integer(progress$processed %||% 0L))
    percent <- if (is.finite(total) && total > 0) round(100 * processed / total, 1) else 0
    tags$div(
      style = "margin-top: 10px; padding: 10px; border: 1px solid #dbe7f3; background: #f8fbff; border-radius: 4px;",
      tags$p(style = "margin: 0 0 4px 0;", tags$strong("Transcript ML status: "), progress$phase %||% "idle"),
      tags$p(style = "margin: 0 0 4px 0;", progress$message %||% ""),
      tags$div(class = "geo-ml-live-bar-track", tags$div(class = "geo-ml-live-bar", style = paste0("width: ", max(0, min(100, percent)), "%;"))),
      tags$p(style = "margin: 0 0 4px 0;", paste0("Processed ", processed, " / ", total, " (", percent, "%).")),
      if (nzchar(progress$current %||% "")) tags$p(style = "margin: 0 0 4px 0;", paste0("Current: ", progress$current)) else NULL,
      if (nzchar(progress$detail %||% "")) tags$p(style = "margin: 0 0 4px 0;", progress$detail) else NULL,
      if (nzchar(progress$stability %||% "")) tags$pre(class = "geo-ml-live-stability", progress$stability) else NULL,
      if (nzchar(progress$cache %||% "")) tags$p(style = "margin: 0; font-size: 12px; color: #596273;", paste0("Cache: ", progress$cache)) else NULL
    )
  })

  output$geo_transcript_ml_table_title <- renderUI({
    results <- geo_transcript_ml_results_current()
    if (!is.data.frame(results) || nrow(results) == 0) {
      return(tags$p(class = "geo-step-note", "No transcript ML results loaded yet."))
    }
    render_geo_table_title("Transcript ML results", "Per-source transcript/group ranking from the local resumable ML pipeline.")
  })

  geo_transcript_ml_results_current <- function() {
    results <- geo_transcript_ml_results()
    if (is.data.frame(results) && nrow(results) > 0) {
      return(results)
    }
    remote_result <- remote_job_preview_result()
    if (geo_remote_mode_active() && is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline")) {
      remote_summary <- remote_result$tables$transcript_ml_summary
      if (!is.data.frame(remote_summary) || nrow(remote_summary) == 0) {
        remote_summary <- remote_result$tables$transcript_ml_screening
      }
      if (is.data.frame(remote_summary) && nrow(remote_summary) > 0) {
        return(remote_summary)
      }
    }
    data.frame()
  }

  geo_transcript_group_details_current <- function() {
    details <- geo_transcript_group_details()
    if (is.data.frame(details) && nrow(details) > 0) {
      return(details)
    }
    remote_result <- remote_job_preview_result()
    if (geo_remote_mode_active() && is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline") &&
        is.data.frame(remote_result$tables$transcript_group_details)) {
      return(remote_result$tables$transcript_group_details)
    }
    data.frame()
  }

  geo_transcript_ml_importance_current <- function() {
    remote_result <- remote_job_preview_result()
    if (geo_remote_mode_active() && is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline") &&
        is.data.frame(remote_result$tables$transcript_ml_importance)) {
      return(remote_result$tables$transcript_ml_importance)
    }
    data.frame()
  }

  geo_transcript_group_dataset_remote <- function(group_id) {
    group_id <- as.character(group_id %||% "")
    remote_result <- remote_job_preview_result()
    datasets <- if (geo_remote_mode_active() && is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline")) {
      remote_result$tables$transcript_group_datasets
    } else {
      NULL
    }
    if (!is.list(datasets) || !nzchar(group_id) || is.null(datasets[[group_id]])) {
      return(data.frame())
    }
    dataset <- datasets[[group_id]]
    if (is.data.frame(dataset)) dataset else data.frame()
  }

  geo_transcript_ml_class_rank_rows_for <- function(rank_mode = c("r2", "spearman", "combined")) {
    rank_mode <- match.arg(rank_mode)
    results <- geo_transcript_ml_results_current()
    required <- c("GroupID", "PrincipalTranscript", "Gene")
    if (!is.data.frame(results) || nrow(results) == 0 || !all(required %in% names(results))) {
      return(data.frame())
    }
    if (!"StratumColumn" %in% names(results)) results$StratumColumn <- ""
    if (!"StratumValue" %in% names(results)) results$StratumValue <- ""
    if (!"StratumSamples" %in% names(results)) results$StratumSamples <- NA_integer_
    rows <- results[
      nzchar(as.character(results$StratumColumn %||% "")) &
        nzchar(as.character(results$StratumValue %||% "")) &
        !is.na(results$StratumColumn) &
        !is.na(results$StratumValue),
      ,
      drop = FALSE
    ]
    if (!is.data.frame(rows) || nrow(rows) == 0) {
      rows <- results
      rows$StratumColumn <- ""
      rows$StratumValue <- "All samples"
    }
    if (!"TriggerBestCpG" %in% names(rows)) {
      rows$TriggerBestCpG <- ""
    }
    if (!"TriggerBestRho" %in% names(rows)) {
      rows$TriggerBestRho <- NA_real_
    }
    missing_cpg <- !nzchar(as.character(rows$TriggerBestCpG %||% "")) | is.na(rows$TriggerBestCpG)
    details <- geo_transcript_group_details_current()
    if (any(missing_cpg) && is.data.frame(details) && nrow(details) > 0 && all(c("GroupID", "CpG", "AbsRho") %in% names(details))) {
      detail_absrho <- suppressWarnings(as.numeric(details$AbsRho))
      detail_rows <- details[is.finite(detail_absrho), , drop = FALSE]
      detail_absrho <- detail_absrho[is.finite(detail_absrho)]
      best_by_group <- lapply(split(seq_len(nrow(detail_rows)), as.character(detail_rows$GroupID)), function(idx) {
        best_idx <- idx[which.max(detail_absrho[idx])]
        detail_rows[best_idx, , drop = FALSE]
      })
      best_by_group <- bind_summary_rows(best_by_group)
      if (is.data.frame(best_by_group) && nrow(best_by_group) > 0) {
        group_match <- match(as.character(rows$GroupID), as.character(best_by_group$GroupID))
        fill <- missing_cpg & !is.na(group_match)
        rows$TriggerBestCpG[fill] <- as.character(best_by_group$CpG[group_match[fill]])
        if ("SpearmanRho" %in% names(best_by_group)) {
          rows$TriggerBestRho[fill] <- suppressWarnings(as.numeric(best_by_group$SpearmanRho[group_match[fill]]))
        }
      }
    }
    rows$ModelMetric <- if ("MedianMetric" %in% names(rows)) suppressWarnings(as.numeric(rows$MedianMetric)) else NA_real_
    rows$SpearmanMetric <- if ("TriggerMaxAbsRho" %in% names(rows)) suppressWarnings(as.numeric(rows$TriggerMaxAbsRho)) else NA_real_
    rows$Metric <- switch(rank_mode,
      r2 = rows$ModelMetric,
      spearman = rows$SpearmanMetric,
      combined = ifelse(is.finite(rows$ModelMetric) & is.finite(rows$SpearmanMetric), 1, NA_real_)
    )
    if (!any(is.finite(rows$Metric))) {
      return(data.frame())
    }
    if (!"CombinedRank" %in% names(rows)) {
      rows$CombinedRank <- NA_real_
    }
    if (!"StratumSamples" %in% names(rows)) {
      rows$StratumSamples <- NA_integer_
    }
    rows <- rows[is.finite(rows$Metric), , drop = FALSE]
    strata <- unique(as.character(rows$StratumValue))
    strata <- strata[nzchar(strata)]
    stratum_column_idx <- which(nzchar(as.character(rows$StratumColumn)))
    if (length(stratum_column_idx) > 0) {
      stratum_column <- as.character(rows$StratumColumn[[stratum_column_idx[[1]]]])
      metadata_strata <- geo_ml_stability_strata(geo_sample_metadata(), stratum_column)
      if (is.data.frame(metadata_strata) && nrow(metadata_strata) > 0) {
        metadata_order <- as.character(metadata_strata$StratumValue)
        strata <- c(intersect(metadata_order, strata), setdiff(strata, metadata_order))
      }
    }
    custom_order <- input$geo_ml_class_compare_order %||% character(0)
    custom_order <- trimws(as.character(custom_order))
    custom_order <- custom_order[nzchar(custom_order)]
    if (!is.null(input$geo_ml_class_compare_order)) {
      strata <- intersect(custom_order, strata)
    } else if (length(custom_order) > 0) {
      strata <- c(intersect(custom_order, strata), setdiff(strata, custom_order))
    }
    if (length(strata) == 0) {
      return(data.frame())
    }
    ranked <- lapply(seq_along(strata), function(stratum_i) {
      stratum <- strata[[stratum_i]]
      class_rows <- rows[as.character(rows$StratumValue) == stratum, , drop = FALSE]
      if (identical(rank_mode, "combined")) {
        model_rank <- if ("ModelRank" %in% names(class_rows)) suppressWarnings(as.numeric(class_rows$ModelRank)) else rep(NA_real_, nrow(class_rows))
        rho_rank <- if ("RhoRank" %in% names(class_rows)) suppressWarnings(as.numeric(class_rows$RhoRank)) else rep(NA_real_, nrow(class_rows))
        if (!any(is.finite(model_rank))) {
          model_rank <- rank(-class_rows$ModelMetric, ties.method = "min", na.last = "keep")
        }
        if (!any(is.finite(rho_rank))) {
          rho_rank <- rank(-class_rows$SpearmanMetric, ties.method = "min", na.last = "keep")
        }
        class_rows$ModelRankForOrder <- model_rank
        class_rows$RhoRankForOrder <- rho_rank
        class_rows$Metric <- model_rank + rho_rank
        class_rows <- class_rows[is.finite(class_rows$Metric), , drop = FALSE]
        class_rows <- class_rows[order(class_rows$Metric, class_rows$PrincipalTranscript, class_rows$GroupID), , drop = FALSE]
        max_combined <- suppressWarnings(max(class_rows$Metric, na.rm = TRUE))
        class_rows$PlotMetric <- if (is.finite(max_combined) && max_combined > 1) {
          1 - ((class_rows$Metric - 1) / (max_combined - 1))
        } else {
          rep(1, nrow(class_rows))
        }
        class_rows$PlotY <- class_rows$PlotMetric
      } else {
        class_rows <- class_rows[order(-class_rows$Metric, class_rows$PrincipalTranscript, class_rows$GroupID), , drop = FALSE]
        class_rows$PlotMetric <- class_rows$Metric
        class_rows$PlotY <- class_rows$Metric
      }
      class_rows$Order <- seq_len(nrow(class_rows))
      class_rows$StratumOrder <- stratum_i
      class_rows$RankMode <- rank_mode
      class_rows$TranscriptLabel <- paste0(
        as.character(class_rows$GroupID),
        " | ",
        as.character(class_rows$PrincipalTranscript),
        " / ",
        as.character(class_rows$Gene)
      )
      class_rows
    })
    ranked <- bind_summary_rows(ranked)
    if (!is.data.frame(ranked) || nrow(ranked) == 0) {
      return(data.frame())
    }
    ranked$StratumValue <- factor(as.character(ranked$StratumValue), levels = strata)
    ranked
  }

  geo_transcript_ml_class_rank_rows <- reactive({
    geo_transcript_ml_class_rank_rows_for("r2")
  })

  output$geo_transcript_ml_class_order_control <- renderUI({
    results <- geo_transcript_ml_results_current()
    if (!is.data.frame(results) || nrow(results) == 0 ||
        !all(c("StratumColumn", "StratumValue") %in% names(results))) {
      return(NULL)
    }
    rows <- results[
      nzchar(as.character(results$StratumColumn %||% "")) &
        nzchar(as.character(results$StratumValue %||% "")) &
        !is.na(results$StratumColumn) &
        !is.na(results$StratumValue),
      ,
      drop = FALSE
    ]
    if (!is.data.frame(rows) || nrow(rows) == 0) {
      return(NULL)
    }
    strata <- unique(as.character(rows$StratumValue))
    strata <- strata[nzchar(strata)]
    stratum_column_idx <- which(nzchar(as.character(rows$StratumColumn)))
    if (length(stratum_column_idx) > 0) {
      stratum_column <- as.character(rows$StratumColumn[[stratum_column_idx[[1]]]])
      metadata_strata <- geo_ml_stability_strata(geo_sample_metadata(), stratum_column)
      if (is.data.frame(metadata_strata) && nrow(metadata_strata) > 0) {
        metadata_order <- as.character(metadata_strata$StratumValue)
        strata <- c(intersect(metadata_order, strata), setdiff(strata, metadata_order))
      }
    }
    if (length(strata) < 2) {
      return(NULL)
    }
    selected <- isolate(input$geo_ml_class_compare_order)
    selected <- if (is.null(selected)) strata else intersect(trimws(as.character(selected)), strata)
    tags$div(
      style = "max-width: 520px; margin: 8px 0 14px 0;",
      selectizeInput(
        "geo_ml_class_compare_order",
        "Class order in plot/table:",
        choices = strata,
        selected = selected,
        multiple = TRUE,
        options = list(
          plugins = list("drag_drop", "remove_button"),
          persist = FALSE,
          create = FALSE,
          placeholder = "Drag classes to reorder"
        )
      ),
      tags$p(
        class = "geo-step-note",
        "Drag the selected class tags to reorder the comparison."
      )
    )
  })

  geo_transcript_ml_class_compare_for <- function(ranked, metric_label = "R2") {
    if (!is.data.frame(ranked) || nrow(ranked) == 0) {
      return(data.frame())
    }
    strata <- levels(ranked$StratumValue)
    ordered_by_class <- lapply(strata, function(stratum) {
      class_rows <- ranked[as.character(ranked$StratumValue) == stratum, , drop = FALSE]
      class_rows[order(class_rows$Order), , drop = FALSE]
    })
    max_order <- max(vapply(ordered_by_class, nrow, integer(1)))
    compare <- data.frame(Order = seq_len(max_order))
    for (i in seq_along(strata)) {
      stratum <- strata[[i]]
      class_rows <- ordered_by_class[[i]]
      transcript_col <- rep(NA_character_, max_order)
      if (nrow(class_rows) > 0) {
        idx <- class_rows$Order
        transcript_label <- paste0(
          as.character(class_rows$PrincipalTranscript),
          " / ",
          as.character(class_rows$Gene),
          " | R2=",
          signif(class_rows$ModelMetric, 5),
          " | |rho|=",
          signif(class_rows$SpearmanMetric, 5)
        )
        if ("TriggerBestCpG" %in% names(class_rows)) {
          transcript_label <- paste0(transcript_label, " | CpG=", as.character(class_rows$TriggerBestCpG))
        }
        if (identical(metric_label, "combined")) {
          transcript_label <- paste0(transcript_label, " | rank sum=", signif(class_rows$Metric, 5))
        }
        if ("GroupID" %in% names(class_rows)) {
          transcript_label <- paste0(as.character(class_rows$GroupID), " | ", transcript_label)
        }
        transcript_col[idx] <- transcript_label
      }
      compare[[stratum]] <- transcript_col
    }
    rownames(compare) <- NULL
    compare
  }

  geo_transcript_ml_class_compare <- reactive({
    geo_transcript_ml_class_compare_for(geo_transcript_ml_class_rank_rows(), "r2")
  })

  geo_transcript_ml_class_change_for <- function(ranked, reference_class = NULL, comparison_class = NULL) {
    if (!is.data.frame(ranked) || nrow(ranked) == 0 || !"StratumValue" %in% names(ranked)) {
      return(data.frame())
    }
    strata <- levels(ranked$StratumValue)
    strata <- strata[nzchar(as.character(strata))]
    if (length(strata) < 2) {
      return(data.frame())
    }
    reference_class <- as.character(reference_class %||% "")
    comparison_class <- as.character(comparison_class %||% "")
    if (!nzchar(reference_class) || !reference_class %in% strata) {
      reference_class <- strata[[1]]
    }
    if (!nzchar(comparison_class) || !comparison_class %in% strata) {
      comparison_class <- strata[[length(strata)]]
    }
    if (identical(reference_class, comparison_class)) {
      return(data.frame())
    }
    ref_rows <- ranked[as.character(ranked$StratumValue) == reference_class, , drop = FALSE]
    cmp_rows <- ranked[as.character(ranked$StratumValue) == comparison_class, , drop = FALSE]
    if (!is.data.frame(ref_rows) || !is.data.frame(cmp_rows) || nrow(ref_rows) == 0 || nrow(cmp_rows) == 0 ||
        !"GroupID" %in% names(ref_rows) || !"GroupID" %in% names(cmp_rows)) {
      return(data.frame())
    }
    keep_cols <- intersect(
      c("GroupID", "PrincipalTranscript", "Gene", "Order", "ModelMetric", "SpearmanMetric", "TriggerBestCpG", "TriggerBestRho", "Metric"),
      names(ranked)
    )
    ref_rows <- ref_rows[, keep_cols, drop = FALSE]
    cmp_rows <- cmp_rows[, keep_cols, drop = FALSE]
    names(ref_rows) <- paste0(names(ref_rows), "Reference")
    names(cmp_rows) <- paste0(names(cmp_rows), "Comparison")
    changes <- merge(
      ref_rows,
      cmp_rows,
      by.x = "GroupIDReference",
      by.y = "GroupIDComparison",
      all = FALSE,
      sort = FALSE
    )
    if (!is.data.frame(changes) || nrow(changes) == 0) {
      return(data.frame())
    }
    changes$GroupID <- as.character(changes$GroupIDReference)
    transcript <- if ("PrincipalTranscriptReference" %in% names(changes)) changes$PrincipalTranscriptReference else changes$PrincipalTranscriptComparison
    gene <- if ("GeneReference" %in% names(changes)) changes$GeneReference else changes$GeneComparison
    changes$PrincipalTranscript <- as.character(transcript)
    changes$Gene <- as.character(gene)
    changes$ReferenceClass <- reference_class
    changes$ComparisonClass <- comparison_class
    changes$ReferenceOrder <- suppressWarnings(as.integer(round(as.numeric(changes$OrderReference))))
    changes$ComparisonOrder <- suppressWarnings(as.integer(round(as.numeric(changes$OrderComparison))))
    changes$OrderDelta <- changes$ComparisonOrder - changes$ReferenceOrder
    changes$AbsOrderDelta <- abs(changes$OrderDelta)
    changes$ReferenceR2 <- suppressWarnings(as.numeric(changes$ModelMetricReference))
    changes$ComparisonR2 <- suppressWarnings(as.numeric(changes$ModelMetricComparison))
    changes$DeltaR2 <- changes$ComparisonR2 - changes$ReferenceR2
    changes$ReferenceAbsRho <- suppressWarnings(as.numeric(changes$SpearmanMetricReference))
    changes$ComparisonAbsRho <- suppressWarnings(as.numeric(changes$SpearmanMetricComparison))
    changes$DeltaAbsRho <- changes$ComparisonAbsRho - changes$ReferenceAbsRho
    rank_mode <- as.character(ranked$RankMode[[1]] %||% "r2")
    if (identical(rank_mode, "spearman")) {
      changes$ReferenceValue <- changes$ReferenceAbsRho
      changes$ComparisonValue <- changes$ComparisonAbsRho
    } else if (identical(rank_mode, "combined")) {
      changes$ReferenceValue <- suppressWarnings(as.numeric(changes$MetricReference))
      changes$ComparisonValue <- suppressWarnings(as.numeric(changes$MetricComparison))
    } else {
      changes$ReferenceValue <- changes$ReferenceR2
      changes$ComparisonValue <- changes$ComparisonR2
    }
    changes$Delta <- changes$ComparisonValue - changes$ReferenceValue
    changes$ReferenceBestCpG <- if ("TriggerBestCpGReference" %in% names(changes)) as.character(changes$TriggerBestCpGReference) else ""
    changes$ComparisonBestCpG <- if ("TriggerBestCpGComparison" %in% names(changes)) as.character(changes$TriggerBestCpGComparison) else ""
    changes$Direction <- ifelse(
      !is.finite(changes$OrderDelta) | changes$OrderDelta == 0,
      "same rank",
      ifelse(changes$OrderDelta < 0, "moves up", "moves down")
    )
    changes$ChangeSummary <- paste0(
      changes$GroupID,
      " | ",
      changes$PrincipalTranscript,
      " / ",
      changes$Gene,
      " | ",
      reference_class,
      " #",
      changes$ReferenceOrder,
      " -> ",
      comparison_class,
      " #",
      changes$ComparisonOrder,
      " (",
      changes$Direction,
      ")"
    )
    changes <- changes[order(
      -abs(changes$Delta),
      -changes$AbsOrderDelta,
      changes$ComparisonOrder,
      changes$GroupID
    ), , drop = FALSE]
    display_cols <- c(
      "GroupID", "PrincipalTranscript", "Gene", "Delta", "ReferenceValue", "ComparisonValue",
      "ReferenceClass", "ComparisonClass", "ReferenceOrder", "ComparisonOrder", "OrderDelta", "Direction",
      "ReferenceR2", "ComparisonR2", "DeltaR2",
      "ReferenceAbsRho", "ComparisonAbsRho", "DeltaAbsRho",
      "ReferenceBestCpG", "ComparisonBestCpG", "ChangeSummary"
    )
    changes[, intersect(display_cols, names(changes)), drop = FALSE]
  }

  geo_transcript_ml_class_change <- reactive({
    geo_transcript_ml_class_change_for(
      geo_transcript_ml_class_rank_rows_for("r2"),
      input$geo_ml_class_change_reference_r2,
      input$geo_ml_class_change_comparison_r2
    )
  })

  geo_transcript_ml_render_change_title <- function(changes, metric_label) {
    if (!is.data.frame(changes) || nrow(changes) == 0) {
      return(NULL)
    }
    tags$div(
      tags$h4(paste0("Largest transcript changes by ", metric_label)),
      tags$p(
        class = "geo-step-note",
        paste0(
          "Showing ", changes$ReferenceClass[[1]], " -> ", changes$ComparisonClass[[1]],
          ". Rows are sorted by absolute delta for the active metric."
        )
      )
    )
  }

  geo_transcript_ml_render_change_table <- function(changes) {
    req(is.data.frame(changes), nrow(changes) > 0)
    display <- changes[, intersect(c("GroupID", "PrincipalTranscript", "Gene", "Delta", "ReferenceValue", "ComparisonValue"), names(changes)), drop = FALSE]
    names(display)[names(display) == "GroupID"] <- "Group"
    names(display)[names(display) == "PrincipalTranscript"] <- "Transcript"
    for (metric_col in intersect(c("Delta", "ReferenceValue", "ComparisonValue"), names(display))) {
      display[[metric_col]] <- signif(suppressWarnings(as.numeric(display[[metric_col]])), 5)
    }
    DT::datatable(display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, selection = "single")
  }

  geo_transcript_ml_class_change_controls_ui <- function(ranked, reference_id, comparison_id) {
    if (!is.data.frame(ranked) || nrow(ranked) == 0 || !"StratumValue" %in% names(ranked)) {
      return(NULL)
    }
    strata <- levels(ranked$StratumValue)
    strata <- strata[nzchar(as.character(strata))]
    if (length(strata) < 2) {
      return(NULL)
    }
    reference_selected <- input[[reference_id]] %||% strata[[1]]
    comparison_selected <- input[[comparison_id]] %||% strata[[length(strata)]]
    if (!reference_selected %in% strata) reference_selected <- strata[[1]]
    if (!comparison_selected %in% strata) comparison_selected <- strata[[length(strata)]]
    tags$div(
      style = "display: flex; gap: 12px; align-items: flex-end; flex-wrap: wrap; margin: 4px 0 14px 0;",
      tags$div(
        style = "width: 220px;",
        selectInput(reference_id, "Reference class:", choices = strata, selected = reference_selected)
      ),
      tags$div(
        style = "width: 220px;",
        selectInput(comparison_id, "Comparison class:", choices = strata, selected = comparison_selected)
      ),
      tags$p(
        class = "geo-step-note",
        style = "margin: 0 0 15px 0;",
        "Delta tables use these two classes. Defaults are the first and last selected class tags."
      )
    )
  }

  output$geo_transcript_ml_class_change_controls_r2 <- renderUI({
    geo_transcript_ml_class_change_controls_ui(
      geo_transcript_ml_class_rank_rows_for("r2"),
      "geo_ml_class_change_reference_r2",
      "geo_ml_class_change_comparison_r2"
    )
  })

  output$geo_transcript_ml_class_change_controls_spearman <- renderUI({
    geo_transcript_ml_class_change_controls_ui(
      geo_transcript_ml_class_rank_rows_for("spearman"),
      "geo_ml_class_change_reference_spearman",
      "geo_ml_class_change_comparison_spearman"
    )
  })

  output$geo_transcript_ml_class_change_controls_combined <- renderUI({
    geo_transcript_ml_class_change_controls_ui(
      geo_transcript_ml_class_rank_rows_for("combined"),
      "geo_ml_class_change_reference_combined",
      "geo_ml_class_change_comparison_combined"
    )
  })

  output$geo_transcript_ml_class_compare_title <- renderUI({
    compare <- geo_transcript_ml_class_compare()
    if (!is.data.frame(compare) || nrow(compare) == 0) {
      return(NULL)
    }
      tags$div(
      tags$h4("Transcript class comparison"),
      tags$p(class = "geo-step-note", "Each row is one rank position. Use the tabs to compare median-R2 order, Spearman-trigger order, or the combined R2 + Spearman rank inside each metadata group.")
    )
  })

  geo_transcript_ml_class_rank_plot_obj <- function(ranked, title, y_title, color_title, hover_metric_label) {
    req(is.data.frame(ranked), nrow(ranked) > 0)
    ranked$Order <- suppressWarnings(as.numeric(ranked$Order))
    ranked$Metric <- suppressWarnings(as.numeric(ranked$Metric))
    ranked$PlotMetric <- suppressWarnings(as.numeric(ranked$PlotMetric %||% ranked$Metric))
    ranked$PlotY <- suppressWarnings(as.numeric(ranked$PlotY %||% ranked$Metric))
    ranked$ModelMetric <- suppressWarnings(as.numeric(ranked$ModelMetric))
    ranked$SpearmanMetric <- suppressWarnings(as.numeric(ranked$SpearmanMetric))
    ranked$StratumOrder <- suppressWarnings(as.numeric(ranked$StratumOrder))
    ranked <- ranked[is.finite(ranked$Order) & is.finite(ranked$StratumOrder) & is.finite(ranked$PlotY), , drop = FALSE]
    req(nrow(ranked) > 0)
    transcript_labels <- unique(as.character(ranked$TranscriptLabel))
    palette <- grDevices::hcl.colors(max(3, length(transcript_labels)), palette = "Dark 3")
    names(palette) <- transcript_labels
    plot_obj <- plotly::plot_ly(source = "geo_ml_class_rank")
    for (label in transcript_labels) {
      transcript_rows <- ranked[as.character(ranked$TranscriptLabel) == label, , drop = FALSE]
      transcript_rows <- transcript_rows[order(transcript_rows$StratumOrder), , drop = FALSE]
      hover <- paste0(
        "<b>", transcript_rows$TranscriptLabel, "</b>",
        "<br>Class: ", as.character(transcript_rows$StratumValue),
        "<br>Order: ", transcript_rows$Order,
        "<br>Median R2: ", signif(transcript_rows$ModelMetric, 5),
        "<br>Best Spearman |rho|: ", signif(transcript_rows$SpearmanMetric, 5),
        if ("TriggerBestCpG" %in% names(transcript_rows)) paste0("<br>Best CpG: ", as.character(transcript_rows$TriggerBestCpG)) else "",
        if ("TriggerBestRho" %in% names(transcript_rows)) paste0("<br>Best rho: ", signif(suppressWarnings(as.numeric(transcript_rows$TriggerBestRho)), 5)) else "",
        "<br>", hover_metric_label, ": ", signif(transcript_rows$Metric, 5),
        "<extra></extra>"
      )
      plot_obj <- plotly::add_trace(
        plot_obj,
        data = transcript_rows,
        x = ~StratumOrder,
        y = ~PlotY,
        type = "scatter",
        mode = "lines+markers+text",
        key = ~paste(GroupID, StratumColumn, as.character(StratumValue), sep = "\r"),
        customdata = ~paste(GroupID, StratumColumn, as.character(StratumValue), sep = "\r"),
        text = ~GroupID,
        textposition = "middle right",
        hovertemplate = hover,
        name = label,
        line = list(color = palette[[label]], width = 2),
        marker = list(
          color = ~PlotMetric,
          colorscale = "Viridis",
          cmin = 0,
          cmax = 1,
          showscale = identical(label, transcript_labels[[1]]),
          size = 12,
          line = list(color = palette[[label]], width = 2),
          colorbar = list(title = color_title)
        ),
        showlegend = TRUE
      )
    }
    strata <- levels(ranked$StratumValue)
    plotly::layout(
      plot_obj,
      title = list(text = title),
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = seq_along(strata),
        ticktext = strata
      ),
      yaxis = list(
        title = y_title,
        range = c(0, 1),
        tickmode = "linear"
      ),
      margin = list(l = 70, r = 30, t = 55, b = 55),
      legend = list(orientation = "h", x = 0, y = -0.18),
      hovermode = "closest"
    )
  }

  output$geo_transcript_ml_class_rank_plot <- renderPlotly({
    geo_transcript_ml_class_rank_plot_obj(
      geo_transcript_ml_class_rank_rows_for("r2"),
      "Transcript order change by class",
      "Median R2",
      "R2",
      "R2 used for order"
    )
  })

  output$geo_transcript_ml_class_spearman_plot <- renderPlotly({
    geo_transcript_ml_class_rank_plot_obj(
      geo_transcript_ml_class_rank_rows_for("spearman"),
      "Spearman trigger order change by class",
      "Best CpG Spearman |rho|",
      "|rho|",
      "Spearman |rho| used for order"
    )
  })

  output$geo_transcript_ml_class_combined_plot <- renderPlotly({
    geo_transcript_ml_class_rank_plot_obj(
      geo_transcript_ml_class_rank_rows_for("combined"),
      "Combined R2 + Spearman order change by class",
      "Combined score",
      "Score",
      "Combined rank sum"
    )
  })

  output$geo_transcript_ml_class_compare_table <- DT::renderDT({
    compare <- geo_transcript_ml_class_compare()
    req(is.data.frame(compare), nrow(compare) > 0)
    compare$Order <- suppressWarnings(as.integer(round(as.numeric(compare$Order))))
    DT::datatable(compare, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$geo_transcript_ml_class_change_title <- renderUI({
    geo_transcript_ml_render_change_title(geo_transcript_ml_class_change(), "R2")
  })

  output$geo_transcript_ml_class_change_table <- DT::renderDT({
    geo_transcript_ml_render_change_table(geo_transcript_ml_class_change())
  })

  output$geo_transcript_ml_class_spearman_table <- DT::renderDT({
    compare <- geo_transcript_ml_class_compare_for(geo_transcript_ml_class_rank_rows_for("spearman"), "spearman")
    req(is.data.frame(compare), nrow(compare) > 0)
    compare$Order <- suppressWarnings(as.integer(round(as.numeric(compare$Order))))
    DT::datatable(compare, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$geo_transcript_ml_class_spearman_change_title <- renderUI({
    geo_transcript_ml_render_change_title(
      geo_transcript_ml_class_change_for(
        geo_transcript_ml_class_rank_rows_for("spearman"),
        input$geo_ml_class_change_reference_spearman,
        input$geo_ml_class_change_comparison_spearman
      ),
      "Spearman"
    )
  })

  output$geo_transcript_ml_class_spearman_change_table <- DT::renderDT({
    geo_transcript_ml_render_change_table(geo_transcript_ml_class_change_for(
      geo_transcript_ml_class_rank_rows_for("spearman"),
      input$geo_ml_class_change_reference_spearman,
      input$geo_ml_class_change_comparison_spearman
    ))
  })

  output$geo_transcript_ml_class_combined_table <- DT::renderDT({
    compare <- geo_transcript_ml_class_compare_for(geo_transcript_ml_class_rank_rows_for("combined"), "combined")
    req(is.data.frame(compare), nrow(compare) > 0)
    compare$Order <- suppressWarnings(as.integer(round(as.numeric(compare$Order))))
    DT::datatable(compare, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$geo_transcript_ml_class_combined_change_title <- renderUI({
    geo_transcript_ml_render_change_title(
      geo_transcript_ml_class_change_for(
        geo_transcript_ml_class_rank_rows_for("combined"),
        input$geo_ml_class_change_reference_combined,
        input$geo_ml_class_change_comparison_combined
      ),
      "combined rank"
    )
  })

  output$geo_transcript_ml_class_combined_change_table <- DT::renderDT({
    geo_transcript_ml_render_change_table(geo_transcript_ml_class_change_for(
      geo_transcript_ml_class_rank_rows_for("combined"),
      input$geo_ml_class_change_reference_combined,
      input$geo_ml_class_change_comparison_combined
    ))
  })

  geo_transcript_ml_final_rows <- reactive({
    short_correlation_method <- function(method) {
      method <- tolower(trimws(as.character(method %||% "")))
      if (identical(method, "pearson")) return("P")
      if (identical(method, "spearman")) return("S")
      if (nzchar(method)) return(toupper(substr(method, 1, 1)))
      "R"
    }
    format_cpg_correlation_label <- function(cpg, method, value) {
      if (!is.finite(value)) {
        return(cpg)
      }
      paste0(cpg, "(", short_correlation_method(method), "=", sprintf("%.2f", value), ")")
    }
    reformat_correlation_cell <- function(value) {
      value <- trimws(as.character(value %||% ""))
      if (!nzchar(value)) {
        return("")
      }
      parts <- unlist(strsplit(value, "[;\n]+"), use.names = FALSE)
      parts <- trimws(parts)
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) {
        return("")
      }
      formatted <- vapply(parts, function(part) {
        match <- regexec("^([^[:space:]]+)\\s+([[:alpha:]]+)=([-+0-9.eE]+)$", part)
        pieces <- regmatches(part, match)[[1]]
        if (length(pieces) == 4) {
          parsed <- suppressWarnings(as.numeric(pieces[[4]]))
          return(format_cpg_correlation_label(pieces[[2]], pieces[[3]], parsed))
        }
        match_compact <- regexec("^([^()]+)\\(([[:alpha:]])=([-+0-9.eE]+)\\)$", part)
        compact <- regmatches(part, match_compact)[[1]]
        if (length(compact) == 4) {
          parsed <- suppressWarnings(as.numeric(compact[[4]]))
          return(format_cpg_correlation_label(compact[[2]], compact[[3]], parsed))
        }
        part
      }, character(1))
      paste(formatted, collapse = "\n")
    }
    normalize_paper_final <- function(final) {
      if (!is.data.frame(final) || nrow(final) == 0) {
        return(data.frame())
      }
      rename_map <- c(
        GroupID = "Group ID",
        Transcripts = "Transcript(s)",
        MedianR2 = "Median R2",
        MinR2 = "Min R2",
        MaxR2 = "Max R2",
        MedianMAE = "Median MAE",
        WBR2 = "wB R2",
        ShuffleMaxR2 = "Shuffle max R2",
        BestSource = "Best source"
      )
      for (old_name in intersect(names(rename_map), names(final))) {
        names(final)[names(final) == old_name] <- rename_map[[old_name]]
      }
      if ("Correlation" %in% names(final)) {
        final$Correlation <- vapply(final$Correlation, reformat_correlation_cell, character(1))
      }
      final
    }
    remote_result <- remote_job_preview_result()
    if (geo_remote_mode_active() && is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline") &&
        is.data.frame(remote_result$tables$transcript_ml_final) && nrow(remote_result$tables$transcript_ml_final) > 0) {
      return(normalize_paper_final(remote_result$tables$transcript_ml_final))
    }
    results <- geo_transcript_ml_results_current()
    if (!is.data.frame(results) || nrow(results) == 0 || !"GroupID" %in% names(results)) {
      return(data.frame())
    }
    details <- geo_transcript_group_details_current()
    result_cache <- new.env(parent = emptyenv())
    flatten_values <- function(values) {
      values <- unlist(values, recursive = TRUE, use.names = FALSE)
      if (is.null(values)) {
        return(character(0))
      }
      values
    }
    trim_nonempty <- function(values) {
      values <- flatten_values(values)
      values <- unique(trimws(as.character(stats::na.omit(values))))
      values[nzchar(values)]
    }
    first_text <- function(row, columns, default = "") {
      hit <- intersect(columns, names(row))
      if (length(hit) == 0) {
        return(default)
      }
      values <- trim_nonempty(row[[hit[[1]]]][[1]])
      value <- if (length(values) > 0) values[[1]] else default
      if (is.na(value)) default else value
    }
    first_number <- function(row, columns) {
      hit <- intersect(columns, names(row))
      if (length(hit) == 0) {
        return(NA_real_)
      }
      values <- flatten_values(row[[hit[[1]]]][[1]])
      value <- suppressWarnings(as.numeric(values))
      value <- value[is.finite(value)]
      if (length(value) > 0) value[[1]] else NA_real_
    }
    metric_vector <- function(df, columns) {
      hit <- intersect(columns, names(df))
      if (length(hit) == 0) {
        return(rep(NA_real_, nrow(df)))
      }
      vapply(seq_len(nrow(df)), function(i) {
        value <- suppressWarnings(as.numeric(flatten_values(df[[hit[[1]]]][[i]])))
        value <- value[is.finite(value)]
        if (length(value) > 0) value[[1]] else NA_real_
      }, numeric(1))
    }
    result_object_for_row <- function(row) {
      path <- first_text(row, c("StabilityResultPath", "ScreenResultPath", "ResultPath"))
      if (!nzchar(path) || !file.exists(path)) {
        return(NULL)
      }
      cache_key <- normalizePath(path, winslash = "/", mustWork = FALSE)
      if (exists(cache_key, envir = result_cache, inherits = FALSE)) {
        return(get(cache_key, envir = result_cache, inherits = FALSE))
      }
      result <- tryCatch(readRDS(path), error = function(e) NULL)
      assign(cache_key, result, envir = result_cache)
      result
    }
    summary_number <- function(row, result, columns, summary_fields = character(0)) {
      value <- first_number(row, columns)
      if (is.finite(value)) {
        return(value)
      }
      if (is.list(result) && is.list(result$final_summary)) {
        for (field in summary_fields) {
          value <- suppressWarnings(as.numeric(flatten_values(result$final_summary[[field]] %||% NA_real_)))
          value <- value[is.finite(value)]
          if (length(value) > 0) {
            return(value[[1]])
          }
        }
      }
      NA_real_
    }
    cpg_label <- function(cpg, method, value) {
      if (!is.finite(value)) {
        return(cpg)
      }
      format_cpg_correlation_label(cpg, method, value)
    }
    best_cpgs_for_group <- function(group_id, summary_row, limit = 5L) {
      group_details <- if (is.data.frame(details) && nrow(details) > 0 && "GroupID" %in% names(details)) {
        details[as.character(details$GroupID) == group_id, , drop = FALSE]
      } else {
        data.frame()
      }
      if (!is.data.frame(group_details) || nrow(group_details) == 0 || !"CpG" %in% names(group_details)) {
        cpg <- first_text(summary_row, c("TriggerBestCpG", "BestCpG"))
        rho <- first_number(summary_row, c("TriggerBestRho", "BestRho"))
        method <- if (is.finite(rho)) "spearman" else ""
        return(list(
          labels = if (nzchar(cpg)) cpg_label(cpg, method, rho) else "",
          best_method = method,
          best_value = abs(rho)
        ))
      }
      spearman <- metric_vector(group_details, c("SpearmanRho", "TriggerBestRho", "BestRho"))
      spearman_abs <- metric_vector(group_details, c("AbsRho", "TriggerMaxAbsRho", "MaxAbsRho"))
      spearman_abs[!is.finite(spearman_abs)] <- abs(spearman[!is.finite(spearman_abs)])
      pearson <- metric_vector(group_details, c("PearsonR", "PearsonRho", "PearsonCorrelation", "PearsonCorr", "Pearson"))
      pearson_abs <- metric_vector(group_details, c("AbsPearsonR", "PearsonAbsR", "AbsPearsonRho", "PearsonAbsRho"))
      pearson_abs[!is.finite(pearson_abs)] <- abs(pearson[!is.finite(pearson_abs)])
      use_pearson <- is.finite(pearson_abs) & (!is.finite(spearman_abs) | pearson_abs > spearman_abs)
      chosen_abs <- ifelse(use_pearson, pearson_abs, spearman_abs)
      chosen_value <- ifelse(use_pearson, pearson, spearman)
      chosen_method <- ifelse(use_pearson, "pearson", "spearman")
      valid <- is.finite(chosen_abs) & nzchar(as.character(group_details$CpG))
      if (!any(valid)) {
        cpgs <- trim_nonempty(group_details$CpG)
        return(list(
          labels = paste(utils::head(cpgs, limit), collapse = "\n"),
          best_method = "",
          best_value = NA_real_
        ))
      }
      cpg_rows <- data.frame(
        CpG = as.character(group_details$CpG[valid]),
        Method = chosen_method[valid],
        Value = chosen_value[valid],
        AbsValue = chosen_abs[valid],
        stringsAsFactors = FALSE
      )
      cpg_rows <- cpg_rows[order(-cpg_rows$AbsValue, cpg_rows$CpG), , drop = FALSE]
      cpg_rows <- cpg_rows[!duplicated(cpg_rows$CpG), , drop = FALSE]
      top_rows <- utils::head(cpg_rows, limit)
      list(
        labels = paste(vapply(seq_len(nrow(top_rows)), function(i) {
          cpg_label(top_rows$CpG[[i]], top_rows$Method[[i]], top_rows$Value[[i]])
        }, character(1)), collapse = "\n"),
        best_method = top_rows$Method[[1]],
        best_value = top_rows$AbsValue[[1]]
      )
    }
    cpg_count_for_group <- function(group_id, row) {
      dataset_path <- first_text(row, c("DatasetPath"))
      if (nzchar(dataset_path) && file.exists(dataset_path)) {
        dataset <- tryCatch(utils::read.csv(dataset_path, stringsAsFactors = FALSE, check.names = FALSE, nrows = 2), error = function(e) data.frame())
        if (is.data.frame(dataset) && ncol(dataset) > 0) {
          cpg_cols <- setdiff(names(dataset), c("sample_id", "target", "EpigeneticClass"))
          return(length(cpg_cols))
        }
      }
      group_details <- if (is.data.frame(details) && nrow(details) > 0 && "GroupID" %in% names(details)) {
        details[as.character(details$GroupID) == group_id, , drop = FALSE]
      } else {
        data.frame()
      }
      if (is.data.frame(group_details) && nrow(group_details) > 0 && "CpG" %in% names(group_details)) {
        if ("CpGKeptForML" %in% names(group_details)) {
          kept <- as.logical(group_details$CpGKeptForML)
          kept[is.na(kept)] <- FALSE
          if (any(kept)) {
            return(length(trim_nonempty(group_details$CpG[kept])))
          }
        }
        return(length(trim_nonempty(group_details$CpG)))
      }
      count <- first_number(row, c("CpGs", "Columns", "NCpGs"))
      if (is.finite(count)) as.integer(count) else NA_integer_
    }
    sample_count_for_row <- function(row) {
      stratum_samples <- first_number(row, c("StratumSamples"))
      if (is.finite(stratum_samples)) {
        return(as.integer(stratum_samples))
      }
      dataset_path <- first_text(row, c("DatasetPath"))
      if (nzchar(dataset_path) && file.exists(dataset_path)) {
        dataset <- tryCatch(utils::read.csv(dataset_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
        if (is.data.frame(dataset) && nrow(dataset) > 0) {
          return(nrow(dataset))
        }
      }
      samples <- first_number(row, c("Samples", "N"))
      if (is.finite(samples)) as.integer(samples) else NA_integer_
    }
    transcripts_for_group <- function(group_id, row) {
      group_details <- if (is.data.frame(details) && nrow(details) > 0 && all(c("GroupID", "Transcript") %in% names(details))) {
        details[as.character(details$GroupID) == group_id, , drop = FALSE]
      } else {
        data.frame()
      }
      enst_columns <- intersect(
        c("EnsemblTranscript", "EnsemblTranscriptID", "TranscriptENST", "ENST", "ensembl_transcript_id", "Ensembl_Transcript_ID"),
        names(group_details)
      )
      transcripts <- trim_nonempty(if (length(enst_columns) > 0) group_details[[enst_columns[[1]]]] else character(0))
      if (length(transcripts) == 0 && is.data.frame(group_details) && nrow(group_details) > 0) {
        all_transcripts <- trim_nonempty(group_details$Transcript)
        enst_transcripts <- grep("^ENST", all_transcripts, value = TRUE)
        transcripts <- if (length(enst_transcripts) > 0) enst_transcripts else all_transcripts
      }
      if (length(transcripts) == 0) {
        all_transcripts <- trim_nonempty(c(
          first_text(row, c("PrincipalTranscript", "Transcript")),
          unlist(strsplit(first_text(row, c("ExtraTranscripts")), ";", fixed = TRUE), use.names = FALSE)
        ))
        enst_transcripts <- grep("^ENST", all_transcripts, value = TRUE)
        transcripts <- if (length(enst_transcripts) > 0) enst_transcripts else all_transcripts
      }
      paste(transcripts, collapse = "; ")
    }
    winner_for_row <- function(row, cpg_info) {
      metric_name <- first_text(row, c("MetricName"), "model")
      model_mean <- first_number(row, c("MedianMetric", "MeanMetric", "BestMetric"))
      lower_is_better <- grepl("rmse|mae|mse|error|loss|deviance", metric_name, ignore.case = TRUE)
      candidates <- numeric(0)
      if (is.finite(model_mean) && !isTRUE(lower_is_better)) {
        candidates[["ML"]] <- model_mean
      }
      if (is.finite(cpg_info$best_value)) {
        method <- cpg_info$best_method
        if (!nzchar(method)) {
          method <- "spearman"
        }
        candidates[[paste0("CpG-", method)]] <- cpg_info$best_value
      }
      if (length(candidates) == 0) {
        return("")
      }
      best <- max(candidates, na.rm = TRUE)
      winners <- names(candidates)[abs(candidates - best) < 1e-12]
      if (all(grepl("^CpG-", winners))) {
        "CpG"
      } else if (all(identical(winners, "ML"))) {
        "ML"
      } else {
        paste(winners, collapse = " + ")
      }
    }
    rows <- lapply(seq_len(nrow(results)), function(i) {
      row <- results[i, , drop = FALSE]
      group_id <- as.character(row$GroupID[[1]] %||% "")
      cpg_info <- best_cpgs_for_group(group_id, row)
      result <- result_object_for_row(row)
      data.frame(
        Result = winner_for_row(row, cpg_info),
        Gene = first_text(row, c("Gene")),
        `Group ID` = group_id,
        `Transcript(s)` = transcripts_for_group(group_id, row),
        Correlation = cpg_info$labels,
        CpGs = cpg_count_for_group(group_id, row),
        Samples = sample_count_for_row(row),
        Model = first_text(row, c("BestModel")),
        `Median R2` = summary_number(row, result, c("MedianR2", "MedianMetric"), c("best_model_median")),
        `Min R2` = summary_number(row, result, c("MinR2", "MinMetric"), c("best_model_min")),
        `Max R2` = summary_number(row, result, c("MaxR2", "MaxMetric", "BestMetric"), c("best_model_max")),
        `Median MAE` = summary_number(row, result, c("MedianMAE", "MAEMedian"), c("best_model_mae_median")),
        `wB R2` = summary_number(row, result, c("WBR2", "BloodAdjustedR2", "WithBloodR2", "R2WithBlood"), c("wb_r2", "blood_adjusted_r2")),
        `Shuffle max R2` = summary_number(row, result, c("ShuffleMaxR2", "ScrambleMaxR2", "AgeShuffleMaxR2"), c("shuffle_max_r2", "scramble_max_r2")),
        `Best source` = if (identical(winner_for_row(row, cpg_info), "ML")) "model" else cpg_info$best_method,
        `Correlation abs` = cpg_info$best_value,
        `Metric name` = first_text(row, c("MetricName"), "R2"),
        Source = first_text(row, c("Source")),
        Phase = first_text(row, c("Phase")),
        StratumColumn = first_text(row, c("StratumColumn")),
        StratumValue = first_text(row, c("StratumValue")),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    final <- bind_summary_rows(rows)
    if (!is.data.frame(final) || nrow(final) == 0) {
      return(data.frame())
    }
    if (all(c("Source", "Group ID", "StratumColumn", "StratumValue", "Phase") %in% names(final))) {
      key <- paste(final$Source, final$`Group ID`, final$StratumColumn, final$StratumValue, sep = "\r")
      stability_keys <- unique(key[as.character(final$Phase) == "stability"])
      final <- final[!(key %in% stability_keys & as.character(final$Phase) != "stability"), , drop = FALSE]
    }
    if ("Phase" %in% names(final)) {
      final$PhaseOrder <- ifelse(as.character(final$Phase) == "stability", 0L, 1L)
    } else {
      final$PhaseOrder <- 1L
    }
    final <- final[order(
      final$PhaseOrder,
      final$Source,
      final$StratumColumn,
      final$StratumValue,
      -suppressWarnings(as.numeric(final$`Median R2`)),
      -suppressWarnings(as.numeric(final$`Correlation abs`)),
      final$`Group ID`
    ), , drop = FALSE]
    final$PhaseOrder <- NULL
    rownames(final) <- NULL
    final
  })

  output$geo_transcript_ml_final_table_title <- renderUI({
    final <- geo_transcript_ml_final_rows()
    if (!is.data.frame(final) || nrow(final) == 0) {
      return(NULL)
    }
    tags$div(
      tags$h4("Transcript summary"),
      tags$p(class = "geo-step-note", "Result marks whether the strongest loaded evidence is ML-centered or CpG-centered.")
    )
  })

  output$geo_transcript_ml_final_table <- DT::renderDT({
    final <- geo_transcript_ml_final_rows()
    req(is.data.frame(final), nrow(final) > 0)
    display_cols <- c(
      "Result", "Gene", "Group ID", "Transcript(s)", "Correlation", "CpGs", "Samples",
      "Model", "Median R2", "Min R2", "Max R2", "Median MAE"
    )
    display <- final[, intersect(display_cols, names(final)), drop = FALSE]
    for (metric_col in intersect(c("Median R2", "Min R2", "Max R2", "Median MAE"), names(display))) {
      display[[metric_col]] <- signif(suppressWarnings(as.numeric(display[[metric_col]])), 5)
    }
    table <- DT::datatable(display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, selection = "single")
    if ("Correlation" %in% names(display)) {
      table <- DT::formatStyle(table, "Correlation", whiteSpace = "pre-line")
    }
    table
  })

  output$geo_transcript_ml_table <- DT::renderDT({
    results <- geo_transcript_ml_results_current()
    req(is.data.frame(results), nrow(results) > 0)
    display_cols <- intersect(
      c("Source", "Phase", "StratumColumn", "StratumValue", "StratumSamples", "GroupID", "PrincipalTranscript", "Gene", "CombinedRank", "ModelRank", "RhoRank", "TriggerMaxAbsRho", "TriggerBestCpG", "TriggerBestRho", "BestModel", "ModelsRun", "ModelsOK", "MetricName", "BestMetric", "MedianMetric", "MeanMetric", "MetricSE", "SeedsRun", "Stable", "StabilityDetail"),
      names(results)
    )
    display <- results[, display_cols, drop = FALSE]
    for (metric_col in intersect(c("TriggerMaxAbsRho", "TriggerBestRho", "BestMetric", "MedianMetric", "MeanMetric", "MetricSE"), names(display))) {
      display[[metric_col]] <- signif(suppressWarnings(as.numeric(display[[metric_col]])), 5)
    }
    DT::datatable(display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, selection = "single")
  })

  observeEvent(plotly::event_data("plotly_click", source = "geo_ml_class_rank", priority = "event"), {
    click <- plotly::event_data("plotly_click", source = "geo_ml_class_rank", priority = "event")
    click_value <- as.character(click$key %||% click$customdata %||% "")
    if (length(click_value) == 0 || !nzchar(click_value[[1]])) {
      return()
    }
    click_value <- click_value[[1]]
    click_parts <- strsplit(click_value, "\r", fixed = TRUE)[[1]]
    group_id <- click_parts[[1]] %||% click_value
    if (nzchar(group_id)) {
      geo_transcript_ml_focus_group(group_id)
      geo_transcript_ml_focus_stratum(list(
        column = click_parts[[2]] %||% "",
        value = click_parts[[3]] %||% ""
      ))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$geo_transcript_ml_table_rows_selected, {
    results <- geo_transcript_ml_results_current()
    selected <- input$geo_transcript_ml_table_rows_selected
    if (is.data.frame(results) && nrow(results) > 0 && length(selected) > 0 &&
        selected[[1]] <= nrow(results) && "GroupID" %in% names(results)) {
      geo_transcript_ml_focus_group(as.character(results$GroupID[[selected[[1]]]] %||% ""))
      geo_transcript_ml_focus_stratum(list(
        column = if ("StratumColumn" %in% names(results)) as.character(results$StratumColumn[[selected[[1]]]] %||% "") else "",
        value = if ("StratumValue" %in% names(results)) as.character(results$StratumValue[[selected[[1]]]] %||% "") else ""
      ))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$geo_transcript_ml_class_change_table_rows_selected, {
    changes <- geo_transcript_ml_class_change()
    selected <- input$geo_transcript_ml_class_change_table_rows_selected
    if (is.data.frame(changes) && nrow(changes) > 0 && length(selected) > 0 && selected[[1]] <= nrow(changes)) {
      geo_transcript_ml_focus_group(as.character(changes$GroupID[[selected[[1]]]] %||% ""))
      geo_transcript_ml_focus_stratum(list(column = "", value = as.character(changes$ComparisonClass[[selected[[1]]]] %||% "")))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$geo_transcript_ml_class_spearman_change_table_rows_selected, {
    changes <- geo_transcript_ml_class_change_for(geo_transcript_ml_class_rank_rows_for("spearman"))
    selected <- input$geo_transcript_ml_class_spearman_change_table_rows_selected
    if (is.data.frame(changes) && nrow(changes) > 0 && length(selected) > 0 && selected[[1]] <= nrow(changes)) {
      geo_transcript_ml_focus_group(as.character(changes$GroupID[[selected[[1]]]] %||% ""))
      geo_transcript_ml_focus_stratum(list(column = "", value = as.character(changes$ComparisonClass[[selected[[1]]]] %||% "")))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$geo_transcript_ml_class_combined_change_table_rows_selected, {
    changes <- geo_transcript_ml_class_change_for(geo_transcript_ml_class_rank_rows_for("combined"))
    selected <- input$geo_transcript_ml_class_combined_change_table_rows_selected
    if (is.data.frame(changes) && nrow(changes) > 0 && length(selected) > 0 && selected[[1]] <= nrow(changes)) {
      geo_transcript_ml_focus_group(as.character(changes$GroupID[[selected[[1]]]] %||% ""))
      geo_transcript_ml_focus_stratum(list(column = "", value = as.character(changes$ComparisonClass[[selected[[1]]]] %||% "")))
    }
  }, ignoreInit = TRUE)

  geo_transcript_ml_selected_data <- reactive({
    results <- geo_transcript_ml_results_current()
    details <- geo_transcript_group_details_current()
    selected <- input$geo_transcript_ml_table_rows_selected
    if (!is.data.frame(results) || nrow(results) == 0 ||
        !is.data.frame(details) || nrow(details) == 0) {
      return(list(row = data.frame(), track = data.frame()))
    }
    focus_group <- geo_transcript_ml_focus_group()
    if (nzchar(focus_group) && "GroupID" %in% names(results)) {
      focus_rows <- results[as.character(results$GroupID) == focus_group, , drop = FALSE]
      if (!is.data.frame(focus_rows) || nrow(focus_rows) == 0) {
        return(list(row = data.frame(), track = data.frame()))
      }
      focus_stratum <- geo_transcript_ml_focus_stratum()
      focus_stratum_col <- as.character(focus_stratum$column %||% "")
      focus_stratum_value <- as.character(focus_stratum$value %||% "")
      if (nzchar(focus_stratum_value) && "StratumValue" %in% names(focus_rows)) {
        class_rows <- focus_rows[as.character(focus_rows$StratumValue) == focus_stratum_value, , drop = FALSE]
        if (nzchar(focus_stratum_col) && "StratumColumn" %in% names(class_rows)) {
          exact_rows <- class_rows[as.character(class_rows$StratumColumn) == focus_stratum_col, , drop = FALSE]
          if (nrow(exact_rows) > 0) {
            class_rows <- exact_rows
          }
        }
        if (nrow(class_rows) > 0) {
          focus_rows <- class_rows
        }
      }
      if ("Phase" %in% names(focus_rows)) {
        stability_rows <- focus_rows[as.character(focus_rows$Phase) == "stability", , drop = FALSE]
        if (nrow(stability_rows) > 0) {
          focus_rows <- stability_rows
        }
      }
      ml_row <- focus_rows[1, , drop = FALSE]
    } else if (length(selected) > 0 && selected[[1]] <= nrow(results)) {
      ml_row <- results[selected[[1]], , drop = FALSE]
    } else {
      return(list(row = data.frame(), track = data.frame()))
    }
    group_id <- as.character(ml_row$GroupID[[1]] %||% "")
    track <- details[as.character(details$GroupID) == group_id, , drop = FALSE]
    if (!is.data.frame(track) || nrow(track) == 0 || !"CpG" %in% names(track)) {
      return(list(row = ml_row, track = data.frame()))
    }
    importance <- data.frame()
    importance_path <- if ("ImportancePath" %in% names(ml_row)) as.character(ml_row$ImportancePath[[1]] %||% "") else ""
    if (!is.na(importance_path) && nzchar(importance_path) && file.exists(importance_path)) {
      importance <- tryCatch(utils::read.csv(importance_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    }
    if ((!is.data.frame(importance) || nrow(importance) == 0) && "ScreenResultPath" %in% names(ml_row)) {
      result_path <- as.character(ml_row$ScreenResultPath[[1]] %||% "")
      if (!is.na(result_path) && nzchar(result_path) && file.exists(result_path)) {
        result <- tryCatch(readRDS(result_path), error = function(e) NULL)
        source_value <- if ("Source" %in% names(ml_row)) as.character(ml_row$Source[[1]] %||% "") else ""
        importance <- geo_ml_importance_table(result$best_model, ml_row, source_value, "screening")
      }
    }
    if (!is.data.frame(importance) || nrow(importance) == 0) {
      remote_importance <- geo_transcript_ml_importance_current()
      if (is.data.frame(remote_importance) && nrow(remote_importance) > 0 &&
          all(c("GroupID", "CpG", "Importance") %in% names(remote_importance))) {
        remote_importance <- remote_importance[as.character(remote_importance$GroupID) == group_id, , drop = FALSE]
        row_stratum_col <- if ("StratumColumn" %in% names(ml_row)) as.character(ml_row$StratumColumn[[1]] %||% "") else ""
        row_stratum_value <- if ("StratumValue" %in% names(ml_row)) as.character(ml_row$StratumValue[[1]] %||% "") else ""
        if (nzchar(row_stratum_value) && "StratumValue" %in% names(remote_importance)) {
          class_importance <- remote_importance[as.character(remote_importance$StratumValue) == row_stratum_value, , drop = FALSE]
          if (nzchar(row_stratum_col) && "StratumColumn" %in% names(class_importance)) {
            exact_importance <- class_importance[as.character(class_importance$StratumColumn) == row_stratum_col, , drop = FALSE]
            if (nrow(exact_importance) > 0) {
              class_importance <- exact_importance
            }
          }
          if (nrow(class_importance) > 0) {
            remote_importance <- class_importance
          }
        }
        if ("Phase" %in% names(remote_importance) && any(as.character(remote_importance$Phase) == "stability")) {
          remote_importance <- remote_importance[as.character(remote_importance$Phase) == "stability", , drop = FALSE]
        }
        importance <- remote_importance
      }
    }
    if (is.data.frame(importance) && nrow(importance) > 0 && all(c("CpG", "Importance") %in% names(importance))) {
      importance <- importance[, intersect(c("CpG", "Importance", "ImportanceRank"), names(importance)), drop = FALSE]
      track <- merge(track, importance, by = "CpG", all.x = TRUE, sort = FALSE)
    }
    if (!"Importance" %in% names(track)) {
      track$Importance <- NA_real_
    }
    if (!"ImportanceRank" %in% names(track)) {
      track$ImportanceRank <- NA_real_
    }
    track$Importance <- suppressWarnings(as.numeric(track$Importance))
    track$AbsRho <- suppressWarnings(as.numeric(track$AbsRho))
    max_importance <- suppressWarnings(max(track$Importance, na.rm = TRUE))
    max_absrho <- suppressWarnings(max(track$AbsRho, na.rm = TRUE))
    track$ImportanceNorm <- if (is.finite(max_importance) && max_importance > 0) pmax(0, pmin(1, track$Importance / max_importance)) else NA_real_
    track$AbsRhoNorm <- if (is.finite(max_absrho) && max_absrho > 0) pmax(0, pmin(1, track$AbsRho / max_absrho)) else NA_real_
    list(row = ml_row, track = track)
  })

  output$geo_transcript_ml_selected_title <- renderUI({
    selected <- geo_transcript_ml_selected_data()
    row <- selected$row
    track <- selected$track
    if (!is.data.frame(row) || nrow(row) == 0) {
      return(tags$p(class = "geo-step-note", "Click a TG point in the rank plot or select a transcript ML result row to inspect it."))
    }
    tags$div(
      tags$h4(paste0("ML importance: ", row$PrincipalTranscript[[1]], " / ", row$Gene[[1]])),
      tags$p(paste0("Best model: ", row$BestModel[[1]], " | CpGs: ", nrow(track), " | Importance values: ", sum(is.finite(track$Importance)))),
      tags$p(
        class = "geo-step-note",
        "Selection can come from the rank plot or the table. The epigenetic story below compares classes against the first class in the selected order."
      )
    )
  })

  geo_transcript_ml_story_context <- reactive({
    selected <- geo_transcript_ml_selected_data()
    row <- selected$row
    track <- selected$track
    if (!is.data.frame(row) || nrow(row) == 0 || !is.data.frame(track) || nrow(track) == 0) {
      return(list(row = data.frame(), track = data.frame(), dataset = data.frame()))
    }
    group_id <- as.character(row$GroupID[[1]] %||% "")
    group_payload <- as.list(row)
    remote_dataset <- geo_transcript_group_dataset_remote(group_id)
    if (is.data.frame(remote_dataset) && nrow(remote_dataset) > 0) {
      group_payload$dataset <- remote_dataset
    }
    dataset_info <- tryCatch(
      geo_ml_group_dataset(group_payload, keep_sample_id = TRUE),
      error = function(e) list(dataset = data.frame(), error = conditionMessage(e))
    )
    dataset <- dataset_info$dataset
    metadata <- geo_sample_metadata()
    if (!is.data.frame(dataset) || nrow(dataset) == 0 || !"sample_id" %in% names(dataset) ||
        !is.data.frame(metadata) || nrow(metadata) == 0 || !"sample_id" %in% names(metadata)) {
      return(list(row = row, track = track, dataset = data.frame(), error = dataset_info$error %||% ""))
    }
    class_col <- if ("StratumColumn" %in% names(row)) as.character(row$StratumColumn[[1]] %||% "") else ""
    if (!nzchar(class_col)) {
      class_col <- input$geo_ml_stability_group_column %||% ""
    }
    if (!nzchar(class_col) || !class_col %in% names(metadata)) {
      return(list(row = row, track = track, dataset = data.frame(), error = "No class column is available for the selected transcript."))
    }
    target_col <- if ("target" %in% names(dataset)) {
      "target"
    } else {
      candidates <- setdiff(names(dataset), c("sample_id", grep("^cg", names(dataset), value = TRUE)))
      candidates[[1]] %||% ""
    }
    if (!nzchar(target_col) || !target_col %in% names(dataset)) {
      return(list(row = row, track = track, dataset = data.frame(), error = "No target column is available in the transcript dataset."))
    }
    kept_track <- track
    if ("CpGKeptForML" %in% names(kept_track)) {
      kept <- as.logical(kept_track$CpGKeptForML)
      kept[is.na(kept)] <- FALSE
      if (any(kept)) {
        kept_track <- kept_track[kept, , drop = FALSE]
      }
    }
    cpg_cols <- intersect(unique(as.character(kept_track$CpG)), names(dataset))
    if (length(cpg_cols) == 0) {
      cpg_cols <- grep("^cg", names(dataset), value = TRUE)
    }
    if (length(cpg_cols) == 0) {
      return(list(row = row, track = track, dataset = data.frame(), error = "No CpG columns are available in the transcript dataset."))
    }
    metadata_values <- geo_ml_class_value(metadata[[class_col]])
    names(metadata_values) <- as.character(metadata$sample_id)
    dataset$EpigeneticClass <- metadata_values[as.character(dataset$sample_id)]
    dataset <- dataset[!is.na(dataset$EpigeneticClass) & nzchar(dataset$EpigeneticClass), , drop = FALSE]
    if (nrow(dataset) == 0) {
      return(list(row = row, track = track, dataset = data.frame(), error = "No transcript dataset samples matched the selected classes."))
    }
    strata <- geo_ml_stability_strata(metadata, class_col)
    class_order <- if (is.data.frame(strata) && nrow(strata) > 0) as.character(strata$StratumValue) else unique(as.character(dataset$EpigeneticClass))
    custom_order <- input$geo_ml_class_compare_order %||% character(0)
    custom_order <- trimws(as.character(custom_order))
    custom_order <- custom_order[nzchar(custom_order)]
    if (!is.null(input$geo_ml_class_compare_order)) {
      class_order <- intersect(custom_order, class_order)
    } else if (length(custom_order) > 0) {
      class_order <- c(intersect(custom_order, class_order), setdiff(class_order, custom_order))
    }
    class_order <- class_order[class_order %in% unique(as.character(dataset$EpigeneticClass))]
    if (length(class_order) == 0) {
      return(list(row = row, track = track, dataset = data.frame(), error = "No selected classes matched the transcript dataset."))
    }
    list(
      row = row,
      track = track,
      dataset = dataset,
      metadata = metadata,
      class_col = class_col,
      target_col = target_col,
      cpg_cols = cpg_cols,
      class_order = class_order,
      error = ""
    )
  })

  geo_transcript_ml_epigenetic_story_data <- reactive({
    context <- geo_transcript_ml_story_context()
    row <- context$row
    dataset <- context$dataset
    class_col <- context$class_col
    target_col <- context$target_col
    cpg_cols <- context$cpg_cols
    class_order <- context$class_order
    if (!is.data.frame(row) || nrow(row) == 0 || !is.data.frame(dataset) || nrow(dataset) == 0 ||
        length(cpg_cols) == 0 || length(class_order) == 0) {
      return(data.frame())
    }
    beta_matrix <- as.matrix(dataset[, cpg_cols, drop = FALSE])
    storage.mode(beta_matrix) <- "numeric"
    sample_beta <- rowMeans(beta_matrix, na.rm = TRUE)
    sample_beta[!is.finite(sample_beta)] <- NA_real_
    target_values <- suppressWarnings(as.numeric(dataset[[target_col]]))
    ml_results <- geo_transcript_ml_results_current()
    group_id <- as.character(row$GroupID[[1]] %||% "")
    group_rows <- if (is.data.frame(ml_results) && nrow(ml_results) > 0 && "GroupID" %in% names(ml_results)) {
      ml_results[as.character(ml_results$GroupID) == group_id, , drop = FALSE]
    } else {
      data.frame()
    }
    story_rows <- lapply(seq_along(class_order), function(i) {
      class_value <- class_order[[i]]
      keep <- as.character(dataset$EpigeneticClass) == class_value
      class_beta <- sample_beta[keep]
      class_target <- target_values[keep]
      cor_keep <- is.finite(class_beta) & is.finite(class_target)
      target_rho <- if (sum(cor_keep) >= 3 && stats::sd(class_beta[cor_keep]) > 0 && stats::sd(class_target[cor_keep]) > 0) {
        suppressWarnings(stats::cor(class_beta[cor_keep], class_target[cor_keep], method = "spearman"))
      } else {
        NA_real_
      }
      class_ml <- if (is.data.frame(group_rows) && nrow(group_rows) > 0 && all(c("StratumColumn", "StratumValue") %in% names(group_rows))) {
        group_rows[
          as.character(group_rows$StratumColumn) == class_col &
            as.character(group_rows$StratumValue) == class_value,
          ,
          drop = FALSE
        ]
      } else {
        data.frame()
      }
      if (is.data.frame(class_ml) && nrow(class_ml) > 1 && "Phase" %in% names(class_ml)) {
        stability_rows <- class_ml[as.character(class_ml$Phase) == "stability", , drop = FALSE]
        if (nrow(stability_rows) > 0) {
          class_ml <- stability_rows
        }
      }
      ml_metric <- if (is.data.frame(class_ml) && nrow(class_ml) > 0 && "MedianMetric" %in% names(class_ml)) {
        suppressWarnings(as.numeric(class_ml$MedianMetric[[1]]))
      } else {
        NA_real_
      }
      data.frame(
        Class = class_value,
        ClassOrder = i,
        Samples = sum(keep),
        CpGs = length(cpg_cols),
        MeanBeta = mean(class_beta, na.rm = TRUE),
        BetaSD = stats::sd(class_beta, na.rm = TRUE),
        TargetRho = target_rho,
        MedianR2 = ml_metric,
        stringsAsFactors = FALSE
      )
    })
    story <- bind_summary_rows(story_rows)
    if (!is.data.frame(story) || nrow(story) == 0 || !any(is.finite(story$MeanBeta))) {
      return(data.frame())
    }
    ref_beta <- story$MeanBeta[[1]]
    ref_r2 <- story$MedianR2[[1]]
    story$DeltaBeta <- story$MeanBeta - ref_beta
    story$DeltaR2 <- story$MedianR2 - ref_r2
    story$MethylationCall <- mapply(function(beta, beta_sd, delta) {
      if (is.finite(beta) && is.finite(beta_sd) && beta >= 0.8 && beta_sd <= 0.04) {
        "locked high"
      } else if (is.finite(beta) && is.finite(beta_sd) && beta <= 0.2 && beta_sd <= 0.04) {
        "locked low"
      } else if (is.finite(delta) && delta >= 0.08) {
        "hypermethylated shift"
      } else if (is.finite(delta) && delta <= -0.08) {
        "hypomethylated shift"
      } else {
        "stable methylation"
      }
    }, story$MeanBeta, story$BetaSD, story$DeltaBeta)
    story$PredictionCall <- mapply(function(r2, delta_r2) {
      if (!is.finite(r2)) {
      "prediction not measured"
      } else if (!is.finite(delta_r2) || abs(delta_r2) < 0.12) {
        "prediction similar"
      } else if (delta_r2 <= -0.25) {
        "prediction strongly reduced"
      } else if (delta_r2 <= -0.12) {
        "prediction reduced"
      } else if (delta_r2 >= 0.25) {
        "prediction strongly increased"
      } else {
        "prediction increased"
      }
    }, story$MedianR2, story$DeltaR2)
    story$Mechanism <- mapply(function(methylation, prediction, delta_beta, delta_r2) {
      if (identical(prediction, "prediction not measured")) {
        return("no class ML result")
      }
      methylation_shift <- is.finite(delta_beta) && abs(delta_beta) >= 0.08
      prediction_shift <- is.finite(delta_r2) && abs(delta_r2) >= 0.12
      if (methylation_shift && prediction_shift) {
        paste(prediction, "with", methylation)
      } else if (prediction_shift) {
        paste(prediction, "without beta shift")
      } else if (methylation_shift) {
        paste(methylation, "without prediction change")
      } else {
        "similar to reference"
      }
    }, story$MethylationCall, story$PredictionCall, story$DeltaBeta, story$DeltaR2)
    story$Interpretation <- ifelse(story$ClassOrder == 1, "reference", story$Mechanism)
    outcome_label <- input$geo_target_column %||% target_col
    outcome_label <- if (nzchar(as.character(outcome_label))) as.character(outcome_label) else "selected variable"
    story$Variable <- outcome_label
    story$AssociationRho <- story$TargetRho
    story$Label <- paste0(
      "<b>", story$Class, "</b>",
      "<br>", story$Interpretation,
      "<br>Mean beta: ", signif(story$MeanBeta, 4),
      "<br>Delta beta vs ", story$Class[[1]], ": ", signif(story$DeltaBeta, 4),
      "<br>Beta SD: ", signif(story$BetaSD, 4),
      "<br>Variable: ", outcome_label,
      "<br>", outcome_label, " rho: ", signif(story$AssociationRho, 4),
      "<br>Median R2: ", signif(story$MedianR2, 4),
      "<br>Delta R2 vs ", story$Class[[1]], ": ", signif(story$DeltaR2, 4),
      "<br>Samples: ", story$Samples,
      "<br>CpGs: ", story$CpGs
    )
    story
  })

  output$geo_transcript_ml_epigenetic_story_title <- renderUI({
    story <- geo_transcript_ml_epigenetic_story_data()
    selected <- geo_transcript_ml_selected_data()
    context <- geo_transcript_ml_story_context()
    row <- selected$row
    if (!is.data.frame(row) || nrow(row) == 0) {
      return(NULL)
    }
    if (!is.data.frame(story) || nrow(story) == 0) {
      detail <- as.character(context$error %||% "")
      message <- "Epigenetic story needs class-based ML results plus the cached transcript CpG dataset."
      if (nzchar(detail)) {
        message <- paste(message, detail)
      }
      return(tags$p(class = "geo-step-note", message))
    }
    changed <- story[story$ClassOrder > 1 & story$Interpretation != "similar to reference", , drop = FALSE]
    headline <- if (nrow(changed) > 0) {
      paste(paste0(changed$Class, ": ", changed$Interpretation), collapse = "; ")
    } else {
      paste0("All classes look similar to the reference for transcript-level beta and ", story$Variable[[1]], " prediction strength.")
    }
    tags$div(
      tags$h4(paste0("Class comparison: ", row$PrincipalTranscript[[1]], " / ", row$Gene[[1]])),
      tags$p(headline),
      tags$p(class = "geo-step-note", paste0("Variable being predicted: ", story$Variable[[1]], ". Reference class: ", story$Class[[1]], ". Calls compare each class against that reference; 'without beta shift' means prediction changed but transcript-level mean methylation did not."))
    )
  })

  output$geo_transcript_ml_epigenetic_story_plot <- renderPlotly({
    story <- geo_transcript_ml_epigenetic_story_data()
    req(is.data.frame(story), nrow(story) > 0)
    story$Class <- factor(story$Class, levels = story$Class)
    story$PlotR2 <- suppressWarnings(as.numeric(story$MedianR2))
    story$PlotR2[!is.finite(story$PlotR2)] <- 0.05
    delta_values <- suppressWarnings(as.numeric(story$DeltaBeta))
    max_abs_delta <- suppressWarnings(max(abs(delta_values), na.rm = TRUE))
    if (!is.finite(max_abs_delta)) {
      max_abs_delta <- 0.01
    }
    y_limit <- max(0.01, max_abs_delta * 1.35)
    p <- ggplot(story, aes(x = Class, y = DeltaBeta, group = 1, text = Label)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#8a96a8", linewidth = 0.45) +
      geom_line(color = "#344054", linewidth = 0.8, alpha = 0.7) +
      geom_point(aes(color = Interpretation, size = PlotR2), alpha = 0.92) +
      scale_y_continuous(limits = c(-y_limit, y_limit)) +
      scale_size_continuous(range = c(3.5, 7), name = "Median R2") +
      labs(x = NULL, y = paste0("Delta mean beta vs ", story$Class[[1]]), color = "Class call", title = "Class-level change vs reference") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE)
  })

  output$geo_transcript_ml_epigenetic_story_table <- DT::renderDT({
    story <- geo_transcript_ml_epigenetic_story_data()
    req(is.data.frame(story), nrow(story) > 0)
    display <- story[, intersect(c("Class", "Interpretation", "Variable", "Samples", "CpGs", "MeanBeta", "DeltaBeta", "BetaSD", "AssociationRho", "MedianR2", "DeltaR2"), names(story)), drop = FALSE]
    for (metric_col in intersect(c("MeanBeta", "DeltaBeta", "BetaSD", "AssociationRho", "MedianR2", "DeltaR2"), names(display))) {
      display[[metric_col]] <- signif(suppressWarnings(as.numeric(display[[metric_col]])), 5)
    }
    DT::datatable(display, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })

  geo_transcript_ml_epigenetic_cpg_change_data <- reactive({
    context <- geo_transcript_ml_story_context()
    row <- context$row
    track <- context$track
    dataset <- context$dataset
    cpg_cols <- context$cpg_cols
    class_order <- context$class_order
    class_col <- context$class_col
    if (!is.data.frame(row) || nrow(row) == 0 || !is.data.frame(track) || nrow(track) == 0 ||
        !is.data.frame(dataset) || nrow(dataset) == 0 || length(cpg_cols) == 0 || length(class_order) < 2) {
      return(data.frame())
    }
    reference_class <- as.character(input$geo_ml_class_change_reference_r2 %||% "")
    comparison_class <- as.character(input$geo_ml_class_change_comparison_r2 %||% "")
    if (!nzchar(reference_class) || !reference_class %in% class_order) {
      reference_class <- class_order[[1]]
    }
    if (!nzchar(comparison_class) || !comparison_class %in% class_order) {
      comparison_class <- class_order[[length(class_order)]]
    }
    if (identical(reference_class, comparison_class)) {
      return(data.frame())
    }
    ref_rows <- as.character(dataset$EpigeneticClass) == reference_class
    cmp_rows <- as.character(dataset$EpigeneticClass) == comparison_class
    if (!any(ref_rows) || !any(cmp_rows)) {
      return(data.frame())
    }
    cpg_rows <- lapply(cpg_cols, function(cpg) {
      values <- suppressWarnings(as.numeric(dataset[[cpg]]))
      ref_mean <- mean(values[ref_rows], na.rm = TRUE)
      cmp_mean <- mean(values[cmp_rows], na.rm = TRUE)
      data.frame(
        CpG = cpg,
        ReferenceClass = reference_class,
        ComparisonClass = comparison_class,
        ReferenceMeanBeta = ref_mean,
        ComparisonMeanBeta = cmp_mean,
        DeltaBeta = cmp_mean - ref_mean,
        AbsDeltaBeta = abs(cmp_mean - ref_mean),
        ReferenceSamples = sum(ref_rows & is.finite(values)),
        ComparisonSamples = sum(cmp_rows & is.finite(values)),
        stringsAsFactors = FALSE
      )
    })
    cpg_changes <- bind_summary_rows(cpg_rows)
    if (!is.data.frame(cpg_changes) || nrow(cpg_changes) == 0) {
      return(data.frame())
    }
    group_id <- as.character(row$GroupID[[1]] %||% "")
    ml_results <- geo_transcript_ml_results_current()
    group_rows <- if (is.data.frame(ml_results) && nrow(ml_results) > 0 && "GroupID" %in% names(ml_results)) {
      ml_results[as.character(ml_results$GroupID) == group_id, , drop = FALSE]
    } else {
      data.frame()
    }
    ml_row_for_class <- function(class_value) {
      if (!is.data.frame(group_rows) || nrow(group_rows) == 0 || !all(c("StratumColumn", "StratumValue") %in% names(group_rows))) {
        return(data.frame())
      }
      class_rows <- group_rows[
        as.character(group_rows$StratumValue) == class_value,
        ,
        drop = FALSE
      ]
      if (nzchar(class_col %||% "") && "StratumColumn" %in% names(class_rows)) {
        exact_rows <- class_rows[as.character(class_rows$StratumColumn) == class_col, , drop = FALSE]
        if (nrow(exact_rows) > 0) {
          class_rows <- exact_rows
        }
      }
      if (nrow(class_rows) > 1 && "Phase" %in% names(class_rows)) {
        stability_rows <- class_rows[as.character(class_rows$Phase) == "stability", , drop = FALSE]
        if (nrow(stability_rows) > 0) {
          class_rows <- stability_rows
        }
      }
      if (nrow(class_rows) > 0) class_rows[1, , drop = FALSE] else data.frame()
    }
    reference_ml <- ml_row_for_class(reference_class)
    comparison_ml <- ml_row_for_class(comparison_class)
    metric_value <- function(ml_row, column) {
      if (is.data.frame(ml_row) && nrow(ml_row) > 0 && column %in% names(ml_row)) {
        suppressWarnings(as.numeric(ml_row[[column]][[1]]))
      } else {
        NA_real_
      }
    }
    text_value <- function(ml_row, column) {
      if (is.data.frame(ml_row) && nrow(ml_row) > 0 && column %in% names(ml_row)) {
        as.character(ml_row[[column]][[1]] %||% "")
      } else {
        ""
      }
    }
    cpg_changes$ReferenceR2 <- metric_value(reference_ml, "MedianMetric")
    cpg_changes$ComparisonR2 <- metric_value(comparison_ml, "MedianMetric")
    cpg_changes$DeltaR2 <- cpg_changes$ComparisonR2 - cpg_changes$ReferenceR2
    cpg_changes$ReferenceBestModel <- text_value(reference_ml, "BestModel")
    cpg_changes$ComparisonBestModel <- text_value(comparison_ml, "BestModel")
    importance_for_class <- function(class_value, ml_row, prefix) {
      remote_importance <- geo_transcript_ml_importance_current()
      importance <- data.frame()
      if (is.data.frame(remote_importance) && nrow(remote_importance) > 0 &&
          all(c("GroupID", "CpG", "Importance") %in% names(remote_importance))) {
        importance <- remote_importance[as.character(remote_importance$GroupID) == group_id, , drop = FALSE]
        if (nzchar(class_value) && "StratumValue" %in% names(importance)) {
          class_importance <- importance[as.character(importance$StratumValue) == class_value, , drop = FALSE]
          if (nzchar(class_col %||% "") && "StratumColumn" %in% names(class_importance)) {
            exact_importance <- class_importance[as.character(class_importance$StratumColumn) == class_col, , drop = FALSE]
            if (nrow(exact_importance) > 0) {
              class_importance <- exact_importance
            }
          }
          if (nrow(class_importance) > 0) {
            importance <- class_importance
          }
        }
        if ("Phase" %in% names(importance) && any(as.character(importance$Phase) == "stability")) {
          importance <- importance[as.character(importance$Phase) == "stability", , drop = FALSE]
        }
      }
      if ((!is.data.frame(importance) || nrow(importance) == 0) && is.data.frame(ml_row) && nrow(ml_row) > 0 &&
          "ImportancePath" %in% names(ml_row)) {
        importance_path <- as.character(ml_row$ImportancePath[[1]] %||% "")
        if (!is.na(importance_path) && nzchar(importance_path) && file.exists(importance_path)) {
          importance <- tryCatch(utils::read.csv(importance_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
        }
      }
      if (!is.data.frame(importance) || nrow(importance) == 0 || !"CpG" %in% names(importance)) {
        return(data.frame(CpG = character(0), stringsAsFactors = FALSE))
      }
      importance <- importance[, intersect(c("CpG", "Importance", "ImportanceRank"), names(importance)), drop = FALSE]
      importance <- importance[!duplicated(as.character(importance$CpG)), , drop = FALSE]
      names(importance)[names(importance) == "Importance"] <- paste0(prefix, "Importance")
      names(importance)[names(importance) == "ImportanceRank"] <- paste0(prefix, "ImportanceRank")
      importance
    }
    reference_importance <- importance_for_class(reference_class, reference_ml, "Reference")
    comparison_importance <- importance_for_class(comparison_class, comparison_ml, "Comparison")
    if (nrow(reference_importance) > 0) {
      cpg_changes <- merge(cpg_changes, reference_importance, by = "CpG", all.x = TRUE, sort = FALSE)
    }
    if (nrow(comparison_importance) > 0) {
      cpg_changes <- merge(cpg_changes, comparison_importance, by = "CpG", all.x = TRUE, sort = FALSE)
    }
    cpg_changes$ReferenceImportance <- if ("ReferenceImportance" %in% names(cpg_changes)) suppressWarnings(as.numeric(cpg_changes$ReferenceImportance)) else NA_real_
    cpg_changes$ComparisonImportance <- if ("ComparisonImportance" %in% names(cpg_changes)) suppressWarnings(as.numeric(cpg_changes$ComparisonImportance)) else NA_real_
    cpg_changes$DeltaImportance <- cpg_changes$ComparisonImportance - cpg_changes$ReferenceImportance
    detail_cols <- intersect(c("CpG", "Region", "Position", "SpearmanRho", "AbsRho", "Importance", "ImportanceRank"), names(track))
    if (length(detail_cols) > 1) {
      detail <- track[, detail_cols, drop = FALSE]
      detail <- detail[!duplicated(as.character(detail$CpG)), , drop = FALSE]
      cpg_changes <- merge(cpg_changes, detail, by = "CpG", all.x = TRUE, sort = FALSE)
    }
    cpg_changes$AbsRho <- if ("AbsRho" %in% names(cpg_changes)) suppressWarnings(as.numeric(cpg_changes$AbsRho)) else NA_real_
    cpg_changes$Importance <- if ("Importance" %in% names(cpg_changes)) suppressWarnings(as.numeric(cpg_changes$Importance)) else NA_real_
    cpg_changes <- cpg_changes[order(
      -cpg_changes$AbsDeltaBeta,
      -abs(cpg_changes$DeltaImportance),
      -cpg_changes$Importance,
      -cpg_changes$AbsRho,
      cpg_changes$CpG
    ), , drop = FALSE]
    display_cols <- c(
      "CpG", "Region", "Position", "ReferenceClass", "ComparisonClass",
      "ReferenceMeanBeta", "ComparisonMeanBeta", "DeltaBeta", "AbsDeltaBeta",
      "ReferenceSamples", "ComparisonSamples", "ReferenceR2", "ComparisonR2", "DeltaR2",
      "ReferenceBestModel", "ComparisonBestModel", "ReferenceImportance", "ComparisonImportance", "DeltaImportance",
      "SpearmanRho", "AbsRho", "Importance", "ImportanceRank"
    )
    cpg_changes[, intersect(display_cols, names(cpg_changes)), drop = FALSE]
  })

  output$geo_transcript_ml_epigenetic_cpg_change_title <- renderUI({
    cpg_changes <- geo_transcript_ml_epigenetic_cpg_change_data()
    if (!is.data.frame(cpg_changes) || nrow(cpg_changes) == 0) {
      return(NULL)
    }
    tags$div(
      tags$h4("Top CpG changes inside selected transcript"),
      tags$p(
        class = "geo-step-note",
        paste0(
          "Ranks CpGs by beta difference between ", cpg_changes$ReferenceClass[[1]],
          " and ", cpg_changes$ComparisonClass[[1]],
          ". Reference/comparison are the first and last selected class tags. R2/model columns show the class-specific ML prediction result for the selected transcript; importance columns compare the CpG's ML importance when available."
        )
      )
    )
  })

  output$geo_transcript_ml_epigenetic_cpg_change_table <- DT::renderDT({
    cpg_changes <- geo_transcript_ml_epigenetic_cpg_change_data()
    req(is.data.frame(cpg_changes), nrow(cpg_changes) > 0)
    display <- cpg_changes
    for (metric_col in intersect(c("ReferenceMeanBeta", "ComparisonMeanBeta", "DeltaBeta", "AbsDeltaBeta", "ReferenceR2", "ComparisonR2", "DeltaR2", "ReferenceImportance", "ComparisonImportance", "DeltaImportance", "SpearmanRho", "AbsRho", "Importance"), names(display))) {
      display[[metric_col]] <- signif(suppressWarnings(as.numeric(display[[metric_col]])), 5)
    }
    DT::datatable(display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$geo_transcript_ml_importance_track <- renderPlotly({
    selected <- geo_transcript_ml_selected_data()
    row <- selected$row
    track <- selected$track
    req(is.data.frame(row), nrow(row) > 0, is.data.frame(track), nrow(track) > 0)
    track$PositionNumeric <- suppressWarnings(as.numeric(track$Position))
    track <- track[is.finite(track$PositionNumeric), , drop = FALSE]
    req(nrow(track) > 0)
    track <- track[order(track$PositionNumeric, track$CpG), , drop = FALSE]
    region_order <- c("Promoter", "TSS1500", "TSS200", "5'UTR", "Exon", "1stExon", "Intron", "Body", "3'UTR")
    region_labels <- c(
      Promoter = "Promoter",
      TSS1500 = "Promoter TSS1500",
      TSS200 = "Promoter TSS200",
      `5'UTR` = "5' UTR",
      Exon = "Exon",
      `1stExon` = "First exon",
      Intron = "Intron",
      Body = "Gene body",
      `3'UTR` = "3' UTR"
    )
    region_colors <- c(
      Promoter = "#f6c85f",
      TSS1500 = "#f6c85f",
      TSS200 = "#f08a4b",
      `5'UTR` = "#8ecae6",
      Exon = "#4dab6d",
      `1stExon` = "#4dab6d",
      Intron = "#d9e2ec",
      Body = "#b8c0ff",
      `3'UTR` = "#c77dff"
    )
    region_bands <- data.frame()
    if ("GeneRegion" %in% names(track)) {
      raw_regions <- trimws(as.character(track$GeneRegion))
      primary_region <- vapply(strsplit(raw_regions, ";", fixed = TRUE), function(parts) {
        parts <- trimws(parts)
        parts <- parts[nzchar(parts)]
        matched <- parts[parts %in% region_order]
        if (length(matched) > 0) matched[[1]] else NA_character_
      }, character(1))
      band_track <- track[!is.na(primary_region) & nzchar(primary_region), , drop = FALSE]
      primary_region <- primary_region[!is.na(primary_region) & nzchar(primary_region)]
      if (nrow(band_track) > 0) {
        x_values <- band_track$PositionNumeric
        padding <- diff(range(track$PositionNumeric, na.rm = TRUE)) * 0.01
        if (!is.finite(padding) || padding <= 0) {
          padding <- 1
        }
        boundaries <- if (length(x_values) == 1) {
          c(x_values - padding, x_values + padding)
        } else {
          mids <- (utils::head(x_values, -1) + utils::tail(x_values, -1)) / 2
          c(min(x_values) - padding, mids, max(x_values) + padding)
        }
        runs <- rle(primary_region)
        ends <- cumsum(runs$lengths)
        starts <- c(1, utils::head(ends, -1) + 1)
        region_bands <- do.call(rbind, lapply(seq_along(runs$values), function(i) {
          xmin <- boundaries[starts[[i]]]
          xmax <- boundaries[ends[[i]] + 1]
          data.frame(
            Sector = runs$values[[i]],
            x = (xmin + xmax) / 2,
            width = max(padding, xmax - xmin),
            Label = paste0(
              "Region: ", unname(region_labels[runs$values[[i]]]),
              "<br>Approx. span: ", round(xmin), "-", round(xmax),
              "<br>Source: Illumina CpG GeneRegion annotation"
            ),
            stringsAsFactors = FALSE
          )
        }))
        if (is.data.frame(region_bands) && nrow(region_bands) > 0) {
          region_bands$Sector <- factor(region_bands$Sector, levels = region_order)
        }
      }
    }
    track$Label <- paste0(
      "CpG: ", track$CpG,
      "<br>Position: ", track$Chr, ":", track$Position,
      "<br>Gene region: ", track$GeneRegion,
      "<br><b>|rho|: ", signif(track$AbsRho, 4), "</b>",
      "<br><b>Importance: ", ifelse(is.finite(track$Importance), signif(track$Importance, 4), "NA"), "</b>",
      "<br>Importance rank: ", ifelse(is.finite(track$ImportanceRank), track$ImportanceRank, "NA")
    )
    p <- ggplot(track, aes(x = PositionNumeric, y = 1)) +
      labs(x = "Genomic position", y = NULL, color = "Normalized")
    if (is.data.frame(region_bands) && nrow(region_bands) > 0) {
      p <- p +
        geom_tile(
          data = region_bands,
          aes(x = x, y = 1.02, width = width, height = 0.92, fill = Sector, text = Label),
          inherit.aes = FALSE,
          alpha = 0.26,
          color = NA
        ) +
        scale_fill_manual(values = region_colors, labels = region_labels, na.translate = FALSE, name = "Region")
    }
    p <- p +
      geom_segment(aes(xend = PositionNumeric, y = 1, yend = 1 + 0.42 * ImportanceNorm, text = Label),
                   color = "#334155", linewidth = 0.8, na.rm = TRUE) +
      geom_point(aes(y = 1 + 0.42 * ImportanceNorm, color = ImportanceNorm, size = ImportanceNorm, text = Label),
                 shape = 21, fill = "white", stroke = 0.8, na.rm = TRUE) +
      geom_point(aes(y = 0.72, color = AbsRhoNorm, text = Label),
                 shape = 16, size = 2.6, alpha = 0.55, na.rm = TRUE) +
      viridis::scale_color_viridis(option = "plasma", limits = c(0, 1), na.value = "#94a3b8") +
      scale_size_continuous(range = c(2.5, 8), guide = "none") +
      scale_y_continuous(breaks = c(0.72, 1), labels = c("|rho|", "importance"), limits = c(0.55, 1.48)) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
    plotly::ggplotly(p, tooltip = "text") %>% plotly::config(displaylogo = FALSE)
  })

  output$geo_transcript_ml_rho_importance_plot <- renderPlotly({
    selected <- geo_transcript_ml_selected_data()
    row <- selected$row
    track <- selected$track
    req(is.data.frame(row), nrow(row) > 0, is.data.frame(track), nrow(track) > 0)
    plot_df <- track[is.finite(track$AbsRhoNorm) & is.finite(track$ImportanceNorm), , drop = FALSE]
    req(nrow(plot_df) > 0)
    plot_df$Delta <- plot_df$ImportanceNorm - plot_df$AbsRhoNorm
    plot_df$Label <- paste0(
      "CpG: ", plot_df$CpG,
      "<br>Position: ", plot_df$Chr, ":", plot_df$Position,
      "<br><b>|rho| norm: ", signif(plot_df$AbsRhoNorm, 4), "</b>",
      "<br><b>Importance norm: ", signif(plot_df$ImportanceNorm, 4), "</b>",
      "<br>Delta importance-rho: ", signif(plot_df$Delta, 4)
    )
    p <- ggplot(plot_df, aes(x = AbsRhoNorm, y = ImportanceNorm, text = Label)) +
      geom_abline(slope = 1, intercept = 0, color = "#94a3b8", linetype = "dashed") +
      geom_point(aes(color = Delta, size = ImportanceNorm), alpha = 0.85) +
      scale_color_gradient2(low = "#2563eb", mid = "#64748b", high = "#dc2626", midpoint = 0, name = "Importance - |rho|") +
      scale_size_continuous(range = c(2.5, 8), guide = "none") +
      coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
      labs(
        x = "Normalized |rho|",
        y = "Normalized ML importance",
        title = "CpG agreement: Spearman vs ML importance",
        subtitle = "Above dashed line: ML adds signal. Below dashed line: Spearman is stronger."
      ) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
    plotly::ggplotly(p, tooltip = "text") %>% plotly::config(displaylogo = FALSE)
  })

  geo_transcript_dataset_cache_path <- function(cache_dir, transcript, target_column, source = NULL) {
    transcript_dir <- file.path(geo_analysis_cache_dir(cache_dir, source), "transcript_datasets", geo_safe_cache_token(target_column))
    if (!dir.exists(transcript_dir)) {
      dir.create(transcript_dir, recursive = TRUE, showWarnings = FALSE)
    }
    file.path(transcript_dir, paste0(geo_safe_cache_token(transcript), ".csv"))
  }

  geo_transcript_raw_dataset_cache_path <- function(cache_dir, transcript, target_column, source = NULL) {
    transcript_dir <- file.path(geo_analysis_cache_dir(cache_dir, source), "transcript_datasets", geo_safe_cache_token(target_column), "_raw")
    if (!dir.exists(transcript_dir)) {
      dir.create(transcript_dir, recursive = TRUE, showWarnings = FALSE)
    }
    file.path(transcript_dir, paste0(geo_safe_cache_token(transcript), "_raw.csv"))
  }

  geo_quarantine_legacy_raw_transcript_files <- function(cache_dir, target_column, source = NULL) {
    transcript_dir <- file.path(geo_analysis_cache_dir(cache_dir, source), "transcript_datasets", geo_safe_cache_token(target_column))
    if (!dir.exists(transcript_dir)) {
      return(invisible(character(0)))
    }
    legacy_files <- list.files(transcript_dir, pattern = "_raw\\.csv$", full.names = TRUE, recursive = FALSE)
    if (length(legacy_files) == 0) {
      return(invisible(character(0)))
    }
    legacy_dir <- file.path(transcript_dir, "_legacy_raw")
    if (!dir.exists(legacy_dir)) {
      dir.create(legacy_dir, recursive = TRUE, showWarnings = FALSE)
    }
    moved <- character(0)
    for (legacy_file in legacy_files) {
      target_file <- file.path(legacy_dir, basename(legacy_file))
      if (file.rename(legacy_file, target_file)) {
        moved <- c(moved, target_file)
      }
    }
    invisible(moved)
  }

  geo_transcript_dataset_has_missing <- function(dataset, target_column) {
    if (!is.data.frame(dataset) || nrow(dataset) == 0) {
      return(TRUE)
    }
    predictor_cols <- setdiff(names(dataset), c("sample_id", target_column))
    if (length(predictor_cols) == 0) {
      return(TRUE)
    }
    mask <- build_missing_mask(
      dataset[, predictor_cols, drop = FALSE],
      missing_definition = geo_transcript_missing_definition()
    )
    sum(mask) > 0
  }

  geo_transcript_all_missing_row_fraction <- function(dataset, target_column) {
    if (!is.data.frame(dataset) || nrow(dataset) == 0) {
      return(1)
    }
    predictor_cols <- setdiff(names(dataset), c("sample_id", target_column))
    if (length(predictor_cols) == 0) {
      return(1)
    }
    mask <- build_missing_mask(
      dataset[, predictor_cols, drop = FALSE],
      missing_definition = geo_transcript_missing_definition()
    )
    mean(rowSums(mask) == length(predictor_cols))
  }

  geo_candidate_cpg_matrix_cache_path <- function(cache_dir, target_column, threshold, source = NULL) {
    transcript_dir <- file.path(geo_analysis_cache_dir(cache_dir, source), "transcript_datasets", geo_safe_cache_token(target_column))
    if (!dir.exists(transcript_dir)) {
      dir.create(transcript_dir, recursive = TRUE, showWarnings = FALSE)
    }
    file.path(transcript_dir, paste0(
      "_candidate_cpg_matrix_absrho_",
      geo_transcript_cache_version(), "_",
      geo_safe_cache_token(format(threshold, trim = TRUE, scientific = FALSE)),
      ".csv"
    ))
  }

  build_geo_transcript_groups <- function(candidates, cache_dir, target_column, threshold,
                                          min_samples_pct, paths, update_stage = FALSE,
                                          progress_callback = NULL) {
    source <- geo_matrix_source_value()
    metadata <- geo_sample_metadata()
    if ((!nzchar(target_column) || !target_column %in% names(metadata)) && is.data.frame(metadata) && nrow(metadata) > 0) {
      target_candidates <- ugplot_geo_target_candidates(metadata)
      target_column <- if ("age" %in% target_candidates) "age" else if (length(target_candidates) > 0) target_candidates[[1]] else ""
    }
    matrix_files <- ugplot_geo_matrix_files(cache_dir, source = source)
    if (!is.data.frame(metadata) || nrow(metadata) == 0 || !target_column %in% names(metadata) || length(matrix_files) == 0) {
      missing_reasons <- c(
        if (!is.data.frame(metadata) || nrow(metadata) == 0) "metadata" else character(0),
        if (!nzchar(target_column) || !target_column %in% names(metadata)) paste0("target column '", target_column, "'") else character(0),
        if (length(matrix_files) == 0) paste0(geo_matrix_source_label(source), " matrix files") else character(0)
      )
      geo_transcript_groups(data.frame())
      geo_transcript_group_details(data.frame())
      update_geo_transcript_build_progress(
        phase = "blocked",
        message = paste0("Missing prerequisite(s): ", paste(missing_reasons, collapse = ", "), "."),
        cache = cache_dir
      )
      return(invisible(FALSE))
    }
    moved_legacy_raw <- geo_quarantine_legacy_raw_transcript_files(cache_dir, target_column, source = source)
    if (length(moved_legacy_raw) > 0) {
      geo_status(ugplot_geo_append_log(
        geo_status(),
        paste0("Moved ", length(moved_legacy_raw), " legacy raw transcript CSV(s) to _legacy_raw.")
      ))
    }

    transcripts <- unique(as.character(stats::na.omit(candidates$Transcript)))
    transcripts <- transcripts[nzchar(transcripts)]
    progress_rows <- if (file.exists(paths$progress)) {
      tryCatch(readRDS(paths$progress), error = function(e) data.frame())
    } else {
      data.frame()
    }
    if (is.data.frame(progress_rows) && nrow(progress_rows) > 0 && all(c("Transcript", "Status", "DatasetPath", "RawDatasetPath") %in% names(progress_rows))) {
      for (progress_i in seq_len(nrow(progress_rows))) {
        final_path <- geo_transcript_dataset_cache_path(cache_dir, progress_rows$Transcript[[progress_i]], target_column, source = source)
        raw_path <- geo_transcript_raw_dataset_cache_path(cache_dir, progress_rows$Transcript[[progress_i]], target_column, source = source)
        old_final_path <- as.character(progress_rows$DatasetPath[[progress_i]])
        old_raw_path <- as.character(progress_rows$RawDatasetPath[[progress_i]])
        if (file.exists(final_path) && !file.exists(raw_path)) {
          legacy_dataset <- tryCatch(utils::read.csv(final_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
          if (geo_transcript_dataset_has_missing(legacy_dataset, target_column)) {
            file.copy(final_path, raw_path, overwrite = TRUE)
          }
        }
        if (identical(progress_rows$Status[[progress_i]], "compatible") &&
            nzchar(old_final_path) && file.exists(old_final_path) && !identical(old_final_path, final_path)) {
          file.copy(old_final_path, final_path, overwrite = TRUE)
          progress_rows$DatasetPath[[progress_i]] <- final_path
        }
        progress_rows$RawDatasetPath[[progress_i]] <- raw_path
      }
      progress_rows <- progress_rows[vapply(seq_len(nrow(progress_rows)), function(progress_i) {
        if (identical(progress_rows$Status[[progress_i]], "compatible")) {
          final_path <- as.character(progress_rows$DatasetPath[[progress_i]])
          return(nzchar(final_path) && file.exists(final_path))
        }
        raw_path <- as.character(progress_rows$RawDatasetPath[[progress_i]])
        nzchar(raw_path) && file.exists(raw_path)
      }, logical(1)), , drop = FALSE]
    }
    processed <- if (is.data.frame(progress_rows) && "Transcript" %in% names(progress_rows)) {
      unique(as.character(progress_rows$Transcript))
    } else {
      character(0)
    }
    compatible_n <- if (is.data.frame(progress_rows) && "Status" %in% names(progress_rows)) sum(progress_rows$Status == "compatible") else 0L
    excluded_n <- if (is.data.frame(progress_rows) && "Status" %in% names(progress_rows)) sum(progress_rows$Status != "compatible") else 0L
    update_geo_transcript_build_progress(
      phase = "starting",
      message = "Preparing transcript CSV build from cached Spearman candidates.",
      processed = length(processed),
      total = length(transcripts),
      compatible = compatible_n,
      excluded = excluded_n,
      current = "",
      cache = dirname(paths$summary)
    )
    if (!is.null(progress_callback)) {
      progress_callback(0.02, paste0("Preparing ", length(transcripts), " transcript(s)"))
    }

    candidate_matrix_path <- geo_candidate_cpg_matrix_cache_path(cache_dir, target_column, threshold, source = source)
    streaming_candidates <- is.data.frame(attr(candidates, "annotation_map", exact = TRUE)) &&
      is.data.frame(attr(candidates, "raw_results", exact = TRUE))
    candidate_matrix <- if (!isTRUE(streaming_candidates) && file.exists(candidate_matrix_path)) {
      tryCatch(utils::read.csv(candidate_matrix_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    } else {
      data.frame()
    }
    if (is.data.frame(candidate_matrix) && nrow(candidate_matrix) > 0) {
      predictor_cols <- setdiff(names(candidate_matrix), c("sample_id", target_column))
      if (length(predictor_cols) > 0) {
        all_missing_rows <- rowSums(is.na(candidate_matrix[, predictor_cols, drop = FALSE])) == length(predictor_cols)
        if (mean(all_missing_rows) > 0.1) {
          candidate_matrix <- data.frame()
          try(unlink(candidate_matrix_path), silent = TRUE)
          progress_rows <- data.frame()
          if (!is.null(progress_callback)) {
            progress_callback(0.03, "Discarded stale candidate matrix with too many all-missing sample rows")
          }
        }
      }
    }
    if (!isTRUE(streaming_candidates) && (!is.data.frame(candidate_matrix) || nrow(candidate_matrix) == 0)) {
      all_candidate_cpgs <- unique(as.character(stats::na.omit(candidates$CpG)))
      all_candidate_cpgs <- all_candidate_cpgs[nzchar(all_candidate_cpgs)]
      if (length(all_candidate_cpgs) > 0) {
        update_geo_transcript_build_progress(
          phase = "building candidate CpG matrix",
          message = paste0("Reading ", length(all_candidate_cpgs), " unique candidate CpGs once from the extracted GEO matrix files."),
          processed = length(processed),
          total = length(transcripts),
          compatible = compatible_n,
          excluded = excluded_n,
          current = "",
          cache = candidate_matrix_path
        )
        if (isTRUE(update_stage)) {
          geo_stage(list(
            step = "Step 6",
            title = "Building candidate CpG matrix",
            message = paste0("Reading ", length(all_candidate_cpgs), " unique CpGs once; this avoids rescanning GEO matrices for every transcript.")
          ))
        }
        if (!is.null(progress_callback)) {
          progress_callback(0.05, paste0("Reading ", length(all_candidate_cpgs), " candidate CpG(s) from GEO matrices"))
        }
        candidate_matrix <- tryCatch(
          ugplot_geo_transcript_dataset(
            matrix_files = matrix_files,
            metadata = metadata,
            target_column = target_column,
            cpgs = all_candidate_cpgs,
            progress_callback = function(scanned, found, total) {
              update_geo_transcript_build_progress(
                phase = "building candidate CpG matrix",
                message = paste0("Scanned ", scanned, " matrix rows; found ", found, " of ", total, " candidate CpG(s)."),
                processed = length(processed),
                total = length(transcripts),
                compatible = compatible_n,
                excluded = excluded_n,
                current = "",
                cache = candidate_matrix_path
              )
              if (isTRUE(update_stage)) {
                geo_stage(list(
                  step = "Step 6",
                  title = "Building candidate CpG matrix",
                  message = paste0("Scanned ", scanned, " matrix rows; found ", found, " of ", total, " candidate CpG(s).")
                ))
              }
              if (!is.null(progress_callback)) {
                progress_callback(
                  0.05 + 0.30 * min(1, found / max(1, total)),
                  paste0("Candidate matrix: scanned ", scanned, " rows; found ", found, " / ", total, " CpG(s)")
                )
              }
            }
          ),
          error = function(e) data.frame()
        )
        if (is.data.frame(candidate_matrix) && nrow(candidate_matrix) > 0) {
          utils::write.csv(candidate_matrix, candidate_matrix_path, row.names = FALSE)
          update_geo_transcript_build_progress(
            phase = "candidate CpG matrix cached",
            message = paste0("Candidate CpG matrix ready with ", nrow(candidate_matrix), " samples and ", max(0, ncol(candidate_matrix) - 2), " CpG columns."),
            processed = length(processed),
            total = length(transcripts),
            compatible = compatible_n,
            excluded = excluded_n,
            cache = candidate_matrix_path
          )
          if (!is.null(progress_callback)) {
            progress_callback(0.35, "Candidate CpG matrix cached")
          }
        }
      }
    }

    for (transcript_id in setdiff(transcripts, processed)) {
      update_geo_transcript_build_progress(
        phase = "processing transcripts",
        message = paste0("Building/reusing CSV and complete-case filter for transcript ", transcript_id, "."),
        processed = nrow(progress_rows),
        total = length(transcripts),
        compatible = if (is.data.frame(progress_rows) && "Status" %in% names(progress_rows)) sum(progress_rows$Status == "compatible") else 0L,
        excluded = if (is.data.frame(progress_rows) && "Status" %in% names(progress_rows)) sum(progress_rows$Status != "compatible") else 0L,
        current = transcript_id,
        cache = geo_transcript_dataset_cache_path(cache_dir, transcript_id, target_column, source = source)
      )
      transcript_rows <- candidates[as.character(candidates$Transcript) == transcript_id, , drop = FALSE]
      transcript_cpgs <- unique(as.character(stats::na.omit(transcript_rows$CpG)))
      transcript_cpgs <- transcript_cpgs[nzchar(transcript_cpgs)]
      dataset_path <- geo_transcript_dataset_cache_path(cache_dir, transcript_id, target_column, source = source)
      raw_dataset_path <- geo_transcript_raw_dataset_cache_path(cache_dir, transcript_id, target_column, source = source)
      transcript_dataset <- data.frame()
      if (file.exists(raw_dataset_path)) {
        transcript_dataset <- tryCatch(utils::read.csv(raw_dataset_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
        if (is.data.frame(transcript_dataset) && nrow(transcript_dataset) > 0 &&
            geo_transcript_all_missing_row_fraction(transcript_dataset, target_column) > 0.1 &&
            is.data.frame(candidate_matrix) && nrow(candidate_matrix) > 0) {
          try(unlink(raw_dataset_path), silent = TRUE)
          transcript_dataset <- data.frame()
        }
      } else if (file.exists(dataset_path)) {
        cached_dataset <- tryCatch(utils::read.csv(dataset_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
        if (geo_transcript_dataset_has_missing(cached_dataset, target_column)) {
          file.copy(dataset_path, raw_dataset_path, overwrite = TRUE)
          transcript_dataset <- cached_dataset
        } else {
          transcript_dataset <- cached_dataset
        }
      }
      if ((!is.data.frame(transcript_dataset) || nrow(transcript_dataset) == 0) &&
          is.data.frame(candidate_matrix) && nrow(candidate_matrix) > 0) {
        available_cpgs <- intersect(transcript_cpgs, names(candidate_matrix))
        if (length(available_cpgs) > 0) {
          transcript_dataset <- candidate_matrix[, c("sample_id", target_column, available_cpgs), drop = FALSE]
          utils::write.csv(transcript_dataset, raw_dataset_path, row.names = FALSE)
        } else {
          transcript_dataset <- data.frame()
        }
      } else if (!is.data.frame(transcript_dataset) || nrow(transcript_dataset) == 0) {
        transcript_dataset <- tryCatch(
          ugplot_geo_transcript_dataset(
            matrix_files = matrix_files,
            metadata = metadata,
            target_column = target_column,
            cpgs = transcript_cpgs
          ),
          error = function(e) data.frame()
        )
        if (is.data.frame(transcript_dataset) && nrow(transcript_dataset) > 0) {
          utils::write.csv(transcript_dataset, raw_dataset_path, row.names = FALSE)
        }
      }

      status <- "excluded"
      kept_cpgs <- character(0)
      kept_samples <- character(0)
      filtered_path <- ""
      best <- data.frame()
      if (is.data.frame(transcript_dataset) && nrow(transcript_dataset) > 0) {
        target_missing <- build_missing_mask(
          transcript_dataset[, target_column, drop = FALSE],
          missing_definition = geo_transcript_missing_definition()
        )[, 1]
        analysis_dataset <- transcript_dataset[!target_missing, , drop = FALSE]
        predictor_cols <- setdiff(names(analysis_dataset), c("sample_id", target_column))
        predictors <- analysis_dataset[, predictor_cols, drop = FALSE]
        if (length(predictor_cols) > 0) {
          predictor_missing <- build_missing_mask(
            predictors,
            missing_definition = geo_transcript_missing_definition()
          )
          source_available_rows <- rowSums(predictor_missing) < length(predictor_cols)
        } else {
          source_available_rows <- rep(FALSE, nrow(analysis_dataset))
        }
        analysis_dataset <- analysis_dataset[source_available_rows, , drop = FALSE]
        predictors <- analysis_dataset[, predictor_cols, drop = FALSE]
        min_samples_required <- ceiling((min_samples_pct / 100) * nrow(analysis_dataset))
        adjusted_min_rows_retained <- if (nrow(analysis_dataset) > 0) {
          min(1, min_samples_required / nrow(analysis_dataset))
        } else {
          1
        }
        scan <- compute_exhaustive_threshold_scan(
          predictors = predictors,
          missing_definition = geo_transcript_missing_definition(),
          min_rows_retained = adjusted_min_rows_retained,
          mode = "complete_case"
        )
        if (is.data.frame(scan) && nrow(scan) > 0) {
          best <- scan[1, , drop = FALSE]
          if (isTRUE(best$complete_case[[1]]) && isTRUE(best$meets_min_samples[[1]]) &&
              best$n_rows_after[[1]] >= min_samples_required) {
            filtered <- apply_missing_filters_with_order(
              predictors = predictors,
              missing_definition = geo_transcript_missing_definition(),
              threshold_cols = best$thr_col[[1]],
              threshold_rows = best$thr_row[[1]],
              order = as.character(best$scan_order[[1]])
            )
            kept_cpgs <- colnames(filtered$filtered_predictors)
            kept_samples <- analysis_dataset$sample_id[filtered$keep_rows]
            filtered_dataset <- cbind(
              analysis_dataset[filtered$keep_rows, c("sample_id", target_column), drop = FALSE],
              filtered$filtered_predictors
            )
            filtered_path <- dataset_path
            utils::write.csv(filtered_dataset, filtered_path, row.names = FALSE)
            status <- "compatible"
          } else {
            status <- "no_complete_case_at_min_samples"
          }
        }
      } else {
        status <- "dataset_unavailable"
      }

      trigger_max <- suppressWarnings(max(transcript_rows$TriggerMaxAbsRho, na.rm = TRUE))
      if (!is.finite(trigger_max)) {
        trigger_max <- NA_real_
      }
      progress_row <- data.frame(
        Transcript = transcript_id,
        Gene = paste(unique(stats::na.omit(transcript_rows$Gene)), collapse = ";"),
        Status = status,
        Columns = length(kept_cpgs),
        Samples = length(kept_samples),
        KeptCpGs = paste(kept_cpgs, collapse = ";"),
        CpGKey = geo_group_key(kept_cpgs),
        SampleKey = geo_group_key(kept_samples),
        TriggerMaxAbsRho = trigger_max,
        TriggerBestCpG = if ("TriggerBestCpG" %in% names(transcript_rows)) as.character(transcript_rows$TriggerBestCpG[[1]]) else "",
        TriggerBestRho = if ("TriggerBestRho" %in% names(transcript_rows)) suppressWarnings(as.numeric(transcript_rows$TriggerBestRho[[1]])) else NA_real_,
        ThresholdCols = if (nrow(best) > 0) best$thr_col[[1]] else NA_real_,
        ThresholdRows = if (nrow(best) > 0) best$thr_row[[1]] else NA_real_,
        FilterOrder = if (nrow(best) > 0) as.character(best$scan_order[[1]]) else "",
        DatasetPath = filtered_path,
        RawDatasetPath = raw_dataset_path,
        stringsAsFactors = FALSE
      )
      progress_rows <- rbind(progress_rows, progress_row)
      tables <- geo_build_group_tables(progress_rows, candidates)
      geo_transcript_groups(tables$summary)
      geo_transcript_group_details(tables$details)
      write_geo_transcript_group_cache(paths, tables, progress_rows)
      update_geo_transcript_build_progress(
        phase = "processing transcripts",
        message = paste0("Finished ", transcript_id, " with status: ", status, "."),
        processed = nrow(progress_rows),
        total = length(transcripts),
        compatible = sum(progress_rows$Status == "compatible"),
        excluded = sum(progress_rows$Status != "compatible"),
        current = transcript_id,
        cache = dataset_path
      )
      if (isTRUE(update_stage)) {
        geo_stage(list(
          step = "Step 6",
          title = "Building transcript ML groups",
          message = paste0("Processed ", nrow(progress_rows), " of ", length(transcripts), " transcript(s). Compatible groups: ", nrow(tables$summary), ".")
        ))
      }
      if (!is.null(progress_callback)) {
        progress_callback(
          0.35 + 0.65 * min(1, nrow(progress_rows) / max(1, length(transcripts))),
          paste0(
            "Processed ", nrow(progress_rows), " / ", length(transcripts),
            "; compatible ", sum(progress_rows$Status == "compatible"),
            "; excluded ", sum(progress_rows$Status != "compatible"),
            "; current ", transcript_id
          )
        )
      }
    }

    tables <- geo_build_group_tables(progress_rows, candidates)
    geo_transcript_groups(tables$summary)
    geo_transcript_group_details(tables$details)
    write_geo_transcript_group_cache(paths, tables, progress_rows)
    update_geo_transcript_build_progress(
      phase = "complete",
      message = paste0("Transcript CSV/group build complete. Compatible groups: ", nrow(tables$summary), "."),
      processed = nrow(progress_rows),
      total = length(transcripts),
      compatible = if (is.data.frame(progress_rows) && "Status" %in% names(progress_rows)) sum(progress_rows$Status == "compatible") else 0L,
      excluded = if (is.data.frame(progress_rows) && "Status" %in% names(progress_rows)) sum(progress_rows$Status != "compatible") else 0L,
      current = "",
      cache = paths$summary
    )
    geo_status(ugplot_geo_append_log(geo_status(), paste0("Transcript ML group table ready: ", nrow(tables$summary), " group(s). Cache: ", paths$summary)))
    if (!is.null(progress_callback)) {
      progress_callback(1, paste0("Complete: ", nrow(tables$summary), " compatible group(s)"))
    }
    invisible(TRUE)
  }

  output$geo_spearman_table <- DT::renderDT({
    results <- geo_spearman_results()
    req(is.data.frame(results), nrow(results) > 0)
    display <- geo_filter_spearman_min_samples(results)
    req(is.data.frame(display), nrow(display) > 0)
    display$SpearmanRho <- round(display$SpearmanRho, 5)
    display$PValue <- formatC(display$PValue, format = "e", digits = 3)
    display$AbsRho <- round(display$AbsRho, 5)
    DT::datatable(
      display,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "28%", targets = 0),
          list(width = "18%", targets = 1),
          list(width = "22%", targets = 2),
          list(width = "12%", targets = 3),
          list(width = "20%", targets = 4)
        )
      ),
      rownames = FALSE,
      class = "compact stripe"
    )
  })

  output$geo_transcript_groups_table <- DT::renderDT({
    groups <- geo_transcript_groups()
    req(is.data.frame(groups), nrow(groups) > 0)
    display_cols <- intersect(c("PrincipalTranscript", "Gene", "Columns", "Samples", "TranscriptCount", "TriggerMaxAbsRho"), names(groups))
    display <- groups[, display_cols, drop = FALSE]
    if ("TriggerMaxAbsRho" %in% names(display)) {
      display$TriggerMaxAbsRho <- round(display$TriggerMaxAbsRho, 5)
    }
    display <- cbind(
      Load = vapply(seq_len(nrow(groups)), function(i) {
        as.character(tags$button(
          type = "button",
          class = "btn btn-default btn-xs",
          `data-group` = groups$GroupID[[i]],
          onclick = "Shiny.setInputValue('geo_load_transcript_group_from_row', this.getAttribute('data-group'), {priority: 'event'});",
          "Load"
        ))
      }, character(1)),
      display,
      stringsAsFactors = FALSE
    )
    DT::datatable(
      display,
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(targets = 0, orderable = FALSE, searchable = FALSE))
      ),
      rownames = FALSE,
      escape = which(names(display) != "Load")
    )
  })

  output$geo_transcript_group_details_title <- renderUI({
    groups <- geo_transcript_groups()
    progress <- geo_transcript_build_progress()
    if (identical(progress$phase %||% "", "complete") && (!is.data.frame(groups) || nrow(groups) == 0)) {
      return(tags$p(
        class = "geo-step-note",
        paste0(
          "No compatible transcript groups were produced with the current settings. ",
          "Try lowering the minimum samples below ", input$geo_transcript_min_samples %||% 80,
          "% or lowering the |rho| threshold."
        )
      ))
    }
    selected <- input$geo_transcript_groups_table_rows_selected
    if (!is.data.frame(groups) || nrow(groups) == 0 || length(selected) == 0) {
      return(tags$p(class = "geo-step-note", "Select a transcript group row to inspect its CpGs and compatible transcripts."))
    }
    group <- groups[selected[[1]], , drop = FALSE]
    extras <- group$ExtraTranscripts[[1]]
    tags$div(
      tags$h4(paste0("Details: ", group$PrincipalTranscript[[1]])),
      tags$p(paste0("Gene: ", group$Gene[[1]], " | CpGs: ", group$Columns[[1]], " | Samples: ", group$Samples[[1]])),
      tags$p(paste0("Extra compatible transcripts: ", if (nzchar(extras)) extras else "None")),
      tags$p(class = "geo-step-note", "Detail rows are capped for inspection; downstream steps use the saved transcript datasets and group summary.")
    )
  })

  output$geo_transcript_group_details_table <- DT::renderDT({
    groups <- geo_transcript_groups()
    details <- geo_transcript_group_details_current()
    selected <- input$geo_transcript_groups_table_rows_selected
    req(is.data.frame(groups), nrow(groups) > 0, length(selected) > 0)
    group <- groups[selected[[1]], , drop = FALSE]
    group_id <- group$GroupID[[1]]
    if (!is.data.frame(details) || nrow(details) == 0 || !"CpG" %in% names(details)) {
      cpgs <- if ("CpGs" %in% names(group)) {
        unlist(strsplit(as.character(group$CpGs[[1]] %||% ""), ";", fixed = TRUE), use.names = FALSE)
      } else {
        character(0)
      }
      cpgs <- cpgs[nzchar(cpgs)]
      display <- data.frame(
        GroupID = group_id,
        PrincipalTranscript = as.character(group$PrincipalTranscript[[1]] %||% ""),
        Gene = as.character(group$Gene[[1]] %||% ""),
        CpG = cpgs,
        stringsAsFactors = FALSE
      )
      req(nrow(display) > 0)
      return(DT::datatable(display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, selection = "single"))
    }
    display <- details[details$GroupID == group_id, , drop = FALSE]
    display_cols <- intersect(
      c("Transcript", "EnsemblTranscript", "Gene", "CpG", "CpGKeptForML", "GeneRegion", "Chr", "Position", "Strand", "CpGIslandRelation", "RegulatoryFeature", "ProbeType", "SpearmanRho", "AbsRho", "PValue"),
      names(display)
    )
    display <- display[, display_cols, drop = FALSE]
    if ("DetailRowsTruncated" %in% names(details) && any(details$DetailRowsTruncated[details$GroupID == group_id] %in% TRUE)) {
      display <- utils::head(display, 300)
    }
    for (metric_col in intersect(c("SpearmanRho", "AbsRho"), names(display))) {
      display[[metric_col]] <- round(display[[metric_col]], 5)
    }
    if ("PValue" %in% names(display)) {
      display$PValue <- signif(display$PValue, 5)
    }
    DT::datatable(display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, selection = "single")
  })

  output$geo_transcript_group_track <- renderPlotly({
    groups <- geo_transcript_groups()
    details <- geo_transcript_group_details_current()
    selected <- input$geo_transcript_groups_table_rows_selected
    req(is.data.frame(groups), nrow(groups) > 0, is.data.frame(details), nrow(details) > 0, length(selected) > 0)
    group_id <- groups$GroupID[[selected[[1]]]]
    track <- details[details$GroupID == group_id, , drop = FALSE]
    req(nrow(track) > 0)
    if (!"CpG" %in% names(track)) {
      shiny::validate(shiny::need(FALSE, "CpG-level detail is not included in this remote result. The transcript group summary is available above."))
    }
    detail_selected <- input$geo_transcript_group_details_table_rows_selected
    selected_cpg <- character(0)
    if (length(detail_selected) > 0) {
      detail_rows <- details[details$GroupID == group_id, , drop = FALSE]
      if (detail_selected[[1]] <= nrow(detail_rows) && "CpG" %in% names(detail_rows)) {
        selected_cpg <- as.character(detail_rows$CpG[[detail_selected[[1]]]])
      }
    }
    ml_summary <- geo_transcript_ml_results_current()
    if ((!is.data.frame(ml_summary) || nrow(ml_summary) == 0) && nzchar(input$geo_accession %||% "")) {
      ml_summary_path <- file.path(
        geo_transcript_ml_dir(
          ugplot_geo_cache_dir(trimws(input$geo_accession %||% "GEO")),
          geo_matrix_source_value(),
          geo_current_transcript_ml_run_key()
        ),
        "summary.csv"
      )
      if (file.exists(ml_summary_path)) {
        ml_summary <- tryCatch(utils::read.csv(ml_summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
        if (is.data.frame(ml_summary) && nrow(ml_summary) > 0) {
          geo_transcript_ml_results(ml_summary)
        }
      }
    }
    ml_row <- if (is.data.frame(ml_summary) && nrow(ml_summary) > 0 && "GroupID" %in% names(ml_summary)) {
      ml_summary[as.character(ml_summary$GroupID) == as.character(group_id), , drop = FALSE]
    } else {
      data.frame()
    }
    importance <- data.frame()
    if (is.data.frame(ml_row) && nrow(ml_row) > 0 && "ImportancePath" %in% names(ml_row)) {
      importance_path <- as.character(ml_row$ImportancePath[[1]])
      if (!is.na(importance_path) && nzchar(importance_path) && file.exists(importance_path)) {
        importance <- tryCatch(utils::read.csv(importance_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      }
    }
    if (is.data.frame(importance) && nrow(importance) > 0 && all(c("CpG", "Importance") %in% names(importance))) {
      importance <- importance[, intersect(c("CpG", "Importance", "ImportanceRank"), names(importance)), drop = FALSE]
      track <- merge(track, importance, by = "CpG", all.x = TRUE, sort = FALSE)
    } else {
      remote_importance <- geo_transcript_ml_importance_current()
      if (is.data.frame(remote_importance) && nrow(remote_importance) > 0 &&
          all(c("GroupID", "CpG", "Importance") %in% names(remote_importance))) {
        remote_importance <- remote_importance[as.character(remote_importance$GroupID) == as.character(group_id), , drop = FALSE]
        if (is.data.frame(remote_importance) && nrow(remote_importance) > 0) {
          remote_importance <- remote_importance[, intersect(c("CpG", "Importance", "ImportanceRank"), names(remote_importance)), drop = FALSE]
          track <- merge(track, remote_importance, by = "CpG", all.x = TRUE, sort = FALSE)
        } else {
          track$Importance <- NA_real_
          track$ImportanceRank <- NA_real_
        }
      } else {
        track$Importance <- NA_real_
        track$ImportanceRank <- NA_real_
      }
    }
    if (!"Importance" %in% names(track)) {
      track$Importance <- NA_real_
    }
    if (!"ImportanceRank" %in% names(track)) {
      track$ImportanceRank <- NA_real_
    }
    track$Importance <- suppressWarnings(as.numeric(track$Importance))
    max_importance <- suppressWarnings(max(track$Importance, na.rm = TRUE))
    if (!is.finite(max_importance) || max_importance <= 0) {
      track$ImportanceScaled <- 0
    } else {
      track$ImportanceScaled <- pmax(0, pmin(1, track$Importance / max_importance))
    }

    track$PositionNumeric <- suppressWarnings(as.numeric(track$Position))
    has_position <- any(is.finite(track$PositionNumeric))
    if (!has_position) {
      track <- track[order(track$Transcript, track$CpG), , drop = FALSE]
      track$PositionNumeric <- ave(seq_len(nrow(track)), track$Transcript, FUN = seq_along)
      x_label <- "CpG order"
    } else {
      track <- track[is.finite(track$PositionNumeric), , drop = FALSE]
      x_label <- "Genomic position"
    }
    req(nrow(track) > 0)

    region_order <- c("Promoter", "TSS1500", "TSS200", "5'UTR", "Exon", "1stExon", "Intron", "Body", "3'UTR")
    region_labels <- c(
      Promoter = "Promoter",
      TSS1500 = "Promoter TSS1500",
      TSS200 = "Promoter TSS200",
      `5'UTR` = "5' UTR",
      Exon = "Exon",
      `1stExon` = "First exon",
      Intron = "Intron",
      Body = "Gene body",
      `3'UTR` = "3' UTR"
    )
    build_txdb_region_bands <- function(df) {
      required <- c("TxDb.Hsapiens.UCSC.hg19.refGene", "GenomicFeatures", "GenomicRanges", "IRanges", "S4Vectors")
      if (!has_position || !all(vapply(required, requireNamespace, logical(1), quietly = TRUE))) {
        return(data.frame())
      }
      txdb <- tryCatch(
        get("TxDb.Hsapiens.UCSC.hg19.refGene", envir = asNamespace("TxDb.Hsapiens.UCSC.hg19.refGene")),
        error = function(e) NULL
      )
      if (is.null(txdb)) {
        return(data.frame())
      }
      tx_gr <- tryCatch(GenomicFeatures::transcripts(txdb, columns = c("tx_id", "tx_name")), error = function(e) NULL)
      if (is.null(tx_gr) || length(tx_gr) == 0 || !"tx_name" %in% names(S4Vectors::mcols(tx_gr))) {
        return(data.frame())
      }
      selected_tx <- unique(as.character(df$Transcript))
      tx_names <- as.character(S4Vectors::mcols(tx_gr)$tx_name)
      tx_keep <- which(tx_names %in% selected_tx)
      if (length(tx_keep) == 0) {
        return(data.frame())
      }
      tx_gr <- tx_gr[tx_keep]
      tx_names <- tx_names[tx_keep]
      tx_ids <- as.character(S4Vectors::mcols(tx_gr)$tx_id)
      names(tx_gr) <- tx_ids
      exon_by_tx <- tryCatch(GenomicFeatures::exonsBy(txdb, by = "tx", use.names = FALSE), error = function(e) NULL)
      utr5_by_tx <- tryCatch(GenomicFeatures::fiveUTRsByTranscript(txdb, use.names = FALSE), error = function(e) NULL)
      utr3_by_tx <- tryCatch(GenomicFeatures::threeUTRsByTranscript(txdb, use.names = FALSE), error = function(e) NULL)
      range_rows <- list()
      add_ranges <- function(tx_name, feature, gr, source_label) {
        if (is.null(gr) || length(gr) == 0) {
          return(NULL)
        }
        data.frame(
          Transcript = tx_name,
          Sector = feature,
          xmin = as.numeric(GenomicRanges::start(gr)),
          xmax = as.numeric(GenomicRanges::end(gr)),
          Source = source_label,
          stringsAsFactors = FALSE
        )
      }
      for (tx_i in seq_along(tx_ids)) {
        tx_id <- tx_ids[[tx_i]]
        tx_name <- tx_names[[tx_i]]
        tx_range <- tx_gr[tx_i]
        tx_start <- as.numeric(GenomicRanges::start(tx_range))
        tx_end <- as.numeric(GenomicRanges::end(tx_range))
        tx_strand <- as.character(GenomicRanges::strand(tx_range))
        promoter <- if (identical(tx_strand, "-")) {
          IRanges::IRanges(start = tx_end + 1, end = tx_end + 1500)
        } else {
          IRanges::IRanges(start = max(1, tx_start - 1500), end = max(1, tx_start - 1))
        }
        range_rows[[length(range_rows) + 1L]] <- add_ranges(
          tx_name, "Promoter",
          GenomicRanges::GRanges(seqnames = as.character(GenomicRanges::seqnames(tx_range)), ranges = promoter, strand = GenomicRanges::strand(tx_range)),
          "TxDb hg19 refGene"
        )
        exon_gr <- if (!is.null(exon_by_tx) && tx_id %in% names(exon_by_tx)) exon_by_tx[[tx_id]] else NULL
        if (!is.null(exon_gr) && length(exon_gr) > 0) {
          range_rows[[length(range_rows) + 1L]] <- add_ranges(tx_name, "Exon", exon_gr, "TxDb hg19 refGene")
          exon_ranges <- IRanges::reduce(GenomicRanges::ranges(exon_gr))
          if (length(exon_ranges) > 1) {
            intron_ranges <- IRanges::IRanges(
              start = utils::head(IRanges::end(exon_ranges), -1) + 1,
              end = utils::tail(IRanges::start(exon_ranges), -1) - 1
            )
            intron_ranges <- intron_ranges[IRanges::width(intron_ranges) > 0]
            if (length(intron_ranges) > 0) {
              range_rows[[length(range_rows) + 1L]] <- add_ranges(
                tx_name, "Intron",
                GenomicRanges::GRanges(seqnames = as.character(GenomicRanges::seqnames(tx_range)), ranges = intron_ranges, strand = GenomicRanges::strand(tx_range)),
                "TxDb hg19 refGene"
              )
            }
          }
        }
        if (!is.null(utr5_by_tx) && tx_id %in% names(utr5_by_tx)) {
          range_rows[[length(range_rows) + 1L]] <- add_ranges(tx_name, "5'UTR", utr5_by_tx[[tx_id]], "TxDb hg19 refGene")
        }
        if (!is.null(utr3_by_tx) && tx_id %in% names(utr3_by_tx)) {
          range_rows[[length(range_rows) + 1L]] <- add_ranges(tx_name, "3'UTR", utr3_by_tx[[tx_id]], "TxDb hg19 refGene")
        }
      }
      bands <- do.call(rbind, range_rows)
      if (!is.data.frame(bands) || nrow(bands) == 0) {
        return(data.frame())
      }
      plot_range <- range(df$PositionNumeric, na.rm = TRUE)
      bands$xmin <- pmax(bands$xmin, plot_range[[1]])
      bands$xmax <- pmin(bands$xmax, plot_range[[2]])
      bands <- bands[is.finite(bands$xmin) & is.finite(bands$xmax) & bands$xmax >= bands$xmin, , drop = FALSE]
      if (nrow(bands) == 0) {
        return(data.frame())
      }
      bands$x <- (bands$xmin + bands$xmax) / 2
      bands$width <- pmax(1, bands$xmax - bands$xmin)
      bands$SectorLabel <- unname(region_labels[bands$Sector])
      bands$Label <- paste0(
        "Transcript: ", bands$Transcript,
        "<br>Region: ", bands$SectorLabel,
        "<br>Span: ", round(bands$xmin), "-", round(bands$xmax),
        "<br>Source: ", bands$Source
      )
      bands$Sector <- factor(bands$Sector, levels = region_order)
      bands
    }
    build_illumina_region_bands <- function(df) {
      if (!has_position || !"GeneRegion" %in% names(df) || !any(is.finite(df$PositionNumeric))) {
        return(data.frame())
      }
      band_rows <- lapply(split(df, as.character(df$Transcript)), function(tx_df) {
        tx_df <- tx_df[is.finite(tx_df$PositionNumeric), , drop = FALSE]
        if (nrow(tx_df) == 0) {
          return(data.frame())
        }
        tx_df <- tx_df[order(tx_df$PositionNumeric), , drop = FALSE]
        raw_regions <- trimws(as.character(tx_df$GeneRegion))
        primary_region <- vapply(strsplit(raw_regions, ";", fixed = TRUE), function(parts) {
          parts <- trimws(parts)
          parts <- parts[nzchar(parts)]
          matched <- parts[parts %in% region_order]
          if (length(matched) > 0) matched[[1]] else NA_character_
        }, character(1))
        keep <- !is.na(primary_region) & nzchar(primary_region)
        tx_df <- tx_df[keep, , drop = FALSE]
        primary_region <- primary_region[keep]
        if (nrow(tx_df) == 0) {
          return(data.frame())
        }
        x_values <- tx_df$PositionNumeric
        padding <- diff(range(df$PositionNumeric, na.rm = TRUE)) * 0.01
        if (!is.finite(padding) || padding <= 0) {
          padding <- 1
        }
        boundaries <- if (length(x_values) == 1) {
          c(x_values - padding, x_values + padding)
        } else {
          mids <- (utils::head(x_values, -1) + utils::tail(x_values, -1)) / 2
          c(min(x_values) - padding, mids, max(x_values) + padding)
        }
        runs <- rle(primary_region)
        ends <- cumsum(runs$lengths)
        starts <- c(1, utils::head(ends, -1) + 1)
        do.call(rbind, lapply(seq_along(runs$values), function(i) {
          xmin <- boundaries[starts[[i]]]
          xmax <- boundaries[ends[[i]] + 1]
          data.frame(
            Transcript = tx_df$Transcript[[1]],
            Sector = runs$values[[i]],
            SectorLabel = unname(region_labels[runs$values[[i]]]),
            x = (xmin + xmax) / 2,
            width = max(padding, xmax - xmin),
            Label = paste0(
              "Transcript: ", tx_df$Transcript[[1]],
              "<br>Region: ", unname(region_labels[runs$values[[i]]]),
              "<br>Approx. span: ", round(xmin), "-", round(xmax),
              "<br>Source: Illumina CpG GeneRegion annotation"
            ),
            stringsAsFactors = FALSE
          )
        }))
      })
      bands <- do.call(rbind, band_rows)
      if (!is.data.frame(bands) || nrow(bands) == 0) {
        return(data.frame())
      }
      bands$Sector <- factor(bands$Sector, levels = region_order)
      bands
    }
    region_bands <- build_illumina_region_bands(track)
    if (!is.data.frame(region_bands) || nrow(region_bands) == 0) {
      region_bands <- build_txdb_region_bands(track)
    }

    track$Transcript <- factor(track$Transcript, levels = rev(unique(as.character(track$Transcript))))
    if (is.data.frame(region_bands) && nrow(region_bands) > 0) {
      region_bands$Transcript <- factor(region_bands$Transcript, levels = levels(track$Transcript))
    }
    if (!"Strand" %in% names(track)) {
      track$Strand <- NA_character_
    }
    track$CpGKeptForML <- as.logical(track$CpGKeptForML)
    track$RhoScaled <- pmax(-1, pmin(1, suppressWarnings(as.numeric(track$SpearmanRho))))
    track$AbsRhoScaled <- pmax(0, pmin(1, suppressWarnings(as.numeric(track$AbsRho))))
    track$YOffset <- ifelse(is.finite(track$RhoScaled), 0.34 * track$RhoScaled, 0)
    track$YBase <- as.numeric(track$Transcript)
    track$YEnd <- track$YBase + track$YOffset
    track$SelectedCpG <- as.character(track$CpG) %in% selected_cpg
    track$StrandDisplay <- ifelse(is.na(track$Strand) | !nzchar(trimws(track$Strand)), "genomic coordinate ->", track$Strand)
    track$Label <- paste0(
      "Transcript: ", track$Transcript,
      "<br>CpG: ", track$CpG,
      "<br>Gene region: ", track$GeneRegion,
      "<br><span style='font-size:14px'><b>Position: ", track$Chr, ":", track$Position, "</b></span>",
      "<br>Direction: ", track$StrandDisplay,
      "<br><span style='font-size:14px'><b>Spearman rho: ", signif(track$SpearmanRho, 4), "</b></span>",
      "<br><span style='font-size:14px'><b>|rho|: ", signif(track$AbsRho, 4), "</b></span>",
      "<br>ML importance: ", ifelse(is.finite(track$Importance), signif(track$Importance, 4), "not run"),
      "<br>Importance rank: ", ifelse(is.finite(track$ImportanceRank), track$ImportanceRank, "not run"),
      "<br>Kept for ML: ", ifelse(track$CpGKeptForML, "yes", "no")
    )
    segment_df <- do.call(rbind, lapply(split(track, track$Transcript), function(df) {
      strand_values <- trimws(as.character(df$Strand))
      strand_values <- strand_values[!is.na(strand_values) & nzchar(strand_values)]
      strand <- if (length(strand_values) > 0) strand_values[[1]] else NA_character_
      is_reverse <- !is.na(strand) && tolower(strand) %in% c("-", "-1", "minus", "reverse")
      x_start <- min(df$PositionNumeric, na.rm = TRUE)
      x_end <- max(df$PositionNumeric, na.rm = TRUE)
      data.frame(
        Transcript = df$Transcript[[1]],
        x_start = x_start,
        x_end = x_end,
        arrow_start = if (is_reverse) x_end else x_start,
        arrow_end = if (is_reverse) x_start else x_end,
        stringsAsFactors = FALSE
      )
    }))

    p <- ggplot(track, aes(x = PositionNumeric, y = Transcript))
    if (is.data.frame(region_bands) && nrow(region_bands) > 0) {
      p <- p +
        geom_tile(
          data = region_bands,
          aes(x = x, y = Transcript, width = width, height = 0.20, fill = Sector, text = Label),
          inherit.aes = FALSE,
          alpha = 0.32,
          color = NA
        ) +
        scale_fill_manual(
          values = c(
            Promoter = "#f6c85f",
            TSS1500 = "#f6c85f",
            TSS200 = "#f08a4b",
            `5'UTR` = "#8ecae6",
            Exon = "#4dab6d",
            `1stExon` = "#4dab6d",
            Intron = "#d9e2ec",
            Body = "#b8c0ff",
            `3'UTR` = "#c77dff"
          ),
          labels = region_labels,
          na.translate = FALSE,
          name = "Region"
        )
    }
    p <- p +
      geom_segment(
        data = segment_df,
        aes(x = x_start, xend = x_end, y = Transcript, yend = Transcript),
        inherit.aes = FALSE,
        color = "#aeb8c4",
        linewidth = 2,
        lineend = "round"
      ) +
      geom_segment(
        data = segment_df,
        aes(x = arrow_start, xend = arrow_end, y = Transcript, yend = Transcript),
        inherit.aes = FALSE,
        color = "#5f6f7f",
        linewidth = 0.65,
        arrow = grid::arrow(length = grid::unit(0.16, "cm"), type = "closed")
      ) +
      geom_segment(
        aes(xend = PositionNumeric, y = YBase, yend = YEnd, color = AbsRhoScaled, alpha = CpGKeptForML, text = Label),
        linewidth = 1.2,
        lineend = "round"
      ) +
      geom_point(
        aes(y = YEnd, color = AbsRhoScaled, alpha = CpGKeptForML, size = ImportanceScaled, text = Label),
        shape = 21,
        stroke = 0.7,
        fill = "white"
      )
    selected_track <- track[isTRUE(length(selected_cpg) > 0) & track$SelectedCpG, , drop = FALSE]
    if (is.data.frame(selected_track) && nrow(selected_track) > 0) {
      p <- p +
        geom_point(
          data = selected_track,
          aes(x = PositionNumeric, y = YEnd, text = Label),
          inherit.aes = FALSE,
          shape = 21,
          size = 8.5,
          stroke = 1.5,
          color = "#111827",
          fill = NA
        ) +
        geom_point(
          data = selected_track,
          aes(x = PositionNumeric, y = YEnd, text = Label),
          inherit.aes = FALSE,
          shape = 4,
          size = 4.5,
          stroke = 1.2,
          color = "#111827"
        )
    }
    p <- p +
      viridis::scale_color_viridis(option = "plasma", discrete = FALSE, na.value = "#8792a2", limits = c(0, 1)) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.3), guide = "none") +
      scale_size_continuous(
        range = c(2.6, 7),
        breaks = c(0, 0.5, 1),
        labels = function(x) {
          ifelse(x <= 0, "none", ifelse(x >= 1, "high", "mid"))
        },
        name = "ML importance"
      ) +
      scale_y_discrete(expand = expansion(mult = c(0.35, 0.35))) +
      labs(x = x_label, y = NULL, color = "|rho|") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold"),
        legend.position = "right",
        plot.margin = ggplot2::margin(8, 12, 8, 8)
      )
    plotly::layout(
      plotly::ggplotly(p, tooltip = "text"),
      margin = list(l = 80, r = 40, t = 10, b = 45),
      xaxis = list(rangeslider = list(visible = TRUE))
    )
  })

  observeEvent(input$geo_matrix_source, {
    source <- geo_matrix_source_value(input$geo_matrix_source %||% "processed")
    remote_result <- remote_job_preview_result()
    if (isTRUE(remote_geo_result_applying())) {
      return()
    }
    if (geo_remote_mode_active() &&
        is.list(remote_result) &&
        identical(remote_result$kind %||% "", "geo_pipeline") &&
        identical(remote_result$matrix_source %||% source, source)) {
      if (!is.data.frame(geo_sample_metadata()) || nrow(geo_sample_metadata()) == 0 ||
          !is.data.frame(geo_spearman_raw_results()) || nrow(geo_spearman_raw_results()) == 0) {
        remote_geo_result_applying(TRUE)
        try(apply_remote_geo_result(remote_result, geo_remote_pipeline_job_id(), clear_existing = FALSE, update_inputs = FALSE), silent = TRUE)
        session$onFlushed(function() {
          session$onFlushed(function() {
            remote_geo_result_applying(FALSE)
          }, once = TRUE)
        }, once = TRUE)
      }
      geo_stage(list(
        step = "Remote GEO",
        title = "Remote matrix source loaded",
        message = paste0(
          "Using ", geo_matrix_source_label(source),
          " from the loaded remote GEO result. Remote cache: ",
          remote_result$cache_dir %||% ""
        )
      ))
      return()
    }
    geo_spearman_results(data.frame())
    geo_spearman_raw_results(data.frame())
    geo_transcript_candidates(data.frame())
    geo_transcript_groups(data.frame())
    geo_transcript_group_details(data.frame())
    geo_transcript_ml_results(data.frame())
    geo_preview_data(data.frame())
    update_geo_transcript_build_progress(
      phase = "idle",
      message = "Matrix source changed. Run Spearman again before building transcript datasets.",
      processed = 0L,
      total = 0L,
      compatible = 0L,
      excluded = 0L,
      current = "",
      cache = ""
    )
    update_geo_transcript_ml_progress(
      phase = "idle",
      message = "Matrix source changed. Load or run transcript ML for this source.",
      processed = 0L,
      total = 0L,
      current = "",
      cache = ""
    )
    remote_files <- geo_remote_files()
    accession <- isolate(trimws(input$geo_accession %||% ""))
    if (!nzchar(accession)) {
      return()
    }
    cache_dir <- ugplot_geo_cache_dir(accession)
    load_geo_cached_state(accession)
    if (!is.data.frame(remote_files) || nrow(remote_files) == 0) {
      return()
    }
    remote_files <- ugplot_geo_annotate_remote_files(remote_files, cache_dir)
    geo_remote_files(remote_files)
    processed_files <- remote_files[remote_files$Loadable, , drop = FALSE]
    pending_files <- processed_files[processed_files$NeedsDownload, , drop = FALSE]
    selected_files <- geo_download_selection(remote_files, input$geo_matrix_source %||% "processed")
    selected_pending <- selected_files[selected_files$NeedsDownload, , drop = FALSE]
    if (nrow(selected_files) > 0 && nrow(selected_pending) == 0) {
      shinyjs::disable("geo_fetch_files")
      if (any(processed_files$LocalStatus == "downloaded" & grepl("\\.gz$", processed_files$File, ignore.case = TRUE))) {
        shinyjs::enable("geo_extract_files")
      } else {
        shinyjs::disable("geo_extract_files")
      }
      load_geo_cached_state(accession)
      geo_stage(list(
        step = "Step 5",
        title = "Matrix files already local",
        message = paste0("Required ", geo_matrix_source_label(source), " files are already available locally. Continue with the next step for this path.")
      ))
    } else {
      shinyjs::enable("geo_fetch_files")
      shinyjs::disable("geo_extract_files")
      geo_stage(list(step = "Step 3", title = "Review matrix download plan", message = paste0("Selected ", geo_matrix_source_label(source), " files still needed: ", nrow(selected_pending), " file(s), ", ugplot_format_bytes(sum(ugplot_geo_size_bytes(selected_pending), na.rm = TRUE)), ".")))
    }
  }, ignoreInit = TRUE)

	  observeEvent(input$geo_inspect_files, {
	    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      geo_status("Please enter a GEO accession, for example GSE87571.")
      geo_stage(list(step = "Step 1", title = "Missing accession", message = "Enter a GEO accession before inspecting files."))
      return()
    }
    if (!requireNamespace("GEOquery", quietly = TRUE)) {
      geo_status("Package 'GEOquery' is not installed. Install it with BiocManager::install('GEOquery') before using GEO import.")
      geo_stage(list(step = "Step 1", title = "GEOquery is missing", message = "Install GEOquery before using GEO import."))
      return()
    }
    geo_files(data.frame())
    geo_sample_metadata(data.frame())
    geo_cpg_annotation(data.frame())
    geo_spearman_raw_results(data.frame())
    geo_spearman_results(data.frame())
    geo_transcript_candidates(data.frame())
    geo_preview_data(data.frame())
    geo_status(ugplot_geo_append_log("", paste0("Inspecting GEO metadata for ", accession, "...")))
    geo_stage(list(step = "Step 1", title = "Inspecting GEO", message = paste0("Reading metadata for ", accession, " and checking supplementary file sizes.")))
    geo_download_progress(list(percent = 0, file = "Waiting", detail = "No download running.", folder = ugplot_geo_cache_dir(accession)))
    tryCatch({
      remote_files <- ugplot_geo_remote_supp_files(accession)
      cache_dir <- ugplot_geo_cache_dir(accession)
      cached_metadata_path <- ugplot_geo_sample_metadata_path(cache_dir, "rds")
      if (file.exists(cached_metadata_path)) {
        cached_metadata <- tryCatch(readRDS(cached_metadata_path), error = function(e) data.frame())
        if (is.data.frame(cached_metadata) && nrow(cached_metadata) > 0) {
          geo_sample_metadata(cached_metadata)
          geo_status(ugplot_geo_append_log(geo_status(), paste0("Loaded cached sample metadata: ", nrow(cached_metadata), " samples.")))
          cached_annotation <- ugplot_geo_load_annotation_cache(ugplot_geo_detect_platform(cached_metadata))
          if (is.data.frame(cached_annotation) && nrow(cached_annotation) > 0) {
            geo_cpg_annotation(cached_annotation)
            geo_status(ugplot_geo_append_log(geo_status(), paste0("Loaded cached CpG annotation: ", nrow(cached_annotation), " CpG-gene/transcript links.")))
          }
        }
      }
      remote_files <- ugplot_geo_annotate_remote_files(remote_files, cache_dir)
      ugplot_geo_write_manifest(cache_dir, accession, remote_files)
      geo_remote_files(remote_files)
      local_files <- ugplot_geo_list_candidate_files(accession, cache_dir)
      geo_files(local_files)
      load_geo_cached_state(accession)
      if (nrow(remote_files) == 0) {
        geo_status(ugplot_geo_append_log(geo_status(), paste0("No supplementary files listed for ", accession, ".")))
        geo_stage(list(step = "Step 2", title = "No supplementary files", message = paste0("No downloadable supplementary files were listed for ", accession, ".")))
        shinyjs::disable("geo_fetch_files")
      } else {
        processed_files <- remote_files[remote_files$Loadable, , drop = FALSE]
        pending_files <- processed_files[processed_files$NeedsDownload, , drop = FALSE]
        selected_files <- geo_download_selection(remote_files, input$geo_matrix_source %||% "processed")
        selected_pending <- selected_files[selected_files$NeedsDownload, , drop = FALSE]
        selected_size <- sum(ugplot_geo_size_bytes(processed_files), na.rm = TRUE)
        geo_status(ugplot_geo_append_log(
          geo_status(),
          paste0(
            "Found ", nrow(remote_files), " remote supplementary files: ",
            ugplot_format_bytes(sum(ugplot_geo_size_bytes(remote_files), na.rm = TRUE)), " known size, ",
            sum(remote_files$Loadable), " processed table candidates, ",
            sum(remote_files$Type == "IDAT"), " IDAT files."
          )
        ))
        geo_status(ugplot_geo_append_log(geo_status(), "Review the table, then click Download supplementary files if it looks correct."))
        shinyjs::enable("geo_fetch_metadata")
        geo_stage(list(
          step = if (is.data.frame(geo_sample_metadata()) && nrow(geo_sample_metadata()) > 0) {
            if (nrow(processed_files) > 0 && nrow(pending_files) == 0) "Step 5" else "Step 3"
          } else {
            "Step 2"
          },
          title = if (!is.data.frame(geo_sample_metadata()) || nrow(geo_sample_metadata()) == 0) {
            "Fetch sample metadata"
          } else if (nrow(processed_files) > 0 && nrow(pending_files) == 0) {
            "Matrix files already local"
          } else {
            "Review matrix download plan"
          },
          message = if (!is.data.frame(geo_sample_metadata()) || nrow(geo_sample_metadata()) == 0) {
            "Fetch sample metadata next so phenotypes can be matched to the matrix samples."
          } else if (nrow(processed_files) > 0 && nrow(pending_files) == 0) {
            paste0("Required processed matrix files are already available locally.")
          } else {
            paste0(
              "Required processed matrices: ", nrow(processed_files), " file(s), about ",
              ugplot_format_bytes(selected_size), ". Still needed: ",
              nrow(pending_files), " file(s)."
            )
          }
        ))
        if (nrow(selected_files) > 0 && nrow(selected_pending) == 0) {
          shinyjs::disable("geo_fetch_files")
          if (any(processed_files$LocalStatus == "downloaded" & grepl("\\.gz$", processed_files$File, ignore.case = TRUE))) {
            shinyjs::enable("geo_extract_files")
          } else {
            shinyjs::disable("geo_extract_files")
          }
        } else {
          shinyjs::enable("geo_fetch_files")
          shinyjs::disable("geo_extract_files")
        }
      }
    }, error = function(e) {
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Could not inspect GEO metadata: ", conditionMessage(e))))
      geo_stage(list(step = "Step 1", title = "Inspection failed", message = conditionMessage(e)))
    })
  })

	  observeEvent(input$geo_fetch_metadata, {
	    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      geo_stage(list(step = "Step 2", title = "Missing accession", message = "Enter and inspect a GEO accession before fetching sample metadata."))
      return()
    }
    cache_dir <- ugplot_geo_cache_dir(accession)
    geo_status(ugplot_geo_append_log(geo_status(), paste0("Fetching sample metadata for ", accession, "...")))
    geo_stage(list(step = "Step 2", title = "Fetching sample metadata", message = "Reading GEO series matrix metadata and parsing sample characteristics."))
    tryCatch({
      metadata <- ugplot_geo_fetch_sample_metadata(accession, cache_dir)
      geo_sample_metadata(metadata)
      cached_annotation <- ugplot_geo_load_annotation_cache(ugplot_geo_detect_platform(metadata))
      if (is.data.frame(cached_annotation) && nrow(cached_annotation) > 0) {
        geo_cpg_annotation(cached_annotation)
      }
      likely_targets <- grep("age|sex|gender|disease|status|treatment|response|case|control|group|phenotype", names(metadata), value = TRUE, ignore.case = TRUE)
      geo_status(ugplot_geo_append_log(
        geo_status(),
        paste0(
          "Sample metadata ready: ", nrow(metadata), " samples, ", ncol(metadata),
          " columns. Saved to ", ugplot_geo_sample_metadata_path(cache_dir, "csv"), "."
        )
      ))
      if (length(likely_targets) > 0) {
        geo_status(ugplot_geo_append_log(geo_status(), paste0("Likely phenotype fields: ", paste(utils::head(likely_targets, 12), collapse = ", "), ".")))
      }
      remote_files <- geo_remote_files()
      if (is.data.frame(remote_files) && nrow(remote_files) > 0) {
        remote_files <- ugplot_geo_annotate_remote_files(remote_files, cache_dir)
        geo_remote_files(remote_files)
        processed_files <- remote_files[remote_files$Loadable, , drop = FALSE]
        pending_files <- processed_files[processed_files$NeedsDownload, , drop = FALSE]
        selected_files <- geo_download_selection(remote_files, input$geo_matrix_source %||% "processed")
        selected_pending <- selected_files[selected_files$NeedsDownload, , drop = FALSE]
        if (nrow(selected_files) == 0 || nrow(selected_pending) > 0) {
          shinyjs::enable("geo_fetch_files")
          geo_stage(list(step = "Step 3", title = "Review matrix download plan", message = paste0("Metadata is ready. Selected GEO files still needed: ", nrow(selected_pending), " file(s), about ", ugplot_format_bytes(sum(ugplot_geo_size_bytes(selected_pending), na.rm = TRUE)), ".")))
        } else {
          shinyjs::disable("geo_fetch_files")
          if (any(processed_files$LocalStatus == "downloaded" & grepl("\\.gz$", processed_files$File, ignore.case = TRUE))) {
            shinyjs::enable("geo_extract_files")
          }
          geo_stage(list(step = "Step 5", title = "Matrix files already local", message = "Metadata is ready and required processed matrix files are local. Extract compressed matrices next."))
        }
      } else {
        geo_stage(list(step = "Step 3", title = "Inspect matrix files", message = "Metadata is ready. Inspect GEO files to plan matrix downloads."))
      }
    }, error = function(e) {
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Could not fetch sample metadata: ", conditionMessage(e))))
      geo_stage(list(step = "Step 2", title = "Metadata failed", message = conditionMessage(e)))
    })
  })

	  observeEvent(input$geo_fetch_files, {
	    if (block_local_geo_step_when_remote("GEO file download")) return()
	    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      geo_status("Please enter a GEO accession, for example GSE87571.")
      geo_stage(list(step = "Step 3", title = "Missing accession", message = "Enter a GEO accession before downloading."))
      return()
    }
    if (!requireNamespace("GEOquery", quietly = TRUE)) {
      geo_status("Package 'GEOquery' is not installed. Install it with BiocManager::install('GEOquery') before using GEO import.")
      geo_stage(list(step = "Step 3", title = "GEOquery is missing", message = "Install GEOquery before using GEO import."))
      return()
    }
    cache_dir <- ugplot_geo_cache_dir(accession)
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    geo_status(ugplot_geo_append_log(geo_status(), paste0("Download folder: ", cache_dir)))
    geo_stage(list(step = "Step 4", title = "Preparing download", message = paste0("Files will be saved under ", cache_dir, ".")))
    geo_download_progress(list(percent = 0, file = "Preparing", detail = "Preparing download plan.", folder = cache_dir))
    session$sendCustomMessage("geoProgress", list(percent = 0, file = "Preparing", detail = "Preparing download plan."))
    remote_files <- geo_remote_files()
    if (is.data.frame(remote_files) && nrow(remote_files) > 0) {
      remote_files <- ugplot_geo_annotate_remote_files(remote_files, cache_dir)
      geo_remote_files(remote_files)
      geo_status(ugplot_geo_append_log(
        geo_status(),
        paste0(
          "Planned download: ", nrow(remote_files), " files; ",
          ugplot_format_bytes(sum(ugplot_geo_size_bytes(remote_files), na.rm = TRUE)), " known size; ",
          sum(remote_files$Loadable), " table candidates; ",
          sum(remote_files$Type == "IDAT"), " IDAT files."
        )
      ))
    } else {
      geo_status(ugplot_geo_append_log(geo_status(), "No prior inspection found; inspecting GEO metadata before downloading."))
      tryCatch({
        remote_files <- ugplot_geo_remote_supp_files(accession)
        remote_files <- ugplot_geo_annotate_remote_files(remote_files, cache_dir)
        geo_remote_files(remote_files)
      }, error = function(e) {
        remote_files <- data.frame()
        geo_status(ugplot_geo_append_log(geo_status(), paste0("Could not inspect GEO metadata before download: ", conditionMessage(e))))
        geo_stage(list(step = "Step 4", title = "Download blocked", message = conditionMessage(e)))
      })
    }
    geo_preview_data(data.frame())
    tryCatch({
      if (!is.data.frame(remote_files) || nrow(remote_files) == 0) {
        stop("No supplementary files were found to download.")
      }
      source <- input$geo_matrix_source %||% "processed"
      selected_remote_files <- geo_download_selection(remote_files, source)
      if (identical(source, "processed")) {
        remote_files <- selected_remote_files
        if (nrow(remote_files) == 0) {
          stop("No directly loadable processed tables were found for this GEO accession.")
        }
        geo_status(ugplot_geo_append_log(
          geo_status(),
          paste0("Processed matrix workflow selected: downloading ", nrow(remote_files), " loadable processed table(s).")
        ))
      } else {
        remote_files <- selected_remote_files
        if (nrow(remote_files) == 0) {
          stop("No raw IDAT/archive supplementary files were found for sesame reprocessing.")
        }
        geo_status(ugplot_geo_append_log(
          geo_status(),
          paste0("Raw IDAT sesame workflow selected: downloading ", nrow(remote_files), " raw IDAT/archive file(s).")
        ))
      }
      remote_files <- remote_files[remote_files$NeedsDownload, , drop = FALSE]
      if (nrow(remote_files) == 0) {
        source <- input$geo_matrix_source %||% "processed"
        files <- ugplot_geo_list_candidate_files(accession, cache_dir)
        geo_files(files)
        annotated_files <- ugplot_geo_annotate_remote_files(geo_remote_files(), cache_dir)
        geo_remote_files(annotated_files)
        ugplot_geo_write_manifest(cache_dir, accession, annotated_files)
        shinyjs::disable("geo_fetch_files")
        if (any(annotated_files$LocalStatus == "downloaded" & annotated_files$Loadable & grepl("\\.gz$", annotated_files$File, ignore.case = TRUE))) {
          shinyjs::enable("geo_extract_files")
        }
        geo_stage(list(
          step = "Step 5",
          title = "Files already local",
          message = if (identical(source, "raw_sesame")) {
            "All selected raw files are already local. Run sesame IDAT QC/reprocessing for this path."
          } else {
            "All selected processed matrices are already local. Extract compressed matrices before preprocessing."
          }
        ))
        geo_download_progress(list(percent = 100, file = "Already downloaded", detail = paste0(nrow(files), " local file(s) available."), folder = cache_dir))
        session$sendCustomMessage("geoProgress", list(percent = 100, file = "Already downloaded", detail = paste0(nrow(files), " local file(s) available.")))
        return()
      }

      total_known_bytes <- sum(ugplot_geo_size_bytes(remote_files), na.rm = TRUE)
      completed_known_bytes <- 0
      geo_stage(list(step = "Step 4", title = "Downloading", message = paste0("Downloading ", nrow(remote_files), " file(s) into ", cache_dir, ".")))
      withProgress(message = paste0("Downloading GEO files for ", accession), value = 0, {
        for (file_i in seq_len(nrow(remote_files))) {
          remote_file <- remote_files[file_i, , drop = FALSE]
          destination <- file.path(cache_dir, remote_file$File)
          file_size_bytes <- ugplot_geo_size_bytes(remote_file)[[1]]
          file_size_label <- if (is.finite(file_size_bytes)) ugplot_format_bytes(file_size_bytes) else "unknown size"
          start_pct <- if (total_known_bytes > 0) round(100 * completed_known_bytes / total_known_bytes, 1) else round(100 * (file_i - 1) / nrow(remote_files), 1)
          geo_download_progress(list(
            percent = start_pct,
            file = remote_file$File,
            detail = paste0("Starting ", file_i, " of ", nrow(remote_files), " - ", file_size_label),
            folder = cache_dir
          ))
          session$sendCustomMessage("geoProgress", list(
            percent = start_pct,
            file = remote_file$File,
            detail = paste0("Starting ", file_i, " of ", nrow(remote_files), " - ", file_size_label)
          ))
          geo_status(ugplot_geo_append_log(
            geo_status(),
            paste0(
              "Downloading ", file_i, "/", nrow(remote_files), " (", start_pct, "%): ",
              remote_file$File, " [", file_size_label, "] -> ", destination
            )
          ))
          shiny::setProgress(
            value = if (total_known_bytes > 0) completed_known_bytes / total_known_bytes else (file_i - 1) / nrow(remote_files),
            detail = paste0(remote_file$File, " (", file_size_label, ")")
          )

          if (file.exists(destination)) {
            local_size <- file.info(destination)$size
            if ((!is.finite(file_size_bytes) || identical(as.numeric(local_size), as.numeric(file_size_bytes))) && ugplot_geo_gzip_valid(destination)) {
              geo_status(ugplot_geo_append_log(geo_status(), paste0("Already present, skipping: ", destination)))
            } else {
              if (!ugplot_geo_gzip_valid(destination)) {
                geo_status(ugplot_geo_append_log(geo_status(), paste0("Existing gzip failed integrity check; re-downloading: ", destination)))
              } else {
                geo_status(ugplot_geo_append_log(geo_status(), paste0("Existing file size differs; re-downloading: ", destination)))
              }
              last_sent_pct <- start_pct
              ugplot_geo_download_file(remote_file$URL, destination, expected_size = file_size_bytes, progress_callback = function(downloaded_bytes, total_bytes) {
                current_file_bytes <- if (is.finite(downloaded_bytes)) downloaded_bytes else 0
                current_total_bytes <- if (is.finite(total_bytes) && total_bytes > 0) total_bytes else file_size_bytes
                overall_pct <- if (total_known_bytes > 0) {
                  round(100 * min(total_known_bytes, completed_known_bytes + current_file_bytes) / total_known_bytes, 1)
                } else if (is.finite(current_total_bytes) && current_total_bytes > 0) {
                  round(100 * ((file_i - 1) + current_file_bytes / current_total_bytes) / nrow(remote_files), 1)
                } else {
                  start_pct
                }
                if (overall_pct >= last_sent_pct + 1 || overall_pct >= 100) {
                  last_sent_pct <<- overall_pct
                  shiny::setProgress(value = overall_pct / 100, detail = paste0(remote_file$File, " - ", overall_pct, "%"))
                  session$sendCustomMessage("geoProgress", list(
                    percent = overall_pct,
                    file = remote_file$File,
                    detail = paste0("Downloaded ", ugplot_format_bytes(current_file_bytes), " of ", ugplot_format_bytes(current_total_bytes))
                  ))
                }
              })
            }
          } else {
            last_sent_pct <- start_pct
            ugplot_geo_download_file(remote_file$URL, destination, expected_size = file_size_bytes, progress_callback = function(downloaded_bytes, total_bytes) {
              current_file_bytes <- if (is.finite(downloaded_bytes)) downloaded_bytes else 0
              current_total_bytes <- if (is.finite(total_bytes) && total_bytes > 0) total_bytes else file_size_bytes
              overall_pct <- if (total_known_bytes > 0) {
                round(100 * min(total_known_bytes, completed_known_bytes + current_file_bytes) / total_known_bytes, 1)
              } else if (is.finite(current_total_bytes) && current_total_bytes > 0) {
                round(100 * ((file_i - 1) + current_file_bytes / current_total_bytes) / nrow(remote_files), 1)
              } else {
                start_pct
              }
              if (overall_pct >= last_sent_pct + 1 || overall_pct >= 100) {
                last_sent_pct <<- overall_pct
                shiny::setProgress(value = overall_pct / 100, detail = paste0(remote_file$File, " - ", overall_pct, "%"))
                session$sendCustomMessage("geoProgress", list(
                  percent = overall_pct,
                  file = remote_file$File,
                  detail = paste0("Downloaded ", ugplot_format_bytes(current_file_bytes), " of ", ugplot_format_bytes(current_total_bytes))
                ))
              }
            })
          }

          if (is.finite(file_size_bytes)) {
            completed_known_bytes <- completed_known_bytes + file_size_bytes
          }
          done_pct <- if (total_known_bytes > 0) round(100 * completed_known_bytes / total_known_bytes, 1) else round(100 * file_i / nrow(remote_files), 1)
          progress_increment <- if (total_known_bytes > 0 && is.finite(file_size_bytes)) file_size_bytes / total_known_bytes else 1 / nrow(remote_files)
          shiny::incProgress(
            progress_increment,
            detail = paste0("Finished ", remote_file$File, " (", done_pct, "%)")
          )
          geo_download_progress(list(
            percent = done_pct,
            file = remote_file$File,
            detail = paste0("Finished ", file_i, " of ", nrow(remote_files), "."),
            folder = cache_dir
          ))
          session$sendCustomMessage("geoProgress", list(
            percent = done_pct,
            file = remote_file$File,
            detail = paste0("Finished ", file_i, " of ", nrow(remote_files), ".")
          ))
          geo_status(ugplot_geo_append_log(geo_status(), paste0("Finished ", remote_file$File, ". Progress: ", done_pct, "%.")))
        }
      })
      geo_status(ugplot_geo_append_log(geo_status(), "Download finished. Scanning local files."))
      files <- ugplot_geo_list_candidate_files(accession, cache_dir)
      geo_files(files)
      annotated_files <- ugplot_geo_annotate_remote_files(geo_remote_files(), cache_dir)
      geo_remote_files(annotated_files)
      ugplot_geo_write_manifest(cache_dir, accession, annotated_files)
      if (nrow(files) == 0) {
        geo_status(ugplot_geo_append_log(geo_status(), paste0("No supplementary files found for ", accession, ".")))
      } else {
        loadable_n <- sum(files$Loadable)
        idat_n <- sum(files$Type == "IDAT")
        geo_status(ugplot_geo_append_log(geo_status(), paste0(
          "Fetched ", nrow(files), " files for ", accession, ". ",
          loadable_n, " processed table candidates are directly loadable. ",
          idat_n, " IDAT files detected. Raw IDAT reprocessing is available in Step 5 when complete Red/Grn pairs are local."
        )))
        geo_stage(list(
          step = "Step 5",
          title = if (identical(input$geo_matrix_source %||% "processed", "raw_sesame")) "Raw files ready for sesame" else "Ready to extract",
          message = if (identical(input$geo_matrix_source %||% "processed", "raw_sesame")) {
            "Download complete. Install sesame if needed, then run raw IDAT QC/reprocessing."
          } else {
            "Download complete. Extract compressed matrix files before building an analysis table."
          }
        ))
        geo_download_progress(list(percent = 100, file = "Download complete", detail = paste0(nrow(files), " file(s) available."), folder = cache_dir))
        session$sendCustomMessage("geoProgress", list(percent = 100, file = "Download complete", detail = paste0(nrow(files), " file(s) available.")))
        selected_after_download <- geo_download_selection(annotated_files, input$geo_matrix_source %||% "processed")
        if (!any(selected_after_download$NeedsDownload)) {
          shinyjs::disable("geo_fetch_files")
          if (any(annotated_files$LocalStatus == "downloaded" & annotated_files$Loadable & grepl("\\.gz$", annotated_files$File, ignore.case = TRUE))) {
            shinyjs::enable("geo_extract_files")
          }
        }
      }
    }, error = function(e) {
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Could not fetch GEO files: ", conditionMessage(e))))
      files <- ugplot_geo_list_candidate_files(accession, cache_dir)
      geo_files(files)
      geo_stage(list(step = "Step 4", title = "Download failed", message = conditionMessage(e)))
    })
  })

	  observeEvent(input$geo_extract_files, {
	    if (block_local_geo_step_when_remote("GEO matrix extraction")) return()
	    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      geo_stage(list(step = "Step 5", title = "Missing accession", message = "Enter and inspect a GEO accession before extracting."))
      return()
    }
    cache_dir <- ugplot_geo_cache_dir(accession)
    remote_files <- geo_remote_files()
    if (!is.data.frame(remote_files) || nrow(remote_files) == 0) {
      geo_stage(list(step = "Step 5", title = "No downloaded files", message = "Inspect and download GEO files before extracting."))
      return()
    }
    remote_files <- ugplot_geo_annotate_remote_files(remote_files, cache_dir)
    extract_files <- remote_files[
      remote_files$Loadable &
        remote_files$LocalStatus == "downloaded" &
        grepl("\\.gz$", remote_files$LocalPath, ignore.case = TRUE),
      ,
      drop = FALSE
    ]
    if (nrow(extract_files) == 0) {
      geo_stage(list(step = "Step 5", title = "Nothing to extract", message = "No downloaded compressed matrix is waiting for extraction."))
      return()
    }

    total_bytes <- sum(suppressWarnings(as.numeric(extract_files$LocalSizeBytes)), na.rm = TRUE)
    if (!is.finite(total_bytes) || total_bytes <= 0) {
      total_bytes <- sum(file.info(extract_files$LocalPath)$size, na.rm = TRUE)
    }
    completed_bytes <- 0
    geo_stage(list(step = "Step 5", title = "Extracting files", message = paste0("Extracting ", nrow(extract_files), " compressed matrix file(s).")))
    geo_download_progress(list(type = "extract", percent = 0, file = "Preparing extraction", detail = "Extraction can take time for GB-scale matrices.", folder = cache_dir))
    session$sendCustomMessage("geoProgress", list(percent = 0, file = "Preparing extraction", detail = "Extraction can take time for GB-scale matrices."))
    tryCatch({
      withProgress(message = paste0("Extracting GEO files for ", accession), value = 0, {
        for (file_i in seq_len(nrow(extract_files))) {
          source_path <- extract_files$LocalPath[[file_i]]
          source_size <- file.info(source_path)$size
          start_pct <- if (total_bytes > 0) round(100 * completed_bytes / total_bytes, 1) else round(100 * (file_i - 1) / nrow(extract_files), 1)
          last_sent_pct <- start_pct
          geo_download_progress(list(
            type = "extract",
            percent = start_pct,
            file = basename(source_path),
            detail = paste0("Extracting ", file_i, " of ", nrow(extract_files), " - ", ugplot_format_bytes(source_size)),
            folder = cache_dir
          ))
          session$sendCustomMessage("geoProgress", list(
            percent = start_pct,
            file = basename(source_path),
            detail = paste0("Extracting ", file_i, " of ", nrow(extract_files), " - ", ugplot_format_bytes(source_size))
          ))
          ugplot_geo_extract_gzip(source_path, progress_callback = function(read_bytes, total_file_bytes) {
            overall_pct <- if (total_bytes > 0) {
              round(100 * min(total_bytes, completed_bytes + read_bytes) / total_bytes, 1)
            } else if (is.finite(total_file_bytes) && total_file_bytes > 0) {
              round(100 * ((file_i - 1) + read_bytes / total_file_bytes) / nrow(extract_files), 1)
            } else {
              start_pct
            }
            if (overall_pct >= last_sent_pct + 1 || overall_pct >= 100) {
              last_sent_pct <<- overall_pct
              shiny::setProgress(value = overall_pct / 100, detail = paste0(basename(source_path), " - ", overall_pct, "%"))
              session$sendCustomMessage("geoProgress", list(
                percent = overall_pct,
                file = basename(source_path),
                detail = paste0("Read ", ugplot_format_bytes(read_bytes), " of ", ugplot_format_bytes(total_file_bytes))
              ))
            }
          })
          completed_bytes <- completed_bytes + source_size
          done_pct <- if (total_bytes > 0) round(100 * completed_bytes / total_bytes, 1) else round(100 * file_i / nrow(extract_files), 1)
          shiny::incProgress(if (total_bytes > 0) source_size / total_bytes else 1 / nrow(extract_files), detail = paste0("Finished ", basename(source_path)))
          session$sendCustomMessage("geoProgress", list(
            percent = done_pct,
            file = basename(source_path),
            detail = paste0("Extraction complete for ", file_i, " of ", nrow(extract_files), ". Compressed .gz removed.")
          ))
        }
      })
      remote_files <- ugplot_geo_annotate_remote_files(remote_files, cache_dir)
      geo_remote_files(remote_files)
      ugplot_geo_write_manifest(cache_dir, accession, remote_files)
      files <- ugplot_geo_list_candidate_files(accession, cache_dir)
      geo_files(files)
      shinyjs::disable("geo_extract_files")
      geo_stage(list(
        step = "Step 6",
        title = "Extraction complete",
        message = "Files were extracted. They are still large; the next step should subset/summarize them before loading into ugPlot."
      ))
      geo_download_progress(list(type = "extract", percent = 100, file = "Extraction complete", detail = paste0(nrow(extract_files), " file(s) extracted."), folder = cache_dir))
      session$sendCustomMessage("geoProgress", list(percent = 100, file = "Extraction complete", detail = paste0(nrow(extract_files), " file(s) extracted.")))
    }, error = function(e) {
      remote_files <- ugplot_geo_annotate_remote_files(remote_files, cache_dir)
      geo_remote_files(remote_files)
      files <- ugplot_geo_list_candidate_files(accession, cache_dir)
      geo_files(files)
      geo_stage(list(step = "Step 5", title = "Extraction failed", message = conditionMessage(e)))
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Could not extract GEO files: ", conditionMessage(e))))
    })
  })

  load_geo_annotation_cache_for_platform <- function(platform_info) {
    geo_stage(list(
      step = "Step 7",
      title = "Loading CpG annotation",
      message = paste0("Building or loading many-to-many CpG annotation for ", platform_info$platform, ".")
    ))
    geo_status(ugplot_geo_append_log(geo_status(), paste0("Loading CpG annotation cache for ", platform_info$platform, ".")))
    annotation_map <- ugplot_geo_build_annotation_cache(platform_info$platform)
    geo_cpg_annotation(annotation_map)
    geo_status(ugplot_geo_append_log(
      geo_status(),
      paste0(
        "CpG annotation ready: ", nrow(annotation_map), " CpG-gene/transcript links, ",
        length(unique(annotation_map$CpG)), " CpGs. Cache: ",
        ugplot_geo_annotation_cache_path(platform_info$platform, "rds")
      )
    ))
    geo_stage(list(
      step = "Step 7",
      title = "CpG annotation ready",
      message = paste0(
        "Loaded ", nrow(annotation_map), " many-to-many CpG-gene/transcript links for ",
        platform_info$platform, ". Spearman output will save annotated and grouped files."
      )
    ))
    if (is.data.frame(geo_spearman_raw_results()) && nrow(geo_spearman_raw_results()) > 0) {
      build_geo_transcript_candidates(update_stage = FALSE)
    }
    invisible(annotation_map)
  }

  build_geo_transcript_candidates <- function(update_stage = FALSE, progress_callback = NULL,
                                             build_groups = FALSE) {
    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      return(invisible(data.frame()))
    }
    source <- geo_matrix_source_value()
    cache_dir <- ugplot_geo_cache_dir(accession)
    target_column <- isolate(input$geo_target_column %||% "")
    metadata <- geo_sample_metadata()
    if ((!nzchar(target_column) || !target_column %in% names(metadata)) && is.data.frame(metadata) && nrow(metadata) > 0) {
      target_candidates <- ugplot_geo_target_candidates(metadata)
      target_column <- if ("age" %in% target_candidates) "age" else if (length(target_candidates) > 0) target_candidates[[1]] else ""
    }
    results <- geo_spearman_raw_results()
    if (!is.data.frame(results) || nrow(results) == 0) {
      spearman_path <- geo_spearman_cache_paths(cache_dir, target_column, source = source)$raw
      if (nzchar(target_column) && file.exists(spearman_path)) {
        results <- tryCatch(utils::read.csv(spearman_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
        geo_spearman_raw_results(results)
      }
    }
    if (!is.data.frame(results) || nrow(results) == 0) {
      if (isTRUE(update_stage)) {
        geo_stage(list(step = "Step 6", title = "Run Spearman first", message = "Run the CpG Spearman scan before building the transcript CpG table."))
        if (isTRUE(build_groups)) {
          update_geo_transcript_build_progress(
            phase = "blocked",
            message = "Run the CpG Spearman scan before building transcript datasets.",
            processed = 0L,
            total = 0L,
            compatible = 0L,
            excluded = 0L,
            current = "",
            cache = geo_analysis_cache_dir(cache_dir, source)
          )
        }
      }
      return(invisible(data.frame()))
    }
    results <- geo_filter_spearman_min_samples(results)
    if (!is.data.frame(results) || nrow(results) == 0) {
      if (isTRUE(update_stage)) {
        geo_stage(list(
          step = "Step 6",
          title = "No CpGs after sample filter",
          message = paste0("No CpG passed the minimum per-CpG sample filter of ", geo_spearman_min_samples_pct(), "%. Lower that filter or rerun Spearman after checking the matrix.")
        ))
        if (isTRUE(build_groups)) {
          update_geo_transcript_build_progress(
            phase = "no CpGs",
            message = paste0("No CpG passed the minimum per-CpG sample filter of ", geo_spearman_min_samples_pct(), "%. Lower that filter or rerun Spearman."),
            processed = 0L,
            total = 0L,
            compatible = 0L,
            excluded = 0L,
            current = "",
            cache = geo_analysis_cache_dir(cache_dir, source)
          )
        }
      }
      return(invisible(data.frame()))
    }

    annotation_map <- geo_cpg_annotation()
    if ((!is.data.frame(annotation_map) || nrow(annotation_map) == 0) && is.data.frame(metadata) && nrow(metadata) > 0) {
      annotation_map <- ugplot_geo_load_annotation_cache(ugplot_geo_detect_platform(metadata))
      if (is.data.frame(annotation_map) && nrow(annotation_map) > 0) {
        geo_cpg_annotation(annotation_map)
      }
    }
    if (!is.data.frame(annotation_map) || nrow(annotation_map) == 0) {
      if (isTRUE(update_stage)) {
        geo_stage(list(step = "Step 7", title = "Build annotation first", message = "Build/load the CpG annotation cache before building transcript candidates."))
        if (isTRUE(build_groups)) {
          update_geo_transcript_build_progress(
            phase = "blocked",
            message = "Build/load the CpG annotation cache before building transcript datasets.",
            processed = 0L,
            total = 0L,
            compatible = 0L,
            excluded = 0L,
            current = "",
            cache = geo_analysis_cache_dir(cache_dir, source)
          )
        }
      }
      return(invisible(data.frame()))
    }

    threshold <- suppressWarnings(as.numeric(isolate(input$geo_transcript_absrho_threshold %||% 0.8)))
    if (!is.finite(threshold)) {
      threshold <- 0.8
    }
    absrho <- suppressWarnings(as.numeric(results$AbsRho))
    result_cols <- intersect(c("CpG", "SpearmanRho", "PValue", "N", "AbsRho"), names(results))
    trigger_results <- unique(results[is.finite(absrho) & absrho >= threshold, result_cols, drop = FALSE])
    trigger_links <- merge(
      trigger_results,
      annotation_map[, intersect(c("CpG", "Gene", "Transcript"), names(annotation_map)), drop = FALSE],
      by = "CpG",
      all.x = FALSE,
      sort = FALSE
    )
    trigger_links <- trigger_links[
      !is.na(trigger_links$Transcript) &
        nzchar(as.character(trigger_links$Transcript)),
      ,
      drop = FALSE
    ]
    selected_transcripts <- unique(as.character(trigger_links$Transcript))
    selected_transcripts <- selected_transcripts[nzchar(selected_transcripts) & !is.na(selected_transcripts)]
    candidate_link_count <- if (length(selected_transcripts) > 0) {
      sum(!is.na(annotation_map$Transcript) & as.character(annotation_map$Transcript) %in% selected_transcripts)
    } else {
      0L
    }
    use_streaming_candidates <- candidate_link_count > 500000L || length(selected_transcripts) > 5000L
    candidates <- if (isTRUE(use_streaming_candidates)) {
      trigger_links <- trigger_links[order(as.character(trigger_links$Transcript), -suppressWarnings(as.numeric(trigger_links$AbsRho)), suppressWarnings(as.numeric(trigger_links$PValue))), , drop = FALSE]
      transcript_levels <- unique(as.character(trigger_links$Transcript))
      streaming_rows <- lapply(transcript_levels, function(transcript_id) {
        df <- trigger_links[as.character(trigger_links$Transcript) == transcript_id, , drop = FALSE]
        best <- df[1, , drop = FALSE]
        best$TriggerCpGs <- paste(unique(df$CpG), collapse = ";")
        best$TriggerGenes <- paste(unique(stats::na.omit(df$Gene)), collapse = ";")
        best$TriggerMaxAbsRho <- max(suppressWarnings(as.numeric(df$AbsRho)), na.rm = TRUE)
        best$TriggerBestCpG <- best$CpG[[1]]
        best$TriggerBestRho <- best$SpearmanRho[[1]]
        best$ThresholdAbsRho <- threshold
        best
      })
      streaming_candidates <- do.call(rbind, streaming_rows)
      rownames(streaming_candidates) <- NULL
      attr(streaming_candidates, "annotation_map") <- annotation_map
      attr(streaming_candidates, "raw_results") <- results
      attr(streaming_candidates, "threshold") <- threshold
      if (isTRUE(update_stage)) {
        geo_status(ugplot_geo_append_log(
          geo_status(),
          paste0(
            "Using streaming transcript candidate mode for ", format(length(selected_transcripts), big.mark = ","),
            " transcript(s) and about ", format(candidate_link_count, big.mark = ","),
            " CpG-transcript links."
          )
        ))
      }
      streaming_candidates
    } else {
      ugplot_geo_transcript_candidates(results, annotation_map, threshold)
    }
    geo_transcript_candidates(candidates)
    safe_threshold <- gsub("[^0-9]+", "_", format(threshold, trim = TRUE, scientific = FALSE))
    candidates_path <- file.path(geo_analysis_cache_dir(cache_dir, source), paste0("ugplot_geo_transcript_candidates_", geo_safe_cache_token(target_column), "_absrho_", safe_threshold, ".csv"))
    if (is.data.frame(candidates) && nrow(candidates) > 0) {
      if (!isTRUE(use_streaming_candidates)) {
        utils::write.csv(candidates, candidates_path, row.names = FALSE)
      }
      geo_status(ugplot_geo_append_log(
        geo_status(),
        paste0(
          "Transcript candidate table ready: ", nrow(candidates), " trigger CpG-transcript rows across ",
          length(unique(candidates$Transcript)), " transcript(s)",
          if (isTRUE(use_streaming_candidates)) {
            "; full candidate CpGs will be reconstructed per transcript without writing one giant candidate CSV."
          } else {
            paste0(". Saved to ", candidates_path, ".")
          }
        )
      ))
      if (isTRUE(update_stage)) {
        geo_stage(list(
          step = "Step 8",
          title = "Transcript CpG table ready",
          message = paste0(
            "Found ", length(unique(candidates$Transcript)), " transcript(s) with at least one CpG above |rho| >= ",
            threshold,
            if (isTRUE(use_streaming_candidates)) {
              ". Large candidate set will be processed transcript-by-transcript."
            } else {
              ". Saved expanded CpG table to disk."
            }
          )
        ))
      }
    } else if (isTRUE(update_stage)) {
      geo_status(ugplot_geo_append_log(geo_status(), paste0("No transcript candidates found for |rho| >= ", threshold, ".")))
      geo_stage(list(
        step = "Step 8",
        title = "No transcript candidates",
        message = paste0("No annotated CpG passed |rho| >= ", threshold, ". Lower the threshold or scan more CpGs.")
      ))
      if (isTRUE(build_groups)) {
        geo_transcript_groups(data.frame())
        geo_transcript_group_details(data.frame())
        update_geo_transcript_build_progress(
          phase = "no candidates",
          message = paste0("No annotated CpG passed |rho| >= ", threshold, ". Lower the transcript CpG threshold and run again."),
          processed = 0L,
          total = 0L,
          compatible = 0L,
          excluded = 0L,
          current = "",
          cache = candidates_path
        )
        return(invisible(candidates))
      }
    }

    min_samples_pct <- suppressWarnings(as.numeric(isolate(input$geo_transcript_min_samples %||% 80)))
    if (!is.finite(min_samples_pct)) {
      min_samples_pct <- 80
    }
    if (!isTRUE(build_groups)) {
      return(invisible(candidates))
    }
    group_paths <- geo_transcript_group_cache_paths(cache_dir, target_column, threshold, min_samples_pct, source = source)
    if (file.exists(group_paths$summary) && file.exists(group_paths$details)) {
      cached_summary <- tryCatch(utils::read.csv(group_paths$summary, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      cached_details <- tryCatch(utils::read.csv(group_paths$details, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      if (is.data.frame(cached_summary) && nrow(cached_summary) > 0 && is.data.frame(cached_details)) {
        if (all(c("PrincipalTranscript", "DatasetPath") %in% names(cached_summary))) {
          for (summary_i in seq_len(nrow(cached_summary))) {
            final_path <- geo_transcript_dataset_cache_path(cache_dir, cached_summary$PrincipalTranscript[[summary_i]], target_column, source = source)
            raw_path <- geo_transcript_raw_dataset_cache_path(cache_dir, cached_summary$PrincipalTranscript[[summary_i]], target_column, source = source)
            old_path <- as.character(cached_summary$DatasetPath[[summary_i]])
            if (file.exists(final_path) && !file.exists(raw_path)) {
              legacy_dataset <- tryCatch(utils::read.csv(final_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
              if (geo_transcript_dataset_has_missing(legacy_dataset, target_column)) {
                file.copy(final_path, raw_path, overwrite = TRUE)
              }
            }
            if (nzchar(old_path) && file.exists(old_path) && !identical(old_path, final_path)) {
              file.copy(old_path, final_path, overwrite = TRUE)
              cached_summary$DatasetPath[[summary_i]] <- final_path
            }
          }
          utils::write.csv(cached_summary, group_paths$summary, row.names = FALSE)
        }
        geo_transcript_groups(cached_summary)
        geo_transcript_group_details(cached_details)
        update_geo_transcript_build_progress(
          phase = "loaded from cache",
          message = paste0("Loaded cached transcript ML groups: ", nrow(cached_summary), " group(s)."),
          processed = nrow(cached_summary),
          total = nrow(cached_summary),
          compatible = nrow(cached_summary),
          excluded = 0L,
          current = "",
          cache = group_paths$summary
        )
        geo_status(ugplot_geo_append_log(geo_status(), paste0("Loaded cached transcript ML groups: ", nrow(cached_summary), " group(s).")))
        if (!is.null(progress_callback)) {
          progress_callback(1, paste0("Loaded cached transcript ML groups: ", nrow(cached_summary), " group(s)"))
        }
        return(invisible(candidates))
      }
    }
    if (is.data.frame(candidates) && nrow(candidates) > 0) {
      build_geo_transcript_groups(
        candidates, cache_dir, target_column, threshold, min_samples_pct, group_paths,
        update_stage = update_stage,
        progress_callback = progress_callback
      )
    } else {
      geo_transcript_groups(data.frame())
      geo_transcript_group_details(data.frame())
    }
    invisible(candidates)
  }

  load_geo_cached_state <- function(accession) {
    accession <- trimws(accession %||% "")
    if (!nzchar(accession)) {
      return(invisible(FALSE))
    }
    cache_dir <- ugplot_geo_cache_dir(accession)
    if (!dir.exists(cache_dir)) {
      return(invisible(FALSE))
    }

    metadata_path <- ugplot_geo_sample_metadata_path(cache_dir, "rds")
    if (file.exists(metadata_path)) {
      metadata <- tryCatch(readRDS(metadata_path), error = function(e) data.frame())
      if (is.data.frame(metadata) && nrow(metadata) > 0) {
        geo_sample_metadata(metadata)
        annotation_map <- ugplot_geo_load_annotation_cache(ugplot_geo_detect_platform(metadata))
        if (is.data.frame(annotation_map) && nrow(annotation_map) > 0) {
          geo_cpg_annotation(annotation_map)
        }
      }
    }

    manifest_path <- ugplot_geo_manifest_path(cache_dir)
    if (file.exists(manifest_path)) {
      manifest <- tryCatch(readRDS(manifest_path), error = function(e) NULL)
      if (is.list(manifest) && is.data.frame(manifest$files)) {
        geo_remote_files(ugplot_geo_annotate_remote_files(manifest$files, cache_dir))
      }
    }
    files <- ugplot_geo_list_candidate_files(accession, cache_dir)
    if (is.data.frame(files)) {
      geo_files(files)
    }
    qc_path <- ugplot_geo_sesame_qc_path(cache_dir)
    beta_path <- ugplot_geo_sesame_beta_path(cache_dir)
    if (file.exists(qc_path)) {
      qc_report <- tryCatch(utils::read.csv(qc_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      if (is.data.frame(qc_report) && nrow(qc_report) > 0) {
        geo_idat_qc_report(qc_report)
        geo_idat_qc_progress(list(
          phase = if (file.exists(beta_path)) "loaded from cache" else "qc report only",
          message = if (file.exists(beta_path)) "Loaded cached sesame beta matrix and QC report." else "Loaded cached sesame QC report, but beta matrix was not found.",
          processed = nrow(qc_report),
          total = nrow(qc_report),
          current = "",
          beta_path = if (file.exists(beta_path)) beta_path else "",
          qc_path = qc_path
        ))
      }
    }

    source <- geo_matrix_source_value()
    metadata <- geo_sample_metadata()
    target_column <- isolate(input$geo_target_column %||% "")
    if ((!nzchar(target_column) || !target_column %in% names(metadata)) && is.data.frame(metadata) && nrow(metadata) > 0) {
      candidates <- ugplot_geo_target_candidates(metadata)
      target_column <- if ("age" %in% candidates) "age" else if (length(candidates) > 0) candidates[[1]] else ""
    }
    analysis_dir <- geo_analysis_cache_dir(cache_dir, source, create = FALSE)
    spearman_paths <- if (dir.exists(analysis_dir)) {
      list.files(analysis_dir, pattern = "^ugplot_geo_spearman_.*\\.csv$", full.names = TRUE)
    } else {
      character(0)
    }
    if (identical(source, "processed") && length(spearman_paths) == 0) {
      spearman_paths <- list.files(cache_dir, pattern = "^ugplot_geo_spearman_.*\\.csv$", full.names = TRUE)
    }
    spearman_paths <- spearman_paths[!grepl("_annotated|_by_gene|_by_transcript", basename(spearman_paths))]
    if (!nzchar(target_column) && length(spearman_paths) > 0) {
      spearman_names <- sub("^ugplot_geo_spearman_(.*)\\.csv$", "\\1", basename(spearman_paths))
      target_column <- if ("age" %in% spearman_names) "age" else spearman_names[[1]]
    }
    spearman_path <- if (nzchar(target_column)) geo_spearman_cache_paths(cache_dir, target_column, source = source, create = FALSE)$raw else ""
    if (identical(source, "processed") && nzchar(target_column) && !file.exists(spearman_path)) {
      legacy_spearman_path <- file.path(cache_dir, paste0("ugplot_geo_spearman_", target_column, ".csv"))
      if (file.exists(legacy_spearman_path)) {
        spearman_path <- legacy_spearman_path
      }
    }
    if (nzchar(target_column) && file.exists(spearman_path)) {
      spearman_results <- tryCatch(utils::read.csv(spearman_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      if (is.data.frame(spearman_results) && nrow(spearman_results) > 0) {
        geo_spearman_raw_results(spearman_results)
        annotation_map <- geo_cpg_annotation()
        if (is.data.frame(annotation_map) && nrow(annotation_map) > 0) {
          geo_spearman_results(spearman_results)
        } else {
          geo_spearman_results(spearman_results)
        }
      }
    }
    candidate_paths <- if (dir.exists(analysis_dir)) {
      list.files(analysis_dir, pattern = "^ugplot_geo_transcript_candidates_.*\\.csv$", full.names = TRUE)
    } else {
      character(0)
    }
    if (identical(source, "processed") && length(candidate_paths) == 0) {
      candidate_paths <- list.files(cache_dir, pattern = "^ugplot_geo_transcript_candidates_.*\\.csv$", full.names = TRUE)
    }
    if (length(candidate_paths) > 0 && (!is.data.frame(geo_transcript_candidates()) || nrow(geo_transcript_candidates()) == 0)) {
      target_matches <- if (nzchar(target_column)) grepl(paste0("^ugplot_geo_transcript_candidates_", target_column, "_"), basename(candidate_paths)) else rep(FALSE, length(candidate_paths))
      preferred_paths <- candidate_paths[target_matches]
      candidate_path <- if (length(preferred_paths) > 0) preferred_paths[[which.max(file.info(preferred_paths)$mtime)]] else candidate_paths[[which.max(file.info(candidate_paths)$mtime)]]
      transcript_candidates <- tryCatch(utils::read.csv(candidate_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      if (is.data.frame(transcript_candidates) && nrow(transcript_candidates) > 0) {
        geo_transcript_candidates(transcript_candidates)
      }
    }
    group_summary_paths <- if (dir.exists(analysis_dir)) {
      list.files(analysis_dir, pattern = "^ugplot_geo_transcript_ml_groups_.*_summary\\.csv$", full.names = TRUE)
    } else {
      character(0)
    }
    if (identical(source, "processed") && length(group_summary_paths) == 0) {
      group_summary_paths <- list.files(cache_dir, pattern = "^ugplot_geo_transcript_ml_groups_.*_summary\\.csv$", full.names = TRUE)
    }
    if (length(group_summary_paths) > 0 && (!is.data.frame(geo_transcript_groups()) || nrow(geo_transcript_groups()) == 0)) {
      current_prefix <- if (nzchar(target_column)) {
        paste0("^ugplot_geo_transcript_ml_groups_", target_column, "_", geo_transcript_cache_version(), "_")
      } else {
        paste0("^ugplot_geo_transcript_ml_groups_.*_", geo_transcript_cache_version(), "_")
      }
      target_matches <- grepl(current_prefix, basename(group_summary_paths))
      preferred_paths <- group_summary_paths[target_matches]
      legacy_target_matches <- if (nzchar(target_column)) {
        grepl(paste0("^ugplot_geo_transcript_ml_groups_", target_column, "_"), basename(group_summary_paths))
      } else {
        rep(TRUE, length(group_summary_paths))
      }
      legacy_paths <- setdiff(group_summary_paths[legacy_target_matches], preferred_paths)
      if (length(preferred_paths) == 0 && length(legacy_paths) > 0) {
        geo_transcript_groups(data.frame())
        geo_transcript_group_details(data.frame())
        update_geo_transcript_build_progress(
          phase = "needs rebuild",
          message = paste0(
            "Transcript groups exist in an older cache format, but the GEO matrix reader was updated. ",
            "Click Build/continue transcript CSVs to rebuild clean transcript datasets."
          ),
          processed = 0L,
          total = 0L,
          compatible = 0L,
          excluded = 0L,
          current = "",
          cache = legacy_paths[[which.max(file.info(legacy_paths)$mtime)]]
        )
      }
      summary_path <- if (length(preferred_paths) > 0) preferred_paths[[which.max(file.info(preferred_paths)$mtime)]] else ""
      if (nzchar(summary_path)) {
      details_path <- sub("_summary\\.csv$", "_details.csv", summary_path)
      cached_summary <- tryCatch(utils::read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      cached_details <- if (file.exists(details_path)) {
        tryCatch(utils::read.csv(details_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      } else {
        data.frame()
      }
      if (is.data.frame(cached_summary) && nrow(cached_summary) > 0) {
        geo_transcript_groups(cached_summary)
        geo_transcript_group_details(cached_details)
        update_geo_transcript_build_progress(
          phase = "loaded from cache",
          message = paste0("Loaded transcript ML groups: ", nrow(cached_summary), " group(s)."),
          processed = nrow(cached_summary),
          total = nrow(cached_summary),
          compatible = nrow(cached_summary),
          excluded = 0L,
          current = "",
          cache = summary_path
        )
      }
      }
    }
    ml_pipeline_dir <- geo_transcript_ml_dir(cache_dir, source, geo_current_transcript_ml_run_key())
    ml_stability_summaries <- if (dir.exists(ml_pipeline_dir)) {
      list.files(ml_pipeline_dir, pattern = "^summary_by_.*[.]csv$", full.names = TRUE)
    } else {
      character(0)
    }
    ml_summary_candidates <- c(
      file.path(ml_pipeline_dir, "summary.csv"),
      ml_stability_summaries,
      file.path(ml_pipeline_dir, "screening_summary.csv")
    )
    ml_summary_existing <- ml_summary_candidates[file.exists(ml_summary_candidates)]
    ml_summary_path <- if (length(ml_summary_existing) > 0) {
      ml_summary_existing[[which.max(file.info(ml_summary_existing)$mtime)]]
    } else {
      ""
    }
    if (file.exists(ml_summary_path) || dir.exists(ml_pipeline_dir)) {
      ml_summary <- if (!nzchar(ml_summary_path) || identical(basename(ml_summary_path), "screening_summary.csv")) {
        geo_ml_load_screening_summary(ml_pipeline_dir)
      } else {
        tryCatch(utils::read.csv(ml_summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      }
      if (is.data.frame(ml_summary) && nrow(ml_summary) > 0) {
        ml_summary <- geo_ml_rank_summary(ml_summary)
        geo_transcript_ml_results(ml_summary)
        update_geo_transcript_ml_progress(
          phase = "loaded from cache",
          message = paste0("Loaded cached transcript ML results: ", nrow(ml_summary), " group(s)."),
          processed = nrow(ml_summary),
          total = nrow(ml_summary),
          current = "",
          cache = ml_summary_path
        )
      }
    } else {
      geo_transcript_ml_results(data.frame())
      update_geo_transcript_ml_progress(
        phase = "idle",
        message = paste0("No transcript ML result cache found for ", geo_matrix_source_label(source), "."),
        processed = 0L,
        total = 0L,
        current = "",
        cache = ml_pipeline_dir
      )
    }
    geo_stage(list(
      step = "Local cache",
      title = "Loaded GEO cache",
      message = paste0("Loaded local files and ", geo_matrix_source_label(source), " cached results for ", accession, " from ", cache_dir, ".")
    ))
    invisible(TRUE)
  }

	  session$onFlushed(function() {
	    accession <- isolate(trimws(input$geo_accession %||% ""))
	    if (nchar(accession) >= 6) {
	      load_geo_cached_state(accession)
	    }
	  }, once = TRUE)

	  observeEvent(input$geo_run_target, {
	    geo_run_target_state(input$geo_run_target %||% "local")
	  }, ignoreInit = TRUE)

	  geo_remote_mode_active <- function() {
	    identical(geo_run_target_state(), "remote")
	  }

	  block_local_geo_step_when_remote <- function(step_title = "GEO step") {
	    if (!geo_remote_mode_active()) {
	      return(FALSE)
	    }
	    geo_stage(list(
	      step = "Remote GEO",
	      title = "Remote pipeline selected",
	      message = paste0(step_title, " was not run locally. Start or refresh the remote GEO pipeline from the top GEO controls.")
	    ))
	    geo_remote_pipeline_status("Remote mode is active; local GEO step buttons are blocked.")
	    TRUE
	  }

	  observeEvent(input$geo_build_annotation, {
	    if (block_local_geo_step_when_remote("CpG annotation build")) return()
	    accession <- trimws(input$geo_accession %||% "")
    cache_dir <- if (nzchar(accession)) ugplot_geo_cache_dir(accession) else ""
    metadata <- geo_sample_metadata()
    if ((!is.data.frame(metadata) || nrow(metadata) == 0) && nzchar(cache_dir) && file.exists(ugplot_geo_sample_metadata_path(cache_dir, "rds"))) {
      metadata <- tryCatch(readRDS(ugplot_geo_sample_metadata_path(cache_dir, "rds")), error = function(e) data.frame())
      geo_sample_metadata(metadata)
    }
    if (!is.data.frame(metadata) || nrow(metadata) == 0) {
      geo_stage(list(step = "Step 7", title = "Missing sample metadata", message = "Fetch sample metadata before building CpG annotation."))
      return()
    }
    platform_id <- ugplot_geo_detect_platform(metadata)
    if (!nzchar(platform_id %||% "")) {
      geo_stage(list(step = "Step 7", title = "Missing platform", message = "Sample metadata does not include a usable platform_id."))
      return()
    }
    platform_info <- ugplot_geo_platform_annotation_package(platform_id)
    if (is.null(platform_info)) {
      geo_stage(list(step = "Step 7", title = "Unsupported platform", message = paste0("No built-in annotation mapping is configured for ", platform_id, ".")))
      return()
    }

    missing_packages <- ugplot_geo_missing_annotation_packages(platform_info)
    if (length(missing_packages) > 0) {
      geo_pending_annotation_platform(platform_info)
      geo_stage(list(
        step = "Step 7",
        title = "CpG annotation packages missing",
        message = paste0("ugPlot needs permission to install: ", paste(missing_packages, collapse = ", "), ".")
      ))
      showModal(modalDialog(
        title = "Install CpG annotation packages?",
        tags$p("To build the CpG-to-gene/transcript cache, ugPlot needs to install missing Bioconductor packages."),
        tags$p(tags$strong("Packages: "), paste(missing_packages, collapse = ", ")),
        tags$p("This can take several minutes, especially for minfi and Illumina annotation packages."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("geo_confirm_install_annotation", "Install packages and continue")
        ),
        easyClose = TRUE
      ))
      return()
    }

    tryCatch({
      load_geo_annotation_cache_for_platform(platform_info)
    }, error = function(e) {
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Could not build/load CpG annotation: ", conditionMessage(e))))
      geo_stage(list(
        step = "Step 7",
        title = "CpG annotation unavailable",
        message = conditionMessage(e)
      ))
    })
  })

	  observeEvent(input$geo_confirm_install_annotation, {
	    if (block_local_geo_step_when_remote("CpG annotation package install")) return()
	    platform_info <- geo_pending_annotation_platform()
    if (is.null(platform_info)) {
      removeModal()
      geo_stage(list(step = "Step 7", title = "No pending install", message = "No annotation platform is waiting for package installation."))
      return()
    }
    removeModal()
    missing_packages <- ugplot_geo_missing_annotation_packages(platform_info)
    if (length(missing_packages) == 0) {
      tryCatch(load_geo_annotation_cache_for_platform(platform_info), error = function(e) {
        geo_stage(list(step = "Step 7", title = "CpG annotation unavailable", message = conditionMessage(e)))
      })
      return()
    }

    geo_stage(list(
      step = "Step 7",
      title = "Installing CpG annotation packages",
      message = paste0("Installing: ", paste(missing_packages, collapse = ", "), ". This may take several minutes.")
    ))
    geo_status(ugplot_geo_append_log(geo_status(), paste0("Installing CpG annotation dependencies: ", paste(missing_packages, collapse = ", "), ".")))
    tryCatch({
      withProgress(message = "Installing CpG annotation packages", value = 0, {
        if (!requireNamespace("BiocManager", quietly = TRUE)) {
          shiny::setProgress(0.1, detail = "Installing BiocManager")
          utils::install.packages("BiocManager", dependencies = TRUE)
        }
        shiny::setProgress(0.35, detail = paste0("Installing ", paste(missing_packages, collapse = ", ")))
        BiocManager::install(missing_packages, ask = FALSE, update = FALSE)
        shiny::setProgress(0.8, detail = "Building annotation cache")
        load_geo_annotation_cache_for_platform(platform_info)
        shiny::setProgress(1, detail = "Annotation ready")
      })
      geo_pending_annotation_platform(NULL)
    }, error = function(e) {
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Could not install/build CpG annotation packages: ", conditionMessage(e))))
      geo_stage(list(
        step = "Step 7",
        title = "CpG annotation install failed",
        message = conditionMessage(e)
      ))
    })
  })

	  observeEvent(input$geo_run_spearman, {
	    if (block_local_geo_step_when_remote("CpG Spearman scan")) return()
	    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      geo_stage(list(step = "Step 6", title = "Missing accession", message = "Enter and inspect a GEO accession before running CpG correlation."))
      return()
    }
    metadata <- geo_sample_metadata()
    cache_dir <- ugplot_geo_cache_dir(accession)
    if ((!is.data.frame(metadata) || nrow(metadata) == 0) && file.exists(ugplot_geo_sample_metadata_path(cache_dir, "rds"))) {
      metadata <- tryCatch(readRDS(ugplot_geo_sample_metadata_path(cache_dir, "rds")), error = function(e) data.frame())
      geo_sample_metadata(metadata)
    }
    if (!is.data.frame(metadata) || nrow(metadata) == 0) {
      geo_stage(list(step = "Step 6", title = "Missing sample metadata", message = "Fetch sample metadata before running Spearman by CpG."))
      return()
    }
    target_column <- input$geo_target_column %||% ""
    if (!nzchar(target_column) || !target_column %in% names(metadata)) {
      geo_stage(list(step = "Step 6", title = "Select metadata field", message = "Choose a metadata field before running Spearman by CpG."))
      return()
    }
    source <- geo_matrix_source_value()
    matrix_files <- ugplot_geo_matrix_files(cache_dir, source = source)
    if (length(matrix_files) == 0) {
      geo_stage(list(step = "Step 6", title = "Missing matrix files", message = paste0("Prepare ", geo_matrix_source_label(source), " files before running CpG correlation.")))
      return()
    }

    max_cpgs <- suppressWarnings(as.integer(input$geo_spearman_max_cpgs %||% 50000))
    sample_map <- tryCatch(ugplot_geo_matrix_sample_map(matrix_files, metadata), error = function(e) data.frame())
    target <- suppressWarnings(as.numeric(as.character(metadata[[target_column]])))
    matched_numeric_samples <- if (is.data.frame(sample_map) && nrow(sample_map) > 0) {
      sum(!is.na(target[sample_map$MetadataRow]))
    } else {
      0L
    }
    min_samples_pct <- geo_spearman_min_samples_pct()
    min_matched_samples <- if (matched_numeric_samples > 0) ceiling(matched_numeric_samples * min_samples_pct / 100) else 3L
    min_matched_samples <- max(3L, min_matched_samples)
    geo_spearman_results(data.frame())
    geo_spearman_raw_results(data.frame())
    geo_transcript_candidates(data.frame())
    geo_status(ugplot_geo_append_log(
      geo_status(),
      paste0("Running ", geo_matrix_source_label(source), " Spearman scan for '", target_column, "' across ", length(matrix_files), " matrix file(s).")
    ))
    geo_stage(list(
      step = "Step 6",
      title = "Running CpG Spearman scan",
      message = paste0("Scanning ", if (max_cpgs > 0) max_cpgs else "all", " CpGs without loading the full matrix into memory.")
    ))
    tryCatch({
      scanned_last <- 0L
      withProgress(message = "Scanning CpGs", value = 0, {
        results <- ugplot_geo_spearman_scan(
          matrix_files = matrix_files,
          metadata = metadata,
          target_column = target_column,
          max_cpgs = max_cpgs,
          min_matched_samples = min_matched_samples,
          progress_callback = function(scanned) {
            scanned_last <<- scanned
            if (max_cpgs > 0) {
              shiny::setProgress(value = min(1, scanned / max_cpgs), detail = paste0(scanned, " CpGs scanned"))
            } else {
              shiny::setProgress(detail = paste0(scanned, " CpGs scanned"))
            }
          }
        )
      })
      scanned <- attr(results, "scanned_cpgs") %||% scanned_last
      matched_samples <- attr(results, "matched_samples") %||% NA_integer_
      min_used_samples <- attr(results, "min_matched_samples") %||% min_matched_samples
      spearman_paths <- geo_spearman_cache_paths(cache_dir, target_column, source = source)
      results_path <- spearman_paths$raw
      utils::write.csv(results, results_path, row.names = FALSE)
      geo_spearman_raw_results(results)

      annotation_map <- geo_cpg_annotation()
      if (!is.data.frame(annotation_map) || nrow(annotation_map) == 0) {
        platform_id <- ugplot_geo_detect_platform(metadata)
        annotation_map <- ugplot_geo_load_annotation_cache(platform_id)
        if (is.data.frame(annotation_map) && nrow(annotation_map) > 0) {
          geo_cpg_annotation(annotation_map)
        }
      }

      annotated_path <- ""
      transcript_path <- ""
      gene_path <- ""
      display_results <- results
      if (is.data.frame(annotation_map) && nrow(annotation_map) > 0) {
        annotated_results <- ugplot_geo_join_spearman_annotation(results, annotation_map)
        annotated_path <- spearman_paths$annotated
        utils::write.csv(annotated_results, annotated_path, row.names = FALSE)
        display_results <- annotated_results

        transcript_summary <- ugplot_geo_group_spearman_annotation(annotated_results, "Transcript")
        if (is.data.frame(transcript_summary) && nrow(transcript_summary) > 0) {
          transcript_path <- spearman_paths$by_transcript
          utils::write.csv(transcript_summary, transcript_path, row.names = FALSE)
        }
        gene_summary <- ugplot_geo_group_spearman_annotation(annotated_results, "Gene")
        if (is.data.frame(gene_summary) && nrow(gene_summary) > 0) {
          gene_path <- spearman_paths$by_gene
          utils::write.csv(gene_summary, gene_path, row.names = FALSE)
        }
      }

      geo_spearman_results(display_results)
      build_geo_transcript_candidates(update_stage = FALSE)
      saved_files <- c(results_path, annotated_path, transcript_path, gene_path)
      saved_files <- saved_files[nzchar(saved_files)]
      geo_status(ugplot_geo_append_log(
        geo_status(),
        paste0(
          "Spearman scan complete: ", scanned, " CpGs scanned, ", matched_samples,
          " matched samples, minimum per CpG ", min_used_samples,
          ". Saved files: ", paste(saved_files, collapse = "; "), "."
        )
      ))
      geo_stage(list(
        step = "Step 6",
        title = "CpG Spearman scan complete",
        message = paste0(
            "Saved all ", nrow(results), " raw CpG results for ", geo_matrix_source_label(source), " field '", target_column, "'.",
          if (is.data.frame(annotation_map) && nrow(annotation_map) > 0) {
            " Annotated many-to-many CpG-gene/transcript results and grouped summaries were also saved."
          } else {
            " Build the CpG annotation cache to also save gene/transcript grouped summaries."
          }
        )
      ))
    }, error = function(e) {
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Could not run CpG Spearman scan: ", conditionMessage(e))))
      geo_stage(list(step = "Step 6", title = "CpG scan failed", message = conditionMessage(e)))
    })
  })

	  observeEvent(input$geo_build_transcript_groups, {
	    if (block_local_geo_step_when_remote("Transcript dataset build")) return()
	    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      geo_stage(list(step = "Step 8", title = "Missing accession", message = "Enter and inspect a GEO accession first."))
      return()
    }
    geo_stage(list(
      step = "Step 8",
      title = "Building transcript CSVs",
      message = "Continuing from cached Spearman/annotation results. Existing transcript CSVs and partial progress will be reused."
    ))
    update_geo_transcript_build_progress(
      phase = "queued",
      message = "Starting transcript CSV/group build from cached Spearman and annotation results.",
      processed = 0L,
      total = 0L,
      compatible = 0L,
      excluded = 0L,
      current = "",
      cache = ugplot_geo_cache_dir(accession)
    )
    tryCatch(
      withProgress(message = "Building transcript CSVs", value = 0, {
        build_geo_transcript_candidates(
          update_stage = TRUE,
          build_groups = TRUE,
          progress_callback = function(value, detail) {
            shiny::setProgress(value = value, detail = detail)
          }
        )
      }),
      error = function(e) {
        geo_status(ugplot_geo_append_log(geo_status(), paste0("Could not build transcript CSVs/groups: ", conditionMessage(e))))
        geo_stage(list(step = "Step 8", title = "Transcript CSV build failed", message = conditionMessage(e)))
      }
    )
  })

	  observeEvent(input$geo_run_transcript_ml, {
	    if (block_local_geo_step_when_remote("Transcript ML screening")) return()
	    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      geo_stage(list(step = "Step 9", title = "Missing accession", message = "Enter and inspect a GEO accession first."))
      return()
    }
    source <- geo_matrix_source_value()
    cache_dir <- ugplot_geo_cache_dir(accession)
    groups <- geo_transcript_groups()
    if (!is.data.frame(groups) || nrow(groups) == 0) {
      geo_stage(list(step = "Step 9", title = "No transcript groups", message = "Build transcript ML datasets before running transcript ML."))
      return()
    }
    min_absrho <- geo_ml_safe_num(input$geo_ml_min_absrho %||% 0.7, 0.7, 0, 1)
    eligible <- groups[suppressWarnings(as.numeric(groups$TriggerMaxAbsRho)) >= min_absrho, , drop = FALSE]
    eligible_tiebreak <- if ("PrincipalTranscript" %in% names(eligible)) eligible$PrincipalTranscript else eligible$GroupID
    eligible <- eligible[order(-suppressWarnings(as.numeric(eligible$TriggerMaxAbsRho)), eligible_tiebreak), , drop = FALSE]
    run_key <- geo_current_transcript_ml_run_key()
    pipeline_dir <- geo_transcript_ml_dir(cache_dir, source, run_key)
    quick_models <- isTRUE(input$geo_ml_quick_models)
    summary_path <- file.path(pipeline_dir, "screening_summary.csv")
    existing_summary <- geo_ml_load_screening_summary(pipeline_dir)
    processed_groups <- if (is.data.frame(existing_summary) && "GroupID" %in% names(existing_summary)) {
      unique(as.character(existing_summary$GroupID))
    } else {
      character(0)
    }
    rank_limit <- suppressWarnings(as.integer(input$geo_ml_rank_limit %||% NA_integer_))
    if (is.finite(rank_limit) && rank_limit > 0 && nrow(eligible) > rank_limit) {
      eligible <- utils::head(eligible, rank_limit)
    }
    if (!is.data.frame(eligible) || nrow(eligible) == 0) {
      geo_stage(list(step = "Step 9", title = "No eligible transcripts", message = paste0("No transcript group has trigger |rho| >= ", min_absrho, ".")))
      return()
    }
    selected_processed <- intersect(processed_groups, as.character(eligible$GroupID))
    if (length(selected_processed) == nrow(eligible)) {
      current_summary <- geo_ml_rank_summary(existing_summary)
      geo_transcript_ml_results(current_summary)
      update_geo_transcript_ml_progress(
        phase = "already complete",
        message = paste0("The selected ", nrow(eligible), " transcript group(s) already have screening results. Nothing was rerun."),
        processed = nrow(eligible),
        total = nrow(eligible),
        current = "",
        cache = summary_path
      )
      geo_stage(list(
        step = "Step 9",
        title = "Transcript ML screening already complete",
        message = paste0("The selected ", nrow(eligible), " transcript group(s) already exist in ", summary_path, ".")
      ))
      return()
    }
    models <- unique(as.character(ml_available))
    models <- models[nzchar(models)]
    if (quick_models) {
      models <- geo_ml_quick_models(models)
    }
    if (length(models) == 0) {
      geo_stage(list(step = "Step 9", title = "No ML models selected", message = "Select at least one installed caret model for transcript ML."))
      return()
    }
    settings <- list(
      screen_seeds = as.integer(geo_ml_safe_num(input$geo_ml_screen_seeds %||% 3, 3, 1)),
      timeout = geo_ml_safe_num(input$geo_ml_timeout %||% 1200, 1200, 1)
    )
    update_geo_transcript_ml_progress(
      phase = "running",
      message = paste0(
        "Screening ", length(models), " model(s) for ", nrow(eligible),
        " eligible group(s) on ", geo_matrix_source_label(source), "."
      ),
      processed = length(intersect(processed_groups, eligible$GroupID)),
      total = nrow(eligible),
      current = "",
      cache = pipeline_dir
    )
    geo_stage(list(
      step = "Step 9",
      title = "Screening transcript ML models",
      message = paste0("Screening results are saved under ", pipeline_dir, " and can resume after interruption.")
    ))
    tryCatch({
      withProgress(message = "Screening transcript ML models", value = 0, {
        summaries <- existing_summary
        for (group_i in seq_len(nrow(eligible))) {
          group <- eligible[group_i, , drop = FALSE]
          group_id <- as.character(group$GroupID[[1]])
          if (group_id %in% processed_groups) {
            next
          }
          update_geo_transcript_ml_progress(
            phase = "running",
            message = paste0("Screening group ", group_i, " / ", nrow(eligible), ": ", group_id, "."),
            processed = length(intersect(processed_groups, eligible$GroupID)),
            total = nrow(eligible),
            current = paste0(group_id, " / ", group$PrincipalTranscript[[1]]),
            cache = geo_transcript_ml_group_dir(cache_dir, source, group_id, run_key)
          )
          shiny::setProgress(value = (group_i - 1) / nrow(eligible), detail = paste0("Group ", group_id))
          summary_row <- geo_ml_run_group_screen(
            group,
            source,
            models,
            settings,
            run_key = run_key,
            progress_callback = function(message) {
              update_geo_transcript_ml_progress(
                phase = "running",
                message = message,
                processed = length(intersect(processed_groups, eligible$GroupID)),
                total = nrow(eligible),
                current = paste0(group_id, " / ", group$PrincipalTranscript[[1]]),
                cache = geo_transcript_ml_group_dir(cache_dir, source, group_id, run_key)
              )
              shiny::setProgress(value = (group_i - 1) / nrow(eligible), detail = message)
            }
          )
          summaries <- if (is.data.frame(summaries) && nrow(summaries) > 0) {
            summaries <- summaries[as.character(summaries$GroupID) != group_id, , drop = FALSE]
            bind_summary_rows(list(summaries, summary_row))
          } else {
            summary_row
          }
          summaries <- geo_ml_rank_summary(summaries)
          utils::write.csv(summaries, summary_path, row.names = FALSE)
          geo_transcript_ml_results(summaries)
          processed_groups <- union(processed_groups, group_id)
          update_geo_transcript_ml_progress(
            processed = length(intersect(processed_groups, eligible$GroupID)),
            total = nrow(eligible),
            message = paste0("Finished ", group_id, ".")
          )
        }
      })
      final_summary <- if (file.exists(summary_path)) {
        tryCatch(utils::read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      } else {
        data.frame()
      }
      final_summary <- geo_ml_rank_summary(final_summary)
      if (is.data.frame(final_summary) && nrow(final_summary) > 0) {
        utils::write.csv(final_summary, summary_path, row.names = FALSE)
      }
      geo_transcript_ml_results(final_summary)
      update_geo_transcript_ml_progress(
        phase = "complete",
        message = paste0("Transcript ML model screening complete for ", nrow(final_summary), " group(s)."),
        processed = nrow(final_summary),
        total = nrow(eligible),
        current = "",
        cache = summary_path
      )
      geo_stage(list(step = "Step 9", title = "Transcript ML screening complete", message = paste0("Saved screening summary: ", summary_path, ". Continue with Step 10 for stability seeds.")))
    }, error = function(e) {
      update_geo_transcript_ml_progress(phase = "failed", message = conditionMessage(e), current = "")
      geo_stage(list(step = "Step 9", title = "Transcript ML screening failed", message = conditionMessage(e)))
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Transcript ML screening failed: ", conditionMessage(e))))
    })
  })

	  observeEvent(input$geo_run_transcript_ml_stability, {
	    if (block_local_geo_step_when_remote("Transcript ML stability")) return()
	    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      geo_stage(list(step = "Step 10", title = "Missing accession", message = "Enter and inspect a GEO accession first."))
      return()
    }
    source <- geo_matrix_source_value()
    cache_dir <- ugplot_geo_cache_dir(accession)
    groups <- geo_transcript_groups()
    if (!is.data.frame(groups) || nrow(groups) == 0) {
      geo_stage(list(step = "Step 10", title = "No transcript groups", message = "Build transcript ML datasets before running stability seeds."))
      return()
    }
    min_absrho <- geo_ml_safe_num(input$geo_ml_min_absrho %||% 0.7, 0.7, 0, 1)
    eligible <- groups[suppressWarnings(as.numeric(groups$TriggerMaxAbsRho)) >= min_absrho, , drop = FALSE]
    if (!is.data.frame(eligible) || nrow(eligible) == 0) {
      geo_stage(list(step = "Step 10", title = "No eligible transcripts", message = paste0("No transcript group has trigger |rho| >= ", min_absrho, ".")))
      return()
    }
    settings <- list(
      screen_seeds = as.integer(geo_ml_safe_num(input$geo_ml_screen_seeds %||% 3, 3, 1)),
      min_stability_seeds = as.integer(geo_ml_safe_num(input$geo_ml_min_stability_seeds %||% 30, 30, 2)),
      max_stability_seeds = as.integer(geo_ml_safe_num(input$geo_ml_max_stability_seeds %||% 4000, 4000, 2)),
      window = as.integer(geo_ml_safe_num(input$geo_ml_stability_window %||% 30, 30, 2)),
      tolerance = geo_ml_safe_num(input$geo_ml_stability_tolerance %||% 0.01, 0.01, 0),
      timeout = geo_ml_safe_num(input$geo_ml_timeout %||% 1200, 1200, 1)
    )
    settings$max_stability_seeds <- max(settings$max_stability_seeds, settings$min_stability_seeds)
    settings$window <- min(settings$window, settings$max_stability_seeds)
    run_key <- geo_current_transcript_ml_run_key()
    pipeline_dir <- geo_transcript_ml_dir(cache_dir, source, run_key)
    metadata <- geo_sample_metadata()
    stratum_column <- input$geo_ml_stability_group_column %||% ""
    strata <- if (nzchar(stratum_column)) {
      geo_ml_stability_strata(metadata, stratum_column)
    } else {
      data.frame(StratumColumn = "", StratumValue = "", StratumSamples = NA_integer_, SampleIDs = "", stringsAsFactors = FALSE)
    }
    if (!is.data.frame(strata) || nrow(strata) == 0) {
      geo_stage(list(step = "Step 10", title = "No usable class groups", message = "The selected class/group column has no usable sample groups."))
      return()
    }
    summary_path <- if (nzchar(stratum_column)) {
      file.path(pipeline_dir, paste0("summary_by_", geo_safe_cache_token(stratum_column), ".csv"))
    } else {
      file.path(pipeline_dir, "summary.csv")
    }
    existing_summary <- if (file.exists(summary_path)) {
      tryCatch(utils::read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    } else {
      data.frame()
    }
    processed_keys <- if (is.data.frame(existing_summary) && "GroupID" %in% names(existing_summary)) {
      existing_col <- if ("StratumColumn" %in% names(existing_summary)) existing_summary$StratumColumn else rep("", nrow(existing_summary))
      existing_value <- if ("StratumValue" %in% names(existing_summary)) existing_summary$StratumValue else rep("", nrow(existing_summary))
      unique(geo_ml_stability_task_key(existing_summary$GroupID, existing_col, existing_value))
    } else {
      character(0)
    }
    total_tasks <- nrow(eligible) * nrow(strata)
    update_geo_transcript_ml_progress(
      phase = "running",
      message = paste0(
        "Running stability seeds for ", nrow(eligible), " eligible group(s)",
        if (nzchar(stratum_column)) paste0(" across ", nrow(strata), " class(es) from ", stratum_column) else "",
        " on ", geo_matrix_source_label(source), "."
      ),
      processed = length(processed_keys),
      total = total_tasks,
      current = "",
      cache = pipeline_dir
    )
    geo_stage(list(step = "Step 10", title = "Running stability seeds", message = paste0("Final results are saved under ", pipeline_dir, " and can resume after interruption.")))
    tryCatch({
      withProgress(message = "Running stability seeds", value = 0, {
        last_progress_detail <- ""
        last_progress_time <- Sys.time() - 10
        last_runner_message <- "waiting for trainer update"
        set_stability_progress <- function(value, detail, force = FALSE) {
          now <- Sys.time()
          elapsed <- as.numeric(difftime(now, last_progress_time, units = "secs"))
          if (isTRUE(force) || (!identical(detail, last_progress_detail) && elapsed >= 2)) {
            shiny::setProgress(value = value, detail = detail)
            last_progress_detail <<- detail
            last_progress_time <<- now
          }
        }
        summaries <- existing_summary
        task_i <- 0L
        for (stratum_i in seq_len(nrow(strata))) {
          stratum <- strata[stratum_i, , drop = FALSE]
          stratum_label <- if (nzchar(stratum$StratumColumn[[1]])) paste0(stratum$StratumColumn[[1]], "=", stratum$StratumValue[[1]]) else "all samples"
          for (group_i in seq_len(nrow(eligible))) {
            task_i <- task_i + 1L
            group <- eligible[group_i, , drop = FALSE]
            group_id <- as.character(group$GroupID[[1]])
            task_key <- geo_ml_stability_task_key(group_id, stratum$StratumColumn[[1]], stratum$StratumValue[[1]])
            if (task_key %in% processed_keys) {
              set_stability_progress(
                value = task_i / total_tasks,
                detail = geo_ml_stability_progress_text(
                  paste0("Task ", task_i, " / ", total_tasks, ": ", group_id, " / ", stratum_label),
                  length(processed_keys),
                  total_tasks,
                  "already complete"
                ),
                force = TRUE
              )
              next
            }
            cache_path <- geo_transcript_ml_group_dir(cache_dir, source, group_id, run_key)
            if (nzchar(stratum$StratumColumn[[1]])) {
              cache_path <- file.path(cache_path, "stability_by", geo_safe_cache_token(stratum$StratumColumn[[1]]), geo_safe_cache_token(stratum$StratumValue[[1]]))
            }
            current_task <- paste0(group_id, " / ", group$PrincipalTranscript[[1]], " / ", stratum_label)
            task_detail <- paste0("Task ", task_i, " / ", total_tasks, ": ", current_task)
            update_geo_transcript_ml_progress(
              phase = "running",
              message = paste0("Running task ", task_i, " / ", total_tasks, "."),
              processed = length(processed_keys),
              total = total_tasks,
              current = current_task,
              detail = "Starting stability run.",
              stability = "",
              values = numeric(0),
              cache = cache_path
            )
            set_stability_progress(
              value = (task_i - 1) / total_tasks,
              detail = geo_ml_stability_progress_text(task_detail, length(processed_keys), total_tasks, "starting stability run"),
              force = TRUE
            )
            summary_row <- geo_ml_run_group_stability(
            group,
            source,
            settings,
            run_key = run_key,
            stratum = stratum,
            progress_callback = function(message, detail = NULL) {
              stability_text <- detail$stability %||% ""
              distribution_text <- detail$distribution %||% ""
              if (nzchar(message %||% "")) {
                last_runner_message <<- message
              }
              runner_message <- last_runner_message
              clean_detail <- geo_ml_stability_progress_text(
                task_detail,
                length(processed_keys),
                total_tasks,
                runner_message,
                stability_text,
                distribution_text
              )
              update_geo_transcript_ml_progress(
                phase = "running",
                message = paste0("Running task ", task_i, " / ", total_tasks, "."),
                  processed = length(processed_keys),
                  total = total_tasks,
                  current = current_task,
                  detail = message,
                  stability = detail$stability %||% NULL,
                  values = detail$values %||% NULL,
                  cache = cache_path
                )
                set_stability_progress(
                  value = min(0.99, (task_i - 0.25) / total_tasks),
                  detail = clean_detail,
                  force = identical(detail$source %||% "", "partial")
                )
              }
            )
            summaries <- if (is.data.frame(summaries) && nrow(summaries) > 0) {
              existing_col <- if ("StratumColumn" %in% names(summaries)) summaries$StratumColumn else rep("", nrow(summaries))
              existing_value <- if ("StratumValue" %in% names(summaries)) summaries$StratumValue else rep("", nrow(summaries))
              keep <- geo_ml_stability_task_key(summaries$GroupID, existing_col, existing_value) != task_key
              summaries <- summaries[keep, , drop = FALSE]
              bind_summary_rows(list(summaries, summary_row))
            } else {
              summary_row
            }
            summaries <- geo_ml_rank_summary(summaries)
            utils::write.csv(summaries, summary_path, row.names = FALSE)
            geo_transcript_ml_results(summaries)
            processed_keys <- union(processed_keys, task_key)
            update_geo_transcript_ml_progress(
              processed = length(processed_keys),
              total = total_tasks,
              message = paste0("Finished stability seeds for ", group_id, " / ", stratum_label, "."),
              detail = "Task complete."
            )
            set_stability_progress(
              value = task_i / total_tasks,
              detail = geo_ml_stability_progress_text(task_detail, length(processed_keys), total_tasks, "task complete"),
              force = TRUE
            )
          }
        }
      })
      final_summary <- if (file.exists(summary_path)) {
        tryCatch(utils::read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      } else {
        data.frame()
      }
      final_summary <- geo_ml_rank_summary(final_summary)
      if (is.data.frame(final_summary) && nrow(final_summary) > 0) {
        utils::write.csv(final_summary, summary_path, row.names = FALSE)
      }
      geo_transcript_ml_results(final_summary)
      update_geo_transcript_ml_progress(
        phase = "complete",
        message = paste0("Transcript ML stability complete for ", nrow(final_summary), " result row(s)."),
        processed = nrow(final_summary),
        total = total_tasks,
        current = "",
        cache = summary_path,
        active = FALSE
      )
      geo_stage(list(step = "Step 10", title = "Transcript ML stability complete", message = paste0("Saved final summary: ", summary_path)))
    }, error = function(e) {
      update_geo_transcript_ml_progress(phase = "failed", message = conditionMessage(e), current = "", active = FALSE)
      geo_stage(list(step = "Step 10", title = "Transcript ML stability failed", message = conditionMessage(e)))
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Transcript ML stability failed: ", conditionMessage(e))))
    })
  })

	  observeEvent(input$geo_install_sesame, {
	    if (block_local_geo_step_when_remote("Sesame installation")) return()
	    if (requireNamespace("sesame", quietly = TRUE)) {
      geo_stage(list(step = "Step 5", title = "Sesame already installed", message = "Package 'sesame' is already available."))
      return()
    }
    geo_stage(list(
      step = "Step 5",
      title = "Installing sesame",
      message = "Installing Bioconductor package 'sesame'. This can take several minutes."
    ))
    geo_idat_qc_progress(list(
      phase = "installing",
      message = "Installing sesame and dependencies.",
      processed = 0L,
      total = 1L,
      current = "sesame",
      beta_path = "",
      qc_path = ""
    ))
    geo_status(ugplot_geo_append_log(geo_status(), "Installing sesame for raw IDAT reprocessing."))
    tryCatch({
      withProgress(message = "Installing sesame", value = 0, {
        if (!requireNamespace("BiocManager", quietly = TRUE)) {
          shiny::setProgress(0.2, detail = "Installing BiocManager")
          utils::install.packages("BiocManager", dependencies = TRUE)
        }
        shiny::setProgress(0.45, detail = "Installing sesame")
        BiocManager::install("sesame", ask = FALSE, update = FALSE)
        shiny::setProgress(1, detail = "sesame installed")
      })
      geo_idat_qc_progress(list(
        phase = "idle",
        message = "Sesame installed. Raw IDAT QC can now run when Red/Grn pairs are local.",
        processed = 0L,
        total = 0L,
        current = "",
        beta_path = "",
        qc_path = ""
      ))
      geo_stage(list(
        step = "Step 5",
        title = "Sesame installed",
        message = "Package 'sesame' is available. Download raw IDAT archives/files, then run sesame IDAT QC."
      ))
      geo_status(ugplot_geo_append_log(geo_status(), "Sesame installation complete."))
    }, error = function(e) {
      geo_idat_qc_progress(list(
        phase = "install failed",
        message = conditionMessage(e),
        processed = 0L,
        total = 1L,
        current = "sesame",
        beta_path = "",
        qc_path = ""
      ))
      geo_stage(list(step = "Step 5", title = "Sesame install failed", message = conditionMessage(e)))
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Sesame installation failed: ", conditionMessage(e))))
    })
  })

	  observeEvent(input$geo_run_sesame_idat, {
	    if (block_local_geo_step_when_remote("Sesame IDAT reprocessing")) return()
	    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      geo_stage(list(step = "Step 5", title = "Missing accession", message = "Enter and inspect a GEO accession before reprocessing raw IDATs."))
      return()
    }
    cache_dir <- ugplot_geo_cache_dir(accession)
    if (!dir.exists(cache_dir)) {
      geo_stage(list(step = "Step 5", title = "No GEO cache", message = "Download raw IDAT files or archives before running sesame."))
      return()
    }
    if (!requireNamespace("sesame", quietly = TRUE)) {
      geo_stage(list(
        step = "Step 5",
        title = "Sesame is not installed",
        message = "Install the Bioconductor package 'sesame' before raw Red/Grn IDAT reprocessing."
      ))
      geo_idat_qc_progress(list(
        phase = "blocked",
        message = "Package 'sesame' is not installed.",
        processed = 0L,
        total = 0L,
        current = "",
        beta_path = "",
        qc_path = ""
      ))
      return()
    }
    detection_p <- suppressWarnings(as.numeric(input$geo_idat_detection_p %||% 0.05))
    max_failed <- suppressWarnings(as.numeric(input$geo_idat_max_failed_fraction %||% 0.05))
    prep_code <- trimws(input$geo_idat_sesame_prep %||% "QCDPB")
    if (!nzchar(prep_code)) {
      prep_code <- "QCDPB"
    }
    geo_stage(list(
      step = "Step 5",
      title = "Running sesame IDAT QC",
      message = paste0("Reprocessing Red/Grn IDAT pairs with prep ", prep_code, ", detection p <= ", detection_p, ".")
    ))
    geo_status(ugplot_geo_append_log(
      geo_status(),
      paste0("Starting sesame IDAT reprocessing for ", accession, " with prep ", prep_code, ".")
    ))
    geo_idat_qc_progress(list(
      phase = "running",
      message = "Preparing raw archives and IDAT pairs.",
      processed = 0L,
      total = 0L,
      current = "",
      beta_path = ugplot_geo_sesame_beta_path(cache_dir),
      qc_path = ugplot_geo_sesame_qc_path(cache_dir)
    ))
    tryCatch({
      result <- withProgress(message = "Reprocessing raw IDATs with sesame", value = 0, {
        ugplot_geo_reprocess_idats_sesame(
          cache_dir = cache_dir,
          detection_p = detection_p,
          max_failed_probe_fraction = max_failed,
          prep = prep_code,
          progress_callback = function(done, total, detail) {
            value <- if (is.finite(total) && total > 0) min(1, max(0, done / total)) else 0
            shiny::setProgress(value = value, detail = detail)
            geo_idat_qc_progress(list(
              phase = "running",
              message = detail,
              processed = floor(done),
              total = total,
              current = detail,
              beta_path = ugplot_geo_sesame_beta_path(cache_dir),
              qc_path = ugplot_geo_sesame_qc_path(cache_dir)
            ))
          }
        )
      })
      qc_report <- result$qc
      geo_idat_qc_report(qc_report)
      files <- ugplot_geo_list_candidate_files(accession, cache_dir)
      geo_files(files)
      geo_idat_qc_progress(list(
        phase = "complete",
        message = paste0("Sesame reprocessing complete. Passed samples: ", sum(as.logical(qc_report$PassedQC), na.rm = TRUE), " / ", nrow(qc_report), "."),
        processed = nrow(qc_report),
        total = nrow(qc_report),
        current = "",
        beta_path = result$beta_path,
        qc_path = result$qc_path
      ))
      geo_stage(list(
        step = "Step 5",
        title = "Sesame IDAT QC complete",
        message = paste0("Saved beta matrix: ", result$beta_path, ". QC report: ", result$qc_path, ". Re-run Spearman to use the reprocessed matrix.")
      ))
      geo_status(ugplot_geo_append_log(
        geo_status(),
        paste0("Sesame IDAT reprocessing complete. Beta matrix: ", result$beta_path, "; QC: ", result$qc_path, ".")
      ))
    }, error = function(e) {
      qc_path <- ugplot_geo_sesame_qc_path(cache_dir)
      qc_n <- 0L
      if (file.exists(qc_path)) {
        qc_report <- tryCatch(utils::read.csv(qc_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(err) data.frame())
        if (is.data.frame(qc_report) && nrow(qc_report) > 0) {
          geo_idat_qc_report(qc_report)
          qc_n <- nrow(qc_report)
        }
      }
      geo_idat_qc_progress(list(
        phase = "failed",
        message = conditionMessage(e),
        processed = qc_n,
        total = qc_n,
        current = "",
        beta_path = if (file.exists(ugplot_geo_sesame_beta_path(cache_dir))) ugplot_geo_sesame_beta_path(cache_dir) else "",
        qc_path = if (file.exists(qc_path)) qc_path else ""
      ))
      geo_stage(list(step = "Step 5", title = "Sesame IDAT QC failed", message = conditionMessage(e)))
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Sesame IDAT reprocessing failed: ", conditionMessage(e))))
    })
  })

  observeEvent(input$geo_transcript_absrho_threshold, {
    remote_result <- remote_job_preview_result()
    if (geo_remote_mode_active() &&
        is.list(remote_result) &&
        identical(remote_result$kind %||% "", "geo_pipeline")) {
      remote_threshold <- suppressWarnings(as.numeric(remote_result$settings$transcript_absrho_threshold %||% NA_real_))
      current_threshold <- suppressWarnings(as.numeric(input$geo_transcript_absrho_threshold %||% NA_real_))
      if (is.finite(remote_threshold) && is.finite(current_threshold) &&
          isTRUE(all.equal(remote_threshold, current_threshold, tolerance = 1e-8))) {
        if ((!is.data.frame(geo_transcript_groups()) || nrow(geo_transcript_groups()) == 0) &&
            is.data.frame(remote_result$tables$transcript_groups)) {
          geo_transcript_groups(remote_result$tables$transcript_groups)
        }
        if ((!is.data.frame(geo_transcript_group_details()) || nrow(geo_transcript_group_details()) == 0) &&
            is.data.frame(remote_result$tables$transcript_group_details)) {
          geo_transcript_group_details(remote_result$tables$transcript_group_details)
        }
        return()
      }
    }
    if (is.data.frame(geo_spearman_raw_results()) && nrow(geo_spearman_raw_results()) > 0) {
      geo_transcript_candidates(data.frame())
      geo_transcript_groups(data.frame())
      geo_transcript_group_details(data.frame())
      update_geo_transcript_build_progress(
        phase = "threshold changed",
        message = "Review the positive/negative Spearman totals, then click Build/continue transcript CSVs.",
        processed = 0L,
        total = 0L,
        compatible = 0L,
        excluded = 0L,
        current = "",
        cache = ""
      )
    }
  }, ignoreInit = TRUE)

  load_geo_transcript_dataset <- function(selected_transcript) {
    candidates <- geo_transcript_candidates()
    if (!is.data.frame(candidates) || nrow(candidates) == 0) {
      geo_stage(list(step = "Step 6", title = "No transcript table", message = "Build transcript candidates by running Spearman with annotation first."))
      return(invisible(FALSE))
    }
    accession <- trimws(input$geo_accession %||% "GEO")
    target_column <- input$geo_target_column %||% ""
    if (!nzchar(selected_transcript) || !selected_transcript %in% as.character(candidates$Transcript)) {
      geo_stage(list(step = "Step 6", title = "Select a transcript", message = "Click Load beside a transcript before loading a transcript CpG dataset."))
      return(invisible(FALSE))
    }
    metadata <- geo_sample_metadata()
    if (!is.data.frame(metadata) || nrow(metadata) == 0 || !target_column %in% names(metadata)) {
      geo_stage(list(step = "Step 6", title = "Missing metadata field", message = "Fetch sample metadata and choose a valid metadata field first."))
      return(invisible(FALSE))
    }
    cache_dir <- ugplot_geo_cache_dir(accession)
    source <- geo_matrix_source_value()
    matrix_files <- ugplot_geo_matrix_files(cache_dir, source = source)
    if (length(matrix_files) == 0) {
      geo_stage(list(step = "Step 6", title = "No matrix files", message = paste0("Prepare ", geo_matrix_source_label(source), " files before building a transcript dataset.")))
      return(invisible(FALSE))
    }
    transcript_rows <- candidates[as.character(candidates$Transcript) == selected_transcript, , drop = FALSE]
    transcript_cpgs <- unique(as.character(stats::na.omit(transcript_rows$CpG)))
    geo_stage(list(
      step = "Step 6",
      title = "Building transcript dataset",
      message = paste0("Reading ", length(transcript_cpgs), " CpG(s) for transcript ", selected_transcript, " from extracted GEO matrices.")
    ))
    transcript_dataset <- tryCatch(
      ugplot_geo_transcript_dataset(
        matrix_files = matrix_files,
        metadata = metadata,
        target_column = target_column,
        cpgs = transcript_cpgs,
        progress_callback = function(scanned, found, total) {
          geo_stage(list(
            step = "Step 6",
            title = "Building transcript dataset",
            message = paste0("Scanned ", scanned, " matrix rows; found ", found, " of ", total, " transcript CpG(s).")
          ))
        }
      ),
      error = function(e) {
        geo_stage(list(step = "Step 6", title = "Transcript dataset failed", message = conditionMessage(e)))
        data.frame()
      }
    )
    if (!is.data.frame(transcript_dataset) || nrow(transcript_dataset) == 0) {
      return(invisible(FALSE))
    }
    dataset_path <- geo_transcript_dataset_cache_path(cache_dir, selected_transcript, target_column, source = source)
    utils::write.csv(transcript_dataset, dataset_path, row.names = FALSE)
    dff <<- as.data.frame(transcript_dataset, stringsAsFactors = FALSE, check.names = FALSE)
    original_dataset_filename(paste0(accession, "_", selected_transcript, "_", target_column, "_dataset"))
    geo_preview_data(utils::head(dff, 100))
    load_dataset_into_table(session)
    refresh_counter(refresh_counter() + 1)
    update_scramble_selector()
    updateTabsetPanel(session, "tabs", selected = "TABLE")
    geo_stage(list(
      step = "TABLE",
      title = "Transcript dataset loaded",
      message = paste0(
        "Loaded ", selected_transcript, " into TABLE as ", nrow(dff), " sample rows x ",
        ncol(dff), " columns (sample_id, target, CpGs). Saved to ", dataset_path, "."
      )
    ))
    geo_status(ugplot_geo_append_log(geo_status(), paste0("Loaded transcript dataset into TABLE: ", selected_transcript, "; ", nrow(dff), " rows x ", ncol(dff), " columns. Saved to ", dataset_path, ".")))
    invisible(TRUE)
  }

  observeEvent(input$geo_load_transcript_from_row, {
    load_geo_transcript_dataset(input$geo_load_transcript_from_row %||% "")
  })

  load_geo_transcript_group_dataset <- function(selected_group) {
    groups <- geo_transcript_groups()
    if (!is.data.frame(groups) || nrow(groups) == 0) {
      geo_stage(list(step = "Step 6", title = "No transcript groups", message = "Build transcript ML groups before loading a dataset."))
      return(invisible(FALSE))
    }
    group <- groups[as.character(groups$GroupID) == as.character(selected_group), , drop = FALSE]
    if (nrow(group) == 0) {
      geo_stage(list(step = "Step 6", title = "Select a transcript group", message = "Click Load beside a transcript group."))
      return(invisible(FALSE))
    }
    dataset_path <- as.character(group$DatasetPath[[1]] %||% "")
    group_payload <- as.list(group)
    if (!(!is.na(dataset_path) && nzchar(dataset_path) && file.exists(dataset_path)) && geo_remote_mode_active()) {
      group_payload$dataset <- geo_transcript_group_dataset_remote(group$GroupID[[1]])
    }
    dataset_info <- tryCatch(
      geo_ml_group_dataset(group_payload, keep_sample_id = TRUE),
      error = function(e) list(error = conditionMessage(e), dataset = data.frame(), dataset_path = dataset_path, sample_count = 0L)
    )
    group_dataset <- dataset_info$dataset
    if (!is.data.frame(group_dataset) || nrow(group_dataset) == 0) {
      if (geo_remote_mode_active() && !is.na(dataset_path) && nzchar(dataset_path)) {
        geo_stage(list(
          step = "Step 6",
          title = "Remote grouped dataset unavailable",
          message = paste0(
            "This result was loaded from a remote job that did not include the complete-case TABLE dataset for ",
            group$PrincipalTranscript[[1]],
            ". Update ugPlotServer and rerun/resume the GEO pipeline so the remote payload includes transcript_group_datasets. Remote path: ",
            dataset_path
          )
        ))
        return(invisible(FALSE))
      }
      geo_stage(list(step = "Step 6", title = "Missing grouped dataset", message = "The cached complete-case CSV for this transcript group was not found. Re-run the transcript group build."))
      return(invisible(FALSE))
    }
    accession <- trimws(input$geo_accession %||% "GEO")
    target_column <- input$geo_target_column %||% ""
    dff <<- as.data.frame(group_dataset, stringsAsFactors = FALSE, check.names = FALSE)
    original_dataset_filename(paste0(accession, "_", group$PrincipalTranscript[[1]], "_", target_column, "_group_dataset"))
    geo_preview_data(utils::head(dff, 100))
    load_dataset_into_table(session)
    refresh_counter(refresh_counter() + 1)
    update_scramble_selector()
    updateTabsetPanel(session, "tabs", selected = "TABLE")
    geo_stage(list(
      step = "TABLE",
      title = "Transcript group dataset loaded",
      message = paste0(
        "Loaded ", group$PrincipalTranscript[[1]], " group into TABLE as ",
        nrow(dff), " sample rows x ", ncol(dff), " columns. Compatible transcripts: ",
        group$TranscriptCount[[1]], "."
      )
    ))
    source_note <- if (!is.na(dataset_path) && nzchar(dataset_path) && file.exists(dataset_path)) paste0("CSV: ", dataset_path) else "remote result payload"
    geo_status(ugplot_geo_append_log(geo_status(), paste0("Loaded transcript group dataset: ", group$PrincipalTranscript[[1]], "; ", nrow(dff), " rows x ", ncol(dff), " columns. Source: ", source_note)))
    invisible(TRUE)
  }

  observeEvent(input$geo_load_transcript_group_from_row, {
    load_geo_transcript_group_dataset(input$geo_load_transcript_group_from_row %||% "")
  })

  observeEvent(input$geo_load_selected_file, {
    files <- geo_files()
    if (!is.data.frame(files) || nrow(files) == 0) {
      geo_status("Fetch GEO files before loading.")
      geo_stage(list(step = "Step 6", title = "No downloaded files", message = "Download GEO files before loading a matrix into ugPlot."))
      return()
    }
    loadable <- files[files$Loadable, , drop = FALSE]
    selected_idx <- suppressWarnings(as.integer(input$geo_selected_file %||% NA_integer_))
    if (!is.finite(selected_idx) || selected_idx < 1 || selected_idx > nrow(loadable)) {
      geo_status("Please select a valid processed table.")
      geo_stage(list(step = "Step 6", title = "Select a table", message = "Choose one downloaded processed table before loading."))
      return()
    }
    selected_path <- loadable$Path[[selected_idx]]
    selected_size_mb <- suppressWarnings(as.numeric(file.info(selected_path)$size / 1024^2))
    if (is.finite(selected_size_mb) && selected_size_mb > 500) {
      geo_stage(list(
        step = "Step 6",
        title = "Matrix too large to load directly",
        message = paste0(
          basename(selected_path), " is ", ugplot_format_bytes(file.info(selected_path)$size),
          ". This should be preprocessed/subset before loading into ugPlot."
        )
      ))
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Blocked direct load of large GEO matrix: ", selected_path)))
      return()
    }
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Loading ", basename(selected_path), "...")))
      geo_stage(list(step = "Step 6", title = "Loading table", message = paste0("Reading ", basename(selected_path), " into ugPlot.")))
    tryCatch({
      data <- ugplot_read_geo_table(selected_path, isTRUE(input$geo_use_first_column_names))
      if (identical(input$geo_loaded_orientation, "cpgs_rows")) {
        data <- as.data.frame(t(as.matrix(data)), stringsAsFactors = FALSE, check.names = FALSE)
      }
      original_dataset_filename(paste0(trimws(input$geo_accession %||% "GEO"), "_", basename(selected_path)))
      dff <<- data
      geo_preview_data(data)
      load_dataset_into_table(session)
      refresh_counter(refresh_counter() + 1)
      update_scramble_selector()
      geo_status(ugplot_geo_append_log(geo_status(), paste0(
        "Loaded ", basename(selected_path), ": ",
        nrow(data), " rows x ", ncol(data), " columns. ",
        "Use TABLE to select columns/rows and add phenotype columns before ML."
      )))
      geo_stage(list(
        step = "Done",
        title = "GEO matrix loaded",
        message = paste0("Loaded ", nrow(data), " rows x ", ncol(data), " columns. Continue in TABLE to select features and add phenotype columns.")
      ))
    }, error = function(e) {
      geo_status(ugplot_geo_append_log(geo_status(), paste0("Could not load selected GEO table: ", conditionMessage(e))))
      geo_stage(list(step = "Step 6", title = "Load failed", message = conditionMessage(e)))
    })
  })

  load_uploaded_dataset_with_sequential_row_names <- function(upload_info) {
    data <- read.table(upload_info$filepath, header = TRUE, sep = upload_info$separator, row.names = NULL,
      dec = ".", stringsAsFactors = FALSE, strip.white = TRUE, skip = upload_info$skipline)
    rownames(data) <- seq_len(nrow(data))
    finish_uploaded_dataset_load(data)
  }

  output$downloadData <- downloadHandler(
    filename = function() {
      file_name <- basename(original_dataset_filename())
      if (is.null(file_name) || !nzchar(file_name)) {
        file_name <- paste0("data-", Sys.Date(), ".csv")
      }
      file_name
    },
    content = function(file) {
      data_to_download <- changed_table[input$row_checkbox_group, input$column_checkbox_group]
      write.csv(data_to_download, file, row.names = TRUE)
    }
  )

  output$downloadBestModel <- downloadHandler(
    filename = function() {
      base_name <- tools::file_path_sans_ext(basename(original_dataset_filename()))
      if (is.null(base_name) || !nzchar(base_name)) {
        base_name <- "ugplot_best_model"
      }
      paste0(base_name, ".rds")
    },
    content = function(file) {
      saveRDS(list(
        model = best_model_object(),
        preprocess_meta = best_model_preprocess(),
        ugplot_bundle_version = 1,
        saved_at = as.character(Sys.time())
      ), file = file)
    }
  )

  output$downloadCalculatedMLTable <- downloadHandler(
    filename = function() {
      base_name <- tools::file_path_sans_ext(basename(original_dataset_filename()))
      if (is.null(base_name) || !nzchar(base_name)) {
        base_name <- "ugplot"
      }
      paste0(base_name, ".csv")
    },
    content = function(file) {
      table_to_download <- ml_table_results()
      req(is.data.frame(table_to_download), nrow(table_to_download) > 0)
      table_to_download <- cbind(
        id = seq_len(nrow(table_to_download)),
        table_to_download
      )
      utils::write.csv(table_to_download, file, row.names = FALSE)
    }
  )

  output$downloadModelUI <- renderUI({
    buttons <- list()
    if (!is.null(best_model_object())) {
      buttons <- c(buttons, list(downloadButton("downloadBestModel", "Download best model")))
    }
    table_to_download <- ml_table_results()
    if (is.data.frame(table_to_download) && nrow(table_to_download) > 0) {
      buttons <- c(buttons, list(downloadButton("downloadCalculatedMLTable", "Download calculated table (CSV)")))
    }
    if (length(buttons) > 0) {
      do.call(
        tags$div,
        c(list(style = "display: flex; gap: 8px; align-items: center; flex-wrap: wrap;"), buttons)
      )
    }
  })

  output$downloadRemoteJobResult <- downloadHandler(
    filename = function() {
      job_id <- input$remote_job_id %||% "job"
      paste0("ugplot-remote-job-", job_id, ".rds")
    },
    content = function(file) {
      result <- remote_result_cache()
      if (is.null(result) || !identical(remote_result_cache_job_id(), input$remote_job_id %||% "")) {
        req(nzchar(input$remote_job_id %||% ""))
        server <- remote_server_by_name(remote_server_name_for_job(input$remote_job_id))
        result <- ugplot_remote_get_result(
          server_url = server$url,
          job_id = input$remote_job_id,
          token = server$token %||% ""
        )
      }
      saveRDS(result, file)
    }
  )

  output$downloadMissingScanBestDataset <- downloadHandler(
    filename = function() {
      paste0("missing_threshold_current_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      preview <- missing_preview_data()
      filtered <- apply_missing_filters_resolved(
        predictors = preview$predictors,
        missing_definition = preview$missing_definition,
        zero_exceptions = preview$zero_exceptions,
        threshold_cols = input$ml_missing_threshold_cols,
        threshold_rows = input$ml_missing_threshold_rows,
        filter_order = "auto",
        min_rows_retained = (input$ml_complete_case_min_samples %||% 80) / 100,
        mode = if (identical(input$ml_missing_strategy, "none")) "complete_case" else "balanced"
      )
      target_filtered <- preview$subset_table[, preview$target_name, drop = FALSE]
      if (ncol(filtered$filtered_predictors) > 0) {
        target_filtered <- target_filtered[filtered$keep_rows, , drop = FALSE]
      }
      full_filtered <- cbind(target_filtered, filtered$filtered_predictors)
      names(full_filtered)[1] <- preview$target_name
      if (identical(input$ml_imputation_scope, "full_once")) {
        imputed_full <- apply_missing_strategy(
          trainSet = full_filtered,
          testSet = full_filtered[0, , drop = FALSE],
          target_name = preview$target_name,
          strategy = input$ml_missing_strategy,
          missing_definition = preview$missing_definition,
          zero_exceptions = preview$zero_exceptions,
          threshold_cols = input$ml_missing_threshold_cols,
          threshold_rows = input$ml_missing_threshold_rows,
          threshold_scope = "full_before_split",
          filter_order = "auto",
          min_rows_retained = (input$ml_complete_case_min_samples %||% 80) / 100
        )
        dataset_to_download <- imputed_full$train_set
      } else {
        dataset_to_download <- full_filtered
      }
      utils::write.csv(dataset_to_download, file, row.names = TRUE)
    }
  )

  output$downloadModelAnalysisTable <- downloadHandler(
    filename = function() {
      base_name <- tools::file_path_sans_ext(basename(original_dataset_filename()))
      if (is.null(base_name) || !nzchar(base_name)) {
        base_name <- "model_analysis_results"
      }
      paste0(base_name, ".csv")
    },
    content = function(file) {
      table_to_download <- model_analysis_results_data()
      req(nrow(table_to_download) > 0)
      has_content <- vapply(table_to_download, function(col) {
        normalized <- trimws(as.character(col))
        any(!is.na(col) & normalized != "")
      }, logical(1))
      table_to_download <- table_to_download[, has_content, drop = FALSE]
      utils::write.csv(table_to_download, file, row.names = FALSE)
    }
  )

  output$downloadModelAnalysisPlotTiff <- downloadHandler(
    filename = function() {
      source_name <- original_dataset_filename()
      if (!is.null(input$file1$name) && nzchar(input$file1$name)) {
        source_name <- input$file1$name
      }
      base_name <- tools::file_path_sans_ext(basename(source_name))
      if (is.null(base_name) || !nzchar(base_name)) {
        base_name <- "model_analysis_plot"
      }
      paste0(base_name, "_model_analysis.tiff")
    },
    contentType = "image/tiff",
    content = function(file) {
      recorded_plot <- model_analysis_recorded_plot()
      req(!is.null(recorded_plot))
      tiff(filename = file, width = 2000, height = 2000, res = 300, compression = "lzw")
      replayPlot(recorded_plot)
      dev.off()
    }
  )

  output$gm_download_nodes <- downloadHandler(
    filename = function() paste0("graph_nodes_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(gm_nodes_metrics(), file, row.names = FALSE)
  )

  output$gm_download_edges <- downloadHandler(
    filename = function() paste0("graph_edges_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(gm_edges_metrics(), file, row.names = FALSE)
  )

  output$downloadHeatmapPlotTiffTop <- downloadHandler(
    filename = function() {
      source_name <- original_dataset_filename()
      if (!is.null(input$file1$name) && nzchar(input$file1$name)) {
        source_name <- input$file1$name
      }
      base_name <- tools::file_path_sans_ext(basename(source_name))
      if (is.null(base_name) || !nzchar(base_name)) {
        base_name <- "heatmap_plot"
      }
      paste0(base_name, ".tiff")
    },
    contentType = "image/tiff",
    content = function(file) {
      tiff(filename = file, width = 2000, height = 2000, res = 300, compression = "lzw")
      draw_heatmap_from_code(input$textarea_code_plot)
      dev.off()
    }
  )

  ####################### TAB 1) LOAD DATA
  observeEvent(input$file1, {
    file_click_count(file_click_count() + 1)
    filepath <- req(input$file1$datapath)
    original_dataset_filename(input$file1$name)
    heatmap_recorded_plot(NULL)
    hide("downloadHeatmapPlotTiff")
    skipline <- input$startfromline - 1
    upload_info <- list(filepath = filepath, separator = tab_separator(), skipline = skipline)
    pending_duplicate_row_names_upload(NULL)
    tryCatch({
      data <- read.table(filepath, header = TRUE, sep = tab_separator(), row.names = 1,
        dec = ".", stringsAsFactors = FALSE, strip.white = TRUE, skip = skipline)
      finish_uploaded_dataset_load(data)
    }, error = function(e) {
      error_info <- ""
      if (grepl("duplicate 'row.names' are not allowed", e$message, fixed = TRUE)) {
        data <- read.table(filepath, header = TRUE, sep = tab_separator(), row.names = NULL,
          dec = ".", stringsAsFactors = FALSE, strip.white = TRUE, skip = skipline)
        error_info <- toString(unique(data[duplicated(data[, 1]) | duplicated(data[, 1], fromLast = TRUE), 1]))
        pending_duplicate_row_names_upload(upload_info)
        showModal(modalDialog(
          title = "Duplicate row names",
          tags$p("The first column contains duplicate values, so it cannot be used as row names."),
          tags$p(paste("Duplicated values:", error_info)),
          tags$p("Do you want ugPlot to automatically use sequential row names instead?"),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_sequential_row_names", "Use sequential row names")
          )
        ))
        return()
      }
      showModal(modalDialog(
        title = "Error",
        paste(e$message, error_info),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })

  observeEvent(input$confirm_sequential_row_names, {
    upload_info <- req(pending_duplicate_row_names_upload())
    tryCatch({
      load_uploaded_dataset_with_sequential_row_names(upload_info)
      pending_duplicate_row_names_upload(NULL)
      removeModal()
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        e$message,
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })

  output$contents <- DT::renderDT({
    if (last_file_click_count == 0 || (last_file_click_count != file_click_count())) {
      ## Code to handle multiple files can be added here
    }
    if (length(input$column_checkbox_group) < 2) {
      table_message_text("")
      return(NULL)
    }
    subset_table <- changed_table[input$row_checkbox_group, input$column_checkbox_group]
    print(paste(nrow(subset_table), " x ", ncol(subset_table)))
    if (ncol(subset_table) > max_table_columns) {
      table_message_text(paste("Data has more than ", max_table_columns,
        " columns. For performance reasons, only the first ", max_table_columns,
        " will be shown on the screen."))
    } else {
      table_message_text("")
    }
    empty <- sapply(subset_table, function(column) all(is.na(column)))
    num_empty_columns <- sum(empty)
    print(paste("Number of completely empty columns:", num_empty_columns))
    na_count_per_column <- sapply(subset_table, function(column) sum(is.na(column)))
    na_count_per_non_empty_column <- na_count_per_column[!empty]
    print("Number of empty rows in each non-completely empty column:")
    print(na_count_per_non_empty_column)
    scrambled_visible <- intersect(scrambled_columns(), colnames(subset_table))
    dt <- DT::datatable(
      subset_table,
      options = list(scrollX = TRUE),
      selection = "none"
    )
    if (length(scrambled_visible) > 0) {
      dt <- DT::formatStyle(
        dt,
        columns = scrambled_visible,
        backgroundColor = "var(--scrambled-column-bg)"
      )
    }
    return(dt)
  })

  output$table_message <- renderUI({
    tags$h5(style = "color: red;", table_message_text())
  })

  output$table_cleaning_message <- renderUI({
    if (table_cleaning_message_text() != "") {
      tags$h5(style = "color: orange;", table_cleaning_message_text())
    }
  })

  output$ml_error_message <- renderUI({
    tags$span(ml_error_message_text(), style = "color: black; font-size: 12px;")
  })

  ml_final_summary_ui <- function(summary_data) {
    if (is.null(summary_data)) {
      return(NULL)
    }
    fmt <- function(value, digits = 2) {
      numeric_value <- suppressWarnings(as.numeric(value))
      if (is.null(value) || length(value) == 0 || !is.finite(numeric_value)) {
        return("N/A")
      }
      format(round(numeric_value, digits), nsmall = digits, trim = TRUE)
    }
    runtime_lines <- list(
      tags$strong("Runtime:"),
      tags$div(paste0("Total: ", fmt(summary_data$total_elapsed_seconds), " seconds")),
      tags$div(paste0(
        "Runs: ", summary_data$ok_runs, " OK / ",
        summary_data$timeout_runs, " timeout / ",
        summary_data$incompatible_runs, " incompatible / ",
        summary_data$invalid_metric_runs, " invalid metrics / ",
        summary_data$error_runs, " error"
      ))
    )
    best_model_title <- paste0(summary_data$best_model, "(", summary_data$dataset_seed, ":", summary_data$training_seed, ")")
    if (identical(summary_data$metric_name, "R2")) {
      tags$div(
        class = "ml-final-summary",
        tags$strong("Best result"),
        tags$div(
          class = "ml-final-summary-content",
          tags$div(best_model_title),
          tags$div(paste0("Min R2: ", fmt(summary_data$best_model_min))),
          tags$div(paste0("Max R2: ", fmt(summary_data$best_model_max))),
          tags$div(paste0("Range R2: ", fmt(summary_data$best_model_range))),
          tags$strong("Medians:"),
          tags$div(paste0("R2: ", fmt(summary_data$best_model_median), " (IQR ", fmt(summary_data$best_model_iqr), ")")),
          tags$div(paste0("MAE: ", fmt(summary_data$best_model_mae_median), " (IQR ", fmt(summary_data$best_model_mae_iqr), ")")),
          tags$div(paste0("RMSE: ", fmt(summary_data$best_model_rmse_median), " (IQR ", fmt(summary_data$best_model_rmse_iqr), ")")),
          runtime_lines
        )
      )
    } else {
      tags$div(
        class = "ml-final-summary",
        tags$strong("Best result"),
        tags$div(
          class = "ml-final-summary-content",
          tags$div(best_model_title),
          tags$div(paste0("Min ", summary_data$metric_name, ": ", fmt(summary_data$best_model_min))),
          tags$div(paste0("Max ", summary_data$metric_name, ": ", fmt(summary_data$best_model_max))),
          tags$div(paste0("Range ", summary_data$metric_name, ": ", fmt(summary_data$best_model_range))),
          tags$strong("Medians:"),
          tags$div(paste0(summary_data$metric_name, ": ", fmt(summary_data$best_model_median), " (IQR ", fmt(summary_data$best_model_iqr), ")")),
          runtime_lines
        )
      )
    }
  }

  output$ml_final_status <- renderUI({
    ml_final_summary_ui(ml_final_summary())
  })

  observeEvent(input$ml_toggle_seeds, {
    is_open <- (input$ml_toggle_seeds %% 2) == 1
    updateActionButton(session, "ml_toggle_seeds", label = if (is_open) "\u25be Seeds" else "\u25b8 Seeds")
  }, ignoreInit = TRUE)

  observeEvent(input$ml_toggle_missing, {
    is_open <- (input$ml_toggle_missing %% 2) == 1
    updateActionButton(session, "ml_toggle_missing",
      label = if (is_open) "\u25be Missing Data Strategy" else "\u25b8 Missing Data Strategy")
  }, ignoreInit = TRUE)

  observeEvent(list(input$column_checkbox_group, input$ml_target), {
    req(input$column_checkbox_group, input$ml_target)
    available_predictors <- setdiff(input$column_checkbox_group, input$ml_target)
    selected_exceptions <- isolate(input$ml_zero_exceptions)
    if (is.null(selected_exceptions)) selected_exceptions <- character(0)
    selected_exceptions <- intersect(selected_exceptions, available_predictors)
    updateSelectizeInput(
      session, "ml_zero_exceptions",
      choices = available_predictors,
      selected = selected_exceptions,
      server = FALSE
    )
  }, ignoreInit = FALSE)

  observeEvent(input$column_checkbox_group, {
    choices <- input$column_checkbox_group %||% character(0)
    updateSelectInput(
      session,
      "gm_target",
      choices = c("None" = "", choices),
      selected = if (!is.null(input$gm_target) && input$gm_target %in% choices) input$gm_target else ""
    )
  }, ignoreInit = FALSE)

  observeEvent(input$gm_build_graph, {
    req(input$row_checkbox_group, input$column_checkbox_group)
    subset_table <- changed_table[input$row_checkbox_group, input$column_checkbox_group, drop = FALSE]
    req(nrow(subset_table) > 2, ncol(subset_table) > 1)

    target <- input$gm_target %||% ""
    if (nzchar(target) && target %in% colnames(subset_table)) {
      subset_table <- subset_table[, setdiff(colnames(subset_table), target), drop = FALSE]
    }

    numeric_mask <- vapply(subset_table, function(x) {
      suppressWarnings(any(!is.na(as.numeric(as.character(x)))))
    }, logical(1))
    numeric_df <- subset_table[, numeric_mask, drop = FALSE]
    req(ncol(numeric_df) > 1)

    numeric_df <- as.data.frame(lapply(numeric_df, function(x) suppressWarnings(as.numeric(as.character(x)))))
    keep_variance <- vapply(numeric_df, function(x) stats::sd(x, na.rm = TRUE), numeric(1))
    keep_variance[is.na(keep_variance)] <- 0
    numeric_df <- numeric_df[, keep_variance > 0, drop = FALSE]
    req(ncol(numeric_df) > 1)

    ord <- order(keep_variance[colnames(numeric_df)], decreasing = TRUE)
    numeric_df <- numeric_df[, ord, drop = FALSE]
    max_nodes <- min(input$gm_max_nodes, ncol(numeric_df))
    numeric_df <- numeric_df[, seq_len(max_nodes), drop = FALSE]

    cor_mat <- suppressWarnings(stats::cor(numeric_df, use = "pairwise.complete.obs"))
    cor_mat[is.na(cor_mat)] <- 0
    diag(cor_mat) <- 0
    idx <- which(abs(cor_mat) >= input$gm_corr_threshold, arr.ind = TRUE)
    idx <- idx[idx[, 1] < idx[, 2], , drop = FALSE]

    if (nrow(idx) == 0) {
      gm_nodes_metrics(data.frame())
      gm_edges_metrics(data.frame())
      return()
    }

    edges <- data.frame(
      source = colnames(cor_mat)[idx[, 1]],
      target = colnames(cor_mat)[idx[, 2]],
      weight = cor_mat[idx],
      abs_weight = abs(cor_mat[idx]),
      stringsAsFactors = FALSE
    )

    deg <- table(c(edges$source, edges$target))
    nodes <- data.frame(
      node = names(deg),
      degree = as.integer(deg),
      stringsAsFactors = FALSE
    )
    nodes <- nodes[nodes$degree >= input$gm_min_degree, , drop = FALSE]
    req(nrow(nodes) > 1)
    keep_nodes <- nodes$node
    edges <- edges[edges$source %in% keep_nodes & edges$target %in% keep_nodes, , drop = FALSE]
    req(nrow(edges) > 0)

    adj <- abs(cor_mat[keep_nodes, keep_nodes, drop = FALSE])
    dist_mat <- 1 - adj
    diag(dist_mat) <- 0

    if (identical(input$gm_layout, "circular")) {
      n <- length(keep_nodes)
      theta <- seq(0, 2 * pi, length.out = n + 1)[-1]
      coords <- data.frame(node = keep_nodes, x = cos(theta), y = sin(theta), z = seq(-1, 1, length.out = n))
    } else {
      n_nodes <- length(keep_nodes)
      max_k <- max(1, n_nodes - 1)
      k2 <- min(2, max_k)
      k3 <- min(3, max_k)
      fit2 <- cmdscale(as.dist(dist_mat), k = k2)
      fit3 <- cmdscale(as.dist(dist_mat), k = k3)

      fit2_mat <- as.matrix(fit2)
      fit3_mat <- as.matrix(fit3)
      x_vals <- fit2_mat[, 1]
      y_vals <- if (ncol(fit2_mat) >= 2) fit2_mat[, 2] else rep(0, nrow(fit2_mat))
      z_vals <- if (ncol(fit3_mat) >= 3) fit3_mat[, 3] else rep(0, nrow(fit3_mat))

      coords <- data.frame(node = rownames(fit2_mat), x = x_vals, y = y_vals, z = z_vals)
    }

    nodes <- merge(nodes, coords, by = "node", all.x = TRUE)
    nodes <- nodes[order(-nodes$degree), , drop = FALSE]
    gm_nodes_metrics(nodes)
    gm_edges_metrics(edges[order(-edges$abs_weight), , drop = FALSE])
  })

  missing_preview_data <- reactive({
    req(input$ml_target)
    req(input$row_checkbox_group, input$column_checkbox_group)
    subset_table <- changed_table[input$row_checkbox_group, input$column_checkbox_group, drop = FALSE]
    req(nrow(subset_table) > 0, ncol(subset_table) > 0)
    target_name <- input$ml_target
    req(target_name %in% colnames(subset_table))
    predictors <- subset_table[, setdiff(colnames(subset_table), target_name), drop = FALSE]
    missing_definition <- input$ml_missing_definition
    if (length(missing_definition) == 0) {
      missing_definition <- character(0)
    }
    zero_exceptions <- input$ml_zero_exceptions
    if (is.null(zero_exceptions)) {
      zero_exceptions <- character(0)
    }
    list(
      subset_table = subset_table,
      target_name = target_name,
      predictors = predictors,
      missing_definition = missing_definition,
      zero_exceptions = zero_exceptions
    )
  })

  output$ml_missing_summary <- renderUI({
    preview <- missing_preview_data()
    predictors <- preview$predictors
    missing_mask <- build_missing_mask(predictors, preview$missing_definition, preview$zero_exceptions)
    missing_count <- sum(missing_mask)
    total_cells <- length(as.matrix(predictors))
    missing_pct <- if (total_cells > 0) round(100 * missing_count / total_cells, 2) else 0
    col_threshold <- input$ml_missing_threshold_cols
    row_threshold <- input$ml_missing_threshold_rows
    strategy <- input$ml_missing_strategy
    filtered <- apply_missing_filters_resolved(
      predictors = predictors,
      missing_definition = preview$missing_definition,
      zero_exceptions = preview$zero_exceptions,
      threshold_cols = col_threshold,
      threshold_rows = row_threshold,
      filter_order = "auto",
      min_rows_retained = (input$ml_complete_case_min_samples %||% 80) / 100,
      mode = if (identical(strategy, "none")) "complete_case" else "balanced"
    )
    filtered_mask <- filtered$filtered_mask
    columns_after <- ncol(filtered_mask)
    samples_after <- nrow(filtered_mask)
    est_missing_after <- if (length(filtered_mask) > 0) sum(filtered_mask) else 0
    removed_columns <- filtered$removed_cols
    removed_samples_idx <- filtered$removed_rows
    row_names <- rownames(predictors)
    removed_samples <- if (length(removed_samples_idx) > 0) {
      if (!is.null(row_names) && any(nzchar(row_names))) {
        row_names[removed_samples_idx]
      } else {
        as.character(removed_samples_idx)
      }
    } else {
      character(0)
    }
    if (strategy %in% c("replace_zero", "mean", "knn", "missforest", "methylimp2")) {
      est_missing_after <- 0
    }

    make_summary_row <- function(label, before_value, after_value) {
      row_class <- if (!identical(before_value, after_value)) "ml-summary-row-changed" else ""
      tags$tr(
        class = row_class,
        tags$td(style = "padding: 8px 12px; border-bottom: 1px solid #edf0f3;", label),
        tags$td(style = "padding: 8px 12px; border-bottom: 1px solid #edf0f3;", as.character(before_value)),
        tags$td(style = "padding: 8px 12px; border-bottom: 1px solid #edf0f3;", as.character(after_value))
      )
    }

    tags$div(
      tags$h5("Dataset Missingness Summary"),
      tags$table(
        class = "ml-summary-table",
        style = "width: 100%; max-width: 760px; border-collapse: collapse; border: 1px solid #e2e6ea; background: #fff;",
        tags$thead(
          tags$tr(
            tags$th(style = "padding: 8px 12px; background: #f5f7fa; border-bottom: 1px solid #e2e6ea;", "Metric"),
            tags$th(style = "padding: 8px 12px; background: #f5f7fa; border-bottom: 1px solid #e2e6ea;", "Current"),
            tags$th(style = "padding: 8px 12px; background: #f5f7fa; border-bottom: 1px solid #e2e6ea;", "After configuration")
          )
        ),
        tags$tbody(
          make_summary_row("Number of columns", ncol(predictors), columns_after),
          make_summary_row("Number of samples", nrow(predictors), samples_after),
          make_summary_row("Missing cells", missing_count, est_missing_after),
          make_summary_row("Missingness (%)", paste0(missing_pct, "%"),
            if ((columns_after * samples_after) > 0) {
              paste0(round(100 * est_missing_after / (columns_after * samples_after), 2), "%")
            } else {
              "0%"
            }
          )
        )
      ),
      tags$p(
        style = "margin-top: 8px; margin-bottom: 2px; font-size: 12px; color: #596273;",
        tags$b("Removed columns: "),
        if (length(removed_columns) > 0) paste(removed_columns, collapse = ", ") else "None"
      ),
      tags$p(
        style = "margin-top: 2px; font-size: 12px; color: #596273;",
        tags$b("Removed samples: "),
        if (length(removed_samples) > 0) paste(removed_samples, collapse = ", ") else "None"
      ),
      tags$p(
        style = "margin-top: 8px; font-size: 12px; color: #596273;",
        "Thresholds are always applied to the full dataset before split (Mode B). Automatic order: ",
        if (identical(filtered$resolved_order %||% "cols_first", "rows_first")) {
          "samples, then columns."
        } else {
          "columns, then samples."
        }
      )
    )
  })

  output$ml_missing_definition_stats <- renderUI({
    preview <- missing_preview_data()
    counts <- missing_definition_counts(preview$predictors, preview$zero_exceptions)
    total_cells <- nrow(preview$predictors) * ncol(preview$predictors)
    counts$Percent <- if (total_cells > 0) {
      paste0(round(100 * counts$Cells / total_cells, 2), "%")
    } else {
      "0%"
    }
    tags$div(
      style = "margin: 4px 0 10px 0; max-width: 520px;",
      tags$p(
        style = "margin-bottom: 4px; font-size: 12px; color: #596273;",
        "Current dataset missing-rule counts:"
      ),
      tags$table(
        class = "ml-summary-table",
        style = "width: 100%; border-collapse: collapse; border: 1px solid #e2e6ea; background: #fff; font-size: 12px;",
        tags$thead(tags$tr(
          tags$th(style = "padding: 6px 8px; background: #f5f7fa;", "Rule"),
          tags$th(style = "padding: 6px 8px; background: #f5f7fa;", "Cells"),
          tags$th(style = "padding: 6px 8px; background: #f5f7fa;", "Columns"),
          tags$th(style = "padding: 6px 8px; background: #f5f7fa;", "%")
        )),
        tags$tbody(lapply(seq_len(nrow(counts)), function(i) {
          tags$tr(
            tags$td(style = "padding: 6px 8px; border-top: 1px solid #edf0f3;", counts$Rule[[i]]),
            tags$td(style = "padding: 6px 8px; border-top: 1px solid #edf0f3;", counts$Cells[[i]]),
            tags$td(style = "padding: 6px 8px; border-top: 1px solid #edf0f3;", counts$Columns[[i]]),
            tags$td(style = "padding: 6px 8px; border-top: 1px solid #edf0f3;", counts$Percent[[i]])
          )
        }))
      )
    )
  })

  threshold_scan_results <- reactiveVal(NULL)
  threshold_scan_status <- reactiveVal("Status: idle (click the button to run exhaustive scan).")

  output$gm_summary <- renderUI({
    nodes <- gm_nodes_metrics()
    edges <- gm_edges_metrics()
    if (nrow(nodes) == 0 || nrow(edges) == 0) {
      return(tags$div(class = "gm-muted-text", "Build graph to view metrics and plots."))
    }
    tags$div(
      class = "ml-final-summary",
      tags$strong("Graph summary"),
      tags$div(class = "ml-final-summary-content",
        tags$div(paste("Nodes:", nrow(nodes))),
        tags$div(paste("Edges:", nrow(edges))),
        tags$div(paste("Average degree:", round(mean(nodes$degree), 2))),
        tags$div(paste("Max absolute correlation:", round(max(edges$abs_weight), 3)))
      )
    )
  })

  output$gm_network_plot <- renderPlot({
    nodes <- gm_nodes_metrics()
    edges <- gm_edges_metrics()
    req(nrow(nodes) > 1, nrow(edges) > 0)
    seg <- merge(edges, nodes[, c("node", "x", "y")], by.x = "source", by.y = "node")
    seg <- merge(seg, nodes[, c("node", "x", "y")], by.x = "target", by.y = "node", suffixes = c("_source", "_target"))
    ggplot() +
      geom_segment(
        data = seg,
        aes(x = x_source, y = y_source, xend = x_target, yend = y_target, size = abs_weight, color = weight > 0),
        alpha = 0.35
      ) +
      geom_point(data = nodes, aes(x = x, y = y, size = degree), color = "#2c7fb8") +
      geom_text(data = nodes, aes(x = x, y = y, label = node), size = 3, vjust = -0.6) +
      scale_color_manual(values = c("TRUE" = "#1a9850", "FALSE" = "#d73027"), guide = "none") +
      scale_size_continuous(range = c(0.4, 6)) +
      theme_minimal() +
      labs(title = "Feature graph", x = NULL, y = NULL)
  })

  output$gm_degree_plot <- renderPlot({
    nodes <- gm_nodes_metrics()
    req(nrow(nodes) > 0)
    ggplot(nodes, aes(x = reorder(node, -degree), y = degree)) +
      geom_col(fill = "#225ea8") +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(title = "Node degree distribution", x = "Nodes", y = "Degree")
  })

  output$gm_network_plot_3d <- renderPlotly({
    req(isTRUE(input$gm_use_3d))
    nodes <- gm_nodes_metrics()
    edges <- gm_edges_metrics()
    req(nrow(nodes) > 1, nrow(edges) > 0)
    seg <- merge(edges, nodes[, c("node", "x", "y", "z")], by.x = "source", by.y = "node")
    seg <- merge(seg, nodes[, c("node", "x", "y", "z")], by.x = "target", by.y = "node", suffixes = c("_source", "_target"))
    plot_ly(type = "scatter3d", mode = "markers") |>
      add_trace(
        data = seg,
        x = ~x_source, y = ~y_source, z = ~z_source,
        type = "scatter3d", mode = "lines",
        line = list(width = 2, color = "#9aa3ad"),
        hoverinfo = "none",
        showlegend = FALSE
      ) |>
      add_trace(
        data = nodes,
        x = ~x, y = ~y, z = ~z,
        type = "scatter3d", mode = "markers+text",
        text = ~node,
        textposition = "top center",
        marker = list(size = ~pmax(4, degree + 2), color = "#2c7fb8", opacity = 0.9),
        hovertemplate = "Node: %{text}<br>Degree: %{customdata}<extra></extra>",
        customdata = ~degree,
        showlegend = FALSE
      ) |>
      layout(title = "Feature graph (3D)") |>
      htmlwidgets::onRender(
        "function(el, x) {
          var gd = document.getElementById(el.id);
          if (!gd) return;
          if (gd._gmRotateId) cancelAnimationFrame(gd._gmRotateId);
          var angle = 0;
          function rotate() {
            angle += 0.005;
            Plotly.relayout(gd, {
              'scene.camera.eye': {
                x: 1.6 * Math.cos(angle),
                y: 1.6 * Math.sin(angle),
                z: 0.9
              }
            });
            gd._gmRotateId = requestAnimationFrame(rotate);
          }
          gd.on('plotly_hover', function() { if (gd._gmRotateId) { cancelAnimationFrame(gd._gmRotateId); gd._gmRotateId = null; } });
          gd.on('plotly_beforeplot', function() { if (gd._gmRotateId) { cancelAnimationFrame(gd._gmRotateId); gd._gmRotateId = null; } });
          rotate();
        }"
      )
  })

  output$gm_nodes_table <- DT::renderDT({
    req(nrow(gm_nodes_metrics()) > 0)
    DT::datatable(gm_nodes_metrics(), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$gm_edges_table <- DT::renderDT({
    req(nrow(gm_edges_metrics()) > 0)
    DT::datatable(gm_edges_metrics(), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$ml_threshold_scan_status <- renderText({
    threshold_scan_status()
  })

  reset_missing_strategy_ui <- function() {
    updateCheckboxGroupInput(session, "ml_missing_definition", selected = c("empty", "na"))
    updateSelectizeInput(session, "ml_zero_exceptions", selected = character(0))
    updateSelectInput(session, "ml_missing_strategy", selected = "none")
    updateSelectInput(session, "ml_imputation_scope", selected = "split_separate")
    updateNumericInput(session, "ml_missing_threshold_cols", value = 100)
    updateNumericInput(session, "ml_missing_threshold_rows", value = 100)
    updateNumericInput(session, "ml_complete_case_min_samples", value = 80)
    threshold_scan_results(NULL)
    threshold_scan_status("Status: idle (click the button to run exhaustive scan).")
  }

  observeEvent(input$ml_run_threshold_scan, {
    preview <- missing_preview_data()
    preview_missing_mask <- build_missing_mask(preview$predictors, preview$missing_definition, preview$zero_exceptions)
    preview_missing_count <- if (length(preview_missing_mask) > 0) sum(preview_missing_mask) else 0
    if (preview_missing_count == 0) {
      threshold_scan_results(NULL)
      threshold_scan_status("Status: skipped. No missing values detected with current definition; exhaustive scan is unnecessary.")
      showNotification("No missing values detected for the selected data/definition. Threshold scan was skipped.", type = "message")
      return(invisible(NULL))
    }
    threshold_scan_status("Status: starting exhaustive scan (0-100% x 0-100%)...")
    started_at <- Sys.time()
    progress_bar <- shiny::Progress$new(session, min = 0, max = 1)
    on.exit(progress_bar$close(), add = TRUE)
    progress_bar$set(message = "Running exhaustive threshold scan", value = 0)
    results <- compute_exhaustive_threshold_scan(
      predictors = preview$predictors,
      missing_definition = preview$missing_definition,
      zero_exceptions = preview$zero_exceptions,
      min_rows_retained = (input$ml_complete_case_min_samples %||% 80) / 100,
      mode = if (identical(input$ml_missing_strategy, "none")) "complete_case" else "balanced",
      progress_callback = function(progress_value) {
        progress_bar$set(
          value = progress_value,
          detail = sprintf("%.0f%% completed", 100 * progress_value)
        )
      },
      status_callback = function(msg) {
        elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
        threshold_scan_status(sprintf("Status: %s | elapsed: %.1fs", msg, elapsed))
      }
    )
    threshold_scan_results(results)
    if (!is.null(results) && nrow(results) > 0) {
      best <- results[1, , drop = FALSE]
      updateNumericInput(session, "ml_missing_threshold_cols", value = as.numeric(best$thr_col))
      updateNumericInput(session, "ml_missing_threshold_rows", value = as.numeric(best$thr_row))
    }
    elapsed_total <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
    threshold_scan_status(sprintf(
      "Status: completed. Tested %s combinations in %.1fs.",
      nrow(threshold_scan_results()), elapsed_total
    ))
  })

  output$ml_threshold_scan_summary <- renderUI({
    results <- threshold_scan_results()
    req(nrow(results) > 0)
    best <- results[1, , drop = FALSE]
    pareto_count <- sum(results$pareto)
    cross_count <- sum(results$cross_point)
    complete_count <- sum(results$complete_case)
    mode_label <- if (identical(input$ml_missing_strategy, "none")) {
      sprintf("Complete-case recommendation (>= %s%% samples retained)", input$ml_complete_case_min_samples %||% 80)
    } else {
      "Best hotspot found (maximize information, minimize missingness)"
    }
    if (identical(input$ml_missing_strategy, "none") && !isTRUE(best$complete_case)) {
      mode_label <- "No complete-case result found; showing best balanced hotspot"
    } else if (identical(input$ml_missing_strategy, "none") && !isTRUE(best$meets_min_samples)) {
      mode_label <- sprintf(
        "Complete-case fallback (no result kept >= %s%% samples)",
        input$ml_complete_case_min_samples %||% 80
      )
    }
    tags$div(
      style = "margin: 8px 0 12px 0; padding: 10px; background: #f6fbf6; border: 1px solid #cfe9cf;",
      tags$b(paste0(mode_label, ": ")),
      sprintf("columns = %s%%, rows = %s%%, order = %s", best$thr_col, best$thr_row, best$scan_order),
      tags$br(),
      sprintf(
        "After filter: %s columns, %s samples, %s filled cells, %.2f%% missing.",
        best$n_cols_after, best$n_rows_after, best$filled_cells, best$missing_pct_after
      ),
      tags$br(),
      sprintf(
        "Missing cells after filter: %s (out of %s total cells).",
        best$missing_cells_after, best$total_cells_after
      ),
      tags$br(),
      sprintf(
        "Complete-case pairs: %s | Pareto hotspots: %s | Crossing points (same result in both orders): %s | Tested pairs: %s.",
        complete_count, pareto_count, cross_count, nrow(results)
      )
    )
  })

  target_distribution_data <- reactive({
    preview <- missing_preview_data()
    target_values <- preview$subset_table[, preview$target_name, drop = TRUE]
    target_filtered <- preview$subset_table[, preview$target_name, drop = FALSE]
    filtered <- apply_missing_filters_resolved(
      predictors = preview$predictors,
      missing_definition = preview$missing_definition,
      zero_exceptions = preview$zero_exceptions,
      threshold_cols = input$ml_missing_threshold_cols,
      threshold_rows = input$ml_missing_threshold_rows,
      filter_order = "auto",
      min_rows_retained = (input$ml_complete_case_min_samples %||% 80) / 100,
      mode = if (identical(input$ml_missing_strategy, "none")) "complete_case" else "balanced"
    )
    if (ncol(filtered$filtered_predictors) > 0) {
      target_filtered <- target_filtered[filtered$keep_rows, , drop = FALSE]
    }
    list(
      target_name = preview$target_name,
      original = target_values,
      filtered = target_filtered[, 1, drop = TRUE]
    )
  })

  output$ml_target_plot_original <- renderPlot({
    dist_data <- target_distribution_data()
    target_values <- dist_data$original
    if (is.numeric(target_values)) {
      hist(target_values, breaks = 20, main = "Target distribution (original)",
        xlab = dist_data$target_name, col = "#9ecae1", border = "white")
    } else {
      counts <- sort(table(target_values), decreasing = TRUE)
      barplot(counts, las = 2, col = "#9ecae1", main = "Target counts (original)",
        ylab = "Count")
    }
  })

  output$ml_target_plot_filtered <- renderPlot({
    dist_data <- target_distribution_data()
    target_values <- dist_data$filtered
    if (is.numeric(target_values)) {
      hist(target_values, breaks = 20, main = "Target distribution (filtered)",
        xlab = dist_data$target_name, col = "#74c476", border = "white")
    } else {
      counts <- sort(table(target_values), decreasing = TRUE)
      barplot(counts, las = 2, col = "#74c476", main = "Target counts (filtered)",
        ylab = "Count")
    }
  })

  output$ml_target_plot_removed <- renderPlot({
    dist_data <- target_distribution_data()
    original_values <- dist_data$original
    filtered_values <- dist_data$filtered

    if (is.numeric(original_values)) {
      breaks <- hist(original_values, breaks = 20, plot = FALSE)$breaks
      original_hist <- hist(original_values, breaks = breaks, plot = FALSE)$counts
      filtered_hist <- hist(filtered_values, breaks = breaks, plot = FALSE)$counts
      removed_counts <- pmax(original_hist - filtered_hist, 0)
      mids <- head(breaks, -1) + diff(breaks) / 2
      barplot(
        removed_counts,
        names.arg = round(mids, 1),
        las = 2,
        col = "#fb6a4a",
        border = "white",
        main = "Removed samples per target range (original - filtered)",
        xlab = dist_data$target_name,
        ylab = "Removed count"
      )
    } else {
      original_counts <- table(original_values)
      filtered_counts <- table(filtered_values)
      all_levels <- union(names(original_counts), names(filtered_counts))
      removed_counts <- as.numeric(original_counts[all_levels]) - as.numeric(filtered_counts[all_levels])
      removed_counts[is.na(removed_counts)] <- 0
      removed_counts <- pmax(removed_counts, 0)
      names(removed_counts) <- all_levels
      barplot(
        removed_counts,
        las = 2,
        col = "#fb6a4a",
        border = "white",
        main = "Removed samples per target class (original - filtered)",
        ylab = "Removed count"
      )
    }
  })

  observeEvent(input$column_checkbox_group, {
    updateSelectizeInput(session, "ml_target", choices = c("", input$column_checkbox_group),
      selected = if (length(input$column_checkbox_group) > 0) input$column_checkbox_group[1] else "",
      server = TRUE)
    update_scramble_selector(selected = isolate(input$scramble_column))
  })

  update_scramble_selector <- function(selected = NULL) {
    if (!(is.data.frame(changed_table) || is.matrix(changed_table))) {
      updateSelectInput(session, "scramble_column", choices = character(0), selected = character(0))
      return(invisible(NULL))
    }
    current_columns <- intersect(colnames(changed_table), input$column_checkbox_group %||% character(0))
    if (is.null(selected) || !(selected %in% current_columns)) {
      selected <- if (length(current_columns) > 0) current_columns[1] else character(0)
    }
    freezeReactiveValue(input, "scramble_column")
    updateSelectInput(
      session,
      inputId = "scramble_column",
      choices = current_columns,
      selected = selected
    )
  }

  observeEvent(input$remove_empty_columns, {
    subset_table <- changed_table[input$row_checkbox_group, input$column_checkbox_group]
    empty <- sapply(subset_table, function(column) all(is.na(column)))
    all_column_names <- names(subset_table)
    non_empty_column_names <- all_column_names[!empty]
    updateCheckboxGroupInput(session, inputId = "column_checkbox_group", selected = non_empty_column_names)
    print("Unchecking empty columns")
  })

  observeEvent(input$add_all_columns, {
    updateTextAreaInput(session, "textarea_columns", value = paste(names(df_pre), collapse = "\n"))
  })

  observeEvent(input$remove_all_columns, {
    updateTextAreaInput(session, "textarea_columns", value = "")
  })

  observeEvent(input$add_all_rows, {
    updateTextAreaInput(session, "textarea_rows", value = paste(rownames(df_pre), collapse = "\n"))
  })

  observeEvent(input$remove_all_rows, {
    updateTextAreaInput(session, "textarea_rows", value = "")
  })

  observeEvent(input$merge_all_columns, {
    column_names <- strsplit(input$textarea_columns, "\n")[[1]]
    rown_names <- strsplit(input$textarea_rows, "\n")[[1]]
    new_df <- as.data.frame(t(df_pre[rown_names, column_names, drop = FALSE]))
    common_rownames <- intersect(rownames(dff), rownames(new_df))
    dff[common_rownames, names(new_df)] <<- new_df[common_rownames, ]
    changed_table <<- as.matrix(dff)
    refresh_counter(refresh_counter() + 1)
    scrambled_columns(character(0))
    scramble_original_columns(list())
    load_checkbox_group()
    update_scramble_selector()
    updateTabsetPanel(session, "tabs", selected = "TABLE")
  })

  observeEvent(input$remove_columns_variability, {
    current_selected <- input$column_checkbox_group
    if (is.null(changed_table) || length(current_selected) == 0) return()
    data <- as.data.frame(changed_table)
    new_selection <- current_selected
    for (col in current_selected) {
      if (col %in% names(data) && is.numeric(data[[col]])) {
        nonzero_values <- data[[col]][data[[col]] != 0 & !is.na(data[[col]])]
        diff_val <- if (length(nonzero_values) == 0) 0 else max(data[[col]], na.rm = TRUE) - min(nonzero_values)
        if (diff_val < input$minvariability) {
          new_selection <- setdiff(new_selection, col)
        }
      }
    }
    updateCheckboxGroupInput(session, inputId = "column_checkbox_group", selected = new_selection)
  })

  observeEvent(input$scramble_column_button, {
    req(input$scramble_column)
    req(is.data.frame(changed_table) || is.matrix(changed_table))
    col_to_scramble <- input$scramble_column
    if (!(col_to_scramble %in% colnames(changed_table))) return()

    changed_table_df <- as.data.frame(changed_table, stringsAsFactors = FALSE)
    original_columns <- scramble_original_columns()

    if (is.null(original_columns[[col_to_scramble]])) {
      original_columns[[col_to_scramble]] <- changed_table_df[[col_to_scramble]]
    }

    column_values <- changed_table_df[[col_to_scramble]]
    if (length(column_values) > 1) {
      changed_table_df[[col_to_scramble]] <- sample(column_values, length(column_values), replace = FALSE)
    }

    changed_table <<- changed_table_df
    refresh_counter(refresh_counter() + 1)
    scramble_original_columns(original_columns)
    scrambled_columns(unique(c(scrambled_columns(), col_to_scramble)))
  })

  observeEvent(input$reset_scramble_columns, {
    req(is.data.frame(changed_table) || is.matrix(changed_table))
    original_columns <- scramble_original_columns()
    if (length(original_columns) == 0) return()

    changed_table_df <- as.data.frame(changed_table, stringsAsFactors = FALSE)
    for (col_name in names(original_columns)) {
      if (col_name %in% colnames(changed_table_df) &&
          length(original_columns[[col_name]]) == nrow(changed_table_df)) {
        changed_table_df[[col_name]] <- original_columns[[col_name]]
      }
    }
    changed_table <<- changed_table_df
    refresh_counter(refresh_counter() + 1)
    scrambled_columns(character(0))
    scramble_original_columns(list())
  })

  observeEvent(input$merge_all_rows, {
    column_names <- strsplit(input$textarea_columns, "\n")[[1]]
    rown_names <- strsplit(input$textarea_rows, "\n")[[1]]
    new_df <- df_pre[rown_names, column_names, drop = FALSE]
    common_rownames <- intersect(rownames(dff), rownames(new_df))
    dff[common_rownames, names(new_df)] <<- new_df[common_rownames, ]
    changed_table <<- as.data.frame(dff)
    refresh_counter(refresh_counter() + 1)
    scrambled_columns(character(0))
    scramble_original_columns(list())
    load_checkbox_group()
    update_scramble_selector()
    updateTabsetPanel(session, "tabs", selected = "TABLE")
  })

  observeEvent(input$process_table_content, {
    req(nzchar(trimws(input$textarea_columns %||% "")))
    req(nzchar(trimws(input$textarea_rows %||% "")))
    scrambled_columns(character(0))
    scramble_original_columns(list())
    heatmap_recorded_plot(NULL)
    hide("downloadHeatmapPlotTiff")
    load_file_into_table(input$textarea_columns, input$textarea_rows, session)
    refresh_counter(refresh_counter() + 1)
    update_scramble_selector()
  })

  observeEvent(input$load_sample, {
    dff <<- sample_data
    original_dataset_filename("sample.csv")
    heatmap_recorded_plot(NULL)
    hide("downloadHeatmapPlotTiff")
    reset_missing_strategy_ui()
    scrambled_columns(character(0))
    scramble_original_columns(list())
    head(dff)
    load_dataset_into_table(session)
    refresh_counter(refresh_counter() + 1)
    update_scramble_selector()
  })

  observeEvent(input$separator, {
    tab_separator(input$separator)
  })

  ####################### TAB 3) HEATMAP PLOT
  draw_heatmap_from_code <- function(comandtorun, template_palette = NULL) {
    cols_to_convert <- intersect(input$checkbox_group_categories, input$column_checkbox_group)
    countdataframe <- 0
    if (length(cols_to_convert) > 0) {
      for (this_target in cols_to_convert) {
        changed_table[[this_target]] <- as.factor(changed_table[[this_target]])
        if (countdataframe == 0) {
          annotation_row <- setNames(data.frame(changed_table[[this_target]]), this_target)
          rownames(annotation_row) <- rownames(changed_table)
        } else {
          annotation_row[[this_target]] <- changed_table[[this_target]]
        }
        countdataframe <- 1
      }
    }
    numeric_table <- data.frame(changed_table[input$row_checkbox_group, input$column_checkbox_group])
    numeric_table <- numeric_table[, !(names(numeric_table) %in% cols_to_convert)]
    numeric_table <- apply(numeric_table, c(1, 2), as.numeric)
    if (input$plot_xy == "ROW x COL") {
      # sem transformacao adicional
    } else if (input$plot_xy == "COL x COL") {
      numeric_table <- cor(numeric_table)
    } else if (input$plot_xy == "ROW x ROW") {
      numeric_table <- cor(t(numeric_table), use = "pairwise.complete.obs")
    }
    comandtorun <- gsub("\\{\\{dataset\\}\\}", "numeric_table", comandtorun)
    if (!is.null(template_palette) && nzchar(template_palette) && changed_palette == 0) {
      comandpalette <- paste("defaultpalette(", template_palette, ")")
      eval(parse(text = comandpalette))
    }
    comandtorun <- gsub("\\{\\{palette\\}\\}", "defaultpalette()", comandtorun)
    comandtorun <- gsub("\\{\\{annotation\\}\\}", "annotation_row", comandtorun)
    annotation_colors_auto <- generate_annotation_colors(annotation_row)
    comandtorun <- gsub("\\{\\{annotation_color\\}\\}", "annotation_colors_auto", comandtorun)
    eval_env <- new.env(parent = environment())
    eval_env$numeric_table <- numeric_table
    eval_env$annotation_row <- annotation_row
    eval_env$annotation_colors_auto <- annotation_colors_auto
    plotted_obj <- eval(parse(text = comandtorun), envir = eval_env)
    if (inherits(plotted_obj, "ggplot") || inherits(plotted_obj, "gg")) {
      print(plotted_obj)
    } else {
      env_objects <- rev(ls(envir = eval_env, all.names = TRUE))
      for (obj_name in env_objects) {
        obj_val <- get(obj_name, envir = eval_env)
        if (inherits(obj_val, "ggplot") || inherits(obj_val, "gg")) {
          print(obj_val)
          break
        }
      }
    }
    invisible(plotted_obj)
  }

  lapply(1:nrow(plotlist), function(i) {
    bname <- paste0("buttonplot", i)
    observeEvent(input[[bname]], {
      output$plot <- renderPlot({
        comandtorun <- plotlist$code[i]
        updateTextAreaInput(session, "textarea_code_plot", value = comandtorun)
        draw_heatmap_from_code(comandtorun, template_palette = plotlist$palette[i])
      })
    })
  })

  lapply(1:nrow(palettelist), function(i) {
    bname <- paste0("buttonpalette", i)
    observeEvent(input[[bname]], {
      changed_palette <<- 1
      comandpalette <- paste("defaultpalette(", palettelist$code[i], ")")
      eval(parse(text = comandpalette))
    })
  })

  observeEvent(input$run_code_plot, {
    output$plot <- renderPlot({
      comandtorun <- input$textarea_code_plot
      draw_heatmap_from_code(comandtorun)
    })
  })

  observeEvent(input$uncheck_all_columns, {
    updateCheckboxGroupInput(session, inputId = "column_checkbox_group", selected = character(0))
  })
  observeEvent(input$check_all_columns, {
    updateCheckboxGroupInput(session, inputId = "column_checkbox_group", selected = names(dff))
  })
  observeEvent(input$uncheck_all_rows, {
    updateCheckboxGroupInput(session, inputId = "row_checkbox_group", selected = character(0))
  })
  observeEvent(input$check_all_rows, {
    updateCheckboxGroupInput(session, inputId = "row_checkbox_group", selected = rownames(dff))
  })
  observeEvent(input$transpose_table, {
    if (transpose_table2() == 0) {
      transpose_table2(1)
    } else {
      transpose_table2(0)
    }
    dff <<- data.frame(t(as.matrix(dff)))
    changed_table <<- dff
    heatmap_recorded_plot(NULL)
    hide("downloadHeatmapPlotTiff")
    refresh_counter(refresh_counter() + 1)
    scrambled_columns(character(0))
    scramble_original_columns(list())
    load_checkbox_group()
    update_scramble_selector()
  })

  ####################### TAB 4) 2D PLOT
  selected_2d_plot_code <- reactiveVal(NULL)

  observe({
    selected_columns <- intersect(input$column_checkbox_group, colnames(changed_table))
    cols_to_convert <- intersect(input$checkbox_group_categories, selected_columns)
    candidate_columns <- setdiff(selected_columns, cols_to_convert)
    current_choice <- isolate(input$plot2d_column_filter)
    valid_choice <- if (!is.null(current_choice) && nzchar(current_choice) && current_choice %in% candidate_columns) current_choice else ""
    updateSelectInput(
      session,
      inputId = "plot2d_column_filter",
      choices = c("All columns" = "", candidate_columns),
      selected = valid_choice
    )
  })

  lapply(1:nrow(plotlist2d), function(i) {
    bname <- paste0("buttonplot2d", i)
    observeEvent(input[[bname]], {
      selected_2d_plot_code(plotlist2d$code[i])
    })
  })

  correlations_2d_results <- reactive({
    req(selected_2d_plot_code())

    selected_rows <- intersect(input$row_checkbox_group, rownames(changed_table))
    selected_columns <- intersect(input$column_checkbox_group, colnames(changed_table))
    cols_to_convert <- intersect(input$checkbox_group_categories, selected_columns)
    numeric_columns <- setdiff(selected_columns, cols_to_convert)
    shiny::validate(shiny::need(length(selected_rows) > 0, "Select at least one row to render correlations."))
    shiny::validate(shiny::need(length(numeric_columns) >= 2, "Select at least two numeric columns to render correlations."))

    numeric_table <- data.frame(changed_table[selected_rows, numeric_columns, drop = FALSE])
    X <- data.frame(lapply(numeric_table, as.numeric), check.names = FALSE)
    shiny::validate(shiny::need(ncol(X) >= 2, "Select at least two numeric columns to render correlations."))

    cor_matrix <- suppressWarnings(cor(X, method = input$correlation, use = "pairwise.complete.obs"))
    num_cols <- ncol(X)
    result_rows <- list()
    for (col_i in seq_len(num_cols - 1)) {
      for (col_j in seq(col_i + 1, num_cols)) {
        correlation_value <- cor_matrix[col_i, col_j]
        if (is.na(correlation_value)) {
          next
        }
        if (correlation_value >= input$correlation_threshhold ||
            correlation_value <= input$correlation_threshhold_negative) {
          x_name <- colnames(X)[col_i]
          y_name <- colnames(X)[col_j]
          if (nzchar(input$plot2d_column_filter) &&
              !identical(input$plot2d_column_filter, x_name) &&
              !identical(input$plot2d_column_filter, y_name)) {
            next
          }
          result_rows[[length(result_rows) + 1]] <- data.frame(
            Column_X = x_name,
            Column_Y = y_name,
            Correlation = correlation_value,
            AbsCorrelation = abs(correlation_value),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    if (length(result_rows) == 0) {
      return(list(results_table = data.frame(), plots_list = list()))
    }

    results_table <- do.call(rbind, result_rows)
    results_table <- results_table[order(-results_table$AbsCorrelation, -results_table$Correlation), , drop = FALSE]

    plots_list <- lapply(seq_len(nrow(results_table)), function(row_idx) {
      comandtorun <- selected_2d_plot_code()
      x_col <- results_table$Column_X[row_idx]
      y_col <- results_table$Column_Y[row_idx]
      correlation_value <- results_table$Correlation[row_idx]
      comandtorun <- gsub("\\{\\{X\\}\\}", sprintf("X[[%s]]", deparse(x_col)), comandtorun)
      comandtorun <- gsub("\\{\\{Y\\}\\}", sprintf("X[[%s]]", deparse(y_col)), comandtorun)
      comandtorun <- gsub("\\{\\{X_NAME\\}\\}", deparse(x_col), comandtorun)
      comandtorun <- gsub("\\{\\{Y_NAME\\}\\}", deparse(y_col), comandtorun)
      comandtorun <- gsub("\\{\\{CORRELATION\\}\\}", as.character(correlation_value), comandtorun)
      eval(parse(text = comandtorun))
    })

    list(
      results_table = results_table[, c("Column_X", "Column_Y", "Correlation"), drop = FALSE],
      plots_list = plots_list
    )
  })

  output$plot2d_results_table <- DT::renderDT({
    req(selected_2d_plot_code())
    results <- correlations_2d_results()$results_table
    shiny::validate(shiny::need(nrow(results) > 0, "No correlations found for these parameters."))
    DT::datatable(
      results,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        dom = "t",
        autoWidth = TRUE
      ),
      class = "compact stripe hover"
    )
  })

  output$plots <- renderUI({
    req(selected_2d_plot_code())
    results <- correlations_2d_results()
    texthtml <- paste(nrow(results$results_table), " correlations found within those parameters")
    output$plotLoadingIndicator <- renderUI({
      h4(texthtml, style = "text-align: center;", br(), br())
    })
    if (length(results$plots_list) == 0) {
      return(tagList())
    }
    plots_with_spacers <- list()
    for (index in seq_along(results$plots_list)) {
      plots_with_spacers[[length(plots_with_spacers) + 1]] <- results$plots_list[[index]]
      if (index != length(results$plots_list)) {
        plots_with_spacers[[length(plots_with_spacers) + 1]] <- div(style = "margin-top: 40px;")
      }
    }
    do.call(tagList, plots_with_spacers)
  })

  ####### TAB 5) MACHINE LEARNING
  all_models_reactive <- reactiveVal(list())
  output$ml_table_results_output <- DT::renderDT({
    ml_results <- ml_table_results()
    ml_results_datatable(ml_results)
  })

  ml_results_datatable <- function(ml_results) {
    if (!is.data.frame(ml_results)) {
      ml_results <- data.frame()
    }
    if (ncol(ml_results) == 0) {
      return(datatable(
        data.frame(Status = "No machine learning result rows loaded yet.", check.names = FALSE),
        options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, info = FALSE),
        rownames = FALSE
      ))
    }
    priority_columns <- c(
      "Model", "R2", "Accuracy", "MAE", "RMSE",
      "dataset_seed", "training_seed", "threshold_scope", "imputation_scope",
      "elapsed_seconds", "Status", "Error"
    )
    ordered_columns <- c(intersect(priority_columns, names(ml_results)), setdiff(names(ml_results), priority_columns))
    ml_results <- ml_results[, ordered_columns, drop = FALSE]
    for (metric_column in intersect(c("R2", "MAE", "RMSE"), names(ml_results))) {
      metric_values <- suppressWarnings(as.numeric(ml_results[[metric_column]]))
      ml_results[[metric_column]] <- ifelse(
        is.finite(metric_values),
        format(round(metric_values, 3), nsmall = 3, trim = TRUE),
        ""
      )
    }
    error_column <- match("Error", names(ml_results)) - 1
    column_defs <- list()
    if (!is.na(error_column)) {
      column_defs <- list(
        list(targets = error_column, width = "420px", className = "dt-error-column")
      )
    }
    datatable(ml_results,
      options = list(
        lengthChange = FALSE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = column_defs
      ),
      rownames = FALSE)
  }

  build_remote_ml_config <- function() {
    req(input$ml_target)
    list(
      runner = "ugplot_run_ml_job",
      job_name = input$remote_job_name %||% "",
      target = input$ml_target,
      category_columns = input$checkbox_group_categories %||% character(0),
      models = input$ml_checkbox_group %||% character(0),
      dataset_seed_start = input$ml_dataset_seedi %||% 1,
      dataset_seed_end = input$ml_dataset_seedf %||% 1,
      training_seed_start = input$ml_seedi %||% 1,
      training_seed_end = input$ml_seedf %||% 1,
      timeout = input$ml_timeout %||% 1200,
      missing_definition = input$ml_missing_definition %||% c("empty", "na"),
      zero_exceptions = input$ml_zero_exceptions %||% character(0),
      missing_strategy = input$ml_missing_strategy %||% "none",
      imputation_scope = input$ml_imputation_scope %||% "split_separate",
      missing_threshold_cols = input$ml_missing_threshold_cols %||% 100,
      missing_threshold_rows = input$ml_missing_threshold_rows %||% 100,
      missing_filter_order = "auto",
      complete_case_min_samples = input$ml_complete_case_min_samples %||% 80,
      performance_mode = input$ml_performance_mode %||% "default",
      cv_method = input$ml_cv_method %||% "cv",
      cv_folds = input$ml_cv_folds %||% 10,
      cv_repeats = input$ml_cv_repeats %||% 1,
      tune_length = input$ml_tune_length %||% 3,
      auto_skip_bad_models = isTRUE(input$ml_auto_skip_bad_models),
      min_r2_skip = input$ml_min_r2_skip %||% 0,
      cpu_limit = selected_remote_cpu_limit(),
      parallel_enabled = isTRUE(input$config_parallel_cubist_models),
      use_callr_timeout = TRUE,
      restart_parallel_each_model = isTRUE(input$config_restart_parallel_each_model),
      retry_parallel_connection_errors = isTRUE(input$config_retry_parallel_connection_errors)
    )
  }

  current_remote_ml_dataset <- function() {
    req(is.data.frame(changed_table), nrow(changed_table) > 0)
    selected_rows <- input$row_checkbox_group %||% rownames(changed_table)
    selected_columns <- input$column_checkbox_group %||% names(changed_table)
    req(input$ml_target %in% selected_columns)
    changed_table[selected_rows, selected_columns, drop = FALSE]
  }

  remote_server_by_name <- function(server_name = NULL) {
    servers <- remote_servers()
    if (!is.data.frame(servers) || nrow(servers) == 0) {
      servers <- ugplot_default_remote_servers()
    }
    if (is.null(server_name) || !nzchar(server_name %||% "")) {
      return(selected_remote_server())
    }
    server <- servers[servers$name == server_name, , drop = FALSE]
    if (nrow(server) != 1) {
      stop("Remote server not found: ", server_name, call. = FALSE)
    }
    server
  }

  remote_job_action_key <- function(server_name, job_id) {
    paste(server_name %||% "", job_id %||% "", sep = "||")
  }

  parse_remote_job_action_key <- function(value) {
    parts <- strsplit(value %||% "", "||", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      return(list(server = parts[[1]], job_id = paste(parts[-1], collapse = "||")))
    }
    list(server = "", job_id = value %||% "")
  }

  remember_remote_job_server <- function(job_id, server_name) {
    if (nzchar(job_id %||% "") && nzchar(server_name %||% "")) {
      remote_selected_job(list(id = as.character(job_id), server = as.character(server_name)))
    }
    invisible(remote_selected_job())
  }

  remote_server_name_for_job <- function(job_id) {
    selected_job <- remote_selected_job()
    if (is.list(selected_job) &&
        identical(as.character(selected_job$id %||% ""), as.character(job_id %||% "")) &&
        nzchar(selected_job$server %||% "")) {
      return(as.character(selected_job$server))
    }
    jobs <- remote_jobs()
    if (!is.data.frame(jobs) || nrow(jobs) == 0 || !"id" %in% names(jobs) || !"server" %in% names(jobs)) {
      return(selected_remote_server()$name[[1]])
    }
    match_index <- which(as.character(jobs$id) == as.character(job_id))[1]
    if (length(match_index) == 1 && !is.na(match_index)) {
      return(as.character(jobs$server[[match_index]]))
    }
    selected_remote_server()$name[[1]]
  }

	  refresh_remote_jobs <- function() {
    servers <- remote_servers()
    if (!is.data.frame(servers) || nrow(servers) == 0) {
      servers <- ugplot_default_remote_servers()
    }

    all_jobs <- list()
    capabilities_by_server <- list()
    server_connection_rows <- list()
    add_server_connection_row <- function(row) {
      server_connection_rows[[length(server_connection_rows) + 1L]] <<- row
    }

    for (i in seq_len(nrow(servers))) {
      server <- servers[i, , drop = FALSE]
      server_name <- server$name[[1]]
      server_jobs <- tryCatch({
        health <- ugplot_remote_health(server_url = server$url, token = server$token %||% "")
        capabilities_by_server[[server_name]] <- health$capabilities %||% list()
        remote_version <- as.character(health$ugplot_build_version %||% "")
        local_version <- ugplot_build_version()
        version_matches <- identical(ugplot_compare_build_versions(local_version, remote_version), 0L)
        version_message <- if (isTRUE(version_matches)) "" else ugplot_version_mismatch_message(local_version, remote_version)
        jobs <- ugplot_remote_list_jobs(
          server_url = server$url,
          token = server$token %||% ""
        )
        if (!is.data.frame(jobs) || nrow(jobs) == 0) {
          jobs <- data.frame(server = character(0), stringsAsFactors = FALSE)
        } else {
          jobs$server <- server_name
          # Keep the jobs refresh lightweight. Detailed status, result previews,
          # and progress estimates are loaded only when a job is selected.
        }
        active_count <- if (is.data.frame(jobs) && nrow(jobs) > 0 && "state" %in% names(jobs)) {
          sum(as.character(jobs$state) %in% c("queued", "running"), na.rm = TRUE)
        } else {
          0L
        }
        resources <- health$resources %||% list()
        add_server_connection_row(data.frame(
          server = server_name,
          state = if (!isTRUE(version_matches)) "version_mismatch" else if (active_count > 0) "active" else "idle",
          jobs = nrow(jobs),
          active = active_count,
          message = if (!isTRUE(version_matches)) version_message else if (active_count > 0) paste(active_count, "active") else "idle",
          interface_version = local_version,
          server_version = if (nzchar(remote_version)) remote_version else NA_character_,
          cpu_pct = suppressWarnings(as.numeric(resources$process_cpu_pct %||% NA_real_)),
          cpu_count = suppressWarnings(as.numeric(resources$host_cpu_count %||% NA_real_)),
          load1 = suppressWarnings(as.numeric(resources$host_load1 %||% NA_real_)),
          memory_pct = suppressWarnings(as.numeric(resources$host_mem_used_pct %||% NA_real_)),
          memory_available_mb = suppressWarnings(as.numeric(resources$host_mem_available_mb %||% NA_real_)),
          memory_total_mb = suppressWarnings(as.numeric(resources$host_mem_total_mb %||% NA_real_)),
          disk_pct = suppressWarnings(as.numeric(resources$disk_used_pct %||% NA_real_)),
          task = paste(as.character(resources$tasks %||% character(0)), collapse = " / "),
          stringsAsFactors = FALSE
        ))
        jobs
      }, error = function(e) {
        capabilities_by_server[[server_name]] <- list()
        add_server_connection_row(data.frame(
          server = server_name,
          state = "offline",
          jobs = NA_integer_,
          active = NA_integer_,
          message = conditionMessage(e),
          interface_version = ugplot_build_version(),
          server_version = NA_character_,
          cpu_pct = NA_real_,
          cpu_count = NA_real_,
          load1 = NA_real_,
          memory_pct = NA_real_,
          memory_available_mb = NA_real_,
          memory_total_mb = NA_real_,
          disk_pct = NA_real_,
          task = "",
          stringsAsFactors = FALSE
        ))
        data.frame()
      })
      if (is.data.frame(server_jobs) && nrow(server_jobs) > 0) {
        all_jobs[[length(all_jobs) + 1L]] <- server_jobs
      }
    }

    jobs <- if (length(all_jobs) > 0) {
      all_columns <- unique(unlist(lapply(all_jobs, names), use.names = FALSE))
      normalized_jobs <- lapply(all_jobs, function(job_data) {
        for (column_name in setdiff(all_columns, names(job_data))) {
          job_data[[column_name]] <- NA
        }
        job_data[, all_columns, drop = FALSE]
      })
      do.call(rbind, normalized_jobs)
    } else {
      data.frame()
    }
    if (is.data.frame(jobs) && nrow(jobs) > 0) {
      preferred_columns <- c("server", "id", "name", "type", "state", "progress", "message", "execution", "tasks", "target", "models", "created_at", "updated_at", "pid")
      jobs <- jobs[, c(intersect(preferred_columns, names(jobs)), setdiff(names(jobs), preferred_columns)), drop = FALSE]
      state_values <- if ("state" %in% names(jobs)) as.character(jobs$state) else rep("", nrow(jobs))
      active_rank <- ifelse(state_values %in% c("queued", "running"), 0L, 1L)
      updated_values <- if ("updated_at" %in% names(jobs)) {
        suppressWarnings(as.POSIXct(as.character(jobs$updated_at), format = "%Y-%m-%d %H:%M:%S %z"))
      } else {
        rep(as.POSIXct(NA), nrow(jobs))
      }
      jobs <- jobs[order(active_rank, updated_values, decreasing = c(FALSE, TRUE), na.last = TRUE), , drop = FALSE]
    }
    remote_server_capabilities(capabilities_by_server)
    remote_server_connection_state(if (length(server_connection_rows) > 0) do.call(rbind, server_connection_rows) else data.frame())
	    remote_jobs(jobs)
	    invisible(jobs)
	  }

	  build_remote_geo_config <- function() {
	    remote_result <- remote_job_preview_result()
	    remote_loaded <- is.list(remote_result) && identical(remote_result$kind %||% "", "geo_pipeline")
	    accession <- trimws(input$geo_accession %||% "")
	    if (!nzchar(accession) && isTRUE(remote_loaded)) {
	      accession <- trimws(as.character(remote_result$accession %||% ""))
	    }
	    if (!nzchar(accession)) {
	      stop("Enter a GEO accession before starting a remote GEO pipeline.", call. = FALSE)
	    }
	    metadata <- geo_sample_metadata()
	    if ((!is.data.frame(metadata) || nrow(metadata) == 0) && !isTRUE(remote_loaded)) {
	      stop("Fetch sample metadata locally before starting a remote GEO pipeline.", call. = FALSE)
	    }
	    candidates <- if (is.data.frame(metadata) && nrow(metadata) > 0) ugplot_geo_target_candidates(metadata) else character(0)
	    target_column <- trimws(input$geo_target_column %||% "")
	    if ((!nzchar(target_column) || (length(candidates) > 0 && !(target_column %in% candidates))) &&
	        isTRUE(remote_loaded)) {
	      remote_target <- trimws(as.character(remote_result$target_column %||% ""))
	      if (nzchar(remote_target) && (length(candidates) == 0 || remote_target %in% candidates)) {
	        target_column <- remote_target
	      }
	    }
	    if (!nzchar(target_column) || (length(candidates) > 0 && !(target_column %in% candidates))) {
	      stop("Choose a metadata field locally before starting a remote GEO pipeline.", call. = FALSE)
	    }
	    server <- selected_geo_remote_server()
	    distributed_workers <- list()
	    if (isTRUE(input$geo_distributed_screening)) {
	      selected_workers <- unique(as.character(input$geo_distributed_worker_names %||% character(0)))
	      states <- remote_server_connection_state()
	      capabilities <- remote_server_capabilities()
	      configured_servers <- remote_servers()
	      valid_workers <- selected_workers[
	        vapply(selected_workers, function(server_name) {
	          state_row <- states[as.character(states$server) == server_name, , drop = FALSE]
	          nrow(state_row) == 1L &&
	            state_row$state[[1]] %in% c("idle", "active") &&
	            isTRUE(capabilities[[server_name]]$distributed_geo_screening %||% FALSE) &&
	            identical(as.integer(capabilities[[server_name]]$distributed_protocol_version %||% 0L), 1L)
	        }, logical(1))
	      ]
	      distributed_workers <- lapply(valid_workers, function(server_name) {
	        worker <- configured_servers[as.character(configured_servers$name) == server_name, , drop = FALSE]
	        worker_cpu_limit <- suppressWarnings(as.integer(worker$cpu_limit[[1]] %||% 1L))
	        if (is.na(worker_cpu_limit) || worker_cpu_limit < 1L) {
	          worker_cpu_limit <- 1L
	        }
	        list(
	          name = as.character(worker$name[[1]]),
	          url = as.character(worker$url[[1]]),
	          token = as.character(worker$token[[1]] %||% ""),
	          cpu_limit = worker_cpu_limit,
	          protocol_version = 1L
	        )
	      })
	    }
	    matrix_source <- geo_matrix_source_value(input$geo_matrix_source %||% "processed")
	    if (isTRUE(remote_loaded)) {
	      remote_source <- as.character(remote_result$matrix_source %||% "")
	      if (nzchar(remote_source)) {
	        matrix_source <- geo_matrix_source_value(remote_source)
	      }
	    }
	    loaded_remote_job_id <- if (isTRUE(remote_loaded)) {
	      geo_remote_pipeline_job_id() %||% ""
	    } else {
	      ""
	    }
	    list(
	      runner = "ugplot_run_geo_pipeline_job",
	      type = "geo",
	      job_name = paste("GEO", accession, matrix_source, "rho", input$geo_transcript_absrho_threshold %||% 0.8),
	      accession = accession,
	      matrix_source = matrix_source,
	      target_column = target_column,
	      resume_cached_geo = TRUE,
	      resume_from_job_id = loaded_remote_job_id,
	      spearman_max_cpgs = input$geo_spearman_max_cpgs %||% 0,
	      spearman_min_samples_pct = input$geo_spearman_min_samples %||% 80,
	      transcript_absrho_threshold = input$geo_transcript_absrho_threshold %||% 0.8,
	      transcript_min_samples = input$geo_transcript_min_samples %||% 80,
	      idat_detection_p = input$geo_idat_detection_p %||% 0.05,
	      idat_max_failed_fraction = input$geo_idat_max_failed_fraction %||% 0.05,
	      idat_sesame_prep = input$geo_idat_sesame_prep %||% "QCDPB",
	      geo_ml_min_absrho = input$geo_ml_min_absrho %||% 0.7,
	      geo_ml_rank_limit = input$geo_ml_rank_limit %||% NA_integer_,
	      geo_ml_quick_models = isTRUE(input$geo_ml_quick_models),
	      geo_ml_screen_seeds = input$geo_ml_screen_seeds %||% 3,
	      geo_ml_timeout = input$geo_ml_timeout %||% 1200,
	      geo_ml_min_stability_seeds = input$geo_ml_min_stability_seeds %||% 30,
	      geo_ml_max_stability_seeds = input$geo_ml_max_stability_seeds %||% 4000,
	      geo_ml_stability_window = input$geo_ml_stability_window %||% 30,
	      geo_ml_stability_tolerance = input$geo_ml_stability_tolerance %||% 0.01,
	      geo_ml_stability_group_column = input$geo_ml_stability_group_column %||% "",
	      models = input$ml_checkbox_group %||% character(0),
	      cpu_limit = selected_remote_cpu_limit(server),
	      parallel_enabled = isTRUE(input$config_parallel_cubist_models),
	      restart_parallel_each_model = isTRUE(input$config_restart_parallel_each_model),
	      retry_parallel_connection_errors = isTRUE(input$config_retry_parallel_connection_errors),
	      distributed_workers = distributed_workers,
	      distributed_protocol_version = 1L,
	      timeout = 0
	    )
	  }

	  reset_geo_remote_loaded_state <- function(clear_files = TRUE) {
	    if (isTRUE(clear_files)) {
	      geo_files(data.frame())
	      geo_remote_files(data.frame())
	    }
	    geo_sample_metadata(data.frame())
	    geo_cpg_annotation(data.frame())
	    geo_spearman_raw_results(data.frame())
	    geo_spearman_results(data.frame())
	    geo_transcript_candidates(data.frame())
	    geo_transcript_groups(data.frame())
	    geo_transcript_group_details(data.frame())
	    geo_transcript_ml_results(data.frame())
	    geo_idat_qc_report(data.frame())
	    geo_preview_data(data.frame())
	    update_geo_transcript_build_progress(
	      phase = "idle",
	      message = "Remote GEO job selected. Load a remote result or refresh local GEO status for local cache details.",
	      processed = 0L,
	      total = 0L,
	      compatible = 0L,
	      excluded = 0L,
	      current = "",
	      cache = ""
	    )
	    update_geo_transcript_ml_progress(
	      phase = "idle",
	      message = "Remote GEO job selected. Transcript ML outputs remain on the remote server until a result is loaded.",
	      processed = 0L,
	      total = 0L,
	      current = "",
	      cache = ""
	    )
	    geo_idat_qc_progress(list(
	      phase = "idle",
	      message = "Remote GEO job selected. IDAT/sesame status is remote unless local status is refreshed.",
	      processed = 0L,
	      total = 0L,
	      current = "",
	      beta_path = "",
	      qc_path = ""
	    ))
	  }

	  apply_remote_geo_config <- function(config, job_id = "", server = NULL, status = NULL) {
	    if (!remote_config_is_geo(config)) {
	      return(invisible(FALSE))
	    }
	    reset_geo_remote_loaded_state(clear_files = TRUE)
	    accession <- trimws(as.character(config$accession %||% ""))
	    matrix_source <- as.character(config$matrix_source %||% "")
	    target_column <- as.character(config$target_column %||% "")
	    update_geo_numeric_input <- function(input_id, value) {
	      numeric_value <- suppressWarnings(as.numeric(value))
	      if (length(numeric_value) > 0 && is.finite(numeric_value[[1]])) {
	        updateNumericInput(session, input_id, value = numeric_value[[1]])
	      }
	    }
	    if (nzchar(accession)) {
	      updateTextInput(session, "geo_accession", value = accession)
	    }
	    if (nzchar(matrix_source)) {
	      updateSelectInput(session, "geo_matrix_source", selected = matrix_source)
	    }
	    if (nzchar(target_column)) {
	      updateSelectInput(session, "geo_target_column", selected = target_column)
	    }
	    update_geo_numeric_input("geo_spearman_max_cpgs", config$spearman_max_cpgs)
	    update_geo_numeric_input("geo_spearman_min_samples", config$spearman_min_samples_pct)
	    update_geo_numeric_input("geo_transcript_absrho_threshold", config$transcript_absrho_threshold)
	    update_geo_numeric_input("geo_transcript_min_samples", config$transcript_min_samples)
	    update_geo_numeric_input("geo_idat_detection_p", config$idat_detection_p)
	    update_geo_numeric_input("geo_idat_max_failed_fraction", config$idat_max_failed_fraction)
	    if (!is.null(config$idat_sesame_prep)) {
	      prep <- as.character(config$idat_sesame_prep %||% "")
	      if (nzchar(prep)) {
	        updateTextInput(session, "geo_idat_sesame_prep", value = prep)
	      }
	    }
	    update_geo_numeric_input("geo_ml_min_absrho", config$geo_ml_min_absrho)
	    update_geo_numeric_input("geo_ml_rank_limit", config$geo_ml_rank_limit)
	    update_geo_numeric_input("geo_ml_screen_seeds", config$geo_ml_screen_seeds)
	    update_geo_numeric_input("geo_ml_timeout", config$geo_ml_timeout)
	    update_geo_numeric_input("geo_ml_min_stability_seeds", config$geo_ml_min_stability_seeds)
	    update_geo_numeric_input("geo_ml_max_stability_seeds", config$geo_ml_max_stability_seeds)
	    update_geo_numeric_input("geo_ml_stability_window", config$geo_ml_stability_window)
	    update_geo_numeric_input("geo_ml_stability_tolerance", config$geo_ml_stability_tolerance)
	    if (!is.null(config$distributed_workers)) {
	      worker_names <- vapply(config$distributed_workers, function(worker) {
	        as.character(worker$name %||% "")
	      }, character(1))
	      worker_names <- worker_names[nzchar(worker_names)]
	      updateCheckboxInput(session, "geo_distributed_screening", value = length(worker_names) > 0)
	      if (length(worker_names) > 0) {
	        updateCheckboxGroupInput(session, "geo_distributed_worker_names", selected = worker_names)
	      }
	    }
	    if (!is.null(config$geo_ml_stability_group_column)) {
	      updateSelectInput(session, "geo_ml_stability_group_column", selected = as.character(config$geo_ml_stability_group_column %||% ""))
	    }
	    geo_remote_pipeline_job_id(job_id %||% geo_remote_pipeline_job_id())
	    status_text <- remote_status_summary_text(status)
	    if (!nzchar(status_text)) {
	      status_text <- paste("Remote GEO job selected:", job_id)
	    }
	    server_label <- if (is.data.frame(server) && nrow(server) > 0) paste0(" on ", server$name[[1]]) else ""
	    geo_remote_pipeline_status(status_text)
	    geo_stage(list(
	      step = "Remote GEO",
	      title = "Remote pipeline selected",
	      message = paste0(
	        "Selected remote GEO job",
	        if (nzchar(job_id %||% "")) paste0(" ", job_id) else "",
	        server_label,
	        ". Accession/source/target were loaded from the saved remote config; files and pipeline outputs remain on the remote server unless you refresh local GEO status."
	      )
	    ))
	    invisible(TRUE)
	  }

	  apply_remote_geo_result <- function(result, job_id = "", clear_existing = TRUE, update_inputs = TRUE) {
	    if (!is.list(result) || !identical(result$kind %||% "", "geo_pipeline")) {
	      stop("Remote job result is not a GEO pipeline result.", call. = FALSE)
	    }
	    remote_geo_result_applying(TRUE)
	    session$onFlushed(function() {
	      session$onFlushed(function() {
	        remote_geo_result_applying(FALSE)
	      }, once = TRUE)
	    }, once = TRUE)
	    remote_job_preview_result(result)
	    if (isTRUE(clear_existing)) {
	      reset_geo_remote_loaded_state(clear_files = TRUE)
	    }
	    if (is.data.frame(result$tables$remote_files)) {
	      geo_remote_files(result$tables$remote_files)
	    }
	    if (is.data.frame(result$tables$metadata_preview)) {
	      geo_sample_metadata(result$tables$metadata_preview)
	    }
	    if (is.data.frame(result$tables$spearman_preview)) {
	      geo_spearman_raw_results(result$tables$spearman_preview)
	      geo_spearman_results(result$tables$spearman_preview)
	    }
	    if (is.data.frame(result$tables$transcript_groups)) {
	      geo_transcript_groups(result$tables$transcript_groups)
	    }
	    if (is.data.frame(result$tables$transcript_group_details)) {
	      geo_transcript_group_details(result$tables$transcript_group_details)
	    }
	    if (is.data.frame(result$tables$transcript_candidates_preview)) {
	      geo_transcript_candidates(result$tables$transcript_candidates_preview)
	    }
	    if (is.data.frame(result$tables$transcript_groups) && nrow(result$tables$transcript_groups) > 0) {
	      detail_rows <- if (is.data.frame(result$tables$transcript_group_details)) nrow(result$tables$transcript_group_details) else 0L
	      update_geo_transcript_build_progress(
	        phase = "loaded from remote",
	        message = paste0("Loaded remote transcript ML groups: ", nrow(result$tables$transcript_groups), " group(s). Large artifacts remain on the remote server."),
	        processed = nrow(result$tables$transcript_groups),
	        total = nrow(result$tables$transcript_groups),
	        compatible = nrow(result$tables$transcript_groups),
	        excluded = 0L,
	        current = "",
	        cache = result$paths$transcript_group_summary %||% "",
	        detail = if (detail_rows > 0) paste0(detail_rows, " transcript detail row(s) loaded from remote.") else NULL
	      )
	    }
	    if ((!is.data.frame(result$tables$transcript_groups) || nrow(result$tables$transcript_groups) == 0) &&
	        (is.data.frame(result$tables$spearman_preview) && nrow(result$tables$spearman_preview) > 0)) {
	      threshold_label <- result$settings$transcript_absrho_threshold %||% "selected threshold"
	      update_geo_transcript_build_progress(
	        phase = "no candidates",
	        message = paste0("No transcript candidates were found for |rho| >= ", threshold_label, ". Lower the transcript CpG threshold and rerun the remote GEO pipeline to run steps 8-10."),
	        processed = 0L,
	        total = 0L,
	        compatible = 0L,
	        excluded = 0L,
	        current = "",
	        cache = result$paths$spearman_by_transcript %||% ""
	      )
	    }
	    ml_summary <- result$tables$transcript_ml_summary
	    if (!is.data.frame(ml_summary) || nrow(ml_summary) == 0) {
	      ml_summary <- result$tables$transcript_ml_screening
	    }
	    if (is.data.frame(ml_summary) && nrow(ml_summary) > 0) {
	      geo_transcript_ml_results(ml_summary)
	      update_geo_transcript_ml_progress(
	        phase = "loaded from remote",
	        message = paste0("Loaded remote transcript ML summary: ", nrow(ml_summary), " row(s). Large artifacts remain on the remote server."),
	        processed = nrow(ml_summary),
	        total = nrow(ml_summary),
	        current = "",
	        cache = result$paths$transcript_ml_summary %||% result$paths$transcript_ml_screening_summary %||% ""
	      )
	    }
	    if (is.data.frame(result$tables$idat_qc)) {
	      geo_idat_qc_report(result$tables$idat_qc)
	    }
	    if (nzchar(result$paths$sesame_beta %||% "") || nzchar(result$paths$sesame_qc %||% "")) {
	      geo_idat_qc_progress(list(
	        phase = "loaded from remote",
	        message = "Loaded remote sesame paths from the selected GEO job.",
	        processed = if (is.data.frame(result$tables$idat_qc)) nrow(result$tables$idat_qc) else 0L,
	        total = if (is.data.frame(result$tables$idat_qc)) nrow(result$tables$idat_qc) else 0L,
	        current = "",
	        beta_path = result$paths$sesame_beta %||% "",
	        qc_path = result$paths$sesame_qc %||% ""
	      ))
	    }
	    if (isTRUE(update_inputs)) {
	      accession <- result$accession %||% ""
	      if (nzchar(accession)) {
	        updateTextInput(session, "geo_accession", value = accession)
	      }
	      if (nzchar(result$matrix_source %||% "")) {
	        updateSelectInput(session, "geo_matrix_source", selected = result$matrix_source)
	      }
	      if (nzchar(result$target_column %||% "")) {
	        updateSelectInput(session, "geo_target_column", selected = result$target_column)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$transcript_absrho_threshold)) {
	        updateNumericInput(session, "geo_transcript_absrho_threshold", value = result$settings$transcript_absrho_threshold)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$transcript_min_samples)) {
	        updateNumericInput(session, "geo_transcript_min_samples", value = result$settings$transcript_min_samples)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$spearman_max_cpgs)) {
	        updateNumericInput(session, "geo_spearman_max_cpgs", value = result$settings$spearman_max_cpgs)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$spearman_min_samples_pct)) {
	        updateNumericInput(session, "geo_spearman_min_samples", value = result$settings$spearman_min_samples_pct)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$geo_ml_min_absrho)) {
	        updateNumericInput(session, "geo_ml_min_absrho", value = result$settings$geo_ml_min_absrho)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$geo_ml_rank_limit)) {
	        updateNumericInput(session, "geo_ml_rank_limit", value = result$settings$geo_ml_rank_limit)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$geo_ml_screen_seeds)) {
	        updateNumericInput(session, "geo_ml_screen_seeds", value = result$settings$geo_ml_screen_seeds)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$geo_ml_timeout)) {
	        updateNumericInput(session, "geo_ml_timeout", value = result$settings$geo_ml_timeout)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$geo_ml_min_stability_seeds)) {
	        updateNumericInput(session, "geo_ml_min_stability_seeds", value = result$settings$geo_ml_min_stability_seeds)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$geo_ml_max_stability_seeds)) {
	        updateNumericInput(session, "geo_ml_max_stability_seeds", value = result$settings$geo_ml_max_stability_seeds)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$geo_ml_stability_window)) {
	        updateNumericInput(session, "geo_ml_stability_window", value = result$settings$geo_ml_stability_window)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$geo_ml_stability_tolerance)) {
	        updateNumericInput(session, "geo_ml_stability_tolerance", value = result$settings$geo_ml_stability_tolerance)
	      }
	      if (is.list(result$settings) && !is.null(result$settings$geo_ml_stability_group_column)) {
	        updateSelectInput(session, "geo_ml_stability_group_column", selected = as.character(result$settings$geo_ml_stability_group_column %||% ""))
	      }
	    }
	    geo_remote_pipeline_job_id(job_id %||% geo_remote_pipeline_job_id())
	    geo_remote_pipeline_status(paste0(
	      "Remote GEO result loaded",
	      if (nzchar(job_id %||% "")) paste0(" from job ", job_id) else "",
	      ". Remote cache: ", result$cache_dir %||% ""
	    ))
	    geo_status(ugplot_geo_append_log(geo_status(), geo_remote_pipeline_status()))
	    geo_stage(list(
	      step = "Remote GEO",
	      title = "Remote result loaded",
	      message = paste0("Pipeline outputs remain on the remote server cache: ", result$cache_dir %||% "")
	    ))
	    session$onFlushed(function() {
	      updateTabsetPanel(session, "tabs", selected = "GEO IMPORT")
	    }, once = TRUE)
	    invisible(result)
	  }

	  refresh_remote_geo_pipeline_status <- function(job_id = geo_remote_pipeline_job_id(), server_name = NULL) {
	    if (!nzchar(job_id %||% "")) {
	      stop("No remote GEO job id is selected.", call. = FALSE)
	    }
	    server <- remote_server_by_name(server_name %||% remote_server_name_for_job(job_id))
	    remember_remote_job_server(job_id, as.character(server$name[[1]]))
	    activate_geo_remote_server_for_job(server)
	    status <- ugplot_remote_job_status(
	      server_url = server$url,
	      job_id = job_id,
	      token = server$token %||% ""
	    )
	    geo_remote_pipeline_status(remote_status_summary_text(status))
	    remote_job_preview_status(status)
	    updateTextInput(session, "remote_job_id", value = job_id)
	    invisible(status)
	  }

	  submit_remote_geo_pipeline <- function(mode = "start") {
	    config <- build_remote_geo_config()
	    server <- selected_geo_remote_server()
	    submit_message <- if (identical(mode, "cached_continue")) {
	      paste0(
	        "Submitting cached GEO continuation on ", server$name[[1]],
	        " for ", config$accession,
	        " with |rho| >= ", config$transcript_absrho_threshold, "."
	      )
	    } else {
	      paste0("Submitting remote GEO pipeline on ", server$name[[1]], ".")
	    }
	    geo_remote_pipeline_status(submit_message)
	    remote_job_status_text(submit_message)
	    showNotification(submit_message, type = "message", duration = 5)
	    started <- ugplot_remote_create_job(
	      server_url = server$url,
	      dataset = data.frame(geo_pipeline = TRUE),
	      config = config,
	      token = server$token %||% ""
	    )
	    job_id <- started$id %||% ""
	    geo_remote_pipeline_job_id(job_id)
	    updateTextInput(session, "remote_job_id", value = job_id)
	    geo_run_target_state("remote")
	    updateRadioButtons(session, "geo_run_target", selected = "remote")
	    refresh_geo_remote_server_inputs(selected = server$name[[1]])
	    geo_remote_pipeline_status(paste("Remote GEO pipeline submitted:", job_id))
	    remote_job_status_text(geo_remote_pipeline_status())
	    showNotification(paste("Remote GEO pipeline submitted:", job_id), type = "message", duration = 8)
	    tryCatch(
	      refresh_remote_jobs(),
	      error = function(e) remote_job_status_text(paste("Remote GEO pipeline submitted:", job_id, "- job list refresh failed:", conditionMessage(e)))
	    )
	    try(updateTabsetPanel(session, "tabs", selected = "JOBS"), silent = TRUE)
	    invisible(started)
	  }

	  remote_server_supports <- function(capability, server_name = NULL) {
    capabilities <- remote_server_capabilities()
    if (!is.null(server_name) && nzchar(server_name %||% "") && is.list(capabilities[[server_name]])) {
      return(isTRUE(capabilities[[server_name]][[capability]] %||% FALSE))
    }
    if (is.null(server_name)) {
      selected_name <- selected_remote_server()$name[[1]]
      if (is.list(capabilities[[selected_name]])) {
        return(isTRUE(capabilities[[selected_name]][[capability]] %||% FALSE))
      }
    }
    isTRUE(capabilities[[capability]] %||% FALSE)
  }

  remote_job_progress_label <- function(progress, status = NULL) {
    progress_value <- suppressWarnings(as.numeric(progress))
    if (length(progress_value) == 0 || !is.finite(progress_value[[1]])) {
      return("N/A")
    }
    label <- paste0(round(max(0, min(1, progress_value[[1]])) * 100), "%")
    if (is.list(status) &&
        identical(remote_status_scalar(status$type), "geo") &&
        grepl("^Stability[[:space:]]+", remote_status_scalar(status$message))) {
      return("Stability running")
    }
    label
  }

  remote_status_scalar <- function(value, default = "") {
    value <- unlist(value %||% default, use.names = FALSE)
    if (length(value) == 0 || is.na(value[[1]])) default else as.character(value[[1]])
  }

  remote_geo_stability_context <- function(status, result = NULL) {
    status_message <- remote_status_scalar(status$message)
    match <- regexec(
      "^Stability ([^[:space:]]+) / ([^:]+): (Running|Finished) ([^[:space:]]+)(?: dataset seed ([0-9]+) training seed ([0-9]+))?",
      status_message
    )
    parts <- regmatches(status_message, match)[[1]]
    if (length(parts) < 5) {
      return(list(is_stability = FALSE, label = ""))
    }

    context <- list(
      is_stability = TRUE,
      group_id = parts[[2]],
      stratum = parts[[3]],
      state = parts[[4]],
      model = parts[[5]],
      dataset_seed = if (length(parts) >= 6) parts[[6]] else "",
      training_seed = if (length(parts) >= 7) suppressWarnings(as.integer(parts[[7]])) else NA_integer_,
      task_index = NA_integer_,
      task_total = NA_integer_,
      min_seeds = NA_integer_,
      estimate_pct = NA_real_,
      label = "Stability running"
    )

    if (is.list(result) && identical(result$kind %||% "", "geo_pipeline") && is.list(result$tables)) {
      screening <- result$tables$transcript_ml_screening
      if (is.data.frame(screening) && nrow(screening) > 0 && "GroupID" %in% names(screening)) {
        ordered <- screening
        order_columns <- intersect(c("CombinedRank", "ModelRank", "RhoRank"), names(ordered))
        if (length(order_columns) > 0) {
          order_args <- lapply(order_columns, function(column_name) suppressWarnings(as.numeric(ordered[[column_name]])))
          ordered <- ordered[do.call(order, c(order_args, list(na.last = TRUE))), , drop = FALSE]
        }
        group_index <- match(context$group_id, as.character(ordered$GroupID))
        min_seeds <- suppressWarnings(as.integer(result$settings$geo_ml_min_stability_seeds %||% 30L))
        if (!is.finite(min_seeds) || min_seeds < 1L) {
          min_seeds <- 30L
        }
        if (is.finite(group_index)) {
          seed_fraction <- if (is.finite(context$training_seed)) min(1, max(0, context$training_seed / min_seeds)) else 0
          estimate <- 100 * ((group_index - 1) + seed_fraction) / nrow(ordered)
          context$task_index <- as.integer(group_index)
          context$task_total <- as.integer(nrow(ordered))
          context$min_seeds <- as.integer(min_seeds)
          context$estimate_pct <- estimate
          context$label <- paste0("~", round(estimate), "% stability")
        }
      }
    }

    context
  }

  remote_remember_progress_estimate <- function(job_id, status, result = NULL) {
    job_id <- remote_status_scalar(job_id)
    if (!nzchar(job_id)) {
      return(invisible(FALSE))
    }
    context <- remote_geo_stability_context(status, result)
    if (!isTRUE(context$is_stability)) {
      return(invisible(FALSE))
    }
    estimates <- remote_job_progress_estimates()
    estimates[[job_id]] <- context$label
    remote_job_progress_estimates(estimates)
    invisible(TRUE)
  }

  remote_latest_resource_message <- function(resources) {
    if (!is.data.frame(resources) || nrow(resources) == 0 || !"current_message" %in% names(resources)) {
      return("")
    }
    messages <- as.character(resources$current_message)
    messages <- messages[nzchar(messages) & !is.na(messages)]
    if (length(messages) == 0) "" else utils::tail(messages, 1L)
  }

  remote_status_with_live_message <- function(status, resources = NULL) {
    if (!is.list(status)) {
      return(status)
    }
    live_message <- remote_latest_resource_message(resources)
    if (nzchar(live_message) && grepl("^Stability[[:space:]]+", live_message)) {
      status$message <- live_message
    }
    status
  }

  remote_geo_stage_summary_text <- function(status, result = NULL) {
    status_message <- remote_status_scalar(status$message)
    is_geo <- identical(remote_status_scalar(status$type), "geo") ||
      (is.list(result) && identical(result$kind %||% "", "geo_pipeline"))
    if (!isTRUE(is_geo)) {
      return("")
    }

    lines <- character()
    stage <- if (is.list(result) && nzchar(result$stage %||% "")) result$stage else status_message
    if (nzchar(stage)) {
      lines <- c(lines, paste0("GEO stage: ", stage))
    }
    progress_path <- remote_status_scalar(status$partial_result_path)
    if (nzchar(progress_path)) {
      lines <- c(lines, "Saved partial GEO result: available")
    }

    stability <- remote_geo_stability_context(status, result)
    if (isTRUE(stability$is_stability)) {
      lines <- c(
        lines,
        paste0("Current stability task: ", stability$group_id, " / ", stability$stratum),
        paste0("Current model: ", stability$model,
               if (nzchar(stability$dataset_seed) && is.finite(stability$training_seed)) {
                 paste0(" | dataset seed ", stability$dataset_seed, " | training seed ", stability$training_seed)
               } else {
                 ""
               }),
        "TG order follows the screening CombinedRank, so TG numbers can move down after higher TG ids."
      )
    }

    if (is.list(result) && identical(result$kind %||% "", "geo_pipeline") && is.list(result$tables)) {
      screening <- result$tables$transcript_ml_screening
      summary <- result$tables$transcript_ml_summary
      if (is.data.frame(screening) && nrow(screening) > 0) {
        lines <- c(lines, paste0("Screening summary rows saved: ", nrow(screening)))
        if (isTRUE(stability$is_stability) && is.finite(stability$estimate_pct)) {
          lines <- c(lines, paste0(
            "Stability estimate: task ", stability$task_index, "/", stability$task_total,
            if (is.finite(stability$training_seed)) paste0(" | minimum-seed pass ", stability$training_seed, "/", stability$min_seeds) else "",
            " | lower-bound progress ~", round(stability$estimate_pct), "%"
          ))
        }
      }
      if (is.data.frame(summary) && nrow(summary) > 0 && "Phase" %in% names(summary)) {
        stability_rows <- sum(as.character(summary$Phase) == "stability", na.rm = TRUE)
        lines <- c(lines, paste0("Stability summary rows saved: ", stability_rows))
      }
    }

    paste(unique(lines[nzchar(lines)]), collapse = "\n")
  }

  remote_status_summary_text <- function(status) {
    if (!is.list(status)) {
      return("")
    }
    base <- paste0(
      "Job ", remote_status_scalar(status$id),
      " | pid: ", remote_status_scalar(status$pid, "N/A"),
      " | state: ", remote_status_scalar(status$state, "unknown"),
      " | progress: ", remote_job_progress_label(status$progress %||% NA_real_, status = status),
      " | ", remote_status_scalar(status$message)
    )
    geo_details <- remote_geo_stage_summary_text(status)
    if (nzchar(geo_details)) paste(base, geo_details, sep = "\n") else base
  }

  remote_status_has_result <- function(status) {
    if (!is.list(status)) {
      return(FALSE)
    }
    paths <- c(status$result_path %||% "", status$partial_result_path %||% "")
    any(nzchar(as.character(unlist(paths, use.names = FALSE))))
  }

  apply_remote_ml_result <- function(result, job_id) {
    remote_result_cache(result)
    remote_result_cache_job_id(job_id)
    loaded_rows <- 0L
    if (is.data.frame(result$results_table)) {
      ml_table_results(result$results_table)
      loaded_rows <- nrow(result$results_table)
    } else {
      ml_table_results(data.frame())
    }
    if (is.list(result$final_summary)) {
      ml_final_summary(result$final_summary)
    }
    best_model_object(result$best_model %||% NULL)
    best_model_preprocess(result$best_model_preprocess %||% NULL)
    if (!is.null(result$best_model) && nzchar(result$best_model_name %||% "")) {
      all_models_reactive(stats::setNames(list(result$best_model), result$best_model_name))
    }
    if (is.list(result$predictions)) {
      ml_prediction <<- result$predictions
    }
    remote_job_status_text(paste("Remote result loaded locally:", job_id, "-", loaded_rows, "result rows"))
    invisible(result)
  }

  remote_result_tested_models <- function(result) {
    if (!is.list(result) || !is.data.frame(result$results_table) || !("Model" %in% names(result$results_table))) {
      return(character(0))
    }
    models <- unique(as.character(result$results_table$Model))
    models <- models[nzchar(models) & !is.na(models)]
    intersect(models, ml_available)
  }

  activate_remote_server_for_job <- function(server) {
    server_name <- as.character(server$name[[1]])
    updateRadioButtons(session, "ml_run_target", selected = "remote")
    refresh_remote_server_inputs(selected = server_name)
    updateSelectInput(session, "remote_server_name", selected = server_name)
    model_deps <- ugplot_remote_model_deps(
      server_url = server$url,
      token = server$token %||% ""
    )
    load_ml_list(model_deps)
    ml_model_source_status_text(paste("Models loaded from remote server:", server_name))
    invisible(model_deps)
  }

  remote_config_is_geo <- function(config) {
    is.list(config) &&
      (
        identical(config$type %||% "", "geo") ||
          identical(config$runner %||% "", "ugplot_run_geo_pipeline_job")
      )
  }

  remote_status_is_geo <- function(status) {
    is.list(status) && identical(status$type %||% "", "geo")
  }

  remote_result_is_geo <- function(result) {
    is.list(result) && identical(result$kind %||% "", "geo_pipeline")
  }

  activate_geo_remote_server_for_job <- function(server) {
    server_name <- as.character(server$name[[1]])
    geo_run_target_state("remote")
    updateRadioButtons(session, "geo_run_target", selected = "remote")
    refresh_geo_remote_server_inputs(selected = server_name)
    refresh_remote_server_inputs(selected = server_name)
    updateSelectInput(session, "geo_remote_server_name", selected = server_name)
    updateSelectInput(session, "remote_server_name", selected = server_name)
    invisible(server)
  }

  load_remote_geo_job_locally <- function(job_id, server, status = NULL, result = NULL, config = NULL) {
    remember_remote_job_server(job_id, as.character(server$name[[1]]))
    activate_geo_remote_server_for_job(server)
    if (!is.list(status)) {
      status <- tryCatch(
        ugplot_remote_job_status(
          server_url = server$url,
          job_id = job_id,
          token = server$token %||% ""
        ),
        error = function(e) NULL
      )
    }
    remote_job_preview_status(status)
    geo_remote_pipeline_job_id(job_id)
    if (remote_config_is_geo(config)) {
      apply_remote_geo_config(config, job_id = job_id, server = server, status = status)
    }
    if (!remote_result_is_geo(result) && remote_status_has_result(status)) {
      result <- tryCatch(
        ugplot_remote_get_result(
          server_url = server$url,
          job_id = job_id,
          token = server$token %||% ""
        ),
        error = function(e) NULL
      )
    }
    if (remote_result_is_geo(result)) {
      remote_job_preview_result(result)
      remote_remember_progress_estimate(job_id, status, result)
      apply_remote_geo_result(
        result,
        job_id,
        clear_existing = identical(status$state %||% "finished", "finished")
      )
      remote_job_status_text(paste("Remote GEO result loaded:", job_id, "- large artifacts remain on", server$name[[1]]))
    } else {
      remote_job_preview_result(NULL)
      status_text <- remote_status_summary_text(status)
      if (!nzchar(status_text)) {
        status_text <- paste("Remote GEO job selected:", job_id)
      }
      if (!remote_config_is_geo(config)) {
        reset_geo_remote_loaded_state(clear_files = TRUE)
      }
      geo_remote_pipeline_status(status_text)
      remote_job_status_text(status_text)
    }
    session$onFlushed(function() {
      updateTabsetPanel(session, "tabs", selected = "GEO IMPORT")
    }, once = TRUE)
    invisible(result)
  }

  refresh_remote_job_preview <- function(job_id, switch_to_ml = FALSE, server_name = NULL) {
    req(nzchar(job_id %||% ""))
    server <- remote_server_by_name(server_name %||% remote_server_name_for_job(job_id))
    status <- ugplot_remote_job_status(
      server_url = server$url,
      job_id = job_id,
      token = server$token %||% ""
    )
    remote_job_preview_status(status)
    status_text <- remote_status_summary_text(status)
    if (remote_status_is_geo(status)) {
      activate_geo_remote_server_for_job(server)
      geo_remote_pipeline_job_id(job_id)
      geo_remote_pipeline_status(status_text)
    }

    if (remote_status_is_geo(status)) {
      remote_job_preview_result(NULL)
      active_geo <- remote_status_scalar(status$state, "unknown") %in% c("queued", "running")
      if (isTRUE(active_geo) && remote_status_has_result(status)) {
        result <- tryCatch(
          ugplot_remote_get_result(
            server_url = server$url,
            job_id = job_id,
            token = server$token %||% ""
          ),
          error = function(e) NULL
        )
        if (remote_result_is_geo(result)) {
          remote_job_preview_result(result)
          remote_remember_progress_estimate(job_id, status, result)
          status_text <- paste(status_text, "| partial GEO metadata loaded for progress estimate")
        } else {
          status_text <- paste(status_text, "| status only; use Load to open the saved GEO result")
        }
      } else {
        status_text <- paste(status_text, "| status only; use Load to open the saved GEO result")
      }
    } else if (remote_status_has_result(status)) {
      if (isTRUE(switch_to_ml)) {
        result <- ugplot_remote_get_result(
          server_url = server$url,
          job_id = job_id,
          token = server$token %||% ""
        )
        if (remote_result_is_geo(result)) {
          load_remote_geo_job_locally(job_id, server, status = status, result = result)
          return(invisible(status))
        } else {
          remote_job_preview_result(ugplot_job_result_preview(result))
          apply_remote_ml_result(result, job_id)
          updateTabsetPanel(session, "tabs", selected = "MACHINE LEARNING")
          status_text <- paste(status_text, "| full result loaded")
        }
      } else {
        result <- if (remote_server_supports("job_preview", server$name[[1]])) {
          ugplot_remote_get_job_preview(
            server_url = server$url,
            job_id = job_id,
            token = server$token %||% ""
          )
        } else {
          ugplot_job_result_preview(ugplot_remote_get_result(
            server_url = server$url,
            job_id = job_id,
            token = server$token %||% ""
          ))
        }
        remote_job_preview_result(result)
        status_text <- paste(status_text, "| preview loaded")
      }
    } else {
      remote_job_preview_result(NULL)
      status_text <- paste(status_text, "| no partial result yet")
    }

    log_text <- tryCatch(
      ugplot_remote_job_log(
        server_url = server$url,
        job_id = job_id,
        token = server$token %||% "",
        max_lines = 120L
      ),
      error = function(e) ""
    )
    remote_job_log_text(log_text)
    resources <- if (remote_server_supports("job_resource_monitor", server$name[[1]])) {
      tryCatch(
        ugplot_remote_job_resources(
          server_url = server$url,
          job_id = job_id,
          token = server$token %||% "",
          max_lines = 60L
        ),
        error = function(e) data.frame()
      )
    } else {
      data.frame()
    }
    remote_job_resources_data(resources)
    remote_job_status_text(status_text)
    invisible(status)
  }

  remote_resource_refresh_timer <- reactiveTimer(30000, session = session)
  observe({
    remote_resource_refresh_timer()
    job_id <- input$remote_job_id %||% ""
    if (!nzchar(job_id)) {
      return()
    }
    server <- tryCatch(remote_server_by_name(remote_server_name_for_job(job_id)), error = function(e) NULL)
    if (is.null(server) || !remote_server_supports("job_resource_monitor", server$name[[1]])) {
      remote_job_resources_data(data.frame())
      return()
    }
    resources <- tryCatch(
      ugplot_remote_job_resources(
        server_url = server$url,
        job_id = job_id,
        token = server$token %||% "",
        max_lines = 60L
      ),
      error = function(e) NULL
    )
    if (is.data.frame(resources)) {
      remote_job_resources_data(resources)
    }
  })

  load_remote_job_bundle_locally <- function(job_id, server_name = NULL) {
    req(nzchar(job_id %||% ""))
    server <- remote_server_by_name(server_name %||% remote_server_name_for_job(job_id))
    status <- tryCatch(
      ugplot_remote_job_status(
        server_url = server$url,
        job_id = job_id,
        token = server$token %||% ""
      ),
      error = function(e) NULL
    )
    if (remote_status_is_geo(status) && !remote_server_supports("job_bundle", server$name[[1]])) {
      return(load_remote_geo_job_locally(job_id, server, status = status))
    }
    if (!remote_server_supports("job_bundle", server$name[[1]])) {
      result <- ugplot_remote_get_result(
        server_url = server$url,
        job_id = job_id,
        token = server$token %||% ""
      )
      if (remote_result_is_geo(result)) {
        return(load_remote_geo_job_locally(job_id, server, status = status, result = result))
      }
      remote_job_preview_result(result)
      apply_remote_ml_result(result, job_id)
      remote_job_status_text(paste(
        "Remote result loaded locally, but the server does not expose the saved dataset/config.",
        "Restart ugPlotServer on the remote machine after updating ugplot to enable full Load."
      ))
      updateTabsetPanel(session, "tabs", selected = "MACHINE LEARNING")
      return(invisible(result))
    }
    bundle <- ugplot_remote_get_job_bundle(
      server_url = server$url,
      job_id = job_id,
      token = server$token %||% ""
    )
    dataset <- bundle$dataset
    config <- bundle$config %||% list()
    status <- bundle$status %||% status
    if (remote_config_is_geo(config) || remote_status_is_geo(status) || remote_result_is_geo(bundle$result)) {
      return(load_remote_geo_job_locally(job_id, server, status = status, result = bundle$result, config = config))
    }
    activate_remote_server_for_job(server)
    if (!is.data.frame(dataset)) {
      stop("Remote job bundle did not include a data.frame dataset.", call. = FALSE)
    }

    dff <<- dataset
    df_pre <<- dataset
    changed_table <<- dataset
    original_dataset_filename(paste0("remote-job-", job_id))
    reset_missing_strategy_ui()
    scrambled_columns(character(0))
    scramble_original_columns(list())
    heatmap_recorded_plot(NULL)
    hide("downloadHeatmapPlotTiff")
    updateTextAreaInput(session, "textarea_columns", value = paste(names(dataset), collapse = "\n"))
    updateTextAreaInput(session, "textarea_rows", value = paste(rownames(dataset), collapse = "\n"))
    load_dataset_into_table(session)
    refresh_counter(refresh_counter() + 1)
    update_scramble_selector()

    target <- config$target %||% config$target_name %||% ""
    if (nzchar(target) && target %in% names(dataset)) {
      updateSelectizeInput(session, "ml_target", choices = c("", names(dataset)), selected = target, server = TRUE)
    }
    updateNumericInput(session, "ml_dataset_seedi", value = config$dataset_seed_start %||% 1)
    updateNumericInput(session, "ml_dataset_seedf", value = config$dataset_seed_end %||% config$dataset_seed_start %||% 1)
    updateNumericInput(session, "ml_seedi", value = config$training_seed_start %||% 1)
    updateNumericInput(session, "ml_seedf", value = config$training_seed_end %||% config$training_seed_start %||% 1)
    updateNumericInput(session, "ml_timeout", value = config$timeout %||% 1200)
    updateSelectInput(session, "ml_performance_mode", selected = config$performance_mode %||% "default")
    updateSelectInput(session, "ml_cv_method", selected = config$cv_method %||% "cv")
    updateNumericInput(session, "ml_cv_folds", value = config$cv_folds %||% 10)
    updateNumericInput(session, "ml_cv_repeats", value = config$cv_repeats %||% 1)
    updateNumericInput(session, "ml_tune_length", value = config$tune_length %||% 3)
    updateCheckboxInput(session, "ml_auto_skip_bad_models", value = isTRUE(config$auto_skip_bad_models %||% FALSE))
    updateNumericInput(session, "ml_min_r2_skip", value = config$min_r2_skip %||% 0)
    updateCheckboxGroupInput(session, "ml_missing_definition", selected = config$missing_definition %||% c("empty", "na"))
    updateSelectizeInput(session, "ml_zero_exceptions", selected = config$zero_exceptions %||% character(0))
    updateSelectInput(session, "ml_missing_strategy", selected = config$missing_strategy %||% "none")
    updateSelectInput(session, "ml_imputation_scope", selected = config$imputation_scope %||% "split_separate")
    updateNumericInput(session, "ml_missing_threshold_cols", value = config$missing_threshold_cols %||% 100)
    updateNumericInput(session, "ml_missing_threshold_rows", value = config$missing_threshold_rows %||% 100)
    updateNumericInput(session, "ml_complete_case_min_samples", value = config$complete_case_min_samples %||% 80)
    category_columns <- intersect(config$category_columns %||% character(0), names(dataset))
    updateCheckboxGroupInput(session, "checkbox_group_categories", selected = category_columns)
    job_name <- as.character(config$job_name %||% bundle$status$name %||% "")
    updateTextInput(session, "remote_job_name", value = job_name)

    remote_job_preview_status(bundle$status %||% NULL)
    if (is.list(bundle$result)) {
      remote_job_preview_result(bundle$result)
      result <- bundle$result
      apply_remote_ml_result(result, job_id)
      tested_models <- remote_result_tested_models(result)
      if (length(tested_models) > 0) {
        updateCheckboxGroupInput(session, "ml_checkbox_group", selected = tested_models)
      } else {
        models <- intersect(config$models %||% config$model_names %||% character(0), ml_available)
        updateCheckboxGroupInput(session, "ml_checkbox_group", selected = models)
      }
    } else {
      remote_job_preview_result(NULL)
      result <- NULL
      ml_table_results(data.frame())
      ml_final_summary(NULL)
      best_model_object(NULL)
      best_model_preprocess(NULL)
      ml_prediction <<- list()
      models <- intersect(config$models %||% config$model_names %||% character(0), ml_available)
      updateCheckboxGroupInput(session, "ml_checkbox_group", selected = models)
      remote_job_status_text(paste("Remote job loaded locally without result table:", job_id))
    }
    session$onFlushed(function() {
      updateTabsetPanel(session, "tabs", selected = "MACHINE LEARNING")
    }, once = TRUE)
    invisible(bundle)
  }

  submit_remote_ml_job <- function() {
    config <- build_remote_ml_config()
    if (length(config$models) == 0) {
      stop("Select at least one ML model before sending a remote job.", call. = FALSE)
    }
    server <- selected_remote_server()
    started <- ugplot_remote_create_job(
      server_url = server$url,
      dataset = current_remote_ml_dataset(),
      config = config,
      token = server$token %||% ""
    )
    updateTextInput(session, "remote_job_id", value = started$id %||% "")
    remote_job_status_text(paste("Remote job submitted:", started$id %||% "unknown"))
    refresh_remote_jobs()
    updateTabsetPanel(session, "tabs", selected = "JOBS")
    invisible(started)
  }

  observeEvent(input$remote_submit_job, {
    tryCatch({
      submit_remote_ml_job()
    }, error = function(e) {
      remote_job_status_text(paste("Remote submit failed:", conditionMessage(e)))
    })
  })

	  observeEvent(input$remote_refresh_jobs, {
	    tryCatch({
	      refresh_remote_jobs()
	      remote_job_status_text("Remote jobs refreshed.")
	    }, error = function(e) {
	      remote_job_status_text(paste("Remote refresh failed:", conditionMessage(e)))
	    })
	  })

	  observeEvent(input$geo_start_remote_pipeline, {
	    tryCatch({
	      submit_remote_geo_pipeline()
	    }, error = function(e) {
	      geo_remote_pipeline_status(paste("Remote GEO submit failed:", conditionMessage(e)))
	      remote_job_status_text(geo_remote_pipeline_status())
	      showNotification(geo_remote_pipeline_status(), type = "error", duration = 10)
	    })
	  })

	  observeEvent(input$geo_continue_remote_pipeline_click, {
	    tryCatch({
	      submit_remote_geo_pipeline(mode = "cached_continue")
	    }, error = function(e) {
	      geo_remote_pipeline_status(paste("Remote GEO continue failed:", conditionMessage(e)))
	      remote_job_status_text(geo_remote_pipeline_status())
	      showNotification(geo_remote_pipeline_status(), type = "error", duration = 10)
	    })
	  })

	  observeEvent(input$geo_refresh_remote_pipeline, {
	    tryCatch({
	      job_id <- geo_remote_pipeline_job_id() %||% input$remote_job_id %||% ""
	      refresh_remote_geo_pipeline_status(job_id, server_name = remote_server_name_for_job(job_id))
	      refresh_remote_jobs()
	    }, error = function(e) {
	      geo_remote_pipeline_status(paste("Remote GEO status failed:", conditionMessage(e)))
	      remote_job_status_text(geo_remote_pipeline_status())
	    })
	  })

	  observeEvent(input$geo_load_remote_pipeline_result, {
	    tryCatch({
	      job_id <- geo_remote_pipeline_job_id() %||% input$remote_job_id %||% ""
	      if (!nzchar(job_id)) {
	        stop("No remote GEO job id is selected.", call. = FALSE)
	      }
	      server <- remote_server_by_name(remote_server_name_for_job(job_id))
	      remember_remote_job_server(job_id, as.character(server$name[[1]]))
	      activate_geo_remote_server_for_job(server)
	      result <- ugplot_remote_get_result(
	        server_url = server$url,
	        job_id = job_id,
	        token = server$token %||% ""
	      )
	      apply_remote_geo_result(result, job_id)
	      refresh_remote_geo_pipeline_status(job_id, server_name = as.character(server$name[[1]]))
	    }, error = function(e) {
	      geo_remote_pipeline_status(paste("Remote GEO result load failed:", conditionMessage(e)))
	      remote_job_status_text(geo_remote_pipeline_status())
	    })
	  })

  output$remote_jobs_table <- DT::renderDT({
    jobs <- remote_jobs()
    if (!is.data.frame(jobs)) {
      jobs <- data.frame()
    }
    raw_jobs <- jobs
    for (date_column in intersect(c("created_at", "updated_at"), names(jobs))) {
      jobs[[date_column]] <- sub("[[:space:]][+-][0-9]{4}$", "", as.character(jobs[[date_column]]))
    }
    if ("progress" %in% names(jobs)) {
      progress_estimates <- remote_job_progress_estimates()
      jobs$progress <- vapply(seq_len(nrow(jobs)), function(i) {
        job_id <- if ("id" %in% names(raw_jobs)) as.character(raw_jobs$id[[i]]) else ""
        estimate_label <- if (nzchar(job_id)) progress_estimates[[job_id]] %||% "" else ""
        row_state <- if ("state" %in% names(raw_jobs)) as.character(raw_jobs$state[[i]]) else ""
        row_message <- if ("message" %in% names(raw_jobs)) as.character(raw_jobs$message[[i]]) else ""
        if (nzchar(estimate_label) &&
            row_state %in% c("queued", "running") &&
            grepl("^Stability[[:space:]]+", row_message)) {
          return(estimate_label)
        }
        row_status <- list(
          type = if ("type" %in% names(raw_jobs)) raw_jobs$type[[i]] else "",
          message = row_message
        )
        remote_job_progress_label(jobs$progress[[i]], status = row_status)
      }, character(1))
    }
    raw_model_values <- if ("models" %in% names(raw_jobs)) as.character(raw_jobs$models) else character(0)
    character_columns <- names(jobs)[vapply(jobs, is.character, logical(1))]
    for (column_name in character_columns) {
      jobs[[column_name]] <- htmltools::htmlEscape(jobs[[column_name]])
    }
    if (nrow(jobs) > 0 && "models" %in% names(jobs)) {
      jobs$models <- vapply(seq_len(nrow(jobs)), function(i) {
        raw_models <- raw_model_values[[i]] %||% ""
        model_items <- trimws(strsplit(raw_models, ",", fixed = TRUE)[[1]])
        model_items <- model_items[nzchar(model_items)]
        model_count <- length(model_items)
        escaped_models <- jobs$models[[i]]
        if (model_count <= 8 && nchar(raw_models) <= 120) {
          return(escaped_models)
        }
        summary_items <- utils::head(model_items, 5)
        summary_text <- paste(summary_items, collapse = ", ")
        if (model_count > length(summary_items)) {
          summary_text <- paste0(summary_text, " ...")
        }
        paste0(
          "<details class='remote-job-models' onclick='event.stopPropagation();'>",
          "<summary>",
          htmltools::htmlEscape(summary_text),
          " <span class='remote-job-model-count'>", model_count, " models</span>",
          "</summary>",
          "<div class='remote-job-model-list'>", escaped_models, "</div>",
          "</details>"
        )
      }, character(1))
    }
    if (nrow(jobs) > 0 && "id" %in% names(raw_jobs)) {
      job_ids <- as.character(raw_jobs$id)
      server_names <- if ("server" %in% names(raw_jobs)) as.character(raw_jobs$server) else rep(selected_remote_server()$name[[1]], length(job_ids))
      states <- if ("state" %in% names(jobs)) as.character(jobs$state) else rep("", length(job_ids))
      can_stop <- states %in% c("queued", "running")
      can_load <- !can_stop
      load_labels <- ifelse(states == "finished", "Load", "Load partial")
      can_delete <- vapply(server_names, function(server_name) remote_server_supports("delete_job", server_name), logical(1)) & !states %in% c("queued", "running")
      can_resume <- if ("resumable" %in% names(raw_jobs)) {
        vapply(server_names, function(server_name) remote_server_supports("resume_job", server_name), logical(1)) &
          tolower(as.character(raw_jobs$resumable)) %in% c("true", "1", "yes")
      } else {
        rep(FALSE, length(states))
      }
      if ("resumable" %in% names(jobs)) {
        jobs$resumable <- NULL
      }
      actions <- vapply(seq_along(job_ids), function(i) {
        job_id <- htmltools::htmlEscape(job_ids[[i]], attribute = TRUE)
        action_key <- htmltools::htmlEscape(remote_job_action_key(server_names[[i]], job_ids[[i]]), attribute = TRUE)
        buttons <- character()
        if (isTRUE(can_load[[i]])) {
          buttons <- c(buttons, paste0("<button type='button' class='btn btn-default btn-sm' onclick=\"event.stopPropagation(); Shiny.setInputValue('remote_load_result_row', '", action_key, "', {priority: 'event'});\">", load_labels[[i]], "</button>"))
        } else {
          buttons <- c(buttons, "<button type='button' class='btn btn-default btn-sm' disabled title='Stop or wait for the job before loading the full dataset/results.'>Load</button>")
        }
        if (isTRUE(can_stop[[i]])) {
          buttons <- c(buttons, paste0("<button type='button' class='btn btn-danger btn-sm' onclick=\"event.stopPropagation(); Shiny.setInputValue('remote_stop_job_row', '", action_key, "', {priority: 'event'});\">Stop</button>"))
        }
        if (isTRUE(can_resume[[i]])) {
          buttons <- c(buttons, paste0("<button type='button' class='btn btn-success btn-sm' onclick=\"event.stopPropagation(); Shiny.setInputValue('remote_resume_job_row', '", action_key, "', {priority: 'event'});\">Resume</button>"))
        }
        if (isTRUE(can_delete[[i]])) {
          buttons <- c(buttons, paste0("<button type='button' class='btn btn-danger btn-sm' onclick=\"event.stopPropagation(); Shiny.setInputValue('remote_delete_job_request', '", action_key, "', {priority: 'event'});\">Delete</button>"))
        }
        paste0("<div class='remote-job-actions'>", paste(buttons, collapse = ""), "</div>")
      }, character(1))
      jobs$Actions <- actions
    }
    hidden_detail_columns <- intersect(c("id", "pid"), names(jobs))
    if (length(hidden_detail_columns) > 0) {
      jobs <- jobs[, setdiff(names(jobs), hidden_detail_columns), drop = FALSE]
    }
    table_options <- list(pageLength = 8, scrollX = TRUE)
    if ("state" %in% names(jobs)) {
      state_column <- which(names(jobs) == "state") - 1L
      table_options$createdRow <- DT::JS(sprintf(
        "function(row, data, dataIndex) {
          var state = (data[%d] || '').toString().toLowerCase();
          $(row).removeClass('remote-job-row-finished remote-job-row-active remote-job-row-problem');
          if (state === 'finished') {
            $(row).addClass('remote-job-row-finished');
          } else if (state === 'queued' || state === 'running') {
            $(row).addClass('remote-job-row-active');
          } else if (state === 'failed' || state === 'stopped' || state === 'error') {
            $(row).addClass('remote-job-row-problem');
          }
        }",
        state_column
      ))
    }
    if ("Actions" %in% names(jobs)) {
      table_options$columnDefs <- list(list(orderable = FALSE, targets = which(names(jobs) == "Actions") - 1L))
    }
    DT::datatable(
      jobs,
      options = table_options,
      rownames = FALSE,
      selection = "single",
      escape = FALSE,
      callback = DT::JS(
        "table.on('mousedown mouseup click dblclick', '.remote-job-actions, .remote-job-actions *, .remote-job-models, .remote-job-models *', function(e) {
          e.stopPropagation();
        });"
      )
    )
  })

  observeEvent(input$remote_jobs_table_rows_selected, {
    selected <- input$remote_jobs_table_rows_selected
    jobs <- remote_jobs()
    if (length(selected) == 1 && is.data.frame(jobs) && nrow(jobs) >= selected && "id" %in% names(jobs)) {
      job_id <- jobs$id[[selected]]
      server_name <- if ("server" %in% names(jobs)) jobs$server[[selected]] else NULL
      if (!is.null(server_name) && nzchar(server_name %||% "")) {
        remember_remote_job_server(job_id, server_name)
        server <- remote_server_by_name(server_name)
        updateRadioButtons(session, "ml_run_target", selected = "remote")
        refresh_remote_server_inputs(selected = as.character(server$name[[1]]))
        updateSelectInput(session, "remote_server_name", selected = as.character(server$name[[1]]))
      }
      updateTextInput(session, "remote_job_id", value = job_id)
      tryCatch({
        refresh_remote_job_preview(job_id, switch_to_ml = FALSE, server_name = server_name)
      }, error = function(e) {
        remote_job_status_text(paste("Remote status failed:", conditionMessage(e)))
      })
    }
  })

  observeEvent(input$remote_job_id, {
    if (isTRUE(remote_job_loading())) {
      return()
    }
    if (!nzchar(input$remote_job_id %||% "")) {
      return()
    }
    selected_job <- remote_selected_job()
    server_name <- if (is.list(selected_job) && identical(as.character(selected_job$id %||% ""), as.character(input$remote_job_id %||% ""))) {
      selected_job$server %||% NULL
    } else {
      NULL
    }
    tryCatch({
      refresh_remote_job_preview(input$remote_job_id, switch_to_ml = FALSE, server_name = server_name)
    }, error = function(e) {
      remote_job_status_text(paste("Remote status failed:", conditionMessage(e)))
    })
  }, ignoreInit = TRUE)

  load_remote_result_locally <- function(job_id, switch_to_ml = TRUE, server_name = NULL) {
    req(nzchar(job_id %||% ""))
    server <- remote_server_by_name(server_name %||% remote_server_name_for_job(job_id))
    result <- ugplot_remote_get_result(
      server_url = server$url,
      job_id = job_id,
      token = server$token %||% ""
    )
    if (remote_result_is_geo(result)) {
      load_remote_geo_job_locally(job_id, server, result = result)
      return(invisible(result))
    }
    apply_remote_ml_result(result, job_id)
    remote_job_status_text(paste("Remote result loaded locally:", job_id))
    if (isTRUE(switch_to_ml)) {
      updateTabsetPanel(session, "tabs", selected = "MACHINE LEARNING")
    }
    invisible(result)
  }

  observeEvent(input$remote_load_result_row, {
    tryCatch({
      action <- parse_remote_job_action_key(input$remote_load_result_row)
      remote_job_loading(TRUE)
      remember_remote_job_server(action$job_id, action$server)
      loaded <- load_remote_job_bundle_locally(action$job_id, server_name = action$server)
      if (!remote_result_is_geo(loaded)) {
        updateTextInput(session, "remote_job_id", value = action$job_id)
      }
      session$onFlushed(function() {
        remote_job_loading(FALSE)
      }, once = TRUE)
    }, error = function(e) {
      remote_job_loading(FALSE)
      remote_job_status_text(paste("Remote result load failed:", conditionMessage(e)))
    })
  })

  observeEvent(input$remote_stop_job_row, {
    tryCatch({
      action <- parse_remote_job_action_key(input$remote_stop_job_row)
      job_id <- action$job_id
      server <- remote_server_by_name(action$server)
      status <- ugplot_remote_stop_job(
        server_url = server$url,
        job_id = job_id,
        token = server$token %||% ""
      )
      updateTextInput(session, "remote_job_id", value = job_id)
      refresh_remote_jobs()
      remote_job_status_text(paste("Remote job stopped:", job_id, "-", status$message %||% ""))
    }, error = function(e) {
      remote_job_status_text(paste("Remote stop failed:", conditionMessage(e)))
    })
  })

  observeEvent(input$remote_resume_job_row, {
    tryCatch({
      action <- parse_remote_job_action_key(input$remote_resume_job_row)
      job_id <- action$job_id
      server <- remote_server_by_name(action$server)
      status <- ugplot_remote_resume_job(
        server_url = server$url,
        job_id = job_id,
        token = server$token %||% ""
      )
      updateTextInput(session, "remote_job_id", value = job_id)
      refresh_remote_jobs()
      remote_job_status_text(paste("Remote job resumed:", job_id, "-", status$message %||% ""))
    }, error = function(e) {
      remote_job_status_text(paste("Remote resume failed:", conditionMessage(e)))
    })
  })

  observeEvent(input$remote_delete_job_request, {
    action <- parse_remote_job_action_key(input$remote_delete_job_request)
    job_id <- action$job_id
    showModal(modalDialog(
      title = "Delete remote job",
      tags$p("Delete this job from the remote server?"),
      tags$p(strong("Server: "), action$server %||% ""),
      tags$code(job_id),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("remote_delete_job_confirm", "Delete", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$remote_delete_job_confirm, {
    tryCatch({
      action <- parse_remote_job_action_key(input$remote_delete_job_request)
      job_id <- action$job_id
      server <- remote_server_by_name(action$server)
      ugplot_remote_delete_job(
        server_url = server$url,
        job_id = job_id,
        token = server$token %||% ""
      )
      if (identical(remote_result_cache_job_id(), job_id)) {
        remote_result_cache(NULL)
        remote_result_cache_job_id("")
      }
      if (identical(input$remote_job_id %||% "", job_id)) {
        updateTextInput(session, "remote_job_id", value = "")
        remote_job_preview_status(NULL)
        remote_job_preview_result(NULL)
        remote_job_log_text("")
        remote_job_resources_data(data.frame())
      }
      refresh_remote_jobs()
      remote_job_status_text(paste("Remote job deleted:", job_id))
      removeModal()
    }, error = function(e) {
      remote_job_status_text(paste("Remote delete failed:", conditionMessage(e)))
      removeModal()
    })
  })

  output$remote_job_status <- renderText({
    remote_job_status_text()
  })

  output$remote_job_status_panel <- renderUI({
    status <- remote_job_preview_status()
    result <- remote_job_preview_result()
    if (remote_status_is_geo(status) || (is.list(result) && identical(result$kind %||% "", "geo_pipeline"))) {
      return(NULL)
    }
    text <- remote_job_status_text()
    if (!nzchar(text %||% "")) {
      return(NULL)
    }
    tags$pre(class = "remote-job-status-panel", text)
  })

  output$remote_server_connection_status <- renderUI({
    server_state <- remote_server_connection_state()
    if (!is.data.frame(server_state) || nrow(server_state) == 0) {
      return(NULL)
    }
    state_styles <- list(
      active = list(label = "ONLINE - RUNNING", background = "#e8f7eb", border = "#2e9d4d", color = "#1f6f37"),
      idle = list(label = "ONLINE - IDLE", background = "#f0e9ff", border = "#7a4fd4", color = "#5632a8"),
      version_mismatch = list(label = "VERSION MISMATCH", background = "#fff4e5", border = "#f0ad4e", color = "#8a5a00"),
      offline = list(label = "OFFLINE", background = "#fdecec", border = "#d9534f", color = "#a5302d")
    )
    cards <- lapply(seq_len(nrow(server_state)), function(i) {
      row <- server_state[i, , drop = FALSE]
      state <- as.character(row$state %||% "offline")
      style <- state_styles[[state]] %||% state_styles$offline
      jobs_label <- if (is.na(row$jobs[[1]])) "no response" else paste0(row$jobs[[1]], " jobs")
      active_label <- if (!is.na(row$active[[1]]) && row$active[[1]] > 0) paste0(" / ", row$active[[1]], " active") else ""
      number <- function(name) {
        if (!name %in% names(row)) return(NA_real_)
        suppressWarnings(as.numeric(row[[name]][[1]]))
      }
      fmt_pct <- function(value) if (is.finite(value)) paste0(round(value, 1), "%") else "N/A"
      fmt_memory <- function(available, total) {
        if (!is.finite(available) || !is.finite(total)) return("N/A")
        paste0(round(available / 1024, 1), " GB free / ", round(total / 1024, 1), " GB")
      }
      cpu_pct <- number("cpu_pct")
      cpu_count <- number("cpu_count")
      load1 <- number("load1")
      memory_pct <- number("memory_pct")
      memory_available <- number("memory_available_mb")
      memory_total <- number("memory_total_mb")
      disk_pct <- number("disk_pct")
      task <- if ("task" %in% names(row)) as.character(row$task[[1]] %||% "") else ""
      tags$div(
        style = paste0(
          "border-left: 5px solid ", style$border, "; background: ", style$background,
          "; color: ", style$color, "; padding: 6px 10px; min-width: 150px;",
          "border-radius: 4px; box-shadow: inset 0 0 0 1px rgba(0,0,0,0.04);"
        ),
        tags$div(style = "font-weight: 700; font-size: 12px; line-height: 1.2;", htmltools::htmlEscape(row$server[[1]])),
        tags$div(style = "font-size: 11px; line-height: 1.25;", style$label),
        tags$div(style = "font-size: 11px; line-height: 1.25;", paste0(jobs_label, active_label)),
        if (!identical(state, "offline")) tags$div(
          style = "font-size: 11px; line-height: 1.35; margin-top: 3px;",
          paste0(
            "CPU ", fmt_pct(cpu_pct),
            " / load ", if (is.finite(load1)) round(load1, 2) else "N/A",
            " / ", if (is.finite(cpu_count)) round(cpu_count) else "?", " CPUs"
          ),
          tags$br(),
          paste0("Memory ", fmt_pct(memory_pct), " (", fmt_memory(memory_available, memory_total), ")"),
          tags$br(),
          paste0("Disk ", fmt_pct(disk_pct)),
          if (nzchar(task)) tags$div(
            style = "font-size: 10px; margin-top: 2px; max-width: 360px; overflow-wrap: anywhere;",
            htmltools::htmlEscape(task)
          )
        ),
        if (identical(state, "version_mismatch")) {
          tags$div(style = "font-size: 10px; margin-top: 2px; max-width: 340px;", htmltools::htmlEscape(row$message[[1]] %||% "Version mismatch"))
        } else if (identical(state, "offline")) {
          tags$div(style = "font-size: 10px; margin-top: 2px; max-width: 280px;", htmltools::htmlEscape(row$message[[1]] %||% "Connection failed"))
        }
      )
    })
    tags$div(
      style = "display: flex; gap: 8px; flex-wrap: wrap; margin: 0;",
      cards
    )
  })

  output$remote_job_geo_progress_report <- renderUI({
    raw_status <- remote_job_preview_status()
    resources <- remote_job_resources_data()
    status <- remote_status_with_live_message(raw_status, resources)
    result <- remote_job_preview_result()
    if (!remote_status_is_geo(status) && !(is.list(result) && identical(result$kind %||% "", "geo_pipeline"))) {
      return(NULL)
    }

    tables <- if (is.list(result) && is.list(result$tables)) result$tables else list()
    n_table <- function(name) {
      table <- tables[[name]]
      if (is.data.frame(table)) nrow(table) else 0L
    }
    status_state <- remote_status_scalar(status$state, "unknown")
    status_message <- remote_status_scalar(status$message)
    stability <- remote_geo_stability_context(status, result)

    remote_files_n <- n_table("remote_files")
    metadata_n <- n_table("metadata_preview")
    idat_n <- n_table("idat_qc")
    spearman_n <- max(n_table("spearman_preview"), n_table("transcript_spearman_preview"))
    candidates_n <- n_table("transcript_candidates_preview")
    groups_n <- n_table("transcript_groups")
    screening_n <- n_table("transcript_ml_screening")
    summary <- tables$transcript_ml_summary
    stability_saved <- if (is.data.frame(summary) && "Phase" %in% names(summary)) {
      sum(as.character(summary$Phase) == "stability", na.rm = TRUE)
    } else {
      0L
    }
    total_groups <- max(groups_n, screening_n, stability$task_total %||% NA_integer_, na.rm = TRUE)
    if (!is.finite(total_groups) || total_groups < 1L) {
      total_groups <- max(groups_n, screening_n, 0L)
    }
    stability_current <- if (isTRUE(stability$is_stability) && is.finite(stability$task_index)) 1L else 0L
    stability_done_lower <- if (isTRUE(stability$is_stability) && is.finite(stability$task_index)) {
      max(stability_saved, stability$task_index - if (identical(stability$state, "Running")) 1L else 0L)
    } else {
      stability_saved
    }
    stability_remaining <- if (is.finite(total_groups) && total_groups > 0) {
      max(0L, total_groups - stability_done_lower - if (identical(stability$state, "Running")) stability_current else 0L)
    } else {
      NA_integer_
    }

    stage_state <- function(done, running = FALSE) {
      if (isTRUE(done)) "done" else if (isTRUE(running)) "running" else "pending"
    }
    badge_text <- function(state) {
      switch(state, done = "Completed", running = "Running", pending = "Pending", "Pending")
    }
    step_card <- function(index, title, state, detail = "", metric = "") {
      tags$div(
        class = paste("remote-geo-step", paste0("remote-geo-step-", state)),
        tags$div(class = "remote-geo-step-top",
          tags$span(class = "remote-geo-step-index", index),
          tags$span(class = "remote-geo-step-title", title),
          tags$span(class = paste("remote-geo-step-badge", paste0("remote-geo-step-badge-", state)), badge_text(state))
        ),
        if (nzchar(metric)) tags$div(class = "remote-geo-step-metric", metric),
        if (nzchar(detail)) tags$div(class = "remote-geo-step-detail", detail)
      )
    }
    stat_card <- function(title, value, detail, level = "neutral") {
      tags$div(
        class = paste("remote-geo-stat", paste0("remote-geo-stat-", level)),
        tags$div(class = "remote-geo-stat-title", title),
        tags$div(class = "remote-geo-stat-value", value),
        tags$div(class = "remote-geo-stat-detail", detail)
      )
    }

    files_done <- remote_files_n > 0 || metadata_n > 0
    idat_done <- idat_n > 0
    spearman_done <- spearman_n > 0 || candidates_n > 0
    groups_done <- groups_n > 0
    screening_done <- screening_n > 0 && (groups_n == 0 || screening_n >= groups_n)
    stability_done <- total_groups > 0 && stability_saved >= total_groups
    final_done <- identical(status_state, "finished")
    stability_running <- isTRUE(stability$is_stability) && status_state %in% c("queued", "running")
    estimate_pct <- if (isTRUE(stability$is_stability) && is.finite(stability$estimate_pct)) {
      stability$estimate_pct
    } else if (total_groups > 0) {
      100 * stability_done_lower / total_groups
    } else {
      NA_real_
    }
    estimate_width <- if (is.finite(estimate_pct)) max(0, min(100, estimate_pct)) else 0
    task_width <- if (isTRUE(stability$is_stability) && is.finite(stability$task_index) && is.finite(stability$task_total) && stability$task_total > 0) {
      max(0, min(100, 100 * stability$task_index / stability$task_total))
    } else {
      0
    }
    seed_width <- if (isTRUE(stability$is_stability) && is.finite(stability$training_seed) && is.finite(stability$min_seeds) && stability$min_seeds > 0) {
      max(0, min(100, 100 * stability$training_seed / stability$min_seeds))
    } else {
      0
    }
    current_seed_detail <- if (isTRUE(stability$is_stability) && is.finite(stability$training_seed)) {
      paste0("seed ", stability$training_seed, "/", if (is.finite(stability$min_seeds)) stability$min_seeds else "?")
    } else if (isTRUE(stability$is_stability)) {
      stability$state
    } else {
      "not started"
    }
    active_status <- if (isTRUE(stability_running)) {
      "Running now"
    } else if (isTRUE(stability_done || final_done)) {
      "Completed"
    } else if (isTRUE(stability$is_stability)) {
      stability$state
    } else {
      "Waiting"
    }
    active_detail <- if (isTRUE(stability$is_stability)) {
      paste0(
        stability$group_id, " / ", stability$stratum,
        " / model ", stability$model,
        if (is.finite(stability$training_seed)) paste0(" / training seed ", stability$training_seed) else ""
      )
    } else {
      "Stability has not started yet."
    }

    tags$div(
      class = "remote-geo-report",
      tags$div(class = "remote-geo-report-header",
        tags$div(
          tags$div(class = "remote-geo-report-title", "GEO job progress"),
          tags$div(class = "remote-geo-report-subtitle",
            paste0(
              remote_status_scalar(status$id),
              " / ", remote_status_scalar(status$target, result$accession %||% ""),
              " / ", status_state
            )
          )
        ),
        tags$div(class = "remote-geo-stage-pill", if (nzchar(status_message)) status_message else "waiting")
      ),
      tags$div(class = "remote-geo-active",
        tags$div(class = "remote-geo-active-main",
          tags$div(class = "remote-geo-active-kicker", "Current running step"),
          tags$div(class = "remote-geo-active-title", active_status),
          tags$div(class = "remote-geo-active-detail", active_detail),
          tags$div(class = "remote-geo-active-bars",
            tags$div(class = "remote-geo-active-row",
              tags$div(class = "remote-geo-active-row-label",
                tags$span("Task"),
                tags$strong(if (isTRUE(stability$is_stability) && is.finite(stability$task_index)) {
                  paste0(stability$task_index, "/", stability$task_total)
                } else {
                  "-"
                })
              ),
              tags$div(class = "remote-geo-bar-shell remote-geo-bar-shell-small",
                tags$div(class = "remote-geo-bar remote-geo-bar-task", style = paste0("width: ", round(task_width), "%;"))
              )
            ),
            tags$div(class = "remote-geo-active-row",
              tags$div(class = "remote-geo-active-row-label",
                tags$span("Minimum seed pass"),
                tags$strong(if (isTRUE(stability$is_stability) && is.finite(stability$training_seed)) {
                  paste0(stability$training_seed, "/", if (is.finite(stability$min_seeds)) stability$min_seeds else "?")
                } else {
                  "-"
                })
              ),
              tags$div(class = "remote-geo-bar-shell remote-geo-bar-shell-small",
                tags$div(class = "remote-geo-bar remote-geo-bar-seed", style = paste0("width: ", round(seed_width), "%;"))
              )
            )
          )
        ),
        tags$div(class = "remote-geo-active-side",
          tags$div(class = "remote-geo-active-number", if (is.finite(estimate_pct)) paste0(round(estimate_pct), "%") else "-"),
          tags$div(class = "remote-geo-active-number-label", "lower-bound stability"),
          tags$div(class = "remote-geo-active-mini",
            tags$span(paste0("Done ", stability_done_lower)),
            tags$span(if (isTRUE(stability_running) && is.finite(stability$task_index)) paste0("Current ", stability$task_index) else "Current -"),
            tags$span(if (is.finite(stability_remaining)) paste0("Left ", stability_remaining) else "Left -")
          )
        )
      ),
      tags$div(class = "remote-geo-stage-grid",
        step_card(1, "Files and metadata", stage_state(files_done), paste0(remote_files_n, " files / ", metadata_n, " metadata rows")),
        step_card(2, "Sesame QC", stage_state(idat_done, files_done && !idat_done), paste0(idat_n, " samples processed")),
        step_card(3, "CpG and transcript scan", stage_state(spearman_done, idat_done && !spearman_done), paste0(spearman_n, " preview CpGs / ", candidates_n, " candidate transcripts")),
        step_card(4, "Transcript groups", stage_state(groups_done, spearman_done && !groups_done), paste0(groups_n, " groups")),
        step_card(5, "Model screening", stage_state(screening_done, groups_done && !screening_done), paste0(screening_n, "/", max(groups_n, screening_n), " groups screened")),
        step_card(
          6,
          "Stability seeds",
          stage_state(stability_done || final_done, stability_running),
          paste0(
            stability_done_lower, "/", if (total_groups > 0) total_groups else "?",
            " groups done",
            if (isTRUE(stability_running) && is.finite(stability$task_index)) paste0(" / current task ", stability$task_index, "/", stability$task_total) else ""
          ),
          current_seed_detail
        ),
        step_card(7, "Final result", stage_state(final_done, stability_done && !final_done), if (final_done) "result.rds available" else "waiting for stability")
      ),
      tags$div(class = "remote-geo-distribution",
        tags$div(class = "remote-geo-distribution-header",
          tags$div(
            tags$div(class = "remote-geo-distribution-title", "Result distribution"),
            tags$div(class = "remote-geo-distribution-subtitle", "Observed transcript-group metrics with normal fit")
          ),
          tags$div(class = "remote-geo-distribution-note", "updates from saved partial result")
        ),
        plotlyOutput("remote_geo_metric_distribution", height = "260px")
      ),
      tags$div(class = "remote-geo-focus",
        tags$div(class = "remote-geo-focus-main",
          tags$div(class = "remote-geo-focus-label", "Stability lower-bound progress"),
          tags$div(class = "remote-geo-focus-value", if (is.finite(estimate_pct)) paste0(round(estimate_pct), "%") else "Waiting"),
          tags$div(class = "remote-geo-bar-shell",
            tags$div(class = "remote-geo-bar", style = paste0("width: ", round(estimate_width), "%;"))
          ),
          tags$div(class = "remote-geo-focus-detail",
            if (isTRUE(stability$is_stability)) {
              paste0(
                stability$group_id, " / ", stability$stratum,
                " / ", stability$model,
                if (is.finite(stability$training_seed)) paste0(" / seed ", stability$training_seed) else ""
              )
            } else {
              "Stability has not started yet."
            }
          )
        ),
        tags$div(class = "remote-geo-stat-grid",
          stat_card("Done", stability_done_lower, "stability groups", "done"),
          stat_card("Current", if (isTRUE(stability_running) && is.finite(stability$task_index)) stability$task_index else "-", "task index", "running"),
          stat_card("Remaining", if (is.finite(stability_remaining)) stability_remaining else "-", "groups after current", "pending"),
          stat_card("Saved partial", if (remote_status_has_result(status)) "Yes" else "No", "safe to keep running", "neutral")
        )
      )
    )
  })

  output$remote_job_metric_panel <- renderUI({
    status <- remote_job_preview_status()
    result <- remote_job_preview_result()
    if (remote_status_is_geo(status) || (is.list(result) && identical(result$kind %||% "", "geo_pipeline"))) {
      return(NULL)
    }
    tags$div(
      style = "display: flex; gap: 12px; align-items: flex-start; flex-wrap: wrap; margin-top: 8px;",
      tags$div(style = "flex: 0 0 360px; max-width: 100%;", uiOutput("remote_job_running_summary")),
      tags$div(style = "flex: 1 1 420px; min-width: 320px; max-width: 620px;", plotlyOutput("remote_job_metric_distribution", height = "260px"))
    )
  })

  output$remote_job_resources <- renderUI({
    resources <- remote_job_resources_data()
    if (!is.data.frame(resources) || nrow(resources) == 0) {
      return(NULL)
    }
    latest <- resources[nrow(resources), , drop = FALSE]
    number_from <- function(row, name, fallback = NA_real_) {
      if (!name %in% names(row)) return(fallback)
      value <- suppressWarnings(as.numeric(row[[name]][[1]]))
      if (length(value) == 0 || !is.finite(value)) fallback else value
    }
    number <- function(name, fallback = NA_real_) number_from(latest, name, fallback)
    text_value <- function(name, fallback = "") {
      if (!name %in% names(latest)) return(fallback)
      value <- as.character(latest[[name]][[1]] %||% fallback)
      if (is.na(value)) fallback else value
    }
    fmt_mb <- function(value) {
      if (!is.finite(value)) return("N/A")
      if (value >= 1024) paste0(round(value / 1024, 1), " GB") else paste0(round(value), " MB")
    }
    fmt_pct <- function(value) if (is.finite(value)) paste0(round(value, 1), "%") else "N/A"
    severity <- function(value, warning_at, critical_at) {
      if (!is.finite(value)) "neutral" else if (value >= critical_at) "critical" else if (value >= warning_at) "warning" else "normal"
    }
    card <- function(title, value, detail, level = "neutral") {
      tags$div(
        class = paste("job-resource-card", paste0("job-resource-", level)),
        tags$div(class = "job-resource-title", title),
        tags$div(class = "job-resource-value", value),
        tags$div(class = "job-resource-detail", detail)
      )
    }

    alive_values <- if ("alive" %in% names(resources)) {
      tolower(as.character(resources$alive)) %in% c("true", "1", "yes")
    } else {
      rep(FALSE, nrow(resources))
    }
    rss_values <- if ("process_rss_mb" %in% names(resources)) suppressWarnings(as.numeric(resources$process_rss_mb)) else rep(NA_real_, nrow(resources))
    live_rows <- which(alive_values & is.finite(rss_values))
    process_row <- if (length(live_rows) > 0) resources[utils::tail(live_rows, 1L), , drop = FALSE] else latest
    process_alive <- isTRUE(utils::tail(alive_values, 1L))
    cpu_pct <- number_from(process_row, "process_cpu_pct")
    cpu_count <- number("host_cpu_count")
    load1 <- number("host_load1")
    memory_pct <- number("host_mem_used_pct")
    memory_available <- number("host_mem_available_mb")
    memory_total <- number("host_mem_total_mb")
    swap_pct <- number("host_swap_used_pct")
    swap_available <- number("host_swap_free_mb")
    swap_total <- number("host_swap_total_mb")
    disk_pct <- number("disk_used_pct")
    disk_available <- number("disk_available_mb")
    disk_total <- number("disk_total_mb")
    rss <- number_from(process_row, "process_rss_mb")
    process_count <- number_from(process_row, "process_count")
    threads <- number_from(process_row, "process_threads")
    process_sampled_at <- if ("timestamp" %in% names(process_row)) as.character(process_row$timestamp[[1]]) else ""
    psi_full <- number("memory_psi_full_avg10")
    oom_delta <- max(number("vm_oom_kill_delta", 0), number("cgroup_oom_kill_delta", 0), na.rm = TRUE)
    oom_total <- max(number("vm_oom_kill", 0), number("cgroup_oom_kill", 0), na.rm = TRUE)
    pressure_level <- if (oom_delta > 0) "critical" else if (is.finite(psi_full) && psi_full > 0) "warning" else "normal"
    model <- text_value("current_model")
    task <- if (nzchar(model)) paste0("model: ", model) else text_value("current_message")
    sampled_at <- text_value("timestamp")

    tags$div(
      class = "job-resource-panel",
      tags$div(
        class = "job-resource-header",
        tags$strong("Server resources"),
        tags$span(paste0("Sample: ", sampled_at, if (nzchar(task)) paste0(" / ", task) else ""))
      ),
      tags$div(
        class = "job-resource-grid",
        card(
          "Job process",
          if (process_alive) paste0(if (is.finite(cpu_pct)) round(cpu_pct) else "Sampling", if (is.finite(cpu_pct)) "% CPU" else "") else "Stopped",
          paste0(
            if (!process_alive && nzchar(process_sampled_at)) paste0("Last seen ", process_sampled_at, ": ") else "",
            fmt_mb(rss), " RSS / ", if (is.finite(process_count)) round(process_count) else "?", " processes / ", if (is.finite(threads)) round(threads) else "?", " threads"
          ),
          if (!process_alive) "neutral" else if (is.finite(cpu_pct) && is.finite(cpu_count)) severity(cpu_pct, cpu_count * 80, cpu_count * 95) else "normal"
        ),
        card(
          "Host CPU",
          paste0("Load ", if (is.finite(load1)) round(load1, 2) else "N/A"),
          paste0(if (is.finite(cpu_count)) round(cpu_count) else "?", " logical CPUs"),
          if (is.finite(load1) && is.finite(cpu_count)) severity(load1, cpu_count * 0.8, cpu_count) else "neutral"
        ),
        card(
          "Memory",
          fmt_pct(memory_pct),
          paste0(fmt_mb(memory_available), " free of ", fmt_mb(memory_total)),
          severity(memory_pct, 80, 90)
        ),
        card(
          "Swap",
          if (is.finite(swap_total) && swap_total > 0) fmt_pct(swap_pct) else "Disabled",
          if (is.finite(swap_total) && swap_total > 0) paste0(fmt_mb(swap_available), " free of ", fmt_mb(swap_total)) else "No swap configured",
          if (is.finite(swap_total) && swap_total > 0) severity(swap_pct, 50, 80) else "warning"
        ),
        card(
          "Disk",
          fmt_pct(disk_pct),
          paste0(fmt_mb(disk_available), " free of ", fmt_mb(disk_total)),
          severity(disk_pct, 80, 90)
        ),
        card(
          "Memory pressure",
          if (oom_delta > 0) paste0("OOM kill +", round(oom_delta)) else if (is.finite(psi_full)) paste0("PSI ", round(psi_full, 2)) else "No signal",
          paste0("OOM kills observed: ", round(oom_total)),
          pressure_level
        )
      )
    )
  })

  output$remote_job_running_summary <- renderUI({
    result <- remote_job_preview_result()
    if (!is.list(result) || !is.list(result$final_summary)) {
      return(NULL)
    }
    ml_final_summary_ui(result$final_summary)
  })

  remote_geo_metric_distribution_data <- function(result, stability = NULL) {
    if (!is.list(result) || !identical(result$kind %||% "", "geo_pipeline") || !is.list(result$tables)) {
      return(NULL)
    }
    tables <- result$tables
    metric_columns <- c("MedianMetric", "MeanMetric", "BestMetric", "MetricValue", "Metric", "R2", "Accuracy")
    candidates <- list()
    summary <- tables$transcript_ml_summary
    if (is.data.frame(summary) && nrow(summary) > 0) {
      if ("Phase" %in% names(summary)) {
        stability_rows <- summary[as.character(summary$Phase) == "stability", , drop = FALSE]
        if (nrow(stability_rows) > 0) {
          candidates[[length(candidates) + 1L]] <- list(rows = stability_rows, source = "stability")
        }
      }
      candidates[[length(candidates) + 1L]] <- list(rows = summary, source = "summary")
    }
    screening <- tables$transcript_ml_screening
    if (is.data.frame(screening) && nrow(screening) > 0) {
      candidates[[length(candidates) + 1L]] <- list(rows = screening, source = "screening")
    }
    if (length(candidates) == 0) {
      return(NULL)
    }

    for (candidate in candidates) {
      rows <- candidate$rows
      metric_column <- intersect(metric_columns, names(rows))[1]
      if (is.na(metric_column)) {
        next
      }
      values_all <- suppressWarnings(as.numeric(rows[[metric_column]]))
      ok <- is.finite(values_all)
      values <- values_all[ok]
      if (length(values) == 0) {
        next
      }
      rows_ok <- rows[ok, , drop = FALSE]
      metric_name <- metric_column
      if ("MetricName" %in% names(rows_ok)) {
        names_available <- unique(trimws(as.character(rows_ok$MetricName)))
        names_available <- names_available[nzchar(names_available) & !is.na(names_available)]
        if (length(names_available) > 0) {
          metric_name <- names_available[[1]]
        }
      }

      current_value <- NA_real_
      current_label <- ""
      if (is.list(stability) && isTRUE(stability$is_stability) && "GroupID" %in% names(rows_ok)) {
        group_rows <- which(as.character(rows_ok$GroupID) == stability$group_id)
        if (length(group_rows) > 0) {
          current_values <- suppressWarnings(as.numeric(rows_ok[[metric_column]][group_rows]))
          current_values <- current_values[is.finite(current_values)]
          if (length(current_values) > 0) {
            current_value <- current_values[[1]]
            current_label <- stability$group_id
          }
        }
      }

      return(list(
        metric_name = metric_name,
        metric_column = metric_column,
        source_label = candidate$source,
        values = values,
        current_value = current_value,
        current_label = current_label,
        n = length(values)
      ))
    }
    NULL
  }

  remote_geo_metric_values <- reactive({
    result <- remote_job_preview_result()
    resources <- remote_job_resources_data()
    status <- remote_status_with_live_message(remote_job_preview_status(), resources)
    stability <- remote_geo_stability_context(status, result)
    remote_geo_metric_distribution_data(result, stability)
  })

  remote_job_metric_values <- reactive({
    result <- remote_job_preview_result()
    if (!is.list(result) || !is.data.frame(result$results_table)) {
      return(NULL)
    }
    results <- result$results_table
    metric_name <- if ("R2" %in% names(results)) {
      "R2"
    } else if ("Accuracy" %in% names(results)) {
      "Accuracy"
    } else {
      NULL
    }
    if (is.null(metric_name)) {
      return(NULL)
    }
    ok_rows <- if ("Status" %in% names(results)) as.character(results$Status) == "OK" else rep(TRUE, nrow(results))
    values <- suppressWarnings(as.numeric(results[[metric_name]][ok_rows]))
    values <- values[is.finite(values)]
    list(metric_name = metric_name, values = values)
  })

  output$remote_job_running_details <- renderText({
    status <- remote_job_preview_status()
    result <- remote_job_preview_result()
    if (remote_status_is_geo(status) || (is.list(result) && identical(result$kind %||% "", "geo_pipeline"))) {
      return("")
    }
    geo_details <- remote_geo_stage_summary_text(status, result)
    metric_data <- remote_job_metric_values()
    if (is.null(metric_data) && !nzchar(geo_details)) {
      return("")
    }
    status_summary <- remote_status_summary_text(status)
    sections <- c(status_summary)
    if (nzchar(geo_details) && !grepl(geo_details, status_summary, fixed = TRUE)) {
      sections <- c(sections, geo_details)
    }
    if (!is.null(metric_data)) {
      sections <- c(sections, format_running_stability_signal(metric_data$values, metric_name = metric_data$metric_name))
    }
    paste(unique(sections[nzchar(sections)]), collapse = "\n\n")
  })

  output$remote_geo_metric_distribution <- plotly::renderPlotly({
    metric_data <- remote_geo_metric_values()
    if (is.null(metric_data) || length(metric_data$values) == 0) {
      return(plotly::plot_ly() |>
        plotly::layout(
          annotations = list(
            text = "Waiting for metric summary",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            xref = "paper",
            yref = "paper"
          ),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          margin = list(l = 35, r = 12, t = 20, b = 35)
        ))
    }

    values <- metric_data$values
    nbins <- max(5L, min(24L, ceiling(sqrt(length(values)) * 2)))
    plot_obj <- plotly::plot_ly()
    plot_obj <- plotly::add_histogram(
      plot_obj,
      x = values,
      nbinsx = nbins,
      histnorm = "probability density",
      marker = list(color = "#ccfbf1", line = list(color = "#14b8a6", width = 1)),
      name = "Observed groups",
      hovertemplate = paste0(metric_data$metric_name, ": %{x}<br>Density: %{y}<extra></extra>")
    )

    value_sd <- stats::sd(values)
    if (length(values) >= 2 && is.finite(value_sd) && value_sd > 0) {
      value_range <- range(values, finite = TRUE)
      pad <- diff(value_range) * 0.08
      if (!is.finite(pad) || pad <= 0) {
        pad <- value_sd
      }
      x_grid <- seq(value_range[[1]] - pad, value_range[[2]] + pad, length.out = 180)
      plot_obj <- plotly::add_lines(
        plot_obj,
        x = x_grid,
        y = stats::dnorm(x_grid, mean = mean(values), sd = value_sd),
        name = "Normal fit",
        line = list(color = "#ef4444", width = 2),
        hovertemplate = paste0(metric_data$metric_name, ": %{x:.4f}<br>Density: %{y:.4f}<extra></extra>")
      )
    }

    shapes <- list()
    annotations <- list()
    if (is.finite(metric_data$current_value)) {
      shapes <- list(list(
        type = "line",
        xref = "x",
        yref = "paper",
        x0 = metric_data$current_value,
        x1 = metric_data$current_value,
        y0 = 0,
        y1 = 1,
        line = list(color = "#7c3aed", width = 2, dash = "dot")
      ))
      annotations <- list(list(
        text = paste("current", metric_data$current_label),
        x = metric_data$current_value,
        y = 1,
        xref = "x",
        yref = "paper",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(color = "#5b21b6", size = 11)
      ))
    }

    plot_obj |>
      plotly::layout(
        title = list(
          text = paste0(
            metric_data$metric_name,
            " distribution from ", metric_data$source_label,
            " rows (n=", metric_data$n, ")"
          ),
          font = list(size = 13)
        ),
        xaxis = list(title = metric_data$metric_name, zeroline = FALSE),
        yaxis = list(title = "Density", zeroline = FALSE),
        bargap = 0.04,
        margin = list(l = 48, r = 18, t = 42, b = 46),
        legend = list(orientation = "h", x = 0, y = -0.22),
        shapes = shapes,
        annotations = annotations
      )
  })

  output$remote_job_metric_distribution <- plotly::renderPlotly({
    metric_data <- remote_job_metric_values()
    if (is.null(metric_data) || length(metric_data$values) == 0) {
      return(plotly::plot_ly() |>
        plotly::layout(
          annotations = list(
            text = "Waiting for metric results",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            xref = "paper",
            yref = "paper"
          ),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          margin = list(l = 35, r = 12, t = 35, b = 35)
        ))
    }
    values <- metric_data$values
    plot_obj <- plotly::plot_ly(
      x = values,
      type = "histogram",
      histnorm = "probability density",
      marker = list(color = "#dbeafe", line = list(color = "#60a5fa", width = 1)),
      name = "Observed",
      hovertemplate = paste0(metric_data$metric_name, ": %{x}<br>Density: %{y}<extra></extra>")
    )
    if (length(values) >= 2 && stats::sd(values) > 0) {
      x_grid <- seq(min(values), max(values), length.out = 160)
      plot_obj <- plot_obj |>
        plotly::add_lines(
          x = x_grid,
          y = stats::dnorm(x_grid, mean = mean(values), sd = stats::sd(values)),
          name = "Normal fit",
          line = list(color = "#ef4444", width = 2),
          hovertemplate = paste0(metric_data$metric_name, ": %{x:.4f}<br>Density: %{y:.4f}<extra></extra>")
        )
    }
    plot_obj |>
      plotly::layout(
        title = list(text = paste(metric_data$metric_name, "distribution"), font = list(size = 14)),
        xaxis = list(title = metric_data$metric_name, zeroline = FALSE),
        yaxis = list(title = "Density", zeroline = FALSE),
        bargap = 0.04,
        margin = list(l = 45, r = 12, t = 45, b = 45),
        legend = list(orientation = "h", x = 0, y = -0.22)
      )
  })

  output$remote_job_log_panel <- renderUI({
    log_text <- remote_job_log_text()
    if (!nzchar(trimws(log_text %||% ""))) {
      return(NULL)
    }
    tags$details(
      style = "margin-top: 8px;",
      open = TRUE,
      tags$summary(style = "cursor: pointer; font-weight: 600;", "Job log"),
      tags$pre(
        style = paste(
          "max-height: 260px; overflow: auto; white-space: pre-wrap;",
          "background: #f7f7f7; border: 1px solid #ddd; border-radius: 4px;",
          "padding: 10px; font-size: 12px;"
        ),
        htmltools::htmlEscape(log_text)
      )
    )
  })

  observeEvent(input$uncheck_all_ml, {
    updateCheckboxGroupInput(session, inputId = "ml_checkbox_group", selected = character(0))
  })

  observeEvent(input$check_all_ml, {
    updateCheckboxGroupInput(session, inputId = "ml_checkbox_group", selected = ml_available)
  })

  observeEvent(input$uncheck_all_ml_missing, {
    updateCheckboxGroupInput(session, inputId = "ml_missing_checkbox_group", selected = character(0))
  })
  observeEvent(input$check_all_ml_missing, {
    updateCheckboxGroupInput(session, inputId = "ml_missing_checkbox_group", selected = ml_not_available)
  })

  output$ml_table <- DT::renderDT(ml_data_table(), options = list(lengthChange = FALSE))

  output$ml_row_details <- renderPrint({
    selected_row <- input$ml_table_results_output_rows_selected
    if (length(selected_row) == 1) {
      row_data <- ml_table_results()[selected_row, ]
      selected_model_name <- ml_table_results()[selected_row, ]$Model
      specific_model <- all_models_reactive()[[selected_model_name]]
      ml_plot_importance(ml_prediction[[selected_model_name]])
      print(ml_plot_importance())
      tryCatch({
        importance <- varImp(specific_model)
        importance_df <- importance$importance
        print(importance_df)
        print(text_result_ml())
        print(summary(specific_model$finalModel))
      }, error = function(e) {
        print("Variable importance is available only for the best model kept in memory, or the selected model does not support it.")
      })
    }
  })

  output$ml_row_details_html <- renderUI({
    HTML(text_result_ml())
  })

  output$dynamic_ml_plot <- renderUI({
    if (!is.null(ml_plot_importance())) {
      plotOutput("ml_plot", height = "300px", width = "100%")
    } else {
      tags$div()
    }
  })

  output$ml_plot <- renderPlot({
    if (!is.null(ml_plot_importance())) {
      data <- ml_plot_importance()
      data$Residual <- data$Prediction.Predicted - data$Prediction.Actual
      residual_plot <- ggplot(data, aes(x = Prediction.Actual, y = Residual)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        geom_point(alpha = 0.5) +
        labs(x = "Actual Value", y = "Prediction Error (Residual)", title = "Residual Plot") +
        theme_minimal()
      residual_count_plot <- ggplot(data, aes(x = Residual)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        labs(x = "Prediction Error (Residual)", y = "Count", title = "Distribution of Prediction Errors") +
        theme_minimal()
      grid.arrange(residual_plot, residual_count_plot, ncol = 2)
    }
  })

  observeEvent(input$install_missing_modules, {
    models_to_install <- input$ml_missing_checkbox_group
    if (length(models_to_install) > 0) {
      ugPlotInstallModelDeps(models = models_to_install, dependencies = TRUE)
    }
    if (identical(input$ml_missing_strategy, "missforest") &&
      !("missForest" %in% rownames(installed.packages()))) {
      install.packages("missForest", dependencies = TRUE)
    }
    if (identical(input$ml_missing_strategy, "methylimp2") &&
      !("methyLImp2" %in% rownames(installed.packages()))) {
      if (!("BiocManager" %in% rownames(installed.packages()))) {
        install.packages("BiocManager", dependencies = TRUE)
      }
      BiocManager::install("methyLImp2", ask = FALSE, update = FALSE)
    }
    load_selected_ml_list()
  })

  observe({
    input$keepAlive
  })

  observeEvent(input$play_search_best_model_caret, {
    if (identical(input$ml_run_target, "remote")) {
      tryCatch({
        submit_remote_ml_job()
      }, error = function(e) {
        remote_job_status_text(paste("Remote submit failed:", conditionMessage(e)))
        updateTabsetPanel(session, "tabs", selected = "JOBS")
      })
      return(invisible(NULL))
    }

    cpu_limit <- configured_cpu_limit()
    apply_runtime_thread_limit(cpu_limit)
    parallel_enabled <- isTRUE(input$config_parallel_cubist_models)
    restart_parallel_each_model <- isTRUE(input$config_restart_parallel_each_model)
    retry_parallel_connection_errors <- isTRUE(input$config_retry_parallel_connection_errors)
    cl <- NULL
    start_main_cluster <- function() {
      stop_main_cluster()
      cl <<- parallel::makeCluster(cpu_limit)
      doParallel::registerDoParallel(cl)
      invisible(cl)
    }
    stop_main_cluster <- function() {
      if (!is.null(cl)) {
        try(parallel::stopCluster(cl), silent = TRUE)
        cl <<- NULL
      }
      foreach::registerDoSEQ()
      invisible(NULL)
    }
    if (parallel_enabled && !restart_parallel_each_model) {
      start_main_cluster()
    }
    on.exit({
      stop_main_cluster()
    }, add = TRUE)

    all_models_reactive(list())
    ml_prediction <<- list()
    best_model_object(NULL)
    best_model_preprocess(NULL)
    ml_error_message_text(paste0(
      "Machine Learning will use up to ", cpu_limit, " CPU thread",
      if (cpu_limit == 1L) "" else "s", "./"
    ))
    ml_final_summary(NULL)
    best_model_name <- "-"
    search_start_time <- proc.time()[["elapsed"]]

    tryCatch({
      withProgress(message = 'Searching the best model...', {
        best_result <- -Inf
        best_model <- "-"
        best_model_name <- "-"
        best_dataset_seed <- NA
        best_training_seed <- NA
        best_mae <- NA_real_
        best_rmse <- NA_real_
        worst_result <- Inf
        worst_model <- "-"
        model_metric_values <- list()
        model_mae_values <- list()
        model_rmse_values <- list()
        invalid_runs <- 0
        invalid_models <- character(0)
        model_invalid_runs <- list()
        target_name <- input$ml_target
        X <- changed_table[input$row_checkbox_group, input$column_checkbox_group]
        Y <- X[[target_name]]
        cols_to_convert <- input$checkbox_group_categories
        if (length(cols_to_convert) > 0) {
          for (this_target in cols_to_convert) {
            if (!is.null(X[[this_target]])) {
              X[[this_target]] <- as.factor(X[[this_target]])
              if (length(levels(X[[this_target]])) == 1) {
                X[[this_target]] <- as.numeric(rep(1, nrow(X)))
              }
              if (this_target == target_name) {
                Y <- as.factor(dff[[target_name]])
                freq_table <- table(Y)
                single_item_levels <- names(freq_table[freq_table <= 2])
                toKeep <- !(Y %in% single_item_levels)
                Y <- Y[toKeep]
                X <- X[toKeep, ]
                Y <- droplevels(Y)
                X[[this_target]] <- droplevels(X[[this_target]])
              }
            }
          }
        }
        X_base <- X
        Y_base <- Y
        ml_table_results(data.frame())
        write_checkpoint_log(last_model = "-", results_table = ml_table_results(), context = list(phase = "starting"))
        append_ml_result_row <- function(row) {
          current_results <- ml_table_results()
          row <- as.data.frame(row, stringsAsFactors = FALSE)
          if (!is.data.frame(current_results) || nrow(current_results) == 0) {
            ml_table_results(row)
            return(invisible(row))
          }
          all_columns <- union(names(current_results), names(row))
          for (column_name in setdiff(all_columns, names(current_results))) {
            current_results[[column_name]] <- NA
          }
          for (column_name in setdiff(all_columns, names(row))) {
            row[[column_name]] <- NA
          }
          current_results <- current_results[, all_columns, drop = FALSE]
          row <- row[, all_columns, drop = FALSE]
          ml_table_results(rbind(current_results, row))
          invisible(row)
        }
        do_dataset_seed <- 0
        loop_dataset_seedi <- as.numeric(input$ml_dataset_seedi)
        loop_dataset_seedf <- as.numeric(input$ml_dataset_seedf)
        all_models <- input$ml_checkbox_group
        active_models <- all_models
        skipped_models <- character(0)
        auto_skip_enabled <- isTRUE(input$ml_auto_skip_bad_models)
        min_r2_threshold <- suppressWarnings(as.numeric(input$ml_min_r2_skip))
        if (is.na(min_r2_threshold)) {
          min_r2_threshold <- 0
        }
        min_r2_threshold <- max(0, min(1, min_r2_threshold))
        mark_model_skipped <- function(model_name, reason) {
          if (!auto_skip_enabled || !(model_name %in% active_models)) {
            return(invisible(NULL))
          }
          active_models <<- setdiff(active_models, model_name)
          skipped_models <<- union(skipped_models, model_name)
          ml_error_message_text(paste(
            ml_error_message_text(),
            " ", "AUTO-SKIP:", model_name, "-", reason, "(next rounds)/"
          ))
          invisible(NULL)
        }
        loop_seedi <- as.numeric(input$ml_seedi)
        loop_seedf <- as.numeric(input$ml_seedf)
        dataset_seed_values <- if (!is.na(loop_dataset_seedi) && !is.na(loop_dataset_seedf)) {
          seq(loop_dataset_seedi, loop_dataset_seedf)
        } else {
          1
        }
        training_seed_values <- if (!is.na(loop_seedi) && !is.na(loop_seedf)) {
          seq(loop_seedi, loop_seedf)
        } else {
          1
        }
        total_seed_runs <- length(training_seed_values)
        total_dataset_runs <- length(dataset_seed_values)
        total_model_runs <- max(1, length(all_models))
        total_search_runs <- max(1, total_dataset_runs * total_model_runs * total_seed_runs)
        completed_search_runs <- 0
        last_progress_value <- 0
        metric_label <- if (is.factor(Y)) "Accuracy" else "R2 (MAE/RMSE na tabela)"
        metric_name <- if (is.factor(Y)) "Accuracy" else "R2"
        running_progress_detail <- function(model_name, loop_dataset_seed, loop_seed,
                                            count_model, active_model_count,
                                            seed_position, total_seed_runs,
                                            dataset_position, total_dataset_runs,
                                            current_run_index, total_search_runs) {
          metric_values <- suppressWarnings(as.numeric(unlist(model_metric_values, use.names = FALSE)))
          metric_values <- metric_values[is.finite(metric_values)]
          mean_label <- if (length(metric_values) > 0) round(mean(metric_values), 4) else "N/A"
          median_label <- if (length(metric_values) > 0) round(median(metric_values), 4) else "N/A"
          stability_signal <- format_running_stability_signal(metric_values, metric_name = metric_name)
          metric_distribution <- format_running_metric_distribution(metric_values, metric_name = metric_name)
          progress_percent <- round((current_run_index / total_search_runs) * 100)
          best_model_label <- if (is.finite(best_result)) {
            best_model
          } else {
            "N/A"
          }
          best_metric_label <- if (is.finite(best_result)) {
            round(best_result, 4)
          } else {
            "N/A"
          }
          worst_label <- if (is.finite(worst_result)) {
            paste0(round(worst_result, 4), " - ", worst_model)
          } else {
            "N/A"
          }
          best_metric_name <- if (identical(metric_name, "R2")) "R2" else metric_name

          paste0(
            "\n",
            "BEST MODEL : ", best_model_label, "\n",
            "BEST ", if (identical(best_metric_name, "R2")) "R2" else best_metric_name, " : ", best_metric_label, "\n\n",
            "MEDIAN ", if (identical(best_metric_name, "R2")) "R2" else best_metric_name, " : ", median_label, "\n\n",
            "Progress: ", current_run_index, "/", total_search_runs, " (", progress_percent, "%)",
            " | Model: ", count_model, "/", max(1, active_model_count), "\n",
            "Running: ", model_name, "\n",
            "Seed: dataset ", loop_dataset_seed, " (", dataset_position, "/", total_dataset_runs,
            ") | train ", loop_seed, " (", seed_position, "/", total_seed_runs, ")\n\n",
            "Worst ", best_metric_name, ": ", worst_label, "\n",
            "Current summary: mean ", mean_label, " | median ", median_label, "\n",
            stability_signal, "\n\n",
            metric_distribution
          )
        }
        set_search_progress <- function(model_name, loop_dataset_seed, loop_seed,
                                        count_model, active_model_count,
                                        seed_position, dataset_position,
                                        progress_runs = completed_search_runs) {
          current_run_index <- min(total_search_runs, max(1, progress_runs))
          progress_value <- min(1, max(0, progress_runs / total_search_runs))
          progress_delta <- max(0, progress_value - last_progress_value)
          last_progress_value <<- max(last_progress_value, progress_value)
          incProgress(
            amount = progress_delta,
            detail = running_progress_detail(
              model_name = model_name,
              loop_dataset_seed = loop_dataset_seed,
              loop_seed = loop_seed,
              count_model = count_model,
              active_model_count = active_model_count,
              seed_position = seed_position,
              total_seed_runs = total_seed_runs,
              dataset_position = dataset_position,
              total_dataset_runs = total_dataset_runs,
              current_run_index = current_run_index,
              total_search_runs = total_search_runs
            )
          )
        }
        if (!is.na(loop_dataset_seedi) && !is.na(loop_dataset_seedf)) {
          do_dataset_seed <- 1
        }
        for (dataset_position in seq_along(dataset_seed_values)) {
          loop_dataset_seed <- dataset_seed_values[[dataset_position]]
          preprocess_meta_for_seed <- NULL
          X <- X_base
          Y <- Y_base
          if (do_dataset_seed == 1) {
            set.seed(loop_dataset_seed)
            print(paste("SEED: ", loop_dataset_seed))
          }
          threshold_scope <- "full_before_split"
          imputation_scope <- input$ml_imputation_scope
          missing_definition <- input$ml_missing_definition
          if (is.null(missing_definition)) {
            missing_definition <- c("empty", "na")
          }
          zero_exceptions <- input$ml_zero_exceptions
          if (is.null(zero_exceptions)) {
            zero_exceptions <- character(0)
          }
          missing_filter_order <- "auto"
          print(paste("Threshold scope:", threshold_scope, "| Missing strategy:", input$ml_missing_strategy, "| Imputation scope:", imputation_scope, "| Filter order:", missing_filter_order))
          if (identical(threshold_scope, "full_before_split")) {
            predictors_all <- X[, setdiff(colnames(X), target_name), drop = FALSE]
            filtered_all <- apply_missing_filters_resolved(
              predictors = predictors_all,
              missing_definition = missing_definition,
              zero_exceptions = zero_exceptions,
              threshold_cols = input$ml_missing_threshold_cols,
              threshold_rows = input$ml_missing_threshold_rows,
              filter_order = missing_filter_order,
              min_rows_retained = (input$ml_complete_case_min_samples %||% 80) / 100,
              mode = if (identical(input$ml_missing_strategy, "none")) "complete_case" else "balanced"
            )
            X <- cbind(X[filtered_all$keep_rows, target_name, drop = FALSE], filtered_all$filtered_predictors)
            names(X)[1] <- target_name
            Y <- X[[target_name]]
          }
          missing_strategy <- input$ml_missing_strategy
          if (identical(imputation_scope, "full_once") && !identical(missing_strategy, "none")) {
            preprocessed_full <- apply_missing_strategy(
              trainSet = X,
              testSet = X[0, , drop = FALSE],
              target_name = target_name,
              strategy = missing_strategy,
              missing_definition = missing_definition,
              zero_exceptions = zero_exceptions,
              threshold_cols = input$ml_missing_threshold_cols,
              threshold_rows = input$ml_missing_threshold_rows,
              threshold_scope = "full_before_split",
              filter_order = missing_filter_order,
              min_rows_retained = (input$ml_complete_case_min_samples %||% 80) / 100
            )
            X <- preprocessed_full$train_set
            Y <- X[[target_name]]
            preprocess_meta_for_seed <- preprocessed_full$preprocess_meta
          }
          trainIndex <- createDataPartition(Y, p = .8, list = FALSE, times = 1)
          trainSet <- X[trainIndex, ]
          testSet  <- X[-trainIndex, ]
          if (!is.data.frame(trainSet)) {
            trainSet <- as.data.frame(trainSet)
          }
          if (!is.data.frame(testSet)) {
            testSet <- as.data.frame(testSet)
          }
          strategy_after_split <- if (identical(imputation_scope, "full_once")) "none" else missing_strategy
          if (!identical(threshold_scope, "full_before_split")) {
            processed_data <- apply_missing_strategy(
              trainSet = trainSet,
              testSet = testSet,
              target_name = target_name,
              strategy = strategy_after_split,
              missing_definition = missing_definition,
              zero_exceptions = zero_exceptions,
              threshold_cols = input$ml_missing_threshold_cols,
              threshold_rows = input$ml_missing_threshold_rows,
              threshold_scope = threshold_scope,
              filter_order = missing_filter_order,
              min_rows_retained = (input$ml_complete_case_min_samples %||% 80) / 100
            )
            trainSet <- processed_data$train_set
            testSet <- processed_data$test_set
            preprocess_meta_for_seed <- processed_data$preprocess_meta
          }
          if (nrow(trainSet) < 5 || ncol(trainSet) < 2) {
            ml_error_message_text(paste(ml_error_message_text(), " ", "Not enough data after missing strategy for seed", loop_dataset_seed, "/"))
            invalid_runs <- invalid_runs + (length(active_models) * total_seed_runs)
            invalid_models <- union(invalid_models, active_models)
            for (model_name in active_models) {
              current_invalid <- if (!is.null(model_invalid_runs[[model_name]])) model_invalid_runs[[model_name]] else 0
              model_invalid_runs[[model_name]] <- current_invalid + total_seed_runs
            }
            completed_search_runs <- completed_search_runs + (total_model_runs * total_seed_runs)
            incProgress(amount = max(0, min(1, completed_search_runs / total_search_runs) - last_progress_value))
            last_progress_value <- max(last_progress_value, min(1, completed_search_runs / total_search_runs))
            next
          }
          count_model <- 0
          do_seed <- 0
          if (!is.na(loop_seedi) && !is.na(loop_seedf)) {
            do_seed <- 1
          }
          for (model_name in all_models) {
            if (!(model_name %in% active_models)) {
              completed_search_runs <- completed_search_runs + total_seed_runs
              incProgress(amount = max(0, min(1, completed_search_runs / total_search_runs) - last_progress_value))
              last_progress_value <- max(last_progress_value, min(1, completed_search_runs / total_search_runs))
              next
            }
            count_model <- count_model + 1
            tryCatch({
              model_info <- getModelInfo(model_name, regex = FALSE)[[model_name]]
              model_libraries <- model_info$library
              for (lib in model_libraries) {
                library(lib, character.only = TRUE)
                print(paste("Loading library: ", lib))
              }
            }, error = function(e) {
              print(paste("Failed to load", model_name))
            })
            safe_num <- function(value, fallback) {
              parsed <- suppressWarnings(as.numeric(value))
              if (is.na(parsed)) fallback else parsed
            }
            cv_settings <- switch(input$ml_performance_mode,
              "high_effort" = list(method = "repeatedcv", number = 10, repeats = 3, tune_length = 10),
              "custom" = list(
                method = input$ml_cv_method,
                number = max(2, safe_num(input$ml_cv_folds, 10)),
                repeats = max(1, safe_num(input$ml_cv_repeats, 1)),
                tune_length = max(1, safe_num(input$ml_tune_length, 3))
              ),
              list(method = "cv", number = 10, repeats = 1, tune_length = 3)
            )
            ctrl <- if (identical(cv_settings$method, "repeatedcv")) {
              trainControl(method = "repeatedcv", number = cv_settings$number, repeats = cv_settings$repeats)
            } else {
              trainControl(method = "cv", number = cv_settings$number)
            }
            ctrl$allowParallel <- parallel_enabled
            if (!parallel_enabled) {
              ctrl$allowParallel <- FALSE
              ml_error_message_text(paste(
                ml_error_message_text(),
                " Parallel processing disabled./"
              ))
            }
            model_types <- model_info$type
            print(paste("Model", model_name, "supports types:", paste(model_types, collapse = ", ")))
            for (seed_position in seq_along(training_seed_values)) {
              loop_seed <- training_seed_values[[seed_position]]
              current_run_index <- ((dataset_position - 1) * total_model_runs * total_seed_runs) +
                ((count_model - 1) * total_seed_runs) +
                seed_position
              if (do_seed == 1) {
                set.seed(loop_seed)
              }
              tryCatch({
                formula <- as.formula(paste(target_name, "~ ."))
                model <- NULL
                pred <- NULL
                run_status <- "OK"
                run_error <- ""
                attempt_start_time <- proc.time()[["elapsed"]]
                classify_training_error <- function(error_message) {
                  if (grepl("wrong model type for (regression|classification)", error_message, ignore.case = TRUE)) {
                    "INCOMPATIBLE"
                  } else if (identical(error_message, "Stopping")) {
                    "INVALID_METRICS"
                  } else {
                    "ERROR"
                  }
                }
                format_training_error <- function(error_message) {
                  if (identical(error_message, "Stopping")) {
                    "The model did not produce usable predictions for this split"
                  } else {
                    error_message
                  }
                }
                checkpoint_context <- function(phase) {
                  list(
                    phase = phase,
                    current_model = model_name,
                    dataset_seed = loop_dataset_seed,
                    training_seed = loop_seed,
                    current_run = current_run_index,
                    total_runs = total_search_runs,
                    model_position = count_model,
                    total_models = total_model_runs,
                    best_model = if (is.finite(best_result)) best_model else "N/A",
                    best_metric = if (is.finite(best_result)) round(best_result, 4) else "N/A"
                  )
                }
                write_checkpoint_log(
                  last_model = model_name,
                  results_table = ml_table_results(),
                  context = checkpoint_context("running")
                )
                set_search_progress(
                  model_name = model_name,
                  loop_dataset_seed = loop_dataset_seed,
                  loop_seed = loop_seed,
                  count_model = count_model,
                  active_model_count = total_model_runs,
                  seed_position = seed_position,
                  dataset_position = dataset_position,
                  progress_runs = current_run_index
                )
                is_parallel_connection_error <- function(error) {
                  grepl(
                    "error (writing|reading) to connection|serialize|unserialize|SOCK",
                    conditionMessage(error),
                    ignore.case = TRUE
                  )
                }
                train_model_once <- function() {
                  if (parallel_enabled && restart_parallel_each_model && isTRUE(ctrl$allowParallel)) {
                    start_main_cluster()
                    on.exit(stop_main_cluster(), add = TRUE)
                  }
                  withTimeout({
                    model <- caret::train(
                      formula,
                      data = trainSet,
                      method = model_name,
                      trControl = ctrl,
                      tuneLength = cv_settings$tune_length
                    )
                    model
                  }, timeout = input$ml_timeout, onTimeout = "error")
                }
                result <- tryCatch({
                  tryCatch(
                    train_model_once(),
                    error = function(e) {
                      if (parallel_enabled &&
                          retry_parallel_connection_errors &&
                          is_parallel_connection_error(e)) {
                        ml_error_message_text(paste(
                          ml_error_message_text(),
                          " Parallel worker failed for", model_name, "- retrying once./"
                        ))
                        print(paste("Retrying model after parallel worker failure:", model_name, conditionMessage(e)))
                        stop_main_cluster()
                        if (!restart_parallel_each_model) {
                          start_main_cluster()
                        }
                        return(train_model_once())
                      }
                      stop(e)
                    }
                  )
                }, TimeoutException = function(ex) {
                  run_status <<- "TIMEOUT"
                  run_error <<- paste0("Timed out after ", input$ml_timeout, " seconds")
                  ml_error_message_text(paste(ml_error_message_text(), " ", "TIMEOUT:", model_name, "/"))
                  print(paste("Training timed out for model:", model_name))
                  mark_model_skipped(model_name, "timeout")
                  return(NULL)
                }, error = function(e) {
                  raw_error <- conditionMessage(e)
                  run_error <<- format_training_error(raw_error)
                  run_status <<- classify_training_error(raw_error)
                  print(paste("Error training model", model_name, ":", conditionMessage(e)))
                  return(NULL)
                })
                if (is.null(result)) {
                  elapsed_seconds <- round(proc.time()[["elapsed"]] - attempt_start_time, 3)
                  failed_result <- data.frame(
                    Model = model_name,
                    Status = run_status,
                    elapsed_seconds = elapsed_seconds,
                    Error = run_error,
                    dataset_seed = loop_dataset_seed,
                    training_seed = loop_seed,
                    threshold_scope = threshold_scope,
                    imputation_scope = imputation_scope
                  )
                  append_ml_result_row(failed_result)
                  write_checkpoint_log(
                    last_model = model_name,
                    results_table = ml_table_results(),
                    context = checkpoint_context(run_status)
                  )
                  next
                }
                model <- result
                if (is.null(pred)) {
                  pred <- predict(model, newdata = testSet)
                }
                if (is.null(pred) || length(pred) == 0) {
                  stop("model returned empty predictions")
                }

                pred_indices <- names(pred)
                if (!is.null(pred_indices) && length(pred_indices) == length(pred) && all(pred_indices %in% rownames(testSet))) {
                  actual_values <- testSet[pred_indices, target_name, drop = TRUE]
                } else if (length(pred) == nrow(testSet)) {
                  actual_values <- testSet[[target_name]]
                } else {
                  min_len <- min(length(pred), nrow(testSet))
                  pred <- pred[seq_len(min_len)]
                  actual_values <- testSet[[target_name]][seq_len(min_len)]
                }

                if (length(actual_values) != length(pred)) {
                  stop("prediction length does not match target length")
                }

                ml_pred_real <- data.frame(Actual = actual_values, Predicted = pred)
                model_prediction <- data.frame(Model = model_name, "Prediction" = ml_pred_real)
                ml_prediction[[model_name]] <<- model_prediction
                if (is.factor(actual_values)) {
                  accuracy <- sum(pred == actual_values) / length(pred)
                  if (accuracy > best_result) {
                    best_result <- accuracy
                    best_model <- paste(model_name, "(", loop_dataset_seed, ":", loop_seed, ")")
                    best_model_name <- model_name
                    best_dataset_seed <- loop_dataset_seed
                    best_training_seed <- loop_seed
                    best_mae <- NA_real_
                    best_rmse <- NA_real_
                    best_model_object(model)
                    best_model_preprocess(preprocess_meta_for_seed)
                    all_models_reactive(stats::setNames(list(model), model_name))
                  }
                  if (accuracy < worst_result) {
                    worst_result <- accuracy
                    worst_model <- paste(model_name, "(", loop_dataset_seed, ":", loop_seed, ")")
                  }
                  model_results <- data.frame(Model = model_name,
                    Status = "OK",
                    elapsed_seconds = round(proc.time()[["elapsed"]] - attempt_start_time, 3),
                    Accuracy = accuracy,
                    Error = "",
                    dataset_seed = loop_dataset_seed,
                    training_seed = loop_seed,
                    threshold_scope = threshold_scope,
                    imputation_scope = imputation_scope)
                  append_ml_result_row(model_results)
                  model_metric_values[[model_name]] <- c(model_metric_values[[model_name]], accuracy)
                } else {
                  result_pred <- postResample(pred, actual_values)
                  rsq_value <- unname(result_pred["Rsquared"])
                  mae_value <- unname(result_pred["MAE"])
                  rmse_value <- unname(result_pred["RMSE"])
                  if (is.na(rsq_value) || is.na(mae_value) || is.na(rmse_value)) {
                    stop("regression metrics returned NA (check missing values after threshold filtering)")
                  }
                  if (rsq_value > best_result) {
                    best_result <- rsq_value
                    best_model <- paste(model_name, "(", loop_dataset_seed, ":", loop_seed, ")")
                    best_model_name <- model_name
                    best_dataset_seed <- loop_dataset_seed
                    best_training_seed <- loop_seed
                    best_mae <- mae_value
                    best_rmse <- rmse_value
                    best_model_object(model)
                    best_model_preprocess(preprocess_meta_for_seed)
                    all_models_reactive(stats::setNames(list(model), model_name))
                  }
                  if (rsq_value < worst_result) {
                    worst_result <- rsq_value
                    worst_model <- paste(model_name, "(", loop_dataset_seed, ":", loop_seed, ")")
                  }
                  model_results <- data.frame(Model = model_name,
                    Status = "OK",
                    elapsed_seconds = round(proc.time()[["elapsed"]] - attempt_start_time, 3),
                    "R2" = rsq_value,
                    MAE = mae_value,
                    RMSE = rmse_value,
                    Error = "",
                    dataset_seed = loop_dataset_seed,
                    training_seed = loop_seed,
                    threshold_scope = threshold_scope,
                    imputation_scope = imputation_scope)
                  append_ml_result_row(model_results)
                  model_metric_values[[model_name]] <- c(model_metric_values[[model_name]], rsq_value)
                  model_mae_values[[model_name]] <- c(model_mae_values[[model_name]], mae_value)
                  model_rmse_values[[model_name]] <- c(model_rmse_values[[model_name]], rmse_value)
                  if (auto_skip_enabled && rsq_value < min_r2_threshold) {
                    mark_model_skipped(
                      model_name,
                      paste0("R2 ", round(rsq_value, 4), " < ", round(min_r2_threshold, 4))
                    )
                  }
                }
                current_results <- ml_table_results()
                sort_column <- if ("Accuracy" %in% names(current_results)) {
                  "Accuracy"
                } else if ("R2" %in% names(current_results)) {
                  "R2"
                } else {
                  NULL
                }
                if (!is.null(sort_column) && nrow(current_results) > 0) {
                  ordered_idx <- order(-as.numeric(as.character(current_results[[sort_column]])))
                  print(head(current_results[ordered_idx, , drop = FALSE], 10))
                } else {
                  print(current_results)
                }
                write_checkpoint_log(
                  last_model = model_name,
                  results_table = current_results,
                  context = checkpoint_context("OK")
                )
              }, error = function(e) {
                elapsed_seconds <- if (exists("attempt_start_time", inherits = FALSE)) {
                  round(proc.time()[["elapsed"]] - attempt_start_time, 3)
                } else {
                  NA_real_
                }
                invalid_runs <- invalid_runs + 1
                invalid_models <- union(invalid_models, model_name)
                current_invalid <- if (!is.null(model_invalid_runs[[model_name]])) model_invalid_runs[[model_name]] else 0
                model_invalid_runs[[model_name]] <- current_invalid + 1
                failed_result <- data.frame(
                  Model = model_name,
                  Status = classify_training_error(conditionMessage(e)),
                  elapsed_seconds = elapsed_seconds,
                  Error = format_training_error(conditionMessage(e)),
                  dataset_seed = loop_dataset_seed,
                  training_seed = loop_seed,
                  threshold_scope = threshold_scope,
                  imputation_scope = imputation_scope
                )
                append_ml_result_row(failed_result)
                write_checkpoint_log(
                  last_model = model_name,
                  results_table = ml_table_results(),
                  context = checkpoint_context(classify_training_error(conditionMessage(e)))
                )
                ml_error_message_text(paste(ml_error_message_text(), " ", "Couldn't run model", model_name, ":", conditionMessage(e)))
                print(paste("Couldn't run model", model_name, ":", conditionMessage(e)))
              }, finally = {
                completed_search_runs <<- completed_search_runs + 1
                set_search_progress(
                  model_name = model_name,
                  loop_dataset_seed = loop_dataset_seed,
                  loop_seed = loop_seed,
                  count_model = count_model,
                  active_model_count = total_model_runs,
                  seed_position = seed_position,
                  dataset_position = dataset_position,
                  progress_runs = completed_search_runs
                )
              })
            }
          }
        }
        metric_name <- if (is.factor(Y_base)) "Accuracy" else "R2"
        dataset_seed_label <- if (!is.na(best_dataset_seed)) best_dataset_seed else "N/A"
        training_seed_label <- if (!is.na(best_training_seed)) best_training_seed else "N/A"
        robust_stats_rows <- lapply(all_models, function(model_name) {
          metrics <- model_metric_values[[model_name]]
          metrics <- metrics[is.finite(metrics)]
          mean_metric <- if (length(metrics) > 0) round(mean(metrics), 4) else NA_real_
          median_metric <- if (length(metrics) > 0) round(median(metrics), 4) else NA_real_
          iqr_metric <- if (length(metrics) > 1) round(IQR(metrics), 4) else NA_real_
          min_metric <- if (length(metrics) > 0) round(min(metrics), 4) else NA_real_
          max_metric <- if (length(metrics) > 0) round(max(metrics), 4) else NA_real_
          range_metric <- if (length(metrics) > 1) round(diff(range(metrics)), 4) else NA_real_
          data.frame(
            Model = model_name,
            MeanMetric = mean_metric,
            MedianMetric = median_metric,
            IQRMetric = iqr_metric,
            MinMetric = min_metric,
            MaxMetric = max_metric,
            RangeMetric = range_metric,
            stringsAsFactors = FALSE
          )
        })
        robust_stats <- do.call(rbind, robust_stats_rows)
        if (is.data.frame(robust_stats) && nrow(robust_stats) > 0) {
          robust_stats <- robust_stats[order(-robust_stats$MedianMetric, robust_stats$Model), , drop = FALSE]
        }
        best_model_metrics <- model_metric_values[[best_model_name]]
        best_model_metrics <- best_model_metrics[is.finite(best_model_metrics)]
        best_model_mae <- model_mae_values[[best_model_name]]
        best_model_mae <- best_model_mae[is.finite(best_model_mae)]
        best_model_rmse <- model_rmse_values[[best_model_name]]
        best_model_rmse <- best_model_rmse[is.finite(best_model_rmse)]
        final_results <- ml_table_results()
        if (is.data.frame(final_results) && "Status" %in% names(final_results)) {
          status_values <- as.character(final_results$Status)
          ok_runs <- sum(status_values == "OK", na.rm = TRUE)
          timeout_runs <- sum(status_values == "TIMEOUT", na.rm = TRUE)
          incompatible_runs <- sum(status_values == "INCOMPATIBLE", na.rm = TRUE)
          invalid_metric_runs <- sum(status_values == "INVALID_METRICS", na.rm = TRUE)
          error_runs <- sum(status_values == "ERROR", na.rm = TRUE)
        } else {
          ok_runs <- 0L
          timeout_runs <- 0L
          incompatible_runs <- 0L
          invalid_metric_runs <- 0L
          error_runs <- 0L
        }
        ml_final_summary(list(
          best_model = best_model_name,
          dataset_seed = dataset_seed_label,
          training_seed = training_seed_label,
          metric_name = metric_name,
          metric_value = if (is.finite(best_result)) best_result else NA_real_,
          best_model_min = if (length(best_model_metrics) > 0) round(min(best_model_metrics), 4) else "N/A",
          best_model_max = if (length(best_model_metrics) > 0) round(max(best_model_metrics), 4) else "N/A",
          best_model_mean = if (length(best_model_metrics) > 0) round(mean(best_model_metrics), 4) else "N/A",
          best_model_median = if (length(best_model_metrics) > 0) round(median(best_model_metrics), 4) else "N/A",
          best_model_iqr = if (length(best_model_metrics) > 1) round(IQR(best_model_metrics), 4) else "N/A",
          best_model_range = if (length(best_model_metrics) > 1) round(diff(range(best_model_metrics)), 4) else "N/A",
          best_model_mae_median = if (length(best_model_mae) > 0) round(median(best_model_mae), 4) else "N/A",
          best_model_mae_iqr = if (length(best_model_mae) > 1) round(IQR(best_model_mae), 4) else "N/A",
          best_model_rmse_median = if (length(best_model_rmse) > 0) round(median(best_model_rmse), 4) else "N/A",
          best_model_rmse_iqr = if (length(best_model_rmse) > 1) round(IQR(best_model_rmse), 4) else "N/A",
          mae = if (identical(metric_name, "R2")) best_mae else NA_real_,
          rmse = if (identical(metric_name, "R2")) best_rmse else NA_real_,
          total_elapsed_seconds = round(proc.time()[["elapsed"]] - search_start_time, 3),
          ok_runs = ok_runs,
          timeout_runs = timeout_runs,
          incompatible_runs = incompatible_runs,
          invalid_metric_runs = invalid_metric_runs,
          error_runs = error_runs,
          model_robust_stats = robust_stats
        ))
        if (auto_skip_enabled && length(skipped_models) > 0) {
          updateCheckboxGroupInput(
            session,
            inputId = "ml_checkbox_group",
            selected = setdiff(input$ml_checkbox_group, skipped_models)
          )
        }
      })
    }, error = function(e) {
      ml_error_message_text(paste(ml_error_message_text(), " ", conditionMessage(e)))
      print(e)
    })
    if (!is.null(best_model_object()) && !is.null(best_model_name) && nzchar(best_model_name) && !identical(best_model_name, "-")) {
      all_models_reactive(stats::setNames(list(best_model_object()), best_model_name))
    } else {
      all_models_reactive(list())
    }
    stop_main_cluster()
  })

  # Tab 6) MODEL ANALYSIS: Carrega o modelo e detecta variavel-alvo
observeEvent(input$model_file, {
  req(input$model_file)
  tryCatch({
    # 1) Carrega o objeto
    loaded_obj <- readRDS(input$model_file$datapath)
    model_obj <- loaded_obj
    preprocess_meta <- NULL
    if (is.list(loaded_obj) && !is.null(loaded_obj$model)) {
      model_obj <- loaded_obj$model
      preprocess_meta <- loaded_obj$preprocess_meta
    }
    loaded_model(list(model = model_obj, preprocess_meta = preprocess_meta))

    # 2) Mostra resumo do modelo
    output$model_details <- renderPrint({
      print(summary(model_obj))
    })
    output$model_preprocess_ui <- renderUI({
      if (!is.null(preprocess_meta) && !is.null(preprocess_meta$strategy)) {
        tags$p(
          strong("Model preprocessing: "),
          tags$span(toupper(preprocess_meta$strategy), style = "color: darkgreen;"),
          " (will be applied automatically in analysis)"
        )
      } else {
        tags$p(
          strong("Model preprocessing: "),
          tags$span("not available in this RDS", style = "color: #B22222;"),
          " (compatibility mode)"
        )
      }
    })

    # 3) Prepara vetor de colunas ativas do dataset (selecao da aba TABLE)
    active_cols <- input$column_checkbox_group %||% character(0)
    cols_dataset <- intersect(active_cols, colnames(changed_table))
    if (length(cols_dataset) == 0) {
      cols_dataset <- colnames(changed_table)
    }

    # 4) Detecta variavel-alvo em varias etapas
    model_target <- ""

    # 4.1) Se for um objeto caret::train, o call$formula guarda a formula
    if (!is.null(model_obj$call$formula)) {
      model_target <- as.character(model_obj$call$formula[[2]])
    }
    # 4.2) Caso seja um objeto randomForest puro treinado por formula
    else if (inherits(model_obj, "randomForest") && !is.null(model_obj$terms)) {
      # extrai a segunda variavel dos terms
      vars <- as.character(attr(model_obj$terms, "variables"))
      if (length(vars) >= 2) model_target <- vars[2]
    }
    # 4.3) Fallback para caret::train (se por algum motivo o formula sumiu):
    #      pegamos o .outcome no trainingData (mas ai o nome real nao fica disponivel)
    else if (!is.null(model_obj$trainingData)) {
      # a coluna .outcome guarda o vetor de resposta
      if (".outcome" %in% colnames(model_obj$trainingData)) {
        # nao e o nome original, mas mostramos ao menos que veio do treinamento
        model_target <- ".outcome"
      }
    }

    # 4.4) Se ainda vazio ou nao estiver nas colunas ativas, usar o que o usuario selecionou
    selected_manual <- input$dataset_response_col
    if (!(model_target %in% cols_dataset)) {
      if (!is.null(selected_manual) && selected_manual %in% cols_dataset) {
        model_target <- selected_manual
      } else if (length(cols_dataset) > 0) {
        model_target <- cols_dataset[[1]]
      } else {
        model_target <- ""
      }
    }

    # 5) Exibe na UI SEMPRE o nome que vamos usar como target
    output$model_target_var_ui <- renderUI({
      tags$p(
        strong("Model target:"),
        tags$span(model_target, style = "color: steelblue;")
      )
    })

    # 6) Atualiza o selectInput do dataset com as colunas disponiveis,
    #    e ja seleciona a variavel-alvo detectada (ou manual)
    updateSelectInput(
      session,
      "dataset_response_col",
      choices  = cols_dataset,
      selected = model_target
    )

  }, error = function(e) {
    showModal(modalDialog(
      title = "Error loading model",
      paste("Error:", e$message),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
})

  model_analysis_missing_preview <- reactive({
    refresh_counter()
    input$model_file
    req(changed_table)
    analysis_data_raw <- as.data.frame(changed_table)
    analysis_data_raw[analysis_data_raw == ""] <- NA
    analysis_data_raw <- analysis_data_raw[, !apply(analysis_data_raw, 2, function(col) all(col == 0)), drop = FALSE]

    dataset_col <- input$dataset_response_col
    if (!is.null(dataset_col) && dataset_col %in% colnames(analysis_data_raw)) {
      predictors_all <- analysis_data_raw[, setdiff(colnames(analysis_data_raw), dataset_col), drop = FALSE]
    } else {
      predictors_all <- analysis_data_raw
    }

    model_features <- character(0)
    if (!is.null(loaded_model())) {
      model_obj <- loaded_model()$model
      model_features <- model_obj$finalModel$xNames %||% character(0)
    }

    if (length(model_features) > 0) {
      present_model_features <- intersect(model_features, colnames(predictors_all))
      predictors <- predictors_all[, present_model_features, drop = FALSE]
      missing_model_features <- setdiff(model_features, colnames(predictors_all))
    } else {
      predictors <- predictors_all
      missing_model_features <- character(0)
    }

    missing_definition <- input$model_analysis_missing_definition
    if (is.null(missing_definition) || length(missing_definition) == 0) {
      missing_definition <- character(0)
    }
    threshold_rows <- input$model_analysis_missing_threshold_rows
    missing_mask <- build_missing_mask(predictors, missing_definition, zero_exceptions = character(0))
    row_missing_pct <- if (ncol(missing_mask) > 0) rowMeans(missing_mask) * 100 else rep(0, nrow(predictors))
    keep_rows <- which(row_missing_pct <= threshold_rows)

    list(
      predictors = predictors,
      missing_definition = missing_definition,
      threshold_rows = threshold_rows,
      missing_mask = missing_mask,
      keep_rows = keep_rows,
      model_features = model_features,
      missing_model_features = missing_model_features
    )
  })

  output$model_analysis_missing_features_ui <- renderUI({
    preview <- model_analysis_missing_preview()
    missing_features <- preview$missing_model_features
    if (length(missing_features) == 0) {
      return(NULL)
    }

    tags$div(
      class = "model-analysis-missing-features",
      tags$p(class = "model-analysis-missing-features-title", tags$strong("Model columns not found in current table:")),
      tags$p(class = "model-analysis-missing-features-list", paste(missing_features, collapse = ", "))
    )
  })

  output$model_analysis_missing_summary <- renderUI({
    preview <- model_analysis_missing_preview()
    predictors <- preview$predictors
    missing_mask <- preview$missing_mask
    keep_rows <- preview$keep_rows
    missing_count <- if (length(missing_mask) > 0) sum(missing_mask) else 0
    total_cells <- length(as.matrix(predictors))
    missing_pct <- if (total_cells > 0) round(100 * missing_count / total_cells, 2) else 0
    filtered_mask <- if (length(keep_rows) > 0) missing_mask[keep_rows, , drop = FALSE] else matrix(FALSE, nrow = 0, ncol = ncol(missing_mask))
    missing_after <- if (length(filtered_mask) > 0) sum(filtered_mask) else 0
    total_after <- if (length(filtered_mask) > 0) length(filtered_mask) else 0

    make_summary_row <- function(label, before_value, after_value) {
      row_class <- if (!identical(before_value, after_value)) "ml-summary-row-changed" else ""
      tags$tr(
        class = row_class,
        tags$td(style = "padding: 8px 12px; border-bottom: 1px solid #edf0f3;", label),
        tags$td(style = "padding: 8px 12px; border-bottom: 1px solid #edf0f3;", as.character(before_value)),
        tags$td(style = "padding: 8px 12px; border-bottom: 1px solid #edf0f3;", as.character(after_value))
      )
    }

    tags$div(
      tags$h5("Model Analysis Missingness Summary"),
      tags$table(
        class = "ml-summary-table",
        style = "width: 100%; max-width: 760px; border-collapse: collapse; border: 1px solid #e2e6ea; background: #fff;",
        tags$thead(
          tags$tr(
            tags$th(style = "padding: 8px 12px; background: #f5f7fa; border-bottom: 1px solid #e2e6ea;", "Metric"),
            tags$th(style = "padding: 8px 12px; background: #f5f7fa; border-bottom: 1px solid #e2e6ea;", "Current"),
            tags$th(style = "padding: 8px 12px; background: #f5f7fa; border-bottom: 1px solid #e2e6ea;", "After threshold")
          )
        ),
        tags$tbody(
          make_summary_row("Number of features", ncol(predictors), ncol(filtered_mask)),
          make_summary_row("Number of samples", nrow(predictors), length(keep_rows)),
          make_summary_row("Missing cells", missing_count, missing_after),
          make_summary_row("Missingness (%)", paste0(missing_pct, "%"),
            if (total_after > 0) paste0(round(100 * missing_after / total_after, 2), "%") else "0%")
        )
      ),
      tags$p(
        style = "margin-top: 8px; margin-bottom: 2px; font-size: 12px; color: #596273;",
        paste0(
          "Consider as missing: ",
          if (length(preview$missing_definition) == 0) "(none selected)" else paste(preview$missing_definition, collapse = ", "),
          " | Row threshold: ", preview$threshold_rows, "%"
        )
      )
    )
  })

  # Tab 6) MODEL ANALYSIS: Run analysis when clicking the button
  observeEvent(input$run_model_analysis, {
    req(loaded_model())
    req(changed_table)
    model_analysis_recorded_plot(NULL)
    model_analysis_metrics_report("")
    model_container <- loaded_model()
    model_obj <- model_container$model
    preprocess_meta <- model_container$preprocess_meta

    # 1) Prepara os dados
    analysis_data <- as.data.frame(changed_table)
    analysis_data[analysis_data == ""] <- NA
    analysis_data <- analysis_data[, !apply(analysis_data, 2, function(col) all(col == 0)), drop = FALSE]

    # 2) Extrai ground truth a partir do selectInput
    dataset_col <- input$dataset_response_col
    if (!is.null(dataset_col) && dataset_col %in% colnames(analysis_data)) {
      if (length(model_obj$levels) > 0) {
        analysis_data[[dataset_col]] <- as.factor(analysis_data[[dataset_col]])
        ground_truth <- analysis_data[[dataset_col]]
      } else {
        ground_truth <- as.numeric(analysis_data[[dataset_col]])
      }
      analysis_data <- analysis_data[, setdiff(colnames(analysis_data), dataset_col), drop = FALSE]
    } else {
      ground_truth <- rep(NA, nrow(analysis_data))
    }

    preview <- model_analysis_missing_preview()
    keep_rows <- preview$keep_rows
    analysis_data <- analysis_data[keep_rows, , drop = FALSE]
    ground_truth <- ground_truth[keep_rows]

    # 3) Garante que todos os features do modelo existam nos dados
    model_features <- model_obj$finalModel$xNames
    for (feat in model_features) {
      if (!(feat %in% colnames(analysis_data))) {
        analysis_data[[feat]] <- NA
      }
    }

    if (nrow(analysis_data) == 0) {
      output$model_analysis_accuracy <- renderPrint({
        report_txt <- "No samples left after missingness filtering.\n"
        model_analysis_metrics_report(report_txt)
        cat(report_txt)
      })
      output$model_analysis_plot_metrics <- renderPrint({
        cat("No valid numeric pairs for plot metrics.\n")
      })
      output$model_analysis_plot_metrics <- renderPrint({
        cat("No valid numeric pairs for plot metrics.\n")
      })
      output$model_analysis_correlation_plot <- renderPlot({
        plot.new()
        text(0.5, 0.5, "No samples left after missingness filtering.")
      })
      output$model_analysis_table <- DT::renderDT({
        DT::datatable(data.frame())
      })
      model_analysis_results_data(data.frame())
      return()
    }

    analysis_data <- apply_saved_preprocess(analysis_data, preprocess_meta)
    analysis_data[is.na(analysis_data)] <- 0

    sample_names   <- rownames(analysis_data)
    pred_raw       <- predict(model_obj, newdata = analysis_data)
    is_classif     <- length(model_obj$levels) > 0

    if (is_classif) {

      # ---- CLASSIFICACAO ----
      predicted_class <- as.character(pred_raw)

      # tenta obter probabilidades
      probs <- tryCatch({
        predict(model_obj, newdata = analysis_data, type = "prob")
      }, error = function(e) NULL)

      if (!is.null(probs)) {
        max_prob    <- apply(probs, 1, max)
        sorted_probs <- t(apply(probs, 1, sort, decreasing = TRUE))
        conf_margin <- sorted_probs[,1] - sorted_probs[,2]
      } else {
        max_prob    <- rep(NA_real_, length(predicted_class))
        conf_margin <- rep(NA_real_, length(predicted_class))
      }

      # status confiavel vs inconclusivo
      threshold <- input$confidence_threshold
      status    <- ifelse(conf_margin < threshold, "inconclusive", "reliable")

      # diferenca numerica (classe codificada como numero)
      diff_num <- if (!all(is.na(ground_truth))) {
        as.numeric(predicted_class) - as.numeric(as.character(ground_truth))
      } else {
        NA_real_
      }

      # monta a tabela de saida
      output_table <- data.frame(
        Sample            = sample_names,
        Ground_Truth      = ground_truth,
        Predicted         = predicted_class,
        Confidence        = max_prob,
        Confidence_Margin = conf_margin,
        Difference        = diff_num,
        Status            = status,
        stringsAsFactors  = FALSE
      )

    } else {

      # ---- REGRESSAO ----
      predicted_value <- as.numeric(pred_raw)
      diff_val        <- predicted_value - ground_truth

      output_table <- data.frame(
        Sample       = sample_names,
        Ground_Truth = ground_truth,
        Predicted    = predicted_value,
        Difference   = diff_val,
        stringsAsFactors = FALSE
      )
    }

    # 4) Metricas adicionais
    if (!all(is.na(ground_truth))) {
      if (is_classif) {
        total_items        <- length(sample_names)
        reliable_idx       <- which(status == "reliable")
        count_reliable     <- length(reliable_idx)
        count_inconclusive <- sum(status == "inconclusive")
        correct_count      <- if (count_reliable>0) sum(predicted_class[reliable_idx]==ground_truth[reliable_idx]) else 0
        wrong_count        <- count_reliable - correct_count
        accuracy           <- if (count_reliable>0) correct_count/count_reliable else NA

        output$model_analysis_accuracy <- renderPrint({
          report_txt <- paste0(
            "Total items: ",            total_items,        "\n",
            "Reliable: ",               count_reliable,     "\n",
            "Inconclusive: ",           count_inconclusive, "\n",
            "Correct (reliable): ",     correct_count,      "\n",
            "Wrong (reliable): ",       wrong_count,        "\n",
            "Accuracy (reliable): ",    accuracy,           "\n"
          )
          model_analysis_metrics_report(report_txt)
          cat(report_txt, sep = "")
        })
      } else {
        valid_reg <- which(!is.na(ground_truth) & !is.na(predicted_value))
        n_pairs <- length(valid_reg)
        if (n_pairs > 0) {
          gt_valid <- ground_truth[valid_reg]
          pred_valid <- predicted_value[valid_reg]
          pearson_r <- if (n_pairs >= 2) stats::cor(gt_valid, pred_valid, method = "pearson") else NA_real_
          r2 <- if (!is.na(pearson_r)) pearson_r^2 else NA_real_
          mae <- mean(abs(pred_valid - gt_valid))
          rmse <- sqrt(mean((pred_valid - gt_valid)^2))
          rmse <- sqrt(mean((pred_valid - gt_valid)^2))
        } else {
          pearson_r <- NA_real_
          r2 <- NA_real_
          mae <- NA_real_
          rmse <- NA_real_
        }
        output$model_analysis_accuracy <- renderPrint({
          cat(
            "n=", n_pairs, "\n",
            "R^2=", format(round(r2, 2), nsmall = 2), "\n",
            "Pearson=", format(round(pearson_r, 2), nsmall = 2), "\n",
            "MAE=", format(round(mae, 2), nsmall = 2), "\n",
            "RMSE=", format(round(rmse, 2), nsmall = 2), "\n",
            sep = ""
          )
        })
      }
    } else {
      output$model_analysis_accuracy <- renderPrint({
        report_txt <- "Ground truth nao disponivel.\n"
        model_analysis_metrics_report(report_txt)
        cat(report_txt)
      })
    }

    output$model_analysis_plot_metrics <- renderPrint({
      if ("Ground_Truth" %in% colnames(output_table) && "Predicted" %in% colnames(output_table)) {
        gt <- suppressWarnings(as.numeric(as.character(output_table$Ground_Truth)))
        pred <- suppressWarnings(as.numeric(as.character(output_table$Predicted)))
        valid <- which(!is.na(gt) & !is.na(pred))
        n_pairs <- length(valid)
        if (n_pairs > 0) {
          gt_valid <- gt[valid]
          pred_valid <- pred[valid]
          pearson_r <- if (n_pairs >= 2) stats::cor(gt_valid, pred_valid, method = "pearson") else NA_real_
          r2 <- if (!is.na(pearson_r)) pearson_r^2 else NA_real_
          mae <- mean(abs(pred_valid - gt_valid))
          rmse <- sqrt(mean((pred_valid - gt_valid)^2))
          cat(
            "n=", n_pairs, "\n",
            "R^2=", format(round(r2, 6), nsmall = 6), "\n",
            "Pearson=", format(round(pearson_r, 6), nsmall = 6), "\n",
            "MAE=", format(round(mae, 6), nsmall = 6), "\n",
            "RMSE=", format(round(rmse, 6), nsmall = 6), "\n",
            sep = ""
          )
        } else {
          cat("No valid numeric pairs for plot metrics.\n")
        }
      } else {
        cat("Plot metrics unavailable for this model/output.\n")
      }
    })

    output$model_analysis_correlation_plot <- renderPlot({
      if ("Ground_Truth" %in% colnames(output_table) && "Predicted" %in% colnames(output_table)) {
        gt <- suppressWarnings(as.numeric(as.character(output_table$Ground_Truth)))
        pred <- suppressWarnings(as.numeric(as.character(output_table$Predicted)))
        valid <- which(!is.na(gt) & !is.na(pred))
        if (length(valid) > 0) {
          gt_valid <- gt[valid]
          pred_valid <- pred[valid]
          lims <- range(c(gt_valid, pred_valid), na.rm = TRUE)
          if (!all(is.finite(lims)) || lims[1] == lims[2]) {
            lims <- lims + c(-0.5, 0.5)
          }
          old_par <- par(no.readonly = TRUE)
          on.exit(par(old_par))
          par(pty = "s")
          plot(
            gt_valid, pred_valid,
            xlab = "Real value",
            ylab = "Predicted value",
            main = "Real vs Predicted",
            pch = 16,
            col = "#1f78b4",
            xlim = lims,
            ylim = lims,
            asp = 1
          )
          abline(a = 0, b = 1, col = "gray40", lty = 2, lwd = 2)
          n_pairs <- length(valid)
          pearson_r <- if (n_pairs >= 2) stats::cor(gt_valid, pred_valid, method = "pearson") else NA_real_
          r2 <- if (!is.na(pearson_r)) pearson_r^2 else NA_real_
          mae <- mean(abs(pred_valid - gt_valid))
          rmse <- sqrt(mean((pred_valid - gt_valid)^2))
          if (length(valid) >= 2) {
            abline(stats::lm(pred_valid ~ gt_valid), col = "#e31a1c", lwd = 2)
          }
          legend(
            "topleft",
            legend = c("Identity line (y = x)", "Regression line"),
            col = c("gray40", "#e31a1c"),
            lty = c(2, 1),
            lwd = c(2, 2),
            bg = "white",
            cex = 0.8
          )
          legend(
            "bottomright",
            legend = c(
              paste0("n: ", n_pairs),
              paste0("Pearson R: ", format(round(pearson_r, 6), nsmall = 6)),
              paste0("R^2: ", format(round(r2, 6), nsmall = 6)),
              paste0("MAE: ", format(round(mae, 6), nsmall = 6)),
              paste0("RMSE: ", format(round(rmse, 6), nsmall = 6))
            ),
            bty = "n",
            cex = 0.8
          )
        } else {
          plot.new()
          text(0.5, 0.5, "No valid numeric pairs for correlation plot.")
        }
      } else {
        plot.new()
        text(0.5, 0.5, "Correlation plot unavailable for this model/output.")
      }
      model_analysis_recorded_plot(recordPlot())
    })

    # 5) Renderiza a tabela final
    output$model_analysis_table <- DT::renderDT({
      DT::datatable(output_table, options = list(paging = FALSE, scrollX = TRUE))
    })
    model_analysis_results_data(output_table)
  })

  dl_history <- reactiveVal(data.frame())
  dl_metrics <- reactiveVal(data.frame())
  dl_predictions <- reactiveVal(data.frame())
  dl_tune_results <- reactiveVal(data.frame())
  dl_log <- reactiveVal("Deep Learning idle. Load data and click train.")
  dl_task_used <- reactiveVal("classification")
  dl_weight_summary <- reactiveVal(data.frame())
  dl_weight_heatmap <- reactiveVal(data.frame())
  dl_path_edges <- reactiveVal(data.frame())
  dl_path_nodes <- reactiveVal(data.frame())
  dl_top_paths <- reactiveVal(data.frame())
  dl_model_shape <- reactiveVal("Train the model to visualize layers and weights.")

  update_dl_views <- function(model_obj, predictor_names, build_views_fn, epoch = NULL, epochs = NULL) {
    weight_views <- build_views_fn(model_obj, predictor_names)
    shape_txt <- weight_views$shape_text
    if (!is.null(epoch) && !is.null(epochs)) {
      shape_txt <- paste0(shape_txt, " | Epoch ", epoch, "/", epochs)
    }
    dl_model_shape(shape_txt)
    dl_weight_summary(weight_views$summary_df)
    dl_weight_heatmap(weight_views$heatmap_df)
    dl_path_edges(weight_views$path_edges_df)
    dl_path_nodes(weight_views$path_nodes_df)
    dl_top_paths(weight_views$top_paths_df)
  }

  observeEvent(
    list(input$tabs, input$column_checkbox_group, input$file1, input$load_sample),
    {
      if (!(is.data.frame(changed_table) || is.matrix(changed_table))) {
        updateSelectInput(session, "dl_target", choices = character(0), selected = character(0))
        return()
      }

      all_columns <- colnames(changed_table)
      if (is.null(all_columns) || length(all_columns) == 0) {
        updateSelectInput(session, "dl_target", choices = character(0), selected = character(0))
        return()
      }

      selected_columns <- input$column_checkbox_group %||% character(0)
      available_columns <- if (length(selected_columns) > 0) {
        intersect(all_columns, selected_columns)
      } else {
        all_columns
      }
      if (length(available_columns) == 0) {
        available_columns <- all_columns
      }

      current_target <- input$dl_target
      selected_target <- if (!is.null(current_target) && current_target %in% available_columns) {
        current_target
      } else {
        available_columns[1]
      }

      updateSelectInput(session, "dl_target", choices = available_columns, selected = selected_target)
    },
    ignoreNULL = FALSE
  )

  output$dl_hidden_units_ui <- renderUI({
    layer_count <- max(1, as.integer(input$dl_hidden_layers %||% 2))
    tagList(lapply(seq_len(layer_count), function(i) {
      default_value <- if (i == 1) 64 else max(8, round(64 / i))
      numericInput(
        inputId = paste0("dl_hidden_units_", i),
        label = paste0("Hidden units (layer ", i, "):"),
        value = default_value,
        min = 4,
        step = 4
      )
    }))
  })

  output$dl_dropout_ui <- renderUI({
    layer_count <- max(1, as.integer(input$dl_hidden_layers %||% 2))
    tagList(lapply(seq_len(layer_count), function(i) {
      sliderInput(
        inputId = paste0("dl_dropout_", i),
        label = paste0("Dropout layer ", i, ":"),
        min = 0,
        max = 0.6,
        value = if (i == 1) 0.20 else 0.15,
        step = 0.05
      )
    }))
  })

  observeEvent(input$dl_run_training, {
    req(changed_table)
    dl_weight_summary(data.frame())
    dl_weight_heatmap(data.frame())
    dl_path_edges(data.frame())
    dl_path_nodes(data.frame())
    dl_top_paths(data.frame())
    dl_tune_results(data.frame())
    dl_model_shape("Training in progress...")

    if (!requireNamespace("torch", quietly = TRUE)) {
      dl_log("Package 'torch' is not installed. Install it with install.packages('torch') and torch::install_torch().")
      return()
    }

    torch_ready <- tryCatch(
      isTRUE(torch::torch_is_installed()),
      error = function(e) FALSE
    )
    if (!torch_ready) {
      dl_log("Torch backend dependencies are missing. Run torch::install_torch() and restart the app.")
      return()
    }

    selected_rows <- input$row_checkbox_group %||% rownames(changed_table)
    selected_cols <- input$column_checkbox_group %||% colnames(changed_table)
    local_df <- as.data.frame(changed_table[selected_rows, selected_cols, drop = FALSE], stringsAsFactors = FALSE)
    target_col <- input$dl_target
    if (is.null(target_col) || !(target_col %in% colnames(local_df))) {
      dl_log("Please choose a valid target column.")
      return()
    }

    set.seed(input$dl_seed)
    seed_set <- tryCatch({
      torch::torch_manual_seed(input$dl_seed)
      TRUE
    }, error = function(e) {
      dl_log(paste0(
        "Torch backend could not be initialized: ",
        conditionMessage(e),
        ". Run torch::install_torch() and restart the app."
      ))
      FALSE
    })
    if (!seed_set) {
      return()
    }

    target_raw <- local_df[[target_col]]
    predictors_raw <- local_df[, setdiff(colnames(local_df), target_col), drop = FALSE]

    if (ncol(predictors_raw) == 0) {
      dl_log("No predictor columns left after selecting target.")
      return()
    }

    predictors_raw[] <- lapply(predictors_raw, function(col) {
      if (is.character(col)) as.factor(col) else col
    })
    design_matrix <- model.matrix(~ . - 1, data = predictors_raw)
    design_matrix[is.na(design_matrix)] <- 0

    valid_rows <- complete.cases(design_matrix) & !is.na(target_raw)
    design_matrix <- design_matrix[valid_rows, , drop = FALSE]
    target_raw <- target_raw[valid_rows]
    if (nrow(design_matrix) < 20) {
      dl_log("Not enough valid rows for deep learning (need at least 20 rows).")
      return()
    }

    detected_task <- if (is.numeric(target_raw) && length(unique(target_raw)) > 10) "regression" else "classification"
    task <- if (isTruthy(input$dl_task) && input$dl_task != "auto") input$dl_task else detected_task
    dl_task_used(task)

    split_fraction <- (100 - input$dl_test_split) / 100
    train_idx <- sample(seq_len(nrow(design_matrix)), size = floor(split_fraction * nrow(design_matrix)))
    test_idx <- setdiff(seq_len(nrow(design_matrix)), train_idx)
    if (length(test_idx) < 2 || length(train_idx) < 5) {
      dl_log("Train/test split produced too few samples. Adjust split or dataset size.")
      return()
    }

    train_matrix_raw <- design_matrix[train_idx, , drop = FALSE]
    test_matrix_raw <- design_matrix[test_idx, , drop = FALSE]
    train_means <- colMeans(train_matrix_raw)
    train_sds <- apply(train_matrix_raw, 2, stats::sd)
    train_sds[!is.finite(train_sds) | train_sds == 0] <- 1

    train_matrix <- scale(train_matrix_raw, center = train_means, scale = train_sds)
    test_matrix <- scale(test_matrix_raw, center = train_means, scale = train_sds)
    train_matrix[is.na(train_matrix)] <- 0
    test_matrix[is.na(test_matrix)] <- 0

    x_train <- torch::torch_tensor(train_matrix, dtype = torch::torch_float())
    x_test <- torch::torch_tensor(test_matrix, dtype = torch::torch_float())

    input_size <- ncol(design_matrix)
    hidden_layers <- max(1, as.integer(input$dl_hidden_layers %||% 2))
    base_hidden_sizes <- vapply(seq_len(hidden_layers), function(i) {
      max(4, as.integer(input[[paste0("dl_hidden_units_", i)]] %||% if (i == 1) 64 else max(8, round(64 / i))))
    }, numeric(1))
    epochs <- max(1, as.integer(input$dl_epochs))
    batch_size <- max(1, as.integer(input$dl_batch_size))
    learning_rate <- as.numeric(input$dl_learning_rate)
    weight_decay <- max(0, as.numeric(input$dl_weight_decay))
    dropout_rates <- vapply(seq_len(hidden_layers), function(i) {
      min(0.8, max(0, as.numeric(input[[paste0("dl_dropout_", i)]] %||% if (i == 1) 0.15 else 0.10)))
    }, numeric(1))
    auto_arch <- isTRUE(input$dl_auto_arch)
    hidden_sizes <- base_hidden_sizes
    if (auto_arch) {
      heuristic_h1 <- round(sqrt(nrow(design_matrix) * input_size) * 1.1)
      hidden_sizes[1] <- max(base_hidden_sizes[1], min(256, heuristic_h1))
      if (hidden_layers > 1) {
        for (i in 2:hidden_layers) {
          hidden_sizes[i] <- max(base_hidden_sizes[i], min(160, round(hidden_sizes[i - 1] * 0.65)))
        }
      }
      dl_log(paste0(
        "Auto architecture enabled: using hidden layers = ",
        paste(hidden_sizes, collapse = " -> "),
        " based on data size (", nrow(design_matrix), " rows, ", input_size, " inputs)."
      ))
    }

    history_df <- data.frame(epoch = integer(), train_loss = numeric(), test_loss = numeric(), metric = numeric())
    predictions_df <- data.frame()
    metrics_df <- data.frame()
    predictor_names <- colnames(design_matrix)

    build_dl_weight_views <- function(model_obj, predictor_labels) {
      state <- model_obj$state_dict()
      state_names <- names(state)
      get_weight <- function(name) {
        if (!(name %in% state_names)) {
          return(NULL)
        }
        arr <- as.array(state[[name]]$to(device = "cpu"))
        if (length(dim(arr)) == 1) {
          matrix(arr, nrow = 1)
        } else {
          arr
        }
      }

      hidden_weights <- list()
      hidden_weight_names <- grep("^hidden_layers\\.[0-9]+\\.weight$", state_names, value = TRUE)
      if (length(hidden_weight_names) > 0) {
        hidden_idx <- as.integer(sub("^hidden_layers\\.([0-9]+)\\.weight$", "\\1", hidden_weight_names))
        hidden_weight_names <- hidden_weight_names[order(hidden_idx)]
        hidden_weights <- lapply(hidden_weight_names, get_weight)
      }
      if (length(hidden_weights) == 0) {
        fc1_w <- get_weight("fc1.weight")
        fc2_w <- get_weight("fc2.weight")
        if (!is.null(fc1_w)) {
          hidden_weights[[1]] <- fc1_w
        }
        if (!is.null(fc2_w)) {
          hidden_weights[[2]] <- fc2_w
        }
      }
      fc1_w <- if (length(hidden_weights) >= 1) hidden_weights[[1]] else NULL
      fc2_w <- if (length(hidden_weights) >= 2) hidden_weights[[2]] else NULL
      out_w <- get_weight("out.weight")
      if (is.null(out_w)) {
        out_w <- get_weight("fc2.weight")
      }
      if (is.null(fc1_w)) {
        return(list(
          shape_text = "Unable to extract layer weights from the trained model.",
          summary_df = data.frame(),
          heatmap_df = data.frame(),
          path_edges_df = data.frame(),
          path_nodes_df = data.frame(),
          top_paths_df = data.frame()
        ))
      }
      if (is.null(out_w)) {
        out_w <- matrix(0, nrow = 1, ncol = if (!is.null(fc2_w)) nrow(fc2_w) else nrow(fc1_w))
      }

      fc1_abs <- colMeans(abs(fc1_w))
      feature_names <- predictor_labels %||% paste0("X", seq_len(ncol(fc1_w)))
      summary_df <- data.frame(
        Feature = feature_names,
        MeanAbsWeight = as.numeric(fc1_abs),
        stringsAsFactors = FALSE
      )
      summary_df <- summary_df[order(summary_df$MeanAbsWeight, decreasing = TRUE), , drop = FALSE]
      summary_df <- utils::head(summary_df, 20)

      feature_idx <- match(summary_df$Feature, feature_names)
      feature_idx <- feature_idx[!is.na(feature_idx)]
      selected_fc1 <- fc1_w[, feature_idx, drop = FALSE]
      heatmap_df <- data.frame(
        HiddenUnit = rep(seq_len(nrow(selected_fc1)), times = ncol(selected_fc1)),
        Feature = rep(feature_names[feature_idx], each = nrow(selected_fc1)),
        Weight = as.numeric(selected_fc1),
        stringsAsFactors = FALSE
      )

      arch_layers <- c(paste0("Input(", ncol(fc1_w), ")"))
      if (length(hidden_weights) > 0) {
        arch_layers <- c(
          arch_layers,
          vapply(seq_along(hidden_weights), function(i) {
            paste0("Hidden", i, "(", nrow(hidden_weights[[i]]), ")")
          }, character(1))
        )
      }
      if (!is.null(out_w)) {
        arch_layers <- c(arch_layers, paste0("Output(", nrow(out_w), ")"))
      }

      shape_text <- paste("Architecture:", paste(arch_layers, collapse = " -> "))
      effective_out_w <- out_w
      if (length(hidden_weights) > 2) {
        downstream_weights <- hidden_weights[3:length(hidden_weights)]
        for (w in rev(downstream_weights)) {
          if (ncol(effective_out_w) != nrow(w)) {
            shape_text <- paste0(
              shape_text,
              " | Path compression skipped due to incompatible layer dimensions."
            )
            effective_out_w <- out_w
            break
          }
          effective_out_w <- effective_out_w %*% w
        }
        if (!grepl("Path compression skipped", shape_text, fixed = TRUE)) {
          shape_text <- paste0(
            shape_text,
            " | Path view is compressed after Hidden2 to keep the graph readable."
          )
        }
      }

      input_labels <- feature_names
      hidden_labels_list <- lapply(seq_along(hidden_weights), function(i) {
        paste0("H", i, "_", seq_len(nrow(hidden_weights[[i]])))
      })

      path_nodes_list <- list(data.frame(
        node = input_labels,
        layer = "Input",
        idx = seq_along(input_labels),
        stringsAsFactors = FALSE
      ))
      for (i in seq_along(hidden_labels_list)) {
        path_nodes_list[[length(path_nodes_list) + 1]] <- data.frame(
          node = hidden_labels_list[[i]],
          layer = paste0("Hidden", i),
          idx = seq_along(hidden_labels_list[[i]]),
          stringsAsFactors = FALSE
        )
      }
      out_labels <- paste0("O_", seq_len(nrow(out_w)))
      path_nodes_list[[length(path_nodes_list) + 1]] <- data.frame(
        node = out_labels,
        layer = "Output",
        idx = seq_along(out_labels),
        stringsAsFactors = FALSE
      )
      path_nodes_df <- do.call(rbind, path_nodes_list)

      edge_from_weight <- function(weight_matrix, from_labels, to_labels, pct, min_edges) {
        flat <- abs(weight_matrix)
        edge_cap <- min(length(flat), max(min_edges, round(length(flat) * pct)))
        top_idx <- order(flat, decreasing = TRUE)[seq_len(edge_cap)]
        rc <- arrayInd(top_idx, dim(flat))
        data.frame(
          from = from_labels[rc[, 2]],
          to = to_labels[rc[, 1]],
          weight = as.numeric(weight_matrix[top_idx]),
          stringsAsFactors = FALSE
        )
      }

      path_edges_list <- list()
      path_edges_list[[length(path_edges_list) + 1]] <- edge_from_weight(
        hidden_weights[[1]], input_labels, hidden_labels_list[[1]], pct = 0.06, min_edges = 30
      )
      if (length(hidden_weights) > 1) {
        for (i in 2:length(hidden_weights)) {
          path_edges_list[[length(path_edges_list) + 1]] <- edge_from_weight(
            hidden_weights[[i]], hidden_labels_list[[i - 1]], hidden_labels_list[[i]], pct = 0.10, min_edges = 30
          )
        }
      }
      path_edges_list[[length(path_edges_list) + 1]] <- edge_from_weight(
        out_w, hidden_labels_list[[length(hidden_labels_list)]], out_labels, pct = 0.25, min_edges = 15
      )
      path_edges_df <- do.call(rbind, path_edges_list)

      top_paths_df <- data.frame()
      hidden1_labels <- hidden_labels_list[[1]]
      hidden2_labels <- if (length(hidden_labels_list) >= 2) hidden_labels_list[[2]] else NULL
      if (!is.null(fc2_w)) {
        limit_in <- min(8, ncol(fc1_w))
        limit_h1 <- min(12, nrow(fc1_w))
        limit_h2 <- min(12, nrow(fc2_w))
        top_inputs <- order(colMeans(abs(fc1_w)), decreasing = TRUE)[seq_len(limit_in)]
        top_h1 <- order(rowMeans(abs(fc1_w)), decreasing = TRUE)[seq_len(limit_h1)]
        top_h2 <- order(rowMeans(abs(fc2_w)), decreasing = TRUE)[seq_len(limit_h2)]
        combos <- expand.grid(i = top_inputs, h1 = top_h1, h2 = top_h2, o = seq_len(nrow(out_w)))
        w1 <- fc1_w[cbind(combos$h1, combos$i)]
        w2 <- fc2_w[cbind(combos$h2, combos$h1)]
        w3 <- effective_out_w[cbind(combos$o, combos$h2)]
        path_score <- abs(w1) * abs(w2) * abs(w3)
        signed_effect <- sign(w1 * w2 * w3) * path_score
        top_paths_df <- data.frame(
          Input = input_labels[combos$i],
          Hidden1 = hidden1_labels[combos$h1],
          Hidden2 = hidden2_labels[combos$h2],
          Output = out_labels[combos$o],
          PathScore = path_score,
          SignedEffect = signed_effect,
          stringsAsFactors = FALSE
        )
      } else {
        limit_in <- min(10, ncol(fc1_w))
        limit_h1 <- min(16, nrow(fc1_w))
        top_inputs <- order(colMeans(abs(fc1_w)), decreasing = TRUE)[seq_len(limit_in)]
        top_h1 <- order(rowMeans(abs(fc1_w)), decreasing = TRUE)[seq_len(limit_h1)]
        combos <- expand.grid(i = top_inputs, h1 = top_h1, o = seq_len(nrow(out_w)))
        w1 <- fc1_w[cbind(combos$h1, combos$i)]
        w2 <- out_w[cbind(combos$o, combos$h1)]
        path_score <- abs(w1) * abs(w2)
        signed_effect <- sign(w1 * w2) * path_score
        top_paths_df <- data.frame(
          Input = input_labels[combos$i],
          Hidden1 = hidden1_labels[combos$h1],
          Hidden2 = "-",
          Output = out_labels[combos$o],
          PathScore = path_score,
          SignedEffect = signed_effect,
          stringsAsFactors = FALSE
        )
      }
      top_paths_df <- top_paths_df[order(top_paths_df$PathScore, decreasing = TRUE), , drop = FALSE]
      top_paths_df <- utils::head(top_paths_df, 40)
      top_paths_df$PathScore <- round(top_paths_df$PathScore, 6)
      top_paths_df$SignedEffect <- round(top_paths_df$SignedEffect, 6)
      if (nrow(top_paths_df) > 0) {
        top_path_label <- paste(
          top_paths_df$Input[1], "->", top_paths_df$Hidden1[1], "->",
          if (top_paths_df$Hidden2[1] != "-") paste0(top_paths_df$Hidden2[1], " -> ") else "",
          top_paths_df$Output[1]
        )
        shape_text <- paste0(
          shape_text,
          " | Strongest path: ",
          top_path_label,
          " (score=",
          format(top_paths_df$PathScore[1], nsmall = 4),
          ")"
        )
        if (nrow(top_paths_df) > 1) {
          alt_path_label <- paste(
            top_paths_df$Input[2], "->", top_paths_df$Hidden1[2], "->",
            if (top_paths_df$Hidden2[2] != "-") paste0(top_paths_df$Hidden2[2], " -> ") else "",
            top_paths_df$Output[2]
          )
          shape_text <- paste0(
            shape_text,
            " | Alternative path: ",
            alt_path_label,
            " (score=",
            format(top_paths_df$PathScore[2], nsmall = 4),
            ")"
          )
        }
      }
      list(
        shape_text = shape_text,
        summary_df = summary_df,
        heatmap_df = heatmap_df,
        path_edges_df = path_edges_df,
        path_nodes_df = path_nodes_df,
        top_paths_df = top_paths_df
      )
    }

    if (task == "classification") {
      y_factor <- as.factor(target_raw)
      if (nlevels(y_factor) < 2) {
        dl_log("Classification requires at least two classes in the target.")
        return()
      }

      y_train <- torch::torch_tensor(as.integer(y_factor[train_idx]), dtype = torch::torch_long())
      y_test <- torch::torch_tensor(as.integer(y_factor[test_idx]), dtype = torch::torch_long())
      class_levels <- levels(y_factor)
      output_size <- nlevels(y_factor)

      model <- torch::nn_module(
        initialize = function(in_features, hidden_sizes, out_features, dropout_rates) {
          self$hidden_layers <- torch::nn_module_list()
          self$bn_layers <- torch::nn_module_list()
          self$drop_layers <- torch::nn_module_list()
          prev_features <- in_features
          for (i in seq_along(hidden_sizes)) {
            hidden_i <- as.integer(hidden_sizes[i])
            self$hidden_layers$append(torch::nn_linear(prev_features, hidden_i))
            self$bn_layers$append(torch::nn_batch_norm1d(hidden_i))
            self$drop_layers$append(torch::nn_dropout(p = as.numeric(dropout_rates[i])))
            prev_features <- hidden_i
          }
          self$out <- torch::nn_linear(prev_features, out_features)
        },
        forward = function(x) {
          layer_count <- length(self$hidden_layers)
          for (i in seq_len(layer_count)) {
            x <- self$hidden_layers[[i]](x)
            x <- self$bn_layers[[i]](x)
            x <- torch::nnf_relu(x)
            x <- self$drop_layers[[i]](x)
          }
          self$out(x)
        }
      )(input_size, hidden_sizes, output_size, dropout_rates)

      optimizer <- torch::optim_adam(model$parameters, lr = learning_rate, weight_decay = weight_decay)
      criterion <- torch::nn_cross_entropy_loss()
      clone_state_dict <- function(state_dict) {
        cloned <- lapply(state_dict, function(value) value$clone())
        names(cloned) <- names(state_dict)
        cloned
      }
      best_metric <- -Inf
      best_state <- clone_state_dict(model$state_dict())

      withProgress(message = "Training torch classification model", value = 0, {
        for (epoch in seq_len(epochs)) {
          model$train()
          train_order <- sample.int(length(train_idx))
          batch_list <- split(train_order, ceiling(seq_along(train_order) / batch_size))
          batch_losses <- numeric(length(batch_list))
          for (b in seq_along(batch_list)) {
            batch_ids <- batch_list[[b]]
            optimizer$zero_grad()
            logits_train <- model(x_train[batch_ids, ])
            loss_train <- criterion(logits_train, y_train[batch_ids])
            loss_train$backward()
            optimizer$step()
            batch_losses[b] <- as.numeric(loss_train$item())
          }
          mean_train_loss <- mean(batch_losses)

          model$eval()
          torch::with_no_grad({
            logits_test <- model(x_test)
            loss_test <- criterion(logits_test, y_test)
            pred_idx <- as.integer(torch::torch_argmax(logits_test, dim = 2)$to(device = "cpu"))
            y_test_vec <- as.integer(y_test$to(device = "cpu"))
            acc <- mean(pred_idx == y_test_vec)
            history_df <<- rbind(history_df, data.frame(
              epoch = epoch,
              train_loss = mean_train_loss,
              test_loss = as.numeric(loss_test$item()),
              metric = acc
            ))
            if (is.finite(acc) && acc > best_metric) {
              best_metric <<- acc
              best_state <<- clone_state_dict(model$state_dict())
            }
          })
          update_dl_views(model, predictor_names, build_dl_weight_views, epoch, epochs)
          incProgress(1 / epochs, detail = paste("Epoch", epoch, "of", epochs))
        }
      })

      model$load_state_dict(best_state)
      model$eval()
      torch::with_no_grad({
        logits_test <- model(x_test)
        pred_idx <- as.integer(torch::torch_argmax(logits_test, dim = 2)$to(device = "cpu"))
      })
      truth_idx <- as.integer(y_factor[test_idx])
      predictions_df <- data.frame(
        Sample = rownames(local_df)[valid_rows][test_idx],
        Truth = class_levels[truth_idx],
        Predicted = class_levels[pred_idx],
        stringsAsFactors = FALSE
      )
      final_acc <- mean(predictions_df$Truth == predictions_df$Predicted)
      metrics_df <- data.frame(
        Metric = c("Task", "Classes", "Train samples", "Test samples", "Final accuracy", "Best epoch accuracy"),
        Value = c("classification", output_size, length(train_idx), length(test_idx), round(final_acc, 4), round(best_metric, 4)),
        stringsAsFactors = FALSE
      )
      dl_log(paste0(
        "Deep Learning (classification) finished. Accuracy: ",
        round(final_acc, 4),
        " | Best epoch accuracy: ",
        round(best_metric, 4)
      ))
    } else {
      y_numeric <- suppressWarnings(as.numeric(target_raw))
      valid_target <- !is.na(y_numeric)
      if (!all(valid_target)) {
        design_matrix <- design_matrix[valid_target, , drop = FALSE]
        y_numeric <- y_numeric[valid_target]
        train_idx <- sample(seq_len(nrow(design_matrix)), size = floor(split_fraction * nrow(design_matrix)))
        test_idx <- setdiff(seq_len(nrow(design_matrix)), train_idx)
        train_matrix_raw <- design_matrix[train_idx, , drop = FALSE]
        test_matrix_raw <- design_matrix[test_idx, , drop = FALSE]
        train_means <- colMeans(train_matrix_raw)
        train_sds <- apply(train_matrix_raw, 2, stats::sd)
        train_sds[!is.finite(train_sds) | train_sds == 0] <- 1
        train_matrix <- scale(train_matrix_raw, center = train_means, scale = train_sds)
        test_matrix <- scale(test_matrix_raw, center = train_means, scale = train_sds)
        train_matrix[is.na(train_matrix)] <- 0
        test_matrix[is.na(test_matrix)] <- 0
        x_train <- torch::torch_tensor(train_matrix, dtype = torch::torch_float())
        x_test <- torch::torch_tensor(test_matrix, dtype = torch::torch_float())
      }

      y_train_raw <- matrix(y_numeric[train_idx], ncol = 1)
      y_test_raw <- matrix(y_numeric[test_idx], ncol = 1)
      y_center <- 0
      y_scale <- 1
      if (isTRUE(input$dl_scale_target)) {
        y_center <- mean(y_train_raw)
        y_scale <- stats::sd(y_train_raw)
        if (!is.finite(y_scale) || y_scale == 0) {
          y_scale <- 1
        }
      }

      y_train_scaled <- (y_train_raw - y_center) / y_scale
      y_test_scaled <- (y_test_raw - y_center) / y_scale

      y_train <- torch::torch_tensor(y_train_scaled, dtype = torch::torch_float())
      y_test <- torch::torch_tensor(y_test_scaled, dtype = torch::torch_float())
      y_test_truth <- y_numeric[test_idx]

      make_regression_model <- function(model_hidden_sizes, model_dropout_rates) {
        torch::nn_module(
          initialize = function(in_features, hidden_sizes, dropout_rates) {
            self$hidden_layers <- torch::nn_module_list()
            self$bn_layers <- torch::nn_module_list()
            self$drop_layers <- torch::nn_module_list()
            prev_features <- in_features
            for (i in seq_along(hidden_sizes)) {
              hidden_i <- as.integer(hidden_sizes[i])
              self$hidden_layers$append(torch::nn_linear(prev_features, hidden_i))
              self$bn_layers$append(torch::nn_batch_norm1d(hidden_i))
              self$drop_layers$append(torch::nn_dropout(p = as.numeric(dropout_rates[i])))
              prev_features <- hidden_i
            }
            self$out <- torch::nn_linear(prev_features, 1)
          },
          forward = function(x) {
            layer_count <- length(self$hidden_layers)
            for (i in seq_len(layer_count)) {
              x <- self$hidden_layers[[i]](x)
              x <- self$bn_layers[[i]](x)
              x <- torch::nnf_relu(x)
              x <- self$drop_layers[[i]](x)
            }
            self$out(x)
          }
        )(input_size, model_hidden_sizes, model_dropout_rates)
      }

      criterion <- torch::nn_smooth_l1_loss()
      clone_state_dict <- function(state_dict) {
        cloned <- lapply(state_dict, function(value) value$clone())
        names(cloned) <- names(state_dict)
        cloned
      }
      train_regression_network <- function(model_hidden_sizes, model_dropout_rates, lr, wd,
                                           x_train_tensor, y_train_tensor, train_n,
                                           x_eval_tensor, y_eval_tensor, y_eval_truth,
                                           n_epochs, progress_detail = "Training",
                                           track_history = TRUE, restore_best = TRUE,
                                           update_views = FALSE) {
        model_obj <- make_regression_model(model_hidden_sizes, model_dropout_rates)
        optimizer <- torch::optim_adam(model_obj$parameters, lr = lr, weight_decay = wd)
        train_state <- new.env(parent = emptyenv())
        train_state$best_metric <- Inf
        train_state$best_epoch <- NA_integer_
        train_state$best_state <- clone_state_dict(model_obj$state_dict())
        train_state$history <- data.frame(epoch = integer(), train_loss = numeric(), test_loss = numeric(), metric = numeric())

        for (epoch in seq_len(n_epochs)) {
          model_obj$train()
          train_order <- sample.int(train_n)
          batch_list <- split(train_order, ceiling(seq_along(train_order) / batch_size))
          batch_losses <- numeric(length(batch_list))
          for (b in seq_along(batch_list)) {
            batch_ids <- batch_list[[b]]
            optimizer$zero_grad()
            pred_train <- model_obj(x_train_tensor[batch_ids, ])
            loss_train <- criterion(pred_train, y_train_tensor[batch_ids, ])
            loss_train$backward()
            optimizer$step()
            batch_losses[b] <- as.numeric(loss_train$item())
          }
          mean_train_loss <- mean(batch_losses)

          model_obj$eval()
          torch::with_no_grad({
            pred_eval <- model_obj(x_eval_tensor)
            loss_eval <- criterion(pred_eval, y_eval_tensor)
            pred_eval_num <- as.numeric(pred_eval$to(device = "cpu")) * y_scale + y_center
            eval_rmse <- sqrt(mean((pred_eval_num - y_eval_truth)^2))
            if (track_history) {
              train_state$history <- rbind(train_state$history, data.frame(
                epoch = epoch,
                train_loss = mean_train_loss,
                test_loss = as.numeric(loss_eval$item()),
                metric = eval_rmse
              ))
            }
            if (is.finite(eval_rmse) && eval_rmse < train_state$best_metric) {
              train_state$best_metric <- eval_rmse
              train_state$best_epoch <- epoch
              train_state$best_state <- clone_state_dict(model_obj$state_dict())
            }
          })
          if (update_views) {
            update_dl_views(model_obj, predictor_names, build_dl_weight_views, epoch, n_epochs)
          }
          incProgress(1, detail = paste(progress_detail, "epoch", epoch, "of", n_epochs))
        }

        if (restore_best && is.finite(train_state$best_metric)) {
          model_obj$load_state_dict(train_state$best_state)
        }
        model_obj$eval()
        torch::with_no_grad({
          final_pred <- as.numeric(model_obj(x_eval_tensor)$to(device = "cpu")) * y_scale + y_center
        })
        list(
          model = model_obj,
          history = train_state$history,
          best_metric = train_state$best_metric,
          best_epoch = train_state$best_epoch,
          pred = final_pred
        )
      }

      if (isTRUE(input$dl_auto_tune)) {
        tune_trials <- max(2, as.integer(input$dl_tune_trials %||% 20))
        validation_fraction <- min(0.4, max(0.1, as.numeric(input$dl_validation_split %||% 20) / 100))
        train_n <- length(train_idx)
        validation_n <- max(2, floor(train_n * validation_fraction))
        if ((train_n - validation_n) < 5) {
          dl_log("Auto tune needs at least five training samples after validation split.")
          return()
        }

        local_order <- sample.int(train_n)
        validation_local_idx <- local_order[seq_len(validation_n)]
        tune_train_local_idx <- setdiff(seq_len(train_n), validation_local_idx)

        hidden_grid <- list(c(32), c(64, 32), c(128, 64), c(128, 32), c(64, 32, 16))
        learning_rate_grid <- c(0.0001, 0.0003, 0.0005, 0.001)
        weight_decay_grid <- c(0, 0.0001, 0.0005, 0.001, 0.002)
        dropout1_grid <- c(0.10, 0.15, 0.20, 0.25, 0.30)
        dropout2_grid <- c(0.05, 0.10, 0.15, 0.20, 0.25)

        tune_state <- new.env(parent = emptyenv())
        tune_state$tune_results <- data.frame()
        tune_state$best_metric <- Inf
        tune_state$best_config <- NULL
        tune_state$best_epoch <- epochs
        tune_state$final_result <- NULL
        tune_state$failed <- FALSE
        progress_steps <- tune_trials * epochs + epochs

        withProgress(message = "Auto tuning torch regression model", value = 0, {
          for (trial in seq_len(tune_trials)) {
            trial_hidden <- hidden_grid[[sample.int(length(hidden_grid), 1)]]
            trial_dropout <- c(
              sample(dropout1_grid, 1),
              rep(sample(dropout2_grid, 1), max(0, length(trial_hidden) - 1))
            )
            trial_lr <- sample(learning_rate_grid, 1)
            trial_wd <- sample(weight_decay_grid, 1)
            trial_seed <- as.integer(input$dl_seed) + trial
            set.seed(trial_seed)
            torch::torch_manual_seed(trial_seed)

            trial_result <- train_regression_network(
              model_hidden_sizes = trial_hidden,
              model_dropout_rates = trial_dropout,
              lr = trial_lr,
              wd = trial_wd,
              x_train_tensor = x_train[tune_train_local_idx, ],
              y_train_tensor = y_train[tune_train_local_idx, ],
              train_n = length(tune_train_local_idx),
              x_eval_tensor = x_train[validation_local_idx, ],
              y_eval_tensor = y_train[validation_local_idx, ],
              y_eval_truth = y_numeric[train_idx][validation_local_idx],
              n_epochs = epochs,
              progress_detail = paste("Trial", trial, "of", tune_trials),
              track_history = FALSE,
              restore_best = TRUE,
              update_views = FALSE
            )

            tune_state$tune_results <- rbind(tune_state$tune_results, data.frame(
              Trial = trial,
              ValidationRMSE = trial_result$best_metric,
              BestEpoch = trial_result$best_epoch,
              Hidden = paste(trial_hidden, collapse = " -> "),
              LearningRate = trial_lr,
              WeightDecay = trial_wd,
              Dropout = paste(trial_dropout, collapse = " -> "),
              stringsAsFactors = FALSE
            ))
            if (is.finite(trial_result$best_metric) && trial_result$best_metric < tune_state$best_metric) {
              tune_state$best_metric <- trial_result$best_metric
              tune_state$best_epoch <- trial_result$best_epoch
              tune_state$best_config <- list(
                hidden = trial_hidden,
                dropout = trial_dropout,
                lr = trial_lr,
                wd = trial_wd
              )
            }
            incProgress(0, detail = paste("Best validation RMSE:", round(tune_state$best_metric, 4)))
          }

          if (is.null(tune_state$best_config)) {
            dl_log("Auto tune did not find a finite validation RMSE.")
            tune_state$failed <- TRUE
            return()
          }

          final_epochs <- max(1, as.integer(tune_state$best_epoch %||% epochs))
          set.seed(as.integer(input$dl_seed))
          torch::torch_manual_seed(as.integer(input$dl_seed))
          tune_state$final_result <- train_regression_network(
            model_hidden_sizes = tune_state$best_config$hidden,
            model_dropout_rates = tune_state$best_config$dropout,
            lr = tune_state$best_config$lr,
            wd = tune_state$best_config$wd,
            x_train_tensor = x_train,
            y_train_tensor = y_train,
            train_n = length(train_idx),
            x_eval_tensor = x_test,
            y_eval_tensor = y_test,
            y_eval_truth = y_test_truth,
            n_epochs = final_epochs,
            progress_detail = "Final model",
            track_history = TRUE,
            restore_best = FALSE,
            update_views = TRUE
          )
        }, max = progress_steps)

        if (isTRUE(tune_state$failed)) {
          return()
        }
        if (is.null(tune_state$final_result)) {
          dl_log("Auto tune did not finish a final model.")
          return()
        }
        final_result <- tune_state$final_result
        tune_results <- tune_state$tune_results
        best_tune_metric <- tune_state$best_metric
        best_tune_config <- tune_state$best_config
        model <- final_result$model
        pred_test_num <- final_result$pred
        best_metric <- best_tune_metric
        history_df <- final_result$history
        tune_results <- tune_results[order(tune_results$ValidationRMSE), , drop = FALSE]
        dl_tune_results(tune_results)
      } else {
        model <- torch::nn_module(
          initialize = function(in_features, hidden_sizes, dropout_rates) {
          self$hidden_layers <- torch::nn_module_list()
          self$bn_layers <- torch::nn_module_list()
          self$drop_layers <- torch::nn_module_list()
          prev_features <- in_features
          for (i in seq_along(hidden_sizes)) {
            hidden_i <- as.integer(hidden_sizes[i])
            self$hidden_layers$append(torch::nn_linear(prev_features, hidden_i))
            self$bn_layers$append(torch::nn_batch_norm1d(hidden_i))
            self$drop_layers$append(torch::nn_dropout(p = as.numeric(dropout_rates[i])))
            prev_features <- hidden_i
          }
          self$out <- torch::nn_linear(prev_features, 1)
        },
        forward = function(x) {
          layer_count <- length(self$hidden_layers)
          for (i in seq_len(layer_count)) {
            x <- self$hidden_layers[[i]](x)
            x <- self$bn_layers[[i]](x)
            x <- torch::nnf_relu(x)
            x <- self$drop_layers[[i]](x)
          }
          self$out(x)
        }
        )(input_size, hidden_sizes, dropout_rates)

        optimizer <- torch::optim_adam(model$parameters, lr = learning_rate, weight_decay = weight_decay)
        best_metric <- Inf
        best_state <- clone_state_dict(model$state_dict())

        withProgress(message = "Training torch regression model", value = 0, {
          for (epoch in seq_len(epochs)) {
            model$train()
            train_order <- sample.int(length(train_idx))
            batch_list <- split(train_order, ceiling(seq_along(train_order) / batch_size))
            batch_losses <- numeric(length(batch_list))
            for (b in seq_along(batch_list)) {
              batch_ids <- batch_list[[b]]
              optimizer$zero_grad()
              pred_train <- model(x_train[batch_ids, ])
              loss_train <- criterion(pred_train, y_train[batch_ids, ])
              loss_train$backward()
              optimizer$step()
              batch_losses[b] <- as.numeric(loss_train$item())
            }
            mean_train_loss <- mean(batch_losses)

            model$eval()
            torch::with_no_grad({
              pred_test <- model(x_test)
              loss_test <- criterion(pred_test, y_test)
              pred_test_num <- as.numeric(pred_test$to(device = "cpu")) * y_scale + y_center
              rmse <- sqrt(mean((pred_test_num - y_test_truth)^2))
              history_df <<- rbind(history_df, data.frame(
                epoch = epoch,
                train_loss = mean_train_loss,
                test_loss = as.numeric(loss_test$item()),
                metric = rmse
              ))
              if (is.finite(rmse) && rmse < best_metric) {
                best_metric <<- rmse
                best_state <<- clone_state_dict(model$state_dict())
              }
            })
            update_dl_views(model, predictor_names, build_dl_weight_views, epoch, epochs)
            incProgress(1 / epochs, detail = paste("Epoch", epoch, "of", epochs))
          }
        })

        if (is.finite(best_metric)) {
          model$load_state_dict(best_state)
        }
        model$eval()
        torch::with_no_grad({
          pred_test <- model(x_test)
          pred_test_num <- as.numeric(pred_test$to(device = "cpu")) * y_scale + y_center
        })
      }
      truth_num <- y_numeric[test_idx]
      mae <- mean(abs(pred_test_num - truth_num))
      rmse <- sqrt(mean((pred_test_num - truth_num)^2))
      r2 <- if (stats::var(truth_num) > 0) 1 - sum((pred_test_num - truth_num)^2) / sum((truth_num - mean(truth_num))^2) else NA_real_
      predictions_df <- data.frame(
        Sample = rownames(local_df)[valid_rows][test_idx],
        Truth = round(truth_num, 6),
        Predicted = round(pred_test_num, 6),
        Residual = round(pred_test_num - truth_num, 6),
        stringsAsFactors = FALSE
      )
      best_metric_name <- if (isTRUE(input$dl_auto_tune)) "Selected validation RMSE" else "Best epoch RMSE"
      metrics_df <- data.frame(
        Metric = c("Task", "Train samples", "Test samples", "MAE", "RMSE", "R2", best_metric_name),
        Value = c("regression", length(train_idx), length(test_idx), round(mae, 4), round(rmse, 4), round(r2, 4), round(best_metric, 4)),
        stringsAsFactors = FALSE
      )
      if (isTRUE(input$dl_auto_tune)) {
        best_tune_row <- tune_results[1, , drop = FALSE]
        metrics_df <- rbind(
          metrics_df,
          data.frame(
            Metric = c("Auto tune trials", "Best validation RMSE", "Selected hidden units", "Selected learning rate", "Selected weight decay", "Selected dropout"),
            Value = c(
              tune_trials,
              round(best_tune_metric, 4),
              best_tune_row$Hidden,
              best_tune_row$LearningRate,
              best_tune_row$WeightDecay,
              best_tune_row$Dropout
            ),
            stringsAsFactors = FALSE
          )
        )
      }
      dl_log(paste0(
        if (isTRUE(input$dl_auto_tune)) "Deep Learning auto tune (regression) finished. " else "Deep Learning (regression) finished. ",
        "RMSE: ",
        round(rmse, 4),
        " | R2: ",
        round(r2, 4),
        if (isTRUE(input$dl_auto_tune)) " | Selected validation RMSE: " else " | Best epoch RMSE: ",
        round(best_metric, 4)
      ))
    }

    dl_history(history_df)
    dl_metrics(metrics_df)
    dl_predictions(predictions_df)
    weight_views <- build_dl_weight_views(model, predictor_names)
    dl_model_shape(weight_views$shape_text)
    dl_weight_summary(weight_views$summary_df)
    dl_weight_heatmap(weight_views$heatmap_df)
    dl_path_edges(weight_views$path_edges_df)
    dl_path_nodes(weight_views$path_nodes_df)
    dl_top_paths(weight_views$top_paths_df)
  })

  output$dl_training_log <- renderText({
    dl_log()
  })

  output$dl_loss_panel <- renderUI({
    req(nrow(dl_history()) > 0)
    tags$div(class = "dl-panel", plotOutput("dl_loss_plot", height = "260px"))
  })

  output$dl_metric_panel <- renderUI({
    req(nrow(dl_history()) > 0)
    tags$div(class = "dl-panel", plotOutput("dl_metric_plot", height = "260px"))
  })

  set_output_option_if_registered <- function(output_id, suspend_when_hidden = FALSE) {
    tryCatch({
      if (output_id %in% names(output)) {
        outputOptions(output, output_id, suspendWhenHidden = suspend_when_hidden)
      }
    }, error = function(e) {
      NULL
    })
  }
  set_output_option_if_registered("dl_loss_panel", FALSE)
  set_output_option_if_registered("dl_metric_panel", FALSE)

  output$dl_loss_plot <- renderPlot({
    history_df <- dl_history()
    req(nrow(history_df) > 0)
    ggplot(history_df, aes(x = epoch)) +
      geom_line(aes(y = train_loss, color = "Train loss"), linewidth = 1) +
      geom_line(aes(y = test_loss, color = "Test loss"), linewidth = 1) +
      scale_color_manual(values = c("Train loss" = "#1f77b4", "Test loss" = "#d62728")) +
      labs(x = "Epoch", y = "Loss", color = "", title = "Training/Test Loss") +
      theme_minimal(base_size = 13)
  })

  output$dl_metric_plot <- renderPlot({
    history_df <- dl_history()
    req(nrow(history_df) > 0)
    metric_label <- if (identical(dl_task_used(), "regression")) "RMSE (test)" else "Accuracy (test)"
    ggplot(history_df, aes(x = epoch, y = metric)) +
      geom_line(color = "#2ca02c", linewidth = 1) +
      labs(x = "Epoch", y = metric_label, title = "Model Performance by Epoch") +
      theme_minimal(base_size = 13)
  })

  output$dl_tuning_tips <- renderUI({
    metrics_df <- dl_metrics()
    req(nrow(metrics_df) > 0)
    task <- as.character(metrics_df$Value[metrics_df$Metric == "Task"][1])
    if (identical(task, "classification")) {
      final_acc <- suppressWarnings(as.numeric(metrics_df$Value[metrics_df$Metric == "Final accuracy"][1]))
      best_acc <- suppressWarnings(as.numeric(metrics_df$Value[metrics_df$Metric == "Best epoch accuracy"][1]))
      tips <- c(
        "If accuracy is low, increase epochs to 100-200 and reduce learning rate to 0.0005.",
        "If training is unstable, increase batch size to 64 and keep weight decay between 0.0001 and 0.001.",
        "If overfitting appears (best epoch much higher than final), increase dropout to 0.20-0.35."
      )
      if (is.finite(final_acc) && final_acc < 0.70) {
        tips <- c(
          paste0("Final accuracy is still low (", round(final_acc, 3), "). Prioritize more epochs and regularization tuning."),
          tips
        )
      }
      if (is.finite(best_acc) && is.finite(final_acc) && (best_acc - final_acc) > 0.05) {
        tips <- c(tips, "There is a gap between the best and final epoch: try manual early stopping using the epoch chart.")
      }
    } else {
      rmse <- suppressWarnings(as.numeric(metrics_df$Value[metrics_df$Metric == "RMSE"][1]))
      best_rmse <- suppressWarnings(as.numeric(metrics_df$Value[metrics_df$Metric == "Best epoch RMSE"][1]))
      tips <- c(
        "For regression, keep Scale numeric target enabled and increase epochs to 120+.",
        "If RMSE does not improve, gradually increase units and/or the number of hidden layers.",
        "If overfitting appears, increase dropout (0.20-0.35) or weight decay (0.0005-0.002)."
      )
      if (is.finite(rmse) && is.finite(best_rmse) && (rmse - best_rmse) > 0.05 * max(1, abs(best_rmse))) {
        tips <- c(tips, "The model degraded after the best epoch: use fewer epochs or a smaller learning rate (0.0003-0.0007).")
      }
    }
    tags$div(
      class = "dl-panel",
      tags$h4("Tuning suggestions"),
      tags$ul(lapply(tips, tags$li))
    )
  })

  output$dl_metrics_table <- DT::renderDT({
    req(nrow(dl_metrics()) > 0)
    DT::datatable(dl_metrics(), options = list(dom = "t", paging = FALSE, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_tune_table <- DT::renderDT({
    req(nrow(dl_tune_results()) > 0)
    DT::datatable(dl_tune_results(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_predictions_table <- DT::renderDT({
    req(nrow(dl_predictions()) > 0)
    DT::datatable(dl_predictions(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_model_shape <- renderText({
    dl_model_shape()
  })

  output$dl_path_plot <- plotly::renderPlotly({
    edges <- dl_path_edges()
    nodes <- dl_path_nodes()
    req(nrow(edges) > 0, nrow(nodes) > 0)

    hidden_levels <- sort(unique(nodes$layer[grepl("^Hidden[0-9]+$", nodes$layer)]))
    hidden_levels <- hidden_levels[order(as.integer(sub("^Hidden", "", hidden_levels)))]
    layer_order <- c("Input", hidden_levels, "Output")
    nodes$layer <- factor(nodes$layer, levels = layer_order)
    nodes <- nodes[order(nodes$layer, nodes$idx), , drop = FALSE]
    max_nodes_layer <- max(as.numeric(table(nodes$layer)))
    nodes$y <- ave(nodes$idx, nodes$layer, FUN = function(x) {
      n_layer <- length(x)
      if (n_layer <= 1) {
        return((max_nodes_layer + 1) / 2)
      }
      seq(1, max_nodes_layer, length.out = n_layer)
    })

    edge_plot <- merge(edges, nodes[, c("node", "layer", "y")], by.x = "from", by.y = "node", all.x = TRUE)
    names(edge_plot)[names(edge_plot) == "layer"] <- "from_layer"
    names(edge_plot)[names(edge_plot) == "y"] <- "from_y"
    edge_plot <- merge(edge_plot, nodes[, c("node", "layer", "y")], by.x = "to", by.y = "node", all.x = TRUE)
    names(edge_plot)[names(edge_plot) == "layer"] <- "to_layer"
    names(edge_plot)[names(edge_plot) == "y"] <- "to_y"

    edge_plot$from_x <- as.numeric(factor(edge_plot$from_layer, levels = layer_order))
    edge_plot$to_x <- as.numeric(factor(edge_plot$to_layer, levels = layer_order))
    edge_plot$abs_weight <- abs(edge_plot$weight)
    edge_plot$abs_weight_fmt <- format(round(edge_plot$abs_weight, 5), nsmall = 5)
    edge_plot$hover <- paste0(
      "From: ", edge_plot$from,
      "<br>To: ", edge_plot$to,
      "<br>Weight: ", round(edge_plot$weight, 5),
      "<br>|Weight|: ", edge_plot$abs_weight_fmt
    )

    node_plot <- nodes
    node_plot$x <- as.numeric(factor(node_plot$layer, levels = layer_order))
    incoming_count <- table(edges$to)
    outgoing_count <- table(edges$from)
    incoming_strength <- tapply(abs(edges$weight), edges$to, max)
    outgoing_strength <- tapply(abs(edges$weight), edges$from, max)

    strongest_from <- edges[order(abs(edges$weight), decreasing = TRUE), c("to", "from", "weight"), drop = FALSE]
    strongest_from <- strongest_from[!duplicated(strongest_from$to), , drop = FALSE]
    names(strongest_from) <- c("node", "strongest_input_node", "strongest_input_weight")

    strongest_to <- edges[order(abs(edges$weight), decreasing = TRUE), c("from", "to", "weight"), drop = FALSE]
    strongest_to <- strongest_to[!duplicated(strongest_to$from), , drop = FALSE]
    names(strongest_to) <- c("node", "strongest_output_node", "strongest_output_weight")

    node_plot$incoming_n <- as.integer(incoming_count[node_plot$node])
    node_plot$outgoing_n <- as.integer(outgoing_count[node_plot$node])
    node_plot$incoming_max <- as.numeric(incoming_strength[node_plot$node])
    node_plot$outgoing_max <- as.numeric(outgoing_strength[node_plot$node])
    node_plot$incoming_n[is.na(node_plot$incoming_n)] <- 0L
    node_plot$outgoing_n[is.na(node_plot$outgoing_n)] <- 0L
    node_plot$incoming_max[is.na(node_plot$incoming_max)] <- 0
    node_plot$outgoing_max[is.na(node_plot$outgoing_max)] <- 0
    node_plot <- merge(node_plot, strongest_from, by = "node", all.x = TRUE)
    node_plot <- merge(node_plot, strongest_to, by = "node", all.x = TRUE)
    node_plot$strongest_input_node[is.na(node_plot$strongest_input_node)] <- "-"
    node_plot$strongest_output_node[is.na(node_plot$strongest_output_node)] <- "-"
    node_plot$strongest_input_weight[is.na(node_plot$strongest_input_weight)] <- 0
    node_plot$strongest_output_weight[is.na(node_plot$strongest_output_weight)] <- 0
    node_plot$hover <- paste0(
      "Node: ", node_plot$node,
      "<br>Layer: ", node_plot$layer,
      "<br>Position: ", node_plot$idx,
      "<br>Incoming edges: ", node_plot$incoming_n,
      "<br>Outgoing edges: ", node_plot$outgoing_n,
      "<br>Strongest incoming: ", node_plot$strongest_input_node,
      " (", round(node_plot$strongest_input_weight, 5), ")",
      "<br>Strongest outgoing: ", node_plot$strongest_output_node,
      " (", round(node_plot$strongest_output_weight, 5), ")",
      "<br>Max |incoming|: ", round(node_plot$incoming_max, 5),
      "<br>Max |outgoing|: ", round(node_plot$outgoing_max, 5)
    )

    p <- ggplot() +
      geom_segment(
        data = edge_plot,
        aes(
          x = from_x, y = from_y,
          xend = to_x, yend = to_y,
          color = weight,
          text = hover
        ),
        linewidth = 0.35,
        alpha = 0.65
      ) +
      geom_point(
        data = node_plot,
        aes(x = x, y = y, text = hover),
        color = "#111111",
        fill = "#fefefe",
        size = 2.4,
        shape = 21,
        stroke = 0.4
      ) +
      scale_x_continuous(
        breaks = seq_along(layer_order),
        labels = layer_order,
        limits = c(0.8, length(layer_order) + 0.2)
      ) +
      scale_color_gradient2(low = "#2c7bb6", mid = "#f0f0f0", high = "#d7191c", midpoint = 0) +
      labs(
        x = "Layer",
        y = "Nodes (full layer span)",
        color = "Weight",
        title = "Top weighted paths across layers"
      ) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text") %>%
      config(displaylogo = FALSE)
  })
  set_output_option_if_registered("dl_path_plot", FALSE)
  set_output_option_if_registered("dl_model_shape", FALSE)

  output$dl_path_table <- DT::renderDT({
    path_df <- dl_top_paths()
    req(nrow(path_df) > 0)
    DT::datatable(
      path_df,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$dl_weight_plot <- renderPlot({
    weight_df <- dl_weight_summary()
    req(nrow(weight_df) > 0)
    ggplot(weight_df, aes(x = reorder(Feature, MeanAbsWeight), y = MeanAbsWeight)) +
      geom_col(fill = "#1f77b4", alpha = 0.85) +
      coord_flip() +
      labs(
        x = "Input feature",
        y = "Mean |weight| (input -> hidden1)",
        title = "Top weighted input features"
      ) +
      theme_minimal(base_size = 12)
  })

  session$onSessionEnded(function() {
    ugplot_cleanup_global_session_objects()
  })

  load_dataset_into_table(session)
  update_scramble_selector()
  tryCatch({
    load_selected_ml_list()
  }, error = function(e) {
    ml_model_source_status_text(paste("Could not load model list:", conditionMessage(e)))
    load_ml_list()
  })

}  # End of server function

# Run the application
shinyApp(ui, server)
