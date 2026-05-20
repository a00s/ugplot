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
source_local_helper("server_deps.R", "ugPlotInstallModelDeps", always_reload = TRUE)
source_local_helper("remote_client.R", "ugplot_remote_create_job", always_reload = TRUE)
source_local_helper("remote_servers.R", "ugplot_read_remote_servers", always_reload = TRUE)
source_local_helper("geo_import.R", "ugplot_geo_cache_dir", always_reload = TRUE)

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
  "),
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
                  actionButton("ml_run_threshold_scan", "Run exhaustive threshold scan (0-100%)"),
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
        verbatimTextOutput("remote_job_status"),
        tags$div(
          style = "display: flex; gap: 12px; align-items: flex-start; flex-wrap: wrap; margin-top: 8px;",
          tags$div(style = "flex: 0 0 360px; max-width: 100%;", uiOutput("remote_job_running_summary")),
          tags$div(style = "flex: 1 1 420px; min-width: 320px; max-width: 620px;", plotlyOutput("remote_job_metric_distribution", height = "260px"))
        ),
        verbatimTextOutput("remote_job_running_details"),
        uiOutput("remote_job_log_panel")
      )
    ),
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
        )
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
    if ("na" %in% missing_definition) {
      missing_col <- missing_col | is.na(col_data)
    }
    if ("empty" %in% missing_definition) {
      missing_col <- missing_col | (!is.na(col_data) & trimws(as.character(col_data)) == "")
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

compute_exhaustive_threshold_scan <- function(predictors, missing_definition,
                                              zero_exceptions = character(0),
                                              progress_callback = NULL, status_callback = NULL) {
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
  results[order(-results$cross_point, -results$pareto, -results$tradeoff_score,
    results$missing_pct_after, -results$filled_cells), , drop = FALSE]
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
                                   threshold_scope = "train_only") {
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
    filtered_train <- apply_missing_filters(
      predictors = predictors_train,
      missing_definition = missing_definition,
      zero_exceptions = zero_exceptions,
      threshold_cols = threshold_cols,
      threshold_rows = threshold_rows
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
  ml_model_source_status_text <- reactiveVal("")

  hideTab(inputId = "tabs", target = "TABLE")
  hideTab(inputId = "tabs", target = "HEATMAP PLOT")
  hideTab(inputId = "tabs", target = "2D PLOT")
  hideTab(inputId = "tabs", target = "MACHINE LEARNING")
  hideTab(inputId = "tabs", target = "MODEL ANALYSIS")
  hideTab(inputId = "tabs", target = "DEEP LEARNING")
  hideTab(inputId = "tabs", target = "GRAPH MODELS")
  hideTab(inputId = "tabs", target = "CONFIGURATIONS")

  disable("merge_all_columns")
  disable("merge_all_rows")
  disable("process_table_content")
  session$allowReconnect(TRUE)

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

  observe({
    refresh_remote_server_inputs()
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
  remote_job_preview_status <- reactiveVal(NULL)
  remote_job_preview_result <- reactiveVal(NULL)
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
  geo_status <- reactiveVal("Waiting for GEO accession.")
  geo_stage <- reactiveVal(list(
    step = "Step 1",
    title = "Inspect a GEO accession",
    message = "Enter a GEO accession and inspect the available supplementary files."
  ))
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
    tags$div(
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

  render_geo_table_details <- function(summary, title_output, table_output, open = FALSE, extra = NULL) {
    tags$details(class = "geo-table-section geo-step-table", open = if (isTRUE(open)) TRUE else NULL,
      tags$summary(summary),
      title_output,
      table_output,
      extra
    )
  }

  output$geo_workflow_ui <- renderUI({
    accession_value <- isolate(input$geo_accession %||% "")
    loadable_only <- isolate(isTRUE(input$geo_download_loadable_only))
    threshold_value <- isolate(input$geo_transcript_absrho_threshold %||% 0.8)
    max_cpgs_value <- isolate(input$geo_spearman_max_cpgs %||% 0)
    metadata <- geo_sample_metadata()
    remote_files <- geo_remote_files()
    local_files <- geo_files()
    annotation_map <- geo_cpg_annotation()
    spearman_results <- geo_spearman_raw_results()
    transcript_table <- geo_transcript_candidates()
    preview <- geo_preview_data()

    metadata_done <- is.data.frame(metadata) && nrow(metadata) > 0
    files_seen <- (is.data.frame(remote_files) && nrow(remote_files) > 0) || (is.data.frame(local_files) && nrow(local_files) > 0)
    files_done <- FALSE
    needs_extract <- FALSE
    if (is.data.frame(remote_files) && nrow(remote_files) > 0) {
      processed_files <- remote_files[remote_files$Loadable, , drop = FALSE]
      files_done <- nrow(processed_files) > 0 && !any(processed_files$NeedsDownload %||% TRUE)
      needs_extract <- any(processed_files$LocalStatus == "downloaded" & grepl("\\.gz$", processed_files$File, ignore.case = TRUE))
    }
    matrix_files <- if (nzchar(trimws(accession_value))) ugplot_geo_matrix_files(ugplot_geo_cache_dir(trimws(accession_value))) else character(0)
    extract_done <- length(matrix_files) > 0
    annotation_done <- is.data.frame(annotation_map) && nrow(annotation_map) > 0
    spearman_done <- is.data.frame(spearman_results) && nrow(spearman_results) > 0
    transcript_done <- is.data.frame(transcript_table) && nrow(transcript_table) > 0
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
        render_geo_step_card(3, "Matrix files", files_done,
          tags$div(
            tags$p(class = "geo-step-note", "Processed matrix tables are the required files for this workflow; raw archives remain optional."),
            checkboxInput("geo_download_loadable_only", "Download only loadable processed tables", value = loadable_only),
            uiOutput("geo_download_summary"),
            if (!files_done) actionButton("geo_fetch_files", "Download matrix files") else NULL
          )
        ),
        render_geo_step_card(4, "Download progress", files_done,
          tags$div(
            if (files_done) tags$p(class = "geo-step-note", "Required processed matrices are already local.") else NULL,
            uiOutput("geo_download_progress_ui")
          )
        ),
        render_geo_step_card(5, "Extract matrix files", extract_done,
          tags$div(
            if (needs_extract) {
              tags$div(
                tags$p(class = "geo-step-note", "Large .gz matrices must be extracted before preprocessing. They are still too large to load directly into ugPlot."),
                actionButton("geo_extract_files", "Extract downloaded .gz files"),
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
        render_geo_step_card(6, "Analyze CpGs and transcripts", spearman_done && transcript_done,
          tags$div(
            uiOutput("geo_target_selector"),
            tags$p(class = "geo-step-note", "Spearman scan uses numeric targets such as age. Transcript candidate tables are built automatically when annotation is available."),
            numericInput("geo_spearman_max_cpgs", "Max CpGs to scan (0 = all):", value = max_cpgs_value, min = 0, step = 10000),
            numericInput("geo_transcript_absrho_threshold", "Transcript CpG threshold |rho|:", value = threshold_value, min = 0, max = 1, step = 0.01),
            uiOutput("geo_annotation_summary"),
            if (!annotation_done) actionButton("geo_build_annotation", "Build/load CpG annotation cache") else NULL,
            actionButton("geo_run_spearman", if (spearman_done) "Re-run CpG Spearman scan" else "Run CpG Spearman scan")
          )
        ),
        render_geo_step_card(7, "Optional direct load", preview_done,
          tags$div(
            uiOutput("geo_file_selector"),
            checkboxInput("geo_use_first_column_names", "Use first column as row names", value = TRUE),
            selectInput("geo_loaded_orientation", "Loaded matrix orientation:", choices = c("Samples x CpGs" = "samples_rows", "CpGs x Samples" = "cpgs_rows"), selected = "samples_rows"),
            actionButton("geo_load_selected_file", "Load selected file")
          )
        )
      ),
      tags$div(class = "geo-table-stack geo-workflow-tables",
        if (metadata_done) render_geo_table_details("Open sample metadata table", uiOutput("geo_metadata_table_title"), DT::DTOutput("geo_metadata_table"), open = FALSE) else NULL,
        if (files_seen) render_geo_table_details("Open GEO files table", uiOutput("geo_files_table_title"), DT::DTOutput("geo_files_table"), open = FALSE) else NULL,
        if (spearman_done) render_geo_table_details("Open CpG Spearman table", uiOutput("geo_spearman_table_title"), DT::DTOutput("geo_spearman_table"), open = FALSE) else NULL,
        if (transcript_done) {
          render_geo_table_details(
            "Open transcript candidate CpG table",
            uiOutput("geo_transcript_candidates_table_title"),
            DT::DTOutput("geo_transcript_candidates_table"),
            open = FALSE,
            extra = tags$div(class = "geo-table-actions", actionButton("geo_load_transcript_candidates", "Load transcript table into TABLE"))
          )
        } else NULL,
        if (preview_done) render_geo_table_details("Open loaded ugPlot preview", uiOutput("geo_preview_table_title"), DT::DTOutput("geo_preview_table"), open = FALSE) else NULL
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
    processed_files <- remote_files[remote_files$Loadable, , drop = FALSE]
    processed_pending <- processed_files[processed_files$NeedsDownload, , drop = FALSE]
    optional_files <- remote_files[!remote_files$Loadable, , drop = FALSE]
    optional_pending <- optional_files[optional_files$NeedsDownload, , drop = FALSE]
    tags$div(
      tags$p(paste0("Found: ", nrow(remote_files), " file(s), ", ugplot_format_bytes(known_size), if (unknown_size_n > 0) paste0(" + ", unknown_size_n, " unknown-size file(s)") else "")),
      tags$p(paste0("Required processed matrices: ", nrow(processed_files), " file(s), ", ugplot_format_bytes(sum(ugplot_geo_size_bytes(processed_files), na.rm = TRUE)), ".")),
      tags$p(paste0("Still needed for required workflow: ", nrow(processed_pending), " file(s), ", ugplot_format_bytes(sum(ugplot_geo_size_bytes(processed_pending), na.rm = TRUE)), ".")),
      if (!isTRUE(input$geo_download_loadable_only) && nrow(optional_files) > 0) {
        tags$p(paste0("Optional raw/metadata files selected: ", nrow(optional_files), " file(s); still missing ", nrow(optional_pending), "."))
      } else NULL,
      tags$p(paste0("Folder: ", ugplot_geo_cache_dir(trimws(input$geo_accession %||% "GEO"))))
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
      "CpG-level correlation against the selected metadata target. If annotation is loaded, CpGs may appear in multiple rows because one CpG can map to multiple transcripts."
    )
  })

  output$geo_transcript_candidates_table_title <- renderUI({
    candidates <- geo_transcript_candidates()
    req(is.data.frame(candidates), nrow(candidates) > 0)
    threshold <- suppressWarnings(as.numeric(input$geo_transcript_absrho_threshold %||% 0.8))
    render_geo_table_title(
      "Transcript candidate CpG table",
      paste0(
        "Automatically built from CpGs with |rho| >= ", threshold,
        ". For every transcript hit by a passing CpG, this table lists all annotated CpGs in that transcript."
      )
    )
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
      return(tags$p("Fetch sample metadata first to choose a target column."))
    }
    candidates <- ugplot_geo_target_candidates(metadata)
    if (length(candidates) == 0) {
      return(tags$p("No usable target-like metadata column was detected. Inspect the sample metadata table."))
    }
    selected <- if ("age" %in% candidates) "age" else candidates[[1]]
    selectInput("geo_target_column", "Target metadata column:", choices = candidates, selected = selected)
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

  output$geo_spearman_table <- DT::renderDT({
    results <- geo_spearman_results()
    req(is.data.frame(results), nrow(results) > 0)
    display <- results
    display$SpearmanRho <- round(display$SpearmanRho, 5)
    display$PValue <- signif(display$PValue, 5)
    display$AbsRho <- round(display$AbsRho, 5)
    DT::datatable(display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$geo_transcript_candidates_table <- DT::renderDT({
    candidates <- geo_transcript_candidates()
    req(is.data.frame(candidates), nrow(candidates) > 0)
    display_cols <- intersect(
      c(
        "Transcript", "Gene", "CpG", "GeneRegion", "Chr", "Position",
        "SpearmanRho", "PValue", "N", "AbsRho", "CpGInSpearmanScan",
        "TriggerBestCpG", "TriggerBestRho", "TriggerMaxAbsRho", "TriggerCpGs", "TriggerGenes", "ThresholdAbsRho"
      ),
      names(candidates)
    )
    display <- candidates[, display_cols, drop = FALSE]
    for (metric_col in intersect(c("SpearmanRho", "AbsRho", "TriggerBestRho", "TriggerMaxAbsRho", "ThresholdAbsRho"), names(display))) {
      display[[metric_col]] <- round(display[[metric_col]], 5)
    }
    if ("PValue" %in% names(display)) {
      display$PValue <- signif(display$PValue, 5)
    }
    DT::datatable(display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  observeEvent(input$geo_download_loadable_only, {
    remote_files <- geo_remote_files()
    accession <- isolate(trimws(input$geo_accession %||% ""))
    if (!is.data.frame(remote_files) || nrow(remote_files) == 0 || !nzchar(accession)) {
      return()
    }
    cache_dir <- ugplot_geo_cache_dir(accession)
    remote_files <- ugplot_geo_annotate_remote_files(remote_files, cache_dir)
    geo_remote_files(remote_files)
    processed_files <- remote_files[remote_files$Loadable, , drop = FALSE]
    pending_files <- processed_files[processed_files$NeedsDownload, , drop = FALSE]
    if (nrow(processed_files) > 0 && nrow(pending_files) == 0) {
      shinyjs::disable("geo_fetch_files")
      if (any(processed_files$LocalStatus == "downloaded" & grepl("\\.gz$", processed_files$File, ignore.case = TRUE))) {
        shinyjs::enable("geo_extract_files")
      } else {
        shinyjs::disable("geo_extract_files")
      }
      geo_stage(list(step = "Step 5", title = "Matrix files already local", message = "Required processed matrix files are already available locally. Extract compressed matrices next."))
    } else {
      shinyjs::enable("geo_fetch_files")
      shinyjs::disable("geo_extract_files")
      geo_stage(list(step = "Step 3", title = "Review matrix download plan", message = paste0("Required processed matrices still needed: ", nrow(pending_files), " file(s), ", ugplot_format_bytes(sum(ugplot_geo_size_bytes(pending_files), na.rm = TRUE)), ".")))
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
        if (nrow(processed_files) > 0 && nrow(pending_files) == 0) {
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
        if (nrow(processed_files) == 0 || nrow(pending_files) > 0) {
          shinyjs::enable("geo_fetch_files")
          geo_stage(list(step = "Step 3", title = "Review matrix download plan", message = paste0("Metadata is ready. Required processed matrices still needed: ", nrow(pending_files), " file(s), about ", ugplot_format_bytes(sum(ugplot_geo_size_bytes(pending_files), na.rm = TRUE)), ".")))
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
      if (isTRUE(input$geo_download_loadable_only)) {
        remote_files <- remote_files[remote_files$Loadable, , drop = FALSE]
        if (nrow(remote_files) == 0) {
          stop("No directly loadable processed tables were found. Disable 'Download only loadable processed tables' to download all supplementary files.")
        }
        geo_status(ugplot_geo_append_log(
          geo_status(),
          paste0("Download filter enabled: downloading only ", nrow(remote_files), " loadable processed table(s).")
        ))
      } else {
        geo_status(ugplot_geo_append_log(geo_status(), "Download filter disabled: downloading all supplementary files."))
      }
      remote_files <- remote_files[remote_files$NeedsDownload, , drop = FALSE]
      if (nrow(remote_files) == 0) {
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
          message = "All selected files are already available locally. Extract compressed matrices before preprocessing."
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
          idat_n, " IDAT files detected; IDAT preprocessing is not implemented in this first version."
        )))
        geo_stage(list(
          step = "Step 5",
          title = "Ready to extract",
          message = paste0("Download complete. Extract compressed matrix files before building an analysis table.")
        ))
        geo_download_progress(list(percent = 100, file = "Download complete", detail = paste0(nrow(files), " file(s) available."), folder = cache_dir))
        session$sendCustomMessage("geoProgress", list(percent = 100, file = "Download complete", detail = paste0(nrow(files), " file(s) available.")))
        if (!any(annotated_files$NeedsDownload[if (isTRUE(input$geo_download_loadable_only)) annotated_files$Loadable else rep(TRUE, nrow(annotated_files))])) {
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
      step = "Step 6",
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
      step = "Step 6",
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

  build_geo_transcript_candidates <- function(update_stage = FALSE) {
    accession <- trimws(input$geo_accession %||% "")
    if (!nzchar(accession)) {
      return(invisible(data.frame()))
    }
    cache_dir <- ugplot_geo_cache_dir(accession)
    target_column <- isolate(input$geo_target_column %||% "")
    results <- geo_spearman_raw_results()
    if (!is.data.frame(results) || nrow(results) == 0) {
      spearman_path <- file.path(cache_dir, paste0("ugplot_geo_spearman_", target_column, ".csv"))
      if (nzchar(target_column) && file.exists(spearman_path)) {
        results <- tryCatch(utils::read.csv(spearman_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
        geo_spearman_raw_results(results)
      }
    }
    if (!is.data.frame(results) || nrow(results) == 0) {
      if (isTRUE(update_stage)) {
        geo_stage(list(step = "Step 6", title = "Run Spearman first", message = "Run the CpG Spearman scan before building the transcript CpG table."))
      }
      return(invisible(data.frame()))
    }

    annotation_map <- geo_cpg_annotation()
    metadata <- geo_sample_metadata()
    if ((!is.data.frame(annotation_map) || nrow(annotation_map) == 0) && is.data.frame(metadata) && nrow(metadata) > 0) {
      annotation_map <- ugplot_geo_load_annotation_cache(ugplot_geo_detect_platform(metadata))
      if (is.data.frame(annotation_map) && nrow(annotation_map) > 0) {
        geo_cpg_annotation(annotation_map)
      }
    }
    if (!is.data.frame(annotation_map) || nrow(annotation_map) == 0) {
      if (isTRUE(update_stage)) {
        geo_stage(list(step = "Step 6", title = "Build annotation first", message = "Build/load the CpG annotation cache before building transcript candidates."))
      }
      return(invisible(data.frame()))
    }

    threshold <- suppressWarnings(as.numeric(isolate(input$geo_transcript_absrho_threshold %||% 0.8)))
    candidates <- ugplot_geo_transcript_candidates(results, annotation_map, threshold)
    geo_transcript_candidates(candidates)
    safe_threshold <- gsub("[^0-9]+", "_", format(threshold, trim = TRUE, scientific = FALSE))
    candidates_path <- file.path(cache_dir, paste0("ugplot_geo_transcript_candidates_", target_column, "_absrho_", safe_threshold, ".csv"))
    if (is.data.frame(candidates) && nrow(candidates) > 0) {
      utils::write.csv(candidates, candidates_path, row.names = FALSE)
      geo_status(ugplot_geo_append_log(
        geo_status(),
        paste0(
          "Transcript candidate table ready: ", nrow(candidates), " CpG-transcript rows across ",
          length(unique(candidates$Transcript)), " transcript(s). Saved to ", candidates_path, "."
        )
      ))
      if (isTRUE(update_stage)) {
        geo_stage(list(
          step = "Step 6",
          title = "Transcript CpG table ready",
          message = paste0(
            "Found ", length(unique(candidates$Transcript)), " transcript(s) with at least one CpG above |rho| >= ",
            threshold, ". Saved expanded CpG table to disk."
          )
        ))
      }
    } else if (isTRUE(update_stage)) {
      geo_status(ugplot_geo_append_log(geo_status(), paste0("No transcript candidates found for |rho| >= ", threshold, ".")))
      geo_stage(list(
        step = "Step 6",
        title = "No transcript candidates",
        message = paste0("No annotated CpG passed |rho| >= ", threshold, ". Lower the threshold or scan more CpGs.")
      ))
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

    metadata <- geo_sample_metadata()
    target_column <- isolate(input$geo_target_column %||% "")
    if ((!nzchar(target_column) || !target_column %in% names(metadata)) && is.data.frame(metadata) && nrow(metadata) > 0) {
      candidates <- ugplot_geo_target_candidates(metadata)
      target_column <- if ("age" %in% candidates) "age" else if (length(candidates) > 0) candidates[[1]] else ""
    }
    spearman_paths <- list.files(cache_dir, pattern = "^ugplot_geo_spearman_.*\\.csv$", full.names = TRUE)
    spearman_paths <- spearman_paths[!grepl("_annotated|_by_gene|_by_transcript", basename(spearman_paths))]
    if (!nzchar(target_column) && length(spearman_paths) > 0) {
      spearman_names <- sub("^ugplot_geo_spearman_(.*)\\.csv$", "\\1", basename(spearman_paths))
      target_column <- if ("age" %in% spearman_names) "age" else spearman_names[[1]]
    }
    spearman_path <- file.path(cache_dir, paste0("ugplot_geo_spearman_", target_column, ".csv"))
    if (nzchar(target_column) && file.exists(spearman_path)) {
      spearman_results <- tryCatch(utils::read.csv(spearman_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      if (is.data.frame(spearman_results) && nrow(spearman_results) > 0) {
        geo_spearman_raw_results(spearman_results)
        annotation_map <- geo_cpg_annotation()
        if (is.data.frame(annotation_map) && nrow(annotation_map) > 0) {
          geo_spearman_results(ugplot_geo_join_spearman_annotation(spearman_results, annotation_map))
          build_geo_transcript_candidates(update_stage = FALSE)
        } else {
          geo_spearman_results(spearman_results)
        }
      }
    }
    candidate_paths <- list.files(cache_dir, pattern = "^ugplot_geo_transcript_candidates_.*\\.csv$", full.names = TRUE)
    if (length(candidate_paths) > 0 && (!is.data.frame(geo_transcript_candidates()) || nrow(geo_transcript_candidates()) == 0)) {
      target_matches <- if (nzchar(target_column)) grepl(paste0("^ugplot_geo_transcript_candidates_", target_column, "_"), basename(candidate_paths)) else rep(FALSE, length(candidate_paths))
      preferred_paths <- candidate_paths[target_matches]
      candidate_path <- if (length(preferred_paths) > 0) preferred_paths[[which.max(file.info(preferred_paths)$mtime)]] else candidate_paths[[which.max(file.info(candidate_paths)$mtime)]]
      transcript_candidates <- tryCatch(utils::read.csv(candidate_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      if (is.data.frame(transcript_candidates) && nrow(transcript_candidates) > 0) {
        geo_transcript_candidates(transcript_candidates)
      }
    }
    geo_stage(list(
      step = "Local cache",
      title = "Loaded GEO cache",
      message = paste0("Loaded local files and cached results for ", accession, " from ", cache_dir, ".")
    ))
    invisible(TRUE)
  }

  observeEvent(input$geo_accession, {
    accession <- trimws(input$geo_accession %||% "")
    if (nchar(accession) >= 6) {
      load_geo_cached_state(accession)
    }
  }, ignoreInit = TRUE)

  session$onFlushed(function() {
    accession <- isolate(trimws(input$geo_accession %||% ""))
    if (nchar(accession) >= 6) {
      load_geo_cached_state(accession)
    }
  }, once = TRUE)

  observeEvent(input$geo_build_annotation, {
    accession <- trimws(input$geo_accession %||% "")
    cache_dir <- if (nzchar(accession)) ugplot_geo_cache_dir(accession) else ""
    metadata <- geo_sample_metadata()
    if ((!is.data.frame(metadata) || nrow(metadata) == 0) && nzchar(cache_dir) && file.exists(ugplot_geo_sample_metadata_path(cache_dir, "rds"))) {
      metadata <- tryCatch(readRDS(ugplot_geo_sample_metadata_path(cache_dir, "rds")), error = function(e) data.frame())
      geo_sample_metadata(metadata)
    }
    if (!is.data.frame(metadata) || nrow(metadata) == 0) {
      geo_stage(list(step = "Step 6", title = "Missing sample metadata", message = "Fetch sample metadata before building CpG annotation."))
      return()
    }
    platform_id <- ugplot_geo_detect_platform(metadata)
    if (!nzchar(platform_id %||% "")) {
      geo_stage(list(step = "Step 6", title = "Missing platform", message = "Sample metadata does not include a usable platform_id."))
      return()
    }
    platform_info <- ugplot_geo_platform_annotation_package(platform_id)
    if (is.null(platform_info)) {
      geo_stage(list(step = "Step 6", title = "Unsupported platform", message = paste0("No built-in annotation mapping is configured for ", platform_id, ".")))
      return()
    }

    missing_packages <- ugplot_geo_missing_annotation_packages(platform_info)
    if (length(missing_packages) > 0) {
      geo_pending_annotation_platform(platform_info)
      geo_stage(list(
        step = "Step 6",
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
        step = "Step 6",
        title = "CpG annotation unavailable",
        message = conditionMessage(e)
      ))
    })
  })

  observeEvent(input$geo_confirm_install_annotation, {
    platform_info <- geo_pending_annotation_platform()
    if (is.null(platform_info)) {
      removeModal()
      geo_stage(list(step = "Step 6", title = "No pending install", message = "No annotation platform is waiting for package installation."))
      return()
    }
    removeModal()
    missing_packages <- ugplot_geo_missing_annotation_packages(platform_info)
    if (length(missing_packages) == 0) {
      tryCatch(load_geo_annotation_cache_for_platform(platform_info), error = function(e) {
        geo_stage(list(step = "Step 6", title = "CpG annotation unavailable", message = conditionMessage(e)))
      })
      return()
    }

    geo_stage(list(
      step = "Step 6",
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
        step = "Step 6",
        title = "CpG annotation install failed",
        message = conditionMessage(e)
      ))
    })
  })

  observeEvent(input$geo_run_spearman, {
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
      geo_stage(list(step = "Step 6", title = "Select target", message = "Choose a metadata target column before running Spearman by CpG."))
      return()
    }
    matrix_files <- ugplot_geo_matrix_files(cache_dir)
    if (length(matrix_files) == 0) {
      geo_stage(list(step = "Step 6", title = "Missing matrix files", message = "Download and extract matrix files before running CpG correlation."))
      return()
    }

    max_cpgs <- suppressWarnings(as.integer(input$geo_spearman_max_cpgs %||% 50000))
    geo_spearman_results(data.frame())
    geo_spearman_raw_results(data.frame())
    geo_transcript_candidates(data.frame())
    geo_status(ugplot_geo_append_log(
      geo_status(),
      paste0("Running Spearman scan for target '", target_column, "' across ", length(matrix_files), " matrix file(s).")
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
      results_path <- file.path(cache_dir, paste0("ugplot_geo_spearman_", target_column, ".csv"))
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
        annotated_path <- file.path(cache_dir, paste0("ugplot_geo_spearman_", target_column, "_annotated.csv"))
        utils::write.csv(annotated_results, annotated_path, row.names = FALSE)
        display_results <- annotated_results

        transcript_summary <- ugplot_geo_group_spearman_annotation(annotated_results, "Transcript")
        if (is.data.frame(transcript_summary) && nrow(transcript_summary) > 0) {
          transcript_path <- file.path(cache_dir, paste0("ugplot_geo_spearman_", target_column, "_by_transcript.csv"))
          utils::write.csv(transcript_summary, transcript_path, row.names = FALSE)
        }
        gene_summary <- ugplot_geo_group_spearman_annotation(annotated_results, "Gene")
        if (is.data.frame(gene_summary) && nrow(gene_summary) > 0) {
          gene_path <- file.path(cache_dir, paste0("ugplot_geo_spearman_", target_column, "_by_gene.csv"))
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
          " matched samples. Saved files: ", paste(saved_files, collapse = "; "), "."
        )
      ))
      geo_stage(list(
        step = "Step 6",
        title = "CpG Spearman scan complete",
        message = paste0(
          "Saved all ", nrow(results), " raw CpG results for target '", target_column, "'.",
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

  observeEvent(input$geo_transcript_absrho_threshold, {
    if (is.data.frame(geo_spearman_raw_results()) && nrow(geo_spearman_raw_results()) > 0) {
      build_geo_transcript_candidates(update_stage = TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$geo_load_transcript_candidates, {
    candidates <- geo_transcript_candidates()
    if (!is.data.frame(candidates) || nrow(candidates) == 0) {
      geo_stage(list(step = "Step 6", title = "No transcript table", message = "Build transcript candidates by running Spearman with annotation first."))
      return()
    }
    dff <<- as.data.frame(candidates, stringsAsFactors = FALSE, check.names = FALSE)
    original_dataset_filename(paste0(trimws(input$geo_accession %||% "GEO"), "_transcript_candidates"))
    geo_preview_data(utils::head(dff, 100))
    load_dataset_into_table(session)
    refresh_counter(refresh_counter() + 1)
    update_scramble_selector()
    updateTabsetPanel(session, "tabs", selected = "TABLE")
    geo_stage(list(
      step = "TABLE",
      title = "Transcript table loaded",
      message = paste0("Loaded ", nrow(dff), " transcript/CpG rows into TABLE for downstream selection and analysis.")
    ))
    geo_status(ugplot_geo_append_log(geo_status(), paste0("Loaded transcript candidate table into TABLE: ", nrow(dff), " rows x ", ncol(dff), " columns.")))
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
      filtered <- apply_missing_filters(
        predictors = preview$predictors,
        missing_definition = preview$missing_definition,
        zero_exceptions = preview$zero_exceptions,
        threshold_cols = input$ml_missing_threshold_cols,
        threshold_rows = input$ml_missing_threshold_rows
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
          threshold_scope = "full_before_split"
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
    filtered <- apply_missing_filters(
      predictors = predictors,
      missing_definition = preview$missing_definition,
      zero_exceptions = preview$zero_exceptions,
      threshold_cols = col_threshold,
      threshold_rows = row_threshold
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
        "Thresholds are always applied to the full dataset before split (Mode B)."
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
    tags$div(
      style = "margin: 8px 0 12px 0; padding: 10px; background: #f6fbf6; border: 1px solid #cfe9cf;",
      tags$b("Best hotspot found (maximize information, minimize missingness): "),
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
        "Pareto hotspots: %s | Crossing points (same result in both orders): %s | Tested pairs: %s.",
        pareto_count, cross_count, nrow(results)
      )
    )
  })

  target_distribution_data <- reactive({
    preview <- missing_preview_data()
    target_values <- preview$subset_table[, preview$target_name, drop = TRUE]
    target_filtered <- preview$subset_table[, preview$target_name, drop = FALSE]
    filtered <- apply_missing_filters(
      predictors = preview$predictors,
      missing_definition = preview$missing_definition,
      zero_exceptions = preview$zero_exceptions,
      threshold_cols = input$ml_missing_threshold_cols,
      threshold_rows = input$ml_missing_threshold_rows
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

  remote_server_name_for_job <- function(job_id) {
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
        }
        active_count <- if (is.data.frame(jobs) && nrow(jobs) > 0 && "state" %in% names(jobs)) {
          sum(as.character(jobs$state) %in% c("queued", "running"), na.rm = TRUE)
        } else {
          0L
        }
        add_server_connection_row(data.frame(
          server = server_name,
          state = if (!isTRUE(version_matches)) "version_mismatch" else if (active_count > 0) "active" else "idle",
          jobs = nrow(jobs),
          active = active_count,
          message = if (!isTRUE(version_matches)) version_message else if (active_count > 0) paste(active_count, "active") else "idle",
          interface_version = local_version,
          server_version = if (nzchar(remote_version)) remote_version else NA_character_,
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
      preferred_columns <- c("server", "id", "name", "type", "state", "progress", "message", "target", "models", "created_at", "updated_at", "pid")
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

  remote_job_progress_label <- function(progress) {
    progress_value <- suppressWarnings(as.numeric(progress))
    if (length(progress_value) == 0 || !is.finite(progress_value[[1]])) {
      return("N/A")
    }
    paste0(round(max(0, min(1, progress_value[[1]])) * 100), "%")
  }

  remote_status_summary_text <- function(status) {
    if (!is.list(status)) {
      return("")
    }
    paste0(
      "Job ", status$id %||% "",
      " | pid: ", status$pid %||% "N/A",
      " | state: ", status$state %||% "unknown",
      " | progress: ", remote_job_progress_label(status$progress %||% NA_real_),
      " | ", status$message %||% ""
    )
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

    if (remote_status_has_result(status)) {
      if (isTRUE(switch_to_ml)) {
        result <- ugplot_remote_get_result(
          server_url = server$url,
          job_id = job_id,
          token = server$token %||% ""
        )
        remote_job_preview_result(ugplot_job_result_preview(result))
        apply_remote_ml_result(result, job_id)
        updateTabsetPanel(session, "tabs", selected = "MACHINE LEARNING")
        status_text <- paste(status_text, "| full result loaded")
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
    remote_job_status_text(status_text)
    invisible(status)
  }

  load_remote_job_bundle_locally <- function(job_id, server_name = NULL) {
    req(nzchar(job_id %||% ""))
    server <- remote_server_by_name(server_name %||% remote_server_name_for_job(job_id))
    activate_remote_server_for_job(server)
    if (!remote_server_supports("job_bundle", server$name[[1]])) {
      result <- ugplot_remote_get_result(
        server_url = server$url,
        job_id = job_id,
        token = server$token %||% ""
      )
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
    if (!is.data.frame(dataset)) {
      stop("Remote job bundle did not include a data.frame dataset.", call. = FALSE)
    }
    config <- bundle$config %||% list()

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
      jobs$progress <- vapply(jobs$progress, remote_job_progress_label, character(1))
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
          buttons <- c(buttons, paste0("<button type='button' class='btn btn-default btn-sm' onclick=\"event.stopPropagation(); Shiny.setInputValue('remote_load_result_row', '", action_key, "', {priority: 'event'});\">Load</button>"))
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
    if (!nzchar(input$remote_job_id %||% "")) {
      return()
    }
    tryCatch({
      refresh_remote_job_preview(input$remote_job_id, switch_to_ml = FALSE)
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
      updateTextInput(session, "remote_job_id", value = action$job_id)
      load_remote_job_bundle_locally(action$job_id, server_name = action$server)
    }, error = function(e) {
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
      tags$div(
        style = paste0(
          "border-left: 5px solid ", style$border, "; background: ", style$background,
          "; color: ", style$color, "; padding: 6px 10px; min-width: 150px;",
          "border-radius: 4px; box-shadow: inset 0 0 0 1px rgba(0,0,0,0.04);"
        ),
        tags$div(style = "font-weight: 700; font-size: 12px; line-height: 1.2;", htmltools::htmlEscape(row$server[[1]])),
        tags$div(style = "font-size: 11px; line-height: 1.25;", style$label),
        tags$div(style = "font-size: 11px; line-height: 1.25;", paste0(jobs_label, active_label)),
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

  output$remote_job_running_summary <- renderUI({
    result <- remote_job_preview_result()
    if (!is.list(result) || !is.list(result$final_summary)) {
      return(NULL)
    }
    ml_final_summary_ui(result$final_summary)
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
    metric_data <- remote_job_metric_values()
    if (is.null(metric_data)) {
      return("")
    }
    paste(
      remote_status_summary_text(status),
      format_running_stability_signal(metric_data$values, metric_name = metric_data$metric_name),
      sep = "\n\n"
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
          print(paste("Threshold scope:", threshold_scope, "| Missing strategy:", input$ml_missing_strategy, "| Imputation scope:", imputation_scope))
          if (identical(threshold_scope, "full_before_split")) {
            predictors_all <- X[, setdiff(colnames(X), target_name), drop = FALSE]
            filtered_all <- apply_missing_filters(
              predictors = predictors_all,
              missing_definition = missing_definition,
              zero_exceptions = zero_exceptions,
              threshold_cols = input$ml_missing_threshold_cols,
              threshold_rows = input$ml_missing_threshold_rows
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
              threshold_scope = "full_before_split"
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
              threshold_scope = threshold_scope
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
