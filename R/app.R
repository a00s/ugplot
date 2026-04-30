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

write_checkpoint_log <- function(last_model = "-", results_table = NULL, max_rows = 20) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  header_lines <- c(
    paste0("ugplot checkpoint log (last update: ", timestamp, ")"),
    paste0("Last model analyzed: ", last_model),
    ""
  )

  if (is.null(results_table) || !is.data.frame(results_table) || nrow(results_table) == 0) {
    body_lines <- "No results recorded yet."
  } else {
    metric_col <- if ("Accuracy" %in% names(results_table)) {
      "Accuracy"
    } else if ("R2" %in% names(results_table)) {
      "R2"
    } else {
      NULL
    }
    ordered_results <- results_table
    if (!is.null(metric_col)) {
      ordered_idx <- order(-as.numeric(as.character(ordered_results[[metric_col]])))
      ordered_results <- ordered_results[ordered_idx, , drop = FALSE]
    }
    ordered_results <- utils::head(ordered_results, max_rows)
    body_lines <- c(
      paste0("Latest results (top ", nrow(ordered_results), "):"),
      capture.output(print(ordered_results, row.names = FALSE))
    )
  }

  writeLines(c(header_lines, body_lines), con = log_file_path, useBytes = TRUE)
}
# Optional: set maximum number of threads
# Sys.setenv(OMP_NUM_THREADS = 2)
# Sys.setenv(MKL_NUM_THREADS = 2)
# Sys.setenv(OPENBLAS_NUM_THREADS = 2)

options(shiny.maxRequestSize = 800 * 1024 * 1024)


`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

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
  'gaussprRadial', 'gaussprLinear', 'rbf', 'randomGLM', 'null'
)
slow_models_text <- paste("Slow or problematic models automatically removed:",
  paste(slow_models, collapse = ", "))

# Global variables (seguindo o padrão utilizado)
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
  tags$head(
    tags$style(HTML("
      .shiny-notification .progress-text {
        white-space: pre-line !important;
      }
    "))
  ),
  tags$script("
    $(document).on('shiny:sessioninitialized', function(event) {
      setInterval(function() {
        Shiny.setInputValue('keepAlive', Math.random());
      }, 60000);
    });
  "),
  includeCSS(path_to_css()),
  add_busy_spinner(spin = "fading-circle"),
  useShinyjs(),
  titlePanel(tags$img(
    src = getImage("ugplot.png"), height = "50px",
    tags$span("version 1.0", style = "color: gray; font-size: 11px;")
  )),
  tabsetPanel(
    id = "tabs",
    tabPanel("1) LOAD DATA",
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
          actionButton("process_table_content", "GO TO STEP 2 (TABLE)")
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
    tabPanel("2) TABLE",
      div(
        style = "width: 100%; overflow-x: auto;",
        column(
          width = 4,
          tags$h4("Columns", style = "margin-top: 10px;"),
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
          div(class = "scrollable-table",
            div(id = "dynamic_rows")),
          actionButton("uncheck_all_rows", "Uncheck all"),
          actionButton("check_all_rows", "Check all"),
          br(), br()
        ),
        column(
          width = 4,
          tags$h4("Categories", style = "margin-top: 10px;"),
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
    tabPanel("3) HEATMAP PLOT",
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
    tabPanel("4) 2D PLOT",
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
    tabPanel("5) MACHINE LEARNING",
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
                "Auto-skip models in next rounds (timeout or low R²)",
                value = FALSE
              )
            ),
            div(
              class = "ml-threshold-input ml-skip-threshold",
              numericInput("ml_min_r2_skip", "Min R² (0-1)", value = 0, min = 0, max = 1, step = 0.01)
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
            column(
              width = 6,
              tags$h4("Models installed", style = "margin-top: 10px;"),
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
              div(class = "scrollable-table", div(id = "dynamic_machine_learning_missing")),
              actionButton("uncheck_all_ml_missing", "Uncheck all"),
              actionButton("check_all_ml_missing", "Check all"),
              actionButton("install_missing_modules", "Install libraries")
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
    # Tab 6: MODEL ANALYSIS (vertical layout)
    tabPanel("6) MODEL ANALYSIS",
      fluidPage(
        # File input and model details display
        fileInput("model_file", "Load RDS Model", accept = c(".rds")),
        verbatimTextOutput("model_details"),
        uiOutput("model_preprocess_ui"),
        ## NOVO: mostrar variável alvo do modelo
        uiOutput("model_target_var_ui"),
        uiOutput("model_analysis_missing_features_ui"),

        ## NOVO: escolher no dataset qual coluna é o ground truth
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
        downloadButton("downloadModelAnalysisMetricsTxt", "Download metrics report (TXT)"),
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
    tabPanel("7) DEEP LEARNING",
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
              selected = "auto"
            ),
            sliderInput("dl_test_split", "Test split (%):", min = 10, max = 40, value = 20, step = 5),
            numericInput("dl_seed", "Random seed:", value = 42, min = 1, step = 1),
            numericInput("dl_epochs", "Epochs:", value = 50, min = 5, step = 5),
            numericInput("dl_batch_size", "Batch size:", value = 32, min = 4, step = 4),
            numericInput("dl_hidden_layers", "Number of hidden layers:", value = 2, min = 1, step = 1),
            uiOutput("dl_hidden_units_ui"),
            numericInput("dl_learning_rate", "Learning rate:", value = 0.001, min = 0.0001, step = 0.0001),
            numericInput("dl_weight_decay", "Weight decay (L2):", value = 0.0001, min = 0, step = 0.0001),
            uiOutput("dl_dropout_ui"),
            checkboxInput("dl_scale_target", "Scale numeric target (regression)", value = TRUE),
            checkboxInput("dl_auto_arch", "Auto adjust hidden layer sizes", value = TRUE),
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
            DT::DTOutput("dl_predictions_table")
          )
        )
      )
    ),
    tabPanel("8) GRAPH MODELS",
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
    )
  )
)

# --- Helper functions (defined globally) ---

load_ml_list <- function() {
  all_models <- getModelInfo()
  ml_available <<- list()
  ml_not_available <<- list()
  for (model_name in names(all_models)) {
    if (any(!all_models[[model_name]]$library %in% installed.packages())) {
      ml_not_available <<- c(ml_not_available, model_name)
    } else {
      if (!(model_name %in% slow_models)) {
        ml_available <<- c(ml_available, model_name)
      }
    }
  }
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
  updateTabsetPanel(localsession, "tabs", selected = "2) TABLE")
  enable("merge_all_columns")
  enable("merge_all_rows")
  showTab(inputId = "tabs", target = "2) TABLE")
  showTab(inputId = "tabs", target = "3) HEATMAP PLOT")
  showTab(inputId = "tabs", target = "4) 2D PLOT")
  showTab(inputId = "tabs", target = "5) MACHINE LEARNING")
  showTab(inputId = "tabs", target = "6) MODEL ANALYSIS")
  showTab(inputId = "tabs", target = "7) DEEP LEARNING")
  showTab(inputId = "tabs", target = "8) GRAPH MODELS")
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
    updateTabsetPanel(localsession, "tabs", selected = "2) TABLE")
    enable("merge_all_columns")
    enable("merge_all_rows")
    showTab(inputId = "tabs", target = "2) TABLE")
    showTab(inputId = "tabs", target = "3) HEATMAP PLOT")
    showTab(inputId = "tabs", target = "4) 2D PLOT")
    showTab(inputId = "tabs", target = "5) MACHINE LEARNING")
    showTab(inputId = "tabs", target = "6) MODEL ANALYSIS")
    showTab(inputId = "tabs", target = "7) DEEP LEARNING")
    showTab(inputId = "tabs", target = "8) GRAPH MODELS")
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

  hideTab(inputId = "tabs", target = "2) TABLE")
  hideTab(inputId = "tabs", target = "3) HEATMAP PLOT")
  hideTab(inputId = "tabs", target = "4) 2D PLOT")
  hideTab(inputId = "tabs", target = "5) MACHINE LEARNING")
  hideTab(inputId = "tabs", target = "6) MODEL ANALYSIS")
  hideTab(inputId = "tabs", target = "7) DEEP LEARNING")
  hideTab(inputId = "tabs", target = "8) GRAPH MODELS")

  disable("merge_all_columns")
  disable("merge_all_rows")
  session$allowReconnect(TRUE)

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
    if (!is.null(best_model_object())) {
      tags$div(
        style = "display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
        downloadButton("downloadBestModel", "Download best model"),
        downloadButton("downloadCalculatedMLTable", "Download calculated table (CSV)")
      )
    }
  })

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

  output$downloadModelAnalysisMetricsTxt <- downloadHandler(
    filename = function() {
      source_name <- original_dataset_filename()
      if (!is.null(input$file1$name) && nzchar(input$file1$name)) {
        source_name <- input$file1$name
      }
      base_name <- tools::file_path_sans_ext(basename(source_name))
      if (is.null(base_name) || !nzchar(base_name)) {
        base_name <- "model_analysis_metrics"
      }
      paste0(base_name, "_metrics_report.txt")
    },
    contentType = "text/plain",
    content = function(file) {
      report_txt <- model_analysis_metrics_report()
      req(nzchar(report_txt))
      writeLines(report_txt, con = file, useBytes = TRUE)
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
    tryCatch({
      df_pre <<- read.table(filepath, header = TRUE, sep = tab_separator(), row.names = 1,
        dec = ".", stringsAsFactors = FALSE, strip.white = TRUE, skip = skipline)
      reset_missing_strategy_ui()
      updateTextAreaInput(session, "textarea_columns", value = paste(names(df_pre), collapse = "\n"))
      updateTextAreaInput(session, "textarea_rows", value = paste(rownames(df_pre), collapse = "\n"))
    }, error = function(e) {
      error_info <- ""
      if (e$message == "duplicate 'row.names' are not allowed") {
        data <- read.table(filepath, header = TRUE, sep = tab_separator(), row.names = NULL,
          dec = ".", stringsAsFactors = FALSE, strip.white = TRUE, skip = skipline)
        error_info <- toString(unique(data[duplicated(data[, 1]) | duplicated(data[, 1], fromLast = TRUE), 1]))
      }
      showModal(modalDialog(
        title = "Error",
        paste(e$message, error_info),
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

  output$ml_final_status <- renderUI({
    summary_data <- ml_final_summary()
    if (is.null(summary_data)) {
      return(NULL)
    }
    fmt <- function(value, digits = 2) {
      if (is.null(value) || length(value) == 0 || !is.finite(as.numeric(value))) {
        return("N/A")
      }
      format(round(as.numeric(value), digits), nsmall = digits, trim = TRUE)
    }
    best_model_title <- paste0(summary_data$best_model, "(", summary_data$dataset_seed, ":", summary_data$training_seed, ")")
    if (identical(summary_data$metric_name, "R2")) {
      tags$div(
        class = "ml-final-summary",
        tags$strong("Best result"),
        tags$div(
          class = "ml-final-summary-content",
          tags$div(best_model_title),
          tags$div(paste0("Min R²: ", fmt(summary_data$best_model_min))),
          tags$div(paste0("Max R²: ", fmt(summary_data$best_model_max))),
          tags$div(paste0("Range R²: ", fmt(summary_data$best_model_range))),
          tags$strong("Medians:"),
          tags$div(paste0("R²: ", fmt(summary_data$best_model_median), " (IQR ", fmt(summary_data$best_model_iqr), ")")),
          tags$div(paste0("MAE: ", fmt(summary_data$best_model_mae_median), " (IQR ", fmt(summary_data$best_model_mae_iqr), ")")),
          tags$div(paste0("RMSE: ", fmt(summary_data$best_model_rmse_median), " (IQR ", fmt(summary_data$best_model_rmse_iqr), ")"))
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
          tags$div(paste0(summary_data$metric_name, ": ", fmt(summary_data$best_model_median), " (IQR ", fmt(summary_data$best_model_iqr), ")"))
        )
      )
    }
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
    updateTabsetPanel(session, "tabs", selected = "2) TABLE")
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
    updateTabsetPanel(session, "tabs", selected = "2) TABLE")
  })

  observeEvent(input$process_table_content, {
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
      # sem transformação adicional
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
    if (!is.data.frame(ml_results)) {
      ml_results <- data.frame()
    }
    datatable(ml_results,
      options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, info = FALSE),
      rownames = FALSE)
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
        print("Variable importance not supported or R^2 smaller than 0.6 for this model.")
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
    all_models <- getModelInfo()
    models_to_install <- input$ml_missing_checkbox_group
    for (model_name in models_to_install) {
      model_info <- getModelInfo(model_name, regex = FALSE)[[model_name]]
      model_libraries <- model_info$library
      for (librarytoinst in model_libraries) {
        if (!(librarytoinst %in% installed.packages())) {
          install.packages(librarytoinst, dependencies = TRUE)
        } else {
          print("Library already installed.")
        }
      }
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
  })

  observe({
    input$keepAlive
  })

  observeEvent(input$play_search_best_model_caret, {
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)

    temp_models_list <- list()
    ml_prediction <<- list()
    best_model_preprocess(NULL)
    ml_error_message_text("")
    ml_final_summary(NULL)

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
        write_checkpoint_log(last_model = "-", results_table = ml_table_results())
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
        total_seed_runs <- if (!is.na(loop_seedi) && !is.na(loop_seedf)) {
          max(1, (loop_seedf - loop_seedi + 1))
        } else {
          1
        }
        metric_label <- if (is.factor(Y)) "Accuracy" else "R2 (MAE/RMSE na tabela)"
        if (!is.na(loop_dataset_seedi) && !is.na(loop_dataset_seedf)) {
          do_dataset_seed <- 1
        }
        for (loop_dataset_seed in loop_dataset_seedi:loop_dataset_seedf) {
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
            next
          }
          count_model <- 0
          do_seed <- 0
          if (!is.na(loop_seedi) && !is.na(loop_seedf)) {
            do_seed <- 1
          }
          for (model_name in all_models) {
            if (!(model_name %in% active_models)) {
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
            model_types <- model_info$type
            print(paste("Model", model_name, "supports types:", paste(model_types, collapse = ", ")))
            for (loop_seed in loop_seedi:loop_seedf) {
              if (do_seed == 1) {
                set.seed(loop_seed)
              }
              tryCatch({
                seed_position <- if (do_seed == 1) (loop_seed - loop_seedi + 1) else 1
                incProgress((1 * count_model / (length(input$ml_checkbox_group) + 1)),
                  detail = paste0(
                    "Current model: ", model_name, "\n",
                    "Dataset/Train seed: ", loop_dataset_seed, "/", loop_seed, "\n",
                    "Threshold scope / Missing strategy / Imputation scope: ", threshold_scope, " / ", missing_strategy, " / ", imputation_scope, "\n",
                    "Model progress: ", count_model, "/", max(1, length(active_models)),
                    " | Seed progress: ", seed_position, "/", total_seed_runs, "\n",
                    "Worst ", metric_label, ": ", if (is.finite(worst_result)) round(worst_result, 4) else "N/A",
                    " (", worst_model, ")\n",
                    "Best ", metric_label, ": ", if (is.finite(best_result)) round(best_result, 4) else "N/A",
                    " (", best_model, ")"
                  ))
                formula <- as.formula(paste(target_name, "~ ."))
                model <- NULL
                write_checkpoint_log(last_model = model_name, results_table = ml_table_results())
                result <- tryCatch({
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
                }, TimeoutException = function(ex) {
                  ml_error_message_text(paste(ml_error_message_text(), " ", "TIMEOUT:", model_name, "/"))
                  print(paste("Training timed out for model:", model_name))
                  mark_model_skipped(model_name, "timeout")
                  return(NULL)
                }, error = function(e) {
                  print(paste("Error training model", model_name, ":", conditionMessage(e)))
                  return(NULL)
                })
                if (is.null(result)) {
                  next
                }
                pred <- predict(model, newdata = testSet)
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
                  }
                  if (accuracy < worst_result) {
                    worst_result <- accuracy
                    worst_model <- paste(model_name, "(", loop_dataset_seed, ":", loop_seed, ")")
                  }
                  model_results <- data.frame(Model = model_name,
                    "Accuracy" = accuracy,
                    "Dataset seed" = loop_dataset_seed,
                    "Training seed" = loop_seed,
                    "Threshold scope" = threshold_scope,
                    "Imputation scope" = imputation_scope)
                  ml_table_results(rbind(ml_table_results(), model_results))
                  model_metric_values[[model_name]] <- c(model_metric_values[[model_name]], accuracy)
                  temp_models_list[[model_name]] <- model
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
                  }
                  if (rsq_value < worst_result) {
                    worst_result <- rsq_value
                    worst_model <- paste(model_name, "(", loop_dataset_seed, ":", loop_seed, ")")
                  }
                  model_results <- data.frame(Model = model_name,
                    "R2" = rsq_value,
                    "MAE" = mae_value,
                    "RMSE" = rmse_value,
                    "Dataset seed" = loop_dataset_seed,
                    "Training seed" = loop_seed,
                    "Threshold scope" = threshold_scope,
                    "Imputation scope" = imputation_scope)
                  ml_table_results(rbind(ml_table_results(), model_results))
                  model_metric_values[[model_name]] <- c(model_metric_values[[model_name]], rsq_value)
                  model_mae_values[[model_name]] <- c(model_mae_values[[model_name]], mae_value)
                  model_rmse_values[[model_name]] <- c(model_rmse_values[[model_name]], rmse_value)
                  if (auto_skip_enabled && rsq_value < min_r2_threshold) {
                    mark_model_skipped(
                      model_name,
                      paste0("R2 ", round(rsq_value, 4), " < ", round(min_r2_threshold, 4))
                    )
                  }
                  if (rsq_value >= 0.6) {
                    temp_models_list[[model_name]] <- model
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
                write_checkpoint_log(last_model = model_name, results_table = current_results)
              }, error = function(e) {
                invalid_runs <- invalid_runs + 1
                invalid_models <- union(invalid_models, model_name)
                current_invalid <- if (!is.null(model_invalid_runs[[model_name]])) model_invalid_runs[[model_name]] else 0
                model_invalid_runs[[model_name]] <- current_invalid + 1
                ml_error_message_text(paste(ml_error_message_text(), " ", "Couldn't run model", model_name, ":", conditionMessage(e)))
                print(paste("Couldn't run model", model_name, ":", conditionMessage(e)))
              })
            }
            # pryr was archived from CRAN (2026-01-30), so we rely on base gc() only.
            memory_used_mb <- sum(gc()[, 2])
            print(paste("Memory used (MB):", round(memory_used_mb, 2)))
            gc()
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
      print(e)
    })
    all_models_reactive(temp_models_list)
    stopCluster(cl)
  })

  # Tab 6) MODEL ANALYSIS: Carrega o modelo e detecta variável‑alvo
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

    # 3) Prepara vetor de colunas ativas do dataset (seleção da aba TABLE)
    active_cols <- input$column_checkbox_group %||% character(0)
    cols_dataset <- intersect(active_cols, colnames(changed_table))
    if (length(cols_dataset) == 0) {
      cols_dataset <- colnames(changed_table)
    }

    # 4) Detecta variável‑alvo em várias etapas
    model_target <- ""

    # 4.1) Se for um objeto caret::train, o call$formula guarda a fórmula
    if (!is.null(model_obj$call$formula)) {
      model_target <- as.character(model_obj$call$formula[[2]])
    }
    # 4.2) Caso seja um objeto randomForest puro treinado por fórmula
    else if (inherits(model_obj, "randomForest") && !is.null(model_obj$terms)) {
      # extrai a segunda variável dos terms
      vars <- as.character(attr(model_obj$terms, "variables"))
      if (length(vars) >= 2) model_target <- vars[2]
    }
    # 4.3) Fallback para caret::train (se por algum motivo o fórmula sumiu):
    #      pegamos o .outcome no trainingData (mas aí o nome real não fica disponível)
    else if (!is.null(model_obj$trainingData)) {
      # a coluna .outcome guarda o vetor de resposta
      if (".outcome" %in% colnames(model_obj$trainingData)) {
        # não é o nome original, mas mostramos ao menos que veio do treinamento
        model_target <- ".outcome"
      }
    }

    # 4.4) Se ainda vazio ou não estiver nas colunas ativas, usar o que o usuário selecionou
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

    # 6) Atualiza o selectInput do dataset com as colunas disponíveis,
    #    e já seleciona a variável‑alvo detectada (ou manual)
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

      # ---- CLASSIFICAÇÃO ----
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

      # status confiável vs inconclusivo
      threshold <- input$confidence_threshold
      status    <- ifelse(conf_margin < threshold, "inconclusive", "reliable")

      # diferença numérica (classe codificada como número)
      diff_num <- if (!all(is.na(ground_truth))) {
        as.numeric(predicted_class) - as.numeric(as.character(ground_truth))
      } else {
        NA_real_
      }

      # monta a tabela de saída
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

      # ---- REGRESSÃO ----
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

    # 4) Métricas adicionais
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
        } else {
          pearson_r <- NA_real_
          r2 <- NA_real_
          mae <- NA_real_
          rmse <- NA_real_
        }
        output$model_analysis_accuracy <- renderPrint({
          report_txt <- paste0(
            "n=", n_pairs, "\n",
            "R^2=", format(round(r2, 2), nsmall = 2), "\n",
            "Pearson=", format(round(pearson_r, 2), nsmall = 2), "\n",
            "MAE=", format(round(mae, 2), nsmall = 2), "\n",
            "RMSE=", format(round(rmse, 2), nsmall = 2), "\n"
          )
          model_analysis_metrics_report(report_txt)
          cat(report_txt, sep = "")
        })
      }
    } else {
      output$model_analysis_accuracy <- renderPrint({
        report_txt <- "Ground truth não disponível.\n"
        model_analysis_metrics_report(report_txt)
        cat(report_txt)
      })
    }

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
              paste0("MAE: ", format(round(mae, 6), nsmall = 6))
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
        value = if (i == 1) 0.15 else 0.10,
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
      criterion <- torch::nn_smooth_l1_loss()
      clone_state_dict <- function(state_dict) {
        cloned <- lapply(state_dict, function(value) value$clone())
        names(cloned) <- names(state_dict)
        cloned
      }
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
            y_test_num <- as.numeric(y_test$to(device = "cpu")) * y_scale + y_center
            rmse <- sqrt(mean((pred_test_num - y_test_num)^2))
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

      model$load_state_dict(best_state)
      model$eval()
      torch::with_no_grad({
        pred_test <- model(x_test)
        pred_test_num <- as.numeric(pred_test$to(device = "cpu")) * y_scale + y_center
      })
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
      metrics_df <- data.frame(
        Metric = c("Task", "Train samples", "Test samples", "MAE", "RMSE", "R2", "Best epoch RMSE"),
        Value = c("regression", length(train_idx), length(test_idx), round(mae, 4), round(rmse, 4), round(r2, 4), round(best_metric, 4)),
        stringsAsFactors = FALSE
      )
      dl_log(paste0(
        "Deep Learning (regression) finished. RMSE: ",
        round(rmse, 4),
        " | R2: ",
        round(r2, 4),
        " | Best epoch RMSE: ",
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
    objects_to_remove <- c("dff", "changed_table", "ml_available", "ml_not_available", "ml_prediction", "df_pre")
    existing_objects <- objects_to_remove[vapply(objects_to_remove, exists, logical(1), envir = globalenv(), inherits = FALSE)]
    if (length(existing_objects) > 0) {
      rm(list = existing_objects, envir = globalenv())
    }
  })

  load_dataset_into_table(session)
  update_scramble_selector()
  load_ml_list()

}  # End of server function

# Run the application
shinyApp(ui, server)
