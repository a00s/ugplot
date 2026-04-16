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
library(keras)
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

sink("ugplot.log", split = TRUE)
# Optional: set maximum number of threads
# Sys.setenv(OMP_NUM_THREADS = 2)
# Sys.setenv(MKL_NUM_THREADS = 2)
# Sys.setenv(OPENBLAS_NUM_THREADS = 2)

options(shiny.maxRequestSize = 800 * 1024 * 1024)

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
  'gaussprRadial', 'gaussprLinear', 'rbf', 'randomGLM', 'gamLoess', 'null'
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
            )
          ),
          plotOutput("plot", height = "90%")
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
        br(),
        plotOutput("model_analysis_correlation_plot", height = "520px", width = "520px"),
        br(),
        downloadButton("downloadModelAnalysisTable", "Download analysis table (CSV)"),
        br(), br(),
        DT::DTOutput("model_analysis_table")
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

  disable("merge_all_columns")
  disable("merge_all_rows")
  session$allowReconnect(TRUE)

  ml_data_table <- reactiveVal(data.frame())
  ml_table_results <- reactiveVal(data.frame())
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

  tab_separator <- reactiveVal(",")
  file_click_count <- reactiveVal(0)
  last_file_click_count <- 0
  original_dataset_filename <- reactiveVal("model_analysis_results")
  model_analysis_results_data <- reactiveVal(data.frame())

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
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

  output$downloadModelUI <- renderUI({
    if (!is.null(best_model_object())) {
      downloadButton("downloadBestModel", "Download best model")
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

  ####################### TAB 1) LOAD DATA
  observeEvent(input$file1, {
    file_click_count(file_click_count() + 1)
    filepath <- req(input$file1$datapath)
    original_dataset_filename(input$file1$name)
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
    return(subset_table)
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
  })

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
    load_checkbox_group()
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

  observeEvent(input$merge_all_rows, {
    column_names <- strsplit(input$textarea_columns, "\n")[[1]]
    rown_names <- strsplit(input$textarea_rows, "\n")[[1]]
    new_df <- df_pre[rown_names, column_names, drop = FALSE]
    common_rownames <- intersect(rownames(dff), rownames(new_df))
    dff[common_rownames, names(new_df)] <<- new_df[common_rownames, ]
    changed_table <<- as.data.frame(dff)
    load_checkbox_group()
    updateTabsetPanel(session, "tabs", selected = "2) TABLE")
  })

  observeEvent(input$process_table_content, {
    load_file_into_table(input$textarea_columns, input$textarea_rows, session)
  })

  observeEvent(input$load_sample, {
    dff <<- sample_data
    original_dataset_filename("sample.csv")
    reset_missing_strategy_ui()
    head(dff)
    load_dataset_into_table(session)
  })

  observeEvent(input$separator, {
    tab_separator(input$separator)
  })

  ####################### TAB 3) HEATMAP PLOT
  lapply(1:nrow(plotlist), function(i) {
    bname <- paste0("buttonplot", i)
    observeEvent(input[[bname]], {
      output$plot <- renderPlot({
        numeric_table <- ""
        comandtorun <- plotlist$code[i]
        updateTextAreaInput(session, "textarea_code_plot", value = comandtorun)
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
          # Insert code for "ROW x COL" plot if needed
        } else if (input$plot_xy == "COL x COL") {
          numeric_table <- cor(numeric_table)
        } else if (input$plot_xy == "ROW x ROW") {
          # Aqui calcula correlação entre amostras
          numeric_table <- cor(t(numeric_table), use = "pairwise.complete.obs")
        }
        comandtorun <- gsub("\\{\\{dataset\\}\\}", "numeric_table", comandtorun)
        if (plotlist$palette[i] != "" && changed_palette == 0) {
          comandpalette <- paste("defaultpalette(", plotlist$palette[i], ")")
          eval(parse(text = comandpalette))
        }
        comandtorun <- gsub("\\{\\{palette\\}\\}", "defaultpalette()", comandtorun)
        comandtorun <- gsub("\\{\\{annotation\\}\\}", "annotation_row", comandtorun)
        annotation_colors_auto <- generate_annotation_colors(annotation_row)
        comandtorun <- gsub("\\{\\{annotation_color\\}\\}", "annotation_colors_auto", comandtorun)
        eval(parse(text = comandtorun))
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
      numeric_table <- ""
      comandtorun <- input$textarea_code_plot
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
        # Code for ROW x COL if needed
      } else if (input$plot_xy == "COL x COL") {
        numeric_table <- cor(numeric_table)
      }
      comandtorun <- gsub("\\{\\{dataset\\}\\}", "numeric_table", comandtorun)
      comandtorun <- gsub("\\{\\{palette\\}\\}", "defaultpalette()", comandtorun)
      comandtorun <- gsub("\\{\\{annotation\\}\\}", "annotation_row", comandtorun)
      annotation_colors_auto <- generate_annotation_colors(annotation_row)
      comandtorun <- gsub("\\{\\{annotation_color\\}\\}", "annotation_colors_auto", comandtorun)
      eval(parse(text = comandtorun))
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
    load_checkbox_group()
  })

  ####################### TAB 4) 2D PLOT
  lapply(1:nrow(plotlist2d), function(i) {
    bname <- paste0("buttonplot2d", i)
    observeEvent(input[[bname]], {
      output$plots <- renderUI({
        comandtorun <- plotlist2d$code[i]
        cols_to_convert <- intersect(input$checkbox_group_categories, input$column_checkbox_group)
        numeric_table <- data.frame(changed_table[input$row_checkbox_group, input$column_checkbox_group])
        numeric_table <- numeric_table[, !(names(numeric_table) %in% cols_to_convert)]
        numeric_table <- apply(numeric_table, c(1, 2), as.numeric)
        X <- numeric_table
        cor_matrix <- cor(X, method = input$correlation)
        num_cols <- ncol(X)
        plots_list <- list()
        for (i in 1:num_cols) {
          for (j in i:num_cols) {
            if (j != i &&
                (cor_matrix[i, j] >= input$correlation_threshhold ||
                    cor_matrix[i, j] <= input$correlation_threshhold_negative)) {
              print(paste(rownames(cor_matrix)[i], colnames(cor_matrix)[j], cor_matrix[i, j]))
              comandtorun <- gsub("\\{\\{X\\}\\}", "X[, colnames(X)[i]]", comandtorun)
              comandtorun <- gsub("\\{\\{Y\\}\\}", "X[, colnames(X)[j]]", comandtorun)
              comandtorun <- gsub("\\{\\{X_NAME\\}\\}", "colnames(X)[i]", comandtorun)
              comandtorun <- gsub("\\{\\{Y_NAME\\}\\}", "colnames(X)[j]", comandtorun)
              comandtorun <- gsub("\\{\\{CORRELATION\\}\\}", "cor_matrix[i, j]", comandtorun)
              p <- eval(parse(text = comandtorun))
              plots_list[[length(plots_list) + 1]] <- p
            }
          }
        }
        texthtml <- paste(length(plots_list), " correlations found within those parameters")
        output$plotLoadingIndicator <- renderUI({
          h4(texthtml, style = "text-align: center;", br(), br())
        })
        plots_with_spacers <- list()
        for (index in 1:length(plots_list)) {
          plots_with_spacers[[length(plots_with_spacers) + 1]] <- plots_list[[index]]
          if (index != length(plots_list)) {
            spacer <- div(style = "margin-top: 40px;")
            plots_with_spacers[[length(plots_with_spacers) + 1]] <- spacer
          }
        }
        do.call(tagList, plots_with_spacers)
      })
    })
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

    tryCatch({
      withProgress(message = 'Searching the best model...', {
        best_result <- -Inf
        best_model <- "-"
        worst_result <- Inf
        worst_model <- "-"
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
        do_dataset_seed <- 0
        loop_dataset_seedi <- as.numeric(input$ml_dataset_seedi)
        loop_dataset_seedf <- as.numeric(input$ml_dataset_seedf)
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
            next
          }
          all_models <- input$ml_checkbox_group
          count_model <- 0
          do_seed <- 0
          loop_seedi <- as.numeric(input$ml_seedi)
          loop_seedf <- as.numeric(input$ml_seedf)
          total_seed_runs <- if (!is.na(loop_seedi) && !is.na(loop_seedf)) {
            max(1, (loop_seedf - loop_seedi + 1))
          } else {
            1
          }
          if (!is.na(loop_seedi) && !is.na(loop_seedf)) {
            do_seed <- 1
          }
          for (model_name in all_models) {
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
            ctrl <- trainControl(method = "cv", number = 10)
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
                    "Model progress: ", count_model, "/", length(input$ml_checkbox_group),
                    " | Seed progress: ", seed_position, "/", total_seed_runs, "\n",
                    "Worst ", metric_label, ": ", if (is.finite(worst_result)) round(worst_result, 4) else "N/A",
                    " (", worst_model, ")\n",
                    "Best ", metric_label, ": ", if (is.finite(best_result)) round(best_result, 4) else "N/A",
                    " (", best_model, ")"
                  ))
                formula <- as.formula(paste(target_name, "~ ."))
                model <- NULL
                result <- tryCatch({
                  withTimeout({
                    model <- caret::train(formula, data = trainSet, method = model_name, trControl = ctrl)
                    model
                  }, timeout = input$ml_timeout, onTimeout = "error")
                }, TimeoutException = function(ex) {
                  ml_error_message_text(paste(ml_error_message_text(), " ", "TIMEOUT:", model_name, "/"))
                  print(paste("Training timed out for model:", model_name))
                  return(NULL)
                }, error = function(e) {
                  print(paste("Error training model", model_name, ":", conditionMessage(e)))
                  return(NULL)
                })
                if (is.null(result)) next
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
              }, error = function(e) {
                ml_error_message_text(paste(ml_error_message_text(), " ", "Couldn't run model", model_name, ":", conditionMessage(e)))
                print(paste("Couldn't run model", model_name, ":", conditionMessage(e)))
              })
            }
            memory_used_mb <- if (requireNamespace("pryr", quietly = TRUE)) {
              as.numeric(pryr::mem_used()) / 1024 / 1024
            } else {
              sum(gc()[, 2])
            }
            print(paste("Memory used (MB):", round(memory_used_mb, 2)))
            gc()
          }
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

    # 3) Prepara vetor de colunas do dataset
    cols_dataset <- colnames(changed_table)

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

    # 4.4) Se ainda vazio ou não estiver no dataset, usar o que o usuário selecionou
    selected_manual <- input$dataset_response_col
    if (!(model_target %in% cols_dataset)) {
      model_target <- selected_manual
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
    req(changed_table)
    analysis_data_raw <- as.data.frame(changed_table)
    analysis_data_raw[analysis_data_raw == ""] <- NA
    analysis_data_raw <- analysis_data_raw[, !apply(analysis_data_raw, 2, function(col) all(col == 0)), drop = FALSE]

    dataset_col <- input$dataset_response_col
    if (!is.null(dataset_col) && dataset_col %in% colnames(analysis_data_raw)) {
      predictors <- analysis_data_raw[, setdiff(colnames(analysis_data_raw), dataset_col), drop = FALSE]
    } else {
      predictors <- analysis_data_raw
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
      keep_rows = keep_rows
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
        cat("No samples left after missingness filtering.\n")
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
          cat(
            "Total items: ",            total_items,        "\n",
            "Reliable: ",               count_reliable,     "\n",
            "Inconclusive: ",           count_inconclusive, "\n",
            "Correct (reliable): ",     correct_count,      "\n",
            "Wrong (reliable): ",       wrong_count,        "\n",
            "Accuracy (reliable): ",    accuracy,           "\n",
            sep = ""
          )
        })
      } else {
        output$model_analysis_accuracy <- renderPrint({
          cat("Regressão: sem métricas de classificação.\n")
        })
      }
    } else {
      output$model_analysis_accuracy <- renderPrint({
        cat("Ground truth não disponível.\n")
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
    })

    # 5) Renderiza a tabela final
    output$model_analysis_table <- DT::renderDT({
      DT::datatable(output_table, options = list(paging = FALSE, scrollX = TRUE))
    })
    model_analysis_results_data(output_table)
  })



  session$onSessionEnded(function() {
    rm(dff, changed_table, ml_available, ml_not_available, ml_prediction, envir = globalenv())
    if (exists("df_pre")) {
      rm(df_pre, envir = globalenv())
    }
  })

  load_dataset_into_table(session)
  load_ml_list()

}  # End of server function

# Run the application
shinyApp(ui, server)
