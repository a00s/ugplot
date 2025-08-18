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
path_to_2dplotlist <- function() {
  system.file("extdata", "2dplotlist.csv", package = "ugplot")
}
lines <- readLines(path_to_2dplotlist())
lines <- lines[!startsWith(trimws(lines), "#")]
plotlist2d <- read.csv(text = lines, sep = ";", header = TRUE)

path_to_plotlist <- function() {
  system.file("extdata", "plotlist.csv", package = "ugplot")
}
lines <- readLines(path_to_plotlist())
lines <- lines[!startsWith(trimws(lines), "#")]
plotlist <- read.csv(text = lines, sep = ";", header = TRUE)

path_to_palette <- function() {
  system.file("extdata", "palette.csv", package = "ugplot")
}
lines <- readLines(path_to_palette())
lines <- lines[!startsWith(trimws(lines), "#")]
palettelist <- read.csv(text = lines, sep = ";", header = TRUE)

path_to_css <- function() {
  system.file("extdata", "styles.css", package = "ugplot")
}

path_to_sample_data <- function() {
  system.file("extdata", "sample.csv", package = "ugplot")
}
lines <- readLines(path_to_sample_data())
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

getImage <- function(fileName) {
  dataURI(file = system.file("extdata", fileName, package = "ugplot"),
    mime = "image/png")
}

# Define the UI of the application
ui <- fluidPage(
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
  tags$style(".small-input { width: 100px; }"),
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
        style = "display: inline-block; vertical-align: top;",
        selectizeInput("ml_target", "Target column (healthy, cancer, ...)", choices = ""),
        # Aqui, os inputs para seeds e timeout foram organizados em um fluidRow
        conditionalPanel(
          condition = "input.ml_target != ''",
          fluidRow(
            column(2, numericInput("ml_dataset_seedi", "Initial Dataset Seed:", step = 1, value = 1)),
            column(2, numericInput("ml_dataset_seedf", "Final Dataset Seed:", step = 1, value = 1)),
            column(2, numericInput("ml_seedi", "Initial Training Seed:", step = 1, value = 1)),
            column(2, numericInput("ml_seedf", "Final Training Seed:", step = 1, value = 1)),
            column(2, numericInput("ml_timeout", "Timeout (s):", step = 1, value = 1200))
          )
        ),
        conditionalPanel(
          condition = "input.ml_target != ''",
          tags$div(
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
        ## NOVO: mostrar variável alvo do modelo
        uiOutput("model_target_var_ui"),

        ## NOVO: escolher no dataset qual coluna é o ground truth
        selectInput("dataset_response_col", "Target column:",
                                                   choices = NULL,
                                                   selected = NULL),

        # Input for confidence threshold
        numericInput("confidence_threshold", "Confidence Threshold", value = 0.8, min = 0, max = 1, step = 0.01),
        actionButton("run_model_analysis", "Run Analysis"),
        br(), br(),
        # Extra metrics will be displayed here (before the table)
        verbatimTextOutput("model_analysis_accuracy"),
        br(),
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

  ml_data_table <- reactiveVal()
  ml_table_results <- reactiveVal()
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
      paste0("ugplot_best_model.rds")
    },
    content = function(file) {
      saveRDS(best_model_object(), file = file)
    }
  )

  output$downloadModelUI <- renderUI({
    if (!is.null(best_model_object())) {
      downloadButton("downloadBestModel", "Download best model")
    }
  })

  ####################### TAB 1) LOAD DATA
  observeEvent(input$file1, {
    file_click_count(file_click_count() + 1)
    filepath <- req(input$file1$datapath)
    skipline <- input$startfromline - 1
    tryCatch({
      df_pre <<- read.table(filepath, header = TRUE, sep = tab_separator(), row.names = 1,
        dec = ".", stringsAsFactors = FALSE, strip.white = TRUE, skip = skipline)
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
    datatable(ml_table_results(),
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
  })

  observe({
    input$keepAlive
  })

  observeEvent(input$play_search_best_model_caret, {
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)

    temp_models_list <- list()
    ml_prediction <<- list()
    ml_error_message_text("")

    tryCatch({
      withProgress(message = 'Searching the best model...', {
        best_result <- 0.00
        best_model <- ""
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
        ml_table_results("")
        do_dataset_seed <- 0
        loop_dataset_seedi <- as.numeric(input$ml_dataset_seedi)
        loop_dataset_seedf <- as.numeric(input$ml_dataset_seedf)
        if (!is.na(loop_dataset_seedi) && !is.na(loop_dataset_seedf)) {
          do_dataset_seed <- 1
        }
        for (loop_dataset_seed in loop_dataset_seedi:loop_dataset_seedf) {
          if (do_dataset_seed == 1) {
            set.seed(loop_dataset_seed)
            print(paste("SEED: ", loop_dataset_seed))
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
          all_models <- input$ml_checkbox_group
          count_model <- 0
          do_seed <- 0
          loop_seedi <- as.numeric(input$ml_seedi)
          loop_seedf <- as.numeric(input$ml_seedf)
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
                incProgress((1 * count_model / (length(input$ml_checkbox_group) + 1)),
                  detail = paste(
                    'Fitting model',
                    paste(model_name, "(", loop_dataset_seed, ":", loop_seed, ")"),
                    ". ", count_model, " of ",
                    length(input$ml_checkbox_group),
                    " (Best model: ", best_model,
                    " Result: ", best_result, ")"))
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
                ml_pred_real <- data.frame(Actual = testSet[[target_name]], Predicted = pred)
                model_prediction <- data.frame(Model = model_name, "Prediction" = ml_pred_real)
                ml_prediction[[model_name]] <<- model_prediction
                if (is.factor(testSet[[target_name]])) {
                  accuracy <- sum(pred == testSet[[target_name]]) / length(pred)
                  if (accuracy > best_result) {
                    best_result <- accuracy
                    best_model <- paste(model_name, "(", loop_dataset_seed, ":", loop_seed, ")")
                    best_model_object(model)
                  }
                  model_results <- data.frame(Model = model_name,
                    "Accuracy" = accuracy,
                    "Dataset seed" = loop_dataset_seed,
                    "Training seed" = loop_seed)
                  ml_table_results(rbind(ml_table_results(), model_results))
                  temp_models_list[[model_name]] <- model
                } else {
                  result_pred <- postResample(pred, testSet[[target_name]])
                  if (result_pred["Rsquared"] > best_result) {
                    best_result <- result_pred["Rsquared"]
                    best_model <- paste(model_name, "(", loop_dataset_seed, ":", loop_seed, ")")
                    best_model_object(model)
                  }
                  model_results <- data.frame(Model = model_name,
                    "R2" = result_pred["Rsquared"],
                    "MAE" = result_pred["MAE"],
                    "Dataset seed" = loop_dataset_seed,
                    "Training seed" = loop_seed)
                  ml_table_results(rbind(ml_table_results(), model_results))
                  if (result_pred["Rsquared"] >= 0.6) {
                    temp_models_list[[model_name]] <- model
                  }
                }
                print(head(ml_table_results()[order(-as.numeric(as.character(ml_table_results()$Accuracy))), ], 10))
              }, error = function(e) {
                ml_error_message_text(paste(ml_error_message_text(), " ", "Couldn't run model", model_name, ":", conditionMessage(e)))
                print(paste("Couldn't run model", model_name, ":", conditionMessage(e)))
              })
            }
            print(paste("Memory used:", pryr::mem_used() / 1024 / 1024))
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
    model_obj <- readRDS(input$model_file$datapath)
    loaded_model(model_obj)

    # 2) Mostra resumo do modelo
    output$model_details <- renderPrint({
      print(summary(model_obj))
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



  # Tab 6) MODEL ANALYSIS: Run analysis when clicking the button
  observeEvent(input$run_model_analysis, {
    req(loaded_model())
    req(changed_table)

    # 1) Prepara os dados
    analysis_data <- as.data.frame(changed_table)
    analysis_data[is.na(analysis_data)]      <- 0
    analysis_data[analysis_data == ""]       <- 0
    analysis_data <- analysis_data[, !apply(analysis_data, 2, function(col) all(col == 0))]

    # 2) Garante que todos os features do modelo existam nos dados
    model_features <- loaded_model()$finalModel$xNames
    for (feat in model_features) {
      if (!(feat %in% colnames(analysis_data))) {
        analysis_data[[feat]] <- 0
      }
    }

    # 3) Extrai ground truth a partir do selectInput
    dataset_col <- input$dataset_response_col
    if (!is.null(dataset_col) && dataset_col %in% colnames(analysis_data)) {
      # classificação só se houver ao menos 1 nível
      if (length(loaded_model()$levels) > 0) {
        analysis_data[[dataset_col]] <- as.factor(analysis_data[[dataset_col]])
        ground_truth <- analysis_data[[dataset_col]]
      } else {
        ground_truth <- as.numeric(analysis_data[[dataset_col]])
      }
      # remove a coluna alvo das features
      analysis_data <- analysis_data[, setdiff(colnames(analysis_data), dataset_col), drop = FALSE]
    } else {
      ground_truth <- NA
    }

    sample_names   <- rownames(analysis_data)
    pred_raw       <- predict(loaded_model(), newdata = analysis_data)
    is_classif     <- length(loaded_model()$levels) > 0

    if (is_classif) {

      # ---- CLASSIFICAÇÃO ----
      predicted_class <- as.character(pred_raw)

      # tenta obter probabilidades
      probs <- tryCatch({
        predict(loaded_model(), newdata = analysis_data, type = "prob")
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

    # 5) Renderiza a tabela final
    output$model_analysis_table <- DT::renderDT({
      DT::datatable(output_table, options = list(paging = FALSE, scrollX = TRUE))
    })
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
