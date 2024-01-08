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

options(shiny.maxRequestSize = 800 * 1024 * 1024)

path_to_2dplotlist <-
  function()
    system.file("extdata", "2dplotlist.csv", package = "ugplot")
lines <- readLines(path_to_2dplotlist())
lines <- lines[!startsWith(trimws(lines), "#")]
plotlist2d <- read.csv(text = lines, sep = ";", header = TRUE)

path_to_plotlist <-
  function()
    system.file("extdata", "plotlist.csv", package = "ugplot")
lines <- readLines(path_to_plotlist())
lines <- lines[!startsWith(trimws(lines), "#")]
plotlist <- read.csv(text = lines, sep = ";", header = TRUE)

path_to_palette <-
  function()
    system.file("extdata", "palette.csv", package = "ugplot")
lines <- readLines(path_to_palette())
lines <- lines[!startsWith(trimws(lines), "#")]
palettelist <- read.csv(text = lines, sep = ";", header = TRUE)

path_to_css <-
  function()
    system.file("extdata", "styles.css", package = "ugplot")

slow_models <-
  c(
    'bam',
    'ANFIS',
    'DENFIS',
    'FIR.DM',
    'FS.HGD',
    'gam',
    'GFS.LT.RS',
    'GFS.FR.MOGUL',
    'GFS.THRIFT',
    'HYFIS'
  )
slow_models_text <-
  paste("Slow models automaticaly removed:",
    paste(slow_models, collapse = ", "))

df_pre <<- ""
dff <<- ""

getImage <- function(fileName) {
  dataURI(file = system.file("extdata", fileName, package = "ugplot"),
    mime = "image/png")
}

ui <- fluidPage(
  includeCSS(path_to_css()),
  add_busy_spinner(spin = "fading-circle"),
  useShinyjs(),
  titlePanel(tags$img(
    src = getImage("ugplot.png"), height = "50px"
  )),
  tags$style(".small-input { width: 100px; }") ,
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "1) LOAD DATA",
      tags$div(
        style = "display: inline-block; vertical-align: top;",
        class = "small-input",
        numericInput(
          "startfromline",
          "Start at line",
          value = 1,
          min = 1,
          step = 1
        )
      ),
      tags$div(
        style = "display: inline-block; vertical-align: top;",
        class = "small-input",
        selectInput(
          inputId = "separator",
          label = "Separator",
          choices = c("space" = " ", "tab" = "\t", ";", ",", "|"),
          selected = ","
        )
      ),
      tags$div(
        style = "display: inline-block; vertical-align: top;",
        fileInput(
          "file1",
          "Choose a CSV file",
          multiple = FALSE,
          accept = c("text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")
        )
      ),
      tags$div(style = "display: inline-block; vertical-align: top;",
        tags$div(
          tags$span(style = "font-size: 17px; color: white;", ".")
        ),
        tags$div(
          actionButton("process_table_content", "GO TO STEP 2 (TABLE)")
        )),
      conditionalPanel(
        condition = "input.textarea_columns != '' || input.textarea_rows != ''",
        tags$div(
          style = "display: inline-block; text-align: center; vertical-align: top;",
          textAreaInput(
            "textarea_columns",
            label = "",
            rows = 20,
            cols = 50
          ),
          actionButton("add_all_columns", "Add all"),
          actionButton("remove_all_columns", "Remove all"),
          actionButton("merge_all_columns", "Join columns")
        ),
        tags$div(
          style = "display: inline-block; text-align: center; vertical-align: top;",
          textAreaInput(
            "textarea_rows",
            label = "",
            rows = 20,
            cols = 50
          ),
          actionButton("add_all_rows", "Add all"),
          actionButton("remove_all_rows", "Remove all"),
          actionButton("merge_all_rows", "Join columns")
        )
      )
    ),
    tabPanel(
      "2) TABLE",
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
          br()
        ),
        column(
          width = 4,
          tags$h4("Rows", style = "margin-top: 10px;"),
          div(class = "scrollable-table",
            div(id = "dynamic_rows")),
          actionButton("uncheck_all_rows", "Uncheck all"),
          actionButton("check_all_rows", "Check all"),
          br(),
          br()
        ),
        column(
          width = 4,
          tags$h4("Categories", style = "margin-top: 10px;"),
          div(
            class = "scrollable-table",
            style = "background-color: #f7f8fa; overflow-y: auto; max-height: 200px;",
            # Add scroll and max height
            div(id = "dynamic_columns_categories")
          ),
          actionButton("transpose_table", "Transpose table", icon = icon("retweet")),
          downloadButton("downloadData", "Download"),
          br(),
          br()
        ),
        DT::DTOutput("contents")
      )
    ),

    tabPanel("3) HEATMAP PLOT",
      br(),
      fluidRow(
        column(
          width = 3,
          class = "sidebar-panel-custom",
          selectInput(
            inputId = "plot_xy",
            label = NULL,
            choices = c("ROW x COL", "COL x COL")
          ),
          div(class = "rowplotlist",
            lapply(1:nrow(plotlist), function(i) {
              bname <- paste0("buttonplot", i)
              imgname <- paste0("img/", plotlist$img[i])
              fluidRow(actionButton(
                bname,
                tags$img(
                  src = getImage(imgname),
                  height = "130px",
                  width = "130px",
                  class = "image-button"
                )
              ))
            })),
          br(),
          div(class = "rowpalettelist",
            lapply(1:nrow(palettelist), function(i) {
              bname <- paste0("buttonpalette", i)
              imgname <- paste0("img/", palettelist$img[i])
              if (imgname != "img/NA") {
                fluidRow(actionButton(
                  bname,
                  tags$img(
                    src = getImage(imgname),
                    height = "20px",
                    width = "130px",
                    class = "image-button"
                  )
                ))
              }
            }))
        ),
        column(
          width = 9,
          class = "plotheatmap",
          plotOutput("plot", height = "100%")
        )
      )),
    tabPanel("4) 2D PLOT",
      class = "sidebar-layout",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar-panel-custom2d",
          div(
            class = "rowplotlist",
            sliderInput(
              inputId = "correlation_threshhold",
              label = "Correlation >= x",
              min = 0,
              max = 1,
              value = 0.7,
              step = 0.01
            ),
            sliderInput(
              inputId = "correlation_threshhold_negative",
              label = "Negative correlation <= x",
              min = -1,
              max = 0,
              value = -0.7,
              step = 0.01
            ),
            lapply(1:nrow(plotlist2d), function(i) {
              bname <- paste0("buttonplot2d", i)
              imgname <- paste0("img/", plotlist2d$img[i])
              fluidRow(
                tags$img(
                  src = getImage(imgname),
                  width = 130,
                  height = 130
                ),
                actionButton(bname, plotlist2d$name[i])
              )
            })
          ),
        ),
        mainPanel(br(),
          uiOutput("plotLoadingIndicator"),
          uiOutput("plots"))
      )),
    tabPanel(
      "5) MACHINE LEARNING",
      tags$div(
        style = "display: inline-block; vertical-align: top;",
        selectInput(
          inputId = "ml_target",
          label = "Target column (healthy, cancer, ...)",
          choices = ""
        ),
        conditionalPanel(
          condition = "input.ml_target != ''",
          tags$div(
            verbatimTextOutput("console_output"),
            column(
              width = 6,
              tags$h4("Models installed", style = "margin-top: 10px;"),
              div(class = "scrollable-table",
                div(id = "dynamic_machine_learning")),
              actionButton("uncheck_all_ml", "Uncheck all"),
              actionButton("check_all_ml", "Check all"),
              actionButton("play_search_best_model_caret",
                "RUN"),
              tags$br(),
              tags$p(slow_models_text, style = "color: gray; font-size: 11px;")
            ),
            column(
              width = 6,
              tags$h4("Models missing", style = "margin-top: 10px;"),
              div(class = "scrollable-table",
                div(id = "dynamic_machine_learning_missing")),
              actionButton("uncheck_all_ml_missing", "Uncheck all"),
              actionButton("check_all_ml_missing", "Check all"),
              actionButton("install_missing_modules",
                "Install libraries")
            ),
            div(style = "width: 100%; overflow-x: auto;", DT::DTOutput("ml_table_results_output")),
            verbatimTextOutput("ml_row_details"),
            div(style = "width: 100%; overflow-x: auto;", DT::DTOutput("ml_table")),
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  hideTab(inputId = "tabs", target = "2) TABLE")
  hideTab(inputId = "tabs", target = "3) HEATMAP PLOT")
  hideTab(inputId = "tabs", target = "4) 2D PLOT")
  hideTab(inputId = "tabs", target = "5) MACHINE LEARNING")
  disable("merge_all_columns")
  disable("merge_all_rows")

  ml_available <- ""
  ml_not_available <- NULL
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

  defaultpalette <-
    reactiveVal(colorRampPalette(c("red", "yellow", "green"))(256))
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
      # Extract the relevant data from 'changed_table' based on user selections
      data_to_download <-
        changed_table[input$row_checkbox_group, input$column_checkbox_group]
      write.csv(data_to_download, file, row.names = TRUE)
    }
  )

  ####################### TAB 1) LOAD DATA
  observeEvent(input$file1, {
    file_click_count(file_click_count() + 1)
    filepath <- req(input$file1$datapath)
    skipline = input$startfromline - 1
    tryCatch({
      df_pre <<-
        read.table(
          filepath,
          header = TRUE,
          sep = tab_separator(),
          row.names = 1,
          dec = ".",
          stringsAsFactors = FALSE,
          strip.white = TRUE,
          skip = skipline
        )
      updateTextAreaInput(session,
        "textarea_columns",
        value = paste(names(df_pre), collapse = "\n"))
      updateTextAreaInput(session,
        "textarea_rows",
        value = paste(rownames(df_pre), collapse = "\n"))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste(e$message),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })

  output$contents <- DT::renderDT({
    if (last_file_click_count == 0 |
        (last_file_click_count != file_click_count())) {
      # This part to deal when a second file got read
    }
    if (length(input$column_checkbox_group) < 2) {
      return(NULL)
    }

    current_target_selection <- input$ml_target
    updateSelectInput(session,
      "ml_target",
      choices = names(dff),
      selected = current_target_selection)
    return(changed_table[input$row_checkbox_group, input$column_checkbox_group])
  })

  observeEvent(input$add_all_columns, {
    updateTextAreaInput(session,
      "textarea_columns",
      value = paste(names(df_pre), collapse = "\n"))
  })

  observeEvent(input$remove_all_columns, {
    updateTextAreaInput(session, "textarea_columns", value = "")
  })

  observeEvent(input$add_all_rows, {
    updateTextAreaInput(session,
      "textarea_rows",
      value = paste(rownames(df_pre), collapse = "\n"))
  })

  observeEvent(input$remove_all_rows, {
    updateTextAreaInput(session, "textarea_rows", value = "")
  })

  observeEvent(input$merge_all_columns, {
    column_names <- strsplit(input$textarea_columns, "\n")[[1]]
    rown_names <- strsplit(input$textarea_rows, "\n")[[1]]
    new_df <-
      as.data.frame(t(df_pre[rown_names, column_names, drop = FALSE]))
    common_rownames <- intersect(rownames(dff), rownames(new_df))
    dff[common_rownames, names(new_df)] <<-
      new_df[common_rownames,]
    changed_table <<- as.matrix(dff)
    load_checkbox_group()
    updateTabsetPanel(session, "tabs", selected = "2) TABLE")
  })

  observeEvent(input$merge_all_rows, {
    column_names <- strsplit(input$textarea_columns, "\n")[[1]]
    rown_names <- strsplit(input$textarea_rows, "\n")[[1]]
    new_df <- df_pre[rown_names, column_names, drop = FALSE]
    common_rownames <- intersect(rownames(dff), rownames(new_df))
    dff[common_rownames, names(new_df)] <<-
      new_df[common_rownames,]
    changed_table <<- as.data.frame(dff)
    load_checkbox_group()
    updateTabsetPanel(session, "tabs", selected = "2) TABLE")
  })

  observeEvent(input$process_table_content, {
    load_file_into_table(input$textarea_columns, input$textarea_rows, session)
  })

  observeEvent(input$separator, {
    tab_separator(input$separator)
  })

  ####################### TAB 3) PLOT

  lapply(1:nrow(plotlist), function(i) {
    bname <- paste0("buttonplot", i)
    observeEvent(input[[bname]], {
      output$plot <- renderPlot({
        numeric_table <- ""
        comandtorun <- plotlist$code[i]
        cols_to_convert <-
          intersect(input$checkbox_group_categories,
            input$column_checkbox_group)
        countdataframe <- 0
        if (length(cols_to_convert) > 0) {
          for (this_target in cols_to_convert) {
            changed_table[[this_target]] <-
              as.factor(changed_table[[this_target]])
            if (countdataframe == 0) {
              annotation_row <-
                setNames(data.frame(changed_table[[this_target]]), this_target)
              rownames(annotation_row) <- rownames(changed_table)
            } else {
              annotation_row[[this_target]] <- changed_table[[this_target]]
            }
            countdataframe <- 1
          }
        }
        numeric_table <-
          data.frame(changed_table[input$row_checkbox_group, input$column_checkbox_group])
        numeric_table <-
          numeric_table[,!(names(numeric_table) %in% cols_to_convert)]
        numeric_table <- apply(numeric_table, c(1, 2), as.numeric)
        if (input$plot_xy == "ROW x COL") {

        } else if (input$plot_xy == "COL x COL") {
          numeric_table <- cor(numeric_table)
        }


        comandtorun <-
          gsub("\\{\\{dataset\\}\\}", "numeric_table", comandtorun)
        if (plotlist$palette[i] != "" && changed_palette == 0) {
          comandpalette <- paste("defaultpalette(", plotlist$palette[i], ")")
          eval(parse(text = comandpalette))
        }
        comandtorun <-
          gsub("\\{\\{palette\\}\\}",
            "defaultpalette()",
            comandtorun)
        comandtorun <-
          gsub("\\{\\{annotation\\}\\}",
            "annotation_row",
            comandtorun)

        annotation_colors_auto <-
          generate_annotation_colors(annotation_row)
        comandtorun <-
          gsub("\\{\\{annotation_color\\}\\}",
            "annotation_colors_auto",
            comandtorun)
        eval(parse(text = comandtorun))
      })
    })
  })

  lapply(1:nrow(palettelist), function(i) {
    bname <- paste0("buttonpalette", i)
    observeEvent(input[[bname]], {
      changed_palette <<- 1
      comandpalette <-
        paste("defaultpalette(", palettelist$code[i], ")")
      eval(parse(text = comandpalette))
    })
  })

  observeEvent(input$uncheck_all_columns, {
    updateCheckboxGroupInput(session,
      inputId = "column_checkbox_group",
      selected = character(0))
  })
  observeEvent(input$check_all_columns, {
    updateCheckboxGroupInput(session,
      inputId = "column_checkbox_group",
      selected = names(dff))
  })
  observeEvent(input$uncheck_all_rows, {
    updateCheckboxGroupInput(session,
      inputId = "row_checkbox_group",
      selected = character(0))
  })
  observeEvent(input$check_all_rows, {
    updateCheckboxGroupInput(session,
      inputId = "row_checkbox_group",
      selected = rownames(dff))
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

  ####################### TAB 4) Plot 2D
  lapply(1:nrow(plotlist2d), function(i) {
    bname <- paste0("buttonplot2d", i)
    observeEvent(input[[bname]], {
      output$plots <- renderUI({
        comandtorun <- plotlist2d$code[i]
        cols_to_convert <-
          intersect(input$checkbox_group_categories,
            input$column_checkbox_group)
        numeric_table <-
          data.frame(changed_table[input$row_checkbox_group, input$column_checkbox_group])
        numeric_table <-
          numeric_table[,!(names(numeric_table) %in% cols_to_convert)]
        numeric_table <- apply(numeric_table, c(1, 2), as.numeric)
        X <- numeric_table
        cor_matrix <- cor(X)
        num_cols <- ncol(X)
        plots_list <- list()
        for (i in 1:num_cols) {
          for (j in i:num_cols) {
            if (j != i &&
                (
                  cor_matrix[i, j] >= input$correlation_threshhold ||
                    cor_matrix[i, j] <= input$correlation_threshhold_negative
                )) {
              comandtorun <-
                gsub("\\{\\{X\\}\\}",
                  "X[, colnames(X)[i]]",
                  comandtorun)
              comandtorun <-
                gsub("\\{\\{Y\\}\\}",
                  "X[, colnames(X)[j]]",
                  comandtorun)
              comandtorun <-
                gsub("\\{\\{X_NAME\\}\\}",
                  "colnames(X)[i]",
                  comandtorun)
              comandtorun <-
                gsub("\\{\\{Y_NAME\\}\\}",
                  "colnames(X)[j]",
                  comandtorun)
              comandtorun <-
                gsub("\\{\\{CORRELATION\\}\\}",
                  "cor_matrix[i, j]",
                  comandtorun)
              p <- eval(parse(text = comandtorun))
              plots_list[[length(plots_list) + 1]] <- p
            }
          }
        }

        texthtml <-
          paste(length(plots_list),
            " correlations found within those parameters")
        output$plotLoadingIndicator <- renderUI({
          h4(texthtml, style = "text-align: center;", br(), br())
        })

        # Add spacers between plots
        plots_with_spacers <- list()
        for (index in 1:length(plots_list)) {
          plots_with_spacers[[length(plots_with_spacers) + 1]] <-
            plots_list[[index]]
          if (index != length(plots_list)) {
            spacer <- div(style = "margin-top: 40px;")
            plots_with_spacers[[length(plots_with_spacers) + 1]] <-
              spacer
          }
        }
        do.call(tagList, plots_with_spacers)
      })
    })
  })

  ####### 4) Machine learning
  all_models_reactive <- reactiveVal(list())
  output$ml_table_results_output = DT::renderDT({
    datatable(
      ml_table_results(),
      selection = "single",
      options = list(
        lengthChange = FALSE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      ),
      rownames = FALSE
    )
  })

  observeEvent(input$uncheck_all_ml, {
    updateCheckboxGroupInput(session,
      inputId = "ml_checkbox_group",
      selected = character(0))
  })
  observeEvent(input$check_all_ml, {
    updateCheckboxGroupInput(session,
      inputId = "ml_checkbox_group",
      selected = ml_available)
  })

  observeEvent(input$uncheck_all_ml_missing, {
    updateCheckboxGroupInput(session,
      inputId = "ml_missing_checkbox_group",
      selected = character(0))
  })
  observeEvent(input$check_all_ml_missing, {
    updateCheckboxGroupInput(session,
      inputId = "ml_missing_checkbox_group",
      selected = ml_not_available)
  })

  output$ml_table = DT::renderDT(ml_data_table(), options = list(lengthChange = FALSE))

  output$ml_row_details <- renderPrint({
    selected_row <- input$ml_table_results_output_rows_selected
    if (length(selected_row) == 1) {
      row_data <- ml_table_results()[selected_row,]
      selected_data <- ml_table_results()[selected_row,]
      selected_model_name <-
        ml_table_results()[selected_row,]$Model
      specific_model <- all_models_reactive()[[selected_model_name]]
      print(specific_model)
      tryCatch({
        importance <- varImp(specific_model)
        print(importance)
        print(text_result_ml())
      }, error = function(e) {
        print("Variable importance not supported for this model.")
      })
    }
  })

  output$ml_row_details_html <- renderUI({
    HTML(text_result_ml())
  })

  observeEvent(input$install_missing_modules, {
    all_models <- getModelInfo()
    models_to_install <- input$ml_missing_checkbox_group
    for (model_name in models_to_install) {
      model_info <-
        getModelInfo(model_name, regex = FALSE)[[model_name]]
      model_libraries <- model_info$library
      for (librarytoinst in model_libraries) {
        if (!(librarytoinst %in% installed.packages())) {
          install.packages(librarytoinst)
        } else {
          print("Library already installed.")
        }
      }
    }
  })

  observeEvent(input$play_search_best_model_caret, {
    temp_models_list <- list()
    withProgress(
      message = 'Searching the best model...',
      min = 1,
      max = length(input$ml_checkbox_group),
      value = 0,
      {
        best_result <- 0.00
        best_model <- ""
        target_name <- input$ml_target
        X <-
          changed_table[input$row_checkbox_group, input$column_checkbox_group]
        Y <- dff[[target_name]]
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
                single_item_levels <-
                  names(freq_table[freq_table <= 2])
                toKeep <- !(Y %in% single_item_levels)
                Y <- Y[toKeep]
                X <- X[toKeep,]
                Y <- droplevels(Y)
                X[[this_target]] <- droplevels(X[[this_target]])
              }
            }
          }
        }
        ml_table_results("")
        trainIndex <- createDataPartition(Y,
          p = .8,
          list = FALSE,
          times = 1)
        trainSet <- X[trainIndex, ]
        testSet  <- X[-trainIndex, ]
        if (!is.data.frame(trainSet)) {
          trainSet <- as.data.frame(trainSet)
        }
        if (!is.data.frame(testSet)) {
          testSet <- as.data.frame(testSet)
        }

        # Get the list of all available models
        all_models <- input$ml_checkbox_group
        count_model <- 0

        for (model_name in all_models) {
          count_model <- count_model + 1
          result <- tryCatch({
            model_info <- getModelInfo(model_name, regex = FALSE)[[model_name]]
            model_libraries <- model_info$library
            for (lib in model_libraries) {
              library(lib, character.only = TRUE)
            }
          }, error = function(e) {
            print(paste("Failed to load", model_name))
          })

          ctrl <- trainControl(method = "cv", number = 10)

          # Train the model
          tryCatch({
            lmessage <-
              paste(
                'Fitting model',
                model_name,
                ". ",
                count_model,
                " of ",
                length(input$ml_checkbox_group),
                " (Best model: ",
                best_model,
                " Result: ",
                best_result,
                ")"
              )

            # Check for missing values in the trainSet and print them
            if (any(is.na(trainSet))) {
              print("Missing values in trainSet:")
              print(trainSet[!complete.cases(trainSet),])
            }

            # Check for missing values in the testSet and print them
            if (any(is.na(testSet))) {
              print("Missing values in testSet:")
              print(testSet[!complete.cases(testSet),])
            }

            setProgress(message = lmessage , value = count_model)
            formula <- as.formula(paste(target_name, "~ ."))
            model <-
              train(
                formula,
                data = trainSet,
                method = model_name,
                trControl = ctrl
              )
            # Make predictions
            pred <- predict(model, newdata = testSet)

            if (is.factor(testSet[[target_name]])) {
              accuracy <- sum(pred == testSet[[target_name]]) / length(pred)
              if (accuracy > best_result) {
                best_result <- accuracy
                best_model <- model_name
              }
              model_results <-
                data.frame(Model = model_name,
                  "Accuracy" = accuracy)
              ml_table_results(rbind(ml_table_results(), model_results))
              temp_models_list[[model_name]] <- model
            } else {
              # Evaluate the model
              result_pred <-
                postResample(pred, testSet[[target_name]])

              if (result_pred["Rsquared"] > best_result) {
                best_result <- result_pred["Rsquared"]
                best_model <- model_name
              }
              model_results <-
                data.frame(Model = model_name,
                  "R*R" = result_pred["Rsquared"],
                  "MAE" = result_pred["MAE"])
              ml_table_results(rbind(ml_table_results(), model_results))
              temp_models_list[[model_name]] <- model
            }
          }, error = function(e) {
            print(paste(
              "Error in model",
              model_name,
              ": ",
              conditionMessage(e)
            ))
          })
        }
      }
    )
    all_models_reactive(temp_models_list)
  })

  session$onSessionEnded(function() {
    rm(dff, changed_table, envir = globalenv())
    if (exists("df_pre")) {
      rm(df_pre, envir = globalenv())
    }
  })
  load_dataset_into_table(session)
  load_ml_list()
}

load_ml_list <- function() {
  all_models <- getModelInfo()
  ml_available <- list()
  ml_not_available <- NULL
  for (model_name in names(all_models)) {
    if (any(!all_models[[model_name]]$library %in% installed.packages())) {
      ml_not_available <- c(ml_not_available, model_name)
    } else {
      if (!(model_name %in% slow_models)) {
        ml_available <- c(ml_available, model_name)
      }
    }
  }
  removeUI(selector = paste0("#", "ml_checkbox_group"))
  insertUI(
    selector = "#dynamic_machine_learning",
    where = "afterEnd",
    ui = checkboxGroupInput(
      inputId = "ml_checkbox_group",
      label = NULL,
      choices = ml_available,
      selected = ml_available
    )
  )
  removeUI(selector = paste0("#", "ml_missing_checkbox_group"))
  insertUI(
    selector = "#dynamic_machine_learning_missing",
    where = "afterEnd",
    ui = checkboxGroupInput(
      inputId = "ml_missing_checkbox_group",
      label = NULL,
      choices = ml_not_available
    )
  )
}

load_file_into_table <-
  function(textarea_columns,
    textarea_rows,
    localsession) {
    column_names <- strsplit(textarea_columns, "\n")[[1]]
    rown_names <- strsplit(textarea_rows, "\n")[[1]]
    dff <<- df_pre[rown_names, column_names, drop = FALSE]
    changed_table <<- dff
    load_checkbox_group()
    updateTabsetPanel(localsession, "tabs", selected = "2) TABLE")
    enable("merge_all_columns")
    enable("merge_all_rows")
    showTab(inputId = "tabs", target = "2) TABLE")
    showTab(inputId = "tabs", target = "3) HEATMAP PLOT")
    showTab(inputId = "tabs", target = "4) 2D PLOT")
    showTab(inputId = "tabs", target = "5) MACHINE LEARNING")
  }

load_dataset_into_table <- function(localsession) {
  if (exists("dff") && is.data.frame(dff) && nrow(dff) > 0) {
    column_names <- colnames(dff)
    rown_names <- rownames(dff)
    changed_table <<- dff
    load_checkbox_group()
    updateTabsetPanel(localsession, "tabs", selected = "2) TABLE")
    enable("merge_all_columns")
    enable("merge_all_rows")
    showTab(inputId = "tabs", target = "2) TABLE")
    showTab(inputId = "tabs", target = "3) HEATMAP PLOT")
    showTab(inputId = "tabs", target = "4) 2D PLOT")
    showTab(inputId = "tabs", target = "5) MACHINE LEARNING")
  }
}

generate_annotation_colors <- function(annotation_df) {
  color_list <- list()
  # Loop through each column in the annotation dataframe
  for (colname in names(annotation_df)) {
    unique_vals <- unique(annotation_df[[colname]])
    # Generate colors for the unique values
    colors <- rainbow(length(unique_vals))
    color_list[[colname]] <- setNames(colors, unique_vals)
  }
  return(color_list)
}

load_checkbox_group <- function() {
  removeUI(selector = paste0("#", "column_checkbox_group"))
  removeUI(selector = paste0("#", "row_checkbox_group"))
  removeUI(selector = paste0("#", "checkbox_group_categories"))
  insertUI(
    selector = "#dynamic_columns",
    where = "afterEnd",
    ui = checkboxGroupInput(
      inputId = "column_checkbox_group",
      label = NULL,
      choices = names(dff),
      selected = names(dff)
    )
  )

  insertUI(
    selector = "#dynamic_rows",
    where = "afterEnd",
    ui = checkboxGroupInput(
      inputId = "row_checkbox_group",
      label = NULL,
      choices = rownames(dff),
      selected = rownames(dff)
    )
  )

  insertUI(
    selector = "#dynamic_columns_categories",
    where = "afterEnd",
    ui = checkboxGroupInput(
      inputId = "checkbox_group_categories",
      label = NULL,
      choices = names(dff)
    )
  )
}

#' Opens a window with the application
#'
#' @param dataset A data.frame to be automatically loaded. Leave empty to add later
#'
#' @return NULL when no error
#' @export
#'
#' @examples
#' \donttest{
#'     ugPlot(dataset = mtcars)
#' }
#'

ugPlot <- function(dataset = data.frame()) {
  if (nrow(dataset) > 0) {
    dff <<- dataset
  }
  shinyApp(ui = ui, server = server)
}

shinyApp(ui, server)
