library(shiny)
library(shinyWidgets)
library(ggplot2)
library(heatmap3)
library(DT)
library(gplots)
library(viridis)
library(RColorBrewer)
library(rsconnect)
library(dendextend)
library(pheatmap)
library(randomForest)
library(glmnet)
library(plotly)
library(tidyr)
library(keras)
library(caret)

#Pending bugs
# Warning: Error in randomForest.default: Can not handle categorical predictors with more than 53 categories.

#https://ugplot.shinyapps.io/ugPlot/

rsconnect::setAccountInfo(name = 'ugplot',
                          token = 'A384F61E20E58D384A86C5FFB84346BF',
                          secret = '63yYHHCavMf444iAW/rz/I31PMeUD1RPE6/zdARG')
options(shiny.maxRequestSize = 800 * 1024 * 1024)

plotlist2d <- read.csv("2dplotlist.csv", sep = ";", header = TRUE)
plotlist <- read.csv("plotlist.csv", sep = ";", header = TRUE)
palettelist <- read.csv("palette.csv", sep = ";", header = TRUE)

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

df_pre <- ""
df <- ""
ml_available <- ""
ml_not_available <- NULL
ui <- fluidPage(
  includeCSS("www/styles.css"),
  titlePanel(tags$img(src = "ugplot.png", height = "50px"), "ugPlot"),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "1) LOAD DATA",
      tags$div(
        style = "display: inline-block; vertical-align: top;",
        numericInput(
          "startfromline",
          "Start from line:",
          value = 1,
          min = 1,
          step = 1
        )
      ),
      tags$div(
        style = "display: inline-block; vertical-align: top;",
        selectInput(
          inputId = "separator",
          label = "CSV separator:",
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
      tags$div(
        style = "display: inline-block; vertical-align: top;",
        actionButton("process_table_content", "GO TO STEP 2 (TABLE)")
      ),
      column(
        width = 6,
        actionButton("add_all_columns", "Add all"),
        actionButton("remove_all_columns", "Remove all"),
        actionButton("merge_all_columns", "Merge tables"),
        textAreaInput(
          "textarea_columns",
          label = "One line per column",
          rows = 20,
          cols = 50
        )
      ),
      column(
        width = 6,
        actionButton("add_all_rows", "Add all"),
        actionButton("remove_all_rows", "Remove all"),
        actionButton("merge_all_rows", "Merge tables"),
        textAreaInput(
          "textarea_rows",
          label = "One line per column",
          rows = 20,
          cols = 50
        )
      ),
    ),
    tabPanel(
      "2) TABLE",
      actionButton("transpose_table", "Transpose table"),
      div(style = "width: 100%; overflow-x: auto;",
          DTOutput("contents")),
      column(
        width = 4,
        actionButton("uncheck_all_columns", "Uncheck all"),
        actionButton("check_all_columns", "Check all"),
        div(class = "scrollable-table",
            div(id = "dynamic_columns"))
      ),
      column(
        width = 4,
        actionButton("uncheck_all_rows", "Uncheck all"),
        actionButton("check_all_rows", "Check all"),
        div(class = "scrollable-table",
            div(id = "dynamic_rows"))
      ),
      column(
        width = 4,
        div(class = "scrollable-table",
        style="background-color: #FFFFD8;",
        label = "Categories",
        div(id = "dynamic_columns_categories"))
      )
    ),

    tabPanel("3) HEATMAP PLOT",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar-panel-custom",
                 selectInput(
                   inputId = "plot_xy",
                   label = "Data to use:",
                   choices = c("LINES x COLUMNS", "COLUMNS x COLUMNS")
                 ),
                 div(class = "rowplotlist",
                     lapply(1:nrow(plotlist), function(i) {
                       bname <- paste0("buttonplot", i)
                       imgname <- paste0("img/", plotlist$img[i])
                       fluidRow(
                         tags$img(
                           src = imgname,
                           width = 130,
                           height = 130
                         ),
                         actionButton(bname, plotlist$name[i])
                       )
                     })),
                 div(class = "rowpalettelist",
                     lapply(1:nrow(palettelist), function(i) {
                       bname <- paste0("buttonpalette", i)
                       imgname <- paste0("img/", palettelist$img[i])
                       if (imgname != "img/NA") {
                         fluidRow(
                           tags$img(
                             src = imgname,
                             width = 130,
                             height = 20
                           ),
                           actionButton(bname, palettelist$name[i])
                         )
                       }
                     }))
               ),
               mainPanel(
                 plotOutput("plot", height = "800px")
                 )
             )),
    tabPanel("4) 2D PLOT",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar-panel-custom",
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
                     print(bname)
                     fluidRow(
                       tags$img(
                         src = imgname,
                         width = 130,
                         height = 130
                       ),
                       actionButton(bname, plotlist2d$name[i])
                     )
                   })
                 ),
                 # div(class = "rowpalettelist",
                 #     lapply(1:nrow(palettelist), function(i) {
                 #       bname <- paste0("buttonpalette2d", i)
                 #       imgname <- paste0("img/", palettelist$img[i])
                 #       if (imgname != "img/NA") {
                 #         fluidRow(
                 #           tags$img(
                 #             src = imgname,
                 #             width = 130,
                 #             height = 20
                 #           ),
                 #           actionButton(bname, palettelist$name[i])
                 #         )
                 #       }
                 #     }))
               ),
               mainPanel(uiOutput("plots"))
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
            actionButton(
              "play_random_forest_regression",
              "PLAY - RANDOM FOREST REGRESSION"
            ),
            # actionButton("play_elastic_net_regression",
            #              "PLAY - ELASTIC NET REGRESSION"),
            actionButton("play_deep_learning",
                         "PLAY - DEEP LEARNING"),
            verbatimTextOutput("console_output"),

            column(
              width = 6,
              actionButton("uncheck_all_ml", "Uncheck all"),
              actionButton("check_all_ml", "Check all"),
              actionButton("play_search_best_model_caret",
                           "RUN"),
              div(class = "scrollable-table",
                  div(id = "dynamic_machine_learning")),
              tags$br(),
              tags$p(slow_models_text, style = "color: gray; font-size: 11px;")
            ),
            column(
              width = 6,
              actionButton("uncheck_all_ml_missing", "Uncheck all"),
              actionButton("check_all_ml_missing", "Check all"),
              actionButton("install_missing_modules",
                           "Install libraries"),
              div(class = "scrollable-table",
                  div(id = "dynamic_machine_learning_missing"))
            ),
            div(style = "width: 100%; overflow-x: auto;",
                DTOutput("ml_table_results")),
            verbatimTextOutput("ml_row_details"),
            div(style = "width: 100%; overflow-x: auto;",
                DTOutput("ml_table"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  ml_data_table <- reactiveVal()
  ml_table_results <- reactiveVal()
  ml_plot_importance <- reactiveVal()
  changed_table <<- ""
  numeric_table <<- ""
  changed_palette <<- 0
  defaultpalette <<-
    reactiveVal(colorRampPalette(c("red", "yellow", "green"))(256))
  transpose_table2 <<- reactiveVal(0)
  refresh_counter <<- reactiveVal(0)
  tab_separator <<- reactiveVal(",")
  file_click_count <<- reactiveVal(0)
  last_file_click_count <<- 0
  num_rows <- reactiveVal(0)
  num_cols <- reactiveVal(0)

  ####################### TAB 1) LOAD DATA
  observeEvent(input$file1, {
    print("Loading 1")
    file_click_count(file_click_count() + 1)
    filepath <- req(input$file1$datapath)
    skipline = input$startfromline - 1
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
  })

  output$contents <- renderDT({
    if (last_file_click_count == 0 |
        (last_file_click_count != file_click_count())) {
      print("This part to deal when a second file got read")
    }
    current_target_selection <- input$ml_target
    updateSelectInput(session,
                      "ml_target",
                      choices = names(df),
                      selected = current_target_selection)
    print("Vendo se changed table ja tem algo")
    print(head(changed_table))
    print("Vendo se changed table ja tem algo B")
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
    new_df <- as.data.frame(t(df_pre[rown_names, column_names, drop = FALSE]))
    common_rownames <- intersect(rownames(df), rownames(new_df))
    df[common_rownames, names(new_df)] <<- new_df[common_rownames, ]
    print(" ----------- > aqui 1")
    changed_table <<- as.matrix(df)
    load_checkbox_group()
    updateTabsetPanel(session, "tabs", selected = "2) TABLE")
  })

  observeEvent(input$merge_all_rows, {
    column_names <- strsplit(input$textarea_columns, "\n")[[1]]
    rown_names <- strsplit(input$textarea_rows, "\n")[[1]]
    new_df <- df_pre[rown_names, column_names, drop = FALSE]
    common_rownames <- intersect(rownames(df), rownames(new_df))
    df[common_rownames, names(new_df)] <<- new_df[common_rownames, ]
    print(" ----------- > aqui 2")
    changed_table <<- as.matrix(df)
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
        comandtorun <- plotlist$code[i]
        print("looking for how data is used")
        print(input$plot_xy)
        numeric_table <<-
          apply(changed_table[input$row_checkbox_group, input$column_checkbox_group], c(1, 2), as.numeric)

        if (input$plot_xy == "LINES x COLUMNS") {

        } else if (input$plot_xy == "COLUMNS x COLUMNS") {
          numeric_table <<- cor(numeric_table)
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
        print(comandtorun)
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
                             selected = names(df))
  })
  observeEvent(input$uncheck_all_rows, {
    updateCheckboxGroupInput(session,
                             inputId = "row_checkbox_group",
                             selected = character(0))
  })
  observeEvent(input$check_all_rows, {
    updateCheckboxGroupInput(session,
                             inputId = "row_checkbox_group",
                             selected = rownames(df))
  })
  observeEvent(input$transpose_table, {
    if (transpose_table2() == 0) {
      transpose_table2(1)
    } else {
      transpose_table2(0)
    }
    df <<- data.frame(t(as.matrix(df)))
    changed_table <<- as.matrix(df)
    load_checkbox_group()
  })

  ####################### TAB 4) Plot 2D
  lapply(1:nrow(plotlist2d), function(i) {
    bname <- paste0("buttonplot2d", i)
    observeEvent(input[[bname]], {
      output$plots <- renderUI({
        comandtorun <- plotlist2d$code[i]
        X <-
          changed_table[input$row_checkbox_group, input$column_checkbox_group]  # all columns except 'age'
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
        do.call(tagList, plots_list)
      })
    })
  })

  ####### 4) Machine learning
  all_models_reactive <- reactiveVal(list())
  output$ml_table_results = renderDT({
    datatable(
      ml_table_results(),
      selection = "single",
        options = list(
          lengthChange = FALSE,
          paging = FALSE,
          searching = FALSE,
          info = FALSE
        ),
        rownames = FALSE)
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

  output$ml_table = renderDT(ml_data_table(), options = list(lengthChange = FALSE))
  observeEvent(input$play_random_forest_regression, {
    ml_table_results("")
    target_name <- input$ml_target
    X <- changed_table[input$row_checkbox_group, setdiff(input$column_checkbox_group, target_name)]  # all columns except 'age'
    Y <- df[[target_name]]  # 'age' column

    cols_to_convert <- input$checkbox_group_categories
    X <- as.data.frame(X)
    if(length(cols_to_convert) > 0){
      for(this_target in cols_to_convert){
        X[[this_target]] <- as.factor(X[[this_target]])
      }
    }

    # Run randomForest
    rf_model <-
      randomForest(X,
                   Y,
                   ntree = 500,
                   importance = TRUE)

    # Calculate R-squared
    rsq <- 1 - rf_model$mse[500] / var(Y)
    # result <- data.frame(Title = "R^2", Result = rsq)
    ml_table_results(rbind(ml_table_results(), data.frame(Title = "R^2", Result = rsq)))

    ################# variance #################
    # Predict values
    Y_pred <- predict(rf_model, newdata = X)

    # Calculate residuals
    residuals <- Y - Y_pred
    ml_table_results(rbind(
      ml_table_results(),
      data.frame(Title = "Variance of residuals (Y - Y_pred)", Result = var(residuals))
    ))
    ###########################################
    # Get variable importance
    importance_table <- importance(rf_model)

    # Order by importance
    importance_ordered <-
      importance_table[order(importance_table[, 1], decreasing = TRUE), ]
    ml_data_table(importance_ordered)
  })

  # observeEvent(input$play_elastic_net_regression, {
  #   ml_table_results("")
  #   target_name <- input$ml_target
  #   X <- changed_table[input$row_checkbox_group, setdiff(input$column_checkbox_group, target_name)]  # all columns except 'age'
  #   Y <- df[[target_name]]
  #
  #   cols_to_convert <- input$checkbox_group_categories
  #   # X <- as.data.frame(X)
  #   if(length(cols_to_convert) > 0){
  #     for(this_target in cols_to_convert){
  #       X[[this_target]] <- as.factor(X[[this_target]])
  #     }
  #   }
  #   # X <- model.matrix(~ . - 1, data = X)
  #
  #
  #   # Y <- df$age
  #
  #   # Elastic net uses a combination of L1 and L2 regularization.
  #   # alpha=0 is equivalent to Ridge, and alpha=1 is equivalent to Lasso.
  #   # We'll use 0.5 as a compromise, but this should be tuned using cross-validation.
  #   alpha <- 0.5
  #
  #   # Now let's fit the model. Note that glmnet uses its own 'cv.glmnet' function for cross-validation
  #   set.seed(123)  # for reproducibility
  #   cv_fit <- cv.glmnet(X, Y, alpha = alpha)
  #
  #   # The best lambda (regularization parameter) found by cross-validation
  #   best_lambda <- cv_fit$lambda.min
  #   print(paste("Best lambda: ", best_lambda))
  #
  #   # Now we can predict using the best model
  #   Y_pred <- predict(cv_fit, newx = X, s = best_lambda)
  #
  #   # And calculate R-squared
  #   rsq <- 1 - sum((Y - Y_pred) ^ 2) / sum((Y - mean(Y)) ^ 2)
  #   print(paste("R-squared: ", rsq))
  #   ml_table_results(rbind(ml_table_results(), data.frame(Title = "R^2", Result = rsq)))
  #
  #   ################## the most important variables
  #   # Get the coefficients at the best lambda
  #   coefs <- coef(cv_fit, s = best_lambda)
  #
  #   # Convert the coefficients to a regular matrix
  #   coefs_mat <- as.matrix(coefs)
  #
  #   # Create a data frame from the matrix
  #   coefs_df <-
  #     data.frame(Coefficient = as.vector(coefs_mat),
  #                Variable = rownames(coefs_mat))
  #
  #   # Sort the coefficients in decreasing order of absolute value
  #   coefs_df <-
  #     coefs_df[order(abs(coefs_df$Coefficient), decreasing = TRUE), ]
  #
  #   # Print the sorted coefficients along with variable names
  #   print(coefs_df)
  #   ml_data_table(coefs_df)
  # })

  observeEvent(input$install_missing_modules, {
    all_models <- getModelInfo()
    models_to_install <- input$ml_missing_checkbox_group
    for (model_name in models_to_install) {
      print(model_name)
      model_info <-
        getModelInfo(model_name, regex = FALSE)[[model_name]]
      model_libraries <- model_info$library
      for (librarytoinst in model_libraries) {
        print(librarytoinst)
        if (!(librarytoinst %in% installed.packages())) {
          install.packages(librarytoinst)
        } else {
          print("Biblioteca ja estava instalada")
        }
      }
    }
  })

  observeEvent(input$play_search_best_model_caret, {
    print("Searching caret best model")
    temp_models_list <- list()
    withProgress(message = 'Searching the best model...', min = 1, max = length(input$ml_checkbox_group), value = 0, {
      best_r2 <- 0.00
      best_model <- ""
      target_name <- input$ml_target
      # X <- changed_table[input$row_checkbox_group, setdiff(input$column_checkbox_group, target_name)]
      X <- changed_table[input$row_checkbox_group, input$column_checkbox_group]
      Y <- df[[target_name]]
      print(class(X))
      print(head(X))
      #return()

      cols_to_convert <- input$checkbox_group_categories
      # X <- as.data.frame(X)
      if(length(cols_to_convert) > 0){
        for(this_target in cols_to_convert){
          X[[this_target]] <- as.factor(X[[this_target]])
        }
      }

      # X <- model.matrix(~ . - 1, data = X)

      ml_table_results("")
      # #
      trainIndex <- createDataPartition(Y,
                                        p = .8,
                                        list = FALSE,
                                        times = 1)
      trainSet <- X[trainIndex,]
      testSet  <- X[-trainIndex,]

      #cols_to_convert <- input$checkbox_group_categories
      # print("Colunas a converter A")
      #print(cols_to_convert)
      # print("Colunas a converter B")
      #trainSet[cols_to_convert] <- lapply(trainSet[cols_to_convert], as.factor)
      #testSet[cols_to_convert] <- lapply(testSet[cols_to_convert], as.factor)


      if (!is.data.frame(trainSet)) {
        trainSet <- as.data.frame(trainSet)
      }
      if (!is.data.frame(testSet)) {
        testSet <- as.data.frame(testSet)
      }
      print(trainSet)
      print("-------------------")
      print(testSet)

      # Get the list of all available models
      all_models <- input$ml_checkbox_group
      count_model <- 0;
      for (model_name in all_models) {
        count_model <- count_model + 1
        print(model_name)
        result <- tryCatch({
          model_info <- getModelInfo(model_name, regex = FALSE)[[model_name]]
          model_libraries <- model_info$library
          print("Carregando library:")
          print(model_libraries)
          for (lib in model_libraries) {
            print(lib)
            library(lib, character.only = TRUE)
          }
        }, error = function(e) {
          print(paste("Failed to load", model_name))
        })

        ctrl <- trainControl(method = "cv", number = 10)

        # Train the model
        tryCatch({
          lmessage <- paste('Fitting model', model_name, ". ",count_model," of ",length(input$ml_checkbox_group), " (Best model: ",best_model," R^2: ",best_r2,")")
          setProgress(message = lmessage , value = count_model)
          formula <- as.formula(paste(target_name, "~ ."))
          model <-
            train(
              formula,
              data = trainSet,
              method = model_name,
              trControl = ctrl
            )
          # trControl = ctrl
          # Make predictions
          pred <- predict(model, newdata = testSet)

          # Evaluate the model
          result_pred <- postResample(pred, testSet[[target_name]])

          if(result_pred["Rsquared"] > best_r2){
            print("found a best model")
            best_r2 <- result_pred["Rsquared"]
            best_model <- model_name
          }
          model_results <-
            data.frame(Model = model_name,
                       "R2" = result_pred["Rsquared"],
                       "MAE" = result_pred["MAE"])
          ml_table_results(rbind(ml_table_results(), model_results))
          temp_models_list[[model_name]] <- model
        }, error = function(e) {
          # Code to handle the error here (e.g., print an error message)
          print(paste(
            "Error in model",
            model_name,
            ": ",
            conditionMessage(e)
          ))
        })
      }
    })
    all_models_reactive(temp_models_list)
  })

  output$ml_row_details <- renderPrint({
    selected_row <- input$ml_table_results_rows_selected
    if (length(selected_row) == 0) {
      return("")
    }
    selected_data <- ml_table_results()[selected_row, ]
    selected_model_name <- ml_table_results()[selected_row, ]$Model

    # Do something with the selected model name
    print(paste("Selected Model: ", selected_model_name))

    specific_model <- all_models_reactive()[[selected_model_name]]

    # # Complete summary of the model
    # print("=== Summary of the Model ===")
    # print(summary(specific_model))
    #
    # # Best model parameters
    # print("=== Best Tuning Parameters ===")
    # print(specific_model$bestTune)
    #
    # # Resampling results (if available)
    # if (!is.null(specific_model$resample)) {
    #   print("=== Resampling Results ===")
    #   print(specific_model$resample)
    # }

    # Variable importance (for models that support it)
    tryCatch({
      importance <- varImp(specific_model)
      print("=== Variable Importance ===")
      print(importance)
    }, error = function(e) {
      print("Variable importance not supported for this model.")
    })

    # Confusion Matrix and related metrics (for classification models)
    # # Assuming 'predictions' and 'actual_values' are available
    # if ("confusionMatrix" %in% rownames(specific_model$results)) {
    #   predictions <- predict(specific_model, newdata = your_test_data)
    #   actual_values <- your_test_data$Your_Target_Column
    #   confusion <- confusionMatrix(predictions, actual_values)
    #   print("=== Confusion Matrix ===")
    #   print(confusion)
    #   print("=== Classification Metrics ===")
    #   print(confusion$overall)
    # }
    #
    # # Regression metrics like RMSE, R^2 (for regression models)
    # # Assuming 'predictions' and 'actual_values' are available
    # if ("RMSE" %in% rownames(specific_model$results)) {
    #   predictions <- predict(specific_model, newdata = your_test_data)
    #   actual_values <- your_test_data$Your_Target_Column
    #   print("=== Regression Metrics ===")
    #   print(postResample(predictions, actual_values))
    # }

  })

  observeEvent(input$play_deep_learning, {
    print("playing deep learning")
    target_name <- input$ml_target
    X <-
      changed_table[input$row_checkbox_group, setdiff(input$column_checkbox_group, target_name)]  # all columns except 'age'
    y <- df[[target_name]]


    # Example data
    # Assuming you have your data stored in 'X' (input features) and 'y' (target variable)

    # Define the model using a function
    create_model <- function(units, dropout_rate) {
      model <- keras_model_sequential()
      model %>%
        layer_dense(
          units = units,
          activation = 'relu',
          input_shape = dim(X)[2]
        ) %>%
        layer_dropout(rate = dropout_rate) %>%
        layer_dense(units = 1, activation = 'linear')
    }

    # Define the hyperparameters to tune
    units <- c(32, 64, 128)
    dropout_rates <- c(0.2, 0.4, 0.6)

    best_model <- NULL
    best_rmse <- Inf

    # Hyperparameter tuning loop
    for (unit in units) {
      for (dropout_rate in dropout_rates) {
        # Create and compile the model
        model <-
          create_model(units = unit, dropout_rate = dropout_rate)
        model %>% compile(loss = 'mean_squared_error', optimizer = 'adam')

        # Train the model
        model %>% fit(
          X,
          y,
          epochs = 50,
          batch_size = 32,
          verbose = 0
        )

        # Evaluate the model
        predictions <- model %>% predict(X)
        rmse <- sqrt(mean((predictions - y) ^ 2))

        # Calculate R-squared
        y_mean <- mean(y)
        r_squared <-
          1 - sum((y - predictions) ^ 2) / sum((y - y_mean) ^ 2)

        # Update best model if RMSE is lower
        if (rmse < best_rmse) {
          best_model <- model
          best_rmse <- rmse
          best_r_squared <- r_squared
        }
      }
    }

    # Print the best model's summary
    # summary(best_model)

    # Print the best model's R-squared
    print(paste("R^2)  ", best_r_squared))
    ml_table_results("")
    ml_table_results(rbind(
      ml_table_results(),
      data.frame(Title = "R^2", Result = best_r_squared)
    ))
  })

  load_ml_list()
}

load_ml_list <- function() {
  all_models <- getModelInfo()
  ml_available <<- list()
  for (model_name in names(all_models)) {
    if (any(!all_models[[model_name]]$library %in% installed.packages())) {
      ml_not_available <<- c(ml_not_available, model_name)
    } else {
      if (!(model_name %in% slow_models)) {
        ml_available <<- c(ml_available, model_name)
      }
    }
  }
  removeUI(selector = paste0("#", "ml_checkbox_group"))
  insertUI(
    selector = "#dynamic_machine_learning",
    where = "afterEnd",
    ui = checkboxGroupInput(
      inputId = "ml_checkbox_group",
      label = "ML available:",
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
      label = "ML missing:",
      choices = ml_not_available
    )
  )
}

load_file_into_table <- function(textarea_columns, textarea_rows, localsession) {
  column_names <- strsplit(textarea_columns, "\n")[[1]]
  rown_names <- strsplit(textarea_rows, "\n")[[1]]
  df <<- df_pre[rown_names, column_names, drop = FALSE]
  # changed_table <<- as.matrix(df)
  changed_table <<- df
  load_checkbox_group()
  updateTabsetPanel(localsession, "tabs", selected = "2) TABLE")
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
      label = "Columns:",
      choices = names(df),
      selected = names(df)
    )
  )

  insertUI(
    selector = "#dynamic_rows",
    where = "afterEnd",
    ui = checkboxGroupInput(
      inputId = "row_checkbox_group",
      label = "Rows:",
      choices = rownames(df),
      selected = rownames(df)
    )
  )

   insertUI(
    selector = "#dynamic_columns_categories",
    where = "afterEnd",
    ui = checkboxGroupInput(
      inputId = "checkbox_group_categories",
      label = "Categories:",
      choices = names(df)
    )
  )


}

shinyApp(ui, server)
