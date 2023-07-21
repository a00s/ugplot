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

library(keras)
library(caret)
# library(rpart)
# library(ada)

#https://ugplot.shinyapps.io/ugPlot/

rsconnect::setAccountInfo(name = 'ugplot',
                          token = 'A384F61E20E58D384A86C5FFB84346BF',
                          secret = '63yYHHCavMf444iAW/rz/I31PMeUD1RPE6/zdARG')
options(shiny.maxRequestSize = 800 * 1024 * 1024)

plotlist2d <- read.csv("2dplotlist.csv", sep = ";", header = TRUE)
plotlist <- read.csv("plotlist.csv", sep = ";", header = TRUE)
palettelist <- read.csv("palette.csv", sep = ";", header = TRUE)

df_pre <- ""
df <- ""
ml_available <- ""
ml_not_available <- ""
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
        width = 6,
        actionButton("uncheck_all_columns", "Uncheck all"),
        actionButton("check_all_columns", "Check all"),
        div(class = "scrollable-table",
            div(id = "dynamic_columns"))
      ),
      column(
        width = 6,
        actionButton("uncheck_all_rows", "Uncheck all"),
        actionButton("check_all_rows", "Check all"),
        div(class = "scrollable-table",
            div(id = "dynamic_rows"))
      ),
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
               mainPanel(plotOutput("plot", height = "800px"))
             )),
    tabPanel("4) 2D PLOT",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar-panel-custom",
                 div(class = "rowplotlist",
                     sliderInput(
                       inputId = "correlation_threshhold",
                       label = "Correlation >=",
                       min = 0,
                       max = 1,
                       value = 0.5,
                       step = 0.1
                     ),
                     actionButton("plotButton", "Plot"),
                     lapply(1:nrow(plotlist2d), function(i) {
                       bname <- paste0("buttonplot2d", i)
                       imgname <- paste0("img/", plotlist$img[i])
                       print(bname)
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
                       bname <- paste0("buttonpalette2d", i)
                       imgname <- paste0("img/", palettelist$img[i])
                       # print(bname)
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
               mainPanel(uiOutput("plots")
                 # plotOutput("plot2d", height = "800px")
              )
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
        numericInput(
          "ml_rfg_trees",
          "Forest Regression - Number of decision trees",
          value = 200,
          min = 1,
          max = 500,
          step = 1
        ),
        actionButton(
          "play_random_forest_regression",
          "PLAY - RANDOM FOREST REGRESSION"
        ),
        actionButton("play_elastic_net_regression",
                     "PLAY - ELASTIC NET REGRESSION"),
        actionButton("play_deep_learning",
                     "PLAY - DEEP LEARNING"),
        actionButton("play_search_best_model",
                     "Search best model"),

        column(
          width = 6,
          actionButton("uncheck_all_ml", "Uncheck all"),
          actionButton("check_all_ml", "Check all"),
          actionButton("play_search_best_model_caret",
                       "RUN"),
          div(class = "scrollable-table",
              div(id = "dynamic_machine_learning"))
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
        # plotOutput("importance_plot"),
        div(style = "width: 100%; overflow-x: auto;",
            DTOutput("ml_table"))
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

  observeEvent(input$process_table_content, {
    column_names <- strsplit(input$textarea_columns, "\n")[[1]]
    rown_names <- strsplit(input$textarea_rows, "\n")[[1]]
    df <<- df_pre[rown_names, column_names, drop = FALSE]
    changed_table <<- as.matrix(df)
    load_checkbox_group()
    updateTabsetPanel(session, "tabs", selected = "2) TABLE")
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

  observeEvent(input$plotButton, {
    print("Cheguei aqui .......")
    output$plots <- renderUI({
      X <- changed_table[input$row_checkbox_group, input$column_checkbox_group]  # all columns except 'age'
      cor_matrix <- cor(X)
      # Get the number of columns in the dataset
      num_cols <- ncol(X)

      # Create an empty list to store the plots
      plots_list <- list()

      # Loop through each column and create a scatter plot with correlations
      for (i in 1:num_cols) {
        # Extract the column name
        col_name <- colnames(X)[i]

        # Loop through all other columns
        for (j in i:num_cols) {
          if (j != i && cor_matrix[i, j] >= input$correlation_threshhold) {
            # Extract the second column name
            col_name2 <- colnames(X)[j]
            # Create the scatter plot
            p <- plot_ly(
              x = X[, col_name],
              y = X[, col_name2],
              type = "scatter",
              mode = "markers",
              marker = list(size = 8)
            )

            # Set the plot title and axis labels using the layout function
            p <- layout(
              p,
              title = paste(col_name, col_name2),
              xaxis = list(title = col_name),
              yaxis = list(title = col_name2)
            )

            # Add the plot to the list
            plots_list[[length(plots_list) + 1]] <- p
          }
        }
      }

      # Return the list of plots to be rendered
      do.call(tagList, plots_list)
    })
  })
  ####################### TAB 4) Plot 2D
  # output$plots <- renderUI({
  #   X <- changed_table[input$row_checkbox_group, input$column_checkbox_group]  # all columns except 'age'
  #   cor_matrix <- cor(X)
  #   # Get the number of columns in the dataset
  #   num_cols <- ncol(X)
  #
  #   # Create an empty list to store the plots
  #   plots_list <- list()
  #
  #   # Loop through each column and create a scatter plot with correlations
  #   for (i in 1:num_cols) {
  #     # Extract the column name
  #     col_name <- colnames(X)[i]
  #
  #     # Loop through all other columns
  #     for (j in i:num_cols) {
  #       if (j != i && cor_matrix[i, j] >= input$correlation_threshhold) {
  #         # Extract the second column name
  #         col_name2 <- colnames(X)[j]
  #         # Create the scatter plot
  #         p <- plot_ly(
  #           x = X[, col_name],
  #           y = X[, col_name2],
  #           type = "scatter",
  #           mode = "markers",
  #           marker = list(size = 8)
  #         )
  #
  #         # Set the plot title and axis labels using the layout function
  #         p <- layout(
  #           p,
  #           title = paste(col_name, col_name2),
  #           xaxis = list(title = col_name),
  #           yaxis = list(title = col_name2)
  #         )
  #
  #         # Add the plot to the list
  #         plots_list[[length(plots_list) + 1]] <- p
  #       }
  #     }
  #   }
  #
  #   # Return the list of plots to be rendered
  #   do.call(tagList, plots_list)
  # })




  # Create a blank canvas for the plots
  # output$plot2d <- renderPlot({
  #   X <- changed_table[input$row_checkbox_group, input$column_checkbox_group]  # all columns except 'age'
  #   cor_matrix <- cor(X)
  #   # Get the number of columns in the dataset
  #   num_cols <- ncol(X)
  #
  #   # Create a list to store the scatter plots
  #   plot_list <- lapply(1:num_cols, function(i) {
  #     # Extract the column name
  #     col_name <- colnames(X)[i]
  #
  #     # Loop through all other columns
  #     plots <- lapply(i:num_cols, function(j) {
  #       if (j != i && cor_matrix[i, j] >= input$correlation_threshhold) {
  #         # Extract the second column name
  #         col_name2 <- colnames(X)[j]
  #         # Create the scatter plot
  #         plot(
  #           X[, col_name],
  #           X[, col_name2],
  #           xlab = col_name,
  #           ylab = col_name2,
  #           main = paste(
  #             col_name,
  #             col_name2
  #           )
  #         )
  #       }
  #     })
  #     # Remove NULL values from the list
  #     plots <- plots[!sapply(plots, is.null)]
  #     # Return the list of plots for each column
  #     plots
  #   })
  #
  #   # Flatten the list of plots into a single list
  #   plot_list <- unlist(plot_list)
  #
  #   # Check if there are any plots to arrange before calling grid.arrange
  #   if (length(plot_list) > 0) {
  #     # Arrange the plots in a grid layout using gridExtra::grid.arrange()
  #     grid.arrange(grobs = plot_list, ncol = 3)  # Adjust ncol as needed
  #   }
  # })


  lapply(1:nrow(plotlist2d), function(i) {
    renderPlot({
      X <- changed_table[input$row_checkbox_group, input$column_checkbox_group]  # all columns except 'age'
      cor_matrix <- cor(X)
      # Get the number of columns in the dataset
      num_cols <- ncol(X)

      # Create a list to store the scatter plots
      plot_list <- lapply(1:num_cols, function(i) {
        # Extract the column name
        col_name <- colnames(X)[i]

        # Loop through all other columns
        plots <- lapply(i:num_cols, function(j) {
          if (j != i && cor_matrix[i, j] >= input$correlation_threshhold) {
            # Extract the second column name
            col_name2 <- colnames(X)[j]
            # Create the scatter plot
            plot(
              X[, col_name],
              X[, col_name2],
              xlab = col_name,
              ylab = col_name2,
              main = paste(
                col_name,
                col_name2
              )
            )
          }
        })
        # Remove NULL values from the list
        plots <- plots[!sapply(plots, is.null)]
        # Return the list of plots for each column
        plots
      })

      # Flatten the list of plots into a single list
      plot_list <- unlist(plot_list)

      # Check if there are any plots to arrange before calling grid.arrange
      if (length(plot_list) > 0) {
        # Arrange the plots in a grid layout using gridExtra::grid.arrange()
        grid.arrange(grobs = plot_list, ncol = 3)  # Adjust ncol as needed
      }
    })
  })

  #
  #
  # lapply(1:nrow(plotlist2d), function(i) {
  #   print("aqui")
  #
  #   bname <- paste0("buttonplot2d", i)
  #   observeEvent(input[[bname]], {
  #     print("Aqui 2")
  #
  #     output$plot2d <- renderPlot({
  #       print("Gerando plot 2d")
  #       X <- changed_table[input$row_checkbox_group, input$column_checkbox_group]  # all columns except 'age'
  #       cor_matrix <- cor(X)
  #       # Get the number of columns in the dataset
  #       num_cols <- ncol(X)
  #       print(paste("Numero de colunas",num_cols))
  #
  #       # Create a blank canvas for the plots
  #       par(mfrow = c(ceiling(sqrt(num_cols)), ceiling(sqrt(num_cols))))
  #
  #       # Loop through each column and create a scatter plot with correlations
  #       for (i in 1:num_cols) {
  #         # Extract the column name
  #         col_name <- colnames(X)[i]
  #
  #         # Loop through all other columns
  #         for (j in i:num_cols) {
  #           if (j != i && cor_matrix[i,j] >= input$correlation_threshhold) {
  #             # Extract the second column name
  #             col_name2 <- colnames(X)[j]
  #             # Plot the two columns against each other
  #             plot(
  #               X[, col_name],
  #               X[, col_name2],
  #               xlab = col_name,
  #               ylab = col_name2,
  #               main = paste(
  #                 col_name,
  #                 col_name2
  #               )
  #             )
  #           }
  #         }
  #       }
  #
  #     })
  #   })
  # })

  ####### 4) Machine learning
  output$ml_table_results = renderDT(
    ml_table_results(),
    options = list(
      lengthChange = FALSE,
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    ),
    rownames = FALSE
  )

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

  # output$ml_table <- renderDT({
  #   my_dataframe
  # })
  # output$importance_plot <- renderPlot({
  # importance_ordered <- importance_table[order(importance_table[, 1], decreasing = TRUE), ]
  # ml_plot_importance
  # Create a bar plot
  # ggplot(ml_plot_importance(), aes(x = Variable, y = Importance)) +
  #   geom_bar(stat = "identity", fill = "steelblue") +
  #   xlab("Variable") +
  #   ylab("Importance") +
  #   ggtitle("Variable Importance")
  # ggplot(ml_plot_importance(), aes(x = Variable, y = Importance))
  # x <- c(1, 2, 3, 4, 5)
  # y <- c(2, 4, 6, 8, 10)
  # plot(x, y, main = "Scatter Plot", xlab = "X", ylab = "Y")
  # print("aqui no plot")
  # print(ml_plot_importance())
  # print(ml_data_table(importance_ordered))
  # dfl <- data.frame(
  #   Sample = rep(c("Sample1", "Sample2", "Sample3"), each = 5),
  #   Value = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10, 3, 6, 9, 12, 15)
  # )
  # ggplot(dfl, aes(x = Value, y = Sample, group = Sample, color = Sample)) +
  #   geom_line() +
  #   xlab("Value") +
  #   ylab("Sample") +
  #   ggtitle("Line Chart")
  #   ggplot(ml_data_table())
  #
  # })

  observeEvent(input$play_random_forest_regression, {
    # ntree_values <- seq(100, 1000, by=100)
    # for (ntree in ntree_values) {
    ml_table_results("")
    target_name <- input$ml_target
    X <-
      changed_table[input$row_checkbox_group, setdiff(input$column_checkbox_group, target_name)]  # all columns except 'age'
    Y <- df[[target_name]]  # 'age' column

    # Run randomForest
    rf_model <-
      randomForest(X,
                   Y,
                   ntree = input$ml_rfg_trees,
                   importance = TRUE)

    # Calculate R-squared
    rsq <- 1 - rf_model$mse[input$ml_rfg_trees] / var(Y)
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
      importance_table[order(importance_table[, 1], decreasing = TRUE),]
    ml_data_table(importance_ordered)
    # print(rownames(importance_ordered))
    # print(ml_data_table()[,1])
    # ggplot(importance_ordered, aes(x = Variable, y = Importance)) +
    #   geom_bar(stat = "identity", fill = "steelblue") +
    #   xlab("Variable") +
    #   ylab("Importance") +
    #   ggtitle("Variable Importance")
    # ggplot(importance_ordered)
    # Print
    # print(head(importance_ordered))

  })
  observeEvent(input$play_elastic_net_regression, {
    ml_table_results("")
    #install.packages("glmnet")
    # Let's assume df is your data frame, and 'age' is the target variable
    #X <-
    #  model.matrix(age ~ ., df)[, -1]  # we remove the intercept column
    target_name <- input$ml_target
    X <-
      changed_table[input$row_checkbox_group, setdiff(input$column_checkbox_group, target_name)]  # all columns except 'age'
    Y <- df[[target_name]]

    # Y <- df$age

    # Elastic net uses a combination of L1 and L2 regularization.
    # alpha=0 is equivalent to Ridge, and alpha=1 is equivalent to Lasso.
    # We'll use 0.5 as a compromise, but this should be tuned using cross-validation.
    alpha <- 0.5

    # Now let's fit the model. Note that glmnet uses its own 'cv.glmnet' function for cross-validation
    set.seed(123)  # for reproducibility
    cv_fit <- cv.glmnet(X, Y, alpha = alpha)

    # The best lambda (regularization parameter) found by cross-validation
    best_lambda <- cv_fit$lambda.min
    print(paste("Best lambda: ", best_lambda))

    # Now we can predict using the best model
    Y_pred <- predict(cv_fit, newx = X, s = best_lambda)

    # And calculate R-squared
    rsq <- 1 - sum((Y - Y_pred) ^ 2) / sum((Y - mean(Y)) ^ 2)
    print(paste("R-squared: ", rsq))
    ml_table_results(rbind(ml_table_results(), data.frame(Title = "R^2", Result = rsq)))

    ################## the most important variables
    # Get the coefficients at the best lambda
    coefs <- coef(cv_fit, s = best_lambda)

    # Convert the coefficients to a regular matrix
    coefs_mat <- as.matrix(coefs)

    # Create a data frame from the matrix
    coefs_df <-
      data.frame(Coefficient = as.vector(coefs_mat),
                 Variable = rownames(coefs_mat))

    # Sort the coefficients in decreasing order of absolute value
    coefs_df <-
      coefs_df[order(abs(coefs_df$Coefficient), decreasing = TRUE),]

    # Print the sorted coefficients along with variable names
    print(coefs_df)
    ml_data_table(coefs_df)
  })

  observeEvent(input$install_missing_modules, {
    # counter_model = 0
    all_models <- getModelInfo()
    for (model_name in names(all_models)) {
      # counter_model <- counter_model + 1
      # if (counter_model > 5) {
      #   next
      # }
      # print(paste("Modelo", model_name))
      # Skip models that require additional packages
      if (any(!all_models[[model_name]]$library %in% installed.packages())) {
        print(paste("Installing", model_name))
        install.packages(model_name)
      }
    }
  })

  # observeEvent(input$temporario, {
  #   all_models <- getModelInfo()
  #   ml_available <<- list()
  #   for (model_name in names(all_models)) {
  #     if (any(!all_models[[model_name]]$library %in% installed.packages())) {
  #       ml_not_available <<- c(ml_not_available, model_name)
  #     } else {
  #       ml_available <<- c(ml_available, model_name)
  #     }
  #   }
  #   removeUI(selector = paste0("#", "ml_checkbox_group"))
  #   insertUI(
  #     selector = "#dynamic_machine_learning",
  #     where = "afterEnd",
  #     ui = checkboxGroupInput(
  #       inputId = "ml_checkbox_group",
  #       label = "ML available:",
  #       choices = ml_available,
  #       selected = ml_available
  #     )
  #   )
  #   removeUI(selector = paste0("#", "ml_missing_checkbox_group"))
  #   insertUI(
  #     selector = "#dynamic_machine_learning_missing",
  #     where = "afterEnd",
  #     ui = checkboxGroupInput(
  #       inputId = "ml_missing_checkbox_group",
  #       label = "ML missing:",
  #       choices = ml_not_available
  #     )
  #   )
  # })

  observeEvent(input$play_search_best_model_caret, {
    print("Searching caret best model")
    ml_table_results("")
    target_name <- input$ml_target
    trainIndex <-
      createDataPartition(df[[target_name]],
                          p = .8,
                          list = FALSE,
                          times = 1)
    trainSet <- df[trainIndex, ]
    testSet  <- df[-trainIndex, ]
    if (!is.data.frame(trainSet)) {
      trainSet <- as.data.frame(trainSet)
    }
    if (!is.data.frame(testSet)) {
      testSet <- as.data.frame(testSet)
    }

    # Get the list of all available models
    # all_models <- getModelInfo()
    all_models <- input$ml_checkbox_group
    # for (model_name in names(all_models)) {
    #   print(paste("Modelo",model_name))
    #   print(all_models[[model_name]]$library)
    #   # Skip models that require additional packages
    #   if (any(!all_models[[model_name]]$library %in% installed.packages())) {
    #     print(paste("Ignoring",model_name))
    #     next
    #   }
    # }
    # Loop through the models
    # counter_model <- 0
    # for (model_name in names(all_models)) {
    for (model_name in all_models) {
      print(model_name)
      # next
      # counter_model <- counter_model + 1
      # if (counter_model > 50) {
      #   # next
      # }
      # print(paste("Modelo", model_name))
      # Skip models that require additional packages
      # if (any(!all_models[[model_name]]$library %in% installed.packages())) {
      # if (any(!model_name$library %in% installed.packages())) {
      #   print(paste("Ignoring", model_name))
      #   next
      # }
      # library(model_name, character.only = TRUE)
      result <- tryCatch({
        library(model_name, character.only = TRUE)
      }, error = function(e) {
        print(paste("Failed to load", model_name))
        return(NULL)
      })

      # If loading was successful, continue with the next iteration
      if (is.null(result)){
        next
      }

      # Train the model
      tryCatch({
        print(paste("Modelo 2", model_name))
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

        # Evaluate the model
        result_pred <- postResample(pred, testSet[[target_name]])
        model_results <-
          data.frame(Model = model_name,
                     "R^2" = result_pred["Rsquared"],
                     MAE = result_pred["MAE"])

        ml_table_results(rbind(ml_table_results(), model_results))

        # Print the results
        # print(paste("Results for", model_name))
        # print(result_pred["Rsquared"])
        # print(result_pred["MAE"])
        # Code to handle the successful model training here

      }, error = function(e) {
        # print(paste("Error in model", model_name))
        # Code to handle the error here (e.g., print an error message)
        print(paste("Error in model", model_name, ": ", conditionMessage(e)))
      })
    }



  })

  observeEvent(input$play_search_best_model, {
    withProgress(message = 'Searching the best model...', value = 0, {
      print("Search best model")
      invalidateLater(100, session)
      ml_table_results("")
      target_name <- input$ml_target
      # X <- changed_table[input$row_checkbox_group, setdiff(input$column_checkbox_group, target_name)]  # all columns except 'age'
      # Y <- df[[target_name]]
      # Combine X and Y into a single data frame
      # df2 <- cbind(X, Y)
      # print("A 2")
      # print(head(df2))
      # Prepare the data
      # Split data into training and testing sets
      trainIndex <-
        createDataPartition(df[[target_name]],
                            p = .8,
                            list = FALSE,
                            times = 1)
      trainSet <- df[trainIndex, ]
      testSet  <- df[-trainIndex, ]
      # Setup cross-validation method
      ctrl <- trainControl(method = "cv", number = 10)

      # Linear Regression (lm)
      # Generalized Linear Models (glm)
      ###### Logistic Regression (glm with family = "binomial") nao pode ser usado pois espera 0 e 1
      # Poisson Regression (glm with family = "poisson")
      # Negative Binomial Regression (glm with family = "negative.binomial")
      ###### Cox Proportional Hazards Model (coxph) é usado para sobrevivencia, entao considera tempo
      ###### Generalized Estimating Equations (gee) ### tentar depois em detalhes
      # Tobit Regression (censReg)
      # Quantile Regression (quantreg)
      # Decision Trees (rpart)
      # Random Forest (rf)
      # Gradient Boosting (gbm or xgboost)
      # Support Vector Machines (svm)
      # Elastic Net (glmnet)
      # Bayesian Regression (bayeslm or brms)
      # Neural Networks (nnet or keras)
      # Generalized Additive Models (gam)
      # Multivariate Adaptive Regression Splines (mars)
      # Generalized Linear Mixed Models (lme4 or glmmTMB)
      # Survival Analysis (survival)
      # Bayesian Additive Regression Trees (bartMachine)
      # Partial Least Squares Regression (pls)
      # Lasso Regression (glmnet with alpha = 1)
      # Ridge Regression (glmnet with alpha = 0)
      # List of models
      # models <- c("lm", "glm", "coxph", "gee","censReg","quantreg","rpart","rf","gbm","xgboost","svm", "glmnet", "bayeslm","brms","nnet","keras","gam","mars","lme4","glmmTMB","survival","bartMachine","pls")
      models <-
        c("lm", "rpart", "rf", "glmnet", "glm_poisson", "glm.nb")
      #https://rdrr.io/cran/caret/man/models.html
      ##adaboost
      ##AdaBoost.M1
      #amdai
      #models <- c("amdai")
      # Initialize a list to store the results
      res_list <- list()
      # Set a seed for reproducibility
      set.seed(123)
      print(head(trainSet))
      # Loop over models
      formula <- as.formula(paste(target_name, "~ ."))
      counter <- 1
      for (i in models) {
        setProgress(message = paste('Fitting model', i, ': ', counter, 'of', length(models)))
        counter <- counter + 1
        if (i == "glm_poisson") {
          fit <-
            train(
              formula,
              data = trainSet,
              method = "glm",
              trControl = ctrl,
              family = "poisson"
            )
        } else {
          fit <- train(
            formula,
            data = trainSet,
            method = i,
            trControl = ctrl
          )
        }
        # res_list[[i]] <- fit
        pred <- predict(fit, newdata = testSet)
        print(paste("Results for", i))
        # RMSE tends to penalize larger errors more than MAE
        result_pred <- postResample(pred, testSet[[target_name]])
        print(result_pred["Rsquared"])
        print(result_pred["MAE"])
        print("----------------------------------------")
        model_results <-
          data.frame(Model = i,
                     "R^2" = result_pred["Rsquared"],
                     MAE = result_pred["MAE"])

        ml_table_results(rbind(ml_table_results(), model_results))
        # Pause for a moment to allow the table to be rendered
        Sys.sleep(0.5)
        # print(postResample(pred, testSet[[target_name]]))
      }
      # Compare the results
      # results <- resamples(res_list)
      # summary(results)

      # Predict and evaluate on testSet
      # for(i in models){
      #   pred <- predict(res_list[[i]], newdata = testSet)
      #   print(paste("Results for", i))
      #   print(postResample(pred, testSet[[target_name]]))
      # }
    })

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

  # output$correlation_threshhold <- renderText({
  #   threshold <- input$correlation_threshhold
  #   paste("Threshold value:", threshold)
  # })

  load_ml_list()
}

load_ml_list <- function() {
  all_models <- getModelInfo()
  ml_available <<- list()
  for (model_name in names(all_models)) {
    if (any(!all_models[[model_name]]$library %in% installed.packages())) {
      ml_not_available <<- c(ml_not_available, model_name)
    } else {
      ml_available <<- c(ml_available, model_name)
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

load_checkbox_group <- function() {
  removeUI(selector = paste0("#", "column_checkbox_group"))
  removeUI(selector = paste0("#", "row_checkbox_group"))
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
}

shinyApp(ui, server)
