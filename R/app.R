library(shiny)
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
#https://ugplot.shinyapps.io/ugPlot/

rsconnect::setAccountInfo(name = 'ugplot',
                          token = 'A384F61E20E58D384A86C5FFB84346BF',
                          secret = '63yYHHCavMf444iAW/rz/I31PMeUD1RPE6/zdARG')
options(shiny.maxRequestSize = 800 * 1024 * 1024)


plotlist <- read.csv("plotlist.csv", sep = ";", header = TRUE)
palettelist <- read.csv("palette.csv", sep = ";", header = TRUE)

df_pre <- ""
df <- ""
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

    tabPanel("3) PLOT",
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
                       print(imgname)
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
                       print(bname)
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
    tabPanel(
      "4) MACHINE LEARNING",
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
        actionButton(
          "play_elastic_net_regression",
          "PLAY - ELASTIC NET REGRESSION"
        )
      )
    )
  )
)

server <- function(input, output, session) {
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

      # filepath <- req(input$file1$datapath)
      # filepath <- "sample_dataset.txt" ### temporary to speed up
      # df <<- read.table(filepath, header = TRUE, sep = tab_separator(), row.names=1, dec=".", stringsAsFactors=FALSE, strip.white = TRUE)
      # changed_palette <<- 0
      # changed_table <<- as.matrix(df)
      # last_file_click_count <<- file_click_count()
      # load_checkbox_group()
      # print("Aqui 3")
      # print(head(df))
    }
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
    print("Go to the next tab")
    column_names <- strsplit(input$textarea_columns, "\n")[[1]]
    rown_names <- strsplit(input$textarea_rows, "\n")[[1]]

    # updateSelectInput(session, "ml_target", choices = strsplit(input$textarea_columns, "\n")[[1]])
    # update_ml_target()
    updateSelectInput(session, "ml_target", choices = column_names)

    # print(column_names)
    df <<- df_pre[rown_names, column_names, drop = FALSE]
    # changed_table <<- df_pre[rown_names, column_names, drop=FALSE]
    changed_table <<- as.matrix(df)
    # changed_table <<- as.matrix(df)
    load_checkbox_group()
    updateTabsetPanel(session, "tabs", selected = "2) TABLE")
  })

  observeEvent(input$separator, {
    tab_separator(input$separator)
  })

  ####################### TAB 2) PLOT

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
  ####### 4) Machine learning
  observeEvent(input$play_random_forest_regression, {
    # ntree_values <- seq(100, 1000, by=100)
    # for (ntree in ntree_values) {
    # df3 <- changed_table[input$row_checkbox_group, input$column_checkbox_group]

    target_name <- input$ml_target
    X <- df[, -which(names(df) %in% target_name)]  # all columns except 'age'
    Y <- df[[target_name]]  # 'age' column

    # Run randomForest
    rf_model <- randomForest(X, Y, ntree=input$ml_rfg_trees, importance=TRUE)

    # Calculate R-squared
    rsq <- 1 - rf_model$mse[input$ml_rfg_trees] / var(Y)
    print(rsq)

        ################# variance #################
    # Predict values
    Y_pred <- predict(rf_model, newdata = X)

    # Calculate residuals
    residuals <- Y - Y_pred

    # Calculate variance of residuals
    residual_variance <- var(residuals)
    print(residual_variance)
    ###########################################
    # Get variable importance
    importance_table <- importance(rf_model)

    # Order by importance
    importance_ordered <- importance_table[order(importance_table[,1], decreasing=TRUE),]

    # Print
    print(head(importance_ordered))

  })
  observeEvent(input$play_elastic_net_regression, {
    #install.packages("glmnet")
    library(glmnet)

    # Let's assume df is your data frame, and 'age' is the target variable
    X <- model.matrix(age ~ ., df)[,-1]  # we remove the intercept column
    Y <- df$age

    # Elastic net uses a combination of L1 and L2 regularization.
    # alpha=0 is equivalent to Ridge, and alpha=1 is equivalent to Lasso.
    # We'll use 0.5 as a compromise, but this should be tuned using cross-validation.
    alpha <- 0.5

    # Now let's fit the model. Note that glmnet uses its own 'cv.glmnet' function for cross-validation
    set.seed(123)  # for reproducibility
    cv_fit <- cv.glmnet(X, Y, alpha=alpha)

    # The best lambda (regularization parameter) found by cross-validation
    best_lambda <- cv_fit$lambda.min
    print(paste("Best lambda: ", best_lambda))

    # Now we can predict using the best model
    Y_pred <- predict(cv_fit, newx=X, s=best_lambda)

    # And calculate R-squared
    rsq <- 1 - sum((Y - Y_pred)^2) / sum((Y - mean(Y))^2)
    print(paste("R-squared: ", rsq))

    ################## the most important variables
    # Get the coefficients at the best lambda
    coefs <- coef(cv_fit, s=best_lambda)

    # Convert the coefficients to a regular matrix
    coefs_mat <- as.matrix(coefs)

    # Create a data frame from the matrix
    coefs_df <- data.frame(Coefficient = as.vector(coefs_mat), Variable = rownames(coefs_mat))

    # Sort the coefficients in decreasing order of absolute value
    coefs_df <- coefs_df[order(abs(coefs_df$Coefficient), decreasing = TRUE),]

    # Print the sorted coefficients along with variable names
    print(coefs_df)

  })
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
