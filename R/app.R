library(shiny)
library(ggplot2)
library(heatmap3)
library(DT)

plotlist <- read.csv("plotlist.csv", sep=";", header = TRUE)
print(plotlist)
df <- ""
ui <- fluidPage(
  tags$style(HTML("
    .scrollable-table {
      height: 200px;
      overflow-y: auto;
    }
  ")),
  titlePanel(tags$img(src = "ugplot.png", height = "50px"),"ugPlot"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      lapply(1:nrow(plotlist), function(i){
        bname <- paste0("buttonplot",i)
        imgname <- paste0("img/",plotlist$img[i])
        print(imgname)
        fluidRow(
          tags$img(src = imgname, width = 130, height = 130),
          actionButton(bname, plotlist$name[i])
        )}),
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("TABLE",
                 div(style = "width: 100%; overflow-x: auto;",
                     DTOutput("contents")
                 ),
                 actionButton("transpose_table", "Transpose table"),
                 actionButton("uncheck_all_columns", "Uncheck All Columns"),
                 actionButton("check_all_columns", "Check All Columns"),
                 actionButton("uncheck_all_rows", "Uncheck All Rows"),
                 actionButton("check_all_rows", "Check All Rows"),
                 column(width = 6,
                        div(class = "scrollable-table",
                            div(id = "dynamic_columns")
                        )
                 ),
                 column(width = 6,
                        div(class = "scrollable-table",
                            div(id = "dynamic_rows")
                        )
                 ),
        ),
        tabPanel("PLOT",
                 plotOutput("plot", height = "800px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  changed_table <<- ""
  transpose_table <<- 0
  file_click_count <<- reactiveVal(0) # Initialize the counter
  last_file_click_count <<- 0
  observeEvent(input$file1, {
    file_click_count(file_click_count() + 1)
  })

  output$contents <- renderDT({
    filepath <- req(input$file1$datapath)
    filepath <- "sample_dataset.txt" ### temporary to speed up
    df <<- read.csv2(filepath, header = TRUE, sep = "\t", row.names=1, dec=".", stringsAsFactors=FALSE, strip.white = TRUE)
    print(paste("Transpose",transpose_table))
    # if(transpose_table == 0){
    #   changed_table <<- as.matrix(df)
    # } else {
    #   changed_table <<- t(as.matrix(df))
    # }
    changed_table <<- as.matrix(df)

    if(last_file_click_count == 0){
      last_file_click_count <<- file_click_count()
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
    return(changed_table[input$row_checkbox_group,input$column_checkbox_group])
  })

  lapply(1:nrow(plotlist), function(i){
    bname <- paste0("buttonplot",i)
    observeEvent(input[[bname]], {
      output$plot <- renderPlot({
        comandtorun <- plotlist$code[i]
        comandtorun <- gsub("\\{\\{dataset\\}\\}", "changed_table[input$row_checkbox_group,input$column_checkbox_group]", comandtorun)
        eval(parse(text = comandtorun))
      })
    })
  })
  observeEvent(input$uncheck_all_columns, {
    updateCheckboxGroupInput(
      session,
      inputId = "column_checkbox_group",
      selected = character(0)
    )
  })
  observeEvent(input$check_all_columns, {
    updateCheckboxGroupInput(
      session,
      inputId = "column_checkbox_group",
      selected = names(df)
    )
  })
  observeEvent(input$uncheck_all_rows, {
    updateCheckboxGroupInput(
      session,
      inputId = "row_checkbox_group",
      selected = character(0)
    )
  })
  observeEvent(input$check_all_rows, {
    updateCheckboxGroupInput(
      session,
      inputId = "row_checkbox_group",
      selected = rownames(df)
    )
  })
  observeEvent(input$transpose_table, {
    print("aqui transpose")
    ntransposetable <- t(changed_table)
    print(ntransposetable)
    transpose_table <<- 1

    #df <<- as.matrix()
    #df <<- read.csv2(filepath, header = TRUE, sep = "\t", row.names=1, dec=".", stringsAsFactors=FALSE, strip.white = TRUE)
    #changed_table <<- as.matrix(df)

    # changed_table <<- ntransposetable
  })

}

shinyApp(ui, server)

