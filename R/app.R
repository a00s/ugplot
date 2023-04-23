library(shiny)
library(ggplot2)
library(heatmap3)
library(DT)
library(gplots)

plotlist <- read.csv("plotlist.csv", sep=";", header = TRUE)
palettelist <- read.csv("palette.csv", sep=";", header = TRUE)

df <- ""
ui <- fluidPage(
  tags$style(HTML("
    .scrollable-table {
      height: 200px;
      overflow-y: auto;
    }
  ")),
  titlePanel(tags$img(src = "ugplot.png", height = "50px"),"ugPlot"),
  tabsetPanel(
    tabPanel("TABLE",
             tags$div(
               style = "display: inline-block; vertical-align: top;",
               fileInput(
                 "file1", "Choose a CSV file",
                 multiple = FALSE,
                 accept = c(
                   "text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"
                 )
               )
             ),
             tags$div(
               style = "display: inline-block; vertical-align: top;",
               selectInput(inputId = "separator", label = "CSV separator:", choices = c("space"=" ", "tab"="\t", ";", ",", "|"), selected = "\t"),
               actionButton("transpose_table", "Transpose table"),
             ),
             div(style = "width: 100%; overflow-x: auto;",
                 DTOutput("contents")
             ),
             column(width = 6,
                    actionButton("uncheck_all_columns", "Uncheck all"),
                    actionButton("check_all_columns", "Check all"),
                    div(class = "scrollable-table",
                        div(id = "dynamic_columns")
                    )
             ),
             column(width = 6,
                    actionButton("uncheck_all_rows", "Uncheck all"),
                    actionButton("check_all_rows", "Check all"),
                    div(class = "scrollable-table",
                        div(id = "dynamic_rows")
                    )
             ),
    ),
    tabPanel("PLOT",
             sidebarLayout(
               sidebarPanel(
                 lapply(1:nrow(plotlist), function(i){
                   bname <- paste0("buttonplot",i)
                   imgname <- paste0("img/",plotlist$img[i])
                   print(imgname)
                   fluidRow(
                     tags$img(src = imgname, width = 130, height = 130),
                     actionButton(bname, plotlist$name[i])
                   )}),
                 lapply(1:nrow(plotlist), function(i){
                   bname <- paste0("buttonpalette",i)
                   imgname <- paste0("img/",palettelist$img[i])
                   print(imgname)
                   if(imgname != "img/NA"){
                     fluidRow(
                       tags$img(src = imgname, width = 130, height = 130),
                       actionButton(bname, palettelist$name[i])
                     )
                   }
                 }
                 )
               ),
               mainPanel(
                 plotOutput("plot", height = "800px")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  changed_table <<- ""
  numeric_table <<- ""
  changed_palette <<- 0
  defaultpalette <<- reactiveVal(colorRampPalette(c("red", "yellow", "green"))(256))

  transpose_table2 <<- reactiveVal(0)
  refresh_counter <<- reactiveVal(0)
  tab_separator <<- reactiveVal(",")
  file_click_count <<- reactiveVal(0)
  last_file_click_count <<- 0
  observeEvent(input$file1, {
    file_click_count(file_click_count() + 1)
  })

  output$contents <- renderDT({
    if(last_file_click_count == 0 | (last_file_click_count != file_click_count())){
      filepath <- req(input$file1$datapath)
      filepath <- "sample_dataset.txt" ### temporary to speed up
      df <<- read.table(filepath, header = TRUE, sep = tab_separator(), row.names=1, dec=".", stringsAsFactors=FALSE, strip.white = TRUE)
      changed_palette <<- 0
      changed_table <<- as.matrix(df)
      last_file_click_count <<- file_click_count()
      load_checkbox_group()
    }
    return(changed_table[input$row_checkbox_group,input$column_checkbox_group])
  })

  lapply(1:nrow(plotlist), function(i){
    bname <- paste0("buttonplot",i)
    observeEvent(input[[bname]], {
      output$plot <- renderPlot({
        comandtorun <- plotlist$code[i]
        numeric_table <<- apply(changed_table[input$row_checkbox_group,input$column_checkbox_group], c(1, 2), as.numeric)
        comandtorun <- gsub("\\{\\{dataset\\}\\}", "numeric_table", comandtorun)
        if(plotlist$palette[i] != "" && changed_palette == 0){
          comandpalette <- paste("defaultpalette(",plotlist$palette[i],")")
          eval(parse(text = comandpalette))
        }
        comandtorun <- gsub("\\{\\{palette\\}\\}", "defaultpalette()", comandtorun)
        print(comandtorun)
        eval(parse(text = comandtorun))
      })
    })
  })

  lapply(1:nrow(palettelist), function(i){
    bname <- paste0("buttonpalette",i)
    observeEvent(input[[bname]], {
      changed_palette <<- 1
      comandpalette <- paste("defaultpalette(",palettelist$code[i],")")
      eval(parse(text = comandpalette))
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
    if(transpose_table2() == 0){
      transpose_table2(1)
    } else {
      transpose_table2(0)
    }
    df <<- data.frame(t(as.matrix(df)))
    changed_table <<- as.matrix(df)
    load_checkbox_group()
  })

  observeEvent(input$separator, {
    tab_separator(input$separator)
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

