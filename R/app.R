library(shiny)
library(ggplot2)
library(heatmap3)
library(DT)
#library(rhandsontable)

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
                     #tableOutput("contents")
                     # DT::dataTableOutput("contents")
                     DTOutput("contents")
                     # dataTableOutput("advancedTable")
                     #DTOutput("table_with_checkboxes"),
                     #actionButton("getSelection", "Obter Seleção")
                 ),
                 column(width = 6,
                        div(class = "scrollable-table",
                            div(id = "dynamic_columns")
                        )
                 ),
                 column(width = 6,
                        div(class = "scrollable-table",
                            div(id = "dynamic_rows")
                        )
                 )
                 # div(style = "width: 50%; heigh: 100px; overflow-x: auto;",

                 # ),
                 #DT::dataTableOutput("mytable1"),
                 # ,
                 # checkboxGroupInput("show_vars", "Columns:",names(diamonds), selected = names(diamonds))
        ),
        tabPanel("PLOT",
                 plotOutput("plot", height = "800px")
        )
      )
    )
  )
)

server <- function(input, output) {
  gdf <<- ""
  # render_table <- function(selected_columns, selected_rows) {
  #   df_sub <- df[selected_rows, selected_columns, drop = FALSE]
  #   return(datatable(df_sub))
  # }

  # output$contents <- renderTable({
  #   filepath <- req(input$file1$datapath)
  #   filepath <- "sample_dataset.txt" ### temporary to speed up
  #   #df <- read.csv2(filepath, header = TRUE, sep = "\t", row.names=1, dec=".", stringsAsFactors=FALSE, strip.white = TRUE)
  #   df <- read.csv2(filepath, header = TRUE, sep = "\t", dec=".", stringsAsFactors=FALSE, strip.white = TRUE)
  #   gdf <<- as.matrix(df)
  #   return(df)
  # })


  output$contents <- renderDT({

    #diamonds2 = diamonds[sample(nrow(diamonds), 1000), ]
    #DT::datatable(diamonds2[, input$show_vars, drop = FALSE])

    filepath <- req(input$file1$datapath)
    filepath <- "sample_dataset.txt" ### temporary to speed up
    #df <- read.csv2(filepath, header = TRUE, sep = "\t", row.names=1, dec=".", stringsAsFactors=FALSE, strip.white = TRUE)
    df <- read.csv2(filepath, header = TRUE, sep = "\t", row.names=1, dec=".", stringsAsFactors=FALSE, strip.white = TRUE)
    gdf <<- as.matrix(df)
    # tablecolumns <<- gdf
    # render_table(names(df), rownames(df))


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
        inputId = "column_checkbox_group",
        label = "Rows:",
        choices = rownames(df),
        selected = rownames(df)
      )
    )
    # print(input$dynamic_columns)
    # selected_data <- gdf[, gdf$dynamic_columns, drop = FALSE]

    # Create the DataTable
    # datatable(selected_data, options = list(pageLength = 10))
    # selected_data <- mtcars[, input$selected_columns, drop = FALSE]
    return(df)
  })

  # observeEvent(input$add_button, {
  #   insertUI(
  #     selector = "#dynamic_ui",
  #     where = "afterEnd",
  #     ui = checkboxGroupInput(
  #       inputId = "my_checkbox_group",
  #       label = "Selecione as opções:",
  #       choices = list("Opção 1" = "option1",
  #                      "Opção 2" = "option2",
  #                      "Opção 3" = "option3")
  #     )
  #   )
  # })

  # choose columns to display
  # diamonds2 = diamonds[sample(nrow(diamonds), 1000), ]
  # output$mytable1 <- DT::renderDataTable({
  #   DT::datatable(diamonds2[, input$show_vars, drop = FALSE])
  # })
  # data <- data.frame(
  #   Name = c("John", "Jane", "Mike", "Alice", "Bob"),
  #   Age = c(25, 30, 22, 28, 35),
  #   City = c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix"),
  #   stringsAsFactors = FALSE
  # )
  #
  # output$table_with_checkboxes <- renderDT({
  #   datatable(
  #     data,
  #     rownames = FALSE,
  #     options = list(
  #       columnDefs = list(
  #         list(targets = "_all", className = "dt-center")
  #       )
  #     ),
  #     callback = JS("
  #       function(table) {
  #         var nrows = table.rows().count();
  #         var ncols = table.columns().count();
  #         for(var i = 0; i < nrows; i++) {
  #           table.cell(i, 0).data('<input type=\"checkbox\" id=\"row' + i + '\"/>');
  #         }
  #         for(var j = 0; j < ncols; j++) {
  #           table.column(j).header().innerHTML = '<input type=\"checkbox\" id=\"col' + j + '\"/>' + table.column(j).header().innerHTML;
  #         }
  #       }
  #     "),
  #     escape = FALSE
  #   )
  # })
  #
  # observeEvent(input$getSelection, {
  #   row_checkboxes <- lapply(0:(nrow(data) - 1), function(i) paste0("input$row", i))
  #   row_values <- lapply(row_checkboxes, function(x) input[[x]])
  #
  #   col_checkboxes <- lapply(0:(ncol(data) - 1), function(i) paste0("input$col", i))
  #   col_values <- lapply(col_checkboxes, function(x) input[[x]])
  #
  #   selected_rows <- which(unlist(row_values) == TRUE)
  #   selected_cols <- which(unlist(col_values) == TRUE)
  #
  #   print(paste("Linhas selecionadas:", toString(selected_rows)))
  #   print(paste("Colunas selecionadas:", toString(selected_cols)))
  # })
  # data <- data.frame(
  #   Name = c("John", "Jane", "Mike", "Alice", "Bob"),
  #   Age = c(25, 30, 22, 28, 35),
  #   City = c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix"),
  #   stringsAsFactors = FALSE
  # )
  # Create the advanced table
  # output$advancedTable <- renderDataTable({
  #   datatable(
  #     data,
  #     options = list(
  #       pageLength = 5,  # Number of rows per page
  #       lengthMenu = c(5, 10, 15, 20),  # Dropdown menu to change the number of rows per page
  #       searchHighlight = TRUE,  # Highlight search terms
  #       dom = 'Bfrtip',  # Add buttons for exporting table data
  #       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  #     ),
  #     rownames = FALSE,
  #     extensions = c('Buttons', 'Scroller'),  # Enable Buttons and Scroller extensions
  #     style = "bootstrap",  # Use Bootstrap styling
  #     class = "compact stripe hover",  # Additional CSS classes for the table
  #     filter = "top"  # Display the search box at the top of the table
  #   )
  # })

  lapply(1:nrow(plotlist), function(i){
    bname <- paste0("buttonplot",i)
    observeEvent(input[[bname]], {
      output$plot <- renderPlot({
        comandtorun <- plotlist$code[i]
        comandtorun <- gsub("\\{\\{dataset\\}\\}", "gdf", comandtorun)
        eval(parse(text = comandtorun))
      })
    })
  })
  # observe({
  # #   # Update the table when the selected columns or rows change
  #   output$contents <- renderDT({
  #     render_table(input$column_checkbox_group, input$row_checkbox_group)
  #   })
  # })
}

shinyApp(ui, server)

