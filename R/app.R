library(shiny)
library(ggplot2)
plotlist <- read.csv("plotlist.csv", sep=";", header = TRUE)
print(plotlist)
df <- ""
ui <- fluidPage(
  titlePanel("ugPlot"),
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
      tableOutput("contents"),
      plotOutput("plot", height = "800px")
    )
  )
)

server <- function(input, output) {
  gdf <<- ""
  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv2("sample_dataset.txt", header = TRUE, sep = "\t", row.names=1, dec=".", stringsAsFactors=FALSE, strip.white = TRUE)
    gdf <<- as.matrix(df)
    return(head(df))
  })

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
}

shinyApp(ui, server)
