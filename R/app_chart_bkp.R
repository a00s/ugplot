library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(style="padding-top: 80px;",
                h1("Absolutely-positioned panels"),
                
                
                plotOutput("plot", height = "800px")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$plot <- renderPlot({
    mtscaled <- as.matrix(scale(mtcars))
    comandtorun <- "heatmap(mtscaled, col = topo.colors(200, alpha=0.5), Colv=F, scale='none')"
    #comandtorun <- "plot(c(1, 2, 3, 4, 5),  c(2, 4, 1, 8, 10), type='l', col='blue', xlab='X-axis label', ylab='Y-axis label', main='Title of the graph')"
    eval(parse(text = comandtorun))
  })
  
  #output$plot2 <- renderPlot({
  #  plot(head(cars, input$n), main="Foo")
  #}, bg = "#F5F5F5")
  
}

shinyApp(ui = ui, server = server)