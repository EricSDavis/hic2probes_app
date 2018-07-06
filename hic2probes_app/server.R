library(shiny)

# Define server logic
shinyServer(function(input, output) {
  
  eventReactive(input$test, {
    system("./hic2probes/shell/hicsq.sh -e 133100000 -n 10", input = "yes")
    data <- as.data.frame(read.delim("../hic2probes/output/probes.bed"))
    data
  })
  
  output$Probes <- renderDataTable({
    stuff()
  })
  
  
  
})
