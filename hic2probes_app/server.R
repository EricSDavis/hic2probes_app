library(shiny)

# Define server logic
shinyServer(function(input, output, session) {
  
  ##------------Switch to Evalutate Page-----------####
  observeEvent(input$run_script, {
    newtab <- switch(input$tabNav,
                     "Define" = "Evaluate",
                     "Evaluate" = "Define"
    )
    updateTabItems(session, "tabNav", newtab)
  })
  
  ##------------Return to Define Page--------------####
  observeEvent(input$return, {
    newtab <- switch(input$tabNav,
                     "Evaluate" = "Define",
                     "Define" = "Evaluate"
                     
    )
    updateTabItems(session, "tabNav", newtab)
    removeUI(selector = "div.loader")
    insertUI(selector = "div#spinner",
             ui = conditionalPanel(
               condition = "input.run_script",
               HTML('
                    <div id="spinner" class="loader"></div>
                    ')
               )
             )
  })
  
  ##-------------Run script-----------------------####
  observeEvent(input$run_script, {
    withProgress(message = "Constructing Probes ...", {
      
      message <- "There were no errors!"
      
      if(input$stop < input$start){
        message <- "Invalid option: Start must be less than stop"
      }
      
      ## Handle NA values for input$max probes ####
      if (is.na(input$max_probes)){
        max_probes <- ""
      } else {
        max_probes <- paste0(" -n ", input$max_probes)
      }
      
      ## Stitch command ####
      command <- paste0("./../hic2probes/shell/hicsq.sh",
                        " -c ", input$chr,
                        " -b ", input$start,
                        " -e ", input$stop,
                        " -r ", input$resenz,
                        max_probes)
      system(command, input = "yes")
    })
    
    ## Test messages ####
    output$test <- renderText({
      message
    })
  })
  
  ## Get script results in reactive context ####
  script_results <- eventReactive(input$run_script, {
    data <- as.data.frame(read.delim("../hic2probes/output/probes.bed", header = F))
    colnames(data) <- c("chr", "start", "stop", "shift", "res.fragment", "dir", "AT", "GC", "seq", "pass")
    data
  })
  
  ##--------------Probe data table----------------####
  output$Probes <- renderDataTable({
    script_results()
  })



  
  
  
  
})
