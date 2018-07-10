library(shiny)
library(shinydashboard)
library(shinyalert)

# Define server logic
shinyServer(function(input, output, session) {
  
  ##--------------------Title----------------------####
  output$title <- renderUI({
    paste0(input$chr, ": ", input$start, "-", input$stop)
  })
  
  ##------------Return to Define Page--------------####
  observeEvent(input$return, {
    newtab <- switch(input$tabNav,
                     "Evaluate" = "Define",
                     "Define" = "Evaluate"
                     
    )
    updateTabItems(session, "tabNav", newtab)
  })
  
  ##-------------Run script-----------------------####
  observeEvent(input$run_script, {
    
    ## Error Handling ####
    if(input$stop <= input$start | is.na(input$stop) | is.na(input$start)){
      message <- "Start must be less than stop"
      shinyalert("Invalid Option:", message, type = "error")
    } else {
      
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
      
      ## Switch to Evaluate Page ####
      observeEvent(input$run_script, {
        newtab <- switch(input$tabNav,
                         "Define" = "Evaluate",
                         "Evaluate" = "Define"
        )
        updateTabItems(session, "tabNav", newtab)
      })
      
    }
  })
  
  ## Get script results in reactive context ####
  script_results <- eventReactive(input$run_script, {
    data <- as.data.frame(read.delim("../hic2probes/output/probes.bed", header = F))
    colnames(data) <- c("chr", "start", "stop", "shift", "res.fragment", "dir", "AT", "GC", "seq", "pass")
    data
  })
  
  ##--------------Probe data table----------------####
  output$Probes <- DT::renderDataTable({
    data <- script_results()
    DT::datatable(data)
  })
  
  
  ##--------------Coverage Histogram----------------####
  output$coverageHistogram <- renderPlot({
    data <- script_results()
    values <- sort(unlist(lapply(seq(1:nrow(data)), function(x) data$start[x]:data$stop[x])))
    req(input$region_slider)
    
    begin <- input$start
    end <- input$stop
    if(end - begin <= 1000){
      breaks <- 100000
    } else if(end - begin <= 10000) {
      breaks <- 10000
    } else if(end - begin <= 100000){
      breaks <- 1000
    } else if (end - begin <= 1000000){
      breaks <- 100
    } else if (end - begin <= 10000000){
      breaks <- 100
    } else if (end - begin <= 100000000){
      breaks <- 10
    } else if (end - begin <= 1000000000){
      breaks <- 1
    } else {
      breaks <- 1
    }
    
    hist(
      values,
      breaks = breaks,
      xlim = c(input$region_slider[1], input$region_slider[2]),
      main = "",
      xlab = paste0(input$chr, " region"),
      col = "grey"
    )
    legend('topright', legend = paste0("bin size= ", breaks))
    
  })
  
  ##--------------GC Content Plot----------------####
  output$gc_plot <- renderPlot({
    data <- script_results()
    req(input$region_slider)
    
    cols <- c("green", "blue", "purple", "red")[factor(data$pass)]
    cols <- adjustcolor(cols, alpha.f = 0.6)
    
    plot(c(0,1), c(0,1), "n", xlim=c(input$region_slider[1], input$region_slider[2]), xlab = paste0(input$chr, " region"), frame.plot = F)
    segments(data$start, data$GC, data$stop, data$GC, col = cols, lwd=5)
    legend('topright', title = "Pass Number", legend = c(0, 1, 2, 3), fill = c("green", "blue", "purple", "red"))
  })
  
  ##--------------Plotting Shift-----------------####
  output$shift_plot <- renderPlot({
    data <- script_results()
    
    cols <- c("green", "blue", "purple", "red")[factor(data$pass)]
    cols <- adjustcolor(cols, alpha.f = 0.6)
    
    plot(data$start, data$shift, "n", frame.plot = F, xlim = c(input$region_slider[1], input$region_slider[2]))
    segments(data$start, data$shift, data$stop, data$shift, col = cols, lwd = 5)
  })
  
  ##-------------Region View Slider--------------####
  
  output$region_slider <- renderUI({
    sliderInput(
      inputId = "region_slider",
      label = "Select Viewing Region:",
      min = input$start,
      max = input$stop,
      value = c(input$start, input$stop),
      step = 10
    )
  })
  
  
  
  ##--------------Summary View Plots-------------####
  output$summary_gc <- renderPlot({
    data <- script_results()
    plot(density(data$GC), main = "GC Content")
  })
  
  output$summary_pass <- renderPlot({
    data <- script_results()
    plot(density(data$pass), main = "Probe Quality")
  })
  
  
  output$summary_shift <- renderPlot({
    data <- script_results()
    plot(density(data$shift), main = "Distance from Restriction Site")
  })
  
  
}) # END


