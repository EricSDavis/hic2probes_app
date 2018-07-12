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
      newtab <- switch(input$tabNav,
                       "Define" = "Evaluate",
                       "Evaluate" = "Define"
      )
      updateTabItems(session, "tabNav", newtab)
      
    }
  })
  

  ## Interrupt Script ####
  # observeEvent(input$cancel_script, {
  #   shinyalert()
  #   system('trap exit INT')
  # })
  
  ## Get script results in reactive context ####
  script_results <- eventReactive(input$max_probes2, {
    setwd("../hic2probes/") # Adjust working directory to find output/all_probes.bed
    system(paste0("Rscript --vanilla ../hic2probes/scripts/reduce_probes.R ", input$max_probes2))
    data <- as.data.frame(read.delim("../hic2probes/output/filtered_probes.bed", header = F))
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
    req(input$region_slider[2] != input$region_slider[1])
    
    ## Dynamically resizing breakpoints
    gdist <- input$stop - input$start
    sdist <- input$region_slider[2] - input$region_slider[1]
    breaks <- gdist/(gdist*(0.01*(sdist/gdist)))
    
    ## Custom histogram barplot
    custom_histogram <- function(data=data, breaks = breaks, xlim){
      ## Cut table into bins along with pass numbers
      bins <- table(cut(data$start, breaks = breaks, include.lowest = T), factor(data$pass, levels = 0:3))
      bin_start <- as.numeric(gsub("^[:(:]|\\[|,.*", "", rownames(bins)))
      bin_stop <- as.numeric(gsub(".*,|\\]","", rownames(bins)))
      bdata <- as.data.frame(cbind(bin_start, bin_stop, bins))
      bdata$bins <- rowSums(bdata[,3:ncol(bdata)])
      bin_size <- bdata$bin_stop[1] - bdata$bin_start[1]
      
      ## Define color palette
      cols <- c("green", "blue", "purple", "red")
      cols <- adjustcolor(cols, alpha.f = 0.6)
      
      ## Plot stacked histogram barplot
      plot(c(0,1), c(0,1), "n",
           xlim = xlim, #c(min(bdata$bin_start), max(bdata$bin_stop)),
           ylim = c(0, max(bdata$bins)),
           xlab = "genomic coordinates",
           ylab = "Number of Probes",
           main = "Histogram of Probe Coverage",
           sub = paste0("N= ", sum(bdata$bins), ", ",
                        "bin size= ", bin_size, "bp"),
           frame.plot = F)
      rect(bdata$bin_start, 0, bdata$bin_stop, bdata$bins)
      rect(bdata$bin_start, 0, bdata$bin_stop, bdata$`0`, col = cols[1])
      rect(bdata$bin_start, bdata$`0`, bdata$bin_stop, bdata$`0`+bdata$`1`, col = cols[2])
      rect(bdata$bin_start, bdata$`0`+bdata$`1`, bdata$bin_stop, bdata$`0`+bdata$`1`+bdata$`2`, col = cols[3])
      rect(bdata$bin_start, bdata$`0`+bdata$`1`+bdata$`2`, bdata$bin_stop, bdata$`0`+bdata$`1`+bdata$`2`+bdata$`3`, col = cols[4])
      legend('topleft', title = "Pass Number", legend = c(0:3), fill = cols, bty = 'n', cex = 0.55)
      
    }
    
    ## Implement function
    custom_histogram(data, breaks = breaks, xlim = c(input$region_slider[1], input$region_slider[2]))
    
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
  
  observeEvent(input$region_slider, {
    if(input$region_slider[2] == input$region_slider[1]){
      updateSliderInput(session, "region_slider", value = c(input$start, input$stop))
      shinyalert("You Cant Do That...", "Stop it!", type = "error")
    }
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


