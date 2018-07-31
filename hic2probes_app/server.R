
# Define server logic
shinyServer(function(input, output, session) {
  
  ##--------------- Shinyjs Toggles----------------####
  shinyjs::onclick("toggleAdvanced1", shinyjs::toggle(id = "advanced1", anim = TRUE))
  observe({
    shinyjs::toggleState("custom_index", input$index == "Custom")
  })
  
  ##--------------------Title----------------------####
  output$title <- renderUI({
    req(input$run_script)
    paste0(input$chr, ": ", input$start, "-", input$stop)
  })

  ##------------Return to Define Page--------------####
  observeEvent(input$return, {
    newtab <- switch(input$tabNav,
                     "Evaluate" = "Define",
                     "Define" = "Evaluate"
    )
    system("pwd")
    system("rm -r ../hic2probes/output/")
    updateTabItems(session, "tabNav", newtab)
    session$reload() #reload session on input$return
  })
  
  ##------------------Set Default Tab-------------####
  observeEvent(input$run_script, {
    updateTabsetPanel(session, "tab_view", selected = "Summary View")
  }) 

  ##-------------Run script-----------------------####
  observeEvent(input$run_script, {
    
    ## Error Handling ####
    if(input$stop <= input$start | is.na(input$stop) | is.na(input$start)){
      message <- "Start must be less than stop"
      shinyalert("Invalid Option:", message, type = "error")
    } else {
      
      ## Stitch command ####
      command <- paste0("./../hic2probes/shell/hicsq.sh",
                        " -c ", input$chr,
                        " -b ", input$start,
                        " -e ", input$stop,
                        " -r ", input$resenz)
      
      system(command, input = "yes")
      
      ## Choose Index based on radioButton input ####
      if (input$index == "Index1"){
        StartIndex <- '"TCGCGCCCATAACTC"'
        EndIndex <- '"CTGAGGGTCCGCCTT"'
      } else if (input$index == "Index2"){
        StartIndex <- '"ATCGCACCAGCGTGT"'
        EndIndex <- '"CACTGCGGCTCCTCA"'
      } else if (input$index == "Index3"){
        StartIndex <- '"CCTCGCCTATCCCAT"'
        EndIndex <- '"CACTACCGGGGTCTG"'
      } else if (input$index == "Custom"){
        StartIndex <- sprintf('"%s"', input$custom_index_1)
        EndIndex <- sprintf('"%s"', input$custom_index_2)
      } else {
        StartIndex <- '""'
        EndIndex <- '""'
      }
      command <- paste0("awk -v OFS='\t' '{print $1, $2, $3, $4, $5, $6, $7, $8, ",  StartIndex, "$9", EndIndex, " , $10}' ../hic2probes/output/all_probes.bed > ../hic2probes/output/temp.bed")
      system(command)
      ## Initial filtering with max_probes
      wd <- getwd()
      setwd("../hic2probes/") # Adjust working directory to find output/all_probes.bed
      system("Rscript --vanilla ../hic2probes/scripts/reduce_probes.R ")
      system("mv ../hic2probes/output/filtered_probes.bed ../hic2probes/output/temp.bed")
      setwd(wd)
      system("mv ../hic2probes/output/temp.bed ../hic2probes/output/all_probes.bed")
      system("cat ../hic2probes/output/all_probes.bed")

      ## Switch to Evaluate Page ####
      newtab <- switch(input$tabNav,
                       "Define" = "Evaluate",
                       "Evaluate" = "Define"
      )
      updateTabItems(session, "tabNav", newtab)
      
    }
  })
  
  ## Get script results in reactive context ####
  script_results <- reactive({
    req(input$index)
    req(input$run_script)
    
    ## Handle NULL values for input$max_probes ####
    if (is.null(input$max_probes)){
      max_probes <- NA
    }else{
      max_probes <- input$max_probes
    }
    
    if(max_probes > 0 || is.na(max_probes)){
      wd <- getwd()
      setwd("../hic2probes/") # Adjust working directory to find output/all_probes.bed
      system(paste0("Rscript --vanilla ../hic2probes/scripts/reduce_probes.R ", max_probes))
      setwd(wd)
      if (is.na(max_probes)){
        wd <- getwd()
        setwd("../hic2probes/") # Adjust working directory to find output/all_probes.bed
        system("Rscript --vanilla ../hic2probes/scripts/reduce_probes.R ")
        setwd(wd)
      }
    }
    data <- as.data.frame(read.delim("../hic2probes/output/filtered_probes.bed", header = F))
    colnames(data) <- c("chr", "start", "stop", "shift", "res.fragment", "dir", "AT", "GC", "seq", "pass")
    data
  })
  
  ##-----------Load in Restriction Sites----------####
  res.sites <- reactive({
    req(script_results())
    ## Load in restriction sites
    res.sites <- read.delim("../hic2probes/output/fragments.bed", header = F)
    sites <- unique(sort(c(res.sites[,2], res.sites[,3])))
    sites <- sites + input$start
    sites
  })
  
  ##------------Load in All Probes--------------####
  all_probes <- reactive({
    all_probes <- read.delim("../hic2probes/output/all_probes.bed", header = F)
    all_probes
  })
  
  ##---------------Probe Number-----------------####
  output$max_probes <- renderUI({
    numericInput(
      inputId = "max_probes",
      label = "Specify Probe Number:",
      value = NA,
      min = 0
    )
  })
  
  ##--------------Probe Density-----------------####
  output$probe_density <- renderUI({
    req(input$run_script)
    req(script_results())
    req(input$start)
    req(input$stop)
    data <- script_results()
    all_probes <- all_probes()
    probes <- nrow(data)
    max_possible <- nrow(all_probes)
    region_length <- input$stop - input$start
    density <- probes/region_length*1000
    max_density <- max_possible/region_length*1000
    numericInput(
      inputId = "probe_density",
      label = "Probe Density (p/kb)",
      min = 0,
      value = density,
      max = max_density
    )
  })
  
  ##--------------Coverage Histogram----------------####
  output$coverageHistogram <- renderPlot({
    data <- script_results()
    sites <- res.sites()
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
           ylim = c(0, 1.5*max(bdata$bins)),
           xlab = "genomic coordinates",
           ylab = "Number of Probes",
           sub = paste0("N= ", sum(bdata$bins), ", ",
                        "bin size= ", bin_size, "bp"),
           frame.plot = F,
           las = 1)
      rect(bdata$bin_start, 0, bdata$bin_stop, bdata$bins, border=NA)
      rect(bdata$bin_start, 0, bdata$bin_stop, bdata$`0`, col = cols[1], border=NA)
      rect(bdata$bin_start, bdata$`0`, bdata$bin_stop, bdata$`0`+bdata$`1`, col = cols[2], border=NA)
      rect(bdata$bin_start, bdata$`0`+bdata$`1`, bdata$bin_stop, bdata$`0`+bdata$`1`+bdata$`2`, col = cols[3], border=NA)
      rect(bdata$bin_start, bdata$`0`+bdata$`1`+bdata$`2`, bdata$bin_stop, bdata$`0`+bdata$`1`+bdata$`2`+bdata$`3`, col = cols[4], border=NA)
      legend('topright', title = "Pass Number", legend = c(0:3), fill = cols, bty = 'n', border=NA)
      if (input$toggle_res.sites == T){ #input$region_slider[2] - input$region_slider[1] <= 50000 & 
        segments(sites, -1, sites, 0.65*max(bdata$bins))
      }
    }
    
    ## Implement function
    custom_histogram(data, breaks = breaks, xlim = c(input$region_slider[1], input$region_slider[2]))
  })
  
  ##--------------Restriction Site Histogram----------------####
  output$restrictionHistogram <- renderPlot({
    data <- script_results()
    sites <- res.sites()
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
      bins <- table(cut(data, breaks = breaks, include.lowest = T))
      bin_start <- as.numeric(gsub("^[:(:]|\\[|,.*", "", rownames(bins)))
      bin_stop <- as.numeric(gsub(".*,|\\]","", rownames(bins)))
      bdata <- as.data.frame(cbind(bin_start, bin_stop, bins))
      bin_size <- bdata$bin_stop[1] - bdata$bin_start[1]
      
      ## Define color palette
      cols <- c("green", "blue", "purple", "red")
      cols <- adjustcolor(cols, alpha.f = 0.6)
      
      ## Plot stacked histogram barplot
      plot(c(0,1), c(0,1), "n",
           xlim = xlim, #c(min(bdata$bin_start), max(bdata$bin_stop)),
           ylim = c(0, 1.5*max(bdata$bins)),
           xlab = "genomic coordinates",
           ylab = "Number of Restriction Sites",
           sub = paste0("N= ", sum(bdata$bins), ", ",
                        "bin size= ", bin_size, "bp"),
           frame.plot = F,
           las = 1)
      rect(bdata$bin_start, 0, bdata$bin_stop, bdata$bins, col = "grey", border = NA)
    }
    
    ## Implement function
    custom_histogram(sites, breaks = breaks, xlim = c(input$region_slider[1], input$region_slider[2]))
  })
  
  ##--------------GC Content Plot----------------####
  output$gc_plot <- renderPlot({
    data <- script_results()
    sites <- res.sites()
    req(input$region_slider)
    
    cols <- c("green", "blue", "purple", "red")[factor(data$pass)]
    cols <- adjustcolor(cols, alpha.f = 0.6)
    
    leg_cols <- c("green", "blue", "purple", "red")
    leg_cols <- adjustcolor(leg_cols, alpha.f = 0.6)
    
    plot(c(0,1), c(0,1), "n",
         xlim=c(input$region_slider[1], input$region_slider[2]),
         xlab = paste0(input$chr, " region"),
         ylab = "GC Fraction",
         las = 1,
         frame.plot = F)
    segments(data$start, data$GC, data$stop, data$GC, col = cols, lwd=5)
    legend('topright', title = "Pass Number", legend = c(0, 1, 2, 3), fill = leg_cols, bty = 'n', border = NA)
    if (input$toggle_res.sites == T){ #input$region_slider[2] - input$region_slider[1] <= 50000 & 
      segments(sites, -1, sites, 0.65)
    }
  })
  
  ##--------------Plotting Shift-----------------####
  output$shift_plot <- renderPlot({
    data <- script_results()
    sites <- res.sites()
    
    cols <- c("green", "blue", "purple", "red")[factor(data$pass)]
    cols <- adjustcolor(cols, alpha.f = 0.6)
    
    leg_cols <- c("green", "blue", "purple", "red")
    leg_cols <- adjustcolor(leg_cols, alpha.f = 0.6)
    
    plot(data$start, data$shift, "n", frame.plot = F,
         xlim = c(input$region_slider[1], input$region_slider[2]),
         ylim = c(min(data$shift), max(data$shift)+0.45*(max(data$shift))),
         xlab = paste0(input$chr, " region"),
         ylab = "Base Pairs from Restriction Site",
         las = 1
         )
    segments(data$start, data$shift, data$stop, data$shift, col = cols, lwd = 5)
    legend('topright', title = "Pass Number", legend = c(0, 1, 2, 3), fill = leg_cols, bty = 'n', border = NA)
    if (input$toggle_res.sites == T){ #input$region_slider[2] - input$region_slider[1] <= 50000 & 
      segments(sites, -1, sites, max(data$shift))
    }
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
  
  
  ##-------------Download Results----------------####
  output$downloadProbes <- downloadHandler(
    filename = function(){
      req(input$chr)
      req(input$start)
      req(input$stop)
      paste0(Sys.Date(), "-",
             input$chr, ":",
             input$start, "-",
             input$stop, "-",
             ".txt")
    },
    content = function(file){
      readr::write_tsv(script_results(), file)
    }
  )
  
  output$downloadPlots <- downloadHandler(
    filename = function(){
      paste("test.pdf")
    },
    content = function(file){
      pdf(file)
      plot(0,1, 'n', main = "Test Plot")
      dev.off()
    }
  )
  
  
  ##--------------Summary View Info-------------####
  ## Settings ####
  output$info_chr <- renderText({
    paste0("Chromosome: ", input$chr)
  })
  output$info_start <- renderText({
    paste0("Start: ", input$start)
  })
  output$info_stop <- renderText({
    paste0("Stop: ", input$stop)
  })
  output$info_resenz <- renderText({
    paste0("Restriction Enzyme: ", input$resenz)
  })

  ## Results ####
  observe({
    output$info_res.sites <- renderText({
      req(input$run_script)
      req(res.sites())
      sites <- res.sites()
      paste0("Restriction Sites: ", length(sites), " found")
    })
    output$info_all_probes <- renderText({
      req(input$run_script)
      req(script_results())
      req(all_probes())
      data <- script_results()
      all_probes <- all_probes()
      probes <- nrow(data)
      max_possible <- nrow(all_probes)
      region_length <- input$stop - input$start
      density <- probes/region_length*1000
      max_density <- max_possible/region_length*1000
      paste0("Maximum Possible: ", max_possible, " probes, (", max_density, " probes/kb)" )
    })
    output$info_selected_probes <- renderText({
      req(input$run_script)
      req(script_results())
      data <- script_results()
      probes <- nrow(data)
      region_length <- input$stop - input$start
      density <- probes/region_length*1000
      paste0("Selected: ", probes, " probes, (", density, " probes/kb)")
    })
    output$info_avgGC <- renderText({
      req(input$run_script)
      req(script_results())
      data <- script_results()
      paste0("Average GC Fraction: ", round(mean(data$GC), 2))
    })
  })

  ##--------------Summary View Plots-------------####
  output$summary_gc <- renderPlot({
    data <- script_results()
    cols <- c("red", "purple", "blue", "green", "blue", "purple", "red")
    cols <- adjustcolor(cols, alpha.f = 0.6)
    breaks <- nrow(data)*0.05
    if (breaks < 1) breaks <- 1
    h <- hist(data$GC, breaks = breaks, plot = F)
    cuts <- cut(h$breaks, c(-Inf, .25, .4, 0.5, 0.6, .7, .8, Inf))
    cols[cuts]
    plot(h, col = cols[cuts], xlim = c(0.2, 0.8),
         main = "GC Content",
         ylab = "Number of Probes",
         xlab = "GC Fraction")
  })
  
  output$summary_pass <- renderPlot({
    data <- script_results()
    cols <- c("green", "blue", "purple", "red")
    cols <- adjustcolor(cols, alpha.f = 0.6)
    barplot(table(factor(data$pass, levels = 0:3)), col = cols,
            main = "Probe Quality",
            ylab = "Number of Probes",
            xlab = "Pass Number")
  })
  
  
  output$summary_shift <- renderPlot({
    data <- script_results()
    hist(data$shift, main = "Distance from Restriction Site",
         xlab = "Distance (bp)")
  })
  
  ##--------------Probe data table----------------####
  observe({
    output$Probes <- DT::renderDataTable({
      data <- script_results()
      DT::datatable(data)
    })
  })
  
}) # END


