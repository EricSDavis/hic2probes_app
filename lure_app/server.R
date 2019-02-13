# Define a function that extracts coords
extractcoords <- function (coordinates)
{
  coordinates<-gsub(" ", "", coordinates)
  match<-str_match(coordinates, "(chr.+):(\\d+)-(\\d+)")
  if(is.na(match[1, 1]) || nchar(match[1, 1]) != nchar(coordinates)) {
    return(NULL)
  }
  return(list("chr"=match[1, 2], "start"=as.numeric(match[1, 3]), "stop"=as.numeric(match[1, 4])))
}

# Define server logic
shinyServer(function(input, output, session) {
  
  options(scipen=999)
  
  ##--------------------Title----------------------####
  output$title <- renderUI({
    req(input$run_script)
    coords<-extractcoords(input$coordinates)
    paste0(input$genome, " - ", coords$chr, ":", coords$start, "-", coords$stop)
  })

  ##------------Return to Define Page--------------####
  observeEvent(input$return, {
    if(dir.exists(output_folder)) {
      unlink(output_folder, recursive = T, force = T)
    }
    updateTabItems(session, "tabNav", "Define")
    if(input$tabNav == "Evaluate") {
      session$reload() #reload session on input$return
    }
  })
  
  ##----------------Set About Tab------------------####
  observeEvent(input$about, {
    updateTabItems(session, "tabNav", "About")
  })
  
  ##----------------Set Download Tab------------------####
  observeEvent(input$download, {
    updateTabItems(session, "tabNav", "Download")
  })
  
  ##----------------Set Contact Tab------------------####
  observeEvent(input$contact, {
    updateTabItems(session, "tabNav", "Contact")
  })
  
  ##------------------Set Default Tab-------------####
  observeEvent(input$run_script, {
    updateTabsetPanel(session, "tab_view", selected = "Summary View")
  })
  
  output_folder <- paste0(tempdir(), '/', session$token)

  ##-------------Run script-----------------------####
  observeEvent(input$run_script, {
    
    ## Define run_script ####
    run_script <- function(coords) {
      ## Stitch command ####
      command <- paste0("./../lure/shell/lure.sh",
                        " -c ", coords$chr,
                        " -b ", coords$start,
                        " -e ", coords$stop,
                        " -r ", input$resenz,
                        " -g ", paste0('"', "./../genomes/", sub(".+: ", "", basename(input$genome)), ".fa", '"'),
                        " -o ", output_folder)
      print (command)
      console_output <- system(command, input = "yes", intern = T)
      if(!is.null(attr(console_output, "status"))) {
        shinyalert("Error", console_output[length(console_output)], type = "error")
      } else {
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
        command <- paste0("awk -v OFS='\t' '{print $1, $2, $3, $4, $5, $6, $7, $8, ",  StartIndex, "$9", EndIndex, " , $10}' ", output_folder, "/all_probes.bed > ", output_folder, "/temp.bed")
        system(command)
        system(paste0("mv ", output_folder, "/temp.bed ", output_folder, "/all_probes.bed"))
        ## Initial filtering with max_probes
        wd <- getwd()
        setwd("../lure/") # Adjust working directory to find output/all_probes.bed
        system(paste0("Rscript --vanilla ../lure/scripts/reduce_probes.R "))
        system(paste0("mv ", output_folder, "/filtered_probes.bed ", output_folder, "/temp.bed"))
        setwd(wd)
        system(paste0("mv ", output_folder, "/temp.bed ", output_folder, "/all_probes.bed"))
        system(paste0("cat ", output_folder, "/all_probes.bed"))
        
        ## Switch to Evaluate Page ####
        newtab <- switch(input$tabNav,
                         "Define" = "Evaluate",
                         "Evaluate" = "Define"
        )
        updateTabItems(session, "tabNav", newtab)
      }
    }
    
    ## Error Handling ####
    coords <- extractcoords(input$coordinates)
    if(is.null(coords)) {
      shinyalert("Invalid Option", "Chromosome coordinates are not in a valid format.", type = "error")
    } else if(coords$stop <= coords$start) {
      message <- "Start must be less than stop."
      shinyalert("Invalid Option", message, type = "error")
    } else if(coords$stop - coords$start > 5000000) {
      shinyalert("Invalid Option", "Chromosome region cannot be larger than 5Mb.", type = "error")
    } else if(coords$stop - coords$start > 2000000) {
      shinyalert(
        title = "Warning!",
        text = "The region you selected is > 2Mb. \r This operation may take a while...",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "Continue",
        confirmButtonCol = "#AEDEF4",
        cancelButtonText = "Cancel",
        timer = 0,
        imageUrl = "",
        animation = TRUE,
        callbackR = function(x) {if(x != FALSE) run_script(coords)}
      )
    } else {
      run_script(coords)
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
      setwd("../lure/") # Adjust working directory to find output/all_probes.bed
      system(paste0("Rscript --vanilla ../lure/scripts/reduce_probes.R ", output_folder, " ", max_probes))
      setwd(wd)
      if (is.na(max_probes)){
        wd <- getwd()
        setwd("../lure/") # Adjust working directory to find output/all_probes.bed
        system(paste0("Rscript --vanilla ../lure/scripts/reduce_probes.R ", output_folder))
        setwd(wd)
      }
    }
    data <- as.data.frame(read.delim(paste0(output_folder, "/filtered_probes.bed"), header = F))
    colnames(data) <- c("chr", "start", "stop", "shift", "res.fragment", "dir", "AT", "GC", "seq", "pass")
    data
  })
  
  ##-----------Load in Restriction Sites----------####
  res.sites <- reactive({
    req(script_results())
    ## Load in restriction sites
    res.sites <- read.delim(paste0(output_folder, "/fragments.bed"), header = F)
    sites <- unique(sort(c(res.sites[,2], res.sites[,3])))
    sites <- sites + extractcoords(input$coordinates)$start
    sites
  })
  
  ##------------Load in All Probes--------------####
  all_probes <- reactive({
    all_probes <- read.delim(paste0(output_folder, "/all_probes.bed"), header = F)
    all_probes
  })
  
  ##----------Maximum Number of Probes----------####
  output$max_probes <- renderUI({
    req(input$run_script)
    req(script_results())
    req(all_probes())
    data <- script_results()
    all_probes <- all_probes()
    probes <- nrow(data)
    max_possible <- nrow(all_probes)
    sliderInput(
      inputId = "max_probes",
      label = "Maximum Number of Probes",
      value = ifelse(is.null(input$max_probes), max_possible, input$max_probes),
      min = 1, 
      max = max_possible
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
    gdist <- extractcoords(input$coordinates)$stop - extractcoords(input$coordinates)$start
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
      cols <- c("#71CAE4", "#666EE5", "#FF7F5C", "#FFD770")
      cols <- adjustcolor(cols, alpha.f = 1)
      
      ## Plot stacked histogram barplot
      par(lty = 0, mgp=c(2.75,.7,.2), bg="NA")
      plot(c(0,1), c(0,1), "n",
           xlim = xlim, #c(min(bdata$bin_start), max(bdata$bin_stop)),
           ylim = c(0, 1.5*max(bdata$bins)),
           xlab = "",
           ylab = "",
           frame.plot = F,
           las = 1,
           axes = FALSE)
      
      abline(h=c(2,4,6,8,10,12), col="#d2d9e0", lty = 3)
      
      par(lty = 0, mgp=c(2.75,.7,.2), bg="NA", new = TRUE)
      plot(c(0,1), c(0,1), "n",
           xlim = xlim, #c(min(bdata$bin_start), max(bdata$bin_stop)),
           ylim = c(0, 1.5*max(bdata$bins)),
           xlab = "genomic coordinates",
           ylab = "Number of Probes",
           yaxs='i',
           sub = paste0("N= ", sum(bdata$bins), ", ",
                        "bin size= ", bin_size, "bp"),
           frame.plot = F,
           las = 1,
           axes = FALSE,
           cex.lab = 1.1
           )
      axis(2, 
           lwd = 0,
           lwd.ticks = 0,
           las = 2)
      axis(1, 
           col = "#b8c2cc",
           lwd = 1,
           lwd.ticks = 1,
           tcl = -.4,
           las = 1)
      
      rect(bdata$bin_start, 0, bdata$bin_stop, bdata$bins, border=NA)
      rect(bdata$bin_start, 0, bdata$bin_stop, bdata$`0`, col = cols[1], border=NA)
      rect(bdata$bin_start, bdata$`0`, bdata$bin_stop, bdata$`0`+bdata$`1`, col = cols[2], border=NA)
      rect(bdata$bin_start, bdata$`0`+bdata$`1`, bdata$bin_stop, bdata$`0`+bdata$`1`+bdata$`2`, col = cols[3], border=NA)
      rect(bdata$bin_start, bdata$`0`+bdata$`1`+bdata$`2`, bdata$bin_stop, bdata$`0`+bdata$`1`+bdata$`2`+bdata$`3`, col = cols[4], border=NA)
      legend('topright', title = "Pass Number", adj=1, legend = c(0:3), fill = cols, bg = "#ffffff", border=NA)
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
    gdist <- extractcoords(input$coordinates)$stop - extractcoords(input$coordinates)$start
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
      cols <- c("#71CAE4", "#666EE5", "#FF7F5C", "#FFD770")
      cols <- adjustcolor(cols, alpha.f = 1)
      
      ## Plot stacked histogram barplot
      plot(c(0,1), c(0,1), "n", 
           xlim = xlim, #c(min(bdata$bin_start), max(bdata$bin_stop)),
           ylim = c(0, 1.5*max(bdata$bins)),
           xlab = "",
           ylab = "",
           frame.plot = F,
           las = 1,
           axes = FALSE
      )
      abline(h=c(2,4,6,8,10,12), col="#d2d9e0", lty = 3)
      
      par(lty = 0, mgp=c(2.75,.7,.2), bg="NA", new = TRUE)
      plot(c(0,1), c(0,1), "n", 
           xlim = xlim, #c(min(bdata$bin_start), max(bdata$bin_stop)),
           ylim = c(0, 1.5*max(bdata$bins)),
           xlab = "genomic coordinates",
           ylab = "Number of Restriction Sites",
           yaxs='i',
           sub = paste0("N= ", sum(bdata$bins), ", ",
                        "bin size= ", bin_size, "bp"),
           frame.plot = F,
           las = 1,
           axes = FALSE,
           cex.lab = 1.1
      )
      axis(2, 
           lwd = 0,
           lwd.ticks = 0,
           las = 2)
      axis(1, 
           col = "#b8c2cc",
           lwd = 1,
           lwd.ticks = 1,
           tcl = -.4,
           las = 1)
      
      rect(bdata$bin_start, 0, bdata$bin_stop, bdata$bins, col = "#b8c2cc", border = NA)
    }
    
    ## Implement function
    custom_histogram(sites, breaks = breaks, xlim = c(input$region_slider[1], input$region_slider[2]))
  })
  
  ##--------------GC Content Plot----------------####
  output$gc_plot <- renderPlot({
    data <- script_results()
    sites <- res.sites()
    req(input$region_slider)

    cols <- c("#71CAE4", "#666EE5", "#FF7F5C", "#FFD770")[factor(data$pass)]
    cols <- adjustcolor(cols, alpha.f = 1)
    
    leg_cols <- c("#71CAE4", "#666EE5", "#FF7F5C", "#FFD770")
    leg_cols <- adjustcolor(leg_cols, alpha.f = 1)
    
    par(mgp=c(2.75,.7,.2))
    plot(c(0,1), c(0,1), "n",
         xlim=c(input$region_slider[1], input$region_slider[2]),
         xlab = "",
         ylab = "",
         las = 1,
         frame.plot = F,
         axes = FALSE)
    abline(h=c(.2,.4,.6,.8,1), col="#d2d9e0", lty = 3)
    
    par(mgp=c(2.75,.7,.2), bg="NA", new = TRUE)
    plot(c(0,1), c(0,1), "n",
         xlim=c(input$region_slider[1], input$region_slider[2]),
         xlab = paste0(extractcoords(input$coordinates)$chr, " region"),
         ylab = "GC Fraction",
         yaxs='i',
         las = 1,
         frame.plot = F,
         axes = FALSE,
         cex.lab = 1.1
         )
    axis(2, 
         lwd = 0,
         lwd.ticks = 0,
         las = 2)
    axis(1, 
         col = "#b8c2cc",
         lwd = 1,
         lwd.ticks = 1,
         tcl = -.4,
         las = 1)
    
    segments(data$start, data$GC, data$stop, data$GC, col = cols, lwd=5)
    legend('topright', title = "Pass Number", legend = c(0, 1, 2, 3), fill = leg_cols, bg = "white", box.lty = 0)
    if (input$toggle_res.sites == T){ #input$region_slider[2] - input$region_slider[1] <= 50000 & 
      segments(sites, -1, sites, 0.65)
    }
  })
  
  ##--------------Plotting Shift-----------------####
  output$shift_plot <- renderPlot({
    data <- script_results()
    sites <- res.sites()
    
    cols <- c("#71CAE4", "#666EE5", "#FF7F5C", "#FFD770")[factor(data$pass)]
    cols <- adjustcolor(cols, alpha.f = 1)
    
    leg_cols <- c("#71CAE4", "#666EE5", "#FF7F5C", "#FFD770")
    leg_cols <- adjustcolor(leg_cols, alpha.f = 1)
    
    par(mgp=c(2.75,.7,.2))
    plot(data$start, data$shift, "n", frame.plot = F,
         xlim = c(input$region_slider[1], input$region_slider[2]),
         ylim = c(min(data$shift), max(data$shift)+0.45*(max(data$shift))),
         xlab = "",
         ylab = "",
         las = 1,
         axes = FALSE)
    
    abline(h=c(50,100,150), col="#d2d9e0", lty = 3)
    
    par(mgp=c(2.75,.7,.2), bg="NA", new = TRUE)
    plot(data$start, data$shift, "n", frame.plot = F,
         xlim = c(input$region_slider[1], input$region_slider[2]),
         ylim = c(min(data$shift), max(data$shift)+0.45*(max(data$shift))),
         xlab = paste0(extractcoords(input$coordinates)$chr, " region"),
         ylab = "Base Pairs from Restriction Site",
         yaxs='i',
         las = 1,
         axes = FALSE,
         cex.lab = 1.1
         )
    axis(2, 
         lwd = 0,
         lwd.ticks = 0,
         las = 2)
    axis(1, 
         col = "#b8c2cc",
         lwd = 1,
         lwd.ticks = 1,
         tcl = -.4,
         las = 1)
    
    segments(data$start, data$shift, data$stop, data$shift, col = cols, lwd = 5)
    legend('topright', title = "Pass Number", legend = c(0, 1, 2, 3), fill = leg_cols, bg = "white", box.lty = 0)
    if (input$toggle_res.sites == T){ #input$region_slider[2] - input$region_slider[1] <= 50000 & 
      segments(sites, -1, sites, max(data$shift))
    }
  })
  
  ##-------------Region View Slider--------------####
  
  output$region_slider <- renderUI({
    sliderInput(
      inputId = "region_slider",
      label = "Viewing Region",
      min = extractcoords(input$coordinates)$start,
      max = extractcoords(input$coordinates)$stop,
      value = c(extractcoords(input$coordinates)$start, extractcoords(input$coordinates)$stop),
      step = 10
    )
  })
  
  observeEvent(input$region_slider, {
    if(input$region_slider[2] == input$region_slider[1]){
      updateSliderInput(session, "region_slider", value = c(extractcoords(input$coordinates)$start, extractcoords(input$coordinates)$stop))
      shinyalert("You Can't Do That...", "Viewing range must be greater than 0 base pairs!", type = "error")
    }
  })
  
  
  ##-------------Download Results----------------####
  output$downloadProbes <- downloadHandler(
    filename = function(){
      req(extractcoords(input$coordinates)$chr)
      req(extractcoords(input$coordinates)$start)
      req(extractcoords(input$coordinates)$stop)
      paste0(Sys.Date(), "-",
             extractcoords(input$coordinates)$chr, ":",
             extractcoords(input$coordinates)$start, "-",
             extractcoords(input$coordinates)$stop,
             ".txt")
    },
    content = function(file){
      write(paste0("# Genome: ", input$genome), file, append = T)
      write(paste0("# Chromosome: ", extractcoords(input$coordinates)$chr), file, append = T)
      write(paste0("# Start: ", extractcoords(input$coordinates)$start), file, append = T)
      write(paste0("# Stop: ", extractcoords(input$coordinates)$stop), file, append = T)
      write(paste0("# Restriction Enzyme: ", input$resenz), file, append = T)
      req(input$index)
      if (input$index == "None"){
        write(paste0("# Index Sequence: None"), file, append = T)
      } else if (input$index == "Custom"){
        req(input$custom_index_1)
        req(input$custom_index_2)
        write(paste0("# Custom Index: ",
               input$custom_index_1, " - N120 - ", input$custom_index_2), file, append = T)
      } else if (input$index == "Index1"){
        write(paste0("# Index 1: TCGCGCCCATAACTC - N120 - CTGAGGGTCCGCCTT"), file, append = T)
      } else if (input$index == "Index2"){
        write(paste0("# Index 2: ATCGCACCAGCGTGT - N120 - CACTGCGGCTCCTCA"), file, append = T)
      } else if (input$index == "Index3"){
        write(paste0("# Index 3: CCTCGCCTATCCCAT - N120 - CACTACCGGGGTCTG"), file, append = T)
      }
      req(input$run_script)
      req(script_results())
      data <- script_results()
      probes <- nrow(data)
      region_length <- extractcoords(input$coordinates)$stop - extractcoords(input$coordinates)$start
      density <- probes/region_length*1000
      write(paste0("# Number of Probes: ", probes), file, append = T)
      write(paste0("# Probe Density (probes/kb): ", density), file, append = T)
      write(NULL, file, append = T)
      readr::write_tsv(script_results(), file, append = T)
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
  output$info_genome <- renderText({
    paste0("Genome: ", input$genome)
  })
  output$info_chr <- renderText({
    paste0("Chromosome: ", extractcoords(input$coordinates)$chr)
  })
  output$info_start <- renderText({
    paste0("Start: ", extractcoords(input$coordinates)$start)
  })
  output$info_stop <- renderText({
    paste0("Stop: ", extractcoords(input$coordinates)$stop)
  })
  output$info_resenz <- renderText({
    paste0("Restriction Enzyme: ", input$resenz)
  })
  output$info_index <- renderText({
    req(input$index)
    if (input$index == "None"){
      paste0("Index Sequence: None")
    } else if (input$index == "Custom"){
      req(input$custom_index_1)
      req(input$custom_index_2)
      paste0("Custom Index: ",
             input$custom_index_1, " - N120 - ", input$custom_index_2)
    } else if (input$index == "Index1"){
      paste0("Index 1: TCGCGCCCATAACTC - N120 - CTGAGGGTCCGCCTT")
    } else if (input$index == "Index2"){
      paste0("Index 2: ATCGCACCAGCGTGT - N120 - CACTGCGGCTCCTCA")
    } else if (input$index == "Index3"){
      paste0("Index 3: CCTCGCCTATCCCAT - N120 - CACTACCGGGGTCTG")
    } else {
      print0("Error!")
    }
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
      region_length <- extractcoords(input$coordinates)$stop - extractcoords(input$coordinates)$start
      density <- probes/region_length*1000
      max_density <- max_possible/region_length*1000
      paste0("Maximum Possible: ", max_possible, " probes (", max_density, " probes/kb)" )
    })
    output$info_selected_probes <- renderText({
      req(input$run_script)
      req(script_results())
      data <- script_results()
      probes <- nrow(data)
      region_length <- extractcoords(input$coordinates)$stop - extractcoords(input$coordinates)$start
      density <- probes/region_length*1000
      paste0("Selected: ", probes, " probes (", density, " probes/kb)")
    })
    output$info_probeLength <- renderText({
      req(input$index)
      if(input$index == "None"){
        paste0("Probe Length: 120 bp")
      } else if (input$index == "Custom"){
        req(input$custom_index_1)
        req(input$custom_index_2)
        paste0("Probe Length: ", nchar(input$custom_index_1)+120+nchar(input$custom_index_2), " bp")
      } else {
        paste0("Probe Length: 150 bp")
      }
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
    cols <- c("#FFD770", "#FF7F5C", "#666EE5", "#71CAE4", "#666EE5", "#FF7F5C", "#FFD770")
    cols <- adjustcolor(cols, alpha.f = 1)
    breaks <- nrow(data)*0.05
    if (breaks < 1) breaks <- 1
    h <- hist(data$GC, breaks = breaks, plot = F)
    cuts <- cut(h$breaks, c(-Inf, .25, .35, .49, .65, .7, .8, Inf))
    cols[cuts]
    par(lty = 0, mgp=c(2.75,.7,0.2), bg="NA")
    plot(h, col = cols[cuts], xlim = c(0.2, 0.8),
         main = "GC Content",
         ylab = "Number of Probes",
         xlab = "GC Fraction",
         axes = FALSE,
         yaxs='i',
         ylim = c(0,max(h$counts)),
         cex.main = 1.3,
         cex.lab = 1.1
         )
    abline(h=c(0.2, 0.4, 0.6, 0.8, 1.0) * max(h$counts), col="#eceff3", lty = 1)
    par(lty = 0, mgp=c(2.75,.7,0.2), bg="NA")
    plot(h, col = cols[cuts], xlim = c(0.2, 0.8),
         main = "GC Content",
         ylab = "Number of Probes",
         xlab = "GC Fraction",
         add = TRUE, 
         axes = FALSE,
         ylim = c(0,max(h$counts)),
         )
    axis(2, 
         lwd = 0,
         lwd.ticks = 0,
         las = 2)
    axis(1, 
         col = "#b8c2cc",
         lwd = 1,
         lwd.ticks = 1,
         tcl = -.4,
         las = 1)
    legend('topright', legend = c("0", "1", "2", "3"), bg = "#ffffff",
           fill = adjustcolor(c("#71CAE4", "#666EE5", "#FF7F5C", "#FFD770"), alpha.f = 1),
           title = "Pass")
  })
  
  output$summary_pass <- renderPlot({
    data <- script_results()
    cols <- c("#71CAE4", "#666EE5", "#FF7F5C", "#FFD770")
    cols <- adjustcolor(cols, alpha.f = 1)
    par(lty = 0, mgp=c(2.75,.7,0.2), bg="NA")
    barplot(table(factor(data$pass, levels = 0:3)), col = NA,
            main = "",
            ylab = "",
            xlab = "",
            border = NA,
            xaxt = "n",
            yaxt = "n",
            axes = FALSE,
            yaxs='i',
            ylim = c(0,max(table(factor(data$pass, levels = 0:3))))
            )
    abline(h=c(0.2, 0.4, 0.6, 0.8, 1.0) * max(table(factor(data$pass, levels = 0:3))), col="#eceff3", lty = 1)
    par(lty = 0, mgp=c(2.75,.7,0.2), bg="NA")
    bp=barplot(table(factor(data$pass, levels = 0:3)), col = cols,
            main = "Probe Quality",
            ylab = "",
            xlab = "Pass Number",
            add = TRUE,
            axes = FALSE,
            xaxt = "n",
            yaxs='i',
            ylim = c(0,max(table(factor(data$pass, levels = 0:3)))),
            cex.main = 1.3,
            cex.lab = 1.1
            )
    axis(2, 
         lwd = 0,
         lwd.ticks = 0,
         las = 2)
    axis(1, 
         lwd = 0,
         lwd.ticks = 0,
         at = bp,
         labels = c(0,1,2,3),
         line = 0)
  })
  
  
  output$summary_shift <- renderPlot({
    data <- script_results()
    col_func <- colorRampPalette(c("#71CAE4", "#666EE5", "#FF7F5C", "#FFD770"))
    h <- hist(data$shift, plot = F)
    par(lty = 0, mgp=c(2.75,.7,0.2), bg="NA")
    plot(
      h,
      xlim = c(0, max(data$shift)+20),
      col = adjustcolor(col_func(length(h$breaks)),alpha.f = 1),
      main = "Restriction Site Distance",
      xlab = "Distance (bp)",
      ylab = "",
      las = 1,
      axes = FALSE,
      yaxs='i',
      ylim = c(0,max(h$counts)),
      cex.main = 1.3,
      cex.lab = 1.1
      )
    abline(h=c(0.2, 0.4, 0.6, 0.8, 1.0) * max(h$counts), col="#eceff3", lty = 1)
    par(lty = 0, mgp=c(2.75,.7,0.2), bg="NA")
    plot(
      h,
      xlim = c(0, max(data$shift)+20),
      col = adjustcolor(col_func(length(h$breaks)),alpha.f = 1),
      main = "Restriction Site Distance",
      xlab = "Distance (bp)",
      ylab = "Number of Probes",
      las = 1,
      add = TRUE,
      axes = FALSE,
      yaxs='i',
      ylim = c(0,max(h$counts)))
    axis(2, 
         col = "#b8c2cc",
         lwd = 0,
         lwd.ticks = 0,
         las = 2)
    axis(1, 
         col = "#b8c2cc",
         lwd = 1,
         lwd.ticks = 1,
         tcl = -.4,
         las = 1)
  })
  
  ##--------------Probe data table----------------####
  observe({
    output$Probes <- DT::renderDataTable({
      data <- script_results()
      DT::datatable(data)
    })
  })
  
  session$onSessionEnded(function() {
    if(dir.exists(output_folder)) {
      unlink(output_folder, recursive = T, force = T)
    }
  })
  
}) # END


