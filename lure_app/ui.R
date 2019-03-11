## Dashboard Header ####
header <- dashboardHeader(
  title = "lure",
  disable = TRUE
)

## Dashboard Sidebar ####
sidebar <- dashboardSidebar(
  collapsed = T,
  disable = T,
  sidebarMenu(
    id = "tabNav",
    menuItem("Define", tabName = "Define"),
    menuItem("Evaluate", tabName = "Evaluate"), 
    menuItem("About", tabName = "About"), 
    menuItem("Download", tabName = "Download"), 
    menuItem("Contact", tabName = "Contact")
  )
)

genomes<-read.csv("./../genomes.csv", stringsAsFactors=F)
genomes<-genomes[file.exists(paste0("./../genomes/", basename(sub(".2bit", ".fa", genomes$URL)))), ]
genome.names<-NULL
if(nrow(genomes)>0) {
  genome.names<-paste0(genomes$Organism, ": ", genomes$Genome)
}

## Dashboard Body ####
body <- dashboardBody(
  useShinyalert(),
  shinyjs::useShinyjs(),
  tags$head(
    ## Link in stylesheets ####
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
    
    ## Link in scripts ####
    tags$script(src = "js/main.js"),
    
    # useShinyjs(),
    includeScript("www/js/app-shinyjs.js"),
    includeCSS("www/css/app.css"),
    includeScript("https://cdnjs.cloudflare.com/ajax/libs/jquery-scrollTo/1.4.3/jquery.scrollTo.min.js")
  ),
  
  tags$footer(tags$a(span(img(src="images/lrrr.png", height=32),
                          style="position:fixed; bottom:50px; right:0px"),
                     href="https://futurama.fandom.com/wiki/Lrrr"),
    tags$a(span(img(src="images/PhanstielLab.png", height=16, align="left")),
           href="http://phanstiel-lab.med.unc.edu/"), align="left",
                style="position:fixed;
                bottom:0px;
                width:101%;
                height:40px;
                color:#eceff3;
                padding: 12px 25px 0px 20px;
                margin-left:-20px;
                background-color:#b8c3d4;
                z-index:1000;",
           tags$ul("© 2018 Phanstiel Lab", align="right", style="font-weight:300") # c4c8cc
  ),
  tags$script('
    $(document).on("keyup", function (e) {
                if(e.keyCode == 13) {
                  Shiny.onInputChange("returnPressed", e.which);
                }
              });
              '),
  
  fluidRow( 
    class="nav-margins",
    align="center",
    column(2),
    column(2, class="nav-btn", actionButton(inputId = "return", label = "HOME", class="nav-btn"), id="home"),
    column(2, class="nav-btn", actionButton(inputId = "about", label = "ABOUT", class="nav-btn"), id="about"),
    column(2, class="nav-btn", actionButton(inputId = "download", label = "DOWNLOAD", class="nav-btn"), id="download"),
    column(2, class="nav-btn", actionButton(inputId = "contact", label = "CONTACT", class="nav-btn"), id="contact"),
    column(2)
  ),
  
  tabItems(
    tabItem(
      tabName = "Define",
    
    br(),
    br(),
    br(),
    
    # Add logo
    fluidRow(
      
      align="middle",
      # add image
      img(src='images/Lure_logo_new.png',width=400)
    ),
    br(),
    br(),
    
    
    # Add input row
    fluidRow(
      class="input-row",
      column(width=4, align="center",
             selectInput(
               inputId = "genome",
               label = "Genome",
               choices = genome.names,
               selected = "Human: hg38",
               multiple = F
             )
      ),
      
      # Coordinates
      column(width=4, align="center",
             textInput(
               inputId = "coordinates",
               label = "Target Region",
               value = "chr3:133000000-133100000"
             )
      ),
      
      # Restriction Enzyme
      column(width=4, align="center",
             selectInput(
               inputId = "resenz",
               label = "Restriction Enzyme",
               choices = c("^GATC,MobI", "A^AGCTT,HindIII"),
               selected = "^GATC,MobI",
               multiple = F
             )
      ) # end column
    ), # end fluidRow
    
    fluidRow(
      column(width=12, align="center", 
        textOutput("range")
      )
    ),
    
    fluidRow(
      column(width=12,align="center", style="margin-top:10px",
             tags$a(id = "toggleAdvanced1", h5("Advanced options", icon("caret-down")), href = "#toggleAdvanced1"),
             shinyjs::hidden(
               div(id = "advanced1",
                   tags$hr(),
                   uiOutput("set_max_probes"),
                   h6("(Defaults to maximum available probes)"),
                   radioButtons(width ="100%", 
                                inputId = "index",inline=F,
                                label = "Add index sequences",
                                choices = c("None",
                                            "1: TCGCGCCCATAACTC-N120-CTGAGGGTCCGCCTT" = "Index1",
                                            "2: ATCGCACCAGCGTGT-N120-CACTGCGGCTCCTCA" = "Index2",
                                            "3: CCTCGCCTATCCCAT-N120-CACTACCGGGGTCTG" = "Index3",
                                            "Custom"), 
                                selected = "None"
                   ), # end of radioButtons,
                   conditionalPanel(
                     condition = "input.index == 'Custom'",
                     fluidRow(      
                       class="custom-row",
                       column(
                         width = 3, offset=2,
                         textInput(
                           inputId = "custom_index_1",
                           label = "",
                           placeholder = "start index"
                         ) # end of textInput
                       ), # end of column
                       column(
                         class="n120",
                         width = 1,
                         br(),
                         span("-N120-")
                       ), # end of column
                       column(
                         width = 3, 
                         textInput(
                           inputId = "custom_index_2",
                           label = "",
                           placeholder = "end index"
                         ) # end of textInput
                       ) # end of column
                     ) # end of fluidRow
                   ) # end of conditionalPanel
               )
             )
      ) # end of column
    ),
    
    br(),
    
    # Design Probes ####
    fluidRow(
      column(12,align="middle",
             actionButton(
               inputId = "run_script",
               label = "Design Probes",
               icon = icon("wrench", lib = "font-awesome"),
               class = "design-btn"
             )
      ) # end column
    ), # end of row
    
    
    # Page-load Spinner ####
    fluidRow(
      column(12,
             conditionalPanel(
               condition = "(input.returnPressed || input.run_script > 0) && $('html').hasClass('shiny-busy')",
               
               HTML('

                            <br /><br /><div id="preloader-5">
		<span></span>
                    <span></span>
                    <span></span>
                    </div>
                             <div class = "loading-message-block">
                             <p id="loading-message1" class="loading-message"> Constructing Probes... </p>
                             <p id="loading-message2" class="loading-message"> They are going to be great... </p>
                             </div>
                    ')
               
             ) # end of conditional panel
      ) # end of column
    ), # end of row
    
    
    br(),
    br(),
    br(),
    br()
    
    ), # end of Define tabItem
    
    
    
    
    
    ## "Evaluate" Page ####
    tabItem(
      tabName = "Evaluate",
      
      br(),
      
      fluidRow( 
        column(width = 12, 
          div(id = "borderless-box", 
            box(width = 12, 
              solidHeader = T,
              collapsible = T,
                fluidRow(
                  column(
                        width = 3,
                        box(
                          width = 12,
                          title = "Settings",
                          status = "primary",
                          textOutput("info_genome"),
                          textOutput("info_chr"),
                          textOutput("info_start"),
                          textOutput("info_stop"),
                          textOutput("info_resenz"),
                          textOutput("info_index")
                        )
                      ), # end of column
                    column(
                      width = 3,
                      box(
                        width = 12,
                        title = "Results",
                        status = "primary",
                        textOutput("info_res.sites"),
                        textOutput("info_all_probes"),
                        textOutput("info_selected_probes"),
                        textOutput("info_probeLength"),
                        textOutput("info_avgGC")
                      )
                    ), # end of column
                  column(
                      width = 4,
                      uiOutput("max_probes")
                    ), # end of column
                  column(
                    width = 2,
                    align = "right",
                    downloadButton(
                      outputId = "downloadProbes",
                      label = "Download"
                    )
                  ) # end of column
                ) # end of fluidRow
              ) # end of box
            ), # end of div
        
          tags$br(),
          
          tabBox( 
            title = uiOutput("title"),
            side = "right",
            width = 12,
            id = "tab_view",
            
            tabPanel(
              title="Region View",
              fluidRow(
                column(
                  width = 12,
                  
                  ## Region view slider ####
                  box(
                    width = 12,
                    uiOutput("region_slider")
                  ),
                  
                  ## Histogram of Probe Coverage ####
                  box(
                    title = "Histogram of Probe Coverage",
                    width = 12,
                    collapsible = T,
                    plotOutput("coverageHistogram")
                  )
                ), # end of column
                
                column(
                  width = 12,
                  ## Histogram of Restriction Sites ####
                  box(
                    title = "Histogram of Restriction Sites",
                    width = 12,
                    collapsible = T,
                    plotOutput("restrictionHistogram")
                  )
                ), # end of column
                
                div(id = "borderless-box", 
                    box(
                      width = 12, 
                      solidHeader = T,
                      materialSwitch(
                        inputId = "toggle_res.sites",
                        label = "Show Restriction Sites?",
                        value = F,
                        status = "primary",
                        right = F
                      )
                    )
                ),
                
                column(
                  width = 12,
                  ## Plot of GC Content ####
                  box(
                    title = "GC Content and Probe Quality",
                    width = 12,
                    collapsible = T,
                    plotOutput("gc_plot")
                  )
                ), # end of column
                
                column(
                  width = 12,
                  ## Plot of Shift ####
                  box(
                    title = "Distance from Restriction Site",
                    width = 12,
                    collapsible = T,
                    plotOutput("shift_plot")
                  )
                ) # end of column
              ) # end of fluidRow
            ), # end of Region View tabPanel
            
            ## Summary View ####
            tabPanel(
              title="Summary View",

              ## Summary View Plots ####
              fluidRow( id="plot-margin",
                column(width = 12, 
                  box(
                    width = 4,
                    plotOutput("summary_gc")
                  ), # end of box
                  box(
                    width = 4,
                    plotOutput("summary_pass")
                  ), # end of box
                  box(
                    width = 4,
                    plotOutput("summary_shift")
                  ) # end of box
                ) # end of column
              ), # end of fluidRow
              fluidRow( id="tab-margin",
                column(
                  width = 12,
                  ## Probe Data Table ####
                  box(
                    title = "Probe Data Table",
                    status = "success",
                    width = 12,
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('Probes')),
                    collapsible = T
                  )
                ) # end of column
              ) # end of fluidRow
            ) # end of Summary View tabPanel
          ) # end of tabBox
        ) # end of column
      ), # end of fluidRow
      br(),
      br()
    ), # end of tabItem
    
    tabItem(tabName = "About",
            ##### About Page ####
            fluidPage(
              # tab 1 contains 4 sections and a scrollspy on the left with text
              tabPanel(
                "tab1",
                div(id = "tab1-content",
                    fluidRow(
                      column(
                        width=2,
                        offset = 1,
                        div(
                          id = "tab1-scrollspy",
                          class = "active-scrollspy",
                          tags$ul(
                            class = "nav nav-pills nav-stacked",
                            tags$li(tags$a(class="test", href = "#section1-1", "Section 1-1")),
                            tags$li(tags$a(href = "#section1-2", "Section 1-2")),
                            tags$li(tags$a(href = "#section1-3", "Section 1-3")),
                            tags$li(tags$a(href = "#section1-4", "Section 1-4"))
                          )
                        )
                      ),
                      column(
                        width = 6,
                        div(id = "section1-1",
                            class = "scrollspy-section",
                            h1("This is the about page (Placeholder Text)"),
                            h4("Long-range interactions between genomic regions are important mediators of gene expression. These contacts, or DNA loops, allow linearly distant cis- or trans-regulatory elements to be brought into close proximity to their effector genes. Many techniques have been devised to explore these distant genomic interactions. The first technique developed, chromosome conformation capture (3C), only allowed for investigation of interactions between pairs of loci. Newer methods, such as Hi-C, allow for genome-wide identification of chromatin interactions by combining 3C methods with high-throughput sequencing."),
                            br(),
                            h4("While Hi-C can generate a genome-wide map of contact frequencies, observations of genomic features are limited by the base-pair resolution. Sequencing deep enough to achieve the resolution necessary to see genome-wide loops (5 Kb) is prohibitively expensive. In 2015, Sanborn and Rao et al. developed an inexpensive, region-targeted method for Hi-C that they termed hybrid-capture Hi-C (Hi-C2). Rather than sequencing an entire Hi-C library, Hi-C2 allows for the selection of a targeted genomic region though enrichment with hybridization probes. ")
                            
                        ),
                        div(id = "section1-2",
                            class = "scrollspy-section",
                            h1("Lets Create More Fake Output"),
                            h4("Long-range interactions between genomic regions are important mediators of gene expression. These contacts, or DNA loops, allow linearly distant cis- or trans-regulatory elements to be brought into close proximity to their effector genes. Many techniques have been devised to explore these distant genomic interactions. The first technique developed, chromosome conformation capture (3C), only allowed for investigation of interactions between pairs of loci. Newer methods, such as Hi-C, allow for genome-wide identification of chromatin interactions by combining 3C methods with high-throughput sequencing."),
                            br(),
                            h4("While Hi-C can generate a genome-wide map of contact frequencies, observations of genomic features are limited by the base-pair resolution. Sequencing deep enough to achieve the resolution necessary to see genome-wide loops (5 Kb) is prohibitively expensive. In 2015, Sanborn and Rao et al. developed an inexpensive, region-targeted method for Hi-C that they termed hybrid-capture Hi-C (Hi-C2). Rather than sequencing an entire Hi-C library, Hi-C2 allows for the selection of a targeted genomic region though enrichment with hybridization probes. "),
                            br(),
                            h4("While Hi-C can generate a genome-wide map of contact frequencies, observations of genomic features are limited by the base-pair resolution. Sequencing deep enough to achieve the resolution necessary to see genome-wide loops (5 Kb) is prohibitively expensive. In 2015, Sanborn and Rao et al. developed an inexpensive, region-targeted method for Hi-C that they termed hybrid-capture Hi-C (Hi-C2). Rather than sequencing an entire Hi-C library, Hi-C2 allows for the selection of a targeted genomic region though enrichment with hybridization probes. "),
                            br(),
                            img(src = "images/HiC2_IL1B_Norm.png", align = "center", height="100%", width="100%"),
                            br(),
                            br(),
                            br()
                        ),
                        div(id = "section1-3",
                            class = "scrollspy-section",
                            h1("Section 3 Output"),
                            h4("Long-range interactions between genomic regions are important mediators of gene expression. These contacts, or DNA loops, allow linearly distant cis- or trans-regulatory elements to be brought into close proximity to their effector genes. Many techniques have been devised to explore these distant genomic interactions. The first technique developed, chromosome conformation capture (3C), only allowed for investigation of interactions between pairs of loci. Newer methods, such as Hi-C, allow for genome-wide identification of chromatin interactions by combining 3C methods with high-throughput sequencing."),
                            br(),
                            h4("While Hi-C can generate a genome-wide map of contact frequencies, observations of genomic features are limited by the base-pair resolution. Sequencing deep enough to achieve the resolution necessary to see genome-wide loops (5 Kb) is prohibitively expensive. In 2015, Sanborn and Rao et al. developed an inexpensive, region-targeted method for Hi-C that they termed hybrid-capture Hi-C (Hi-C2). Rather than sequencing an entire Hi-C library, Hi-C2 allows for the selection of a targeted genomic region though enrichment with hybridization probes. "),
                            br(),
                            h4("While Hi-C can generate a genome-wide map of contact frequencies, observations of genomic features are limited by the base-pair resolution. Sequencing deep enough to achieve the resolution necessary to see genome-wide loops (5 Kb) is prohibitively expensive. In 2015, Sanborn and Rao et al. developed an inexpensive, region-targeted method for Hi-C that they termed hybrid-capture Hi-C (Hi-C2). Rather than sequencing an entire Hi-C library, Hi-C2 allows for the selection of a targeted genomic region though enrichment with hybridization probes. "),
                            br(),
                            img(src = "images/HiC2_IL1B_Norm.png", align = "center", height="100%", width="100%"),
                            br(),
                            br(),
                            br()
                        ),
                        div(id = "section1-4",
                            class = "scrollspy-section",
                            h1("Section 4"),
                            h4("Long-range interactions between genomic regions are important mediators of gene expression. These contacts, or DNA loops, allow linearly distant cis- or trans-regulatory elements to be brought into close proximity to their effector genes. Many techniques have been devised to explore these distant genomic interactions. The first technique developed, chromosome conformation capture (3C), only allowed for investigation of interactions between pairs of loci. Newer methods, such as Hi-C, allow for genome-wide identification of chromatin interactions by combining 3C methods with high-throughput sequencing."),
                            br(),
                            h4("While Hi-C can generate a genome-wide map of contact frequencies, observations of genomic features are limited by the base-pair resolution. Sequencing deep enough to achieve the resolution necessary to see genome-wide loops (5 Kb) is prohibitively expensive. In 2015, Sanborn and Rao et al. developed an inexpensive, region-targeted method for Hi-C that they termed hybrid-capture Hi-C (Hi-C2). Rather than sequencing an entire Hi-C library, Hi-C2 allows for the selection of a targeted genomic region though enrichment with hybridization probes. "),
                            br(),
                            h4("While Hi-C can generate a genome-wide map of contact frequencies, observations of genomic features are limited by the base-pair resolution. Sequencing deep enough to achieve the resolution necessary to see genome-wide loops (5 Kb) is prohibitively expensive. In 2015, Sanborn and Rao et al. developed an inexpensive, region-targeted method for Hi-C that they termed hybrid-capture Hi-C (Hi-C2). Rather than sequencing an entire Hi-C library, Hi-C2 allows for the selection of a targeted genomic region though enrichment with hybridization probes. "),
                            br(),
                            img(src = "images/HiC2_IL1B_Norm.png", align = "center", height="100%", width="100%"),
                            br(),
                            br(),
                            br()
                        )              
                      )
                    )
                )
              )
            )
            ##### End About Page #####
    ),
    tabItem(tabName = "Download", 
      h3("Download")
    ),
    tabItem(tabName = "Contact"
    ) # end of tabItem
  ) # end of tabItems
) # end of body

## Dashboard Page ####
dashboardPage(header, sidebar, body)
