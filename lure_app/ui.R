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
    tags$script(src = "js/main.js")
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
                background-color:#b8c2cc;
                z-index:1000;",
           tags$ul("© 2018 Phanstiel Lab", align="right", style="font-weight:300") # c4c8cc
  ),
  
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
      column(width=12,align="center", style="margin-top:10px",
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
               condition = "input.run_script > 0 && $('html').hasClass('shiny-busy')",
               
               HTML('

                            <div id="preloader-5">
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
                fluidRow(
                  column(
                    width = 6,
                    uiOutput("max_probes")
                  ), # end of column
                  column(
                    width = 2,
                    align = "left",
                    downloadButton(
                      outputId = "downloadProbes",
                      label = "Download Probes"
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
              fluidRow( style = "margin-top:5px",
                column(
                  width = 6,
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
                  width = 6,
                  box(
                    width = 12,
                    title = "Results",
                    status = "primary",
                    textOutput("info_res.sites"),
                    textOutput("info_all_probes"),
                    textOutput("info_selected_probes"),
                    textOutput("info_probeLength"),
                    textOutput("info_avgGC"),
                    br()
                  )
                ) # end of column
              ), # end of fluidRow
              
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
      h3("About")
    ),
    tabItem(tabName = "Download", 
      h3("Download")
    ),
    tabItem(tabName = "Contact", 
      h3("Contact")
    ) # end of tabItem
  ) # end of tabItems
) # end of body

## Dashboard Page ####
dashboardPage(header, sidebar, body)
