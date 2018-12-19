## Dashboard Header ####
header <- dashboardHeader(
  title = "LURE",
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

## Dashboard Body ####
body <- dashboardBody(
  useShinyalert(),
  shinyjs::useShinyjs(),
  tags$head(
    ## Link in stylesheets ####
    # tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
    
    ## Link in scripts ####
    tags$script(src = "js/main.js"),
    tags$style(HTML("@media (min-width: 768px) {
                             #home {text-align: left; margin-left:-.8%}
                             #about {text-align: center; padding-right: 4%}
                             #download {text-align: center;}
                             #contact {text-align: right; padding-left: 4.85%}
                              .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                             }
                             #borderless-box div { -webkit-box-shadow: none; -moz-box-shadow: none; box-shadow: none; }"
    )
    )
  ),
  
  tags$footer(tags$a(span(img(src="images/PhanstielLab.png",height=16,align="left", style="")), href="http://phanstiel-lab.med.unc.edu/"), align = "left", style = "
                       position: fixed;
                       bottom:0;
                       width:101%;
                       height:40px;
                       color: #eceff3;
                       padding: 12px 25px 0px 20px;
                       margin-left: -20px;
                       background-color: #b8c2cc;
                       z-index: 1000;",
              tags$ul("© 2018 Phanstiel Lab", align="right", style="font-weight:300") # c4c8cc
  ),
  
  fluidRow(
    
    align="center",
    column(4),
    column(1, actionLink(inputId = "return", label = "Home"), id="home"),
    column(1, actionLink(inputId = "about", label = "About"), id="about"),
    column(1, actionLink(inputId = "download", label = "Download"), id="download"),
    column(1, actionLink(inputId = "contact", label = "Contact"), id="contact")
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
    img(src='images/lure_logo_current.png',width=400)
  ),
  br(),
  br(),
  
  
  # Add input row
  fluidRow(
    column(width=4, align="center",
           selectInput(
             inputId = "genome",
             label = "Genome",
             choices = paste0(genomes$Organism, ": ", genomes$Genome),
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
    column(width=12,align="center",
           radioButtons(width ="100%",
                        inputId = "index",inline=F,
                        label = "Add index sequences",
                        choices = c("None",
                                    "Index 1: TCGCGCCCATAACTC-N120-CTGAGGGTCCGCCTT" = "Index1",
                                    "Index 2: ATCGCACCAGCGTGT-N120-CACTGCGGCTCCTCA" = "Index2",
                                    "Index 3: CCTCGCCTATCCCAT-N120-CACTACCGGGGTCTG" = "Index3",
                                    "Custom"),
                        selected = "None"
           ), # end of radioButtons,
           conditionalPanel(
             condition = "input.index == 'Custom'",
             fluidRow(
               column(
                 width = 5,
                 textInput(
                   inputId = "custom_index_1",
                   label = "",
                   placeholder = "start index"
                 ) # end of textInput
               ), # end of column
               column(
                 width = 2,
                 br(),
                 span("-120-")
               ), # end of column
               column(
                 width = 5,
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
             icon = icon("wrench", lib = "font-awesome")
           )
    ) # end column
  ), # end of row
  
  
  # Page-load Spinner ####
  fluidRow(
    column(12,
           conditionalPanel(
             condition = "input.run_script > 0 && $('html').hasClass('shiny-busy')",
             
             HTML('
                           <div class="loader"></div>
                           <p id="loading-message1" class="loading-message"> Constructing Probes... </p>
                           <p id="loading-message2" class="loading-message"> They are going to be great... </p>
                           ')
             
           ) # end of conditional panel
    ) # end of column
  ), # end of row
  
  
  br(),
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
            title = "Refine probe design",
            solidHeader = T,
              fluidRow(
                column(
                  width = 4,
                  uiOutput("max_probes")
                ), # end of column
                column(
                  width = 4,
                  uiOutput("probe_density")
                ) # end of column
              ), 
              fluidRow(
                column(
                  width = 4,
                  dropdown(
                    right = F, 
                    circle = F,
                    size = "sm",
                    label = "Download",
                    inputId = "Download_menu",
                    downloadLink(
                      outputId = "downloadProbes",
                      label = "Download Probes"
                    ),
                    br(),
                    downloadLink(
                      outputId = "downloadPlots",
                      label = "Download Plots"
                    ),
                    br(),
                    downloadLink(
                      outputId = "downloadAll",
                      label = "Download All"
                    )
                  ) # end of dropdown
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
            fluidRow(
              column(
                width = 6,
                box(
                  width = 12,
                  h1("Settings"),
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
                  h1("Results"),
                  textOutput("info_res.sites"),
                  textOutput("info_all_probes"),
                  textOutput("info_selected_probes"),
                  textOutput("info_probeLength"),
                  textOutput("info_avgGC")
                )
              ) # end of column
            ), # end of fluidRow
            
            ## Summary View Plots ####
            fluidRow(
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
            fluidRow(
              column(
                width = 12,
                ## Probe Data Table ####
                box(
                  title = "Probe Data Table",
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
