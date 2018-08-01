
## Dashboard Header ####
header <- dashboardHeader(
  title = "LURE"
)

## Dashboard Sidebar ####
sidebar <- dashboardSidebar(
  collapsed = T,
  disable = T,
  sidebarMenu(
    id = "tabNav",
    menuItem("Define", tabName = "Define"),
    menuItem("Evaluate", tabName = "Evaluate")
  )
)

## Dashboard Body ####
body <- dashboardBody(
  useShinyalert(),
  shinyjs::useShinyjs(),
  
  ## Link in stylesheets ####
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
  
  ## Pages ####
  tabItems(
    
    ## "Define" Page ####
    tabItem(
      tabName = "Define",

      ## Create Sidebar Menu ####
      fluidRow(
        box(
          class = "sidebar",
          solidHeader = T,
          title = "DEFINE REGION",
          width = 4,
          
          ## Genome Input ####
          selectInput(
            inputId = "genome",
            label = "Genome:",
            choices = list("Human" = c("hg38", "hg19", "hg18"),
                           "Mouse" = c("GRCm38/mm10", "NCBI37/mm9"),
                           "Rat" = c("RGSC 6.0/rn6", "RGSC 5.0/rn5"),
                           "Fruitfly" = c(),
                           "Worm" = c(),
                           "Yeast" = c()
                           ),
            selected = "hg19",
            multiple = F
          ),
          
          ## Chromosome Input ####
          selectInput(
            inputId = "chr",
            label = "Chromosome:",
            choices = c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6",
                        "chr7", "chr8", "chr9", "chr10", "chr11", "chr12",
                        "chr13", "chr14", "chr15", "chr16", "chr17", "chr18",
                        "chr19", "chr20", "chr21", "chr22", "chrX", "chrY", "chrM"),
            selected = "chr8",
            multiple = F
          ),
          
          ## Start Input ####
          numericInput(
            inputId = "start",
            label = "Start:",
            value = "133000000"
          ),
          
          ## Stop Input ####
          numericInput(
            inputId = "stop",
            label = "Stop:",
            value = "133100000"
          ),
          
          ## Restriction Enzyme Input ####
          selectInput(
            inputId = "resenz",
            label = "Restriction Enzyme:",
            choices = c("^GATC,MobI", "A^AGCTT,HindIII"),
            selected = "^GATC,MobI",
            multiple = F
          ),
          
          a(id = "toggleAdvanced1", p("Advanced options", icon("caret-down")), href = "#toggleAdvanced1"),
          shinyjs::hidden(
            div(
              id = "advanced1",
              radioButtons(
                inputId = "index",
                label = "Add index sequences:",
                choices = c("Index 1: TCGCGCCCATAACTC-N120-CTGAGGGTCCGCCTT" = "Index1",
                            "Index 2: ATCGCACCAGCGTGT-N120-CACTGCGGCTCCTCA" = "Index2",
                            "Index 3: CCTCGCCTATCCCAT-N120-CACTACCGGGGTCTG" = "Index3",
                            "None",
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
            ) # end of div
          ), # end of shinyjs::hidden

          
          ## Design Probes ####
          div(
            class = "rightAlign",
            ## Tie return key to run_script button this doesn't work correctly
            # tags$script('
            #             $(document).keyup(function(event) {
            #               if(event.keyCode == 13) {
            #                  $("#run_script").click();
            #             }
            #             });
            #             '),
            actionButton(
              inputId = "run_script",
              label = "Design Probes",
              icon = icon("wrench", lib = "font-awesome")
            )
          ) # end of div
        ), # end of box
        
        box(
          class = "main-panel",
          solidHeader = T,
          #title = "LURE: A Probe Design Tool for Fishing Hi-C Data",
          width = 8,
          height = 527,
          
          h1("LURE"),
          h3("A Probe Design Tool for Fishing Hi-C Data")
          
        ), # end of column
        
        ## Page-load Spinner ####
        conditionalPanel(
          condition = "input.run_script > 0 && $('html').hasClass('shiny-busy')",
          
            HTML('
             <div class="loader"></div>
                 <p id="loading-message1" class="loading-message"> Constructing Probes... </p>
                 <p id="loading-message2" class="loading-message"> They are going to be great... </p>
                 ')

        ) # end of conditional panel
      ) # end of fluidRow
    ), # end of tabItem
    
    
    ## "Evaluate" Page ####
    tabItem(
      tabName = "Evaluate",
      
      fluidRow(
        column(
          width = 6,
          ## Return to Start Page ####
          actionButton(
            inputId = "return",
            label = "Start-Over",
            icon = icon("arrow-left", lib = "font-awesome")
          ) # end of actionButton
        ), # end of column
        column(
          width = 6,
          fixedPanel(
            class = "options",
            dropdownButton(
              size = "sm",
              circle = T,
              icon = icon("gear"),
              width = 350,
              right = T,
              h3("Options", style="font-weight:300;"),
              fluidRow(
                width = 12,
                column(
                  width = 6,
                  uiOutput("max_probes")
                ), # end of column
                column(
                  width = 6,
                  uiOutput("probe_density")
                ) # end of column
              ), # end of fluidRow
              materialSwitch(
                inputId = "toggle_res.sites",
                label = "Show Restriction Sites?",
                value = F,
                status = "primary",
                right = F
              ),
              dropdown(
                circle = F,
                size = "sm",
                label = "Download",
                inputId = "Download_menu",
                tooltip = tooltipOptions(title = "Select which data to download"),
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
            ) # end of dropdownButton
          ) # end of fixedPanel
        ) # end of column
      ), # end of fluidRow
      
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
            column(
              width = 4,
              plotOutput("summary_gc")
            ), #end of column
            column(
              width = 4,
              plotOutput("summary_pass")
            ), #end of column
            column(
              width = 4,
              plotOutput("summary_shift")
            ) #end of column
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
    ) # end of tabItem
  ) # end of tabItems
) # end of body

## Dashboard Page ####
dashboardPage(header, sidebar, body)