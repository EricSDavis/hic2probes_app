library(shiny)
library(shinydashboard)
library(shinyalert)
library(DT)

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

          ## Number of Probes ####
          numericInput(
            inputId = "max_probes",
            label = "Number of Probes:",
            min = 0,
            value = NA
          ),
          
          ## Design Probes ####
          div(
            class = "rightAlign",
            ## Tie return key to run_script button
            tags$script('
                        $(document).keyup(function(event) {
                          if (event.keyCode == 13) {
                             $("#run_script").click();
                        }
                        });
                        '),
            # actionButton(
            #   inputId = "cancel_script",
            #   label = "Cancel",
            #   icon = icon("times", lib = "font-awesome")
            # ),
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
          width = 12,
        
          ## Return to Start Page ####
          actionButton(
            inputId = "return",
            label = "Start-Over",
            icon = icon("arrow-left", lib = "font-awesome")
          ), # end of actionButton
          numericInput(
            inputId = "max_probes2",
            label = "Adjust Probe Number:",
            value = NA,
            min = 0
          )
        ) # end of column
      ), # end of fluidRow
      
      tags$br(),
      
      
      tabBox(
        title = uiOutput("title"),
        side = "right",
        width = 12,
        
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
            ), # end of column
            
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

        ), # end of Region View tabPanel
        
        ## Summary View Plots ####
        tabPanel(
          title="Summary View",
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
          ) # end of fluidRow
        ) # end of Summary View tabPanel
      ) # end of tabBox
    ) # end of tabItem
  ) # end of tabItems
) # end of body

## Dashboard Page ####
dashboardPage(header, sidebar, body)