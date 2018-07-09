library(shiny)
library(shinydashboard)

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
          status = "primary",
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
            actionButton(
              inputId = "run_script",
              label = "Design Probes",
              icon = icon("wrench", lib = "font-awesome")
            )
          ) # end of div
          
        ), # end of box
        div(id="spinner"),
        conditionalPanel(
          condition = "input.run_script",
          HTML('
             <div class="loader"></div>
               ')
        )
        
      ) # end of fluidRow
      
    ), # end of tabItem
    
    
    ## "Evaluate" Page ####
    tabItem(
      tabName = "Evaluate",
      h2("This is the Evaluate Page"),
      
      ## Return to Start Page ####
      actionButton(
        inputId = "return",
        label = "Start-Over",
        icon = icon("arrow-left", lib = "font-awesome")
      ),
      
      tags$br(),
      tags$br(),
      
      ## Probe Data Table ####
      dataTableOutput("Probes"),
      
      textOutput("test")
      
    ) # end of tabItem
    
  ) # end of tabItems
  
) # end of body

## Dashboard Page ####
dashboardPage(header, sidebar, body)