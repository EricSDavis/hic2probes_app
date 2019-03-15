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
    ## Favicon ####
    tags$link(rel = "icon", type = "image/ico", href = "images/favicon.ico"),
    
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
                            tags$li(tags$a(href = "#section1-1", "Introduction")),
                            tags$li(tags$a(href = "#section1-2", "Quickstart")),
                            tags$li(tags$a(href = "#section1-3", "How to Cite")),
                            tags$li(tags$a(href = "#section1-4", "Advanced Options")),
                            tags$li(tags$a(href = "#section1-5", "How it Works")),
                            tags$li(tags$a(href = "#section1-6", "Validation")),
                            tags$li(tags$a(href = "#section1-7", "FAQs"))
                          )
                        )
                      ),
                      column(
                        width = 6,
                        div(
                          class="aboutPageContent",
                          div(id = "section1-1",
                              class = "scrollspy-section",
                              div(
                                id = "aboutHeader",
                                h1("Introducing Lure"),
                                h2("A probe design tool for fishing Hi-C data"),
                                br()
                              ),
                              HTML(
                                "
                                <h4>
                                  Lure is a fast, user-friendly web application for designing oligonucleotide probes for hybrid capture Hi-C (Hi-C<sup>2</sup>). Developed by the <a href='http://aidenlab.org/' target='_blank'> Aiden Lab </a> , Hi-C<sup>2</sup> allows scientists to enrich Hi-C data in a small, continuous genomic region <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4664323/' target='_blank'>(Sanborn et al. 2015)</a>. Lure is tailored specifically to Hi-C<sup>2</sup>, designing the optimal probes for Hi-C library enrichment.
                                </h4>
                                "
                              )
                          ),
                          div(id = "section1-2",
                              class = "scrollspy-section",
                              h1("Quick Start"),
                              h4("Designing probes with Lure is fast, easy, and doesn't require downloads or dependencies. To get started:"),
                              tags$ol(
                                tags$li("Select a genome (ex: Human: hg38)"),
                                tags$li("Enter your target region (ex: chr3:133,000,000-133,100,000)"),
                                tags$li("Choose your Restriction Enzyme (ex: ^GATC,MobI)"),
                                tags$li("Design Probes!")
                              ),
                              tags$br(),
                              tags$br(),
                              img(src = "images/quickstart.gif", align = "center", height="100%", width="100%"),
                              tags$br(),
                              tags$br()
                          ),
                          div(id = "section1-3",
                              class = "scrollspy-section",
                              h1("How to Cite"),
                              HTML(
                                "
                                  <h4>
                                    Lure was developed in the <a href='http://phanstiel-lab.med.unc.edu/' target='_blank'> Phanstiel Lab </a> at UNC by <a href='https://ericscottdavis.com/' target='_blank'>Eric Davis</a>, Erika Deoudes, Craig Wenger, and Doug Phanstiel.
                                    <br></br>
                                    If you use this webapp to design your probes, please cite:
                                    <br></br>
                                    <div style='background: #E2E2E2; padding:10px'>
                                      Davis ES, Deoudes EM, Thulson EA, Kramer N, Wenger C and Phanstiel DH. Lure: A Probe Design Tool for Hybrid-Capture Hi-C. <i>Super Awesome Journal</i>. 2019.
                                    </div>
                                  </h4>
                                "
                              )
                          ),
                          div(id = "section1-4",
                              class = "scrollspy-section",
                              h1("Advanced options"),
                              HTML(
                                "
                                <h4>
                                  Lure offers advanced options for adjusting the number of desired probes and adding predefined or custom index sequences to the start and end of each probe sequence. This allows for pooling of probes to reduce cost.
                                </h4>
                                <img class='centered' src='images/advancedOptions.png' align='center' height='80%' width='80%'></img>
                                "
                              )
                          ),
                          div(id = "section1-5",
                              class = "scrollspy-section",
                              h1("How it Works"),
                              HTML(
                                "
                                <h3> Implementation </h3>
                                <h4>
                                  The Lure web application is a Shiny R implementation of the <a href='https://github.com/PhanstielLab/lure'>Lure command line tool</a>, which is written primarily using BASH and R. It utilizes several command line tools to identify and fragment the region of interest (ROI) by restriction site (<a href='https://www.bioinformatics.babraham.ac.uk/projects/hicup/'>HiCUP - Hi-C User Pipeline</a>), extract genomic fragments (<a href='http://bedtools.readthedocs.io/en/latest/'>Bedtools</a>), and assess potential probes in parallel (<a href='https://www.gnu.org/software/parallel/'>GNU Parallel</a>). The diagram below outlines LURE's workflow:
                                </h4>
                                <img class='centered' src='images/flow_diagram2.png' align='center' height='80%' width='80%'></img>

                                <h3> Selection </h3>
                                <h4>
                                  Criteria for selecting probes were modeled after Sanborn et al. (2015) where they outline their Hi-C<sup>2</sup> method. Briefly, potential probes were identified upstream and downstream of each restriction site. All potential probes were scored for repetitive bases, GC content, and distance from restriction site. Each probe was assigned a \"Pass Number\" (0-3) for overall quality according to the scheme below:
                                </h4>
                                <img class='centered' src='images/selectionTable.png' align='center' height='100%' width='100%'></img>
                                <br></br>
                                <h4>
                                  Selection criteria relax as pass number increases, making the highest quality probes (passing the most restrictive criteria) pass 0 and lowest quality probes (passing the least strict criteria) pass 3.
                                </h4>

                                
                                <h3> Filtering </h3>
                                <h4>
                                  By default all suitable probes are shown for every restriction site. However, users may wish to reduce the number of probes ordered to reduce cost or achieve a specific probe density. The web application allows users to specify the maximum number of probes desired before designing probes, or adjust them with a slider after designing probes. Probes are removed (pruned) from the pool by quality score to ensure that those remaining are the highest quality available. If all remaining probes are of the highest quality, they are randomly removed to preserve even coverage. For reproduciblilty, a seed is set in the program so that the same settings will always return the same probes.
                                </h4>
                                "
                              )
                          ),
                          div(id = "section1-6",
                              class = "scrollspy-section",
                              h1("Validation"),
                              HTML(
                                "
                                <h4>
                                  To validate probes designed by Lure, we performed Hi-C<sup>2</sup> on several Hi-C libraries. The preliminary results show XXX enrichment of the target region when compared to genome-wide Hi-C. Below are representative images of the same genomic region with Hi-C (<i>left</i>) and Hi-C<sup>2</sup> (<i>right</i>):
                                </h4>
                                <img class='centered' src='images/HiC2_IL1B_Norm.png' align='center' height='100%' width='100%'></img>
                                <br></br>
                                <h4>
                                  Both maps look very similar in the target region. However, the Hi-C map (<i>left</i>), used 990 million reads while the Hi-C<sup>2</sup> map (<i>right</i>) used only 12 million reads.
                                </h4>
                                "
                              )
                              
                          ),
                          div(id = "section1-7",
                              class = "scrollspy-section",
                              h1("FAQs"),
                              HTML(
                                "
                                <h3>
                                  What kinds of questions should we put on here?
                                </h3>
                                <blockquote>
                                  I am not really sure. Let's ask the lab what they think...
                                </blockquote>
                                "
                              ),
                              
                              br(),
                              br(),
                              br(),
                              br()
                          ) 
                        )
                        
                      )
                    )
                )
              )
            )
            ##### End About Page #####
    ),
    tabItem(tabName = "Download",
            ##### Download Page ####
            fluidPage(
              tabPanel(
                "tab2",
                div(id = "tab2-content",
                    fluidRow(
                      column(
                        width = 6,
                        offset = 3,
                        div(
                          class="aboutPageContent",
                          div(id = "section2-1",
                              class = "scrollspy-section",
                              div(
                                id = "aboutHeader",
                                br(),
                                br(),
                                h1("Download Lure"),
                                h2("Available as a command-line interface or Shiny web application"),
                                br()
                              ),
                              HTML(
                                "
                                <h4>
                                  Lure is available as both a command-line interface, and as a Shiny web application. The easiest way to use Lure is through this website. For programmatic access and piping through other command line tools, use Lure command line. For further webapp development, fork the github repo in the links shown below. 
                                </h4>
                                "
                    ),
                    fluidRow(
                      column(
                        width = 6,
                        h2("Lure Command Line", align="center"),
                        HTML('<a href="https://github.com/PhanstielLab/lure" target="_blank"><img src="images/commandLineLogo.svg" width="60%" height="60%" class="centered"> </img></a>')
 
                      ),
                      column(
                        width = 6,
                        h2("Lure Web Application", align="center"),
                        HTML('<a href="https://github.com/PhanstielLab/lure_app" target="_blank"><img src="images/Lure_logo_new.png" width="100%" height="100%" class="centered"> </img></a>')
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      ##### End Download Page #####
    ),
    tabItem(tabName = "Contact",
            ##### Contact Page ####
            fluidPage(
              tabPanel(
                "tab3",
                div(id = "tab3-content",
                    fluidRow(
                      column(
                        width = 6,
                        offset = 3,
                        div(
                          class="aboutPageContent",
                          div(id = "section3-1",
                              class = "scrollspy-section",
                              div(
                                id = "aboutHeader",
                                br(),
                                br(),
                                h1("Contact Us"),
                                h2("Phanstiel Lab, The University of North Carolina at Chapel Hill"),
                                br()
                              ),
                              HTML(
                                "
                                <h4>
                                  The Phanstiel Lab is located in the Thurston-Bowles Building at the University of North Carolina at Chapel Hill. For questions, or suggestions for Lure, open an issue on the github repositories. Other questions can be directed to Doug Phanstiel at douglas_phanstiel@med.unc.edu. 
                                </h4>
                                "
                              )
                          )
                        )
                      )
                    )
                )
              )
            )
            ##### End Contact Page #####
    ) # end of tabItem
  ) # end of tabItems
) # end of body

## Dashboard Page ####
dashboardPage(header, sidebar, body)
