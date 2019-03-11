
fluidPage(
  fluidRow(
    column(
      width = 2,
        tags$ul(
          width = 2,
          align = "right",
          style = "background-color: transparent; position: fixed; width: inherit; border-right: 2px solid; border-color: lightgrey; padding-right: 10px;",
          class = "nav nav-list affix",
          tags$a(id="list_section1", h3("This is the about page"), href = "#section1"),
          tags$a(id="list_section2", h3("Lets create more fake output"), href = "#section2"),
          tags$a(id="list_section3", h3("Another section here"), href = "#section3"),
          br()
        )
    ),
    column(
      width = 6,
      offset = 1,
      div(
        id = "section1",
        h1("This is the about page (Placeholder Text)"),
        h4("Long-range interactions between genomic regions are important mediators of gene expression. These contacts, or DNA loops, allow linearly distant cis- or trans-regulatory elements to be brought into close proximity to their effector genes. Many techniques have been devised to explore these distant genomic interactions. The first technique developed, chromosome conformation capture (3C), only allowed for investigation of interactions between pairs of loci. Newer methods, such as Hi-C, allow for genome-wide identification of chromatin interactions by combining 3C methods with high-throughput sequencing."),
        br(),
        h4("While Hi-C can generate a genome-wide map of contact frequencies, observations of genomic features are limited by the base-pair resolution. Sequencing deep enough to achieve the resolution necessary to see genome-wide loops (5 Kb) is prohibitively expensive. In 2015, Sanborn and Rao et al. developed an inexpensive, region-targeted method for Hi-C that they termed hybrid-capture Hi-C (Hi-C2). Rather than sequencing an entire Hi-C library, Hi-C2 allows for the selection of a targeted genomic region though enrichment with hybridization probes. ")
      ),
      div(
        id = "section2",
        h1("Lets Create More Fake Output"),
        h4("Long-range interactions between genomic regions are important mediators of gene expression. These contacts, or DNA loops, allow linearly distant cis- or trans-regulatory elements to be brought into close proximity to their effector genes. Many techniques have been devised to explore these distant genomic interactions. The first technique developed, chromosome conformation capture (3C), only allowed for investigation of interactions between pairs of loci. Newer methods, such as Hi-C, allow for genome-wide identification of chromatin interactions by combining 3C methods with high-throughput sequencing."),
        br(),
        h4("While Hi-C can generate a genome-wide map of contact frequencies, observations of genomic features are limited by the base-pair resolution. Sequencing deep enough to achieve the resolution necessary to see genome-wide loops (5 Kb) is prohibitively expensive. In 2015, Sanborn and Rao et al. developed an inexpensive, region-targeted method for Hi-C that they termed hybrid-capture Hi-C (Hi-C2). Rather than sequencing an entire Hi-C library, Hi-C2 allows for the selection of a targeted genomic region though enrichment with hybridization probes. ")
      ),
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
