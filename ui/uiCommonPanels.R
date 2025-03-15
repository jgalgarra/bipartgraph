###############################################################################
# BipartGraph
#  
# Module         : uiCommonPanels.R
# Description    : Data controls
#                  
###############################################################################
library(shiny)
library(shinythemes)

# Common page header
headerPanel <- function() {
  control<-""
  return(control)
}

# Footer
footerPanel <- function() {
  control<-tags$div(
    class="footerPanel",
    tags$hr(),
    tags$div(
      class="footerPanelContent",
      tags$p(
        class="footerPanelContentLeft",
        strings$value("LABEL_FOOTER_CONTENT_LEFT")
      ),
      tags$p(
        class="footerPanelContentRight",
        strings$value("LABEL_FOOTER_CONTENT_RIGHT")
      )
    )
  )
  return(control)
}


summaryPanel <- function() {
  info    <- tags$div(
                tags$p("Interactive visualization tool of bipartite ecological networks"),
                tags$p(tags$a(href="http://jgalgarra.github.io/bipartgraph", target="_BLANK", "Web site")),
                
                tags$p(
                tags$a(href="user_guide/BipartGraph%20Guide.pdf", target="_BLANK", "User Guide")),
                tags$p("Garcia-Algarra, J., Pastor, J. M., Mouronte, M. L., & Galeano, J. (2018). A structural approach to disentangle the visualization of bipartite biological networks. Complexity, 2018, 1-11."),
                tags$a(href="https://onlinelibrary.wiley.com/doi/10.1155/2018/6204947", target="_BLANK", "https://onlinelibrary.wiley.com/doi/10.1155/2018/6204947"),
                tags$p("  "),
                tags$p("Garcia-Algarra, J., Pastor, J. M., Iriondo, J. M., & Galeano, J. (2017). Ranking of critical species to preserve the functionality of mutualistic networks using the k-core decomposition. PeerJ, 5, e3321."),
                tags$a(href="https://peerj.com/articles/3321/", target="_BLANK", "https://peerj.com/articles/3321/"),
                tags$p("  "),
                tags$p(HTML("Example networks downloaded from <a href='https://www.web-of-life.es/', target='_BLANK' >Web of Life database</a>. &nbsp; Core Line - Free icons by <a href='https://www.streamlinehq.com/icons?tab=free'>Streamlinehq.com</a> under CC BY 4.0 License"))
                
  )
  author  <- "Javier Garcia-Algarra & Juan Manuel Garcia-Santi"
  version <- "v2.0 - December 2024"
  panel<-tags$div(class="panelContent", fluidRow(
      column(12,
        fluidRow(groupHeader(text=strings$value("LABEL_ABOUT_INFO_HEADER"), image="logos-flexline/information.png")),
        fluidRow(tags$h5(class="aboutInfo", info)),
        fluidRow(groupHeader(text=strings$value("LABEL_ABOUT_LICENSE"), image="logos-flexline/license.png")),
        
        fluidRow(tags$p(class="aboutInfo", "MIT License"),
                 tags$p(class="aboutInfo", "Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the 'Software'), 
to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.")),
        fluidRow(tags$p(class="aboutInfo","THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.")),
        fluidRow(groupHeader(text=strings$value("LABEL_ABOUT_AUTHOR_HEADER"), image="logos-flexline/author.png")),
        fluidRow(tags$h5(class="aboutAuthor", author)),
        fluidRow(groupHeader(text=strings$value("LABEL_ABOUT_VERSION_HEADER"), image="logos-flexline/release.png")),
        fluidRow(tags$h5(class="aboutVersion", version))
        
      )
    ))
  return(panel)
}
