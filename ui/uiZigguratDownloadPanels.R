###############################################################################
# BipartGraph
#  
# Module         : uiDownloadPanels.R
# Description    : printable Ziggurat panel
#                  
###############################################################################

library(shiny)
library(shinythemes)
library(shinyjs)

downloadPanel <- function() {
  panel<- 
    tags$div(
    class="panelContent",
    fluidRow(
      column(9, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_INTERACTIVE_HEADER"), image="network.png")),
    ),
    fluidRow(
      column(9,renderText("<br> <br>"))
    ),
    fluidRow(
      column(4, zigguratsaveSVGControl())
    ),
    fluidRow(
      column(9,renderText("<br> <br><br> <br>"))
    ),
    fluidRow(
      column(9, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_STATIC_HEADER"), image="tiff.png"))
    ),

    fluidRow(
      column(3, paperLandscape()),
      column(3, paperSizeControl()),
      column(3, ppiControl())
    ),

    fluidRow(
     column(3, zigguratBckgdColorControl()),
     column(3, zigguratAspectRatio()),
     column(3, FileFormat())
    ),
    fluidRow(div(
      tags$br()
    )),
    useShinyjs(),
    fluidRow(
      column(3, zigguratcodeDownloadControl()),
      column(3, zigguratDownloadControl())
    )
  )
  return(panel)
}
