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
source("ui/uiDownloadControls.R", encoding="UTF-8")

downloadPanel <- function() {
  panel<- 
    tags$div(
    class="panelContent",
    fluidRow(
      column(3, paperLandscape()),
      column(3, paperSizeControl()),
      column(3, ppiControl())
    ),

    fluidRow(
     column(3, zigguratBckgdColorControl()),
     column(3, zigguratAspectRatio()),
     column(3, zigguratPlotFormat())
    ),
    fluidRow(div(
      tags$br()
    )),
    useShinyjs(),
    fluidRow(
      column(3, zigguratDownloadControl()),
      column(3, zigguratcodeDownloadControl())
     # column(3, zigguratsaveZigConfigControl())
    )
  )
  return(panel)
}
