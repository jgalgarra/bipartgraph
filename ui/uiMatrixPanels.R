###############################################################################
# BipartGraph
#  
# Module         : MatrixPanels.R
# Description    : Matrix plot panel 
#
###############################################################################

library(shiny)
library(shinythemes)
source("ui/uiMatrixControls.R", encoding="UTF-8")

# Matrix plot panel and control configuration
matrixPanel<-function() {
  panel<-tabsetPanel(
    tabPanel(strings$value("LABEL_POLAR_DIAGRAM_PANEL"),        tags$div(class="panelContent", matrixDiagramPanel())),
    tabPanel(strings$value("LABEL_POLAR_CONFIGURATION_PANEL"),  tags$div(class="panelContent", matrixConfigPanel())),
    tabPanel(strings$value("LABEL_MENU_DOWNLOAD_PANEL"),  tags$div(class="panelContent", matrixdownloadPanel()))
  )
  return(panel)
}

# Matrix graph
matrixDiagramPanel <- function() {
  # if (exists("mat"))
  #   nfic <- mat$
  # else
  #   nfic <- ""
  control<-fluidRow(valign="top",
    column(8,
      fluidRow(
             column(2, matrixOrderby()),
             column(2, matrixTextresizeControl() ),
             column(2, matrixPlotresizeControl() ),
             column(1, matrixRotateControl()),
             column(1, matrixShowNamesControl()),
             column(1, matrixShowTitleControl()),
             column(1, matrixShowLegendControl()),
      
             column(1, matrixWeightsControl()),
             

      ),
      fluidRow(align="center",plotOutput("matrix",inline = FALSE))
      ),
    column(4,
      fluidRow(
               uiOutput("networkinfoDetailmatrix")
      ),
      fluidRow(
          column(5,tags$small(
               uiOutput("networkinfoDetailmatrixA")
            )),
          column(5,tags$small(
               uiOutput("networkinfoDetailmatrixB")
            ))
    )
    )
  )
  return(control)
}

# Config panel
matrixConfigPanel <- function() {
  panel<-fluidRow(
    fluidRow(
      column(12, groupHeader(text=strings$value("LABEL_POLAR_GENERAL_CONFIG_HEADER"), image="logos-flexline/configure.png"))
    ),
    # fluidRow(
    #   column(1, matrixShowTitleControl()),
    #   column(1, matrixShowLegendControl()),
    # ),
    fluidRow(
      column(2, matrixColorControl("GuildA", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_1_CONTROL"), czA2)),
      column(2, matrixColorControl("GuildB", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_1_CONTROL"), czB2)),
    )
  )
  return(panel)
}

matrixdownloadPanel <- function() {
  panel<-fluidRow(
    fluidRow(
      column(3, PrintppiControl("matrix")),
      column(3, PrintFileFormat("matrix"))
    ),
    fluidRow(
      column(3,matrixcodeDownloadControl()),
      column(3,PrintDownloadControl("matrix"))
    )
  )
}
