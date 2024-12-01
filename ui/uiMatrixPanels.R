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
             column(4, matrixOrderby()),
             column(1, matrixRotateControl()),
             column(1, matrixShowNamesControl()),
             column(1, matrixShowTitleControl()),
             column(1, matrixShowLegendControl()),
             column(1, matrixWeightsControl()),
             column(2, matrixTextresizeControl() ),

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
    fluidRow(
      #column(2, matrixDisplayTextControl()),
      column(2, matrixAlphaLevelControl()),
      column(2, matrixFillNodesControl()),
      column(2, matrixPrintTitleControl()),
      column(2, matrixscreenwidthControl())
    ),
    fluidRow(
      column(12, groupHeader(text=strings$value("LABEL_POLAR_LABELS_CONFIG_HEADER"), image="logos-flexline/labels.png"))
    ),
    fluidRow(
      column(3, matrixLabelsSizeControl("Title", strings$value("LABEL_POLAR_TITLE_LABEL_SIZE_CONTROL"), 16)),
      column(3, matrixLabelsSizeControl("Legend", strings$value("LABEL_POLAR_LEGEND_LABEL_SIZE_CONTROL"), 10)),
      column(3, matrixLabelsSizeControl("LegendTitle", strings$value("LABEL_POLAR_LEGEND_TITLE_LABEL_SIZE_CONTROL"), 10))
    )
  )
  return(panel)
}

matrixdownloadPanel <- function() {
  panel<-fluidRow(
    fluidRow(
      column(12, groupHeader(text=strings$value("LABEL_POLAR_GENERAL_CONFIG_HEADER"), image="logos-flexline/configure.png"))
    ),
    fluidRow(
      column(3, matrixppiControl()),
      column(3, matrixFileFormat())
    ),
    fluidRow(
      column(3,matrixcodeDownloadControl()),
      column(3,matrixDownloadControl())
    )
  )
}
