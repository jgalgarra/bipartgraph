###############################################################################
# BipartGraph
#  
# Module         : PolarPanels.R
# Description    : Polar plot panel 
#
###############################################################################

library(shiny)
library(shinythemes)
source("ui/uiPolarControls.R", encoding="UTF-8")

# Polar plot panel and control configuration
polarPanel<-function() {
  panel<-tabsetPanel(
    tabPanel(strings$value("LABEL_POLAR_DIAGRAM_PANEL"),        tags$div(class="panelContent", polarDiagramPanel())),
    tabPanel(strings$value("LABEL_POLAR_CONFIGURATION_PANEL"),  tags$div(class="panelContent", polarConfigPanel())),
    tabPanel(strings$value("LABEL_MENU_DOWNLOAD_PANEL"),  tags$div(class="panelContent", polardownloadPanel()))
  )
  return(panel)
}

# Polar graph
polarDiagramPanel <- function() {
  if (exists("zgg"))
    nfic <- zgg$polar_file
  else
    nfic <- ""
  control<-fluidRow(valign="top",
    column(8,
      fluidRow(plotOutput("polar",inline = FALSE))
      ),
    column(4,
      fluidRow(
               uiOutput("networkinfoDetailpolar")
      ),
      fluidRow(
          column(5,tags$small(
               uiOutput("networkinfoDetailpolarA")
            )),
          column(5,tags$small(
               uiOutput("networkinfoDetailpolarB")
            ))
    )
    )
  )
  return(control)
}


# Config panel
polarConfigPanel <- function() {
  panel<-fluidRow(
    tabsetPanel(
      fluidRow(
        uiOutput("networknamepolar")
      ),
      tabPanel(id="tab_vis_bip",
               title=strings$value("LABEL_ZIGGURAT_CONFIG_VISUALIZATION_PANEL"),
        fluidRow(
          column(12, groupHeader(text=strings$value("LABEL_POLAR_GENERAL_CONFIG_HEADER"), image="logos-flexline/configure.png"))
        ),
        fluidRow(
          column(2, polarDisplayTextControl()),
          column(2, polarAlphaLevelControl()),
          column(2, polarFillNodesControl()),
          column(2, polarPrintTitleControl())
          #column(2, polarscreenwidthControl())
        ),
        fluidRow(
          column(12, groupHeader(text=strings$value("LABEL_POLAR_LABELS_CONFIG_HEADER"), image="logos-flexline/labels.png"))
        ),
        fluidRow(
          column(3, polarLabelsSizeControl("Title", strings$value("LABEL_POLAR_TITLE_LABEL_SIZE_CONTROL"), 16)),
          column(3, polarLabelsSizeControl("Legend", strings$value("LABEL_POLAR_LEGEND_LABEL_SIZE_CONTROL"), 10)),
          column(3, polarLabelsSizeControl("LegendTitle", strings$value("LABEL_POLAR_LEGEND_TITLE_LABEL_SIZE_CONTROL"), 10))
        )
      ),
      tabPanel(
        strings$value("LABEL_ZIGGURAT_LOADSAVE_PANEL"),
        useShinyjs(),
        fluidRow(
          column(4, polarloadPolarConfigFileControl()),
          column(2, polarshowPolarConfigFileControl()),
          column(4, tags$h2(" "),polarsavePolarConfigFileControl())
        ),
        # Show bipartite configuration file raw JSON contents
        fluidRow(
          column(10, verbatimTextOutput("contentsfileconfigpolarplot"))
        )
      )
      
    )
  )
  return(panel)
}

polardownloadPanel <- function() {
  panel<-fluidRow(
    fluidRow(
      column(12, groupHeader(text=strings$value("LABEL_POLAR_GENERAL_CONFIG_HEADER"), image="logos-flexline/configure.png"))
    ),
    fluidRow(
      column(3, PrintppiControl("polar")),
      column(3, PrintFileFormat("polar"))
    ),
    fluidRow(
      column(3,polarcodeDownloadControl()),
      column(3,PrintDownloadControl("polar"))
    )
  )
}
