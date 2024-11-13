###############################################################################
# BipartGraph
#
# Module         : uiBipartitePanels.R
# Description    : interactive bipartite panel
#
###############################################################################

library(shiny)
library(shinythemes)
library(shinyjs)
source("ui/uiBipartiteControls.R", encoding="UTF-8")

bipartitePanel<-function() {
  panel<-tabsetPanel(
    tabPanel(strings$value("LABEL_ZIGGURAT_DIAGRAM_PANEL"), tags$div(class="panelContent", bipartiteDiagramPanel())),
    #tabPanel(strings$value("LABEL_ZIGGURAT_CONFIG_PANEL"),  tags$div(class="panelContent", zigguratConfigPanel())),
    #tabPanel(strings$value("LABEL_MENU_DOWNLOAD_PANEL"),  tags$div(class="panelContent", downloadPanel()))
  )
  return(panel)
}


# Ziggurat graph panel
bipartiteDiagramPanel <- function() {
  control<- fluidRow(column(12,
                      fluidRow(
                            column(2, bipartiteSvgScaleFactorControl() ),
                            column(2, bipartiteNodeRescale()),
                            column(2, bipartiteGuildgapincreaseControl()),
                            column(2, bipartitePlottype()),
                            column(2,
                                   fluidRow(align="center",
                                            tags$span(
                                              tags$img(id="zoominbip",     
                                                       onclick="svgZoomIn('bipartite')",   
                                                       src="images/logos-flexline/zoom-in.png")
                                            ),
                                            tags$span(
                                              tags$img(id="zoomoutbip",    
                                                       onclick="svgZoomOut('bipartite')",   
                                                       src="images/logos-flexline/zoom-out.png")
                                            ),
                                            downloadButton("zigguratsaveSVGBip", label="SVG", class = "butt1"),
                                            tags$head(tags$style(".butt1, .butt1:active , .butt1:visited, .butt1:hover {background-color:rgba(0,0,0,0);
                                        color: black;
                                        font-size: 12px;
                                        border-color: rgba(0,0,0,0);
                                        -webkit-box-shadow: 2px;
                                        box-shadow: 0px;}"))
                                   )
                                 ),
                            column(2,
                                   fluidRow(align="left",
                                            uiOutput("networkinfoDetailbip"))
                            )
                              ),
                      tags$span(id="bipartiteplot",
                      fluidRow(       #align="center",valign="top",style="padding-left: 2%;padding-right: 2%; padding-top:2%;",
                                  column(10,align="center",uiOutput("bipartite"))
                              )
                           )
                      )
                    )

  return(control)
}
