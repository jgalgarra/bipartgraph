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
    tabPanel(strings$value("LABEL_ZIGGURAT_CONFIG_PANEL"),  tags$div(class="panelContent", bipartiteConfigPanel())),
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

# Configuration
bipartiteConfigPanel <- function() {
  panel<-tabsetPanel(
    fluidRow(
      uiOutput("networknamebip")
    ),
    tabPanel(id="tab_vis",
             title=strings$value("LABEL_ZIGGURAT_CONFIG_VISUALIZATION_PANEL"),
             fluidRow(
               column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_LINKS_HEADER"), image="logos-flexline/links.png"))
             ),
             fluidRow(
                 column(2, bipartiteLinkSizeControl()),
                 column(2, bipartiteweighted_links()),
             #   column(2, bipartiteColorControl("Link", strings$value("LABEL_ZIGGURAT_LINKS_COLOR_CONTROL"), "#888888")),
             #   column(2, bipartiteAlphaLevelLinkControl())
             ),
             # fluidRow(
             #   column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_NODES_HEADER"), image="logos-flexline/nodes.png"))
             # ),
             # fluidRow(
             #   column(2, bipartiteColorControl("GuildA1", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_1_CONTROL"), czA1)),
             #   column(2, bipartiteColorControl("GuildA2", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"), czA2)),
             #   column(2, bipartiteColorControl("GuildB1", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_1_CONTROL"), czB1)),
             #   column(2, bipartiteColorControl("GuildB2", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"), czB2)),
             #   column(2, bipartiteAlphaLevelControl()),
             #   column(2, restoreColorsControl())
             # ),
             # 
             # tabPanel(
             #   strings$value("LABEL_ZIGGURAT_LOADSAVE_PANEL"),
             #   fluidRow(
             #     column(4, bipartiteloadZigConfigControlFile()),
             #     column(2, bipartiteshowZigConfigControlFile()),
             #     column(4, tags$h2(" "),bipartitesaveZigConfigControlFile())
             #   ),
             #   # Show bipartite configuration file raw JSON contents
             #   fluidRow(
             #     column(10, verbatimTextOutput("contentsfileconfigzigplot"))
             #   )
             # )
    )
  )
    return(panel)
}
