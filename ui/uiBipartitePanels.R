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
    tabPanel(strings$value("LABEL_MENU_DOWNLOAD_PANEL"),  tags$div(class="panelContent", bipartiteDownloadPanel()))
  )
  return(panel)
}

bipartiteDownloadPanel <- function() {
  panel<- 
    tags$div(
      class="panelContent",
      
      fluidRow(
        column(3, PrintppiControl("bipartite")),
        column(3, PrintFileFormat("bipartite")),
        column(2, bipartiteShowTitleControl()),
        column(2, bipartiteShowLegendControl())
      ),
      
      fluidRow(div(
        tags$br()
      )),
      useShinyjs(),
      fluidRow(
        column(3, bipartitecodeDownloadControl()),
        column(3, PrintDownloadControl("bipartite"))
      )
    )
  return(panel)
}

# Ziggurat graph panel
bipartiteDiagramPanel <- function() {
  control<- fluidRow(align="left",
                     column(12,
                            fluidRow(
                              column(2, bipartiteTextRescaleControl()),
                              column(2, bipartiteGuildgapincreaseControl()),
                              column(1, bipartiteVerticalLayoutControl()),
                              column(1, bipartitePlottype()),
                              column(2, fluidRow(align="center",
                                                 tags$span(
                                                   tags$img(id="svgleft",     
                                                            title = strings$value("LABEL_SVG_MOVE"),
                                                            onclick=paste("svgMoveHoriz(",-svg_jump_size,",'bipartite')"),   
                                                            src="images/logos-flexline/arrow-left.png", width="14")
                                                 ),
                                                 tags$span(
                                                   tags$img(id="svgright", 
                                                            title = strings$value("LABEL_SVG_MOVE"),
                                                            onclick=paste("svgMoveHoriz(",svg_jump_size,",'bipartite')"),   
                                                            src="images/logos-flexline/arrow-right.png", width="14")
                                                 ),
                                                 tags$span(
                                                   tags$img(id="zoominbip",     
                                                            title= strings$value("LABEL_SVG_ZOOMIN"),
                                                            onclick="svgZoomIn('bipartite')",
                                                            src="images/logos-flexline/zoom-in.png")
                                                 ),
                                                 tags$span(
                                                   tags$img(id="zoomoutbip",
                                                            title = strings$value("LABEL_SVG_ZOOMOUT"),
                                                            onclick="svgZoomOut('bipartite')",   
                                                            src="images/logos-flexline/zoom-out.png")
                                                 ),
                                                 downloadButton("bipartitesaveSVG", label="SVG", class = "butt1"),
                                                 tags$head(tags$style("butt1, .butt1:active , .butt1:visited, .butt1:hover {background-color:rgba(0,0,0,0);
                                          color: black;
                                          font-size: 12px;
                                          border-color: rgba(0,0,0,0);
                                          -webkit-box-shadow: 2px;
                                          box-shadow: 0px;}")),
                              )
                              ),
                              column(4,
                                     fluidRow(
                                       uiOutput("networkinfoDetailbipartite")
                                     ),
                                     
                              ),
                     ),
                     tags$span(id="bipartiteplot",
                               class="svgcontainer",
                               fluidRow(align="center",valign="top",
                                        uiOutput("bipartite"))

                     )
                     
                     ),
                     
  )
  return(control)
}

# Configuration
bipartiteConfigPanel <- function() {
  panel<-fluidRow(
    tabsetPanel(
      fluidRow(
        uiOutput("networknamebipartite")
      ),
      tabPanel(id="tab_vis_bip",
               title=strings$value("LABEL_ZIGGURAT_CONFIG_VISUALIZATION_PANEL"),
               fluidRow(
                 column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_LINKS_HEADER"), image="logos-flexline/links.png"))
               ),
               fluidRow(
                 column(2, bipartiteLinkSizeControl()),
                 column(2, bipartiteweighted_links()),
                 column(2, bipartiteColorControl("Link", strings$value("LABEL_ZIGGURAT_LINKS_COLOR_CONTROL"), "#888888")),
                 column(2, bipartiteAlphaLevelLinkControl())
               ),
               fluidRow(
                 column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_NODES_HEADER"), image="logos-flexline/nodes.png"))
               ),
               fluidRow(
                 #column(2, bipartiteNodeRescale()),
                 column(2, bipartiteColorControl("GuildA1", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_1_CONTROL"), czA1)),
                 column(2, bipartiteColorControl("GuildA2", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"), czA2)),
                 column(2, bipartiteColorControl("GuildB1", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_1_CONTROL"), czB1)),
                 column(2, bipartiteColorControl("GuildB2", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"), czB2)),
                 column(2, bipartiteOneColorControl())
                 ),
               
      ),
      tabPanel(
        strings$value("LABEL_ZIGGURAT_LOADSAVE_PANEL"),
        useShinyjs(),
        fluidRow(
          column(4, bipartiteloadBipConfigFileControl()),
          column(2, bipartiteshowBipConfigFileControl()),
          column(4, tags$h2(" "),bipartitesaveBipConfigFileControl())
        ),
        # Show bipartite configuration file raw JSON contents
        fluidRow(
          column(10, verbatimTextOutput("contentsfileconfigbipplot"))
        )
      )
    )
  )
  return(panel)
}