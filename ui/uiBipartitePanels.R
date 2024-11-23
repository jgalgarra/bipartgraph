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
        column(3, bipartiteppiControl()),
        column(3, bipartiteFileFormat()),
        column(2, bipartiteShowTitleControl()),
        column(2, bipartiteShowLegendControl())
      ),
      
      fluidRow(div(
        tags$br()
      )),
      useShinyjs(),
      fluidRow(
        column(3, bipartitecodeDownloadControl()),
        column(3, bipartiteDownloadControl())
      )
    )
  return(panel)
}

# Ziggurat graph panel
bipartiteDiagramPanel <- function() {
  control<- fluidRow(align="left",
                     column(10,
                            fluidRow(
                              column(2, bipartiteSvgScaleFactorControl() ),
                              column(2, bipartiteNodeRescale()),
                              column(2, bipartiteGuildgapincreaseControl()),
                              column(1, bipartiteVerticalLayoutControl()),
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
                                              downloadButton("bipartitesaveSVG", label="SVG", class = "butt1"),
                                              tags$head(tags$style(".butt1, .butt1:active , .butt1:visited, .butt1:hover {background-color:rgba(0,0,0,0);
                                          color: black;
                                          font-size: 12px;
                                          border-color: rgba(0,0,0,0);
                                          -webkit-box-shadow: 2px;
                                          box-shadow: 0px;}"))
                                     )
                                     
                              ),
                            ),
                            
                            fluidRow( tags$span(id="bipartiteplot",align="center",
                                                uiOutput("bipartite"))
                            )
                     ),
                     
                     column(2,
                            fluidRow(align="left",
                                     uiOutput("networkinfoDetailBip")
                            ),
                            
                     )
  )
  return(control)
}

# Configuration
bipartiteConfigPanel <- function() {
  panel<-fluidRow(
    tabsetPanel(
    # fluidRow(
    #   uiOutput("networknamebip")
    # ),
    tabPanel(id="tab_vis_bip",
            title=strings$value("LABEL_ZIGGURAT_CONFIG_VISUALIZATION_PANEL"),
    fluidRow(
      column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_LABELS_SIZE_HEADER"), image="logos-flexline/labels.png"))
    ),
    fluidRow(
      column(2, bipartiteLabelsSizeControl("kCoreMax", strings$value("LABEL_BIPARTITE_KCOREMAX_LABEL_SIZE_CONTROL"), 3.5)),
    ),
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
      column(2, bipartiteColorControl("GuildA1", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_1_CONTROL"), czA1)),
      column(2, bipartiteColorControl("GuildA2", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"), czA2)),
      column(2, bipartiteColorControl("GuildB1", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_1_CONTROL"), czB1)),
      column(2, bipartiteColorControl("GuildB2", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"), czB2)),
      column(2, bipartiteAlphaLevelControl()),
      #column(2, restorebipartiteColorsControl())
      )
     ),
    tabPanel(
      strings$value("LABEL_ZIGGURAT_LOADSAVE_PANEL"),
      useShinyjs(),
      fluidRow(
        column(4, bipartiteloadBipConfigControlFile()),
        column(2, bipartiteshowBipConfigControlFile()),
        column(4, tags$h2(" "),bipartitesaveBipConfigControlFile())
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