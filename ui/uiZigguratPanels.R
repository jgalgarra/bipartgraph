###############################################################################
# BipartGraph
#
# Module         : uiZigguratpanels.R
# Description    : interactive Ziggurat panel
#
###############################################################################

library(shiny)
library(shinythemes)
library(shinyjs)
source("ui/uiZigguratControls.R", encoding="UTF-8")

downloadPanel <- function() {
  panel<- 
    tags$div(
      class="panelContent",
      
      fluidRow(
        #column(3, paperSizeControl()),
        column(3, PrintppiControl("ziggurat")),
        column(2, paperLandscape()),
        column(2, zigguratShowTitleControl()),
        column(2, zigguratShowLegendControl())
      ),
      
      fluidRow(
        #column(3, zigguratBckgdColorControl()),
        column(3, zigguratAspectRatio()),
        column(3, PrintFileFormat("ziggurat")),
        
      ),
      fluidRow(div(
        tags$br()
      )),
      useShinyjs(),
      fluidRow(
        column(3, zigguratcodeDownloadControl()),
        column(3, PrintDownloadControl("ziggurat"))
      )
    )
  return(panel)
}


zigguratPanel<-function() {
  panel<-tabsetPanel(
    tabPanel(strings$value("LABEL_ZIGGURAT_DIAGRAM_PANEL"), tags$div(class="panelContent", zigguratDiagramPanel())),
    tabPanel(strings$value("LABEL_ZIGGURAT_CONFIG_PANEL"),  tags$div(class="panelContent", zigguratConfigPanel())),
    tabPanel(strings$value("LABEL_MENU_DOWNLOAD_PANEL"),  tags$div(class="panelContent", downloadPanel()))
  )
  return(panel)
}

# Ziggurat graph panel
zigguratDiagramPanel <- function() {
  control<- fluidRow(align="left",
                     column(8,
                            fluidRow(
                              column(2,zigguratSvgScaleFactorControl() ),
                              column(2, zigguratSVGup()),
                              column(2, zigguratSVGright()),
                              column(2, zigguratkcoremaxorder()),
                              
                              column(4,
                                     fluidRow(align="center",
                                              #          id="zoomPanel",
                                              # tags$span(
                                              #        tags$img(id="zoomfit",    onclick="svgZoomFit()",   src="images/fit_to_width.png")
                                              #        ),
                                              tags$span(
                                                tags$img(id="zoomin",     onclick="svgZoomIn('ziggurat')",    src="images/logos-flexline/zoom-in.png")
                                              ),
                                              tags$span(
                                                tags$img(id="zoomout",    onclick="svgZoomOut('ziggurat')",   src="images/logos-flexline/zoom-out.png")
                                              ),
                                              # tags$span(
                                              #          tags$img(id="zoomreset",  onclick="svgZoomReset()", src="images/sinchronize.png")
                                              #          ),
                                              downloadButton("zigguratsaveSVG", label="SVG", class = "butt1"),
                                              tags$head(tags$style(".butt1, .butt1:active , .butt1:visited, .butt1:hover {background-color:rgba(0,0,0,0);
                                        color: black;
                                        font-size: 12px;
                                        border-color: rgba(0,0,0,0);
                                        -webkit-box-shadow: 2px;
                                        box-shadow: 0px;}"))
                                     )
                              ),
                            ),
                            tags$span(id="ziggplot",
                                      fluidRow(align="center",valign="top",
                                               uiOutput("ziggurat"))
                            )
                     ),
                     column(4,
                            fluidRow(
                              uiOutput("networkinfoDetailziggurat")
                            ),
                            fluidRow(
                              column(1, tags$small(strings$value("LABEL_ZIGGURAT_INFO_DETAILS_ID"))),
                              column(2, tags$small(strings$value("LABEL_ZIGGURAT_INFO_DETAILS_TYPE"))),
                              column(4, tags$small(strings$value("LABEL_ZIGGURAT_INFO_DETAILS_NAME"))),
                              column(1, tags$small("kshell")),
                              column(1, tags$small("krad") ),
                              column(1, tags$small("kdeg"))
                            ),
                            fluidRow(
                              uiOutput("zigguratNodesDetail")
                            )
                     )
  )
  
  return(control)
}

# Configuration
zigguratConfigPanel <- function() {
  panel<-tabsetPanel(
    fluidRow(
      uiOutput("networknamezigg")
    ),
    tabPanel(id="tab_vis",
             title=strings$value("LABEL_ZIGGURAT_CONFIG_VISUALIZATION_PANEL"),
             fluidRow(
               column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_LINKS_HEADER"), image="logos-flexline/links.png"))
             ),
             fluidRow(
               column(1, zigguratPaintLinksControl()),
               column(1, zigguratUseSplineControl()),
               column(2, zigguratSplinePointsControl()),
               column(2, zigguratLinkSizeControl()),
               column(2, zigguratweighted_links()),
               column(2, zigguratColorControl("Link", strings$value("LABEL_ZIGGURAT_LINKS_COLOR_CONTROL"), "#888888")),
               column(2, zigguratAlphaLevelLinkControl())
             ),
             fluidRow(
               column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_NODES_HEADER"), image="logos-flexline/nodes.png"))
             ),
             fluidRow(
               column(2, zigguratHeightExpandControl()),
               column(2, ziggurat1shellExpandControl()),
               column(2, zigguratCoreMaxHExp()),
               column(2, zigguratCoreMaxWExp()),
               column(2, zigguratHopx())
             ),
             fluidRow(
               column(2, zigguratColorControl("GuildA1", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_1_CONTROL"), czA1)),
               column(2, zigguratColorControl("GuildA2", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"), czA2)),
               column(2, zigguratColorControl("GuildB1", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_1_CONTROL"), czB1)),
               column(2, zigguratColorControl("GuildB2", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"), czB2)),
               column(2, zigguratAlphaLevelControl()),
               #column(2, restoreColorsControl())
             ),
             fluidRow(
               column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_Y_DISPLACE_CONTROL"), image="logos-flexline/bidir-arrow.png"))
             ),
             
             fluidRow(
               column(1, uiOutput("networkGuildALabel")),
               column(1, zigguratYDisplaceControlS("A", "2")),
               column(1, zigguratYDisplaceControlS("A", "3")),
               column(1, zigguratYDisplaceControlS("A", "4")),
               column(1, zigguratYDisplaceControlS("A", "5")),
               column(1, zigguratYDisplaceControlS("A", "6")),
               column(1, zigguratYDisplaceControlS("A", "7")),
               column(1, zigguratYDisplaceControlS("A", "8")),
               column(1, zigguratYDisplaceControlS("A", "9")),
               column(1, zigguratYDisplaceControlS("A", "10")),
               column(1, zigguratYDisplaceControlS("A", "11")),
               column(1, zigguratYDisplaceControlS("A", "12")),
               column(1, zigguratYDisplaceControlS("A", "13")),
               column(1, zigguratYDisplaceControlS("A", "14")),
               column(1, zigguratYDisplaceControlS("A", "15")),
               column(1, zigguratYDisplaceControlS("A", "16")),
               column(1, zigguratYDisplaceControlS("A", "17")),
               column(1, zigguratYDisplaceControlS("A", "18")),
               column(1, zigguratYDisplaceControlS("A", "19")),
               column(1, zigguratYDisplaceControlS("A", "20"))
             ),
             
             fluidRow(
               column(1, uiOutput("networkGuildBLabel")),
               column(1, zigguratYDisplaceControlS("B", "2")),
               column(1, zigguratYDisplaceControlS("B", "3")),
               column(1, zigguratYDisplaceControlS("B", "4")),
               column(1, zigguratYDisplaceControlS("B", "5")),
               column(1, zigguratYDisplaceControlS("B", "6")),
               column(1, zigguratYDisplaceControlS("B", "7")),
               column(1, zigguratYDisplaceControlS("B", "8")),
               column(1, zigguratYDisplaceControlS("B", "9")),
               column(1, zigguratYDisplaceControlS("B", "10")),
               column(1, zigguratYDisplaceControlS("B", "11")),
               column(1, zigguratYDisplaceControlS("B", "12")),
               column(1, zigguratYDisplaceControlS("B", "13")),
               column(1, zigguratYDisplaceControlS("B", "14")),
               column(1, zigguratYDisplaceControlS("B", "15")),
               column(1, zigguratYDisplaceControlS("B", "16")),
               column(1, zigguratYDisplaceControlS("B", "17")),
               column(1, zigguratYDisplaceControlS("B", "18")),
               column(1, zigguratYDisplaceControlS("B", "19")),
               column(1, zigguratYDisplaceControlS("B", "20"))
             ),
             fluidRow(
               column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_OUTSIDERS_HEADER"), image="logos-flexline/outsiders.png"))
             ),
             fluidRow(
               column(2, zigguratPaintOutsidersControl()),
               column(2, zigguratoutsiders_expand_horiz()),
               column(2, zigguratoutsiders_expand_vert()),
               column(2, zigguratoutsiders_separation_expand()),
               column(2, zigguratoutsiders_legend_expand())
             )
    ),
    
    tabPanel(
      strings$value("LABEL_ZIGGURAT_CONFIG_TAILS_PANEL"),
      fluidRow(
        column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_TAILS_PANEL"), image="logos-flexline/tails.png"))
      ),
      fluidRow(
        column(3, zigguratKcore1TailDistToCoreControl("1", strings$value("LABEL_ZIGGURAT_KCORE1_TAIL_DIST_TO_CORE_CONTROL_1"))),
        column(3, zigguratKcore1TailDistToCoreControl("2", strings$value("LABEL_ZIGGURAT_KCORE1_TAIL_DIST_TO_CORE_CONTROL_2"))),
        column(3, zigguratKcore2TailVerticalSeparationControl()),
        column(3, zigguratInnerTailVerticalSeparationControl())
      ),
      fluidRow(        
        column(1, uiOutput("networkGuildALabelTail")),
        column(3, zigguratfattailjumphorizA()),
        column(3, zigguratfattailjumpvertA())
      ),
      
      fluidRow(
        column(1, uiOutput("networkGuildBLabelTail")),
        column(3, zigguratfattailjumphorizB()),
        column(3, zigguratfattailjumpvertB())
      ),
      fluidRow(
        column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_SPECIALIST"), image="logos-flexline/specialists.png"))
      ),
      fluidRow(
        column(3, zigguratroot_specialist_expand_horiz()),
        column(3, zigguratroot_specialist_expand_vert()),
        column(3, zigguratroot_specialistskcore2_horiz()),
        column(3, zigguratroot_specialistskcore2_vert())
      ),
      fluidRow(
        column(3, zigguratroot_specialist_boxesseparation()),
        column(3, zigguratkcore1specialists_leafs_vertical_separation())
      )
    ),
    
    
    tabPanel(
      strings$value("LABEL_ZIGGURAT_CONFIG_LABELS_PANEL"),
      fluidRow(
        column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_LABELS_SIZE_HEADER"), image="logos-flexline/labels.png"))
      ),
      fluidRow(
        column(2, zigguratLabelsSizeControl("Legend", strings$value("LABEL_ZIGGURAT_LEGEND_LABEL_SIZE_CONTROL"), 5))
      ),
      fluidRow(
        column(2, zigguratLabelsSizeControl("kCoreMax", strings$value("LABEL_ZIGGURAT_KCOREMAX_LABEL_SIZE_CONTROL"), 5)),
        column(2, zigguratLabelsSizeControl("Ziggurat", strings$value("LABEL_ZIGGURAT_ZIGGURAT_LABEL_SIZE_CONTROL"), 4)),
        column(2, zigguratLabelsSizeControl("kCore1", strings$value("LABEL_ZIGGURAT_KCORE1_LABEL_SIZE_CONTROL"), 4))
      ),
      fluidRow(
        column(2, zigguratCoreBoxSizeControl()),
        column(2, zigguratLabelsSizeControl("CoreBox", strings$value("LABEL_ZIGGURAT_COREBOX_LABEL_SIZE_CONTROL"), 4))
      )
    ),
    
    tabPanel(
      strings$value("LABEL_ZIGGURAT_LOADSAVE_PANEL"),
      fluidRow(
        column(4, zigguratloadZigConfigFileControl()),
        column(2, zigguratshowZigConfigFileControl()),
        column(4, tags$h2(" "),zigguratsaveZigConfigFileControl())
      ),
      # Show ziggurat configuration file raw JSON contents
      fluidRow(
        column(10, verbatimTextOutput("contentsfileconfigzigplot"))
      )
    )
  )
  return(panel)
}