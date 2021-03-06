###############################################################################
# BipartGraph
#
# Module         : uiZigguratpanels.R
# Description    : interactive Ziggurat panel
#
###############################################################################

library(shiny)
library(shinythemes)
source("ui/uiZigguratControls.R", encoding="UTF-8")

zigguratPanel<-function() {
  panel<-tabsetPanel(
    tabPanel(strings$value("LABEL_ZIGGURAT_DIAGRAM_PANEL"), tags$div(class="panelContent", zigguratDiagramPanel())),
    tabPanel(strings$value("LABEL_ZIGGURAT_CONFIG_PANEL"),  tags$div(class="panelContent", zigguratConfigPanel()))
  )
  return(panel)
}

# Ziggurat graph panel
zigguratDiagramPanel <- function() {
  control<- fluidRow(
      fluidRow(
        column(8,
          fluidRow(
            tags$span(
              id="zoomPanel",

              tags$img(id="zoomin",     onclick="svgZoomIn()",    src="images/zoom_in.png"),
              tags$img(id="zoomout",    onclick="svgZoomOut()",   src="images/zoom_out.png"),
              tags$img(id="zoomfit",    onclick="svgZoomFit()",   src="images/fit_to_width.png"),
              tags$img(id="zoomreset",  onclick="svgZoomReset()", src="images/sinchronize.png")
            )
          ),
          fluidRow(
            uiOutput("ziggurat")
          )
        ),
        column(4,
          fluidRow(
            uiOutput("networkinfoDetail")
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
          ),
          fluidRow(
            groupHeader(text=strings$value("LABEL_ZIGGURAT_DIAGRAM_WIKI_HEADER"), image="wikipedia.png")
          ),
          fluidRow(
            uiOutput("zigguratWikiDetail")
          )
        )
      )
  )
  return(control)
}

# Configuration
zigguratConfigPanel <- function() {
  panel<-tabsetPanel(
      tabPanel(
        strings$value("LABEL_ZIGGURAT_CONFIG_VISUALIZATION_PANEL"),
        fluidRow(
          column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_LINKS_HEADER"), image="link.png"))
        ),
      fluidRow(
        column(2, zigguratPaintLinksControl()),
        column(2, zigguratUseSplineControl()),
        column(2, zigguratSplinePointsControl()),
        column(2, zigguratLinkSizeControl()),
        column(2, zigguratweighted_links())

      ),
      fluidRow(
        column(2, zigguratAlphaLevelLinkControl()),
        column(2, zigguratColorControl("Link", strings$value("LABEL_ZIGGURAT_LINKS_COLOR_CONTROL"), "#888888"))
      ),
      fluidRow(
        column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_NODES_HEADER"), image="tree_structure.png"))
      ),
      fluidRow(
        column(2, zigguratHeightExpandControl()),
        column(2, zigguratHopx()),
        column(2, ziggurat1shellExpandControl()),
        column(2, zigguratCoreMaxHExp()),
        column(2, zigguratCoreMaxWExp())

      ),
      fluidRow(
        column(2, zigguratAlphaLevelControl()),
        column(2, zigguratColorControl("GuildA1", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_1_CONTROL"), czA1)),
        column(2, zigguratColorControl("GuildA2", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"), czA2)),
        column(2, zigguratColorControl("GuildB1", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_1_CONTROL"), czB1)),
        column(2, zigguratColorControl("GuildB2", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"), czB2)),
        column(2, restoreColorsControl())
      ),
      fluidRow(
        column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_Y_DISPLACE_CONTROL"), image="vertdis.png"))
      ),

      fluidRow(
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
        column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_OUTSIDERS_HEADER"), image="outsiders.png"))
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
      strings$value("LABEL_ZIGGURAT_CONFIG_LABELS_PANEL"),
      fluidRow(
        column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_LABELS_GENERAL_HEADER"), image="settings.png"))
      ),
      fluidRow(
        #column(2, zigguratAspectRatioControl()),                        Only works for non interactive ziggurats
        column(3, zigguratdisplace_legend_horiz()),
        column(3, zigguratdisplace_legend_vert())
      ),
      fluidRow(
        column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_LABELS_SIZE_HEADER"), image="generic_text.png"))
      ),
      fluidRow(
        column(2, zigguratLabelsSizeControl("Legend", strings$value("LABEL_ZIGGURAT_LEGEND_LABEL_SIZE_CONTROL"), 5))
      ),
      fluidRow(
        column(2, zigguratLabelsSizeControl("kCoreMax", strings$value("LABEL_ZIGGURAT_KCOREMAX_LABEL_SIZE_CONTROL"), 5)),
        column(2, zigguratLabelsSizeControl("Ziggurat", strings$value("LABEL_ZIGGURAT_ZIGGURAT_LABEL_SIZE_CONTROL"), 4.5)),
        column(2, zigguratLabelsSizeControl("kCore1", strings$value("LABEL_ZIGGURAT_KCORE1_LABEL_SIZE_CONTROL"), 4))
      ),
      fluidRow(
        column(2, zigguratCoreBoxSizeControl()),
        column(2, zigguratLabelsSizeControl("CoreBox", strings$value("LABEL_ZIGGURAT_COREBOX_LABEL_SIZE_CONTROL"), 5))
      )
    ),
    tabPanel(
      strings$value("LABEL_ZIGGURAT_CONFIG_TAILS_PANEL"),
      fluidRow(
        column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_CONFIG_TAILS_PANEL"), image="tails.png"))
      ),
      fluidRow(
        column(3, zigguratKcore2TailVerticalSeparationControl()),
        column(3, zigguratKcore1TailDistToCoreControl("1", strings$value("LABEL_ZIGGURAT_KCORE1_TAIL_DIST_TO_CORE_CONTROL_1"))),
        column(3, zigguratKcore1TailDistToCoreControl("2", strings$value("LABEL_ZIGGURAT_KCORE1_TAIL_DIST_TO_CORE_CONTROL_2"))),
        column(3, zigguratInnerTailVerticalSeparationControl())
      ),
      fluidRow(
        column(3, zigguratfattailjumphorizA()),
        column(3, zigguratfattailjumpvertA()),
        column(3, zigguratfattailjumphorizB()),
        column(3, zigguratfattailjumpvertB())
      ),
      fluidRow(
        column(12, groupHeader(text=strings$value("LABEL_ZIGGURAT_WEIRD"), image="weird.png"))
      ),
      fluidRow(
        column(3, zigguratroot_weird_expand_horiz()),
        column(3, zigguratroot_weird_expand_vert()),
        column(3, zigguratroot_weirdskcore2_horiz()),
        column(3, zigguratroot_weirdskcore2_vert())
      ),
      fluidRow(
        column(3, zigguratroot_weird_boxesseparation()),
        column(3, zigguratkcore1weirds_leafs_vertical_separation())
      )
    ),
    tabPanel(
      strings$value("LABEL_ZIGGURAT_CONFIG_SVG_HEADER"),
      fluidRow(
        column(12, groupHeader(text="Plot", image="settings.png"))
      ),
      fluidRow(
        column(4, zigguratSVGup()),
        column(4, zigguratSvgScaleFactorControl())
      )
    )

  )
  return(panel)
}
