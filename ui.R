###############################################################################
# BipartGraph
#  
# Module         : ui.R
# Description    : User interfaz
###############################################################################

library(shiny)
library(shinythemes)
source("ui/uiCommonPanels.R", encoding="UTF-8")
source("ui/uiDataPanels.R", encoding="UTF-8")
source("ui/uiZigguratPanels.R", encoding="UTF-8")
source("ui/uiPolarPanels.R", encoding="UTF-8")
source("ui/uiDownloadPanels.R", encoding="UTF-8")

shinyUI(
    tagList(
        tags$head(

        tags$script(src="scripts/perfect-scrollbar.jquery.js"),
        tags$script(src="scripts/jquery.qtip.js"),
        tags$script(src="scripts/jquery.dragscrollable.js"),
        tags$script(src="scripts/jquery.waituntilexists.js"),
        tags$script(src="scripts/redesbipartitas.js"),
        tags$link(rel="stylesheet", type="text/css", href="css/perfect-scrollbar.css"),
        tags$link(rel="stylesheet", type="text/css", href="css/jquery.qtip.css"),
        tags$link(rel="stylesheet", type="text/css", href="css/redesbipartitas.css"),
        tags$script("$(window).load(function() {windowLoad()})")#,
      ),
      FullPage <- navbarPage(
        title = "BipartGraph",
        theme   = shinytheme("flatly"),
        header  = headerPanel(),
        tabPanel(
          strings$value("LABEL_MENU_DATA_PANEL"),
          dataPanel()
        ),
        # Interactive Ziggurat panel
        tabPanel(
          strings$value("LABEL_MENU_ZIGGURAT_PANEL"),
          zigguratPanel()
        ),
        # Printable ziggurat
        tabPanel(
          strings$value("LABEL_MENU_DOWNLOAD_PANEL"),
          downloadPanel()
        ),
        # Polar plot panel
        tabPanel(
          strings$value("LABEL_MENU_POLAR_PANEL"),
          polarPanel()
        ),
        # About panel
        tabPanel(
          strings$value("LABEL_MENU_ABOUT_PANEL"),
          summaryPanel()
        )
      )
    )

)
