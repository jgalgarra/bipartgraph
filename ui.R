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
source("ui/uiMatrixPanels.R", encoding="UTF-8")
source("ui/uiBipartitePanels.R", encoding="UTF-8")
source("ui/uiPolarPanels.R", encoding="UTF-8")



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
        #tags$script("$(window).load(function() {windowLoad()})"),   # deprecated
        tags$script("$(window).on('load', function(){windowLoad()})")
      ),
      FullPage <- navbarPage(
        title = div("", img(src = "images/rect-BipartGraph-tr.png", id = "bipartgraph", height = "25px",style = "position: relative; margin:-15px 0px; display:right-align;")),
        theme = "css/bipartgraph_shiny.css",
      
        header  = headerPanel(),
        tabPanel(
          strings$value("LABEL_MENU_DATA_PANEL"),
          dataPanel()
        ),
        #Interactive Ziggurat panel
        tabPanel(id = "zigguratmenutab",
          strings$value("LABEL_MENU_ZIGGURAT_PANEL"),
          zigguratPanel()
        ),
        # Interactive bipartite panel
        tabPanel(id = "bipartitemenutab",
                 strings$value("LABEL_MENU_BIPARTITE_PANEL"),
                 bipartitePanel()
        ),
        
        # Matrix panel
        tabPanel(
          strings$value("LABEL_MENU_MATRIX_PANEL"),
              matrixPanel()
        ),
        # Polar panel
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
