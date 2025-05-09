# BipartGraph
#  
# Module         : uiDataPanels.R
# 
#                  
###############################################################################


source("ui/uiDataControls.R", encoding="UTF-8")

dataPanel <- function() {
  panel<-tabsetPanel(
    id="dataPanel",
    tabPanel(strings$value("LABEL_SELECT_DATA_PANEL"),   tags$div(class="panelContent", selectDataPanel())),
    tabPanel(strings$value("LABEL_MANAGE_FILES_PANEL"),  tags$div(class="panelContent", manageFilesPanel()))
  )
  return(panel)
}

selectDataPanel<-function() {
  panel<-fluidRow(
    fluidRow(
      column(12, groupHeader(text=strings$value("LABEL_SELECT_DATA_HEADER"), image="logos-flexline/new-file.png"))
    ),
    fluidRow(
      column(12, tags$h6(strings$value("LABEL_SELECT_DATA_TIP")))
    ),
    fluidRow(
      column(3, selectDataFileControl(path=dataDir, pattern=dataFilePattern)),
      column(2, selectDataSeparatorControl()),
      column(1, selectDataSpeciesNamesControl()),
      
      column(2, tags$h5(htmlOutput("NetworkType"))),
      column(1, actionButton("ResetAll", label = strings$value("LABEL_ACTION_RESET")))
    ),

    fluidRow(
      column(2, DataLabelGuildAControl()),
      column(1, tags$h5(textOutput("NodesGuildA"))),
      column(2, DataLabelGuildBControl()),
      column(1, tags$h5(textOutput("NodesGuildB"))),
      column(2, networkAnalysisControl()),
      column(2, downloadLabelsControl()),
    ),

    fluidRow(
      column(12, groupHeader(text=strings$value("LABEL_DATA_CONTENT_HEADER"), image="logos-flexline/table.png"))
    ),    
    fluidRow(
      column(12, dataTableOutput("selectedDataFileContent"))
    )
  )
  return(panel)
}

manageFilesPanel<-function() {
  panel<-fluidRow(
    fluidRow(
      column(6, groupHeader(text=strings$value("LABEL_FILE_UPLOAD_HEADER"), image="logos-flexline/upload.png")),
      column(6, groupHeader(text=strings$value("LABEL_FILE_LAST_UPLOADED_HEADER"), image="logos-flexline/text-file.png"))
    ),
    fluidRow(
      column(6,
        tags$h6(strings$value("LABEL_FILE_UPLOAD_TIP")),
        uploadFilesControl()
      ),
      column(6, dataTableOutput("uploadedFilesTable"))
    ),

    fluidRow(
      column(12, groupHeader(text=strings$value("LABEL_AVAILABLE_FILES"), image="logos-flexline/folder.png"))
    ),

    fluidRow(
    column(12, DT::dataTableOutput('availableFilesTable'))
    ),

    fluidRow(
      column(12, deleteFilesControl())
    )
  )
  return(panel)
}