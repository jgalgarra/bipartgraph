###############################################################################
# BipartGraph
#  
# Module         : uiDataControls.R
# 
#                  
###############################################################################
library(shinyjs)

selectDataFileControl <- function(path, pattern) {
  filesList<-as.list(list.files(path=path, pattern=pattern))
  names(filesList)<-filesList
  choices<-list(c(""))
  names(choices)<-strings$value("LABEL_SELECT_DATAFILE_LOADING")
  control<-selectInput(
    inputId   = "selectedDataFile",
    label     = controlLabel(strings$value("LABEL_SELECT_DATAFILE_CONTROL")),
    choices   = choices,
    selected  = NULL,
    multiple  = FALSE
  )
  return(control)
}

uploadFilesControl <- function() {
  control<-fileInput(
    inputId   = "uploadedFiles",
    accept    = c("txt/csv", "text/comma-separated-values", "text/plain", ".csv"),
    label     = controlLabel(strings$value("LABEL_UPLOAD_FILES_CONTROL")),
    multiple  = TRUE
  )
  return(control)
}

deleteFilesControl <- function() {
  control<-actionButton(
    inputId   = "deleteFiles",
    label     = controlLabel(strings$value("LABEL_DELETE_FILES_CONTROL"))
  )
  return(control)
}

restoreColorsControl <- function() {
  control<-actionButton(
    inputId   = "restoreColors",
    label     = controlLabel(strings$value("LABEL_ZIGGURAT_RESTORE_COLORS"))
  )
  return(control)
}

networkAnalysisControl <- function() {
  control<-downloadButton("networkAnalysis",label = strings$value("LABEL_NETORK_ANALYSIS"))
  shinyjs::hidden(p(id = "networkAnalysis", "Processing..."))
  return(control)
}

DataLabelGuildAControl<-function() {
  control<-textInput(
    inputId = "DataLabelGuildAControl",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_LABEL_GUILDA")),
    value   = labelA
  )
  return(control)
}

DataLabelGuildBControl<-function() {
  control<-textInput(
    inputId = "DataLabelGuildBControl",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_LABEL_GUILDB")),
    value   = labelB
  )
  return(control)
}

selectLanguage <- function() {
  values<-c("en", "es")
  names(values)<-values
  control<-selectInput(
    inputId   = "selectLanguage",
    label     = "",
    choices   = values,
    selected  = "en",
    multiple  = FALSE
  )
  return(control)
}