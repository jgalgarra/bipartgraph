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

# Separator character
selectDataSeparatorControl <- function(){
  control <- radioButtons("selectDataSeparator", HTML(paste("<span class='controlLabel'>",
                                                          strings$value("LABEL_DATA_SEPARATOR"),"</span>")),
                          choiceNames =  c(strings$value("LABEL_DATA_SEP_COMMA"),strings$value("LABEL_DATA_SEP_SEMICOLON"),
                                           strings$value("LABEL_DATA_SEP_TAB")),
                          choiceValues = valorseparator,
                          selected = ","
  )
  return(control)
}

#The data file contains the species names as row and col names
selectDataSpeciesNamesControl <- function() {
  control<-checkboxInput(
    inputId = "selectDataSpeciesNames",
    label   = controlLabel(strings$value("LABEL_SPECIES_HEADER")),
    value   = TRUE
  )
  return(control)
}


networkAnalysisControl <- function() {
  control<-downloadButton("networkAnalysis",label = strings$value("LABEL_NETORK_ANALYSIS"))
  return(control)
}

downloadLabelsControl <- function() {
  control<-downloadButton("downloadLabels",label = strings$value("LABEL_MATRIX_SHOW_SPECIES_NAMES"))
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
    selected  = "es",
    multiple  = FALSE
  )
  return(control)
}

