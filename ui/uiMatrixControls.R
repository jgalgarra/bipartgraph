###############################################################################
# BipartGraph
#  
# Module         : uiMatrixControls.R
# Description    : Matrix graph panel
#                  
###############################################################################

library(shinyjs)

# Rotate matrix 90 degrees
matrixRotateControl <- function() {
  control<-checkboxInput(
    inputId = "matrixRotate",
    label   = controlLabel(strings$value("LABEL_MATRIX_ROTATE")),
    value   = FALSE
  )
  return(control)
}

# Rotate link weights
matrixWeightsControl <- function() {
  control<-checkboxInput(
    inputId = "matrixWeights",
    label   = controlLabel(strings$value("LABEL_MATRIX_WEIGHTS")),
    value   = FALSE
  )
  return(control)
}

# Show species names
matrixShowNamesControl <- function() {
  control<-checkboxInput(
    inputId = "matrixShowNames",
    label   = controlLabel(strings$value("LABEL_MATRIX_SHOW_SPECIES_NAMES")),
    value   = TRUE
  )
  return(control)
}

# Show network name as plot title
matrixShowTitleControl <- function() {
  control<-checkboxInput(
    inputId = "matrixShowTitle",
    label   = controlLabel(strings$value("LABEL_POLAR_SHOW_NAME")),
    value   = FALSE
  )
  return(control)
}


# Show guild names as legend
matrixShowLegendControl <- function() {
  control<-checkboxInput(
    inputId = "matrixShowLegend",
    label   = controlLabel(strings$value("LABEL_BIPARTITE_SHOW_LEGEND")),
    value   = TRUE
  )
  return(control)
}

# Set nodes order
matrixOrderby <- function(){
  control <- radioButtons("matrixOrderby", HTML(paste("<span class='controlLabel'>",
                                                          strings$value('LABEL_MATRIX_ORDER'),"</span>")),
                          choices = valordmatrix,
                          selected = "kradius"
  )
  return(control)
}


# Text scale
matrixTextresizeControl <- function() {
  control<- sliderInput(
    inputId = "matrixTextresize",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_TEXT_SCALE")),
    min     = 0.5,
    max     = 2,
    value   = 1,
    step    = 0.1
  )
  return(control)
}

# Plot scale
matrixPlotresizeControl <- function() {
  control<- sliderInput(
    inputId = "matrixPlotresize",
    label   = controlLabel(strings$value("LABEL_MATRIX_PLOT_SCALE")),
    min     = 40,
    max     = 200,
    value   = 100,
    step    = 10
  )
  return(control)
}

# Download link
downloadLink <- function() {
  control<-checkboxInput(
    inputId = "downloadLink",
    label   = controlLabel(strings$value("LABEL_POLAR_INCLUDE_HISTOGRAMS")),
    value   = TRUE
  )
  return(control)
}

# Color picker
matrixColorControl <- function(name, description, default) {
  control <- colourInput(
    paste0("matrixColor" , name),
    controlLabel(description),
    value = default
  )
  return(control)
}

matrixcodeDownloadControl <- function() {
  control<-downloadButton("matrixcodeDownload",label = strings$value("LABEL_ZIGGURAT_CODE_DOWNLOAD"))
  return(control)
}

# Save bipartite plot config parameters
matrixsaveMatrixConfigFileControl <- function() {
  control<-downloadButton("matrixsaveMatrixConfigFile",label = strings$value("LABEL_ZIGGURAT_SAVECONFIG_CONTROL"))
  return(control)
}

#Load matrix plot config parameters
matrixloadMatrixConfigFileControl <- function() {
  control<-fileInput(
    inputId   = "matrixloadMatrixConfigFile",
    accept    = c(".json"),
    label     = controlLabel(strings$value("LABEL_ZIGGURAT_LOADCONFIG_CONTROL")),
    multiple  = FALSE
  )
  return(control)
}

matrixshowMatrixConfigFileControl <- function() {
  control<-checkboxInput(
    inputId = "matrixshowMatrixConfigFile",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SHOW_CONFIG_FILE_CONTROL")),
    value   = FALSE
  )
  return(control)
}