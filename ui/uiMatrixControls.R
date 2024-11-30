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

# Set nodes order
matrixOrderby <- function(){
  control <- radioButtons("matrixOrderby", HTML(paste("<span class='controlLabel'>",
                                                          strings$value('LABEL_MATRIX_ORDER'),"</span>")),
                          choices = valordmatrix,
                          selected = "kradius", inline=TRUE
  )
  return(control)
}


# Show text
matrixDisplayTextControl <- function() {
  control<- sliderInput(
    inputId = "matrixDisplayText",
    label   = controlLabel(strings$value("LABEL_POLAR_SHOW_LABELS_CONTROL")),
    min     = 0,
    max     = 100,
    value   = 100,
    step    = 1
  )
  return(control)
}

# Include histograms
matrixDisplayHistograms <- function() {
  control<-checkboxInput(
    inputId = "matrixDisplayHistograms",
    label   = controlLabel(strings$value("LABEL_POLAR_INCLUDE_HISTOGRAMS")),
    value   = TRUE
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

# Fill nodes
matrixFillNodesControl <- function() {
  control<-checkboxInput(
    inputId = "matrixFillNodesControl",
    label   = controlLabel(strings$value("LABEL_POLAR_FILL_NODES")),
    value   = FALSE
  )
  return(control)
}

# Show Title
matrixPrintTitleControl <- function() {
  control<-checkboxInput(
    inputId = "matrixPrintTitleControl",
    label   = controlLabel(strings$value("LABEL_POLAR_SHOW_NAME")),
    value   = FALSE
  )
  return(control)
}

# Node transparency
matrixAlphaLevelControl <- function() {
  control<-sliderInput(
    inputId = "matrixAlphaLevel",
    label   = controlLabel(strings$value("LABEL_POLAR_ALPHA_LEVEL_CONTROL")),
    min     = 0.0,
    max     = 1.0,
    value   = 0.5,
    step    = 0.1
  )
  return(control)
}

# Label size
matrixLabelsSizeControl <- function(name, description, default) {
  control<-sliderInput(
    inputId = paste0("matrixLabelsSize", name),
    label   = controlLabel(description),
    min     = 8,
    max     = 20,
    value   = default,
    step    = 1
  )
  return(control)
}

# Screen size control
matrixscreenwidthControl <- function() {
  values<-c(400, 600, 800, 1000, 1200)
  names(values)<-values
  control<-selectInput(
    inputId   = "screenwidthControl",
    label     = "pixels",
    choices   = values,
    selected  = 600,
    multiple  = FALSE
  )
  return(control)
}

# Matrix plot resolution
matrixppiControl <- function(typeplot) {
  values<-c(72, 96, 150, 300, 600)
  
  names(values)<-values
  control<-selectInput(
    inputId   = "matrixppi",
    label     = controlLabel(strings$value("LABEL_RESOLUTION_SIZE_CONTROL")),
    choices   = values,
    selected  = 300,
    multiple  = FALSE
  )
  return(control)
}

# Plot file format
matrixFileFormat <- function() {
  values<-c("png","jpg","eps","tiff","svg")
  names(values)<-values
  control<-selectInput(
    inputId   = "matrixfileextension",
    label     = controlLabel(strings$value("LABEL_ZIGGURAT_DOWNLOAD_PLOT_FILE_FORMAT")),
    choices   = values,
    selected  = "png",
    multiple  = FALSE
  )
  return(control)
}

matrixDownloadControl <- function() {
  control<-downloadButton("matrixDownload",label = strings$value("LABEL_PLOT_DOWNLOAD"))
  #shinyjs::hidden(p(id = "matrixDownload", "Processing..."))
  return(control)
}

matrixcodeDownloadControl <- function() {
  control<-downloadButton("matrixcodeDownload",label = strings$value("LABEL_POLAR_CODE_DOWNLOAD"))
  return(control)
}
