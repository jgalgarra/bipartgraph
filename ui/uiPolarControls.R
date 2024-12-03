###############################################################################
# BipartGraph
#  
# Module         : uiPolarControls.R
# Description    : Polar graph panel
#                  
###############################################################################

library(shinyjs)

# Show text
polarDisplayTextControl <- function() {
  control<- sliderInput(
    inputId = "polarDisplayText",
    label   = controlLabel(strings$value("LABEL_POLAR_SHOW_LABELS_CONTROL")),
    min     = 0,
    max     = 100,
    value   = 100,
    step    = 1
  )
  return(control)
}

# Include histograms
polarDisplayHistograms <- function() {
  control<-checkboxInput(
    inputId = "polarDisplayHistograms",
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
polarFillNodesControl <- function() {
  control<-checkboxInput(
    inputId = "polarFillNodes",
    label   = controlLabel(strings$value("LABEL_POLAR_FILL_NODES")),
    value   = FALSE
  )
  return(control)
}

# Show Title
polarPrintTitleControl <- function() {
  control<-checkboxInput(
    inputId = "polarPrintTitle",
    label   = controlLabel(strings$value("LABEL_POLAR_SHOW_NAME")),
    value   = FALSE
  )
  return(control)
}

# Node transparency
polarAlphaLevelControl <- function() {
  control<-sliderInput(
    inputId = "polarAlphaLevel",
    label   = controlLabel(strings$value("LABEL_POLAR_ALPHA_LEVEL_CONTROL")),
    min     = 0.0,
    max     = 1.0,
    value   = 0.5,
    step    = 0.1
  )
  return(control)
}

# Label size
polarLabelsSizeControl <- function(name, description, default) {
  control<-sliderInput(
    inputId = paste0("polarLabelsSize", name),
    label   = controlLabel(description),
    min     = 8,
    max     = 20,
    value   = default,
    step    = 1
  )
  return(control)
}

# Screen size control
polarscreenwidthControl <- function() {
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

polarcodeDownloadControl <- function() {
  control<-downloadButton("polarcodeDownload",label = strings$value("LABEL_POLAR_CODE_DOWNLOAD"))
  return(control)
}

# Save matrix plot config parameters
polarsavePolarConfigFileControl <- function() {
  control<-downloadButton("polarsavePolarConfigFile",label = strings$value("LABEL_ZIGGURAT_SAVECONFIG_CONTROL"))
  return(control)
}

#Load matrix plot config parameters
polarloadPolarConfigFileControl <- function() {
  control<-fileInput(
    inputId   = "polarloadPolarConfigFile",
    accept    = c(".json"),
    label     = controlLabel(strings$value("LABEL_ZIGGURAT_LOADCONFIG_CONTROL")),
    multiple  = FALSE
  )
  return(control)
}

polarshowPolarConfigFileControl <- function() {
  control<-checkboxInput(
    inputId = "polarshowPolarConfigFile",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SHOW_CONFIG_FILE_CONTROL")),
    value   = FALSE
  )
  return(control)
}
