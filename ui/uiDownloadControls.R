###############################################################################
# BipartGraph
#  
# Module         : uiDownloadControls.R
# Description    : printable Ziggurat panel
#                  
###############################################################################

library(shinyjs)


zigguratDownloadControl <- function() {
    control<-downloadButton("zigguratDownload",label = strings$value("LABEL_PLOT_DOWNLOAD"))
    return(control)
}

zigguratsaveSVGControl <- function() {
  control<-downloadButton("zigguratsaveSVG",label = strings$value("LABEL_PLOT_SVG_DOWNLOAD"))
  return(control)
}


polarDownloadControl <- function() {
  control<-downloadButton("polarDownload",label = strings$value("LABEL_PLOT_DOWNLOAD"))
  #shinyjs::hidden(p(id = "polarDownload", "Processing..."))
  return(control)
}

polarcodeDownloadControl <- function() {
  control<-downloadButton("polarcodeDownload",label = strings$value("LABEL_POLAR_CODE_DOWNLOAD"))
  return(control)
}

zigguratcodeDownloadControl <- function() {
  control<-downloadButton("zigguratcodeDownload",label = strings$value("LABEL_ZIGGURAT_CODE_DOWNLOAD"))
  return(control)
}

# control generico para seleccion de color
zigguratBckgdColorControl <- function() {
  control <- colourInput(
    "zigguratBckgdColorControl",
    controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_BACKGROUND_COLOR")),
    value = "#FFFFFF"
  )
  return(control)
}

# Aspect ratio of  the printable plot
zigguratAspectRatio <- function() {
  control<-sliderInput(
    inputId = "zigguratAspectRatio",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_ASPECT_RATIO")),
    min     = 0.2,
    max     = 2,
    value   = 1,
    step    = 0.1
  )
  return(control)
}

