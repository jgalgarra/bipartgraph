###############################################################################
# BipartGraph
#  
# Module         : uiZigguratControls.R
# Description    : interactive Ziggurat controls
#                  
###############################################################################

library(shinyjs)
library(colourpicker)

# hide links
zigguratPaintLinksControl <- function() {
  control<-checkboxInput(
    inputId = "zigguratPaintLinks",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_PAINT_LINKS_CONTROL")),
    value   = TRUE
  )
  return(control)
}

# Show labels
zigguratDisplayLabelsControl <- function() {
  control<-checkboxInput(
    inputId = "zigguratDisplayLabels",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_DISPLAY_LABELS_CONTROL")),
    value   = TRUE
  )
  return(control)
}

# DEPRECATED
zigguratFlipResultsControl <- function() {
  control<-checkboxInput(
    inputId = "zigguratFlipResults",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_FLIP_RESULT_CONTROL")),
    value   = FALSE
  )
  return(control)
}

# Outsiders 
zigguratPaintOutsidersControl <- function() {
  control<-checkboxInput(
    inputId = "zigguratPaintOutsiders",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_PAINT_OUTSIDERS_CONTROL")),
    value   = TRUE
  )
  return(control)
}

# Spline control
zigguratUseSplineControl <- function() {
  control<-checkboxInput(
    inputId = "zigguratUseSpline",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_USE_SPLINE_CONTROL")),
    value   = TRUE
  )
  return(control)
}

# Spline points number control
zigguratSplinePointsControl <- function() {
  control<-sliderInput(
    inputId = "zigguratSplinePoints",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPLINE_POINTS_CONTROL")),
    min     = 5,
    max     = 50,
    value   = 10,
    step    = 5
  )
  return(control)
}

# Aspect ratio for the SVG            DEPRECATED
zigguratAspectRatioControl <- function() {
  control<-sliderInput(
    inputId = "zigguratAspectRatio",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_ASPECT_RATIO_CONTROL")),
    min     = 0.1,
    max     = 5.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# # horizontal legend expand
# zigguratdisplace_legend_horiz <- function() {
#   control<-sliderInput(
#     inputId = "zigguratdisplace_legend_horiz",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_LEGEND_DISPLACE_HORIZ")),
#     min     = -2,
#     max     = 3.0,
#     value   = 0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# 
# # Legend expansion vert
# zigguratdisplace_legend_vert<- function() {
#   control<-sliderInput(
#     inputId = "zigguratdisplace_legend_vert",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_LEGEND_DISPLACE_VERT")),
#     min     = -2,
#     max     = 3.0,
#     value   = 0,
#     step    = 0.1
#   )
#   return(control)
# }

# Expand horizontal separation among inner ziggurats
zigguratHopx <- function() {
  control<-sliderInput(
    inputId = "zigguratHopx",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_HOPX")),
    min     = 0.6,
    max     = 4.0,
    value   = 1.0,
    step    = 0.2
  )
  return(control)
}

# Transparency
zigguratAlphaLevelControl <- function() {
  control<-sliderInput(
    inputId = "zigguratAlphaLevel",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_ALPHA_LEVEL_CONTROL")),
    min     = 0.1,
    max     = 1.0,
    value   = 0.2,
    step    = 0.1
  )
  return(control)
}

# Color picker
zigguratColorControl <- function(name, description, default) {
  control <- colourInput(
    paste0("zigguratColor" , name),
    controlLabel(description),
    value = default
  )
  return(control)
}

# Link transparency
zigguratAlphaLevelLinkControl <- function() {
  control<-sliderInput(
    inputId = "zigguratAlphaLevelLink",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_ALPHA_LEVEL_LINK_CONTROL")),
    min     = 0.1,
    max     = 1.0,
    value   = 0.5,
    step    = 0.1
  )
  return(control)
}

# link width
zigguratLinkSizeControl <- function() {
  control<-sliderInput(
    inputId = "zigguratLinkSize",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_LINK_SIZE_CONTROL")),
    min     = 0.1,
    max     = 2.0,
    value   = 0.3,
    step    = 0.1
  )
  return(control)
}

# Weight aggregation
zigguratweighted_links <- function() {
  control <- selectInput(inputId = "zigguratweighted_links", label = controlLabel(strings$value('LABEL_ZIGGURAT_LINKS_WEIGHT_CONTROL')),
                         selected = "none",
                         weightchoices)
  return(control)
}

# core box width
zigguratCoreBoxSizeControl <- function() {
  control<-sliderInput(
    inputId = "zigguratCoreBoxSize",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_COREBOX_SIZE_CONTROL")),
    min     = 0.0,
    max     = 2.0,
    value   = 0.2,
    step    = 0.1
  )
  return(control)
}

# ziggurat displacement
zigguratYDisplaceControl <- function(name, description) {
  control<-sliderInput(
    inputId = paste0("zigguratYDisplace", name),
    label   = controlLabel(description),
    min     = 0.0,
    max     = 15.0,
    value   = 11.0,
    step    = 1.0
  )
  return(control)
}

# 
zigguratYDisplaceControlS <- function(name, description)
{
  control<-sliderInput(
    inputId = paste0("zigguratYDisplaceS", name, description),
    label   = controlLabel(paste0(description,"-shell ",name)),
    min     = -2.0,
    max     = 2.0,
    value   = 0.0,
    step    = 0.1
  )
  return(control)
}

# Node box height
zigguratHeightExpandControl <- function() {
  control<-sliderInput(
    inputId = "zigguratHeightExpand",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_HEIGHT_EXPAND_CONTROL")),
    min     = 0.5,
    max     = 5.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# 1-shell nodes area expand
ziggurat1shellExpandControl <- function() {
  control<-sliderInput(
    inputId = "ziggurat1shellExpandControl",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_1SHELL_EXPAND_CONTROL")),
    min     = 0.5,
    max     = 5.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}


# Coremax height expand control
zigguratCoreMaxHExp <- function(){
  control<-sliderInput(
    inputId = "zigguratCoreMaxHExp",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_COREMAX_HEIGHT_EXPAND_CONTROL")),
    min     = 0.5,
    max     = 2.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# Coremax width expand control
zigguratCoreMaxWExp <- function(){
  control<-sliderInput(
    inputId = "zigguratCoreMaxWExp",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_COREMAX_WIDTH_EXPAND_CONTROL")),
    min     = 0.5,
    max     = 2.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# outsiders distance horizontal expansion
zigguratoutsiders_expand_horiz <- function() {
  control<-sliderInput(
    inputId = "zigguratoutsiders_expand_horiz",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_OUTSIDERS_DISPLACE_COMPONENT__HORIZ")),
    min     = -5.0,
    max     = 5.0,
    value   = 0.0,
    step    = 0.1
  )
  return(control)
}

# outsiders distance vertical expansion
zigguratoutsiders_expand_vert <- function() {
  control<-sliderInput(
    inputId = "zigguratoutsiders_expand_vert",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_OUTSIDERS_DISPLACE_COMPONENT__VERT")),
    min     = -5.0,
    max     = 5.0,
    value   = 0.0,
    step    = 0.1
  )
  return(control)
}

# outsider boxes expansion
zigguratoutsiders_separation_expand <- function() {
  control<-sliderInput(
    inputId = "zigguratoutsiders_separation_expand",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_OUTSIDERS_SEP_EXPAND")),
    min     = 0.25,
    max     = 5.0,
    value   = 1,
    step    = 0.25
  )
  return(control)
}

# outsider legend expansion
zigguratoutsiders_legend_expand <- function() {
  control<-sliderInput(
    inputId = "zigguratoutsiders_legend_expand",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_OUTSIDERS_LEGEND_EXPAND")),
    min     = 0.1,
    max     = 5.0,
    value   = 1,
    step    = 0.1
  )
  return(control)
}

# 2-shell tail distance expansion
zigguratKcore2TailVerticalSeparationControl <- function() {
  control<-sliderInput(
    inputId = "zigguratKcore2TailVerticalSeparation",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_KCORE2_TAIL_VERTICAL_SEPARATION_CONTROL")),
    min     = 0.5,
    max     = 5.0,
    value   = 1.0,
    step    = 0.5
  )
  return(control)
}

# 1-shell distance to max shell
zigguratKcore1TailDistToCoreControl <- function(name, description) {
  control<-sliderInput(
    inputId = paste0("zigguratKcore1TailDistToCore", name),
    label   = controlLabel(description),
    min     = 0.5,
    max     = 5.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# distancia de la cola kcore-1 al core central                DEPRECATED
# ziggurathoriz_kcoremax_tails_expand <- function() {
#   control<-sliderInput(
#     inputId = "ziggurathoriz_kcoremax_tails_expand",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_EDGE_TAIL_HORIZ_SEPARATION_CONTROL")),
#     min     = 0.5,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }

# Chain of specialists to 1-shell
zigguratkcore1specialists_leafs_vertical_separation <- function() {
  control<-sliderInput(
    inputId = "zigguratkcore1specialists_leafs_vertical_separation",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_K1_LEAFS_VERT_SEP")),
    min     = 0.1,
    max     = 5.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}


# Vertical separation of inner tails
zigguratInnerTailVerticalSeparationControl <- function() {
  control<-sliderInput(
    inputId = "zigguratInnerTailVerticalSeparation",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_INNER_TAIL_VERTICAL_SEPARATION_CONTROL")),
    min     = 0.2,
    max     = 5.0,
    value   = 1.0,
    step    = 0.2
  )
  return(control)
}

# fattail A horizontal expansion
zigguratfattailjumphorizA <- function() {
  control<-sliderInput(
    inputId = "zigguratfattailjumphorizA",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_FAT_TAIL_JUMP_HORIZ_A")),
    min     = 0.2,
    max     = 5.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# fattail A vertical expansion
zigguratfattailjumpvertA <- function() {
  control<-sliderInput(
    inputId = "zigguratfattailjumpvertA",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_FAT_TAIL_JUMP_VERT_A")),
    min     = -2.0,
    max     = 5.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# fattail B horizontal expansion
zigguratfattailjumphorizB <- function() {
  control<-sliderInput(
    inputId = "zigguratfattailjumphorizB",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_FAT_TAIL_JUMP_HORIZ_B")),
    min     = 0.2,
    max     = 5.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# fattail B vertical expansion
zigguratfattailjumpvertB <- function() {
  control<-sliderInput(
    inputId = "zigguratfattailjumpvertB",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_FAT_TAIL_JUMP_VERT_B")),
    min     = -2.0,
    max     = 5.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# specialist chain root distance horizontal expansion
zigguratroot_specialist_expand_horiz <- function() {
  control<-sliderInput(
    inputId = "zigguratroot_specialist_expand_horiz",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPECIALIST_ROOT_EXPAND_HORIZ")),
    min     = 0.1,
    max     = 3.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# specialist chain root distance to 2-shell horizontal expansion
zigguratroot_specialistskcore2_horiz <- function() {
  control<-sliderInput(
    inputId = "zigguratroot_specialistskcore2_horiz",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPECIALIST_2SHELL_EXPAND_HORIZ")),
    min     = 0.1,
    max     = 3.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# specialist chain root distance to 2-shell vertical expansion
zigguratroot_specialistskcore2_vert <- function() {
  control<-sliderInput(
    inputId = "zigguratroot_specialistskcore2_vert",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPECIALIST_2SHELL_EXPAND_VERT")),
    min     = 0.1,
    max     = 3.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# specialist chain boxes separation expand
zigguratroot_specialist_boxesseparation <- function() {
  control<-sliderInput(
    inputId = "zigguratroot_specialist_boxesseparation",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPECIALIST_BOXES_SEPARATION")),
    min     = 0.25,
    max     = 8,
    value   = 1,
    step    = 0.25
  )
  return(control)
}

# specialist chain root distance vertical expansion
zigguratroot_specialist_expand_vert <- function() {
  control<-sliderInput(
    inputId = "zigguratroot_specialist_expand_vert",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPECIALIST_ROOT_EXPAND_VERT")),
    min     = 0.1,
    max     = 3.0,
    value   = 1.0,
    step    = 0.1
  )
  return(control)
}

# etiqueta clan A
zigguratLabelGuildAControl<-function() {
  control<-textInput(
    inputId = "zigguratLabelGuildA",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_LABEL_GUILDA")),
    value   = strings$value("LABEL_ZIGGURAT_LABEL_GUILDA_DEFAULT")
  )
}

# Guild B label
zigguratLabelGuildBControl<-function() {
  control<-textInput(
    inputId = "zigguratLabelGuildB",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_LABEL_GUILDB")),
    value   = strings$value("LABEL_ZIGGURAT_LABEL_GUILDB_DEFAULT")
  )
}

# label sizes
zigguratLabelsSizeControl <- function(name, description, default) {
  control<-sliderInput(
    inputId = paste0("zigguratLabelsSize", name),
    label   = controlLabel(description),
    min     = 0,
    max     = 10.0,
    value   = default,
    step    = 0.5
  )
  return(control)
}

# Save ziggurat plot config parameters
zigguratsaveZigConfigControlFile <- function() {
  control<-downloadButton("zigguratsaveZigConfigFile",label = strings$value("LABEL_ZIGGURAT_SAVECONFIG_CONTROL"))
  return(control)
}

#Load ziggurat plot config parameters
zigguratloadZigConfigControlFile <- function() {
  control<-fileInput(
    inputId   = "zigguratloadZigConfigFile",
    accept    = c(".json"),
    label     = controlLabel(strings$value("LABEL_ZIGGURAT_LOADCONFIG_CONTROL")),
    multiple  = FALSE
  )
  return(control)
}

zigguratshowZigConfigControlFile <- function() {
  control<-checkboxInput(
    inputId = "zigguratshowZigConfigControlFile",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SHOW_CONFIG_FILE_CONTROL")),
    value   = FALSE
  )
  return(control)
}


zigguratSvgScaleFactorControl <- function() {
  control<-sliderInput(
    inputId = "zigguratSvgScaleFactor",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SVG_SCALE")),
    min     = 0.2,
    max     = 5,
    value   = 1,
    step    = 0.2
  )
  return(control)
}



# Crop top SVG
zigguratSVGup <- function() {
  control<-sliderInput(
    inputId = "zigguratSVGup",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SVG_UP")),
    min     = 0,
    max     = 50,
    value   = 0,
    step    = 5
  )
  return(control)
}

# Downlad diagram size
paperSizeControl <- function() {
  values<-1:6
  names(values)<-paste0("A", values)
  control<-selectInput(
    inputId   = "paperSize",
    label     = controlLabel(strings$value("LABEL_PAPER_SIZE_CONTROL")),
    choices   = values,
    selected  = 4,
    multiple  = FALSE
  )
  return(control)
}

# Ziggurat plot resolution
zigguratppiControl <- function() {
  values<-c(72, 96, 150, 300, 600)

  names(values)<-values
  control<-selectInput(
    inputId   = "zigguratppi",
    label     = controlLabel(strings$value("LABEL_RESOLUTION_SIZE_CONTROL")),
    choices   = values,
    selected  = 300,
    multiple  = FALSE
  )
  return(control)
}


# Ziggurat Plot file format
zigguratFileFormat <- function() {
  values<-c("png","jpg","eps","tiff","svg")
  names(values)<-values
  control<-selectInput(
    inputId   = "zigguratfileextension",
    label     = controlLabel(strings$value("LABEL_ZIGGURAT_DOWNLOAD_PLOT_FILE_FORMAT")),
    choices   = values,
    selected  = "png",
    multiple  = FALSE
  )
  return(control)
}

#Paper orientation
paperLandscape <- function() {
  control<-checkboxInput(
    inputId = "paperLandscape",
    label   = controlLabel(strings$value("LABEL_PAPER_ORIENTATION")),
    value   = TRUE
  )
  return(control)
}



zigguratDownloadControl <- function() {
  control<-downloadButton("zigguratDownload",label = strings$value("LABEL_PLOT_DOWNLOAD"))
  return(control)
}

zigguratsaveSVGControl <- function() {
  control<-downloadButton("zigguratsaveSVG",label = strings$value("LABEL_PLOT_SVG_DOWNLOAD"))
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