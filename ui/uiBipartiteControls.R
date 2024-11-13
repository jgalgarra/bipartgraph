###############################################################################
# BipartGraph
#  
# Module         : uiBipartiteControls.R
# Description    : interactive bipartite controls
#                  
###############################################################################

library(shinyjs)
library(colourpicker)

bipartiteSvgScaleFactorControl <- function() {
  control<-sliderInput(
    inputId = "bipartiteSvgScaleFactor",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SVG_SCALE")),
    min     = 0.2,
    max     = 5,
    value   = 1,
    step    = 0.2
  )
  return(control)
}

# Increase/reduce horizontal gap
bipartiteGuildgapincreaseControl <- function() {
  control<-sliderInput(
    inputId = "bipartiteGuildgapincrease",
    label   = controlLabel(HTML(paste("&larr;",strings$value("LABEL_BIPARTITE_GUILD_GAP_INCREASE"),"&rarr;"))),
    min     = -80,
    max     = 200,
    value   = 0,
    step    = 20
  )
  return(control)
}

bipartiteNodeRescale <- function() {
  control<-sliderInput(
    inputId = "bipartiteNodeRescale",
    label   = controlLabel(strings$value("LABEL_BIPARTITE_NODE_RESCALE")),
    min     = 0.2,
    max     = 4,
    value   = 1,
    step    = 0.2
  )
  return(control)
}

#Plot type
bipartitePlottype <- function(){
  control <- radioButtons("bipartitePlottype", HTML(paste("<span class='controlLabel'>",
                                                          strings$value("LABEL_BIPARTITE_PLOTTYPE"),"</span>")),
                          choices = valPlottype,
                          selected = "chilopodograph"
  )
  return(control)
}

# Crop top SVG
bipartiteSVGup <- function() {
  control<-sliderInput(
    inputId = "bipartiteSVGup",
    label   = controlLabel(HTML(paste("&darr;",strings$value("LABEL_ZIGGURAT_SVG_UP"),"&uarr;"))),
    min     = -50,
    max     = 50,
    value   = 0,
    step    = 10
  )
  return(control)
}

# Crop right SVG
bipartiteSVGright <- function() {
  control<-sliderInput(
    inputId = "bipartiteSVGright",
    label   = controlLabel(HTML(paste("&larr;",strings$value("LABEL_ZIGGURAT_SVG_RIGHT"),"&rarr;"))),
    min     = -50,
    max     = 50,
    value   = 0,
    step    = 10
  )
  return(control)
}

# 
# # hide links
# bipartitePaintLinksControl <- function() {
#   control<-checkboxInput(
#     inputId = "bipartitePaintLinks",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_PAINT_LINKS_CONTROL")),
#     value   = TRUE
#   )
#   return(control)
# }
# 
# # Show labels
# bipartiteDisplayLabelsControl <- function() {
#   control<-checkboxInput(
#     inputId = "bipartiteDisplayLabels",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_DISPLAY_LABELS_CONTROL")),
#     value   = TRUE
#   )
#   return(control)
# }
# 
# # DEPRECATED
# bipartiteFlipResultsControl <- function() {
#   control<-checkboxInput(
#     inputId = "bipartiteFlipResults",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_FLIP_RESULT_CONTROL")),
#     value   = FALSE
#   )
#   return(control)
# }
# 
# # Outsiders 
# bipartitePaintOutsidersControl <- function() {
#   control<-checkboxInput(
#     inputId = "bipartitePaintOutsiders",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_PAINT_OUTSIDERS_CONTROL")),
#     value   = TRUE
#   )
#   return(control)
# }
# 
# # Spline control
# bipartiteUseSplineControl <- function() {
#   control<-checkboxInput(
#     inputId = "bipartiteUseSpline",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_USE_SPLINE_CONTROL")),
#     value   = TRUE
#   )
#   return(control)
# }
# 
# # Spline points number control
# bipartiteSplinePointsControl <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteSplinePoints",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPLINE_POINTS_CONTROL")),
#     min     = 5,
#     max     = 50,
#     value   = 10,
#     step    = 5
#   )
#   return(control)
# }
# 
# # Aspect ratio for the SVG            DEPRECATED
# bipartiteAspectRatioControl <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteAspectRatio",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_ASPECT_RATIO_CONTROL")),
#     min     = 0.1,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # Expand horizontal separation among inner bipartites
# bipartiteHopx <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteHopx",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_HOPX")),
#     min     = 0.6,
#     max     = 4.0,
#     value   = 1.0,
#     step    = 0.2
#   )
#   return(control)
# }
# 
# # Transparency
# bipartiteAlphaLevelControl <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteAlphaLevel",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_ALPHA_LEVEL_CONTROL")),
#     min     = 0.1,
#     max     = 1.0,
#     value   = 0.2,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # Color picker
# bipartiteColorControl <- function(name, description, default) {
#   control <- colourInput(
#     paste0("bipartiteColor" , name),
#     controlLabel(description),
#     value = default
#   )
#   return(control)
# }
# 
# # Link transparency
# bipartiteAlphaLevelLinkControl <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteAlphaLevelLink",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_ALPHA_LEVEL_LINK_CONTROL")),
#     min     = 0.1,
#     max     = 1.0,
#     value   = 0.5,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # link width
# bipartiteLinkSizeControl <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteLinkSize",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_LINK_SIZE_CONTROL")),
#     min     = 0.1,
#     max     = 2.0,
#     value   = 0.3,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # Weight aggregation
# bipartiteweighted_links <- function() {
#   control <- selectInput(inputId = "bipartiteweighted_links", label = controlLabel(strings$value('LABEL_ZIGGURAT_LINKS_WEIGHT_CONTROL')),
#                          selected = "none",
#                          weightchoices)
#   return(control)
# }
# 
# # core box width
# bipartiteCoreBoxSizeControl <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteCoreBoxSize",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_COREBOX_SIZE_CONTROL")),
#     min     = 0.0,
#     max     = 1.0,
#     value   = 0.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # 
# bipartiteYDisplaceControlS <- function(name, description)
# {
#   control<-sliderInput(
#     inputId = paste0("bipartiteYDisplaceS", name, description),
#     label   = controlLabel(HTML(paste0("&darr;&nbsp;",description,"-shell ","&uarr;"))),
#     ticks = FALSE,
#     min     = -2.0,
#     max     = 2.0,
#     value   = 0.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # Inner bipartites box height expansion, ecluded kcoremax
# bipartiteHeightExpandControl <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteHeightExpand",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_HEIGHT_EXPAND_CONTROL")),
#     min     = 0.5,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # 1-shell nodes area expand
# bipartite1shellExpandControl <- function() {
#   control<-sliderInput(
#     inputId = "bipartite1shellExpandControl",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_1SHELL_EXPAND_CONTROL")),
#     min     = 0.5,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# 
# # Coremax height expand control
# bipartiteCoreMaxHExp <- function(){
#   control<-sliderInput(
#     inputId = "bipartiteCoreMaxHExp",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_COREMAX_HEIGHT_EXPAND_CONTROL")),
#     min     = 0.5,
#     max     = 2.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # Coremax width expand control
# bipartiteCoreMaxWExp <- function(){
#   control<-sliderInput(
#     inputId = "bipartiteCoreMaxWExp",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_COREMAX_WIDTH_EXPAND_CONTROL")),
#     min     = 0.5,
#     max     = 2.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # outsiders distance horizontal expansion
# bipartiteoutsiders_expand_horiz <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteoutsiders_expand_horiz",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_OUTSIDERS_DISPLACE_COMPONENT__HORIZ")),
#     min     = -5.0,
#     max     = 5.0,
#     value   = 0.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # outsiders distance vertical expansion
# bipartiteoutsiders_expand_vert <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteoutsiders_expand_vert",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_OUTSIDERS_DISPLACE_COMPONENT__VERT")),
#     min     = -5.0,
#     max     = 5.0,
#     value   = 0.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # outsider boxes expansion
# bipartiteoutsiders_separation_expand <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteoutsiders_separation_expand",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_OUTSIDERS_SEP_EXPAND")),
#     min     = 0.25,
#     max     = 5.0,
#     value   = 1,
#     step    = 0.25
#   )
#   return(control)
# }
# 
# # outsider legend expansion
# bipartiteoutsiders_legend_expand <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteoutsiders_legend_expand",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_OUTSIDERS_LEGEND_EXPAND")),
#     min     = 0.1,
#     max     = 5.0,
#     value   = 1,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # 2-shell tail distance expansion
# bipartiteKcore2TailVerticalSeparationControl <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteKcore2TailVerticalSeparation",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_KCORE2_TAIL_VERTICAL_SEPARATION_CONTROL")),
#     min     = 0.5,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.5
#   )
#   return(control)
# }
# 
# # 1-shell distance to max shell
# bipartiteKcore1TailDistToCoreControl <- function(name, description) {
#   control<-sliderInput(
#     inputId = paste0("bipartiteKcore1TailDistToCore", name),
#     label   = controlLabel(description),
#     min     = 0.5,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # distancia de la cola kcore-1 al core central                DEPRECATED
# # bipartitehoriz_kcoremax_tails_expand <- function() {
# #   control<-sliderInput(
# #     inputId = "bipartitehoriz_kcoremax_tails_expand",
# #     label   = controlLabel(strings$value("LABEL_ZIGGURAT_EDGE_TAIL_HORIZ_SEPARATION_CONTROL")),
# #     min     = 0.5,
# #     max     = 5.0,
# #     value   = 1.0,
# #     step    = 0.1
# #   )
# #   return(control)
# # }
# 
# # Chain of specialists to 1-shell
# bipartitekcore1specialists_leafs_vertical_separation <- function() {
#   control<-sliderInput(
#     inputId = "bipartitekcore1specialists_leafs_vertical_separation",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_K1_LEAFS_VERT_SEP")),
#     min     = 0.1,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# 
# # Vertical separation of inner tails
# bipartiteInnerTailVerticalSeparationControl <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteInnerTailVerticalSeparation",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_INNER_TAIL_VERTICAL_SEPARATION_CONTROL")),
#     min     = 0.2,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.2
#   )
#   return(control)
# }
# 
# # fattail A horizontal expansion
# bipartitefattailjumphorizA <- function() {
#   control<-sliderInput(
#     inputId = "bipartitefattailjumphorizA",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_FAT_TAIL_JUMP_HORIZ_A")),
#     min     = 0.2,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # fattail A vertical expansion
# bipartitefattailjumpvertA <- function() {
#   control<-sliderInput(
#     inputId = "bipartitefattailjumpvertA",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_FAT_TAIL_JUMP_VERT_A")),
#     min     = -2.0,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # fattail B horizontal expansion
# bipartitefattailjumphorizB <- function() {
#   control<-sliderInput(
#     inputId = "bipartitefattailjumphorizB",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_FAT_TAIL_JUMP_HORIZ_B")),
#     min     = 0.2,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # fattail B vertical expansion
# bipartitefattailjumpvertB <- function() {
#   control<-sliderInput(
#     inputId = "bipartitefattailjumpvertB",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_FAT_TAIL_JUMP_VERT_B")),
#     min     = -2.0,
#     max     = 5.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # specialist chain root distance horizontal expansion
# bipartiteroot_specialist_expand_horiz <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteroot_specialist_expand_horiz",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPECIALIST_ROOT_EXPAND_HORIZ")),
#     min     = 0.1,
#     max     = 3.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # specialist chain root distance to 2-shell horizontal expansion
# bipartiteroot_specialistskcore2_horiz <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteroot_specialistskcore2_horiz",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPECIALIST_2SHELL_EXPAND_HORIZ")),
#     min     = 0.1,
#     max     = 3.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # specialist chain root distance to 2-shell vertical expansion
# bipartiteroot_specialistskcore2_vert <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteroot_specialistskcore2_vert",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPECIALIST_2SHELL_EXPAND_VERT")),
#     min     = 0.1,
#     max     = 3.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # specialist chain boxes separation expand
# bipartiteroot_specialist_boxesseparation <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteroot_specialist_boxesseparation",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPECIALIST_BOXES_SEPARATION")),
#     min     = 0.25,
#     max     = 8,
#     value   = 1,
#     step    = 0.25
#   )
#   return(control)
# }
# 
# # specialist chain root distance vertical expansion
# bipartiteroot_specialist_expand_vert <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteroot_specialist_expand_vert",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_SPECIALIST_ROOT_EXPAND_VERT")),
#     min     = 0.1,
#     max     = 3.0,
#     value   = 1.0,
#     step    = 0.1
#   )
#   return(control)
# }
# 
# # label sizes
# bipartiteLabelsSizeControl <- function(name, description, default) {
#   control<-sliderInput(
#     inputId = paste0("bipartiteLabelsSize", name),
#     label   = controlLabel(description),
#     min     = 0,
#     max     = 10.0,
#     value   = default,
#     step    = 0.5
#   )
#   return(control)
# }
# 
# # Save bipartite plot config parameters
# bipartitesaveZigConfigControlFile <- function() {
#   control<-downloadButton("bipartitesaveZigConfigFile",label = strings$value("LABEL_ZIGGURAT_SAVECONFIG_CONTROL"))
#   return(control)
# }
# 
# #Load bipartite plot config parameters
# bipartiteloadZigConfigControlFile <- function() {
#   control<-fileInput(
#     inputId   = "bipartiteloadZigConfigFile",
#     accept    = c(".json"),
#     label     = controlLabel(strings$value("LABEL_ZIGGURAT_LOADCONFIG_CONTROL")),
#     multiple  = FALSE
#   )
#   return(control)
# }
# 
# bipartiteshowZigConfigControlFile <- function() {
#   control<-checkboxInput(
#     inputId = "bipartiteshowZigConfigControlFile",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_SHOW_CONFIG_FILE_CONTROL")),
#     value   = FALSE
#   )
#   return(control)
# }
# 
# 
# 
# #Set core max order
# bipartitekcoremaxorder <- function(){
#    control <- radioButtons("orderkcoremaxby", HTML(paste("<span class='controlLabel'>",strings$value("LABEL_ZIGGURAT_MAXCORE_ORDER"),"</span>")),
#                           choices = valordkcoremax,
#                           selected = "kradius"
#                           )
#   return(control)
# }
# 
# # bipartitekcoremaxorder <- function(){
# #   control<- checkboxInput(
# #     inputId = "orderkcoremaxby",
# #     label   = controlLabel(strings$value("LABEL_ZIGGURAT_MAXCORE_ORDER")),
# #     value   = FALSE
# #   )
# #   return(control)
# # }
# 
# # Downlad diagram size
# paperSizeControl <- function() {
#   values<-1:6
#   names(values)<-paste0("A", values)
#   control<-selectInput(
#     inputId   = "paperSize",
#     label     = controlLabel(strings$value("LABEL_PAPER_SIZE_CONTROL")),
#     choices   = values,
#     selected  = 4,
#     multiple  = FALSE
#   )
#   return(control)
# }
# 
# # Ziggurat plot resolution
# bipartiteppiControl <- function() {
#   values<-c(72, 96, 150, 300, 600)
# 
#   names(values)<-values
#   control<-selectInput(
#     inputId   = "bipartiteppi",
#     label     = controlLabel(strings$value("LABEL_RESOLUTION_SIZE_CONTROL")),
#     choices   = values,
#     selected  = 300,
#     multiple  = FALSE
#   )
#   return(control)
# }
# 
# 
# # Ziggurat Plot file format
# bipartiteFileFormat <- function() {
#   values<-c("png","jpg","eps","tiff","svg")
#   names(values)<-values
#   control<-selectInput(
#     inputId   = "bipartitefileextension",
#     label     = controlLabel(strings$value("LABEL_ZIGGURAT_DOWNLOAD_PLOT_FILE_FORMAT")),
#     choices   = values,
#     selected  = "png",
#     multiple  = FALSE
#   )
#   return(control)
# }
# 
# #Paper orientation
# paperLandscape <- function() {
#   control<-checkboxInput(
#     inputId = "paperLandscape",
#     label   = controlLabel(strings$value("LABEL_PAPER_ORIENTATION")),
#     value   = TRUE
#   )
#   return(control)
# }
# 
# bipartiteDownloadControl <- function() {
#   control<-downloadButton("bipartiteDownload",label = strings$value("LABEL_PLOT_DOWNLOAD"))
#   return(control)
# }
# 
# bipartitesaveSVGControl <- function() {
#   control<-downloadButton("bipartitesaveSVG",label = strings$value("LABEL_PLOT_SVG_DOWNLOAD"))
#   return(control)
# }
# 
# 
# bipartitecodeDownloadControl <- function() {
#   control<-downloadButton("bipartitecodeDownload",label = strings$value("LABEL_ZIGGURAT_CODE_DOWNLOAD"))
#   return(control)
# }
# 
# # control generico para seleccion de color
# bipartiteBckgdColorControl <- function() {
#   control <- colourInput(
#     "bipartiteBckgdColorControl",
#     controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_BACKGROUND_COLOR")),
#     value = "#FFFFFF"
#   )
#   return(control)
# }
# 
# # Aspect ratio of  the printable plot
# bipartiteAspectRatio <- function() {
#   control<-sliderInput(
#     inputId = "bipartiteAspectRatio",
#     label   = controlLabel(strings$value("LABEL_ZIGGURAT_CONFIG_ASPECT_RATIO")),
#     min     = 0.2,
#     max     = 2,
#     value   = 1,
#     step    = 0.1
#   )
#   return(control)
# }