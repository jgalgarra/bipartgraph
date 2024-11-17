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

# link width
bipartiteLinkSizeControl <- function() {
  control<-sliderInput(
    inputId = "bipartiteLinkSize",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_LINK_SIZE_CONTROL")),
    min     = 0.1,
    max     = 2.0,
    value   = 0.3,
    step    = 0.1
  )
  return(control)
}
# Color picker
bipartiteColorControl <- function(name, description, default) {
  control <- colourInput(
    paste0("bipartiteColor" , name),
    controlLabel(description),
    value = default
  )
  return(control)
}

# Link transparency
bipartiteAlphaLevelLinkControl <- function() {
  control<-sliderInput(
    inputId = "bipartiteAlphaLevelLink",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_ALPHA_LEVEL_LINK_CONTROL")),
    min     = 0.1,
    max     = 1.0,
    value   = 0.3,
    step    = 0.1
  )
  return(control)
}

# Weight aggregation
bipartiteweighted_links <- function() {
  control <- selectInput(inputId = "bipartiteweighted_links", label = controlLabel(strings$value('LABEL_ZIGGURAT_LINKS_WEIGHT_CONTROL')),
                         selected = "none",
                         weightchoices)
  return(control)
}

# Transparency
bipartiteAlphaLevelControl <- function() {
  control<-sliderInput(
    inputId = "bipartiteAlphaLevel",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_ALPHA_LEVEL_CONTROL")),
    min     = 0.1,
    max     = 1.0,
    value   = 0.2,
    step    = 0.1
  )
  return(control)
}

# label sizes
bipartiteLabelsSizeControl <- function(name, description, default) {
  control<-sliderInput(
    inputId = paste0("bipartiteLabelsSize", name),
    label   = controlLabel(description),
    min     = 0,
    max     = 10.0,
    value   = default,
    step    = 0.5
  )
  return(control)
}

