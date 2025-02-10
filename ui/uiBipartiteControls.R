###############################################################################
# BipartGraph
#  
# Module         : uiBipartiteControls.R
# Description    : interactive bipartite controls
#                  
###############################################################################

library(shinyjs)
library(colourpicker)

bipartiteZoomSVGControl <- function() {
  control<-sliderInput(
    inputId = "bipartiteZoomSVG",
    label   = controlLabel(strings$value("LABEL_BIPARTITE_ZOOM_SVGPLOT")),
    min     = 0.2,
    max     = 2,
    value   = 1,
    step    = 0.1
  )
  return(control)
}


# Increase/reduce horizontal gap
bipartiteGuildgapincreaseControl <- function() {
  control<-sliderInput(
    inputId = "bipartiteGuildgapincrease",
    #label   = controlLabel(HTML(paste("&larr;",strings$value("LABEL_BIPARTITE_GUILD_GAP_INCREASE"),"&rarr;"))),
    label   = controlLabel(HTML(strings$value("LABEL_BIPARTITE_GUILD_GAP_INCREASE"))),
    
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
                          selected = "chilopod"
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
bipartiteLabelsSizeControl <- function(description, default) {
  control<-sliderInput(
    inputId = paste0("bipartiteLabelsSize"),
    label   = controlLabel(description),
    min     = 0,
    max     = 10.0,
    value   = default,
    step    = 0.5
  )
  return(control)
}


bipartiteTextRescaleControl <- function() {
  control<-sliderInput(
    inputId = "bipartiteTextRescale",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_TEXT_SCALE")),
    min     = 0.2,
    max     = 5,
    value   = 1,
    step    = 0.2
  )
  return(control)
}


bipartitesaveSVGControl <- function() {
  control<-downloadButton("bipartitesaveSVG",label = strings$value("LABEL_PLOT_SVG_DOWNLOAD"))
  return(control)
}

# Show title in printed file
bipartiteShowTitleControl <- function() {
  control<-checkboxInput(
    inputId = "bipartiteShowTitle",
    label   = controlLabel(strings$value("LABEL_BIPARTITE_SHOW_TITLE")),
    value   = FALSE
  )
  return(control)
}

# Legend position
valShowLegend <<- c(strings$value("LABEL_BIPARTITE_SHOW_LEGEND_TOP"),strings$value("LABEL_BIPARTITE_SHOW_LEGEND_BOTTOM"),
                    strings$value("LABEL_BIPARTITE_SHOW_LEGEND_HIDE"))

bipartiteShowLegendControl <- function(){
  control <- radioButtons("bipartiteShowLegend", HTML(paste("<span class='controlLabel'>",
                                                          strings$value("LABEL_BIPARTITE_SHOW_LEGEND"),"</span>")),
                          choiceNames = valShowLegend,
                          choiceValues= valShowLegendValue,
                          selected = "TOP"
  )
  return(control)
}

# Rotate plot 90 degrees
bipartiteVerticalLayoutControl <- function() {
  control<-checkboxInput(
    inputId = "bipartiteVerticalLayout",
    label   = controlLabel(strings$value("LABEL_BIPARTITE_VERTICAL_LAYOUT")),
    value   = FALSE
  )
  return(control)
}
# bipartite plot resolution
bipartiteppiControl <- function() {
  values<-c(72, 96, 150, 300, 600)
  
  names(values)<-values
  control<-selectInput(
    inputId   = "bipartiteppi",
    label     = controlLabel(strings$value("LABEL_RESOLUTION_SIZE_CONTROL")),
    choices   = values,
    selected  = 300,
    multiple  = FALSE
  )
  return(control)
}


# bipartite Plot file format
bipartiteDownloadControl <- function() {
  control<-downloadButton("bipartiteDownload",label = strings$value("LABEL_PLOT_DOWNLOAD"))
  return(control)
}


bipartitecodeDownloadControl <- function() {
  control<-downloadButton("bipartitecodeDownload",label = strings$value("LABEL_ZIGGURAT_CODE_DOWNLOAD"))
  return(control)
}

# Save bipartite plot config parameters
bipartitesaveBipConfigFileControl <- function() {
  control<-downloadButton("bipartitesaveBipConfigFile",label = strings$value("LABEL_ZIGGURAT_SAVECONFIG_CONTROL"))
  return(control)
}

#Load bipartite plot config parameters
bipartiteloadBipConfigFileControl <- function() {
  control<-fileInput(
    inputId   = "bipartiteloadBipConfigFile",
    accept    = c(".json"),
    label     = controlLabel(strings$value("LABEL_ZIGGURAT_LOADCON")),
    multiple  = FALSE
  )
  return(control)
}

bipartiteshowBipConfigFileControl <- function() {
  control<-checkboxInput(
    inputId = "bipartiteshowBipConfigFile",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_SHOW_CONFIG_FILE_CONTROL")),
    value   = FALSE
  )
  return(control)
}

bipartiteOneColorControl <- function() {
  control<-checkboxInput(
    inputId = "bipartiteOneColor",
    label   = controlLabel(strings$value("LABEL_ZIGGURAT_ONE_COLOR")),
    value   = FALSE
  )
  return(control)
}
