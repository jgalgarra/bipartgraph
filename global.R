###############################################################################
# BipartGraph
#  
# Module         : global.R
# Description    : Global function and panels
#
###############################################################################
source("strings.R", encoding="UTF-8")

# Search data files
dataDir         <- "data"
fileExtension   <- ".csv"
dataFilePattern <- paste0("*.*", fileExtension)

MAX_NUM_CORES <- 20
weightchoices <-  c("none" = "none","ln" = "ln","log10" = "log10","sqrt" = "sqrt")
outsidercontrols <- c("zigguratPaintOutsiders","zigguratoutsiders_expand_horiz","zigguratoutsiders_expand_vert",
                      "zigguratoutsiders_separation_expand","zigguratoutsiders_legend_expand")
specialistcontrols <- c("zigguratroot_specialist_expand_horiz","zigguratroot_specialistskcore2_horiz",
                        "zigguratroot_specialistskcore2_vert","zigguratroot_specialist_boxesseparation",
                        "zigguratroot_specialist_expand_vert","zigguratkcore1specialists_leafs_vertical_separation")
tailcontrols <- c("zigguratKcore2TailVerticalSeparation","zigguratKcore1TailDistToCore","zigguratKcore1TailDistToCore1",
                  "zigguratKcore1TailDistToCore2",
                  "zigguratInnerTailVerticalSeparation","zigguratfattailjumphorizA","zigguratfattailjumpvertA",
                  "zigguratfattailjumphorizB","zigguratfattailjumpvertB")
weightcontrols <- c("zigguratweighted_links")
# Order kcoremax
valordkcoremax <- c("kradius","kdegree")
# Bipartite plot types
valPlottype <- c("chilopod","legacy","kcoreorder")
# Legend position
valShowLegendValue <<- c("TOP","BOTTOM","HIDE")
# Order matrix
valordmatrix <- c("kradius","kdegree","degree")
# Data file field separator
valorseparator <- c(",",";","\t")
# Standard static plot width
static_plot_width <- 8
# Standard plot resolution
standard_ppi <- 300
# SVG jump size in pixels
svg_jump_size <<- 50
# SVG displacement orientation for bipartite plot
svg_disp_orientation <<- 'h'
# Printer formats
printer_formats <<- c("png","jpg","eps","tiff","svg","pdf")


# Copy the default colors file if the personal file does not exist
if (!file.exists("conf/labelcolors.csv")){
  file.copy("conf/default/labelcolors.csv", "conf/labelcolors.csv")
}
# Copy the default references file if the personal file does not exist
if (!file.exists("data/refs/my_references.csv")){
  file.copy("data/refs/references.csv", "data/refs/my_references.csv")
}

if (file.exists("data/refs/my_references.csv")){
  network_references <- read.csv("data/refs/my_references.csv")
  names(network_references) <- gsub("\\.","_",names(network_references))
  network_references$Reference <- iconv(network_references$Reference, from = "ISO-8859-1", to = "UTF-8")
}

get.edges<-igraph::get.edges
f <- "resources/controls_jsonfields.csv"
if (!file.exists(f)){
  print("FATAL ERROR, resources folder corrupted, reinstall bipartgraph package")
} else {
  controls_jsonfields <- read.csv(f, sep=";")
}
# Remove global ziggurat colors data frame
if (exists("labelcolors"))
  rm("labelcolors")
# Remove temporary report files
dirreports <- 'www/reports'
delfiles <- c(dir(path=dirreports ,pattern="*.html"),dir(path=dirreports ,pattern="*.svg"))
file.remove(file.path(dirreports, delfiles))

unlink("analysis_indiv", recursive = TRUE)
unlink("plot_results", recursive = TRUE)
unlink("tmpcode", recursive = TRUE)
unlink("tmppolar", recursive = TRUE)
unlink("www/reports/*.*", recursive = FALSE)

# New group header
groupHeader<-function(text, image) {
  header<-tags$div(
    class="groupHeader",
    tags$span(
      class="groupHeaderIcon",
      tags$img(src=paste0("images/", image))
    ),
    tags$span(
      class="groupHeaderText",
      tags$h5(text)
    )
  )
  return(header)
}

# Control label
controlLabel <- function(text) {
  label<-tags$span(
    class="controlLabel",
    text
  )
  return(label)
}

# Links label
linkLabel <- function(text, img) {
  label<-tags$span(
    class="linkLabel",
    tags$img(src=paste0("images/", img)),
    text
  )
  return(label)
}

# Print file format
PrintFileFormat <- function(plotlabel) {
  values <- printer_formats
  names(values)<-values
  control<-selectInput(
    inputId   = paste0(plotlabel,"fileextension"),
    label     = controlLabel(strings$value("LABEL_ZIGGURAT_DOWNLOAD_PLOT_FILE_FORMAT")),
    choices   = values,
    selected  = "png",
    multiple  = FALSE
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

#Paper orientation
paperLandscape <- function() {
  control<-checkboxInput(
    inputId = "paperLandscape",
    label   = controlLabel(strings$value("LABEL_PAPER_ORIENTATION")),
    value   = TRUE
  )
  return(control)
}


# Ziggurat plot resolution
PrintppiControl <- function(plotlabel) {
  values<-c(72, 96, 150, 300, 600)
  names(values)<-values
  control<-selectInput(
    inputId   = paste0(plotlabel,"ppi"),
    label     = controlLabel(strings$value("LABEL_RESOLUTION_SIZE_CONTROL")),
    choices   = values,
    selected  = 300,
    multiple  = FALSE
  )
  return(control)
}

PrintDownloadControl <- function(plotlabel) {
  control<-downloadButton(paste0(plotlabel,"Download"),label = strings$value("LABEL_PLOT_DOWNLOAD"))
  return(control)
}

saveSVGControl <- function(plotlabel) {
  control<-downloadButton(paste0(plotlabel,"saveSVG"),label = strings$value("LABEL_PLOT_SVG_DOWNLOAD"))
  return(control)
}

