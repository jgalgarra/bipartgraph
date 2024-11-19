###############################################################################
# BipartGraph
#  
# Module         : global.R
# Description    : Global function and panels
#
###############################################################################
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
valPlottype <- c("chilopodograph","legacy","kcoreorder")

if (file.exists("data/refs/references.csv")){
  network_references <- read.csv("data/refs/references.csv")
  names(network_references) <- gsub("\\.","_",names(network_references))
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
unlink("tmpcode", recursive = TRUE)
unlink("tmppolar", recursive = TRUE)

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