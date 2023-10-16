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
weightchoices <-  c("none" = "none","ln" = "ln","log10" = "log10","sqrt" = "sqrt")
if (file.exists("data/references.csv")){
  network_references <- read.csv("data/references.csv")
  names(network_references) <- gsub("\\.","_",names(network_references))
}


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