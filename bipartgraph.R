###############################################################################
# BipartGraph
#  
# Module         : bipartgraph.R
# Description    : Application launcher
#
###############################################################################

source("strings.R", encoding="UTF-8")
source("global.R", encoding="UTF-8")
library(shiny)
library(gridExtra)
library(grDevices)
library(gtable)
library(grid)
library(DT)
library(kcorebip)
library(shinythemes)
library(ggtext)
library(rlang)


fconf <- "conf/CONFIG.txt"
# Copy default config file
if (!file.exists(fconf)){
  file.copy("conf/default/CONFIG.txt", "conf/CONFIG.txt")
}
if (file.exists(fconf)){
  config_params <- read.table(fconf, stringsAsFactors=FALSE, header = TRUE, sep = ";")
  strings<<-LocalizedStrings(config_params$LANGUAGE[1])
  static_strings <<- read.csv("resources/strings.csv")
  czA1 <<- config_params$ColorGuildA1[1]
  czA2 <<- config_params$ColorGuildA2[1]
  czB1 <<- config_params$ColorGuildB1[1]
  czB2 <<- config_params$ColorGuildB2[1]
  labelA <<- config_params$LabelA[1]
  labelB <<- config_params$LabelB[1]
  shinyport <<- config_params$PORT
  WikipediaSubdomain <<- config_params$WikipediaSubdomain
} else {
  strings<<-LocalizedStrings("en")
  czA1 <<- "#4169E1"
  czA2 <<- "#00008B"
  czB1 <<- "#F08080"
  czB2 <<- "#FF0000"
  labelA <<- strings$value("LABEL_ZIGGURAT_LABEL_GUILDA_DEFAULT")
  labelB <<- strings$value("LABEL_ZIGGURAT_LABEL_GUILDB_DEFAULT")
  shinyport <<- 8080
}

# Launch Application
runApp(
  appDir        = ".",
  port          = config_params$PORT,
  host          = "0.0.0.0",
  display.mode  = "normal",
  launch.browser = TRUE
)