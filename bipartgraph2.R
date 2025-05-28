###############################################################################
# BipartGraph
#  
# Module         : bipartgraph.R
# Description    : Application launcher
#
###############################################################################
rm(list = ls())
source("global.R", encoding="UTF-8")
library(gridExtra)
library(grDevices)
library(gtable)
library(grid)
library(DT)
library(kcorebip)
library(ggtext)
library(rlang)

library(shiny)
library(shinythemes)

jscode <- paste("`My screen resolution is: ${window.screen.width} * ${window.screen.height}`")
runjs(jscode)

fconf <- "conf/CONFIG.txt"
# Copy default config file
if (!file.exists(fconf)){
  file.copy("conf/default/CONFIG.txt", fconf)
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
  dir.create("plot_results/", showWarnings = FALSE)
}
tmpplots <<- "tmp"
# Remove tmp files of last run
unlink(tmpplots,recursive=TRUE)
reportplots <<- "www/reports/*.*"
# Remove tmp files of last run
unlink(reportplots,recursive=FALSE)

# Launch Application
runApp(
  appDir        = ".",
  port          = config_params$PORT,
  host          = "0.0.0.0",
  display.mode  = "normal",
  launch.browser = TRUE
)