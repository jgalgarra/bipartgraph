# Installation script for BipartGraph.

print(Sys.getenv("R_LIBS_USER"))
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
print(.libPaths())

list.of.packages <- c('ggplot2', 'colourpicker','svglite',
                      'grid','igraph', 'bipartite','jsonlite',
                      'gridExtra', 'vegan', 'sna', 'fields','DT',
                      'shiny', 'shinythemes', 'shinyjs')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.us.r-project.org")
install.packages("devtools",repos="http://cran.us.r-project.org")
library("devtools")
install_github('jgalgarra/kcorebip',force=TRUE)
print("This is the end, my friend. Installation completed")
Sys.sleep(30)
