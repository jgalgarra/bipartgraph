###############################################################################
# BipartGraph
#
# Module         : nonreactive.R
# Descriction    : Nonreactive functions for the Shiny server module
#
###############################################################################

# Show the detailed information of the selected node
showNodeDetails <- function(type, kcore, nodeDf) {
  rows    <- eval(parse(text="fluidRow()"))
  columns <- ""
  sizes   <- c(1, 2, 4, 1, 1, 1)
  
  # Create rows
  name    <- nodeDf[1, c("name_species")]
  label   <- nodeDf[1, c("label")]
  label   <- paste0("tags$a(\"", label, "\", href=\"", paste0("javascript:showWiki('", type, "',", label, ", '", name , "')"), "\")")
  kcore   <- paste0("\"", kcore, "\"")
  type    <- paste0("\"", type, "\"")
  name    <- paste0("\"", name, "\"")
  kradius <- paste0("\"", round(nodeDf[1, c("kradius")], 2), "\"")
  kdegree <- paste0("\"", round(nodeDf[1, c("kdegree")], 2), "\"")
  columns <- ""
  values  <- c(label, type, name, kcore, kradius, kdegree)
  for (i in 1:length(values)) {
    if (nchar(columns)>0) {
      columns<-paste0(columns, ", ")
    }
    columns <- paste0(columns, "column(", sizes[i], ", tags$small(", values[i], "))")
  }
  rows<-paste(rows, eval(parse(text=paste0("fluidRow(", columns, ")"))))
  
  return(rows)
}

# Shows the species information in Wikipedia
showWiki <- function(types, nodesData) {
  content<-""
  if (is.null(nodesData)) {
    content<-paste(content,eval(parse(text="fluidRow()")))
  } else {
    tab<-""
    tab<-paste0(tab, "tabsetPanel(")
    for (i in 1:nrow(nodesData)) {
      if (i>1) {
        tab<-paste0(tab, ", ")
      }
      type<-types[i]
      nodeData<-nodesData[i,]
      tab<-paste0(tab, "tabPanel(")
      tab<-paste0(tab, "\"", type, " [#", nodeData$nodeId, "]\"")
      tab<-paste0(tab, ", fluidRow(")
      tab<-paste0(tab, "column(12, ")
      tab<-paste0(tab, "tags$div(id=\"wikiDetail-", type, "-", nodeData$nodeId)
      tab<-paste0(tab, "\"")
      tab<-paste0(tab, ", class=\"wikiDetail\"")
      tab<-paste0(tab, ", \"", strings$value("MESSAGE_WIKI_LOADING"), "\"")
      #tab<-paste0(tab, "tags$h6(\"(información descargada de Wikipedia para el elemento ")
      #tab<-paste0(tab, nodeData$name)
      #tab<-paste0(tab, "...)\"")
      tab<-paste0(tab, "))))")
    }
    tab<-paste0(tab, ", id=\"wikiTabsetPanel\", type=\"pills\")")
    content<-paste(content, eval(parse(text=tab)))
  }
  return(content)
}

# Get the list of available network files
availableFilesList<-function() {
  # Get the list of files
  filesList<-list.files(path=dataDir, pattern=dataFilePattern)
  names(filesList)<-filesList
  # Add first entry
  empty<-c("")
  names(empty)<-c(strings$value("MESSAGE_SELECT_DATA_FILE_INPUT"))
  return(c(empty, filesList))
}

# Get details of available files
availableFilesDetails<-function(filesList) {
  # Columns with the deatils
  filesDetailsColumns<-c(strings$value("LABEL_AVAILABLE_FILES_DETAILS_NAME"), 
                         strings$value("LABEL_AVAILABLE_FILES_DETAILS_SIZE"), 
                         "", "", strings$value("LABEL_AVAILABLE_FILES_DETAILS_MODIFICATION_DATE"), 
                         "", strings$value("LABEL_AVAILABLE_FILES_DETAILS_ACCESS_DATE"))
  # drop empty entries
  filesList<-filesList[filesList!=""]
  # Get details
  if (length(filesList)>0) {
    # Get file sizes and adds a column with the file name
    filesDetails  <- file.info(paste0(dataDir, "/", filesList), extra_cols=FALSE)
    filesDetails  <- cbind(gsub(paste0(dataDir, "/"), "", rownames(filesDetails)), filesDetails)
  } else {
    # Creates a void data frame
    filesDetails<-data.frame(name=character(), size=integer(), isdir=logical(), mode=integer(), mtime=character(), ctime=character(), atime=character())
  }
  # Rename columns
  colnames(filesDetails)<-filesDetailsColumns
  # Drop unnamed columns
  filesDetails<-filesDetails[!(colnames(filesDetails) %in% c(""))]
  return(filesDetails)
}

# Get the configurable options to print a static plot
#   paperSize: 0-DINA0, 1-DINA1, ...
#   ppi: pixels per inch

calculateDiagramOptions<-function(paperSize, ppi, extension) {
  options<-list(paperSize=1, width=480, height=480, ppi=300, cairo=FALSE, ext=extension)
  # Dimensions in DIN and inches
  widths    <- c(t(sapply(c(841, 841), function(x) {x*(1/2)^(0:3)})))
  heights   <- c(t(sapply(c(1189, 1189), function(x) {x*(1/2)^(0:3)})))
  pdfSizes  <- data.frame(width=widths[2:7], height=heights[3:8])
  inchesmm  <- (1/25.4)
  inches    <- inchesmm*pdfSizes[paperSize,]
  # Type
  type      <- capabilities(c("cairo"))
  ext       <- capabilities(c("jpeg", "png", "eps", "tiff"))
  # Update values
  options$paperSize <- paperSize
  options$ppi       <- ppi
  options$width     <- inches$width*ppi
  options$height    <- inches$height*ppi
  options$cairo     <- type[c("cairo")]
  options$ext       <- ifelse(ext[c("png")], "png", ifelse(ext[c("jpeg")], "jpeg", ifelse(ext[c("tiff")], "tiff", "")))
  return(options)
}

# Check if resolution and paper size are valid
validateDiagramOptions<-function(options) {
  return(validate(
    need(
      options$ppi==600 && options$paperSize>3 || options$ppi==300 && options$paperSize>1 || options$ppi<300,
      paste0(strings$value("MESSAGE_PAPERSIZE_ERROR_1"), options$ppi, strings$value("MESSAGE_PAPERSIZE_ERROR_2"), options$paperSize, collapse="")
    )
  ))
}

# Print static plot to file
plotDiagram<-function(file, plot, options) {
  type<-ifelse(options$cairo, "cairo", "windows")
  pointsize<-12
  if(is.null(zgg$landscape_plot))
    zgg$landscape_plot <- TRUE
  if (zgg$landscape_plot){
    w <- options$height
    h <- options$width
  } else {
    h <- options$height
    w <- options$width
  }
  if (options$ext=="png") {
    png(filename=file, type=type, width=w, height=h, units="px", res=options$ppi, pointsize=pointsize)
  } else if (options$ext=="jpeg") {
    jpeg(filename=file, type=type, width=w, height=h, units="px", res=options$ppi, pointsize=pointsize)
  } else if (options$ext=="tiff") {
    tiff(filename=file, type=type, width=w, height=h, units="px", res=options$ppi, pointsize=pointsize)
  } else if (options$ext=="eps"){
    cairo_ps(filename=file, width=w, height=h, units="px", res=options$ppi, pointsize=pointsize)
  }
  plot(plot)
  dev.off()
}

# Print PDF
plotPDF<-function(file, ziggurat, polar, options) {
  type<-ifelse(options$cairo, "cairo", "windows")
  pointsize<-12
  
  # imprime el PDF
  pdf(file=file, width=options$width/options$ppi, height=options$height/options$ppi, pointsize=pointsize, onefile=TRUE)
  plot(ziggurat$plot)
  plot(polar["polar_plot"][[1]])
  plot(arrangeGrob(polar["histo_dist"][[1]], polar["histo_core"][[1]], polar["histo_degree"][[1]], nrow=1, ncol=3))
  dev.off()
}

# If the file has been analyzed and core max > 1 is stored in conf/safefiles.csv
searchsafefile <- function(fred="")
{
  datossafefiles <- data.frame("file" = c())
  if (file.exists("conf/datossafefiles.csv")){
    sfiles <<- read.table("conf/datossafefiles.csv",sep=";",header = TRUE)
    if (length(sfiles[toupper(sfiles$file) == toupper(fred),])>0)
      return(TRUE)
  }
  return(FALSE)
}
  
  # Search labels and colors
searchlabcols <- function(fred="")
{
  datoslabcol <- data.frame("file" = c(), "LabelGuildA" = c(), "LabelGuildB" = c(),
                              "ColorZigGuildA1" = c(), "ColorZigGuildA2" = c(),
                              "ColorZigGuildB1" = c(), "ColorZigGuildB2" = c())
  if (file.exists("conf/labelcolors.csv")){
      labelcolors <<- read.table("conf/labelcolors.csv",sep=";",header = TRUE)
  if (nrow(labelcolors[toupper(labelcolors$file) == toupper(fred),])>0){
      datoslabcol <- labelcolors[toupper(labelcolors$file) == toupper(fred),][1,]
    }
  }
  return(datoslabcol)
}

  #Aux function to add a parameter and reproduce the function call
addCallParam <- function(com,lpar,param,quoteparam = FALSE)
{
  if (quoteparam)
    com <- paste0(com," ,",param," = \"",lpar[param],"\"")
  else
    com <- paste0(com," ,",param," = ",lpar[param])
  return(com)
}
  

static_error_msg <- function(mykey){
  return(return(static_strings[static_strings$key==mykey,][config_params$LANGUAGE]))
}

# Check that configuration file contents are correct
validateconfigfile <- function(filedata){
    if (gsub(".csv","",filedata$filename)!=zgg$network_name){
      return(static_error_msg("MESSAGE_ERROR_JSON_NNAME"))
    }
    return("OK")
}
  

create_report <- function(input_file, output_file) {
  
  htmlsvg <- zgg$svg$html()
  cat(htmlsvg, file = paste0("www/reports/",gsub(" ","",zgg$network_name),".svg"))
  # Read the contents of the input file
  text <- readLines(input_file, warn = FALSE)
  
  modified_text <- gsub("STR_NETWORK_NAME", zgg$network_name, text)
  modified_text <- gsub("STR_GUILD_A", paste0("<span class='GuildTitle' style='color:",zgg$color_guild_a[2],"'>",zgg$name_guild_a,"</span >"), modified_text)
  modified_text <- gsub("STR_GUILD_B", paste0("<span class='GuildTitle' style='color:",zgg$color_guild_b[2],"'>",zgg$name_guild_b,"</span >"), modified_text)
  names_A <- ""
  labelsA <- names(zgg$result_analysis$matrix[1,])
  for (i in 1:length(labelsA))
    names_A <- paste(names_A,sprintf("%2d",i)," ",labelsA[i],"\n")
  names_B <- ""
  labelsB <- names(zgg$result_analysis$matrix[,1])
  for (i in 1:length(labelsB))
    names_B <- paste(names_B,sprintf("%2d",i)," ",labelsB[i],"\n")
  modified_text <- gsub("STR_SPECIES_A", paste0("<span class='GuildNamesList'  style='color:",zgg$color_guild_a[1],"'>",names_A,"</span>"), modified_text)
  modified_text <- gsub("STR_SPECIES_B", paste0("<span class='GuildNamesList'  style='color:",zgg$color_guild_b[1],"'>",names_B,"</span>"), modified_text)
  if (exists("network_references")){
     if (sum(network_references$ID==zgg$network_name)!=0)
        modified_text <- gsub("STR_REFERENCE", paste(network_references[network_references$ID==zgg$network_name,]$Reference,"<BR>",
                              network_references[network_references$ID==zgg$network_name,]$Locality_of_Studi), modified_text)
     else
        modified_text <- gsub("STR_REFERENCE"," ",modified_text)
  }
  else
    modified_text <- gsub("STR_REFERENCE"," ",modified_text)
  # Write the modified text to the output file
  writeLines(modified_text, con = output_file)
}
