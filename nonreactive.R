###############################################################################
# BipartGraph
#
# Module         : nonreactive.R
# Descriction    : Nonreactive functions for the Shiny server module
#
###############################################################################

# Show the detailed information of the selected node
showNodeDetails <- function(type, kcore, nodeDf,network) {
  rows    <- eval(parse(text="fluidRow()"))
  columns <- ""
  sizes   <- c(1, 2, 4, 1, 1, 1)
  
  # Create rows
  name    <- nodeDf[1, c("name_species")]
  name    <- clean_species_names(name,network)
  label   <- nodeDf[1, c("label")]
  label   <- paste0("tags$a(\"", label, "\", href=\"", paste0("javascript:linktoWiki('", type, "',", label, ", '", name , "', '", WikipediaSubdomain , "')"), "\")")
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


# Print file format
PrintFileFormat <- function(plotlabel) {
  values<-c("png","jpg","eps","tiff","svg")
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


# Get the configurable options to print a static plot
#   paperSize: 0-DINA0, 1-DINA1, ...
#   ppi: pixels per inch

calculateDiagramOptions<-function(paperSize, ppi, extension, show_title, show_legend) {
  options<-list(paperSize=1, width=480, height=480, ppi=300, cairo=FALSE, ext=extension)
  # Dimensions in DIN and inches
  widths    <- c(t(sapply(c(841, 841), function(x) {x*(1/2)^(0:3)})))
  heights   <- c(t(sapply(c(1189, 1189), function(x) {x*(1/2)^(0:3)})))
  pdfSizes  <- data.frame(width=widths[2:7], height=heights[3:8])
  inchesmm  <- (1/25.4)
  inches    <- inchesmm*pdfSizes[paperSize,]
  # Type
  type      <- capabilities(c("cairo"))
  ext       <- capabilities(c("jpeg", "png", "eps", "tiff","svg"))
  # Update values
  options$paperSize <- paperSize
  options$ppi       <- ppi
  options$show_title <- show_title
  options$show_legend  <- show_legend
  options$width     <- inches$width*ppi
  options$height    <- inches$height*ppi
  options$cairo     <- type[c("cairo")]
  options$ext       <- ifelse(ext[c("png")], "png", ifelse(ext[c("jpeg")], "jpeg", ifelse(ext[c("tiff")], "tiff", ifelse(ext[c("svg")], "svg", ""))))
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
plotStaticDiagram<-function(file, plot, options, plottype, myenv=zgg) {
  type<-ifelse(options$cairo, "cairo", "windows")
  pointsize<-12
  mres = as.numeric(options$ppi)
  if (plottype=="bipartite"){
    if (!myenv$flip_results){
      w <- options$height * 0.85
      h <- options$width * 0.5
    } else {
      h <- options$height *0.85
      w <- options$width * myenv$tot_height/myenv$tot_width
    }
  }
  else if (plottype=="matrix"){
    w=myenv$plot_width*mres
    h=myenv$plot_height*mres
    if (myenv$flip_results){
      w = h
      h = myenv$plot_width*mres
    }
  }
  else if (plottype=="polar"){
    w=14*mres
    h=w
  }
  else if (plottype =="ziggurat"){
    if(is.null(zgg$landscape_plot)){
      myenv$landscape_plot <- TRUE
      options$height <- options$width
      
    }
    if (myenv$landscape_plot){
      w <- options$height
      h <- options$width
    } else {
      h <- options$height
      w <- options$width
    }

  }
  if (options$ext=="png") {
    png(filename=file, type=type, width=w, height=h, units="px", res=mres, pointsize=pointsize)
  } else if (options$ext=="jpg") {
    jpeg(filename=file, type=type, width=w, height=h, units="px", res=mres, pointsize=pointsize)
  } else if (options$ext=="tiff") {
    tiff(filename=file, type=type, width=w, height=h, units="px", res=mres, pointsize=pointsize)
  } else if (options$ext=="eps"){
    ggsave(filename = file,width=w/as.numeric(options$ppi), height=h/mres, fallback_resolution=options$ppi,
           plot = print(plot),
           device = cairo_ps)
  }else if (options$ext=="svg"){
    ggsave(filename = file,width=w/as.numeric(options$ppi), height=h/as.numeric(options$ppi))
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
  return(static_strings[static_strings$key==mykey,][config_params$LANGUAGE])
}

# Check that configuration file contents are correct
validateconfigfile <- function(jsondata,network_name,plottype){
  if (jsondata$style %in% valPlottype)
    fileplottype <- "bipartite"
  else
    fileplottype <- jsondata$style
  configname <- get_network_name(jsondata$filename)
  if (is.null(configname))
    return(static_error_msg("MESSAGE_ERROR_JSON_NNAME"))
  if (fileplottype!=plottype){
    return(static_error_msg("MESSAGE_ERROR_JSON_PLOTTYPE"))
  }
  if (configname!=network_name){
    return(static_error_msg("MESSAGE_ERROR_JSON_NNAME"))
  }
  return("OK")
}


create_zigg_report <- function(z,input_file, output_file) {
  htmlsvg <- z$svg$html()
  cat(htmlsvg, file = paste0("www/reports/zigg_",gsub(" ","",zgg$network_name),".svg"))
  # Read the contents of the input file
  text <- readLines(input_file, warn = FALSE)
  modified_text <- gsub("IMG_STR_NETWORK_FILE", paste0("zigg_",zgg$network_name), text)
  modified_text <- gsub("STR_NETWORK_NAME", zgg$network_name, modified_text)
  modified_text <- gsub("STR_PLOT_TYPE", "ZIGGURAT", modified_text)
  modified_text <- gsub("STR_GUILD_A", paste0("<span class='GuildTitle' style='color:",zgg$color_guild_a[2],"'>",zgg$name_guild_a,"</span >"), modified_text)
  modified_text <- gsub("STR_GUILD_B", paste0("<span class='GuildTitle' style='color:",zgg$color_guild_b[2],"'>",zgg$name_guild_b,"</span >"), modified_text)
  pastechar ="<br style='display: block; margin: 1px;'>"
  names_A <- ""
  labelsA <- clean_species_names(names(zgg$result_analysis$matrix[1,]),zgg$network_name)
  for (i in 1:length(labelsA))
    names_A <- paste(names_A,"<tr><td class='GuildNamesList'style='color:",zgg$color_guild_a[1],"'>",sprintf("%2d",i)," ",labelsA[i],"</td><tr>")
  names_B <- ""
  labelsB <- clean_species_names(names(zgg$result_analysis$matrix[,1]),zgg$network_name)
  for (i in 1:length(labelsB))
    names_B <- paste(names_B,"<tr><td class='GuildNamesList'style='color:",zgg$color_guild_b[1],"'>",sprintf("%2d",i)," ",labelsB[i],"</td><tr>")
  modified_text <- gsub("STR_SPECIES_A", paste0("<span class='GuildNamesList'  style='color:",zgg$color_guild_a[1],"'>",names_A,"</span>"), modified_text)
  modified_text <- gsub("STR_SPECIES_B", paste0("<span class='GuildNamesList'  style='color:",zgg$color_guild_b[1],"'>",names_B,"</span>"), modified_text)
  if (exists("network_references")){
    if (sum(network_references$ID==zgg$network_name)!=0)
      modified_text <- gsub("STR_REFERENCE", paste(network_references[network_references$ID==zgg$network_name,]$Reference,"&nbsp;",
                                                   network_references[network_references$ID==zgg$network_name,]$Locality_of_Study), modified_text)
    else
      modified_text <- gsub("STR_REFERENCE"," ",modified_text)
  }
  else
    modified_text <- gsub("STR_REFERENCE"," ",modified_text)
  # Write the modified text to the output file
  writeLines(modified_text, con = output_file)
}

get_network_name <- function(namefile){
  return(gsub(".CSV","",gsub(".csv","",namefile)))
}

clean_species_names <- function(listspecies,nnetwork){
  splabels <- gsub("\\."," ",listspecies)
  splabels <- gsub(nnetwork,"",splabels)
  splabels <- trimws(splabels)
  return <- splabels
}


create_static_report <- function(p, input_file, output_file, result_analysis, strGuildA,
                                 strGuildB, w = static_plot_width, h = static_plot_width, pwidth = 600, printplot = TRUE,
                                 myenv_argg = polar$polar_argg, myenv=polar, plottype = "polar"
                                 ) 
  {
  nname <- get_network_name(myenv_argg$filename)
  fileplot <- paste0(nname,"_",toupper(plottype))
  if (printplot){
    fileplot <- paste0(fileplot,".png")
    myoptions <- data.frame("width"=static_plot_width,"ppi"=standard_ppi,"ext"="png","cairo"=FALSE)
    if (plottype=="polar")
      mplot <- p$polar_plot
    if (plottype=="matrix"){
      mplot <- myenv$plot
      pwidth <- round(0.9*pwidth)
    }
    if (plottype=="ziggurat"){
      myenv$landscape_pot <- TRUE
      mplot <- myenv$plot
      myoptions$height <- 0.9*(16/9)*myoptions$width
      myoptions$height <- myoptions$height*myoptions$ppi/2
      myoptions$width <- myoptions$width*myoptions$ppi/2
    }
    plotStaticDiagram(paste0("www/reports/",fileplot),mplot,myoptions,plottype,myenv=myenv)
  }  
  else{
    if (plottype=="bipartite"){
      h <- w 
      plot <- bpp$plot
      if (!bpp$flip_results)
        h <- h/2
      else{
        w <- w/2
        pwidth <- (2/3)*pwidth
      }
    }
    if (plottype=="ziggurat"){
      plot <- zgg$plot
      h <- 0.9*w
    }
    fileplot <- paste0(fileplot,".svg")
    ggsave(filename = paste0("www/reports/",fileplot),width=w, height=h)
  }
    
  # Read the contents of the input file
  ftext <- readLines(input_file, warn = FALSE)
  modified_text <-paste(ftext,collapse="")
  modified_text <- gsub("IMG_STR_NETWORK_FILE", fileplot, modified_text)
  modified_text <- gsub("STR_NETWORK_NAME", nname, modified_text)
  modified_text <- gsub("STR_PLOT_TYPE", plottype, modified_text)
  modified_text <- gsub("IMG_STR_WIDTH", pwidth, modified_text)
  modified_text <- gsub("STR_GUILD_A", paste0("<span class='GuildTitle' style='color:",myenv_argg$color_guild_a[1],"'>",strGuildA,"</span >"), modified_text)
  modified_text <- gsub("STR_GUILD_B", paste0("<span class='GuildTitle' style='color:",myenv_argg$color_guild_b[1],"'>",strGuildB,"</span >"), modified_text)
  #pastechar ="<br style='display: block; margin: 1px;'>"
  namesA <- paste0("<span style='color:",myenv_argg$color_guild_a[1],"'>")
  for (i in 1:(result_analysis$num_guild_a-1))
    namesA <- paste0(namesA,i," ",
                  names(result_analysis$matrix[1,])[i],
                  ", ")
  namesA <- paste0(namesA,result_analysis$num_guild_a,
                   names(result_analysis$matrix[1,])[result_analysis$num_guild_a],"</span>")
  modified_text <- gsub("STR_SPECIES_A", namesA, modified_text)
  namesB <- paste0("<span style='color:",myenv_argg$color_guild_b[1],"'>")
  for (i in 1:(result_analysis$num_guild_b-1))
    namesB <- paste0(namesB,i," ",
                     names(result_analysis$matrix[,1])[i],
                     ", ")
  namesB <- paste0(namesB,result_analysis$num_guild_b,
                   names(result_analysis$matrix[,1])[result_analysis$num_guild_b],"</span>")
  modified_text <- gsub("STR_SPECIES_B", namesB, modified_text)
  if (exists("network_references")){
    if (sum(network_references$ID==nname)!=0)
      modified_text <- gsub("STR_REFERENCE", paste(network_references[network_references$ID==nname,]$Reference,"&nbsp;",
                                                   network_references[network_references$ID==nname,]$Locality_of_Study), modified_text)
    else
      modified_text <- gsub("STR_REFERENCE"," ",modified_text)
  }
  else
    modified_text <- gsub("STR_REFERENCE"," ",modified_text)
  writeLines(modified_text, con = output_file)
}

create_polar_report <- function(p, input_file, output_file, w = 10, h = 10) {
  
  fileplot <- gsub("_report.html",".svg",output_file)
  ggsave(filename = fileplot,width=w, height=h)
  
  nname <- get_network_name(p$polar_argg$filename)
  # Read the contents of the input file
  text <- readLines(input_file, warn = FALSE)
  modified_text <- gsub("IMG_STR_NETWORK_FILE", paste0("polar_",nname), text)
  modified_text <- gsub("STR_NETWORK_NAME", nname, modified_text)
  modified_text <- gsub("STR_PLOT_TYPE", "POLAR", modified_text)
  modified_text <- gsub("STR_GUILD_A", paste0("<span class='GuildTitle'>",p$polar_argg$glabels[1],"</span >"), modified_text)
  modified_text <- gsub("STR_GUILD_B", paste0("<span class='GuildTitle'>",p$polar_argg$glabels[2],"</span >"), modified_text)
  pastechar ="<br style='display: block; margin: 1px;'>"
  names_A <- ""
  labelsA <- clean_species_names(names(p$result_analysis$matrix[1,]),nname)
  for (i in 1:length(labelsA))
    names_A <- paste(names_A,"<tr><td class='GuildNamesList'>",sprintf("%2d",i)," ",labelsA[i],"</td><tr>")
  names_B <- ""
  labelsB <- clean_species_names(names(p$result_analysis$matrix[,1]),nname)
  for (i in 1:length(labelsB))
    names_B <- paste(names_B,"<tr><td class='GuildNamesList''>",sprintf("%2d",i)," ",labelsB[i],"</td><tr>")
  modified_text <- gsub("STR_SPECIES_A", paste0("<span class='GuildNamesList'>",names_A,"</span>"), modified_text)
  modified_text <- gsub("STR_SPECIES_B", paste0("<span class='GuildNamesList'>",names_B,"</span>"), modified_text)
  if (exists("network_references")){
    if (sum(network_references$ID==nname)!=0)
      modified_text <- gsub("STR_REFERENCE", paste(network_references[network_references$ID==nname,]$Reference,"&nbsp;",
                                                   network_references[network_references$ID==nname,]$Locality_of_Study), modified_text)
    else
      modified_text <- gsub("STR_REFERENCE"," ",modified_text)
  }
  else
    modified_text <- gsub("STR_REFERENCE"," ",modified_text)
  # Write the modified text to the output file
  writeLines(modified_text, con = output_file)
}

visibilityZigDispControl <- function(option="hide",inf,sup){
  if (exists("zgg")){
    if ((option=="hide") && (zgg$kcoremax==2)){
      shinyjs::hide("networkGuildALabel")
      shinyjs::hide("networkGuildBLabel")
    }
    if (option=="show"){
      shinyjs::show("networkGuildALabel")
      shinyjs::show("networkGuildBLabel")
    }
    for (j in (zgg$kcoremax):MAX_NUM_CORES){
      if (option=="hide"){
        shinyjs::hide(paste0("zigguratYDisplaceSA",j))
        shinyjs::hide(paste0("zigguratYDisplaceSB",j))
      }
      if (option=="show"){
        shinyjs::show(paste0("zigguratYDisplaceSA",j))
        shinyjs::show(paste0("zigguratYDisplaceSB",j))
      }
    }
  }
}

SwitchControls <- function(option="disable",lcontrols){
  if (exists("zgg")){
    for (i in lcontrols){
      if (option=="disable")
        shinyjs::disable(i)
      if (option=="enable")
        shinyjs::enable(i)
    }
  }
}


updckbx <- function(idchkbx,jsonvalue,session){
  updateCheckboxInput(
    session =  session,
    inputId =  idchkbx, 
    value = jsonvalue
  )
}

parseJSONConfig <- function(controls_jsonfields,session,json_data,plottype){
  if (plottype %in% valPlottype)
    myplottype <- "bipartite"
  else
    myplottype <- plottype
  controls_jsonfields <- controls_jsonfields[grepl(myplottype,controls_jsonfields$ControlName),]#|grepl("DataLabel",controls_jsonfields$ControlName)),]
  for (i in 1:nrow(controls_jsonfields)){
        if (controls_jsonfields$ControlType[i] == "slider")
          updateSliderInput(session, controls_jsonfields$ControlName[i],
                            value = as.numeric(json_data[controls_jsonfields$JSONfield[i]][[1]][controls_jsonfields$ListElement[i]])/as.numeric(controls_jsonfields$Divideby[i]))
        else if (controls_jsonfields$ControlType[i] == "checkbox"){
          etq = json_data[controls_jsonfields$JSONfield[i]][[1]][controls_jsonfields$ListElement[i]]
          updckbx(controls_jsonfields$ControlName[i],etq,session)
        }
        else if (controls_jsonfields$ControlType[i] == "radiobuttons"){
          etq = json_data[controls_jsonfields$JSONfield[i]][[1]][controls_jsonfields$ListElement[i]]
          
          updateRadioButtons(session, controls_jsonfields$ControlName[i],
                             choices = get(controls_jsonfields$Labels[i]),
                             selected = etq)
        }
        else if (controls_jsonfields$ControlType[i] == "textinput"){
          etq = json_data[controls_jsonfields$JSONfield[i]][[1]][controls_jsonfields$ListElement[i]]
          updateTextInput(session, controls_jsonfields$ControlName[i],label=etq,value=etq)
        }
        else if (controls_jsonfields$ControlType[i] == "colourinput"){
          updateColourInput(session,controls_jsonfields$ControlName[i],
                            value=json_data[controls_jsonfields$JSONfield[i]][[1]][controls_jsonfields$ListElement[i]])
        }
        else if (controls_jsonfields$ControlType[i] == "selectinput")
          updateSelectInput(session, controls_jsonfields$ControlName[i],
                            choices = weightchoices,
                            selected = json_data[controls_jsonfields$JSONfield[i]][[1]][controls_jsonfields$ListElement[i]])
      }
}