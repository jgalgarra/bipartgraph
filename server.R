###############################################################################
# BipartGraph
#
# Module         : server.R
# Descriction    : Server module of the Shiny application, with the "shinyServer"
#                  function and the reactive environment
#                  Nonreactive functions are sourced from nonreactive.R
###############################################################################

source("nonreactive.R")

#
# Server process
#
#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
#'

shinyServer(function(input, output, session) {
  
  shinyjs::hide("polarDownload")
  shinyjs::hide("polarcodeDownload")
  shinyjs::hide("networkAnalysis")
  shinyjs::hide("downloadLabels")  
  
  
  observe( {
    # Create user messages for javascript
    messagesNames<-c("LABEL_ZIGGURAT_INFO_DETAILS_TYPE", "LABEL_ZIGGURAT_INFO_DETAILS_KCORE", "LABEL_ZIGGURAT_INFO_DETAILS_ID", "LABEL_ZIGGURAT_INFO_DETAILS_NAME", "LABEL_ZIGGURAT_INFO_DETAILS_KRADIUS", "LABEL_ZIGGURAT_INFO_DETAILS_KDEGREE", "MESSAGE_CONFIRM_DELETE_FILES", "MESSAGE_WIKIPEDIA_NO_INFO_ERROR", "MESSAGE_WIKIPEDIA_DOWNLOAD_ERROR")
    messages<-strings$value(messagesNames)
    names(messages)<-messagesNames
    session$sendCustomMessage(type="messagesHandler", as.list(messages))
  })
  
  # Write labels and colors
  writelabcols <- function()
  {
    
    datoslabcol <- data.frame("file" = input$selectedDataFile,
                              "LabelGuildA" = input$DataLabelGuildAControl,
                              "LabelGuildB" = input$DataLabelGuildBControl,
                              "sep" = input$selectDataSeparator,
                              "speciesinheader" = input$selectDataSpeciesNames)
    if (exists("labelcolors")){
      labelcolors <<- labelcolors[toupper(labelcolors$file) != toupper(input$selectedDataFile),]
      labelcolors <<- rbind(tail(labelcolors, 199),datoslabcol)
    } else
      labelcolors <<- datoslabcol
    dir.create("conf/", showWarnings = FALSE)
    write.table(labelcolors,file=paste0("conf/labelcolors.csv"),sep=";",row.names = FALSE)
  }
  
  
  #Restore the default ziggurat colors
  restoredefaultzigcolors <- function()
  {
    updateColourInput(session, "zigguratColorGuildA1",
                      label = strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_1_CONTROL"),
                      value = czA1
    )
    updateColourInput(session, "zigguratColorGuildA2",
                      label = strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"),
                      value = czA2
    )
    updateColourInput(session, "zigguratColorGuildB1",
                      label = strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_1_CONTROL"),
                      value = czB1
    )
    updateColourInput(session, "zigguratColorGuildB2",
                      label = strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"),
                      value = czB2
    )
  }
  
  
  #Restore the default bipartite colors
  restoredefaultbipartitecolors <- function()
  {
    updateColourInput(session, "bipartiteColorGuildA1",
                      label = strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_1_CONTROL"),
                      value = czA1
    )
    updateColourInput(session, "bipartiteColorGuildA2",
                      label = strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"),
                      value = czA2
    )
    updateColourInput(session, "bipartiteColorGuildB1",
                      label = strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_1_CONTROL"),
                      value = czB1
    )
    updateColourInput(session, "bipartiteColorGuildB2",
                      label = strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"),
                      value = czB2
    )
  }
  
  # Reads the network data file
  selectedDataFileContent<-reactive({
    shinyjs::hideElement(id= "panelB")
    # jscode <- paste0("document.getElementById('toggleButtonSliderIdright').style.visibility='hidden'; ")
    # runjs(jscode)
    # jscode <- paste0("document.getElementById('toggleButtonSliderIdbottom').style.visibility='hidden'; ")
    # runjs(jscode)
    
    max_core <- 0
    analyze_file <- FALSE
    file<-input$selectedDataFile
    if (!is.null(file) && nchar(file)>0) {
      content<-read.csv(file=paste0(dataDir, "/", file), stringsAsFactors = FALSE, sep=input$selectDataSeparator,
                        header=input$selectDataSpeciesNames)
      an <<- new.env()
      an$sep <- input$selectDataSeparator
      an$speciesinheader <- input$selectDataSpeciesNames
      # Restore controls visibility
      visibilityZigDispControl("show",1,MAX_NUM_CORES)
      SwitchControls("enable",outsidercontrols)
      SwitchControls("enable",specialistcontrols)
      SwitchControls("enable",tailcontrols)
      SwitchControls("enable",weightcontrols)
      auxnguild_a = config_params$LabelA
      auxnguild_b = config_params$LabelB
      if (grepl("M_SD_",file)){
        auxnguild_a = "Disperser"
        auxnguild_b = "Seed"
      }
      if (grepl("M_PL_",file)){
        auxnguild_a = "Pollinator"
        auxnguild_b = "Plant"
      }
      if (grepl("RA_HP_",file)){
        auxnguild_a = "Host"
        auxnguild_b = "Parasite"
      }
      result_prim <- analyze_network(file, directory = paste0(dataDir, "/"),
                                       guild_a = auxnguild_a, guild_b = auxnguild_b, 
                                       only_NODF = TRUE, sep=input$selectDataSeparator,
                                       speciesinheader=input$selectDataSpeciesNames)
      jscode <- paste("localStorage.clear();")
      runjs(jscode)
      max_core <- result_prim$max_core
      result_prim$network_name <- strsplit(file,split='\\.')[[1]][1]
      analyze_file <- TRUE
      if (max_core == 1){
        updateTextInput(session, "DataLabelGuildAControl",
                        label = strings$value("LABEL_ZIGGURAT_LABEL_GUILDA"),
                        value = "ERROR: max core is 1"
        )
        updateTextInput(session, "DataLabelGuildBControl",
                        label = strings$value("LABEL_ZIGGURAT_LABEL_GUILDB"),
                        value = "Choose a different file"
        )
        content<-data.frame()
      } 
    } else {
      content<-data.frame()
    }
    if (!is.null(file) && nchar(file)>0 && (max_core != 1)){
      shinyjs::show("networkAnalysis")
      shinyjs::show("downloadLabels")      
      output$NodesGuildA <- renderText({
        paste(ncol(content)-(input$selectDataSpeciesNames),strings$value("LABEL_SPECIES"))
      })
      
      output$NodesGuildB <- renderText({
        paste(nrow(content),strings$value("LABEL_SPECIES"))
      })
      
      output$NetworkType <- renderText({
        tryCatch({
          strlinks <- paste(sum(content[1:nrow(content),2:ncol(content)]>0),strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_LINKS_HEADER"))
          if (sum(content[1:nrow(content),2:ncol(content)] > 1)==0)
            paste(strings$value("LABEL_ZIGGURAT_INFO_BINARY"),strlinks)
          else
            paste(strings$value("LABEL_ZIGGURAT_INFO_WEIGHTED"),strlinks)
          },
          error = function(cond) {
            result_validation = "File_Format_mismatch"
            message(paste("File format mismatch:",cond))
            errorMsg("File format mismatch")
          },
          warning = function(w) {
            result_validation = "File_Format_mismatch"
            message(paste("File format mismatch:",w))
            errorMsg("File format mismatch")
          }
        )
      })
      
      dflabcols <- searchlabcols(fred = file)
      if (ncol(dflabcols)>0){
        updckbx("selectDataSeparator",dflabcols$sep,session)
        updckbx("selectDataSpeciesNames",dflabcols$speciesinheader,session)
        updateTextInput(session, "DataLabelGuildAControl",
                        label = strings$value("LABEL_ZIGGURAT_LABEL_GUILDA"),
                        value = dflabcols$LabelGuildA
        )
        updateTextInput(session, "DataLabelGuildBControl",
                        label = strings$value("LABEL_ZIGGURAT_LABEL_GUILDB"),
                        value = dflabcols$LabelGuildB
        )
        updateColourInput(session, "zigguratColorGuildA1",
                          label = strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_1_CONTROL"),
                          value = as.character(dflabcols$ColorZigGuildA1)
        )
        updateColourInput(session, "zigguratColorGuildA2",
                          label = strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"),
                          value = as.character(dflabcols$ColorZigGuildA2)
        )
        updateColourInput(session, "zigguratColorGuildB1",
                          label = strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_1_CONTROL"),
                          value = as.character(dflabcols$ColorZigGuildB1)
        )
        updateColourInput(session, "zigguratColorGuildB2",
                          label = strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"),
                          value = as.character(dflabcols$ColorZigGuildB2)
        )
        names_A <- headslider("titleguildA",czA1,dflabcols$LabelGuildA,
                              result_prim$matrix[1,],result_prim$network_name)
        names_B <- headslider("titleguildB",czB1,dflabcols$LabelGuildB,result_prim$matrix[,1],result_prim$network_name)
        
        textinSlider <- paste("<span valign=\"top\"><div class=\"containerslider\">","<div class=\"columnslider\">",names_A,"</div>",
                              "<div class=\"columnslider\">",names_B,"</div>","</div>","</span>")


        jscode <- paste("document.getElementById('slideTextId').innerHTML='",textinSlider,"';")
        runjs(jscode)
      }
      else {
        updateTextInput(session, "DataLabelGuildAControl",
                        label <- strings$value("LABEL_ZIGGURAT_LABEL_GUILDA"),
                        value <- auxnguild_a
        )
        updateTextInput(session, "DataLabelGuildBControl",
                        label <- strings$value("LABEL_ZIGGURAT_LABEL_GUILDB"),
                        value <- auxnguild_b
        )
        restoredefaultzigcolors
      }
    }
    else{
      output$NodesGuildA <- renderText("")
      output$NodesGuildB <- renderText("")
    }
    return(content)
  })
  
  # Select uploaded files
  uploadedFilesList<-reactive({
    # Get list of uploaded files
    files<-input$uploadedFiles
    
    if (is.null(files)) {
      files<-list(c(), c(), c(), c())
    } else {
      # Copy file
      for (i in 1:length(files$datapath)) {
        from  <- files$datapath[i]
        to    <- paste0(dataDir, "/", files$name[i])
        file.copy(from, to)
      }
      # Update file list
      availableFiles$list<-availableFilesList()
    }
    
    # Rename columns
    names(files)<-c(strings$value("LABEL_UPLOADED_FILES_DETAILS_NAME"), strings$value("LABEL_UPLOADED_FILES_DETAILS_SIZE"), strings$value("LABEL_UPLOADED_FILES_DETAILS_TYPE"), "")
    
    # Drop unnamed columns
    files<-files[!(names(files) %in% c(""))]
    
    # Condition met when the application starts
    if(is.null(files$Filename))
      files <- data.frame(Filename = " ", Size = " ", Type = " ")
    
    return(files)
  })
  
  # List of available files
  availableFiles<-reactiveValues(list=list(), details=list())
  
  # list of selected nodes in the ziggurat
  markedNodes<-reactiveValues(data=data.frame())
  
  # Reacts when the list of available files changes
  observeEvent(availableFiles$list, {
    availableFiles$details<-availableFilesDetails(availableFiles$list)
    updateSelectInput(session, "selectedDataFile", choices=availableFiles$list)
  })
  
  # Reacts when there are changes in the data selection panel
  observeEvent(input$dataPanel, {
    # update available files list
    availableFiles$list<-availableFilesList()
  })
  
  # Refresh file list button
  observeEvent(input$refreshFiles, {
    # refresca la lista de ficheros
    availableFiles$list<-availableFilesList()
    output$availableFilesTable = DT::renderDataTable(availableFiles$details,
                                                     options = list(pageLength = 5),
                                                     server = TRUE)
  })
  
  # Display the list of available files
  output$availableFilesTable <-DT::renderDataTable(
    availableFiles$details,
    options = list(pageLength = 5)
  )
  
  # Restore default ziggurat colors
  observeEvent(input$restoreColorsControl, {
    restoredefaultzigcolors()
  })
  
  # Restore default bipartite colors
  observeEvent(input$restorebipartiteColorsControl, {
    restoredefaultbipartitecolors()
  })
  
  # Delete selected files
  observeEvent(input$deleteFiles, {
    s = input$availableFilesTable_rows_selected
    output$availableFilesTable = DT::renderDataTable(availableFiles$details,
                                                     options = list(pageLength = 5),
                                                     server = TRUE)
    q <- availableFiles$details[s,]
    rn <- rownames(q)
    for (j in rn)
      file.remove(j)
    availableFiles$list<-availableFilesList()
    output$availableFilesTable = DT::renderDataTable(availableFiles$details,
                                                     options = list(pageLength = 5),
                                                     server = TRUE)
    
  })
  
  # Restart session, set all values to default
  observeEvent(input$ResetAll, {
    zgg<-NULL
    bpp<-NULL
    bpot<-NULL
    mat<-NULL
    pol<-NULL
    session = getDefaultReactiveDomain()
    session$reload()
  })
  
  observeEvent(input$zigguratOneColor, {
    if (input$zigguratOneColor){
      manageColorControl(session,input$zigguratColorGuildA1, input$zigguratColorGuildA2, "zigguratColorGuildA2", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"))
      manageColorControl(session,input$zigguratColorGuildB1, input$zigguratColorGuildB2, "zigguratColorGuildB2", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"))
      shinyjs::disable("zigguratColorGuildA2")
      shinyjs::disable("zigguratColorGuildB2")
    } else {
      shinyjs::enable("zigguratColorGuildA2")
      shinyjs::enable("zigguratColorGuildB2")
    }
    
  })
  
  observeEvent(input$zigguratColorGuildA1,{
    if (exists("zgg")){
      updateSliderContents(zgg,"titleguildA","titleguildB",input$zigguratColorGuildA1,input$zigguratColorGuildB1,TRUE)
      updateSliderContents(zgg,"titleguildA","titleguildB",input$zigguratColorGuildA1,input$zigguratColorGuildB1,FALSE)
    }
    if (input$zigguratOneColor)
      manageColorControl(session,input$zigguratColorGuildA1, input$zigguratColorGuildA2, "zigguratColorGuildA2", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"))
  })
  
  observeEvent(input$zigguratColorGuildB1,{
    if (exists("zgg")){
      updateSliderContents(zgg,"titleguildA","titleguildB",input$zigguratColorGuildA1,input$zigguratColorGuildB1,TRUE)
      updateSliderContents(zgg,"titleguildA","titleguildB",input$zigguratColorGuildA1,input$zigguratColorGuildB1,FALSE)
    }
    if (input$zigguratOneColor)
      manageColorControl(session,input$zigguratColorGuildB1, input$zigguratColorGuildB2, "zigguratColorGuildB2", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"))
  })
  
  
  observeEvent(input$bipartiteOneColor, {
    if (input$bipartiteOneColor){
      manageColorControl(session,input$bipartiteColorGuildA1, input$bipartiteColorGuildA2, "bipartiteColorGuildA2", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"))
      manageColorControl(session,input$bipartiteColorGuildB1, input$bipartiteColorGuildB2, "bipartiteColorGuildB2", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"))
      shinyjs::disable("bipartiteColorGuildA2")
      shinyjs::disable("bipartiteColorGuildB2")
    } else {
      shinyjs::enable("bipartiteColorGuildA2")
      shinyjs::enable("bipartiteColorGuildB2")
    }
  })
  
  observeEvent(input$bipartiteVerticalLayout,{
    if (exists("bpp")){
      updateSliderContents(bpp,"titleguildA","titleguildB",input$bipartiteColorGuildA1,input$bipartiteColorGuildB1,
                           t_vertical = TRUE)
      updateSliderContents(bpp,"titleguildA","titleguildB",input$bipartiteColorGuildA1,input$bipartiteColorGuildB1,
                           t_vertical = FALSE)
    }
  
  })
  
  observeEvent(input$bipartiteColorGuildA1,{
    if (exists("bpp")){
      updateSliderContents(bplot,"titleguildA","titleguildB",input$bipartiteColorGuildA1,input$bipartiteColorGuildB1,TRUE)
      updateSliderContents(bplot,"titleguildA","titleguildB",input$bipartiteColorGuildA1,input$bipartiteColorGuildB1,FALSE)
    }
    if (input$bipartiteOneColor)
      manageColorControl(session,input$bipartiteColorGuildA1, input$bipartiteColorGuildA2, "bipartiteColorGuildA2", strings$value("LABEL_ZIGGURAT_GUILD_A_COLOR_2_CONTROL"))
  })
  
  observeEvent(input$bipartiteColorGuildB1,{
    if (exists("bpp")){
      updateSliderContents(bplot,"titleguildA","titleguildB",input$bipartiteColorGuildA1,input$bipartiteColorGuildB1,TRUE)
      updateSliderContents(bplot,"titleguildA","titleguildB",input$bipartiteColorGuildA1,input$bipartiteColorGuildB1,FALSE)
    }
    if (input$bipartiteOneColor)
      manageColorControl(session,input$bipartiteColorGuildB1, input$bipartiteColorGuildB2, "bipartiteColorGuildB2", strings$value("LABEL_ZIGGURAT_GUILD_B_COLOR_2_CONTROL"))
  })
  
  
  
  
  # Reactive bipartite plotting
  bipartite<-reactive({
    validate(
      need(nchar(input$selectedDataFile)>0, strings$value("MESSAGE_SELECT_DATE_FILE_ERROR"))
    )
    # Auxiliar trim function
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    
    # Empties select nodes
    markedNodes$data<-data.frame()
    
    # Progress bar
    progress<-shiny::Progress$new()
    progress$set(message="", value = 0)
    
    # Close progress bar
    on.exit(progress$close())
    
    # Disables bipartite container panel 
    session$sendCustomMessage(type="disableDivHandler", list(id=input$bipartitePlottype, disable=TRUE)) 
    bplot<-bipartite_graph(datadir = paste0(dataDir, "/"),
                           filename = input$selectedDataFile,
                           sep = input$selectDataSeparator,
                           speciesinheader = input$selectDataSpeciesNames,
                           flip_results = input$bipartiteVerticalLayout,
                           style=input$bipartitePlottype,orderkcoremaxby = "kradius",
                           weighted_links = input$bipartiteweighted_links,
                           color_link = input$bipartiteColorLink,
                           alpha_link = input$bipartiteAlphaLevelLink,
                           color_guild_a = c(input$bipartiteColorGuildA1, input$bipartiteColorGuildA2),
                           color_guild_b = c(input$bipartiteColorGuildB1, input$bipartiteColorGuildB2),
                           hide_plot_border = TRUE,
                           guild_gap_increase = (100+input$bipartiteGuildgapincrease)/100,
                           size_link = input$bipartiteLinkSize,
                           label_strguilda = trim(input$DataLabelGuildAControl),
                           label_strguildb = trim(input$DataLabelGuildBControl),
                           svg_scale_factor = 1,
                           lsize_kcoremax  = 4.5*input$bipartiteTextRescale,
                           lsize_legend = 5*input$bipartiteTextRescale,
                           landscape_plot  = input$paperLandscape,
                           show_title = input$bipartiteShowTitle,
                           show_legend = input$bipartiteShowLegend,
                           progress=progress)
    # ziggurat igraph object
    g<-bplot$result_analysis$graph
    
    # Guild positions
    guildAVertex<-which(V(g)$guild_id=="a")
    guildBVertex<-which(V(g)$guild_id=="b")
    # Neighbors of each node
    guildANeighbors<-sapply(guildAVertex, function(x) {neighbors(g, x)$id})
    guildBNeighbors<-sapply(guildBVertex, function(x) {neighbors(g, x)$id})
    # store labels and colors
    writelabcols()
    updateSliderContents(bplot,"titleguildA","titleguildB",input$bipartiteColorGuildA1,input$bipartiteColorGuildB1,TRUE)
    updateSliderContents(bplot,"titleguildA","titleguildB",input$bipartiteColorGuildA1,input$bipartiteColorGuildB1,FALSE)
    session$sendCustomMessage(type="disableDivHandler", list(id="bipartite", disable=FALSE))
    session$sendCustomMessage(type="bipartiteDataHandler", 
                              list(ids=c("a", "b"),
                                   names=c(bplot$name_guild_a, bplot$name_guild_b), 
                                   data=list(a=bplot$list_dfs_a, b=bplot$list_dfs_b), 
                                   neighbors=list(a=guildANeighbors, b=guildBNeighbors)))
    return(bplot)
  })
  
  # Reactive ziggurat plotting
  ziggurat<-reactive({
    
    validate(
      need(nchar(input$selectedDataFile)>0, strings$value("MESSAGE_SELECT_DATE_FILE_ERROR"))
    )
    # Auxiliar trim function
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    
    # Empties select nodes
    markedNodes$data<-data.frame()
    
    # Progress bar
    progress<-shiny::Progress$new()
    progress$set(message="", value = 0)
    
    # Close progress bar
    on.exit(progress$close())
    
    # Disables ziggurat container panel
    session$sendCustomMessage(type="disableDivHandler", list(id="ziggurat", disable=TRUE))
    
    #Plot ziggurat
    z<-ziggurat_graph(
      datadir                                       = paste0(dataDir, "/"),
      filename                                      = input$selectedDataFile,
      sep                                           = input$selectDataSeparator,
      speciesinheader                               = input$selectDataSpeciesNames,
      paintlinks                                    = input$zigguratPaintLinks,
      print_to_file                                 = FALSE,
      plotsdir                                      = tempdir(),
      orderkcoremaxby                               = input$zigguratOrderkcoremaxby,#"kradius",
      alpha_level                                   = input$zigguratAlphaLevel,
      color_guild_a                                 = c(input$zigguratColorGuildA1, input$zigguratColorGuildA2),
      color_guild_b                                 = c(input$zigguratColorGuildB1, input$zigguratColorGuildB2),
      color_link                                    = input$zigguratColorLink,
      alpha_link                                    = input$zigguratAlphaLevelLink,
      size_link                                     = input$zigguratLinkSize,
      displace_y_a                                  = c(0, input$zigguratYDisplaceSA2, input$zigguratYDisplaceSA3,
                                                        input$zigguratYDisplaceSA4,input$zigguratYDisplaceSA5,
                                                        input$zigguratYDisplaceSA6,input$zigguratYDisplaceSA7,
                                                        input$zigguratYDisplaceSA8,input$zigguratYDisplaceSA9,
                                                        input$zigguratYDisplaceSA10,input$zigguratYDisplaceSA11,
                                                        input$zigguratYDisplaceSA12,input$zigguratYDisplaceSA13,
                                                        input$zigguratYDisplaceSA14,input$zigguratYDisplaceSA15,
                                                        input$zigguratYDisplaceSA16,input$zigguratYDisplaceSA17,
                                                        input$zigguratYDisplaceSA18,input$zigguratYDisplaceSA19,
                                                        input$zigguratYDisplaceSA20),
      displace_y_b                                  =   c(0, input$zigguratYDisplaceSB2, input$zigguratYDisplaceSB3,
                                                          input$zigguratYDisplaceSB4,input$zigguratYDisplaceSB5,
                                                          input$zigguratYDisplaceSB6,input$zigguratYDisplaceSB7,
                                                          input$zigguratYDisplaceSB8,input$zigguratYDisplaceSB9,
                                                          input$zigguratYDisplaceSB10,input$zigguratYDisplaceSB11,
                                                          input$zigguratYDisplaceSB12,input$zigguratYDisplaceSB13,
                                                          input$zigguratYDisplaceSB14,input$zigguratYDisplaceSB15,
                                                          input$zigguratYDisplaceSB16,input$zigguratYDisplaceSB17,
                                                          input$zigguratYDisplaceSB18,input$zigguratYDisplaceSB19,
                                                          input$zigguratYDisplaceSB20),
      lsize_kcoremax                                = input$zigguratSvgScaleFactor*input$zigguratLabelsSizekCoreMax,
      lsize_zig                                     = input$zigguratSvgScaleFactor*input$zigguratLabelsSizeZiggurat,
      lsize_kcore1                                  = input$zigguratSvgScaleFactor*input$zigguratLabelsSizekCore1,
      lsize_legend                                  = input$zigguratLabelsSizeLegend,
      lsize_core_box                                = input$zigguratSvgScaleFactor*input$zigguratLabelsSizeCoreBox,
      labels_color                                  = c(input$zigguratColorLabelGuildA, input$zigguratColorLabelGuildB),
      height_box_y_expand                           = input$zigguratHeightExpand,
      kcore2tail_vertical_separation                = input$zigguratKcore2TailVerticalSeparation,
      kcore1tail_disttocore                         = c(input$zigguratKcore1TailDistToCore1, input$zigguratKcore1TailDistToCore2),
      innertail_vertical_separation                 = input$zigguratInnerTailVerticalSeparation,
      factor_hop_x                                  = input$zigguratHopx,
      fattailjumphoriz                              = c(input$zigguratfattailjumphorizA,input$zigguratfattailjumphorizB),
      fattailjumpvert                               = c(input$zigguratfattailjumpvertA,input$zigguratfattailjumpvertB),
      coremax_triangle_height_factor                = input$zigguratCoreMaxHExp,
      coremax_triangle_width_factor                 = input$zigguratCoreMaxWExp,
      paint_outsiders                               = input$zigguratPaintOutsiders,
      displace_outside_component                    = c(input$zigguratoutsiders_expand_horiz,input$zigguratoutsiders_expand_vert),
      outsiders_separation_expand                   = input$zigguratoutsiders_separation_expand,
      outsiders_legend_expand                       = input$zigguratoutsiders_legend_expand,
      specialistskcore2_horizontal_dist_rootleaf_expand  = input$zigguratroot_specialistskcore2_horiz,
      specialistskcore2_vertical_dist_rootleaf_expand    = input$zigguratroot_specialistskcore2_vert,
      specialists_boxes_separation_count                 = input$zigguratroot_specialist_boxesseparation,
      root_specialist_expand                             = c(input$zigguratroot_specialist_expand_horiz,input$zigguratroot_specialist_expand_vert),
      hide_plot_border                              = TRUE,
      rescale_plot_area                             = c(1,1),
      kcore1specialists_leafs_vertical_separation        = input$zigguratkcore1specialists_leafs_vertical_separation,
      corebox_border_size                           = input$zigguratCoreBoxSize,
      kcore_species_name_display                    = c(),
      kcore_species_name_break                      = c(),
      shorten_species_name                          = 0,
      label_strguilda                               = trim(input$DataLabelGuildAControl),
      label_strguildb                               = trim(input$DataLabelGuildBControl),
      landscape_plot                                = input$paperLandscape,
      backg_color                                   = "transparent",
      show_title                                    = input$zigguratShowTitle,
      show_legend                                   = input$zigguratShowLegend,
      use_spline                                    = input$zigguratUseSpline,
      spline_points                                 = input$zigguratSplinePoints,
      weighted_links                                = input$zigguratweighted_links,
      square_nodes_size_scale                       = input$ziggurat1shellExpand,
      svg_scale_factor                              = 25*input$zigguratSvgScaleFactor,
      # move_all_SVG_up                               = 0.01*input$zigguratSVGup,
      # move_all_SVG_right                            = 0.01*input$zigguratSVGright,
      aspect_ratio                                  = 1,
      progress                                      = progress
    )
    
    # ziggurat igraph object
    g<-z$result_analysis$graph
    
    # Guild positions
    guildAVertex<-which(V(g)$guild_id=="a")
    guildBVertex<-which(V(g)$guild_id=="b")
    # Neighbors of each node
    guildANeighbors<-sapply(guildAVertex, function(x) {neighbors(g, x)$id})
    guildBNeighbors<-sapply(guildBVertex, function(x) {neighbors(g, x)$id})
    # store labels and colors
    writelabcols()
    # Enables ziggurat container panel
    session$sendCustomMessage(type="disableDivHandler", list(id="ziggurat", disable=FALSE))
    session$sendCustomMessage(type="zigguratDataHandler", list(ids=c("a", "b"), 
                                                               names=c(z$name_guild_a, z$name_guild_b), 
                                                               data=list(a=z$list_dfs_a, b=z$list_dfs_b), 
                                                               neighbors=list(a=guildANeighbors,
                                                                              b=guildBNeighbors)))
    # Only shows ziggurat displacement controls for existing k-shells 
    visibilityZigDispControl("hide",zgg$kcoremax+1,MAX_NUM_CORES)
    # Disable outsider controls if all nodes are part of the giaan component
    if(length(names(zgg$outsider))==0)
      SwitchControls("disable",outsidercontrols)
    # Disable specialist controls if there are not specialists
    if(nrow(zgg$df_chains)==0)
      SwitchControls("disable",specialistcontrols)
    # Disable tail controls if there are no tails
    if (is.na(zgg$orphans_a)[1] && is.na(zgg$orphans_b)[1])
      SwitchControls("disable",tailcontrols)
    # Binary network
    if(sum(zgg$result_analysis$matrix > 1)==0)
      SwitchControls("disable",weightcontrols)
    jscode <- paste("document.getElementById('titleguildA').innerHTML='",z$name_guild_a,"';")
    runjs(jscode)
    jscode <- paste("document.getElementById('titleguildB').innerHTML='",z$name_guild_b,"';")
    runjs(jscode)
    updateSliderContents(zgg,"titleguildA","titleguildB",input$zigguratColorGuildA1,input$zigguratColorGuildB1,TRUE)
    updateSliderContents(zgg,"titleguildA","titleguildB",input$zigguratColorGuildA1,input$zigguratColorGuildB1,FALSE)
    
    # Show species button
    jscode <- paste0("document.getElementById('toggleButtonSliderIdright').style.visibility='visible'; ")
    runjs(jscode)
    jscode <- paste0("document.getElementById('toggleButtonSliderIdbottom').style.visibility='visible'; ")
    runjs(jscode)
    return(z)
  })
  
  # Updates selected nodes info
  observeEvent(input$markedNodesData, {
    result    <- data.frame(guild=character(0), kcore=integer(0), nodeId=integer(0), plottype=character(0), stringsAsFactors=FALSE)
    nodesData <- input$markedNodesData
    plottype <- ifelse(sum(grepl("ziggurat",nodesData))==0,"bipartite","ziggurat")
    guilds    <- nodesData[grep("\\.guild", names(nodesData))]
    kcores    <- as.integer(nodesData[grep("\\.kcore", names(nodesData))])
    plottype    <- nodesData[grep("\\.plottype", names(nodesData))][1]
    if (!is.null(guilds) && length(guilds)>0) {
      for (i in 1:length(guilds)) {
        nodeIds <- as.integer(nodesData[grep(paste0("^", i, "\\.nodeIds"), names(nodesData))])
        row     <- data.frame(guild=guilds[i], kcore=kcores[i], 
                              nodeId=nodeIds, plottype = plottype, 
                              row.names=NULL, stringsAsFactors=FALSE)
        result  <- rbind(result, row)
      }
    }
    row.names(result)<-NULL
    markedNodes$data<-result
  })
  
  # Shows the contents of a data file
  output$selectedDataFileContent<-renderDataTable(
    selectedDataFileContent(),
    options = list(pageLength=10, lengthMenu=list(c(10, 25, 50, -1), list('10', '25', '50', 'Todos')), searching=TRUE)
  )
  
  # Shows the list of last uploaded files
  output$uploadedFilesTable<-renderDataTable(
    uploadedFilesList(),
    options=list(pageLength=5, lengthMenu=list(c(5, 10, 25, 50, -1), list('5', '10', '25', '50', 'Todos')), searching=FALSE)
  )
  
  # Plots ziggurat object
  output$ziggurat<-renderUI({
    z<-ziggurat()
    svg<-z$svg
    html<-paste0(svg$html(), "<script>updateSVGEvents('ziggurat')</script>")
    return(HTML(html))
  })
  
  # Plots bipartite object
  output$bipartite<-renderUI({
    b<-bipartite()
    svg<-b$svg
    html<-paste0(svg$html(), "<script>updateSVGEvents('",input$bipartitePlottype,"')</script>") 
    return(HTML(html))
  })
  
  # Shows the details of a selected node in ziggurat
  output$zigguratNodesDetail<-renderUI({
    z         <- ziggurat()
    nodesData <- markedNodes$data
    plotstyle <- nodesData$plottype[1]
    details   <- ""
    if (nrow(nodesData)>0) {
      # Sort selected nodes
      nodesData <- nodesData[order(ifelse(nodesData$guild=="a", 0, 1), -nodesData$kcore, nodesData$nodeId),]
      
      # Shows nodes data
      for (i in 1:nrow(nodesData)) {
        guild   <- nodesData[i, "guild"]
        type    <- ifelse(guild=="a", z$name_guild_a, z$name_guild_b)
        kcore   <- as.integer(nodesData[i, "kcore"])
        nodeId  <- as.integer(nodesData[i, "nodeId"])
        
        if (guild=="a") {
          # k-shell data
          kcore_df<-z$list_dfs_a[[kcore]]
        } else {
          kcore_df<-z$list_dfs_b[[kcore]]
        }
        # Data frame with species details
        nodeDf  <- kcore_df[kcore_df$label==nodeId, c("label", "name_species", "kdegree", "kradius")]
        if (plotstyle=="ziggurat")
          details <- paste(details, showNodeDetails(type, kcore, nodeDf,zgg$network_name), collapse="")
        else
          details <- ""
      }
    }
    
    # Scroll
    details<-paste(details, "<script>updateZigguratNodesDetailScroll()</script>")
    return(HTML(details))
  })
  
  # Network information for bipartite plot
  output$networkinfoDetailBip<-renderUI({
    bplot <- bipartite()
    if (sum(bpp$result_analysis$matrix > 1)==0)
      strw = strings$value("LABEL_ZIGGURAT_INFO_BINARY")
    else
      strw = strings$value("LABEL_ZIGGURAT_INFO_WEIGHTED")
    details <- paste(bpp$network_name,"&nbsp;",strw,"&nbsp;",
                     bpp$result_analysis$links,"&nbsp;",
                     strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_LINKS_HEADER"),
                     "<br><h5>", 
                     "<span  style='color:",bpp$color_guild_a[1],"'>",bpp$result_analysis$num_guild_a, bpp$name_guild_a,"</span >","&nbsp;",
                     "<span  style='color:",bpp$color_guild_b[1],"'>",bpp$result_analysis$num_guild_b, bpp$name_guild_b,"</span >")
    details <- paste0(details,"<a href='reports/bipartite_",bpp$network_name,"_report.html' target='report' style='font-size:12px;' >&nbsp;&nbsp;&nbsp;",strings$value("LABEL_ZIGGURAT_SEE_DETAILS"),"</a></h5><hr>")
    return(HTML(details))
  })
  
  # Network information
  output$networkinfoDetailziggurat<-renderUI({
    z <- ziggurat()
    if (sum(zgg$result_analysis$matrix > 1)==0)
      strw = strings$value("LABEL_ZIGGURAT_INFO_BINARY")
    else
      strw = strings$value("LABEL_ZIGGURAT_INFO_WEIGHTED")
    create_static_report(pol, "www/reports/templates/indexhoriz.html",
                         paste0("www/reports/zigg_",zgg$network_name,"_report.html"), 
                         zgg$result_analysis, input$DataLabelGuildAControl,printplot = FALSE,
                         input$DataLabelGuildBControl, myenv = zgg, h=1.5*static_plot_width, 
                         pwidth=1000,
                         myenv_argg = zgg$ziggurat_argg, plottype = "ziggurat")
    
    details <- paste("&nbsp;&nbsp;&nbsp; ",strings$value("LABEL_NETWORK"),":&nbsp;",zgg$network_name,"&nbsp;",strw,"&nbsp;",
                     zgg$result_analysis$links,"&nbsp;",strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_LINKS_HEADER"),
                     "<br><h5>", 
                     "<span  style='color:",zgg$color_guild_a[1],"'>","&nbsp;&nbsp;", zgg$result_analysis$num_guild_a, zgg$name_guild_a,"</span >","&nbsp;",
                     "<span  style='color:",zgg$color_guild_b[1],"'>","&nbsp;&nbsp;", zgg$result_analysis$num_guild_b, zgg$name_guild_b,"</span >")
    details <- paste0(details,"&nbsp;&nbsp;<a href='reports/zigg_",zgg$network_name,"_report.html' target='report' style='font-size:12px;' >&nbsp;&nbsp;&nbsp;",strings$value("LABEL_ZIGGURAT_SEE_DETAILS"),"</a></h5><hr>")
    return(HTML(details))
  })
  
  buildcoloredguildlabels <- function(color,guildname){
    z <- ziggurat()
    details <- paste("<span  style='color:",color,"'>",guildname,"</span >")
    return(HTML(details))
  }
  
  output$networkGuildALabel<-renderUI({
    return(buildcoloredguildlabels(zgg$color_guild_a[1],zgg$name_guild_a))
  })
  
  output$networkGuildALabelTail<-renderUI({
    return(buildcoloredguildlabels(zgg$color_guild_a[1],zgg$name_guild_a))
  })
  
  output$networkGuildBLabel<-renderUI({
    return(buildcoloredguildlabels(zgg$color_guild_b[1],zgg$name_guild_b))
  })
  
  output$networkGuildBLabelTail<-renderUI({
    return(buildcoloredguildlabels(zgg$color_guild_b[1],zgg$name_guild_b))
  })
  # Matrix A labels
  output$networkinfoDetailbipartiteA<-renderUI({
    p <- bipartite()
    mdetails <- buildMatrixGuildLabels(bpp$bipartite_argg$label_strguilda,
                                       p$result_analysis$matrix[1,],bpp$bipartite_argg$filename,labelcol=bpp$bipartite_argg$color_guild_a)
    return(HTML(mdetails))
  })
  
  output$networkinfoDetailbipartiteB<-renderUI({
    p <- bipartite()
    mdetails <- buildMatrixGuildLabels(bpp$bipartite_argg$label_strguildb,
                                       p$result_analysis$matrix[,1],bpp$bipartite_argg$filename,labelcol=bpp$bipartite_argg$color_guild_b)
    return(HTML(mdetails))
  })
  # Network information
  
  output$networkinfoDetailbipartite<-renderUI({
    p <- bipartite()
    nname <- get_network_name(bpp$bipartite_argg$filename)
    if (sum(bpp$result_analysis$matrix > 1)==0)
      strw = strings$value("LABEL_ZIGGURAT_INFO_BINARY")
    else
      strw = strings$value("LABEL_ZIGGURAT_INFO_WEIGHTED")
    create_static_report(bplot$plot, "www/reports/templates/indexhoriz.html",
                        paste0("www/reports/bipartite_",bpp$network_name,"_report.html"), 
                        bpp$result_analysis, input$DataLabelGuildAControl,
                        input$DataLabelGuildBControl, 
                        pwidth = ifelse(bpp$flip_results,800,800),
                        w=static_plot_width,
                        h=ifelse(bpp$flip_results,1,0.5)*static_plot_width,
                        myenv = bpp,printplot = TRUE,
                        myenv_argg = bpp$bipartite_argg, 
                        plottype = "bipartite")
    details <- paste("&nbsp;&nbsp;&nbsp; ",strings$value("LABEL_NETWORK"),":&nbsp;",bpp$network_name,"&nbsp;",strw,"&nbsp;",
                     bpp$result_analysis$links,"&nbsp;",strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_LINKS_HEADER"),
                     "<br><h5>", 
                     "<span  style='color:",bpp$color_guild_a[1],"'>","&nbsp;&nbsp;", bpp$result_analysis$num_guild_a, bpp$name_guild_a,"</span >","&nbsp;",
                     "<span  style='color:",bpp$color_guild_b[1],"'>","&nbsp;&nbsp;", bpp$result_analysis$num_guild_b, bpp$name_guild_b,"</span >")
    details <- paste0(details,"&nbsp;&nbsp;<a href='reports/bipartite_",bpp$network_name,"_report.html' target='report' style='font-size:12px;' >&nbsp;&nbsp;&nbsp;",strings$value("LABEL_ZIGGURAT_SEE_DETAILS"),"</a></h5><hr>")
    return(HTML(details))
    
  })
  
  output$polardetailsheader<-renderUI({
    p <- polar()
    details <- paste("<span>", p$polar_argg$glabels[1],"</span >","&nbsp;<span>", p$polar_argg$glabels[2],"</span >")
    return(HTML(details))
  })
  
  # Network information
  output$networknamezigg<-renderUI({
    z <- ziggurat()
    if (exists("zgg") && !is.null(zgg))
      return(HTML( paste( "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",strings$value("LABEL_NETWORK"),": ",zgg$network_name,"</B>") ) )
    else
      return(HTML("                                    "))
  })
  
  # Network information
  output$networknamepolar<-renderUI({
    pol <- polar()
    if (exists("pol") && !is.null(pol))
      return(HTML( paste( "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",strings$value("LABEL_NETWORK"),": ",pol$network_name,"</B>") ) )
    else
      return(HTML("                                    "))
  })
  
  
  output$networknamematrix<-renderUI({
    mp <- matrix()
    if (exists("mat") && !is.null(mat))
      return(HTML( paste( "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",strings$value("LABEL_NETWORK"),": ",mat$network_name,"</B>") ) )
    else
      return(HTML("                                    "))
  })
  
  # Network information
  output$networknamebipartite<-renderUI({
    mybp <- bipartite()
    if (exists("bpp") && !is.null(bpp))
      return(HTML( paste( "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",strings$value("LABEL_NETWORK"),": ",bpp$network_name,"</B>") ) )
    else
      return(HTML("                                    "))
  })
  # Wikipedia information
  output$zigguratWikiDetail<-renderUI({
    z         <- ziggurat()
    nodesData <- markedNodes$data
    details   <- ""
    if (nrow(nodesData)>0) {
      # Sort selected nodes
      nodesData <- nodesData[order(ifelse(nodesData$guild=="a", 0, 1), -nodesData$kcore, nodesData$nodeId),]
      types   <- ifelse(nodesData$guild=="a", z$name_guild_a, z$name_guild_b)
      details <- paste(details, showWiki(types, nodesData), collapse="")
    }
    
    return(HTML(details))
  })
  
  # Selects Wikipedia pane
  observeEvent(input$wikiPanelName, {
    updateTabsetPanel(session, "wikiTabsetPanel", selected=input$wikiPanelName)
  })
  
  # Analyzes the network
  netanalysis <- reactive({
    progress<-shiny::Progress$new()
    progress$set(message=strings$value("MESSAGE_ANALYSIS_POGRESS"), value = 0)
    on.exit(progress$close())
    
    # Get network name
    red <- input$selectedDataFile
    red_name <- strsplit(red,".csv")[[1]][1]
    result_analysis <- analyze_network(red, directory = paste0(dataDir, "/"),
                                       guild_a = input$DataLabelGuildAControl, only_NODF = TRUE,
                                       guild_b = input$DataLabelGuildBControl, plot_graphs = FALSE,
                                       sep=input$selectDataSeparator,
                                       speciesinheader=input$selectDataSpeciesNames)
    numlinks <- result_analysis$links
    results_indiv <- data.frame(Name = c(), Species = c(), kradius = c(), kdegree = c(), kshell = c(), krisk = c())
    clase <- grepl(input$DataLabelGuildAControl,V(result_analysis$graph)$name)
    
    for (i in V(result_analysis$graph)){
      if (clase[i])
        nom_spe <- names(result_analysis$matrix[1,])[as.integer(strsplit(V(result_analysis$graph)$name[i],input$DataLabelGuildAControl)[[1]][2])]
      else
        nom_spe <- names(result_analysis$matrix[,1])[as.integer(strsplit(V(result_analysis$graph)$name[i],input$DataLabelGuildBControl)[[1]][2])]
      results_indiv <- rbind(results_indiv,data.frame(Name = gsub("\\."," ",nom_spe),
                                                      Species =V(result_analysis$graph)$name[i],
                                                      kradius = V(result_analysis$graph)$kradius[i],
                                                      kdegree = V(result_analysis$graph)[i]$kdegree, kshell = V(result_analysis$graph)[i]$kcorenum,
                                                      krisk = V(result_analysis$graph)[i]$krisk))
    }
    results_indiv$degree <- igraph::degree(result_analysis$graph)
    dir.create("analysis_indiv/", showWarnings = FALSE)
    fsal <- paste0("analysis_indiv/",red_name,"_analysis.csv")
    write.table(results_indiv,file=fsal,row.names=FALSE,sep = ";")
    return(fsal)
  })
  
  
  # Analyzes the network
  getspeciesnames <- reactive({
    createspecies_name <- function(GuildLabel,isA=TRUE){
      if (isA)
        nom_spe <- names(result_analysis$matrix[1,])[as.integer(strsplit(V(result_analysis$graph)$name[i],GuildLabel)[[1]][2])]
      else
        nom_spe <- names(result_analysis$matrix[,1])[as.integer(strsplit(V(result_analysis$graph)$name[i],GuildLabel)[[1]][2])]
      nom_spe <- paste(gsub(GuildLabel,"",V(result_analysis$graph)$name[i]),nom_spe)
      return(nom_spe)
    }
    
    progress<-shiny::Progress$new()
    progress$set(message=strings$value("MESSAGE_ANALYSIS_POGRESS"), value = 0)
    on.exit(progress$close())
    
    # Get network name
    red <- input$selectedDataFile
    red_name <- strsplit(red,".csv")[[1]][1]
    result_analysis <- analyze_network(red, directory = paste0(dataDir, "/"),
                                       guild_a = input$DataLabelGuildAControl, only_NODF = TRUE,
                                       guild_b = input$DataLabelGuildBControl, plot_graphs = FALSE,
                                       sep=input$selectDataSeparator,
                                       speciesinheader=input$selectDataSpeciesNames)
    results_indiv <- data.frame(Name = c(), Species = c())
    clase <- grepl(input$DataLabelGuildAControl,V(result_analysis$graph)$name)

    for (i in V(result_analysis$graph)){
      if (clase[i])
        nom_spe <- createspecies_name(input$DataLabelGuildAControl,isA=TRUE)
      else
        nom_spe <- createspecies_name(input$DataLabelGuildBControl,isA=FALSE)
      results_indiv <- rbind(results_indiv,data.frame(Name = gsub("\\."," ",nom_spe),
                                                      Species =V(result_analysis$graph)$name[i]))
    }
    namesA = paste(results_indiv[grepl(input$DataLabelGuildAControl,results_indiv$Species),]$Name,collapse=", ")
    namesB = paste(results_indiv[grepl(input$DataLabelGuildBControl,results_indiv$Species),]$Name,collapse=", ")
    dir.create("analysis_indiv/", showWarnings = FALSE)
    fsal <- paste0("analysis_indiv/",red_name,"_analysis.csv")
    fileConn <- file(fsal)
    writeLines(c(input$DataLabelGuildAControl, namesA, input$DataLabelGuildBControl, namesB), fileConn)
    close(fileConn)
    return(fsal)
  })
  
  # Reactive function to plot the polar graph
  polar<-reactive({
    validate(
      need(nchar(input$selectedDataFile)>0, strings$value("MESSAGE_SELECT_DATE_FILE_ERROR"))
    )
    swidth = input$screenwidthControl
    sheight = input$screenwidthControl
    progress<-shiny::Progress$new()
    progress$set(message="", value = 0)
    on.exit(progress$close())
    
    # Disables polar panel
    session$sendCustomMessage(type="disableDivHandler", list(id="polar", disable=TRUE))
    session$sendCustomMessage(type="disableDivHandler", list(id="histogramDist", disable=TRUE))
    session$sendCustomMessage(type="disableDivHandler", list(id="histogramCore", disable=TRUE))
    session$sendCustomMessage(type="disableDivHandler", list(id="histogramDegree", disable=TRUE))
    
    # Plots polar graph and histograms
    pgraph<-polar_graph(
      "data/",input$selectedDataFile,
      sep = input$selectDataSeparator,
      speciesinheader = input$selectDataSpeciesNames,
      fill_nodes          = input$polarFillNodes,
      print_title         = input$polarPrintTitle, 
      alpha_nodes         = input$polarAlphaLevel,
      plotsdir            = "tmppolar/",
      print_to_file       = TRUE,
      printable_labels    = input$polarDisplayText,
      show_histograms     = FALSE,
      glabels             = c(input$DataLabelGuildAControl, input$DataLabelGuildBControl),
      lsize_title         = input$polarLabelsSizeTitle,
      lsize_axis          = input$polarLabelsSizeAxis,
      lsize_legend        = input$polarLabelsSizeLegend,
      lsize_axis_title    = input$polarLabelsSizeAxisTitle,
      lsize_legend_title  = input$polarLabelsSizeLegendTitle,
      progress            = progress
    )
    
    # Enables polar container
    session$sendCustomMessage(type="disableDivHandler", list(id="polar", disable=FALSE))
    session$sendCustomMessage(type="disableDivHandler", list(id="histogramDist", disable=FALSE))
    session$sendCustomMessage(type="disableDivHandler", list(id="histogramCore", disable=FALSE))
    session$sendCustomMessage(type="disableDivHandler", list(id="histogramDegree", disable=FALSE))
    
    return(pgraph)
  })
  
  # Plot the polar graph
  output$polar <- renderImage({
    p <- polar()
    shinyjs::enable("polarloadPolarConfigFile")
    shinyjs::show("polarDownload")
    shinyjs::show("polarcodeDownload")
    # Return a list containing the filename
    pw = 600
    list(src = normalizePath(p["polar_file"][[1]]),
         contentType = 'image/png',
         width = pw,#input$screenwidthControl,
         height = pw,#input$screenwidthControl,
         alt = "Polar graph")
  }, deleteFile = FALSE)
  
  
  # Build polar guild labels
  buildPolarGuildLabels <- function(cabecera,mylabels,pfile,labelcol='grey9'){
    namesg <- cabecera
    nname <- get_network_name(pfile)
    details <- paste("<br><span class='GuildTitle' valign='top'><h5 style='color:",labelcol[1],"'>",namesg,"</h5></span>")
    labels <- clean_species_names(names(mylabels),nname)
    for (i in 1:length(labels))
      labels[i] <- paste0("<span class='GuildNamesList'>
                          <a href='https://",
                          config_params$WikipediaSubdomain,".wikipedia.org/wiki/",labels[i],"' target='wikipedia' >",sprintf("%2d",i),"</a>&nbsp;&nbsp;",
                          labels[i],"</span>")
    details <- paste(details,paste(labels,collapse="<br>"))
    return(details)
  }
  
  # Polar guild A labels
  output$networkinfoDetailpolarA<-renderUI({
    p <- polar()
    mdetails <- buildPolarGuildLabels(p$polar_argg$glabels[1],
                                      p$result_analysis$matrix[1,],p$polar_argg$filename)
    return(HTML(mdetails))
  })
  
  output$networkinfoDetailpolarB<-renderUI({
    p <- polar()
    mdetails <- buildPolarGuildLabels(p$polar_argg$glabels[2],p$result_analysis$matrix[,1],p$polar_argg$filename)
    return(HTML(mdetails))
  })
  
  # Reactive function to plot the matrix graph
  matrix<-reactive({
    validate(
      need(nchar(input$selectedDataFile)>0, strings$value("MESSAGE_SELECT_DATE_FILE_ERROR"))
    )
    progress<-shiny::Progress$new()
    progress$set(message="Matrix...", value = 0)
    on.exit(progress$close())
    # Disables matrix panel
    session$sendCustomMessage(type="disableDivHandler", list(id="matrix", disable=TRUE))
    # Plots matrix_graph
    p <- matrix_graph("data/",input$selectedDataFile,
                      sep = input$selectDataSeparator,
                      speciesinheader = input$selectDataSpeciesNames,
                      print_to_file = TRUE,
                      orderby = input$matrixOrderby,ppi=300,
                      flip_matrix = input$matrixRotate, 
                      links_weight = input$matrixWeights,
                      show_species_names = input$matrixShowNames,
                      show_title = input$matrixShowTitle,
                      show_legend = input$matrixShowLegend,
                      label_size = 18 * input$matrixTextresize,
                      label_strguilda = input$DataLabelGuildAControl,
                      label_strguildb = input$DataLabelGuildBControl,
                      color_guild_a = input$matrixColorGuildA,
                      color_guild_b = input$matrixColorGuildB,
                      plot_size = 10*input$matrixPlotresize/100,
                      progress            = progress)
    # Enables matrix container
    session$sendCustomMessage(type="disableDivHandler", list(id="matrix", disable=FALSE))
    shinyjs::enable("matrixDownload")
    if (p$binary_network)
      shinyjs::hide("matrixWeights")
    else
      shinyjs::show("matrixWeights")
    return(p)
  })
  
  # Plot the matrix graph
  output$matrix <- renderImage({
    p <- matrix()
    # Return a list containing the filename
    list(src = normalizePath(p[["matrix_file"]]),
         contentType = 'image/png',
         width = paste0(0.01*input$matrixPlotresize*min(100,1.1*round(100*ifelse(mat$mat_argg$flip_matrix,1/p$aspect,p$aspect))),"%"),
         # input$screenwidthControl,
         alt = "Matrix graph")
  }, deleteFile = FALSE)
  
  buildMatrixGuildLabels <- buildPolarGuildLabels
  
  # Matrix A labels
  output$networkinfoDetailmatrixA<-renderUI({
    p <- matrix()
    mdetails <- buildMatrixGuildLabels(p$mat_argg$label_strguilda,
                                       p$result_analysis$matrix[1,],p$mat_argg$filename,labelcol=mat$mat_argg$color_guild_a)
    
    return(HTML(mdetails))
  })
  
  output$networkinfoDetailmatrixB<-renderUI({
    p <- matrix()
    mdetails <- buildMatrixGuildLabels(p$mat_argg$label_strguildb,
                                       p$result_analysis$matrix[,1],p$mat_argg$filename,labelcol=mat$mat_argg$color_guild_b)
    return(HTML(mdetails))
  })
  
  
  # Matrix information
  output$networkinfoDetailmatrix<-renderUI({
    mymatrix <- matrix()
    nname <- get_network_name(mat$matrix_argg$filename)
    if (sum(mat$result_analysis$matrix > 1)==0)
      strw = strings$value("LABEL_ZIGGURAT_INFO_BINARY")
    else
      strw = strings$value("LABEL_ZIGGURAT_INFO_WEIGHTED")
    nname <- get_network_name(mymatrix$mat_argg$filename)
    create_static_report(mymatrix$plot, "www/reports/templates/indexhoriz.html",
                          paste0("www/reports/matrix_",nname,"_report.html"), 
                          mymatrix$result_analysis, input$DataLabelGuildAControl,
                          input$DataLabelGuildBControl, pwidth = 600*input$matrixPlotresize/100,
                          printplot=TRUE,myenv=mat,myenv_argg = mymatrix$mat_argg, plottype = "matrix")
    
    details <- paste("&nbsp;&nbsp;&nbsp; ",strings$value("LABEL_NETWORK"),":&nbsp;",mat$network_name,"&nbsp;",strw,"&nbsp;",
                     mat$result_analysis$links,"&nbsp;",strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_LINKS_HEADER"),
                     "<br><h5>", 
                     "<span  style='color:",mymatrix$mat_argg$color_guild_a[1],"'>","&nbsp;&nbsp;", mat$result_analysis$num_guild_a, mat$name_guild_a,"</span >","&nbsp;",
                     "<span  style='color:",mymatrix$mat_argg$color_guild_b[1],"'>","&nbsp;&nbsp;", mat$result_analysis$num_guild_b, mat$name_guild_b,"</span >")
    details <- paste0(details,"&nbsp;&nbsp;<a href='reports/matrix_",mat$network_name,"_report.html' target='report' style='font-size:12px;' >&nbsp;&nbsp;&nbsp;",strings$value("LABEL_ZIGGURAT_SEE_DETAILS"),"</a></h5><hr>")
    return(HTML(details))
    
  })

  
  # Polar information
  output$networkinfoDetailpolar<-renderUI({
    pol <- polar()
    if (sum(pol$result_analysis$matrix > 1)==0)
      strw = strings$value("LABEL_ZIGGURAT_INFO_BINARY")
    else
      strw = strings$value("LABEL_ZIGGURAT_INFO_WEIGHTED")
    
    nname <- get_network_name(pol$polar_argg$filename)
    create_static_report(pol, "www/reports/templates/indexhoriz.html",
                         paste0("www/reports/polar_",nname,"_report.html"), 
                         pol$result_analysis, input$DataLabelGuildAControl,
                         input$DataLabelGuildBControl, myenv = pll,
                         myenv_argg = pol$polar_argg, plottype = "polar")
    
    details <- paste("&nbsp;&nbsp;&nbsp; ",strings$value("LABEL_NETWORK"),":&nbsp;",pol$network_name,"&nbsp;",strw,"&nbsp;",
                     pol$result_analysis$links,"&nbsp;",strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_LINKS_HEADER"),
                     "<br><h5>", 
                     "<span>","&nbsp;&nbsp;", pol$result_analysis$num_guild_a, input$DataLabelGuildAControl,"</span >","&nbsp;",
                     "<span>","&nbsp;&nbsp;", pol$result_analysis$num_guild_b, input$DataLabelGuildBControl,"</span >")
    details <- paste0(details,"&nbsp;&nbsp;<a href='reports/polar_",pol$network_name,"_report.html' target='report' style='font-size:12px;' >&nbsp;&nbsp;&nbsp;",strings$value("LABEL_ZIGGURAT_SEE_DETAILS"),"</a></h5><hr>")
    return(HTML(details))
    
  })
  
  zigguratdiagramOptions<-reactive({
    return(calculateDiagramOptions(#as.numeric(input$paperSize), 
      4,as.numeric(input$zigguratppi), 
      input$zigguratfileextension, input$zigguratShowTitle, input$zigguratShowLegend))
  })
  
  bipartitediagramOptions<-reactive({
    return(calculateDiagramOptions(4, as.numeric(input$bipartiteppi), 
                                   input$bipartitefileextension, 
                                   input$bipartiteShowTitle, 
                                   input$bipartiteShowLegend,
                                   landscape = !input$bipartiteVerticalLayout,
                                   plottype = "bipartite"))
  })
  
  polardiagramOptions<-reactive({
    return(calculateDiagramOptions(4, as.numeric(input$polarppi), 
                                   input$polarfileextension, TRUE, TRUE))
  })
  
  matrixdiagramOptions<-reactive({
    return(calculateDiagramOptions(4, as.numeric(input$matrixppi), 
                                   input$matrixfileextension, TRUE, TRUE))
  })
  
  output$zigguratsaveSVG<-downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-ziggurat.svg")
      return(file)
    },
    content=function(file){
      # Gets the diagram
      z<-ziggurat()
      htmlsvg <- z$svg$html()
      dir.create("tmpcode/", showWarnings = FALSE)
      cat(htmlsvg, file = "tmpcode/tmp.svg")
      file.copy("tmpcode/tmp.svg",file)
    },
    contentType=paste0("text/svg+xml")
  )
  
  output$bipartitesaveSVG<-downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-bipartite-",input$bipartitePlottype,".svg")
      return(file)
    },
    content=function(file){
      # Gets the diagram
      bp <- bipartite()
      htmlsvg <- bp$svg$html()
      dir.create("tmpcode/", showWarnings = FALSE)
      cat(htmlsvg, file = "tmpcode/tmpbip.svg")
      file.copy("tmpcode/tmpbip.svg",file)
    },
    contentType=paste0("text/svg+xml")
  )
  
  # Ziggurat plot download button
  output$zigguratDownload<-downloadHandler(
    filename=function() {
      opt <- calculateDiagramOptions(4, 
                                     as.numeric(input$zigguratppi), 
                                     input$zigguratfileextension, input$zigguratShowTitle, input$zigguratShowTitle)
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-ziggurat." , input$zigguratfileextension)
      return(file)
    },
    content=function(file) {
      myoptions<-zigguratdiagramOptions()
      validateDiagramOptions(myoptions)
      myoptions$ppi <- input$zigguratppi
      myoptions$ext <- input$zigguratfileextension
      # Gets the diagram
      z<-ziggurat()
      plot<-z$plot
      plotStaticDiagram(file, plot,myoptions,"ziggurat",myenv=zgg)
    },
    contentType=paste0("image/", zigguratdiagramOptions()$ext)
  )
  
  # Bipartite plot download button
  output$bipartiteDownload<-downloadHandler(
    filename=function() {
      opt <- calculateDiagramOptions(4, as.numeric(input$bipartiteppi), input$bipartitefileextension, 
                                     input$bipartiteShowTitle, input$bipartiteShowLegend, 
                                     landscape=!bpp$flip_results, plottype="bipartite")
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-",
                        input$bipartitePlottype,"." , input$bipartitefileextension)
      return(file)
    },
    content=function(file) {
      myoptions<-bipartitediagramOptions()
      myoptions$ppi <- input$bipartiteppi
      myoptions$ext <- input$bipartitefileextension
      # Gets the diagram
      g<-bipartite()
      bplot<-g$plot
      #myfratio <- (g$tot_height/g$tot_width)
      if (input$bipartiteVerticalLayout){
        bplot <- bplot + coord_fixed() + scale_x_reverse() + coord_flip()
      }
      plotStaticDiagram(file, bplot,myoptions,"bipartite",myenv=bpp)
    },
    contentType=paste0("image/", bipartitediagramOptions()$bipartitefileextension)
  )
  
  # Download the polar plot
  output$polarDownload <- downloadHandler(
    filename=function() {
      opt <- calculateDiagramOptions(4, as.numeric(input$polarppi), input$polarfileextension, input$show_title, input$show_legend)
      file<-paste0(get_network_name(input$selectedDataFile), "-polar." , input$polarfileextension, input$show_title, input$show_legend)
      return(file)
    },
    content <- function(file) {
      myoptions<-polardiagramOptions()
      myoptions$width     <- static_plot_width*as.numeric(input$polarppi)
      myoptions$height    <- static_plot_width*as.numeric(input$polarppi)
      validateDiagramOptions(myoptions)
      myoptions$ppi <- input$polarppi
      myoptions$ext <- input$polarfileextension
      p <- polar()
      plot <- p$full_plot
      plotStaticDiagram(file, plot,myoptions,"polar",myenv=an)
    },
    contentType=paste0("image/", polardiagramOptions()$polarfileextension)
  )
  
  # Download the matrix plot
  output$matrixDownload <- downloadHandler(
    filename=function() {
      opt <- calculateDiagramOptions(4, as.numeric(input$matrixppi), input$matrixfileextension, input$show_title, input$show_legend)
      file<-paste0(get_network_name(input$selectedDataFile), "MATRIX_orderby_",input$matrixOrderby,".",input$matrixfileextension)
      return(file)
    },
    content <- function(file) {
      myoptions<-matrixdiagramOptions()
      myoptions$width     <- 10*as.numeric(input$matrixppi) 
      if (input$matrixRotate)
        myoptions$height    <- myoptions$width*mat$plot_width/mat$plot_height
      else
        myoptions$height    <- myoptions$width*mat$plot_height/mat$plot_width 
      validateDiagramOptions(myoptions)
      myoptions$ppi <- input$matrixppi
      myoptions$ext <- input$matrixfileextension
      p <- matrix()
      plot <- p$plot
      plotStaticDiagram(file, plot,myoptions,"matrix",myenv=p)
    },
    contentType=paste0("image/", matrixdiagramOptions()$matrixfileextension)
  )
  
  session$onSessionEnded(function() { unlink("analysis_indiv", recursive = TRUE)
    unlink("tmpcode", recursive = TRUE)
    unlink("tmppolar", recursive = TRUE)})
  
  # Download the network analysis
  output$networkAnalysis <- downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-analyisis.csv")
      return(file)
    },
    content <- function(file) {
      fresults <- netanalysis()
      file.copy(fresults, file)    },
    contentType="text/csv"
  )
  
  # Download species names
  output$downloadLabels <- downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-species-labels.txt")
      return(file)
    },
    content <- function(file) {
      fresults <- getspeciesnames()
      file.copy(fresults, file)    },
    contentType="text/csv"
  )
  
  # Downloads the polar generating code
  output$polarcodeDownload <- downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-polar-code.txt")
      return(file)
    },
    content <- function(file) {
      p <- polar()
      dir.create("tmpcode/", showWarnings = FALSE)
      sink("tmpcode/codepolar.txt")
      llamada <- p["polar_argg"]
      comando <- paste0("polg <- polar_graph(\"",llamada$polar_argg$datadir,"\",\"",llamada$polar_argg$filename)
      comando <- paste0(comando,"\",plotsdir = \"plot_results/\",print_to_file = TRUE")
      comando <- addCallParam(comando,llamada$polar_argg,"sep",quote=TRUE)
      comando <- addCallParam(comando,llamada$polar_argg,"speciesinheader")
      comando <- paste0(comando,",glabels = c(\"",llamada$polar_argg$glabels[1],"\",\"",llamada$polar_argg$glabels[2],"\")")
      comando <- addCallParam(comando,llamada$polar_argg,"lsize_title")
      comando <- addCallParam(comando,llamada$polar_argg,"lsize_legend")
      comando <- addCallParam(comando,llamada$polar_argg,"print_title")
      comando <- paste0(comando,",progress = NULL")
      comando <- addCallParam(comando,llamada$polar_argg,"fill_nodes")
      comando <- addCallParam(comando,llamada$polar_argg,"alpha_nodes")
      comando <- addCallParam(comando,llamada$polar_argg,"printable_labels")
      comando <- paste0(comando,")")
      cat(comando)
      sink()
      file.copy("tmpcode/codepolar.txt", file)
    },
    contentType="text/plain"
  )
  
  # Downloads the polar generating code
  output$matrixcodeDownload <- downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-matrix-code.txt")
      return(file)
    },
    content <- function(file) {
      mp <- matrix()
      dir.create("tmpcode/", showWarnings = FALSE)
      sink("tmpcode/codematrix.txt")
      llamada <- mp$mat_argg
      comando <- paste0("matg <- matrix_graph(\"",llamada$datadir,"\"," ,"\"",llamada$filename,"\"")
      comando <- addCallParam(comando,llamada,"sep",quote=TRUE)
      comando <- addCallParam(comando,llamada,"speciesinheader")
      comando <- paste0(comando, ",orderby = \"",llamada$orderby,"\",")
      comando <- paste0(comando, "label_strguilda = \"",llamada$label_strguilda,"\",")
      comando <- paste0(comando, "label_strguildb = \"",llamada$label_strguildb,"\",")
      comando <- paste0(comando, "label_size = ",llamada$label_size,",")
      comando <- paste0(comando, "color_guild_a = \"",llamada$color_guild_a,"\",")
      comando <- paste0(comando, "color_guild_b = \"",llamada$color_guild_b,"\",")
      comando <- paste0(comando, "color_links = \"",llamada$color_links,"\",")
      comando <- paste0(comando, "flip_matrix = ",llamada$flip_matrix,",")
      comando <- paste0(comando, "links_weight = ",llamada$links_weight,",")
      comando <- paste0(comando, "show_species_names = ",llamada$show_species_names,",")
      comando <- paste0(comando, "show_title = ",llamada$show_title,",")
      comando <- paste0(comando, "show_legend = ",llamada$show_legend,",")
      comando <- paste0(comando, "print_to_file = ",llamada$print_to_file,",")
      comando <- paste0(comando, "ppi = ",llamada$ppi,",")
      comando <- paste0(comando,"progress = NULL")
      comando <- paste0(comando,")")
      cat(comando)
      sink()
      file.copy("tmpcode/codematrix.txt", file)
    },
    contentType="text/plain"
  )
  #Downloads the ziggurat generating code
  output$zigguratcodeDownload <- downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-ziggurat-code.txt")
      return(file)
    },
    content <- function(file) {
      p <- ziggurat()
      dir.create("tmpcode/", showWarnings = FALSE)
      sink("tmpcode/codeziggurat.txt")
      llamada <- zgg$ziggurat_argg
      comando <- paste0("ziggurat_graph(\"data/\"",",\"",llamada$filename,"\"")
      comando <- addCallParam(comando,llamada,"sep",quote=TRUE)
      comando <- addCallParam(comando,llamada,"speciesinheader")
      comando <- addCallParam(comando,llamada,"paintlinks")
      comando <- addCallParam(comando,llamada,"orderkcoremaxby",quote=TRUE)
      comando <- paste0(comando,",print_to_file = TRUE")
      comando <- paste0(comando,",plotsdir = \"plot_results/\"")
      comando <- addCallParam(comando,llamada,"alpha_level")
      comando <- paste0(comando,",color_guild_a = c(\"",llamada$color_guild_a[1],"\",\"",llamada$color_guild_a[2],"\")")
      comando <- paste0(comando,",color_guild_b = c(\"",llamada$color_guild_b[1],"\",\"",llamada$color_guild_b[2],"\")")
      comando <- paste(comando,"\n")
      comando <- addCallParam(comando,llamada,"alpha_link")
      comando <- addCallParam(comando,llamada,"size_link")
      comando <- addCallParam(comando,llamada,"color_link", quote = TRUE)
      comando <- paste0(comando,",displace_y_b = ",paste("c(",paste(llamada$displace_y_b,collapse=",")),")")
      comando <- paste0(comando,",displace_y_a = ",paste("c(",paste(llamada$displace_y_a,collapse=",")),")")
      comando <- addCallParam(comando,llamada,"lsize_kcoremax")
      comando <- addCallParam(comando,llamada,"lsize_zig")
      comando <- addCallParam(comando,llamada,"lsize_kcore1")
      comando <- addCallParam(comando,llamada,"lsize_legend")
      comando <- addCallParam(comando,llamada,"lsize_core_box")
      comando <- addCallParam(comando,llamada,"labels_color")
      comando <- addCallParam(comando,llamada,"height_box_y_expand")
      comando <- addCallParam(comando,llamada,"kcore2tail_vertical_separation")
      comando <- paste0(comando,",kcore1tail_disttocore = ",paste("c(",paste(llamada$kcore1tail_disttocore,collapse=",")),")")
      comando <- addCallParam(comando,llamada,"innertail_vertical_separation")
      comando <- addCallParam(comando,llamada,"factor_hop_x")
      #comando <- paste0(comando,",displace_legend = ",paste("c(",paste(llamada$displace_legend,collapse=",")),")")
      comando <- paste0(comando,",fattailjumphoriz = ",paste("c(",paste(llamada$fattailjumphoriz,collapse=",")),")")
      comando <- paste0(comando,",fattailjumpvert = ",paste("c(",paste(llamada$fattailjumpvert,collapse=",")),")")
      comando <- addCallParam(comando,llamada,"coremax_triangle_height_factor")
      comando <- addCallParam(comando,llamada,"paint_outsiders")
      comando <- paste0(comando,",displace_outside_component = ",paste("c(",paste(llamada$displace_outside_component,collapse=",")),")")
      comando <- addCallParam(comando,llamada,"coremax_triangle_width_factor")
      comando <- addCallParam(comando,llamada,"outsiders_separation_expand")
      comando <- addCallParam(comando,llamada,"outsiders_legend_expand")
      comando <- addCallParam(comando,llamada,"specialistskcore2_horizontal_dist_rootleaf_expand")
      comando <- addCallParam(comando,llamada,"specialistskcore2_vertical_dist_rootleaf_expand")
      comando <- addCallParam(comando,llamada,"specialists_boxes_separation_count")
      comando <- paste0(comando,",root_specialist_expand = ",paste("c(",paste(llamada$root_specialist_expand,collapse=",")),")")
      comando <- addCallParam(comando,llamada,"hide_plot_border")
      comando <- paste0(comando,",rescale_plot_area = ",paste("c(",paste(llamada$rescale_plot_area,collapse=",")),")")
      comando <- addCallParam(comando,llamada,"kcore1specialists_leafs_vertical_separation")
      comando <- addCallParam(comando,llamada,"corebox_border_size")
      comando <- addCallParam(comando,llamada,"label_strguilda", quote = TRUE)
      comando <- addCallParam(comando,llamada,"label_strguildb", quote = TRUE)
      if (is.null(llamada$landscape_plot))
        llamada$landscape_plot <- TRUE
      comando <- addCallParam(comando,llamada,"landscape_plot")
      comando <- addCallParam(comando,llamada,"backg_color", quote = TRUE)
      comando <- addCallParam(comando,llamada,"show_title")
      comando <- addCallParam(comando,llamada,"show_legend", quote = TRUE)
      comando <- addCallParam(comando,llamada,"use_spline")
      comando <- addCallParam(comando,llamada,"spline_points")
      comando <- addCallParam(comando,llamada,"file_name_append", quote =TRUE)
      comando <- addCallParam(comando,llamada,"svg_scale_factor")
      comando <- addCallParam(comando,llamada,"aspect_ratio")
      comando <- addCallParam(comando,llamada,"weighted_links", quote =TRUE)
      comando <- addCallParam(comando,llamada,"square_nodes_size_scale")
      comando <- addCallParam(comando,llamada,"move_all_SVG_up")
      comando <- addCallParam(comando,llamada,"move_all_SVG_right")
      comando <- paste0(comando,",progress = NULL")
      comando <- paste0(comando,")")
      cat(comando)
      sink()
      file.copy("tmpcode/codeziggurat.txt", file)
    },
    contentType="text/plain"
  )
  
  #Downloads the bipartite generating code
  output$bipartitecodeDownload <- downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-bipartite-code.txt")
      return(file)
    },
    content <- function(file) {
      p <- bipartite()
      dir.create("tmpcode/", showWarnings = FALSE)
      sink("tmpcode/codebipartite.txt")
      llamada <- bpp$bipartite_argg
      comando <- paste0("bipartite_graph(\"data/\"",",\"",llamada$filename,"\"")
      comando <- addCallParam(comando,llamada,"sep",quote=TRUE)
      comando <- addCallParam(comando,llamada,"speciesinheader")
      comando <- addCallParam(comando,llamada,"paintlinks")
      comando <- addCallParam(comando,llamada,"orderkcoremaxby",quote=TRUE)
      comando <- addCallParam(comando,llamada,"print_to_file")
      comando <- paste0(comando,",plotsdir = \"plot_results/\"")
      comando <- addCallParam(comando,llamada,"guild_gap_increase")
      comando <- addCallParam(comando,llamada,"style",quote=TRUE)
      comando <- addCallParam(comando,llamada,"flip_results")
      comando <- addCallParam(comando,llamada,"alpha_level")
      comando <- paste0(comando,",color_guild_a = c(\"",llamada$color_guild_a[1],"\",\"",llamada$color_guild_a[2],"\")")
      comando <- paste0(comando,",color_guild_b = c(\"",llamada$color_guild_b[1],"\",\"",llamada$color_guild_b[2],"\")")
      comando <- paste(comando,"\n")
      comando <- addCallParam(comando,llamada,"alpha_link")
      comando <- addCallParam(comando,llamada,"size_link")
      comando <- addCallParam(comando,llamada,"color_link", quote = TRUE)
      comando <- addCallParam(comando,llamada,"lsize_kcoremax")
      comando <- addCallParam(comando,llamada,"lsize_kcore1")
      comando <- addCallParam(comando,llamada,"lsize_legend")
      comando <- addCallParam(comando,llamada,"lsize_core_box")
      comando <- addCallParam(comando,llamada,"hide_plot_border")
      comando <- addCallParam(comando,llamada,"label_strguilda", quote = TRUE)
      comando <- addCallParam(comando,llamada,"label_strguildb", quote = TRUE)
      comando <- addCallParam(comando,llamada,"backg_color", quote = TRUE)
      comando <- addCallParam(comando,llamada,"show_title")
      comando <- addCallParam(comando,llamada,"show_legend", quote = TRUE)
      comando <- addCallParam(comando,llamada,"svg_scale_factor")
      comando <- addCallParam(comando,llamada,"weighted_links", quote =TRUE)
      comando <- paste0(comando,",progress = NULL")
      comando <- paste0(comando,")")
      cat(comando)
      sink()
      file.copy("tmpcode/codebipartite.txt", file)
    },
    contentType="text/plain"
  )
  
  #Downloads the ziggurat plot configuration parameters
  output$zigguratsaveZigConfigFile <- downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-ziggurat-plot-config.json")
      return(file)
    },
    content <- function(file) {
      dir.create("tmpcode/", showWarnings = FALSE)
      zgg$ziggurat_argg$plotsdir <-""
      jsonzgg <- jsonlite::toJSON(x = zgg$ziggurat_argg[1:length(zgg$ziggurat_argg)-1], pretty = TRUE, force = TRUE)
      cat(paste(jsonzgg,"\n"), file = "tmpcode/zgg.json")
      file.copy("tmpcode/zgg.json",file)
    },
    contentType="text/plain"
  )
  
  #Downloads the polar plot configuration parameters
  output$polarsavePolarConfigFile <- downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-polar-plot-config.json")
      return(file)
    },
    content <- function(file){ 
      pol <- polar()
      dir.create("tmpcode/", showWarnings = FALSE)
      myargg <- pol$polar_argg[1:(length(pol$polar_argg)-1)]
      myargg$network_name <- pol$network_name
      myargg$style <- "polar"
      myargg$plotsdir <-""
      jsonbpp <- jsonlite::toJSON(x = myargg, pretty = TRUE, force = TRUE)
      cat(paste(jsonbpp,"\n"), file = "tmpcode/polar.json")
      file.copy("tmpcode/polar.json",file)
    },
    contentType="text/plain"
  )
  
  #Downloads the bipartite plot configuration parameters
  output$bipartitesaveBipConfigFile <- downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-bipartite-plot-config.json")
      return(file)
    },
    content <- function(file) {
      dir.create("tmpcode/", showWarnings = FALSE)
      bpp$bipartite_argg$plotsdir <-""
      jsonbpp <- jsonlite::toJSON(x = bpp$bipartite_argg[1:length(bpp$bipartite_argg)-1], pretty = TRUE, force = TRUE)
      cat(paste(jsonbpp,"\n"), file = "tmpcode/bpp.json")
      file.copy("tmpcode/bpp.json",file)
    },
    contentType="text/plain"
  )
  
  #Downloads the matrix plot configuration parameters
  output$matrixsaveMatrixConfigFile <- downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-matrix-plot-config.json")
      return(file)
    },
    content <- function(file) {
      dir.create("tmpcode/", showWarnings = FALSE)
      myargg <- mat$mat_argg[1:(length(mat$mat_argg)-1)]
      myargg$plotsdir <-""
      argsfiltered <- myargg[!grepl("function",myargg)]
      argsfiltered$network_name <- mat$network_name
      jsonbpp <- jsonlite::toJSON(x = argsfiltered, pretty = TRUE, force = TRUE)
      cat(paste(jsonbpp,"\n"), file = "tmpcode/mat.json")
      file.copy("tmpcode/mat.json",file)
    },
    contentType="text/plain"
  )
  
  output$language <- reactive({
    updateSelectInput(session, selectLanguage,
                      choices = c("en", "es"),
                      selected = input$selectLanguage)
    
  })
  
  observeEvent(input$zigguratReport, { 
    create_report("www/reports/templates/index.html",paste0("www/reports/",zgg$network_name,"_report.html"))
  })
  
  
  output$contentsfileconfigzigplot <- reactive({
    if (!is.null(input$zigguratloadZigConfigFile)){
      filePath <- input$zigguratloadZigConfigFile$datapath
      contentsfileconfigzigplot <- paste(readLines(filePath), collapse = "\n")
      contentsfileconfigzigplot <- paste("JSON CONTENTS","\n",contentsfileconfigzigplot)
      result_validation ="FILE ERROR"
      if (!grepl(".json",filePath))
        result_validation <- static_error_msg("MESSAGE_ERROR_JSON_WRONG_EXTENSION")
      else {
        tryCatch(
          {
            json_data <- jsonlite::fromJSON(filePath, simplifyVector = TRUE)
            fields_json_data <- names(json_data)
            result_validation <- validateconfigfile(json_data,zgg$network_name,'ziggurat')
          },
          error = function(cond) {
            result_validation = "Invalid_JSON"
            message(paste("Invalid JSON",cond))
          },
          warning = function(w) {
            result_validation = "Invalid_JSON"
            message(paste("Invalid JSON",w))
          }
        )
      }
      if (result_validation=="OK"){
        parseJSONConfig(controls_jsonfields,session,json_data,'ziggurat')
        if (input$zigguratshowZigConfigFile){
          contentsfileconfigzigplot
        }
      }
      else
        contentsfileconfigzigplot <- paste(result_validation,"\n\n",contentsfileconfigzigplot)
    }
  })
  
  output$contentsfileconfigbipplot <- reactive({
    if (!is.null(input$bipartiteloadBipConfigFile)){
      filePath <- input$bipartiteloadBipConfigFile$datapath
      contentsfileconfigbipplot <- paste(readLines(filePath), collapse = "\n")
      contentsfileconfigbipplot <- paste("JSON CONTENTS","\n",contentsfileconfigbipplot)
      result_validation ="FILE ERROR"
      if (!grepl(".json",filePath))
        result_validation <- static_error_msg("MESSAGE_ERROR_JSON_WRONG_EXTENSION")
      else {
        tryCatch(
          {
            json_data <- jsonlite::fromJSON(filePath, simplifyVector = TRUE)
            fields_json_data <- names(json_data)
            result_validation <- validateconfigfile(json_data,bpp$network_name,'bipartite')
          },
          error = function(cond) {
            result_validation = "Invalid_JSON"
            message(paste("Invalid JSON",cond))
          },
          warning = function(w) {
            result_validation = "Invalid_JSON"
            message(paste("Invalid JSON",w))
          }
        )
      }
      
      if (result_validation=="OK"){
        parseJSONConfig(controls_jsonfields,session,json_data,'bipartite')
        if (input$bipartiteshowBipConfigFile){
          contentsfileconfigbipplot
        }
      }
      else
        contentsfileconfigbipplot <- paste(result_validation,"\n\n",contentsfileconfigbipplot)
    }
  })
  
  output$contentsfileconfigmatrixplot <- reactive({
    if (!is.null(input$matrixloadMatrixConfigFile)){
      filePath <- input$matrixloadMatrixConfigFile$datapath
      contentsfileconfigmatrixplot <- paste(readLines(filePath), collapse = "\n")
      contentsfileconfigmatrixplot <- paste("JSON CONTENTS","\n",contentsfileconfigmatrixplot)
      result_validation ="FILE ERROR"
      if (!grepl(".json",filePath))
        result_validation <- static_error_msg("MESSAGE_ERROR_JSON_WRONG_EXTENSION")
      else {
        tryCatch(
          {
            json_data <- jsonlite::fromJSON(filePath, simplifyVector = TRUE)
            fields_json_data <- names(json_data)
            result_validation <- validateconfigfile(json_data,mat$network_name,'matrix')
          },
          error = function(cond) {
            result_validation = "Invalid_JSON"
            message(paste("Invalid JSON",cond))
          },
          warning = function(w) {
            result_validation = "Invalid_JSON"
            message(paste("Invalid JSON",w))
          }
        )
      }
      if (result_validation=="OK"){
        parseJSONConfig(controls_jsonfields,session,json_data,'matrix')
        if (input$matrixshowMatrixConfigFile){
          contentsfileconfigmatrixplot
        }
      }
      else
        contentsfileconfigmatrixplot <- paste(result_validation,"\n\n",contentsfileconfigmatrixplot)
    }
  })
  
  output$contentsfileconfigpolarplot <- reactive({
    if (!is.null(input$polarloadPolarConfigFile)){
      filePath <- input$polarloadPolarConfigFile$datapath
      contentsfileconfigpolarplot <- paste(readLines(filePath), collapse = "\n")
      contentsfileconfigpolarplot <- paste("JSON CONTENTS","\n",contentsfileconfigpolarplot)
      result_validation ="FILE ERROR"
      if (!grepl(".json",filePath))
        result_validation <- static_error_msg("MESSAGE_ERROR_JSON_WRONG_EXTENSION")
      else {
        tryCatch(
          {
            json_data <- jsonlite::fromJSON(filePath, simplifyVector = TRUE)
            fields_json_data <- names(json_data)
            result_validation <- validateconfigfile(json_data,an$network_name,'polar')
          },
          error = function(cond) {
            result_validation = "Invalid_JSON"
            message(paste("Invalid JSON",cond))
          },
          warning = function(w) {
            result_validation = "Invalid_JSON"
            message(paste("Invalid JSON",w))
          }
        )
      }
      if (result_validation=="OK"){
        parseJSONConfig(controls_jsonfields,session,json_data,'polar')
        if (input$polarshowPolarConfigFile){
          contentsfileconfigpolarplot
        }
      }
      else
        contentsfileconfigpolarplot <- paste(result_validation,"\n\n",contentsfileconfigpolarplot)
    }
  })
})