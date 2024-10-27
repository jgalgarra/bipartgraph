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
  
  # shinyjs::hide("polarDownload")
  # shinyjs::hide("polarcodeDownload")
  shinyjs::hide("networkAnalysis")
  
  
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
                              "LabelGuildB" = input$DataLabelGuildBControl)
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
  
  # Reads the network data file
  selectedDataFileContent<-reactive({
    shinyjs::hideElement(id= "panelB")
    max_core <- 0
    analyze_file <- FALSE
    file<-input$selectedDataFile
    if (!is.null(file) && nchar(file)>0) {
      content<-read.csv(file=paste0(dataDir, "/", file), header=TRUE, stringsAsFactors = FALSE)
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
      if (!searchsafefile(fred=file)){
        # Guild names by default for web of life files
        result_prim <- analyze_network(file, directory = paste0(dataDir, "/"),
                                       guild_a = auxnguild_a, guild_b = auxnguild_b, only_NODF = TRUE)
        max_core <- result_prim$max_core
        analyze_file <- TRUE
      }
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
      } else if (analyze_file) {
        if (exists("sfiles"))
          sfiles <<- rbind(sfiles,data.frame("file"=file))
        else
          sfiles <<- data.frame("file"=file)
        write.table(sfiles,file="conf/datossafefiles.csv",row.names=FALSE)
      }
    } else {
      content<-data.frame()
    }
    if (!is.null(file) && nchar(file)>0 && (max_core != 1)){
      shinyjs::show("networkAnalysis")
      
      output$NodesGuildA <- renderText({
        paste(ncol(content)-1,strings$value("LABEL_SPECIES"))
      })
      
      output$NodesGuildB <- renderText({
        paste(nrow(content),strings$value("LABEL_SPECIES"))
      })
      
      output$NetworkType <- renderText({
        if (sum(content[1:nrow(content),2:ncol(content)] > 1)==0)
          strings$value("LABEL_ZIGGURAT_INFO_BINARY")
        else
          strings$value("LABEL_ZIGGURAT_INFO_WEIGHTED")
      })
      
      output$NetworkLinks <- renderText({
        paste(sum(content[1:nrow(content),2:ncol(content)]>0),strings$value("LABEL_ZIGGURAT_CONFIG_COLOURS_LINKS_HEADER"))
      })
      
      dflabcols <- searchlabcols(fred = file)
      if (ncol(dflabcols)>0){
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
  observeEvent(input$restoreColors, {
    restoredefaultzigcolors()
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
    session = getDefaultReactiveDomain()
    session$reload()
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
    

    # Plot ziggurat
    z<-ziggurat_graph(
      datadir                                       = paste0(dataDir, "/"),
      filename                                      = input$selectedDataFile,
      paintlinks                                    = input$zigguratPaintLinks,
      print_to_file                                 = FALSE,
      plotsdir                                      = tempdir(),
      #orderkcoremaxby                               = valordkcoremax[as.numeric(input$orderkcoremaxby)+1],
      orderkcoremaxby                               = input$orderkcoremaxby,
      
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
      lsize_kcoremax                                = input$zigguratLabelsSizekCoreMax,
      lsize_zig                                     = input$zigguratLabelsSizeZiggurat,
      lsize_kcore1                                  = input$zigguratLabelsSizekCore1,
      lsize_legend                                  = input$zigguratLabelsSizeLegend,
      lsize_core_box                                = input$zigguratLabelsSizeCoreBox,
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
      backg_color                                   = input$zigguratBckgdColorControl,
      show_title                                    = TRUE,
      use_spline                                    = input$zigguratUseSpline,
      spline_points                                 = input$zigguratSplinePoints,
      square_nodes_size_scale                       = input$ziggurat1shellExpandControl,
      weighted_links                                = input$zigguratweighted_links,
      svg_scale_factor                              = 25*input$zigguratSvgScaleFactor,
      move_all_SVG_up                               = 0.01*input$zigguratSVGup,
      move_all_SVG_right                            = 0.01*input$zigguratSVGright,
      aspect_ratio                                  = input$zigguratAspectRatio,
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
    session$sendCustomMessage(type="zigguratDataHandler", list(ids=c("a", "b"), names=c(z$name_guild_a, z$name_guild_b), data=list(a=z$list_dfs_a, b=z$list_dfs_b), neighbors=list(a=guildANeighbors, b=guildBNeighbors)))
    # Enables ziggurat container panel
    session$sendCustomMessage(type="disableDivHandler", list(id="ziggurat", disable=FALSE))
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
    
    return(z)
  })
  
  # Updates selected nodes info
  observeEvent(input$markedNodesData, {
    result    <- data.frame(guild=character(0), kcore=integer(0), nodeId=integer(0), stringsAsFactors=FALSE)
    nodesData <- input$markedNodesData
    guilds    <- nodesData[grep("\\.guild", names(nodesData))]
    kcores    <- as.integer(nodesData[grep("\\.kcore", names(nodesData))])
    if (!is.null(guilds) && length(guilds)>0) {
      for (i in 1:length(guilds)) {
        nodeIds <- as.integer(nodesData[grep(paste0("^", i, "\\.nodeIds"), names(nodesData))])
        row     <- data.frame(guild=guilds[i], kcore=kcores[i], nodeId=nodeIds, row.names=NULL, stringsAsFactors=FALSE)
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
    html<-paste0(svg$html(), "<script>updateSVGEvents()</script>")
    return(HTML(html))
  })
  
  # Shows the details of a selected node in ziggurat
  output$zigguratNodesDetail<-renderUI({
    z         <- ziggurat()
    nodesData <- markedNodes$data
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
        details <- paste(details, showNodeDetails(type, kcore, nodeDf,zgg$network_name), collapse="")
      }
    }
    
    # Scroll
    details<-paste(details, "<script>updateZigguratNodesDetailScroll()</script>")
    return(HTML(details))
  })
  
  # Network information
  output$networkinfoDetail<-renderUI({
    z <- ziggurat()
    if (sum(zgg$result_analysis$matrix > 1)==0)
      strw = strings$value("LABEL_ZIGGURAT_INFO_BINARY")
    else
      strw = strings$value("LABEL_ZIGGURAT_INFO_WEIGHTED")
    create_zigg_report(z,"www/reports/templates/index.html",paste0("www/reports/zigg_",zgg$network_name,"_report.html"))
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
  
  # Network information
  output$networkinfoDetailpolar<-renderUI({
    p <- polar()
    nname <- get_network_name(p$polar_argg$red)
    create_polar_report(p,"www/reports/templates/index.html",paste0("www/reports/polar_",nname,"_report.html"))
    details <- paste("<h5>",strings$value("LABEL_NETWORK"),"&nbsp;",nname)
    details <- paste0(details,"&nbsp;<a href='reports/polar_",nname,"_report.html' target='report' style='font-size:12px;' >&nbsp;&nbsp;",strings$value("LABEL_POLAR_SEE_DETAILS"),"</a></h5>")
    return(HTML(details))
  })
  
  output$polardetailsheader<-renderUI({
    p <- polar()
    details <- paste("<span>", p$polar_argg$glabels[1],"</span >","&nbsp;<span>", p$polar_argg$glabels[2],"</span >")
    return(HTML(details))
  })
  
  # Network information
  output$networkname<-renderUI({
    z <- ziggurat()
    if (exists("zgg") && !is.null(zgg))
      return(HTML( paste( "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",strings$value("LABEL_NETWORK"),": ",zgg$network_name,"</B>") ) )
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
                                       guild_b = input$DataLabelGuildBControl, plot_graphs = FALSE)
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
    p<-polar_graph(
      red                 = input$selectedDataFile,
      directorystr        = paste0(dataDir, "/"),
      fill_nodes          = input$polarFillNodesControl,
      print_title         = input$polarPrintTitleControl, 
      alpha_nodes         = input$polarAlphaLevel,
      plotsdir            = "tmppolar/",
      print_to_file       = TRUE,
      printable_labels    = input$polarDisplayText,
      show_histograms     = FALSE,
      glabels             = c(input$DataLabelGuildAControl, input$DataLabelGuildBControl),
      gshortened          = c(substr(input$DataLabelGuildAControl, 1, 4),substr(input$DataLabelGuildBControl, 1, 4)),
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
    
    return(p)
  })
  
  # Plot the polar graph
  output$polar <- renderImage({
    p <- polar()
    # shinyjs::show("polarDownload")
    # shinyjs::show("polarcodeDownload")
    # Return a list containing the filename
    list(src = normalizePath(p["polar_file"][[1]]),
         contentType = 'image/png',
         width = input$screenwidthControl,
         height = input$screenwidthControl,
         alt = "Polar graph")
  }, deleteFile = FALSE)
  
  
# Build polar guild labels
  buildPolarGuildLabels <- function(cabecera,mylabels,pfile){
    namesg <- cabecera
    nname <- get_network_name(pfile)
    details <- paste("<br><br><span class='GuildTitle' valign='top'><h5>",namesg,"</h5></span><br>")
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
    mdetails <- buildPolarGuildLabels(p$polar_argg$glabels[1],p$result_analysis$matrix[1,],p$polar_argg$red)
    return(HTML(mdetails))
  })
  
  output$networkinfoDetailpolarB<-renderUI({
    p <- polar()
    mdetails <- buildPolarGuildLabels(p$polar_argg$glabels[2],p$result_analysis$matrix[,1],p$polar_argg$red)
    return(HTML(mdetails))
  })
  
  diagramOptions<-reactive({
    return(calculateDiagramOptions(as.numeric(input$paperSize), as.numeric(input$zigguratppi), input$zigguratileextension))
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
   
  # Ziggurat plot download button
  output$zigguratDownload<-downloadHandler(
    filename=function() {
      opt <- calculateDiagramOptions(as.numeric(input$paperSize), as.numeric(input$zigguratppi), input$zigguratfileextension)
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-ziggurat." , input$zigguratfileextension)
      return(file)
    },
    content=function(file) {
      myoptions<-diagramOptions()
      validateDiagramOptions(myoptions)
      myoptions$ppi <- input$zigguratppi
      myoptions$ext <- input$zigguratfileextension
      # Gets the diagram
      z<-ziggurat()
      plot<-z$plot
      plotZigguratDiagram(file, plot, myoptions)
    },
    contentType=paste0("image/", diagramOptions()$ext)
  )
  
 
  # Download the polar plot
  output$polarDownload <- downloadHandler(
    filename=function() {
      #opt <- calculateDiagramOptions(as.numeric(input$paperSize), as.numeric(input$polarppi), input$polarfileextension)
      opt <- calculateDiagramOptions(4, as.numeric(input$polarppi), input$polarfileextension)
      file<-paste0(get_network_name(input$selectedDataFile), "-polar." , input$polarfileextension)
      return(file)
    },
    content <- function(file) {
      myoptions<-diagramOptions()
      myoptions$width     <- 14*as.numeric(input$polarppi) #options$width*ppi
      myoptions$height    <- 14*as.numeric(input$polarppi) #options$height*ppi
      validateDiagramOptions(myoptions)
      myoptions$ppi <- input$polarppi
      myoptions$ext <- input$polarfileextension
      p <- polar()
      plot <- p$full_plot
      plotpolarDiagram(file, plot, myoptions)
    },
    contentType=paste0("image/", input$polarfileextension)
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
  
  #Aux function to add a parameter and reproduce the function call
  addCallParam <- function(com,lpar,param,quoteparam = FALSE)
  {
    if (quoteparam)
      com <- paste0(com," ,",param," = \"",lpar[param],"\"")
    else
      com <- paste0(com," ,",param," = ",lpar[param])
    return(com)
  }
  
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
      comando <- paste0("polg <- polar_graph(\"",llamada$polar_argg$red,"\",")
      comando <- paste0(comando, "directorystr = \"",llamada$polar_argg$directorystr,"\"")
      comando <- paste0(comando,",plotsdir = \"plot_results/polar/\",print_to_file = TRUE,")
      comando <- paste0(comando,"glabels = c(\"",llamada$polar_argg$glabels[1],"\",\"",llamada$polar_argg$glabels[2],"\"),")
      comando <- paste0(comando,"gshortened = c(\"",llamada$polar_argg$gshortened[1],"\",\"",llamada$polar_argg$gshortened[2],"\")")
      comando <- addCallParam(comando,llamada$polar_argg,"show_histograms")
      comando <- addCallParam(comando,llamada$polar_argg,"lsize_title")
      #comando <- addCallParam(comando,llamada$polar_argg,"lsize_axis")
      comando <- addCallParam(comando,llamada$polar_argg,"lsize_legend")
      #comando <- addCallParam(comando,llamada$polar_argg,"lsize_axis_title")
      comando <- addCallParam(comando,llamada$polar_argg,"file_name_append",quote = TRUE)
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
      comando <- addCallParam(comando,llamada,"paintlinks")
      comando <- addCallParam(comando,llamada,"orderkcoremaxby",quote=TRUE)
      comando <- paste0(comando,",print_to_file = TRUE")
      comando <- paste0(comando,",plotsdir = \"plot_results/ziggurat\"")
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
      comando <- paste0(comando,",displace_legend = ",paste("c(",paste(llamada$displace_legend,collapse=",")),")")
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
  
  #Downloads the ziggurat plot configuration parameters
  output$zigguratsaveZigConfigFile <- downloadHandler(
    filename=function() {
      file<-paste0(gsub(fileExtension, "", input$selectedDataFile), "-ziggurat-plot-config.json")
      return(file)
    },
    content <- function(file) {
      dir.create("tmpcode/", showWarnings = FALSE)

      jsonzgg <- jsonlite::toJSON(x = zgg$ziggurat_argg[1:length(zgg$ziggurat_argg)-1], pretty = TRUE, force = TRUE)
      cat(paste(jsonzgg,"\n"), file = "tmpcode/zgg.json")
      file.copy("tmpcode/zgg.json",file)
    },
    contentType="text/plain"
  )
  
  updckbx <- function(idchkbx,jsonvalue){
    updateCheckboxInput(
      session =  session,
      inputId =  idchkbx, 
      value = jsonvalue
    )
  }
  
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
      if (!grepl(".json",filePath))
        result_validation <- static_error_msg("MESSAGE_ERROR_JSON_WRONG_EXTENSION")
      else {
        json_data <- jsonlite::fromJSON(filePath, simplifyVector = TRUE)
        fields_json_data <- names(json_data)
        result_validation <- validateconfigfile(json_data)
      }
      if (result_validation=="OK"){
        for (i in 1:nrow(controls_jsonfields)){
          if (controls_jsonfields$ControlType[i] == "slider")
            updateSliderInput(session, controls_jsonfields$ControlName[i],
                              value = as.numeric(json_data[controls_jsonfields$JSONfield[i]][[1]][controls_jsonfields$ListElement[i]])/as.numeric(controls_jsonfields$Divideby[i]))
          else if (controls_jsonfields$ControlType[i] == "checkbox"){
            etq = json_data[controls_jsonfields$JSONfield[i]][[1]][controls_jsonfields$ListElement[i]]
            updckbx(controls_jsonfields$ControlName[i],etq)
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
        if (input$zigguratshowZigConfigControlFile){
          contentsfileconfigzigplot
        }
      }
      else
        contentsfileconfigzigplot <- paste(result_validation,"\n\n",contentsfileconfigzigplot)
    }
  })
  
})