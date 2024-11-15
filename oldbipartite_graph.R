library(grid)
library(gridExtra)
library(bipartite)
library(igraph)
library(ggplot2)
library(rlang)
library(ggtext)

debugging = TRUE
# if (debugging){
#   source("network-kanalysis.R")
#   source("SVG.R")
# }

#' Plotting a bipartite graph
#'
#' This function plots the ziggurat graph of a bipartite network. Configuration parameters and
#' results are stored in a global environment called bpp. This environment is not destroyed
#' after the function is executed, so the developer can store it in a configuration file, retrieve
#' network analysis variables and so on. If a new bipartite_graph is called, bpp is destroyed and
#' created again for the new plot. Plotting options are explained in the user manual.
#'
#' @param datadir the name of the file of the interaction matrix
#' @param filename the file with the interaction matrix
#' @param print_to_file if set to FALSE the plot is displayed in the R session window
#' @param plotsdir the directory where the plot is stored
#' @param orderkcoremaxby sets order of kcoremax nodes, by kradius or kdegree
#' @param style bipartite representation style: legacy, kcoreorder, chilopodograph
#' @param guild_gap_increase controls the disptance between guild rows
#' @param flip_results displays the graph in portrait configuration
#' @param aspect_ratio ziggurat plot default aspect ratio
#' @param alpha_level transparency for ziggurats' filling
#' @param color_guild_a default filling for nodes of guild_a
#' @param color_guild_b default filling for nodes of guild_b
#' @param color_link default links color
#' @param alpha_link link transparency
#' @param size_link width of the links
#' @param displace_y_b relative vertical displacement of guild_b inner ziggurats
#' @param displace_y_a relative vertical displacement of guild_a inner ziggurats
#' @param lsize_kcoremax nodes in kshell max label size
#' @param lsize_zig nodes in inner ziggurats label size
#' @param lsize_kcore1 labels of nodes in kshell 1
#' @param lsize_legend legend label size
#' @param lsize_kcorebox default kshell boxes label size
#' @param labels_color default label colors
#' @param height_box_y_expand expand inner ziggurat rectangles default height by this factor
#' @param kcore2tail_vertical_separation expand vertical of kshell 1 species linked to kshell 2 by this factor
#' @param kcore1tail_disttocore expand vertical separation of kshell 1 species from kshell max (guild_a, guild,b)
#' @param innertail_vertical_separation expand vertical separation of kshell species connected to khsell > 2 & < kshell max
#' @param factor_hop_x expand inner ziggurats horizontal distance
#' @param fattailjumphoriz displace kshell 1 species linked to leftmost kshell max species
#' @param fattailjumpvert idem for vertical position
#' @param coremax_triangle_width_factor expand khsell max rectangles width by this factor
#' @param coremax_triangle_height_factor expand khsell max rectangles height by this factor
#' @param paint_outsiders paint species not connected to giant component
#' @param displace_outside_component displace outsider species (horizontal, vertical)
#' @param outsiders_separation_expand multiply by this factor outsiders' separation
#' @param outsiders_legend_expand displace outsiders legend
#' @param specialistskcore2_horizontal_dist_rootleaf_expand expand horizontal distance of specialist tail root node connected to kshell 2
#' @param specialistskcore2_vertical_dist_rootleaf_expand expand vertical distance of specialist tails connected to kshell 2
#' @param specialists_boxes_separation_count specialist species boxes separation count
#' @param root_specialist_expand expand root specialist distances of tails connected to kshell <> 2
#' @param hide_plot_border hide border around the plot
#' @param rescale_plot_area full plot area rescaling (horizontal, vertical)
#' @param kcore1specialists_leafs_vertical_separation expand vertical separation of specialist tails connected to kshell 1 species
#' @param corebox_border_size width of kshell boxes
#' @param kcore_species_name_display display species names of  shells listed in this vector
#' @param kcore_species_name_break allow new lines in species names of  shells listed in this vector
#' @param shorten_species_name number of characters of species name to display
#' @param exclude_species_number do not include species number in species
#' @param label_strguilda string labels of guild a
#' @param label_strguildb string labels of guild b
#' @param landscape_plot paper landscape configuration
#' @param backg_color plot background color
#' @param show_title show plot title
#' @param use_spline use splines to draw links
#' @param spline_points number of points for each spline
#' @param file_name_append a label that the user may append to the plot file name for convenience
#' @param svg_scale_factor only for interactive apps, do not modify
#' @param weighted_links function to add link weight: 'none', 'log10' , 'ln', 'sqrt'
#' @param square_nodes_size_scale scale nodes area of kcore1 and outsiders
#' @param move_all_SVG_up move up all the SVG plot by this fraction, useful to crop upper white space
#' @param move_all_SVG_right move rightwards all the SVG plot by this fraction, useful to crop right white space
#' @param progress only for interactive apps, do not modifiy
#' @export
#' @examples bipartite_graph("data/","M_PL_001.csv",plotsdir="grafresults/",print_to_file = TRUE)

old_bipartite_graph <- function(datadir,filename,
                           paintlinks = TRUE, print_to_file = FALSE, plotsdir ="plot_results/ziggurat/", 
                           orderkcoremaxby = "kradius", style="legacy", guild_gap_increase = 1, 
                           flip_results = FALSE, aspect_ratio = 1,
                           alpha_level = 0.2, color_guild_a = c("#4169E1","#00008B"), color_guild_b = c("#F08080","#FF0000"),
                           color_link = "slategray3", alpha_link = 0.5, size_link = 0.5,
                           displace_y_b = rep(0,20),
                           displace_y_a = rep(0,20),
                           lsize_kcoremax = 3.5, lsize_zig = 3, lsize_kcore1 = 2.5, lsize_legend = 4, lsize_core_box = 2.5,
                           labels_color = c(),
                           height_box_y_expand = 1, kcore2tail_vertical_separation = 1,  kcore1tail_disttocore = c(1,1),
                           innertail_vertical_separation = 1,
                           factor_hop_x = 1, fattailjumphoriz = c(1,1), fattailjumpvert = c(1,1),
                           coremax_triangle_height_factor = 1, coremax_triangle_width_factor = 1,
                           paint_outsiders = TRUE, displace_outside_component = c(0,0),
                           outsiders_separation_expand = 1, outsiders_legend_expand = 1,
                           specialistskcore2_horizontal_dist_rootleaf_expand = 1,
                           specialistskcore2_vertical_dist_rootleaf_expand = 0, specialists_boxes_separation_count = 1,
                           root_specialist_expand = c(1,1), hide_plot_border = TRUE, rescale_plot_area = c(1,1),
                           kcore1specialists_leafs_vertical_separation = 1, corebox_border_size = 0.2,
                           kcore_species_name_display = c(), kcore_species_name_break = c(),
                           shorten_species_name = 0, exclude_species_number = FALSE, label_strguilda = "",
                           label_strguildb = "", landscape_plot = TRUE,
                           backg_color = "white", show_title = TRUE, use_spline =TRUE, spline_points = 10,
                           file_name_append = "", svg_scale_factor= 10, weighted_links = "none",
                           square_nodes_size_scale = 1, move_all_SVG_up = 0, move_all_SVG_right = 0,
                           progress=NULL
)
{
  # This assignment stores the call parameters in ziggurat_argg as a list. This list is useful
  # to save plotting parameters for a future simulation

  bipartite_argg <- c(as.list(environment()))
  fsvgtext <<- 5
  # Create global environment
  bpp <<- new.env()
  if (!is.null(progress)) progress$inc(1/11, detail=strings$value("MESSAGE_ZIGGURAT_PROGRESS_ANALYZING_NETWORK"))
  # Analyze network
  f <- read_and_analyze(datadir,filename,label_strguilda, label_strguildb)
  bpp$result_analysis <- f["result_analysis"][[1]]
  bpp$str_guild_a <- f["str_guild_a"][[1]]
  bpp$str_guild_b <- f["str_guild_b"][[1]]
  bpp$name_guild_a <- f["name_guild_a"][[1]]
  bpp$name_guild_b <- f["name_guild_b"][[1]]
  bpp$network_name <- f["network_name"][[1]]
  bpp$mtxlinks <- data.frame(igraph::as_edgelist(bpp$result_analysis$graph))
  names(bpp$mtxlinks) <- c("guild_a","guild_b")
  
  # Exit if kcore max == 1
  if (bpp$result_analysis$max_core == 1){
    msg = "Max core is 1. Ziggurat plot only works if max core is bigger than 1"
    if (!is.null(progress))
      progress$inc(1/11, detail=strings$value(msg))
    else
      print(msg)
    return(bpp)
  }
  # Copy input parameters to the bpp environment
  def_configuration(paintlinks, print_to_file, plotsdir, orderkcoremaxby, style,
                    guild_gap_increase, flip_results, aspect_ratio,
                    alpha_level, color_guild_a, color_guild_b,
                    color_link, alpha_link, size_link,
                    displace_y_b, displace_y_a, lsize_kcoremax, lsize_zig, lsize_kcore1,
                    lsize_legend, lsize_core_box, labels_color,
                    height_box_y_expand, kcore2tail_vertical_separation,  kcore1tail_disttocore,
                    innertail_vertical_separation,
                    factor_hop_x, fattailjumphoriz, fattailjumpvert,
                    coremax_triangle_height_factor, coremax_triangle_width_factor,
                    paint_outsiders, displace_outside_component,
                    outsiders_separation_expand, outsiders_legend_expand, specialistskcore2_horizontal_dist_rootleaf_expand,
                    specialistskcore2_vertical_dist_rootleaf_expand , specialists_boxes_separation_count,
                    root_specialist_expand, hide_plot_border, rescale_plot_area,kcore1specialists_leafs_vertical_separation,
                    corebox_border_size, kcore_species_name_display,kcore_species_name_break,shorten_species_name,exclude_species_number,
                    label_strguilda, label_strguildb, landscape_plot, backg_color, show_title,
                    use_spline, spline_points, file_name_append, svg_scale_factor, weighted_links,
                    square_nodes_size_scale, move_all_SVG_up, move_all_SVG_right, progress
  )
  # Removes nodes without any tie. This is not usual in input files but happens
  # when performing destruction simulations
  strip_isolated_nodes()
  init_working_values()
  draw_bipartite_plot(svg_scale_factor, progress)
  # Copy input parameters as a string for reroducibility
  #bpp$bipartite_argg <- bipartite_argg
  return(bpp)
}


# Labels of square nodes: tails, specialist chains and outsiders
gen_sq_label <- function(nodes, joinchars = "\n", is_guild_a = TRUE)
{
  # If kcore1 nodes name are displayed
  dispname <- is.element(1,bpp$kcore_species_name_display)
  if (dispname)
    if (is_guild_a)
      nspec <- colnames(bpp$result_analysis$matrix)
    else
      nspec <- rownames(bpp$result_analysis$matrix)
  nnodes <- length(nodes)
  lrow <- round(sqrt(nnodes))
  ssal <- ""
  for (i in 1:nnodes)
  {
    if (dispname)
      ssal <- paste(ssal,nspec[as.integer(nodes[i])])
    else
      ssal <- paste(ssal,nodes[i])
    if ((i %% lrow == 0) & (nnodes > 1) & (i<nnodes))
      ssal <- gsub("  "," ",paste(ssal,joinchars))
  }
  return(ssal)
}


gen_vert_label <- function(nodes, joinchars = "\n")
{
  nnodes <- length(nodes)
  if (nnodes == 1)
    return(nodes)
  ssal <- ""
  for (i in 1:(nnodes-1))
      ssal <- paste0(ssal,nodes[i],ifelse(i %% 2 == 0, "\n",joinchars))
  ssal <- paste0(ssal,nodes[nnodes])
  ssal <- gsub("  "," ",ssal)
  return(ssal)
}

# Create the label species. May be complex if user chooses to display
# the binomial name
create_label_species <- function(strent,newline = FALSE){
  strchar <- ifelse(newline,"\n","")
  pieces <- unlist(strsplit(unlist(strent)," "))
  if (is.na(pieces[2]))
    pieces[2] = ""
  if (bpp$shorten_species_name>0){
    pieces[1] = paste0(substr(pieces[1],1,bpp$shorten_species_name),".")
    if (nchar(pieces[2])>2)
      pieces[2] = paste0(substr(pieces[2],1,bpp$shorten_species_name),".")
  }
  if (length(pieces)>2)
    strsal <- paste(pieces[1],strchar,"XX")
  else
    strsal <- paste(pieces[1],strchar,pieces[2])
  return(strsal)
}

# Decides length and rotation of labels
name_species_preprocess <- function (kcore, list_dfs, kcore_species_name_display,
                                     kcore_species_name_break) {
  if (is.element(kcore,kcore_species_name_display)){
    if (!bpp$flip_results)
      kcoremaxlabel_angle <- 90
    else
      kcoremaxlabel_angle <- 0
    labelszig <- rep("",nrow(list_dfs))
    pnewline <- is.element(kcore,kcore_species_name_break)
    for (j in 1:length(list_dfs$name_species)){
      labelszig[j] <- create_label_species(list_dfs$name_species[j],newline=pnewline)
      if (!bpp$exclude_species_number)
        labelszig[j] <- paste(list_dfs$label[j],labelszig[j])
    }
  } else {
    kcoremaxlabel_angle <- 0
    labelszig <- list_dfs$label
  }
  calc_values <- list("kcoremaxlabel_angle" = kcoremaxlabel_angle, "labelszig" = labelszig)
  return(calc_values)
}

# Draws a square, both in ggplot2 and SVG flavours
draw_square<- function(idPrefix, grafo,svg,basex,basey,side,fillcolor,alphasq,labelcolor,
                       langle,hjust,vjust,slabel,lbsize = bpp$labels_size,
                       inverse="no",adjustoxy = "no", edgescolor="transparent")
{
  x1 <- c(basex)
  x2 <- c(basex+side)
  y1 <- c(basey)
  y2 <- c(basey+side/bpp$aspect_ratio)
  ds <- data.frame(x1, x2, y1, y2, fillcolor)
  signo <- 1
  if (inverse == "yes")
  {
    ds$y1 <- -(ds$y1)
    ds$y2 <- -(ds$y2)
    signo <- -1
  }

  p <- grafo + geom_rect(data=ds, linewidth=0.01,
                         mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
                         fill = fillcolor, alpha = alphasq, color="transparent")
  pxx <- x1+0.05*(x2-x1)
  pyy <- signo*(y1+(y2-y1)/2)
  if (adjustoxy == "yes"){
    pxx <- (x2+x1)/2
    pyy <- signo*(y1+y2)/2
  }
  p <- p + annotate(geom="text", x=pxx, y=pyy, label=slabel,
                   colour = labelcolor, size=lbsize, hjust = hjust,
                   vjust = vjust, angle = langle)
  svg$rect(idPrefix=idPrefix, data=ds, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
           fill = fillcolor, alpha=alphasq, size=0.5, color="transparent")
  svg$text(idPrefix=idPrefix, data=data.frame(x=x1+(x2-x1)/2,y=pyy),
           mapping=aes(x=x, y=y),color=labelcolor, 
           label=slabel, size = fsvgtext*lbsize, angle=langle)
  calc_vals <- list("p" = p, "svg" = svg)
  return(calc_vals)
}

# Draw a rectangle. Nodes of inner ziggurats and coremax are rectangles
draw_rectangle<- function(idPrefix,basex,basey,widthx,widthy,grafo,svg,bordercolor,fillcolor,palpha,slabel,
                          inverse="no",sizelabel=3, bordersize =0.5 )
{

  x1 <- c(basex)
  x2 <- c(basex+widthx)
  y1 <- c(basey)
  y2 <- c(basey+widthy)
  ds <- data.frame(x1, x2, y1, y2, fillcolor)
  signo <- 1
  if (inverse == "yes")
  {
    ds$y1 <- -(ds$y1)
    ds$y2 <- -(ds$y2)
    signo <- -1
  }
  if (bordersize > 0)
    p <- grafo + geom_rect(data=ds,
                         mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
                         fill = fillcolor, alpha =palpha, color=bordercolor, linewidth = bordersize, linetype = 3)
  else
    p <- grafo
  p <- p +annotate(geom="text", x=x1+(x2-x1)/8, y=signo*(y1+(y2-y1)/2), label=slabel,
                   colour = fillcolor, size=sizelabel, hjust = 0)
  svg$rect(idPrefix=idPrefix, data=ds, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
           fill=fillcolor, alpha=palpha, color=bordercolor, size=bordersize, linetype=3)
  svg$text(idPrefix=idPrefix, data=data.frame(x=c(x1+(x2-x1)/8), y=c(signo*(y1+(y2-y1)/2))),
           mapping=aes(x=x, y=y), color=fillcolor, label=slabel, size=fsvgtext*sizelabel)

  calc_vals <- list("p" = p, "svg" = svg)
  return(calc_vals)
}


# Computes the weight of links depending on the grouping option chosen by the user
get_link_weights <- function(matrixweight)
{
  if (bpp$weighted_links == "none")
    return(1)
  if (bpp$weighted_links == "log10")
    return(1+log10(matrixweight))
  if (bpp$weighted_links == "ln")
    return(1+log(matrixweight))
  if (bpp$weighted_links == "sqrt")
    return(sqrt(matrixweight))
}

# Draw tail, species or set of species of 1-shell connected to higher k-index species
draw_tail_bip <- function(idPrefix, p,svg,fat_tail,lado,color,sqlabel,basex,basey,gap,
                      lxx2=0,lyy2=0,sqinverse = "no",
                      position = "West", background = "no",
                      first_leaf = "yes", spline = "no",
                      psize = bpp$lsize_kcore1, is_guild_a = TRUE, wlink=1, style = "ziggurat", lvp = 0)
{
 
  adjust <- "yes"
  lvjust <- 0
  lhjust <- 0
  langle <- 0
  ecolor <- "transparent"
  bgcolor <- color
  labelcolor <- ifelse(length(bpp$labels_color)>0,bpp$labels_color[2-as.numeric(is_guild_a)],color)
  palpha <- bpp$alpha_link
  sidex <- lado
  paintsidex <- sidex
  signo <- 1
  yy <- abs(basey)
  plxx2 <- lxx2
  plyy2 <- lyy2
  # Lower half plane of the ziggurat
  if (sqinverse=="yes")
    signo <- -1
  # Tails connected eastwards to inner ziggurats. Fat tails
  if (position == "West"){
    adjust = "yes"
    lhjust <- 0.5
    lvjust <- 0.5
    if (style=="chilopodograph"){
      xx <- basex-gap
      posxx1 <- xx+bpp$xstep
      posyy1 = signo*(yy)+signo*(0.5*bpp$xstep/(bpp$aspect_ratio))
    } else {
      xx <- basex-gap
      posxx1 <- xx+sidex
      posyy1 = signo*(yy)+signo*(0.5*gap/(bpp$aspect_ratio))
    }
  }
  # Group of species linked to the highest kdegree species in max shell
  else if (position == "East"){
      gap <- bpp$hop_x/2
      xx <- basex+gap
      posxx1 <- xx
      posyy1 = signo*(yy+sidex/(2*bpp$aspect_ratio))
  }
  # Tails connected to other nodes of the max shell
  else if ((position == "North") |(position == "South")) {
    xx <- basex
    posxx1 <- xx
    posyy1 = signo*(yy)
    if (style=="chilopodograph"){
      adjust = "yes"
      lhjust <- 0.5
      lvjust <- 0.5
    }
  }

  if (background == "no")
  {
    ecolor <- "transparent"
    if (bpp$alpha_level != 1)
      palpha <- max(bpp$alpha_level-0.09,0)
      else if (position == "North")
      {
        langle <- rot_angle
      }
      else if (position == "South")
      {
        langle <- -rot_angle
      }
      else if (position == "West"){
        adjust <- "yes"
      }
  }
  # if ((bpp$flip_results) & (langle == 0) & (position!="West") )
  #   langle <- langle + 70
  # Draw square node

  f <- draw_square(idPrefix, p,svg, xx,yy,
                   ifelse(bpp$style=="chilopodograph",lado,
                   paintsidex*sqrt(bpp$square_nodes_size_scale)),
                   bgcolor,palpha,labelcolor,langle,lhjust,lvjust,
                   slabel=sqlabel,lbsize = psize,inverse = sqinverse,
                   adjustoxy = adjust, edgescolor = ecolor)
  p <- f["p"][[1]]
  svg <- f["svg"][[1]]
  # Add tail link
  if (bpp$paintlinks){
    if ((position == "North") |(position == "South"))
      posxx1 = posxx1+sidex/2
    add_link(xx1=posxx1, xx2 = plxx2,
                   yy1 = posyy1, yy2 = plyy2,
                   slink = bpp$size_link*wlink, clink = c(bpp$color_link),
                   alpha_l = bpp$alpha_link)
  }
  calc_vals <- list("p" = p, "svg" = svg, "sidex" = sidex, "xx" = posxx1, "yy" = posyy1)
  return(calc_vals)
}


draw_edge_tails_bip <- function(p,svg,point_x,point_y,kcoreother,long_tail,list_dfs,color_guild, inverse = "no",
                            vertical = "yes", orientation = "South", revanddrop = "no",
                            pbackground = "yes", joinchars = "\n", tspline = "no", 
                            is_guild_a = TRUE, wlink = 1)
{

  rxx <- point_x
  ryy <- point_y
  bpp$joinstr <- joinchars
  signo <- 1
  if (inverse == "yes")
    signo <- -1
  list_spec <- list_dfs[[kcoreother]]$label
  if (revanddrop == "yes")
    list_spec <- rev(list_spec)[1:length(list_spec)-1]
  if (orientation == "East")
    list_spec <- rev(list_spec)
  llspec <- length(list_spec)
  m <- 0
  separacion <- 0.035*bpp$tot_width
  last_vertical_position <- 0
  for (i in list_spec)
  {
    conn_species <- which(long_tail$partner == i)
    if (length(conn_species)>0)
    {
      little_tail <- long_tail[long_tail$partner == i,]
      
      data_row <- list_dfs[[kcoreother]][which(list_dfs[[kcoreother]]$label == i),]
      xx2 <- (data_row$x2+data_row$x1)/2
      rxx <- data_row$x1
      yy2 <- data_row$y2
      yfactor <- min(10,nrow(little_tail))
      if (nrow(little_tail>1) & last_vertical_position == yfactor)
        yfactor <- yfactor+1
      ryy <- data_row$y2 + ifelse(yy2>0, yfactor*bpp$xstep, -yfactor*bpp$xstep )
      tailweight <- 0
      for (h in 1:nrow(little_tail))
        if (is_guild_a)
          tailweight <- tailweight + bpp$result_analysis$matrix[as.numeric(little_tail$partner[h]),
                                                                        as.numeric(little_tail$orph[h])]
        else
          tailweight <- tailweight + bpp$result_analysis$matrix[as.numeric(little_tail$orph[h]),
                                                                as.numeric(little_tail$partner[h])]
      little_tail$weightlink <- get_link_weights(tailweight)
      v<- draw_tail_bip(paste0(ifelse(is_guild_a, "edge-kcore1-a-", "edge-kcore1-b-"), i),
                    p,svg,little_tail,0.9*bpp$xstep,color_guild[2],
                    gen_vert_label(little_tail$orph,joinchars = " "),
                    rxx,ryy,bpp$gap,lxx2 = xx2,
                    lyy2 = yy2, sqinverse = inverse, position = orientation,
                    background = pbackground, spline = tspline, psize = bpp$lsize_kcore1,
                    is_guild_a = is_guild_a, wlink = little_tail$weightlink[1],style=bpp$style,
                    lvp = last_vertical_position)
      last_vertical_position <- ifelse(yfactor==1, 0, yfactor)
      p <- v["p"][[1]]
      svg <- v["svg"][[1]]
      rxx <- v["xx"][[1]]
      ryy <- v["yy"][[1]]
      if (vertical == "yes"){
        salto <- v["sidex"][[1]]/bpp$aspect_ratio
        point_y <- point_y + 1.4*signo*salto
        rxx <- point_x
      }

      # tails connected to kcoremax except first species

      else{
        if (orientation == "West")
          salto <- 0
        else
          salto <- 0.4*v["sidex"][[1]]/bpp$aspect_ratio
        point_x <- point_x #- separacion - v["sidex"][[1]]
        point_y <- point_y #- 1.4*signo*salto
        ryy <- point_y
        rxx <- point_x
      }
    }
    else
      last_vertical_position <- 0
    m <- m +1
  }
  calc_vals <- list("p" = p, "svg" = svg, "lastx" = rxx, "lasty" = ryy)
  return(calc_vals)
}


# Species disconnected of the Giant Component, called 'outsiders'
# Compute coordinates
conf_outsiders <- function(outsiders,basex,basey,sidex,fillcolor,strguild)
{
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  r <- c()
  col_row <- c()
  numboxes <- length(outsiders)
  pbasex <- basex
  xstep <- 2*sidex*bpp$outsiders_separation_expand
  xsep <- 2.5*bpp$outsiders_separation_expand
  ysep <- xsep
  for (j in (1:numboxes))
  {
    x1 <- c(x1, pbasex+(j*xsep*xstep))
    x2 <- c(x2, x1[j]+xstep)
    y1 <- c(y1, basey-ysep*xstep/bpp$aspect_ratio)
    y2 <- c(y2, y1[j]-xstep/bpp$aspect_ratio)
    r <- c(r,j)
    col_row <- c(col_row,fillcolor)
  }
  d1 <- data.frame(x1, x2, y1, y2, r, col_row)
  d1$label <- ""
  for (i in 1:length(outsiders))
    d1[i,]$label <- strsplit(outsiders[i],strguild)[[1]][2]
  return(d1)
}

# Draw outsider square nodes
draw_sq_outsiders <- function(idPrefix, p,svg,dfo,paintsidex,alpha_level,lsize,is_guild_a = TRUE)
{
  if (length(bpp$labels_color)>0)
    labelscolor <- rep(bpp$labels_color[2-as.numeric(is_guild_a)],nrow(dfo))
  else
    labelscolor <- dfo$col_row
  p <- p + geom_rect(data=dfo, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
                     fill = dfo$col_row, alpha = alpha_level,color="transparent") +
       geom_text(data=dfo, aes(x=(x2+x1)/2, y= (y2+y1)/2), color=labelscolor,
              label = dfo$label, size=lsize, vjust=0.5)
  svg$rect(idPrefix=idPrefix, data=dfo, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill=dfo$col_row, alpha=alpha_level,
           color="transparent", size=0.5)
  svg$text(idPrefix=idPrefix, data=dfo, mapping=aes(x=(x2+x1)/2, y= (y2+y1)/2), color=labelscolor, label=dfo$label, 
           size=fsvgtext*lsize)
  calc_vals <- list("p" = p, "svg" = svg)
  return(calc_vals)
}

# Full outsiders management procedure
handle_outsiders <- function(p,svg,outsiders,df_chains) {
  if (length(bpp$outsider)>0){
    paintsidex <- 2*bpp$height_y*bpp$aspect_ratio
    paintsidex <- paintsidex * sqrt(bpp$square_nodes_size_scale)
    bpp$outsiders_a <- bpp$outsider$name[grep(bpp$str_guild_a,bpp$outsider$name)]
    bpp$outsiders_b <- bpp$outsider$name[grep(bpp$str_guild_b,bpp$outsider$name)]
    pox <- -(bpp$hop_x/4)+ bpp$tot_width * (bpp$displace_outside_component[1])
    poy <- min(-bpp$last_ytail_b[!is.na(bpp$last_ytail_b)]-4*bpp$lado,df_chains$y1) 
    if (poy<0) 
      poy <- poy*(1-bpp$displace_outside_component[2])
    else
      poy <- poy*(1+bpp$displace_outside_component[2])
    dfo_a <- conf_outsiders(bpp$outsiders_a,pox,poy,
                            bpp$lado*sqrt(bpp$square_nodes_size_scale),bpp$color_guild_a[2],bpp$str_guild_a)
    guild_sep <- poy-max(1,length(bpp$outsider)/10)*6*bpp$lado*sqrt(bpp$square_nodes_size_scale)*bpp$outsiders_separation_expand/bpp$aspect_ratio
    dfo_b <- conf_outsiders(bpp$outsiders_b,pox,guild_sep,
                            bpp$lado*sqrt(bpp$square_nodes_size_scale),bpp$color_guild_b[2],bpp$str_guild_b)
    f <- draw_sq_outsiders("edge-kcore1-a",p,svg,dfo_a,paintsidex,bpp$alpha_level,bpp$lsize_kcore1)
    p <- f["p"][[1]]
    svg <- f["svg"][[1]]
    f <- draw_sq_outsiders("edge-kcore1-b",p,svg,dfo_b,paintsidex,bpp$alpha_level,bpp$lsize_kcore1, is_guild_a = FALSE)
    p <- f["p"][[1]]
    svg <- f["svg"][[1]]
    for (j in 1:nrow(dfo_a))
    {
      mtxa <- bpp$mtxlinks[which(bpp$mtxlinks$guild_a == paste0(bpp$str_guild_a,dfo_a[j,]$label)),]
      for (i in 1:nrow(dfo_b))
      {
        if (sum(as.character(mtxa$guild_b) == paste0(bpp$str_guild_b,dfo_b[i,]$label))>0)
        {
          bend_line = "no"
          link <- data.frame(x1=c(dfo_a[j,]$x1 + (dfo_a[j,]$x2-dfo_a[j,]$x1)/2),
                             x2 = c(dfo_b[i,]$x1 +(dfo_b[i,]$x2-dfo_b[i,]$x1)/2),
                             y1 = c(dfo_a[j,]$y2),  y2 = c(dfo_b[i,]$y1) )
          lcolor = "orange"
          tailweight <- get_link_weights(bpp$result_analysis$matrix[as.numeric(dfo_b[i,]$label),
                                                                                       as.numeric(dfo_a[j,]$label)])
          add_link(xx1=link$x1, xx2 = link$x2,
                         yy1 = link$y1, yy2 = link$y2,
                         slink = bpp$size_link*tailweight, clink = c(bpp$color_link),
                         alpha_l = bpp$alpha_link , spline = bend_line)
        }
      }
    }
    margin <- bpp$height_y
    x_inf <- min(dfo_a$x1,dfo_b$x1) - 1.5*margin
    widthx <- max(dfo_a$x2,dfo_b$x2) - x_inf + margin
    y_inf <- min(dfo_a$y2,dfo_b$y2) - 2*margin/bpp$aspect_ratio
    widthy <- max(dfo_a$y2,dfo_b$y2) - y_inf + 2*margin/bpp$aspect_ratio
    divcolor <- "grey50"
    position_x_text <- x_inf+20*bpp$outsiders_legend_expand
    corelabel <- paste("Outside the giant component")
    position_y_text <- y_inf + margin/bpp$aspect_ratio + (0.9+0.2*bpp$outsiders_legend_expand)*widthy
    px <- position_x_text
    py <- position_y_text
    if (bpp$flip_results){
      px <- x_inf + widthx + margin * bpp$outsiders_legend_expand
      py <- y_inf
    }
    p <- p +annotate(geom="text", x=px, y=py, label=corelabel, colour = divcolor,
                     size=bpp$lsize_core_box, hjust = 0, vjust = 0, angle = 0)

    svg$text("corelabel", data=data.frame(x=c(px), y=c(py)), mapping=aes(x=x, y=y), color=divcolor, 
             label=corelabel, size=fsvgtext*bpp$lsize_core_box)
  }
  calc_vals <- list("p" = p, "svg" = svg)
  return(calc_vals)
}

# Draw one triangle of the innermost shell
draw_parallel_guilds <- function(basex,topx,basey,topy,numboxes,nnodes,fillcolor,strlabels,
                                 igraphnet,strguild,orderby = "kradius",style="legacy",guild="A")
{
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  r <- c()
  kdegree <- c()
  kradius <- c()
  col_row <- c()
  name_species <- c()
  pbasex <- bpp$coremax_triangle_width_factor*( basex - (nnodes %/%8) * abs(topx-basex)/3)
  xstep <- bpp$square_nodes_size_scale*(topx-pbasex)/max(12,nnodes)
  bpp$xstep <- xstep
  vertsep <- 3
  ptopy <- vertsep*basey+(if (basey>0) 1 else -1)*xstep
  ystep <- 0
  
  for (j in (1:numboxes))
  {
    x1 <- c(x1, pbasex+(j-1)*xstep)
    x2 <- c(x2, x1[j]+0.9*bpp$xstep)
    y1 <- c(y1, vertsep*basey)
    y2 <- c(y2, ptopy)#-(j-1)*ystep)
    r <- c(r,j)
    col_row <- c(col_row,fillcolor[1+j%%2])
    kdegree <- c(kdegree,0)
    kradius <- c(kradius,1)
    name_species <- c(name_species,"")
  }
  d1 <- data.frame(x1, x2, y1, y2, r, col_row, kdegree, kradius, name_species, stringsAsFactors=FALSE)
  d1$label <- strlabels
  
  # Remove empty nodes
  d1 <- d1[d1$label!="EMPTY",]
  
  if (style == "legacy")
    nodelabels <- strlabels
  else if ((style == "kcoreorder")||(style == "chilopodograph")) {
    d1$kcore = 0
    for (i in 1:nrow(d1)){
      s <- strsplit(d1$label[i],"shell")
      d1$label[i] <- s[[1]][1]
      d1$kcore[i] <- s[[1]][2]
    }
  }
  d1 <- d1[!(d1$label %in% c("","NA")),]         # Remove empty kshell separator cells 
  d1 <- d1[!is.na(d1$x1),] 
  for (i in 1:nrow(d1)){
    d1[i,]$kdegree <- igraphnet[paste0(strguild,d1[i,]$label)]$kdegree
    d1[i,]$kradius <- igraphnet[paste0(strguild,d1[i,]$label)]$kradius
    d1[i,]$name_species <- igraphnet[paste0(strguild,d1[i,]$label)]$name_species
  }

  if (style == "legacy"){
    mlinks <- bpp$result_analysis$matrix > 0
    if (guild=="A")
      degrees <- colSums(mlinks)
    else if (guild=="B")
      degrees <- rowSums(mlinks)
    
    d1$degree <- 0
    for (i in 1:nrow(d1))
      d1$degree[i]=degrees[which(names(degrees)==d1[i,]$name_species)]
    ordvector <- rev(order(d1$degree))
    d1$label <- d1[ordvector,]$label
    d1$kradius <- d1[ordvector,]$kradius
    d1$kdegree <- d1[ordvector,]$kdegree
    d1$name_species <- d1[ordvector,]$name_species
    d1$degree <- d1[ordvector,]$degree
    # if (orderby == "kradius"){
    #   ordvector <- order(1000*d1$kradius-d1$kdegree)
    #   d1$label <- d1[ordvector,]$label
    #   d1$kradius <- d1[ordvector,]$kradius
    #   d1$kdegree <- d1[ordvector,]$kdegree
    #   d1$name_species <- d1[ordvector,]$name_species
    # }
    # else if (orderby == "kdegree"){
    #   ordvector <- rev(order(1000*d1$kdegree-d1$kradius))
    #   d1$label <- d1[ordvector,]$label
    #   d1$kradius <- d1[ordvector,]$kradius
    #   d1$kdegree <- d1[ordvector,]$kdegree
    #   d1$name_species <- d1[ordvector,]$name_species
    # }
  } else if ((style=="kcoreorder") || (style=="chilopodograph")){
    subscol <- which(names(d1)=="kdegree"):ncol(d1)
    shells <- sort(unique(d1$kcore))
    for (k in shells){
      d2 <- d1[d1$kcore == k,]
      if (nrow(d2)>1){
        d2 <-d2[rev(order(1000*d2$kdegree-d2$kradius)),]
        d1[d1$kcore==k,][,subscol] <- d2[,subscol]
      }
      
    }
  }
  
  # Fixed aspect ratio of bipartite plot
  
  bpp$tot_width <- max(max(d1$x2)+xstep,bpp$tot_width)
  bpp$tot_height <- (9/16)*bpp$tot_width

  return(d1)
}

# This function adds the following information for species of kcore 1 in lists_dfs_x: label, name_species, kdegree, kradius
conf_kcore1_info <- function(strguild)
{
  auxlistdf <- data.frame(x1=NA,x2=NA,y1=NA,y2=NA,r=NA,col_row=NA,kdegree=NA,kradius=NA,name_species=NA,label=NA)
  retlistdf <- data.frame(x1=c(),x2=c(),y1=c(),y2=c(),r=c(),col_row=c(),kdegree=c(),kradius=c(),name_species=c(),label=c())
  num_s <- bpp$df_cores[1,]$num_species_guild_a
  if (strguild == bpp$str_guild_a)
    listspecies = bpp$df_cores[1,]$species_guild_a
  else
    listspecies = bpp$df_cores[1,]$species_guild_b
  for (j in listspecies[[1]])
  {
    ind <- paste0(strguild,j)
    auxlistdf$label <- j
    auxlistdf$kdegree <-  V(bpp$result_analysis$graph)[ind]$kdegree
    auxlistdf$kradius <-  V(bpp$result_analysis$graph)[ind]$kradius
    auxlistdf$name_species <-  V(bpp$result_analysis$graph)[ind]$name_species
    retlistdf <- rbind(retlistdf,auxlistdf)
  }
  return(retlistdf)
}

# Similar to conf_kcore1_info for species outside the giant component
conf_outsiders_info <- function(strguild)
{
  auxlistdf <- data.frame(x1=NA,x2=NA,y1=NA,y2=NA,r=NA,col_row=NA,kdegree=NA,kradius=NA,name_species=NA,label=NA)
  retlistdf <- data.frame(x1=c(),x2=c(),y1=c(),y2=c(),r=c(),col_row=c(),kdegree=c(),kradius=c(),name_species=c(),label=c())
  listspecies <- NULL
  num_s <- bpp$cores[1,]$num_species_guild_a
  if (strguild == bpp$str_guild_a)
      listspecies = bpp$outsiders_a
  else
      listspecies = bpp$outsiders_b
  for (j in listspecies)
  {
    auxlistdf$label <- gsub(strguild,"",j)
    auxlistdf$kdegree <-  V(bpp$result_analysis$graph)[j]$kdegree
    auxlistdf$kradius <-  V(bpp$result_analysis$graph)[j]$kradius
    auxlistdf$name_species <-  V(bpp$result_analysis$graph)[j]$name_species
    retlistdf <- rbind(retlistdf,auxlistdf)
  }
  return(retlistdf)
}

# Find orphans. Orphans are species 1-shell connected to species 1-shell
# They form weird chains AKA chains of specialists
find_orphans <- function(mtxlinks,orphans,gnet,guild_a="yes")
{
  m <- 0
  orph <- NA
  partner <- NA
  kcore <- NA
  repeated <- NA
  df_orph <- data.frame(orph,partner,kcore,repeated)
  if (!is.null(orphans))
    for (i in orphans)
    {
      if (guild_a == "yes")
      {
        partner <- mtxlinks$guild_b[(mtxlinks$guild_a == paste0(bpp$str_guild_a,i))]
        str_opp <- bpp$str_guild_b
        str_own <- bpp$str_guild_a
      }
      else{
        partner <- mtxlinks$guild_a[(mtxlinks$guild_b == paste0(bpp$str_guild_b,i))]
        str_opp <- bpp$str_guild_a
        str_own <- bpp$str_guild_b
      }
      for (t in 1:length(partner))
      {
        m <- m+1
        df_orph[m,]$orph <- i
        df_orph[m,]$partner <- strsplit(as.character(partner[t]),str_opp)[[1]][2]
        df_orph[m,]$kcore <- bpp$g[as.character(partner[t])]$kcorenum
        if (length(partner)>1)
          df_orph[m,]$repeated = "yes"
        else
          df_orph[m,]$repeated <- "no"
      }
      df_orph <- df_orph[!is.na(df_orph$orph),]
    }
  return(df_orph)
}

# Add one link to the graph
add_link <- function(xx1 = 0,xx2 = 0,yy1 = 0,yy2 = 0,
                      slink = 1,clink = c("gray70"),alpha_l = 0.1, spline = "no")
{
  if (!bpp$use_spline)
    spline  <- "no"
  link <- data.frame(x1=xx1, x2 = xx2, y1 = yy1,  y2 = yy2, weightlink = slink)
  npoints_link <- bpp$spline_points
  col_link <- clink[1]
  if (spline == "no")
    bpp$straight_links <- rbind(bpp$straight_links,link)
  else{
    if (spline == "horizontal"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.05,link$x1+(link$x2-link$x1)*0.75,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.1,link$y1+(link$y2-link$y1)*0.65,link$y2)
    }
    else if (spline == "diagonal"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.5,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.55,link$y2)
    }
    else if (spline == "vertical"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.85,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.9,link$y2)
    }
    else if (spline == "lshaped"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.80,link$x1+(link$x2-link$x1)*0.90,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.95,link$y1+(link$y2-link$y1)*0.99,link$y2)
    }
    else if (spline == "specialisthorizontal"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.40,link$x1+(link$x2-link$x1)*0.75,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.6,link$y1+(link$y2-link$y1)*0.70,link$y2)
    }
    else if (spline == "arc"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.9,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.85,link$y2)
    }
    xout <- seq(min(x),max(x),length.out = npoints_link)
    s1 <- spline(x,y,xout=xout,method='natural')
    ds1 <- as.data.frame(s1)
    bpp$count_bent_links <- bpp$count_bent_links + 1
    ds1$number <- bpp$count_bent_links
    ds1$weightlink <- slink
    bpp$bent_links <- rbind(bpp$bent_links,ds1)

  }
  return(0)
}

# Analysis of the chains of specialists
specialist_analysis <- function(specialists,opposite_specialists,species)
{
  ldf <- specialists[specialists$orph == species,]
  if (max(ldf$kcore)>1)
    return(ldf)
}

# Store speciailsts in intermediate df_store data frame and compute positions
# Very hard, indeed
store_specialist_species <- function (row_orph, df_store, strguild, lado, gap, original_specialists_a, original_specialists_b)
{

  sidex <- lado
  index <- nrow(df_store)+1
  df_store[index,]$kcorepartner <- row_orph$kcore
  separation <- (1+bpp$specialists_boxes_separation_count)*sidex
  tot_specialists <- nrow(original_specialists_a)+nrow(original_specialists_b)
  jumpfactor <- (4-min(3,(tot_specialists%/%10)))
  cgap <- (lado+gap/(5-min(3,(tot_specialists%/%10))))

  if (row_orph$kcore > 1){
    df_store$guild[index] <- as.character(strguild)
    df_store$orph[index] <- row_orph$orph
    df_store$partner[index] <- row_orph$partner
    if (strguild == bpp$str_guild_b)
      data_row <- bpp$list_dfs_a[[row_orph$kcore]][bpp$list_dfs_a[[row_orph$kcore]]$label==row_orph$partner,]
    if (strguild == bpp$str_guild_a)
      data_row <- bpp$list_dfs_b[[row_orph$kcore]][bpp$list_dfs_b[[row_orph$kcore]]$label==row_orph$partner,]
    if (row_orph$kcore == bpp$kcoremax)
    {
      if (strguild == bpp$str_guild_b){
        edge_row <- bpp$list_dfs_a[[bpp$kcoremax]][1,]
        xbase <-  min(bpp$last_xtail_a[[bpp$kcoremax]],edge_row$x1 - 2*gap) - gap
      }
      else{
        edge_row <- bpp$list_dfs_b[[bpp$kcoremax]][1,]
        xbase <-  min(bpp$last_xtail_b[[bpp$kcoremax]],edge_row$x1 - 2*gap)- gap
      }
      df_store$x1[index] <- xbase - 2 * gap

      if (bpp$kcoremax > 2)
        df_store$y1[index] <- max(abs(edge_row$y2),abs(edge_row$y1)) + 6*cgap/(bpp$aspect_ratio)
      else{
        xbase <- 0
        df_store$y1[index] <- max(abs(edge_row$y2),abs(edge_row$y1)) + 6*cgap/(bpp$aspect_ratio)
      }

      if (df_store$guild[index] == bpp$str_guild_a){
        df_store$y1[index] = -abs(df_store$y1[index])
        df_store$y2[index] = -abs(df_store$y2[index])
      }
      repetitions <- sum((df_store$partner == row_orph$partner) & (df_store$guild == strguild))
      if (repetitions > 1){
        df_store$x1[index] <- df_store$x1[index] -  (repetitions-1) * sidex
        df_store$y1[index] <- df_store$y1[index] +  sign(df_store$y1[index])*(repetitions-1) * (3+as.integer(bpp$kcoremax>3)) * sidex/bpp$aspect_ratio
      }
      repetitions_root <- sum((df_store$kcorepartner == bpp$kcoremax) & (df_store$guild == strguild))
      if (repetitions_root > 1){
        df_store$x1[index] <- df_store$x1[index] +  2 * (repetitions_root) * (bpp$kcoremax) * sidex
        df_store$y1[index] <- df_store$y1[index] +  sign(df_store$y1[index])*(1/jumpfactor)*((repetitions_root+ 0.7*index) * 3* sidex/bpp$aspect_ratio)
      }

    } else {
      if (row_orph$kcore == 2){
        xoffset <- 2*bpp$specialistskcore2_horizontal_dist_rootleaf_expand*separation   # Controls the separation of specialists root leaves connected to core 2
        bpp$yoffset <- bpp$specialistskcore2_vertical_dist_rootleaf_expand*separation/bpp$aspect_ratio
      } else{
        xoffset <- 0
        bpp$yoffset <- 0
      }
      if (strguild == bpp$str_guild_b)
      {
        data_row_pic <- bpp$list_dfs_a[[row_orph$kcore]]
        df_store$x1[index]<-  min(data_row_pic$x2) + bpp$factor_hop_x* separation + xoffset
        df_store$y1[index] <- bpp$last_ytail_a[row_orph$kcore] - (1+sqrt(bpp$kcoremax))*sidex/bpp$aspect_ratio - bpp$yoffset
        bpp$last_ytail_a[row_orph$kcore] <- -abs(df_store$y1[index])
      }
      if (strguild == bpp$str_guild_a)
      {
        data_row_pic <- bpp$list_dfs_b[[row_orph$kcore]]
        df_store$x1[index] <- min(data_row_pic$x2) + bpp$factor_hop_x* separation + xoffset
        df_store$y1[index] <- bpp$last_ytail_b[row_orph$kcore] +  (1+sqrt(bpp$kcoremax))*sidex/bpp$aspect_ratio + bpp$yoffset
        bpp$last_ytail_b[row_orph$kcore] <- abs(df_store$y1[index])
      }
    }
    if (row_orph$kcore == bpp$kcoremax)
      df_store$x1[index] <- df_store$x1[index]* bpp$root_specialist_expand[1]
    else if (bpp$root_specialist_expand[1]>1)
      df_store$x1[index] <- df_store$x1[index]* bpp$root_specialist_expand[1]
    else if (row_orph$kcore > 2)
      df_store$x1[index] <- df_store$x1[index]* (9+bpp$root_specialist_expand[1])/10
    df_store$y1[index] <- df_store$y1[index]* bpp$root_specialist_expand[2] * bpp$aspect_ratio
  }
  else {                                          # Branch leaves
    df_store$partner[index] <- row_orph$orph
    df_store$orph[index] <- row_orph$partner
    df_store$guild[index] <- as.character(strguild)
    data_row <- df_store[(df_store$orph == row_orph$orph) & (swap_strguild(strguild) == df_store$guild),]
    repetitions <- sum((df_store$partner == row_orph$orph) & (df_store$guild == strguild))

    if (data_row$kcorepartner == bpp$kcoremax){
      if (bpp$kcoremax > 2){
        df_store$x1[index] <- data_row$x1 - 1.5*separation*(repetitions)
        df_store$y1[index] <- data_row$y1 + sign(data_row$y1)*(((3+(bpp$specialists_boxes_separation_count))*bpp$height_y) + ((repetitions-1)*sidex/bpp$aspect_ratio))
      }
      else{
        df_store$x1[index] <- data_row$x1 - (4+0.2*sqrt(index))*sidex
        df_store$y1[index] <- data_row$y1+ sign(data_row$y1)*(bpp$specialists_boxes_separation_count*bpp$height_y + sidex/bpp$aspect_ratio) +
          (2+0.2*sqrt(index))*sign(data_row$y1)*(repetitions-1)*sidex/bpp$aspect_ratio
      }
    }
    else{                                   # specialists connected to root leaf connected to kcoremax
      hjump <- sign(data_row$x1)*bpp$factor_hop_x* separation
      df_store$x1[index] <- data_row$x1 + hjump
      df_store$y1[index] <- data_row$y1 + sign(data_row$y1)*2*(repetitions-1)*sidex/bpp$aspect_ratio
    }
    reps <- sum((df_store$partner == df_store$partner[index]) & (df_store$guild == strguild))

    if (bpp$kcore1specialists_leafs_vertical_separation!=1){
      addjump <- sign(df_store$y1[index])*reps*bpp$kcore1specialists_leafs_vertical_separation*sidex/bpp$aspect_ratio
      df_store$y1[index]<- addjump + df_store$y1[index]
    }

  }
  df_store$x2[index] <- df_store$x1[index] + sidex
  df_store$y2[index] <- df_store$y1[index] + sidex/bpp$aspect_ratio
  if (row_orph$kcore == bpp$kcoremax){
    df_store$xx2[index] <- (data_row$x2+data_row$x1)/2
    df_store$yy2[index] <- data_row$y2
  }
  else
  {
    df_store$yy2[index] <- (data_row$y2+data_row$y1)/2
    if (df_store$kcorepartner[index] > 1){
      df_store$xx2[index] <- data_row$x2
    }
    else {
      df_store$xx2[index] <- data_row$x1 + (data_row$x2>0)*sidex
    }
  }
  return(df_store)
}


# Draw a chain of specialists
draw_specialist_chains <- function(grafo, svg, df_chains, ladosq)
{
  p <- grafo
  df_chains$weightlink <- 0
  for (i in 1:nrow(df_chains))
  {
    sqinverse = "no"
    if (df_chains[i,]$guild == bpp$str_guild_a){
      bgcolor <- bpp$color_guild_a[2]
      is_guild_a <- TRUE
    }
    if (df_chains[i,]$guild == bpp$str_guild_b){
      bgcolor <- bpp$color_guild_b[2]
      is_guild_a <- FALSE
    }
    hjust <- 0
    vjust <- 0
    labelcolor <- ifelse(length(bpp$labels_color)>0,bpp$labels_color[2-as.numeric(is_guild_a)], bgcolor)
    sqlabel = gen_sq_label(df_chains[i,]$orph,is_guild_a = is_guild_a)
    f <- draw_square(paste0(ifelse(is_guild_a, "edge-kcore1-a-", "edge-kcore1-b-"), i),p,
                     svg,df_chains[i,]$x1,df_chains[i,]$y1, ladosq,
                     bgcolor,bpp$alpha_level,
                     labelcolor,0,hjust,vjust,
                     sqlabel,
                     lbsize = bpp$lsize_kcore1,inverse = "no")
    p <- f["p"][[1]]
    svg <- f["svg"][[1]]
    if (bpp$paintlinks){

      yy2 = df_chains[i,]$yy2
      xx2 = df_chains[i,]$xx2
      if (df_chains[i,]$x2>0){
        xx1 = df_chains[i,]$x1
      } else {
        xx1 = df_chains[i,]$x2
      }
      if (df_chains[i,]$y2>0){
        yy1 <-  (df_chains[i,]$y1+df_chains[i,]$y2)/2
      } else {
        if (df_chains[i,]$kcorepartner != bpp$kcoremax)
          yy1 <-  df_chains[i,]$y2 - ladosq/(2*bpp$aspect_ratio)
        else
          yy1 <-  df_chains[i,]$y2
      }


      if ( (df_chains[i,]$kcorepartner >1) & (df_chains[i,]$kcorepartner < bpp$kcoremax) )
      {
        splineshape = "lshaped"
      }
      else
        splineshape = "arc"
      tailweight <- 0
      # for (h in 1:nrow(df_chains))
        if (df_chains$guild[i] == bpp$str_guild_a)
          tailweight <- tailweight + bpp$result_analysis$matrix[as.numeric(df_chains$partner[i]),
                                                                      as.numeric(df_chains$orph[i])]
        else
          tailweight <- tailweight + bpp$result_analysis$matrix[as.numeric(df_chains$orph[i]),
                                                                as.numeric(df_chains$partner[i])]
      df_chains[i,]$weightlink <- get_link_weights(tailweight)
      add_link(xx1=xx1, xx2 = xx2,
                     yy1 = yy1, yy2 = yy2,
                     slink = bpp$size_link*df_chains[i,]$weightlink, clink = c(bpp$color_link),
                     alpha_l = bpp$alpha_link , spline = splineshape)
    }
  }
  calc_vals <- list("p" = p, "svg" = svg)
  return(calc_vals)
}

swap_strguild <- function(strguild)
{
  if (strguild == bpp$str_guild_a)
    strguild = bpp$str_guild_b
  else
    strguild = bpp$str_guild_a
  return(strguild)
}

# Store the root node of a chain of specialists
store_root_leaf <- function(specialists,df_chains,strguild,lado, gap, original_specialists_a, original_specialists_b)
{
  for (i in 1:nrow(specialists))
  {
    if (specialists[i,]$kcore > 1){
      df_chains <- store_specialist_species(specialists[i,], df_chains, strguild, lado , gap, original_specialists_a, original_specialists_b)
      specialists[i,]$drawn = "yes"
    }
  }
  calc_vals <- list("df_chains" = df_chains, "specialists" = specialists)
  return(calc_vals)
}

# Store leafs of a specialist chain
store_branch_leaf <- function(specialists, specialists_opp,df_chains, pstrguild, plado, gap, original_specialists_a, original_specialists_b)
{
  for (i in 1:nrow(specialists))
  {
    if (specialists[i,]$drawn == "no"){
      strguild <- pstrguild
      if (sum( ((df_chains$orph == specialists[i,]$orph) & (df_chains$guild == strguild)) )>0 ){
        strguild <- swap_strguild(pstrguild)
        df_chains <- store_specialist_species(specialists[i,], df_chains, strguild, plado, gap, original_specialists_a, original_specialists_b)
        specialists[i,]$drawn = "yes"
        mirror_specialist <- which((specialists_opp$partner == specialists[i,]$orph) & (specialists_opp$orph == specialists[i,]$partner))
        if (length(mirror_specialist)>0)
          specialists_opp[mirror_specialist , ]$drawn = "yes"
      }
    }
  }
  calc_vals <- list("df_chains" = df_chains, "specialists" = specialists, "specialists_opp" = specialists_opp)
  return(calc_vals)
}

# Draw links among species of same k-index
draw_innercores_tails_bip <- function(p,svg,kc,list_dfs,df_orph,color_guild, inverse="no", is_guild_a = TRUE)
{
  lastx <- 0
  lasty <- 0

  lpoint_x <- 0
  if (length(list_dfs[[kc]])>0)
    if (kc>2)
      lpoint_x <- list_dfs[[kc]][nrow(list_dfs[[kc]]),]$x2
  else
    lpoint_x <- list_dfs[[kc]][nrow(list_dfs[[kc]]),]$x2 + 4*bpp$lado
  if (kc>2)
    lpoint_y <- (list_dfs[[kc]][1,]$y1+list_dfs[[kc]][1,]$y2)/2
  else{
    if (bpp$kcoremax > 2)
      lpoint_y <- (list_dfs[[kc]][1,]$y1+list_dfs[[kc]][1,]$y2)/4
    else{
      bpp$kcore2tail_vertical_separation <- bpp$kcore2tail_vertical_separation + 2
      lpoint_y <- 1.5*max(list_dfs[[2]]$y2)
    }
  }
  long_tail <- df_orph[(df_orph$kcore == kc) & (df_orph$repeated == "no"),]
  if (length(long_tail)>0){
    v<-  draw_edge_tails_bip(p,svg,lpoint_x,lpoint_y,kc,long_tail,list_dfs,color_guild,
                         inverse = inverse, joinchars = bpp$joinstr,pbackground = "no",
                         tspline = "lshaped", is_guild_a = is_guild_a)
    p <- v["p"][[1]]
    svg <- v["svg"][[1]]
    if (length(v["lastx"][[1]])>0)
      lastx <- v["lastx"][[1]]
    if (length(v["lasty"][[1]])>0)
      lasty <-v["lasty"][[1]]
  }
  calc_vals <- list("p" = p, "svg" = svg, "point_x" = lpoint_x, "point_y" = lpoint_y, "lastx" = lastx, "lasty" = lasty)
  return(calc_vals)
}

# Draw lnks amnog inner ziggurats
draw_inner_links <- function(p, svg)
{
    for (kcb in seq(bpp$kcoremax,2))
    {
      for (kc in seq(bpp$kcoremax,2))
      {
        labels_a <- bpp$list_dfs_a[[kc]]$label
        for (j in seq(along=labels_a))
        {
          numberlinksa <- sum(bpp$mtxlinks$guild_a == paste0(bpp$str_guild_a,labels_a[j]) )
          data_a <- bpp$list_dfs_a[[kc]][j,]
          foundlinksa <- 0
          labels_b <- bpp$list_dfs_b[[kcb]]$label
          for (i in seq(along =labels_b))
          {
            if (sum(bpp$mtxlinks$guild_a == paste0(bpp$str_guild_a,labels_a[j]) & bpp$mtxlinks$guild_b == paste0(bpp$str_guild_b,labels_b[i]))>0)
            {
              foundlinksa <- foundlinksa + 1
              data_b <- bpp$list_dfs_b[[kcb]][i,]
              weightlink <- get_link_weights(bpp$result_analysis$matrix[as.numeric(data_b$label),
                                                                        as.numeric(data_a$label)])
              bend_line = "no"
              if (((kc == 2) & (kcb == bpp$kcoremax)) | ((kc == bpp$kcoremax) & (kcb == 2)))
                bend_line = "horizontal"
              if ((kc == bpp$kcoremax) & (kcb == bpp$kcoremax))
              {
                link <- data.frame(x1= data_a$x1 + (data_a$x2-data_a$x1)/2,
                                   x2 = data_b$x1 +(data_b$x2-data_b$x1)/2,
                                   y1 = data_a$y1,  y2 = bpp$list_dfs_b[[kcb]]$y1[i] )
                lcolor = "orange"
                bend_line = "no"
              }
              else if (kc == kcb) {
                link <- data.frame(x1=  data_a$x1,
                                   x2 = data_b$x1,
                                   y1 = data_a$y1,
                                   y2 = data_b$y1 )
                bend_line = "no"
                lcolor = "pink"
              }
              else if (kc > kcb) {
                if (kc == bpp$kcoremax)
                  link <- data.frame(x1= (data_a$x2 + data_a$x1)/2,
                                     x2 = data_b$x1,
                                     y1 = data_a$y2,  y2 = data_b$y1 )
                else{
                  link <- data.frame(x1=  data_a$x2 ,
                                     x2 = data_b$x1,
                                     y1 = data_a$y1,  y2 = data_b$y1 )
                  bend_line = "diagonal"
                }
                lcolor = "green"
              }
              else
              {
                if (kcb == bpp$kcoremax){
                  y_2 <- data_b$y2
                  x_2 <- (data_b$x2 + data_b$x1)/2
                }
                else{
                  y_2 <- data_b$y1
                  x_2 <- data_b$x2

                }
                link <- data.frame(x1= data_a$x1,
                                   x2 = x_2,
                                   y1 = data_a$y1,  y2 = y_2)
                lcolor = "blue"
              }

              add_link(xx1=link$x1, xx2 = link$x2,
                             yy1 = link$y1, yy2 = link$y2,
                             slink = bpp$size_link*weightlink, clink =  c(bpp$color_link),
                             alpha_l = bpp$alpha_link , spline = bend_line)
            }
            if (foundlinksa >= numberlinksa )
              break
          }
        }
      }
    }
    calc_vals <- list("p" = p, "svg" = svg)
    return(calc_vals)
}

# Draw tail connected to highest kdegree node
draw_fat_tail<- function(p,svg,fat_tail,nrows,list_dfs,color_guild,pos_tail_x,pos_tail_y,fattailjhoriz,fattailjvert,fgap,
                         inverse="no", is_guild_a =TRUE, bipartite = FALSE, gstyle = "ziggurat")
{

  if (gstyle == "chilopodograph"){
    ppos_tail_x <- pos_tail_x-(1+log10(nrows))*bpp$xstep
    pos_tail_y <-list_dfs[[bpp$kcoremax]][1,]$y1# (list_dfs[[bpp$kcoremax]][1,]$y2+list_dfs[[bpp$kcoremax]][1,]$y1)/2
    ppos_tail_y <- pos_tail_y
  }
  else{
    ppos_tail_x <- pos_tail_x * fattailjhoriz
    pos_tail_y <- (0.25+0.1*sqrt(nrows))*(list_dfs[[bpp$kcoremax]][1,]$y2+list_dfs[[bpp$kcoremax]][1,]$y1)/2
    ppos_tail_y <- pos_tail_y * fattailjvert
  }
  
  if (nrow(fat_tail)>0)
  {
    nodekcoremax <- list_dfs[[bpp$kcoremax]][1,]
    plyy2 <-  (nodekcoremax$y1+nodekcoremax$y2)/2
    v<- draw_tail_bip(ifelse(is_guild_a, "edge-kcore1-a", "edge-kcore1-b"), p,svg,
                  fat_tail,ifelse(bpp$style=="chilopodograph",bpp$xstep,bpp$lado),
                  color_guild,gen_sq_label(fat_tail$orph,is_guild_a = is_guild_a),
                  ppos_tail_x,ppos_tail_y,fgap,
                  lxx2 = list_dfs[[bpp$kcoremax]][1,]$x1,
                  lyy2 = plyy2,
                  sqinverse = inverse, background = "no", psize = bpp$lsize_kcore1,
                  is_guild_a = is_guild_a, wlink = fat_tail$weightlink[1],style=bpp$style)
    p <- v["p"][[1]]
    svg <- v["svg"][[1]]
  }
  calc_vals <- list("p" = p, "svg" = svg, "pos_tail_x" = bpp$pos_tail_x)
  return(calc_vals)
}

# Management of chains of specialists
handle_specialists <- function(p,svg,specialists_a,specialists_b,lado,gap)
{
  ladosq <- 2 * lado * sqrt(bpp$square_nodes_size_scale)
  specialists_a <- data.frame(c())
  specialists_b <- data.frame(c())
  if (exists("df_orph_a", envir = bpp))
    if (nrow(bpp$df_orph_a)>0)
     {
      specialists_a <-  bpp$df_orph_a[bpp$df_orph_a$repeated== "yes",]
      specialists_a <-  specialists_a[rev(order(specialists_a$orph,specialists_a$kcore)),]
      if (nrow(specialists_a)>0)
        specialists_a$drawn <- "no"
      }
  if (exists("df_orph_b", envir = bpp))
    if (nrow(bpp$df_orph_b)>0)
      {
      specialists_b <-  bpp$df_orph_b[bpp$df_orph_b$repeated== "yes",]
      specialists_b <-  specialists_b[rev(order(specialists_b$orph,specialists_b$kcore)),]
      if (nrow(specialists_b)>0)
        specialists_b$drawn <- "no"
      }

  # Create empty df_chains data frame
  bpp$df_chains <- data.frame(x1 = numeric(0), x2 = numeric(0), y1 = numeric(0), y2 = numeric(0),
                          guild = character(0), orph = integer(0), partner = integer(0),
                          kcorepartner = integer(0), xx2 = numeric(0), yy2 = numeric(0), stringsAsFactors = FALSE )

  if  (( ( nrow(specialists_a)+nrow(specialists_b) )>0)) {
    original_specialists_a <- specialists_a
    original_specialists_b <- specialists_b
    while (((nrow(specialists_a)+nrow(specialists_b))>0))
    {
      if (nrow(specialists_a)>0){
        k <- store_root_leaf(specialists_a, bpp$df_chains, bpp$str_guild_a, ladosq, gap, original_specialists_a, original_specialists_b)
        bpp$df_chains <- k["df_chains"][[1]]
        specialists_a <- k["specialists"][[1]]
      }
      if (nrow(specialists_b)>0){
        k <- store_root_leaf(specialists_b, bpp$df_chains, bpp$str_guild_b, ladosq, gap, original_specialists_a, original_specialists_b)
        bpp$df_chains <- k["df_chains"][[1]]
        specialists_b <- k["specialists"][[1]]
      }
      if (nrow(specialists_a)>0){
        k <- store_branch_leaf(specialists_a, specialists_b, bpp$df_chains, bpp$str_guild_a, ladosq, gap, original_specialists_a, original_specialists_b)
        bpp$df_chains <- k["df_chains"][[1]]
        specialists_a <- k["specialists"][[1]]
        specialists_b <- k["specialists_opp"][[1]]
      }
      if (nrow(specialists_b)>0){
        k <- store_branch_leaf(specialists_b, specialists_a, bpp$df_chains, bpp$str_guild_b, ladosq, gap, original_specialists_a, original_specialists_b)
        bpp$df_chains <- k["df_chains"][[1]]
        specialists_b <- k["specialists"][[1]]
        specialists_a <- k["specialists_opp"][[1]]
      }
      # Now they may be some specialists of core 1 linked to core 1 that were not
      # stored in the previous procedure
      specialists_a <- specialists_a[specialists_a$drawn == "no",]
      specialists_b <- specialists_b[specialists_b$drawn == "no",]
    }
    f <- draw_specialist_chains(p, svg, bpp$df_chains, ladosq)
    p <- f["p"][[1]]
    svg <- f["svg"][[1]]
  }
  calc_vals <- list("p" = p, "svg" = svg, "df_chains" = bpp$df_chains)
  return(calc_vals)
}

find_specialists_bip <- function(specialists_a,specialists_b)
{
  specialists_a <- data.frame(c())
  specialists_b <- data.frame(c())
  if (exists("df_orph_a", envir = bpp))
    if (nrow(bpp$df_orph_a)>0)
    {
      specialists_a <-  bpp$df_orph_a[bpp$df_orph_a$repeated== "yes",]
      specialists_a <-  specialists_a[rev(order(specialists_a$orph,specialists_a$kcore)),]
      if (nrow(specialists_a)>0)
        specialists_a$drawn <- "no"
    }
  if (exists("df_orph_b", envir = bpp))
    if (nrow(bpp$df_orph_b)>0)
    {
      specialists_b <-  bpp$df_orph_b[bpp$df_orph_b$repeated== "yes",]
      specialists_b <-  specialists_b[rev(order(specialists_b$orph,specialists_b$kcore)),]
      if (nrow(specialists_b)>0)
        specialists_b$drawn <- "no"
    }
  
  calc_vals <- list("specialists_a" = specialists_a, "specialists_b" = specialists_b)
  return(calc_vals)
}

# Management of chains of specialists
handle_specialists_bip <- function(p,svg,specialists_a,specialists_b,lado,gap)
{
  ladosq <- 2 * lado * sqrt(bpp$square_nodes_size_scale)
  specialists_a <- data.frame(c())
  specialists_b <- data.frame(c())
  if (exists("df_orph_a", envir = bpp))
    if (nrow(bpp$df_orph_a)>0)
    {
      specialists_a <-  bpp$df_orph_a[bpp$df_orph_a$repeated== "yes",]
      specialists_a <-  specialists_a[rev(order(specialists_a$orph,specialists_a$kcore)),]
      if (nrow(specialists_a)>0)
        specialists_a$drawn <- "no"
    }
  if (exists("df_orph_b", envir = bpp))
    if (nrow(bpp$df_orph_b)>0)
    {
      specialists_b <-  bpp$df_orph_b[bpp$df_orph_b$repeated== "yes",]
      specialists_b <-  specialists_b[rev(order(specialists_b$orph,specialists_b$kcore)),]
      if (nrow(specialists_b)>0)
        specialists_b$drawn <- "no"
    }
  
  # Create empty df_chains data frame
  bpp$df_chains <- data.frame(x1 = numeric(0), x2 = numeric(0), y1 = numeric(0), y2 = numeric(0),
                              guild = character(0), orph = integer(0), partner = integer(0),
                              kcorepartner = integer(0), xx2 = numeric(0), yy2 = numeric(0), stringsAsFactors = FALSE )
  
  if  (( ( nrow(specialists_a)+nrow(specialists_b) )>0)) {
    original_specialists_a <- specialists_a
    original_specialists_b <- specialists_b
    while (((nrow(specialists_a)+nrow(specialists_b))>0))
    {
      if (nrow(specialists_a)>0){
        k <- store_root_leaf(specialists_a, bpp$df_chains, bpp$str_guild_a, ladosq, gap, original_specialists_a, original_specialists_b)
        bpp$df_chains <- k["df_chains"][[1]]
        specialists_a <- k["specialists"][[1]]
      }
      if (nrow(specialists_b)>0){
        k <- store_root_leaf(specialists_b, bpp$df_chains, bpp$str_guild_b, ladosq, gap, original_specialists_a, original_specialists_b)
        bpp$df_chains <- k["df_chains"][[1]]
        specialists_b <- k["specialists"][[1]]
      }
      if (nrow(specialists_a)>0){
        k <- store_branch_leaf(specialists_a, specialists_b, bpp$df_chains, bpp$str_guild_a, ladosq, gap, original_specialists_a, original_specialists_b)
        bpp$df_chains <- k["df_chains"][[1]]
        specialists_a <- k["specialists"][[1]]
        specialists_b <- k["specialists_opp"][[1]]
      }
      if (nrow(specialists_b)>0){
        k <- store_branch_leaf(specialists_b, specialists_a, bpp$df_chains, bpp$str_guild_b, ladosq, gap, original_specialists_a, original_specialists_b)
        bpp$df_chains <- k["df_chains"][[1]]
        specialists_b <- k["specialists"][[1]]
        specialists_a <- k["specialists_opp"][[1]]
      }
      # Now they may be some specialists of core 1 linked to core 1 that were not
      # stored in the previous procedure
      specialists_a <- specialists_a[specialists_a$drawn == "no",]
      specialists_b <- specialists_b[specialists_b$drawn == "no",]
    }
    f <- draw_specialist_chains(p, svg, bpp$df_chains, ladosq)
    p <- f["p"][[1]]
    svg <- f["svg"][[1]]
  }
  calc_vals <- list("p" = p, "svg" = svg, "df_chains" = bpp$df_chains)
  return(calc_vals)
}
# Final annotations
write_annotations <- function(p, svg)
{
  title_text = ""
  if (bpp$show_title)
    title_text <- paste0("Network: ",bpp$network_name)
  # Legend size conversion factor
  lzcf <- 3
  legends_text <- paste0(
    "<span style = 'color:",bpp$color_guild_a[1],"; font-size:",lzcf*bpp$lsize_legend,"pt'>",bpp$name_guild_a,
    "</span> <span style = 'color:",bpp$color_guild_b[1],"; font-size:",lzcf*bpp$lsize_legend,"pt'>",bpp$name_guild_b,"</span>")
  #p <- p+ ggtitle(title_text,subtitle=paste("<span align='right' size>",legends_text,"</span>"))
  p <- p + coord_fixed(ratio=bpp$aspect_ratio) +theme_bw() + theme(panel.grid.minor.x = element_blank(),
                                                               panel.grid.minor.y = element_blank(),
                                                               panel.grid.major.x = element_blank(),
                                                               panel.grid.major.y = element_blank(),
                                                               axis.text.x = element_blank(),
                                                               axis.text.y = element_blank(),
                                                               axis.ticks.x=element_blank(),
                                                               axis.ticks.y=element_blank(),
                                                               axis.title.x = element_blank(),
                                                               axis.title.y = element_blank(),                                                               plot.background = element_rect(fill = bpp$backg_color),
                                                               panel.background = element_rect(fill = bpp$backg_color),
                                                               plot.title = element_text(size = lzcf*(bpp$lsize_legend+1),
                                                                                         hjust = 0.5,
                                                                                         face="plain"),
                                                               
                                                               plot.subtitle = ggtext::element_markdown()
                                                                )
  if (bpp$hide_plot_border)
    p <- p + theme(panel.border=element_blank())
  #landmark_top <- 1.2*max(bpp$last_ytail_b[!is.na(bpp$last_ytail_b)],bpp$ymax)*bpp$rescale_plot_area[2]
  landmark_top <- 4*bpp$xstep+bpp$ymax*bpp$rescale_plot_area[2]
  
  # This dot marks the right edge of the plot
  mlabel <- "."
  landmark_right <- (bpp$tot_width+1.5*bpp$hop_x)*bpp$rescale_plot_area[1]
  f <- draw_square("annotation",p,svg,landmark_right,0,1,"transparent",0.5,"transparent",0,0,0,slabel="")
  p <- f["p"][[1]]
  svg <- f["svg"][[1]]
  p <- p +annotate(geom="text", x= landmark_right, y=0, label=mlabel,
                   colour = "red", size=1, hjust = 0, vjust = 0, angle = 0)
  svg$text("annotation", data=data.frame(x=landmark_right*bpp$move_all_SVG_right, y=0), 
           mapping=aes(x=x, y=y), color="red", label=mlabel, size=1, angle=0)
  landmark_left <- bpp$pos_tail_x*bpp$rescale_plot_area[1]
  mlabel <- "."
  p <- p +annotate(geom="text", x=landmark_left, y=0, label=mlabel,
                   colour = "red", size=2, hjust = 0, vjust = 0, angle = 0)
  svg$text("annotation", data=data.frame(x=landmark_left, y=0), mapping=aes(x=x, y=y), color="red", label=mlabel, size=1, angle=0)
  x_span <- landmark_right - landmark_left

  if (!(bpp$flip_results)){
    x_legend <- 0.8*landmark_right
    y_legend <- 0.8*landmark_top
  } else {
    x_legend <- 0.8*landmark_top
    y_legend <- 0.8*landmark_right
  }
  landmark_bottom <- min(bpp$last_ytail_a[!is.na(bpp$last_ytail_b)],1.2*bpp$ymin)*bpp$rescale_plot_area[2]
  
  bpp$landmark_left <<- landmark_left
  bpp$landmark_right <<- landmark_right
  bpp$landmark_top <<- landmark_top
  bpp$landmark_bottom <- landmark_bottom
 
  calc_vals <- list("p" = p, "svg" = svg)
  return(calc_vals)
}

# Handle specialist chain species
handle_orphans <- function(vg)
{
  bpp$df_orph_a <- data.frame(c())
  bpp$df_orph_b <- data.frame(c())
  if (length(grep(bpp$str_guild_b,bpp$mtxlinks[1,1]))>0)
    names(bpp$mtxlinks) <- rev(names(bpp$mtxlinks))
  bpp$orphans_a <- bpp$df_cores$species_guild_a[[1]]
  bpp$orphans_b <- bpp$df_cores$species_guild_b[[1]]
  if (!is.null(bpp$orphans_a))
    if (!is.na(bpp$orphans_a[1]))
      bpp$df_orph_a <- find_orphans(bpp$mtxlinks,bpp$orphans_a,bpp$g,guild_a="yes")
  if (!is.null(bpp$orphans_b))
    if (!is.na(bpp$orphans_b[1]))
      bpp$df_orph_b <- find_orphans(bpp$mtxlinks,bpp$orphans_b,bpp$g,guild_a="no")
  calc_vals <- list("mtxlinks" = bpp$mtxlinks, "orphans_a" = bpp$orphans_a,
                    "orphans_b" = bpp$orphans_b, "df_orph_a" = bpp$df_orph_a, "df_orph_b" = bpp$df_orph_b )
  return(calc_vals)
}

# Draw specialist chains connected to inner ziggurats
draw_bipartite_leafs <- function(p, svg)
{
    bpp$kcore2tail_vertical_separation <- bpp$kcore2tail_vertical_separation + 2
    if (exists("df_orph_a", envir = bpp)){
      w <- draw_innercores_tails_bip(p,svg,2,bpp$list_dfs_b,bpp$df_orph_a,bpp$color_guild_a, inverse="yes")
      p <- w["p"][[1]]
      svg <- w["svg"][[1]]
      bpp$last_xtail_b[2] <- w["lastx"][[1]]
      bpp$last_ytail_b[2] <-w["lasty"][[1]]
    }

    if (exists("df_orph_b", envir = bpp)){
      w <- draw_innercores_tails_bip(p,svg,2,bpp$list_dfs_a,bpp$df_orph_b,bpp$color_guild_b, inverse="no", is_guild_a = FALSE)
      p <- w["p"][[1]]
      svg <- w["svg"][[1]]
      bpp$last_xtail_a[2] <- w["lastx"][[1]]
      bpp$last_ytail_a[2] <- w["lasty"][[1]]
    }
  #}

  calc_vals <- list("p" = p, "svg" = svg)
  return(calc_vals)
}

draw_bipartite_leafs <- function(p, svg)
{
    bpp$kcore2tail_vertical_separation <- bpp$kcore2tail_vertical_separation + 2
    if (exists("df_orph_a", envir = bpp)){
      w <- draw_innercores_tails_bip(p,svg,2,bpp$list_dfs_b,bpp$df_orph_a,bpp$color_guild_a, inverse="yes")
      p <- w["p"][[1]]
      svg <- w["svg"][[1]]
      bpp$last_xtail_b[2] <- w["lastx"][[1]]
      bpp$last_ytail_b[2] <-w["lasty"][[1]]
    }
    
    if (exists("df_orph_b", envir = bpp)){
      w <- draw_innercores_tails_bip(p,svg,2,bpp$list_dfs_a,bpp$df_orph_b,bpp$color_guild_b, inverse="no", is_guild_a = FALSE)
      p <- w["p"][[1]]
      svg <- w["svg"][[1]]
      bpp$last_xtail_a[2] <- w["lastx"][[1]]
      bpp$last_ytail_a[2] <- w["lasty"][[1]]
    }
  #}
  
  calc_vals <- list("p" = p, "svg" = svg)
  return(calc_vals)
}

# Manage fat tails
handle_fat_tails_bip <- function(p, svg, style = "ziggurat")
{
  fat_tail_x <- min(bpp$last_xtail_a[[bpp$kcoremax]],
                    bpp$last_xtail_b[[bpp$kcoremax]],
                    bpp$list_dfs_a[[bpp$kcoremax]][1,]$x1,
                    bpp$list_dfs_b[[bpp$kcoremax]][1,]$y2)
  if (bpp$orderkcoremaxby == "kdegree")
    max_b_k <- bpp$list_dfs_b[[bpp$kcoremax]][which(bpp$list_dfs_b[[bpp$kcoremax]]$kdegree == max(bpp$list_dfs_b[[bpp$kcoremax]]$kdegree)),]$label
  if (bpp$orderkcoremaxby == "kradius")
    max_b_k <- bpp$list_dfs_b[[bpp$kcoremax]][which(bpp$list_dfs_b[[bpp$kcoremax]]$kradius == min(bpp$list_dfs_b[[bpp$kcoremax]]$kradius)),]$label
  
  if (exists("df_orph_a", envir = bpp)){
    fat_tail_a <- bpp$df_orph_a[(bpp$df_orph_a$partner == max(max_b_k)) 
                                & (bpp$df_orph_a$repeated == "no"),]
    if (nrow(fat_tail_a)>1)
      bpp$df_orph_a <- bpp$df_orph_a[!(bpp$df_orph_a$orph %in% fat_tail_a$orph),]
    tailweight <- 0
    if (nrow(fat_tail_a)>0) {
    for (h in 1:nrow(fat_tail_a))
      tailweight <- tailweight + bpp$result_analysis$matrix[as.numeric(fat_tail_a$partner[h]),
                                                                  as.numeric(fat_tail_a$orph[h])]
      fat_tail_a$weightlink <- get_link_weights(tailweight)
    }
  }
  if (!exists("fat_tail_a"))
    fat_tail_a <- data.frame(c())
  if (bpp$orderkcoremaxby == "kdegree")
    max_a_k <- bpp$list_dfs_a[[bpp$kcoremax]][which(bpp$list_dfs_a[[bpp$kcoremax]]$kdegree == max(bpp$list_dfs_a[[bpp$kcoremax]]$kdegree)),]$label
  if (bpp$orderkcoremaxby == "kradius")
    max_a_k <- bpp$list_dfs_a[[bpp$kcoremax]][which(bpp$list_dfs_a[[bpp$kcoremax]]$kradius == min(bpp$list_dfs_a[[bpp$kcoremax]]$kradius)),]$label
  
  if (exists("df_orph_b", envir = bpp)){
    fat_tail_b <- bpp$df_orph_b[(bpp$df_orph_b$partner == max(max_a_k)) & (bpp$df_orph_b$repeated == "no"),]
    if (nrow(fat_tail_b)>1)
      bpp$df_orph_b <- bpp$df_orph_b[!(bpp$df_orph_b$orph %in% fat_tail_b$orph),]
    
    tailweight <- 0
    if (nrow(fat_tail_b)>0) {
      for (h in 1:nrow(fat_tail_b))
        tailweight <- tailweight+bpp$result_analysis$matrix[as.numeric(fat_tail_b$orph[h]),
                                                    as.numeric(fat_tail_b$partner[h])]
      fat_tail_b$weightlink <- get_link_weights(tailweight)
    }
  }
  if (!exists("fat_tail_b"))
    fat_tail_b <- data.frame(c())
  nrows_fat <- nrow(fat_tail_b)+nrow(fat_tail_a)
  if (style=="chilopodograph")
    fgap <- 0.7*bpp$hop_x
  else
    fgap <- 0.7*bpp$hop_x + (1+sum(nrows_fat>40))*bpp$lado
  bpp$pos_tail_x <- min(bpp$last_xtail_a[[bpp$kcoremax]],
                        bpp$last_xtail_b[[bpp$kcoremax]],
                        bpp$list_dfs_b[[bpp$kcoremax]][1,]$x1-fgap,
                        bpp$list_dfs_a[[bpp$kcoremax]][1,]$x1-fgap)
  if (exists("fat_tail_a")) {
    f <- draw_fat_tail(p,svg,fat_tail_a,nrows_fat,bpp$list_dfs_b,bpp$color_guild_a[2],
                       bpp$pos_tail_x,pos_tail_y,bpp$fattailjumphoriz[1],
                       bpp$fattailjumpvert[1],fgap,inverse="yes", bipartite = TRUE, 
                       gstyle = style)
    p <- f["p"][[1]]
    svg <- f["svg"][[1]]
  }

  if (exists("fat_tail_b")) {
    f <- draw_fat_tail(p,svg,fat_tail_b,nrows_fat,bpp$list_dfs_a,bpp$color_guild_b[2],bpp$pos_tail_x,
                       pos_tail_y,bpp$fattailjumphoriz[2],bpp$fattailjumpvert[2],fgap,
                       inverse="no", is_guild_a = FALSE, bipartite = TRUE, gstyle = style)
    p <- f["p"][[1]]
    svg <- f["svg"][[1]]
  }
  calc_vals <- list("p" = p, "svg" = svg, "pos_tail_x" = bpp$pos_tail_x)
  return(calc_vals)
}

draw_maxcore_bip <- function(svg)
{
  kcoremax_label_display <- function (idPrefix, gp,svg,kcoremaxlabel_angle,pdata,plabel,plabelsize,phjust=0, is_guild_a = TRUE) 
  {

    labelcolor <- ifelse(length(bpp$labels_color)>0,bpp$labels_color[2-as.numeric(is_guild_a)], pdata$col_row)
    if (kcoremaxlabel_angle == 0) 
    {
        gp <- gp +  geom_text(data=pdata, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2), label=plabel,
                              color = labelcolor, size=plabelsize, angle = kcoremaxlabel_angle)
        svg$text(idPrefix=idPrefix, data=pdata, mapping=aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2), label=plabel, color=labelcolor, size=fsvgtext*plabelsize, angle=kcoremaxlabel_angle)
    } 
    else {
        gp <- gp + geom_text(data=pdata, aes(x=x1, y=y1+(y2-y1)/20), label=plabel,
                             color = labelcolor, size=plabelsize, angle = kcoremaxlabel_angle,
                             vjust = 1, hjust = phjust)
        svg$text(idPrefix=idPrefix, data=pdata, mapping=aes(x=x1, y=y1+(y2-y1)/20), label=plabel, color=labelcolor, size=fsvgtext*plabelsize, angle=kcoremaxlabel_angle)
            }
    calc_vals <- list("p" = gp, "svg" = svg)
    return(calc_vals)
  }
  
  adjust_lengths <- function(spA,spB,fill="right"){
    diff_lengths <- length(spA) - length(spB)
    if (fill=="right"){
      specA <- c(rep("EMPTY",abs(diff_lengths)*(diff_lengths<0)),spA)
      specB <- c(rep("EMPTY",diff_lengths*(diff_lengths>0)),spB)
    }
    else{
      specA <- c(spA,rep("EMPTY",abs(diff_lengths)*(diff_lengths<0)))
      specB <- c(spB,rep("EMPTY",diff_lengths*(diff_lengths>0)))
    }
    calc_vals <- list("specA" = specA, "specB" = specB)
    return(calc_vals)
  }

  bpp$last_ytail_a[bpp$kcoremax]<- bpp$toopy
  bpp$last_xtail_a[bpp$kcoremax]<- bpp$topxa
  #Species outside the giant component
  outsiders_A <- gsub(bpp$str_guild_a,"",bpp$outsiders_a)
  outsiders_B <- gsub(bpp$str_guild_b,"",bpp$outsiders_b)
  if (bpp$style == "legacy"){
    species_A <- unlist(bpp$df_cores$species_guild_a)
    species_B <- unlist(bpp$df_cores$species_guild_b)
    species_A <- c(species_A,outsiders_A,"EMPTY")
    species_B <- c(species_B,outsiders_B,"EMPTY")
  }
  else if ((bpp$style == "kcoreorder") ||(bpp$style == "chilopodograph")) {
    species_A <- c()
    species_B <- c()
    if (bpp$style == "kcoreorder"){
      # Kcore-1 on the left side of the plot
      # shell number is passed together with node label
      spA1 <- paste0(bpp$df_cores$species_guild_a[[1]],"shell1")
      spB1 <- paste0(bpp$df_cores$species_guild_b[[1]],"shell1")
      sadj <- adjust_lengths(spA1,spB1)
      species_A <- sadj$specA
      species_B <- sadj$specB
    }
    if (bpp$style == "chilopodograph"){
      spA1_spec <- c()
      spB1_spec <- c()
      # specialist chains
      sbip <- find_specialists_bip(specialists_a,specialists_b)
      if (nrow(sbip$specialists_a)>0)
        spA1_spec <- paste0(unique(sbip$specialists_a$orph),"shell1")
      if (nrow(sbip$specialists_b)>0)
        spB1_spec <- paste0(unique(sbip$specialists_b$orph),"shell1")
      # shell number is passed together with node label
      sadj <- adjust_lengths(spA1_spec,spB1_spec,fill="right")
      spA1_spec <- sadj$specA
      spB1_spec <- sadj$specB
    }
    for (k in bpp$kcoremax:2) {
      sadj <- adjust_lengths(paste0(rev(unlist(bpp$df_cores[k,]$species_guild_a)),"shell",k),
                             paste0(rev(unlist(bpp$df_cores[k,]$species_guild_b)),"shell",k),
                             fill = "left")  
      species_A <- c(species_A, "EMPTY", sadj$specA) 
      species_B <- c(species_B, "EMPTY", sadj$specB) 
    }
    # Specialist chains 
    if (bpp$style == "chilopodograph"){
      species_A <- c(species_A, "EMPTY", spA1_spec) 
      species_B <- c(species_B, "EMPTY", spB1_spec)
    }  
    if (length(outsiders_A>0)){
      sadj <- adjust_lengths(outsiders_A,outsiders_B)
      species_A <- c(species_A, "EMPTY", paste0(sadj$specA,"shell",0))
      species_B <- c(species_B, "EMPTY", paste0(sadj$specB,"shell",0))
      species_A[grepl("EMPTY",species_A)] <- "EMPTY"
      species_B[grepl("EMPTY",species_B)] <- "EMPTY"
    }
  }
 nnodes <- max(length(species_A), length(species_B))
 bpp$list_dfs_a[[bpp$kcoremax]]<- draw_parallel_guilds(bpp$basex,bpp$topxa,bpp$basey*bpp$guild_gap_increase,bpp$toopy,
                                                         length(species_A),nnodes,bpp$color_guild_a,
                                                         species_A,
                                                         bpp$rg, bpp$str_guild_a, 
                                                         orderby = "kdegree",
                                                         style=bpp$style,guild="A")
  nsp <- name_species_preprocess(bpp$kcoremax,bpp$list_dfs_a[[bpp$kcoremax]],bpp$kcore_species_name_display,
                                 bpp$kcore_species_name_break)
  kcoremaxlabel_angle <- nsp$kcoremaxlabel_angle
  labelszig <- nsp$labelszig
  p <- ggplot() +
    scale_x_continuous(name="x") +
    scale_y_continuous(name="y") +
    geom_rect(data=bpp$list_dfs_a[[bpp$kcoremax]], 
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill = bpp$list_dfs_a[[bpp$kcoremax]]$col_row,  
              color="transparent",alpha=bpp$alpha_level)
  
  svg$rect(paste0("kcore", bpp$kcoremax, "-a"), data=bpp$list_dfs_a[[bpp$kcoremax]],
           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
           fill=bpp$list_dfs_a[[bpp$kcoremax]]$col_row, alpha=bpp$alpha_level,
           color="transparent", size=0.5)
  f <- kcoremax_label_display(paste0("kcore", bpp$kcoremax, "-a"),p,svg,
                              kcoremaxlabel_angle,bpp$list_dfs_a[[bpp$kcoremax]],
                              labelszig,bpp$lsize_kcoremax)
  p <- f["p"][[1]]
  svg <- f["svg"][[1]]
  num_b_coremax <- bpp$df_cores[bpp$kcoremax,]$num_species_guild_b
  bpp$basey <- - bpp$basey
  bpp$topxb <- bpp$topxa
  bpp$toopy <- - bpp$toopy
  bpp$list_dfs_b[[bpp$kcoremax]] <- draw_parallel_guilds(bpp$basex,bpp$topxb,bpp$basey*bpp$guild_gap_increase,bpp$toopy,
                                                          length(species_B),nnodes,
                                                          bpp$color_guild_b,
                                                          species_B,bpp$rg,
                                                          bpp$str_guild_b,  orderby = "kdegree",
                                                          style=bpp$style,guild="B")
  bpp$last_ytail_b[bpp$kcoremax]<- bpp$toopy
  bpp$last_xtail_b[bpp$kcoremax]<- bpp$topxb
  nsp <- name_species_preprocess(bpp$kcoremax,bpp$list_dfs_b[[bpp$kcoremax]],bpp$kcore_species_name_display,
                                 bpp$kcore_species_name_break)
  kcoremaxlabel_angle <- nsp$kcoremaxlabel_angle
  labelszig <- nsp$labelszig
  p <- p + geom_rect(data=bpp$list_dfs_b[[bpp$kcoremax]] , mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
                     fill = bpp$list_dfs_b[[bpp$kcoremax]]$col_row, color="transparent", alpha=bpp$alpha_level)

  svg$rect(paste0("kcore", bpp$kcoremax, "-b"), bpp$list_dfs_b[[bpp$kcoremax]] , mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
           fill=bpp$list_dfs_b[[bpp$kcoremax]]$col_row, alpha=bpp$alpha_level,
           color="transparent", size=0.5)
  f <- kcoremax_label_display(paste0("kcore", bpp$kcoremax, "-b"),p,svg,kcoremaxlabel_angle,
                              bpp$list_dfs_b[[bpp$kcoremax]],labelszig,
                              bpp$lsize_kcoremax, phjust = 1, is_guild_a = FALSE)
  p <- f["p"][[1]]
  svg <- f["svg"][[1]]

  calc_vals <- list("p" = p, "svg" = svg, "basey" = bpp$basey, "topy" = bpp$toopy, "topxa" = bpp$topxa, "topxb" = bpp$topxb,
                    "list_dfs_a" = bpp$list_dfs_a, "list_dfs_b" = bpp$list_dfs_b,
                    "last_xtail_a" = bpp$last_xtail_a, "last_ytail_a" = bpp$last_ytail_a,
                    "last_xtail_b" = bpp$last_xtail_b, "last_ytail_b" = bpp$last_ytail_b)
  return(calc_vals)
}

draw_maxcore_tails_bip <- function(p, svg)
{
  long_kcoremax_tail <- FALSE
  #leftjump <- (1.2-0.02*nrow(bpp$list_dfs_a[[bpp$kcoremax]])) * bpp$hop_x
  point_x <- bpp$list_dfs_a[[bpp$kcoremax]][nrow(bpp$list_dfs_a[[bpp$kcoremax]]),]$x2 #- leftjump
  #point_y <- bpp$posic_zig * 0.8
  point_y <- bpp$xstep
  if (exists("df_orph_a", envir = bpp))
    #long_tail_a <- bpp$df_orph_a[(bpp$df_orph_a$kcore == bpp$kcoremax) & (bpp$df_orph_a$repeated == "no"),]
    long_tail_a <- bpp$df_orph_a[(bpp$df_orph_a$repeated == "no"),]
  if ((exists("long_tail_a")) & (bpp$kcoremax > 2))
  {
    if (length(long_tail_a)>5)
      long_kcoremax_tail <- TRUE
    v<-  draw_edge_tails_bip(p,svg,point_x,point_y*bpp$aspect_ratio,bpp$kcoremax,
                         long_tail_a,bpp$list_dfs_b,bpp$color_guild_a, inverse = "yes",
                         vertical = "no", orientation = "South", revanddrop = "yes",
                         pbackground = "no", tspline = "arc", joinchars=bpp$joinstr)
    p <- v["p"][[1]]
    svg <- v["svg"][[1]]
    bpp$last_xtail_b[bpp$kcoremax] <- v["lastx"][[1]]
    if (length(v["lasty"][[1]])>0)
      bpp$last_ytail_b[bpp$kcoremax] <- v["lasty"][[1]]
    else                                                   # only for degenerate networks with all nodes in kcoremax
      bpp$last_ytail_b[bpp$kcoremax] <- bpp$toopy
  }
  leftjump <- (1.2-0.02*nrow(bpp$list_dfs_b[[bpp$kcoremax]]))* bpp$hop_x
  point_x <- bpp$list_dfs_b[[bpp$kcoremax]][nrow(bpp$list_dfs_b[[bpp$kcoremax]]),]$x2
  point_y <- -bpp$xstep
  if (exists("df_orph_b", envir = bpp))
    long_tail_b <- bpp$df_orph_b[(bpp$df_orph_b$repeated == "no"),]
  if ( (exists("long_tail_b")) & (bpp$kcoremax > 2) ){
    if (nrow(long_tail_b)>5)
      long_kcoremax_tail <- TRUE
    tailweight <- 0
    if (nrow(long_tail_b)>0){
      for (h in 1:nrow(long_tail_b))
        tailweight <- tailweight + bpp$result_analysis$matrix[as.numeric(long_tail_b$orph[h]),
                                                                    as.numeric(long_tail_b$partner[h])]
      long_tail_b$weightlink <- get_link_weights(tailweight)
    }

    v <-  draw_edge_tails_bip(p,svg,point_x,point_y*bpp$aspect_ratio,
                              bpp$kcoremax,long_tail_b,bpp$list_dfs_a,bpp$color_guild_b,
                         inverse = "no",
                         vertical = "no", orientation = "North", revanddrop = "yes",
                         pbackground = "no", tspline = "arc", joinchars=bpp$joinstr, is_guild_a = FALSE,
                         wlink = long_tail_b$weightlink[1])
    p <- v["p"][[1]]
    svg <- v["svg"][[1]]
    bpp$last_xtail_a[bpp$kcoremax] <- v["lastx"][[1]]
    if (length(v["lasty"][[1]])>0)
      bpp$last_ytail_b[bpp$kcoremax] <- v["lasty"][[1]]
    else                                                   # only for degenerate networks with all nodes in kcoremax
      bpp$last_ytail_b[bpp$kcoremax] <- bpp$toopy
  }
  calc_vals <- list("p" = p, "svg" = svg,
                    "last_xtail_a" = bpp$last_xtail_a, "last_ytail_a" = bpp$last_ytail_a,
                    "last_xtail_b" = bpp$last_xtail_b, "last_ytail_b" = bpp$last_ytail_b)
  return(calc_vals)
}

display_plot <- function(p, printfile, flip, plwidth=14, ppi = 300, landscape = bpp$label_strguild, fname_append = "")
{
  if (flip)
    p <- p + coord_flip()
  if (printfile){
    if (fname_append != "")
      ftname_append <- paste0("_",fname_append)
    else
      ftname_append <- fname_append
    dir.create(bpp$plotsdir, showWarnings = FALSE)
    if (landscape)
      png(paste0(bpp$plotsdir,"/",bpp$network_name,"_ziggurat",ftname_append,".png"), width=(plwidth*ppi), height=(9/16)*plwidth*ppi, res=ppi)
    else
      png(paste0(bpp$plotsdir,"/",bpp$network_name,"_ziggurat",ftname_append,".png"), width=(plheight*ppi), height=(9/16)*plwidth*ppi, res=ppi)
  }
  print(p)
  if (printfile)
    dev.off()
}

strip_isolated_nodes <- function()
{
  lgrados <- igraph::degree(bpp$result_analysis$graph)
  if (sum(lgrados == 0) > 0)
    for (k in 1:length(lgrados))
    {
      if (lgrados[k] == 0){
        bpp$result_analysis$graph <- delete_vertices(bpp$result_analysis$graph,names(lgrados[k]))
        if ( length(grep(bpp$str_guild_b,names(lgrados[k]) )) >0 )
          bpp$result_analysis$num_guild_b <<- bpp$result_analysis$num_guild_b -1
        else
          bpp$result_analysis$num_guild_a <<- bpp$result_analysis$num_guild_a -1
      }
    }
}


read_and_analyze <- function(directorystr,network_file,label_strguilda,label_strguildb)
{

  str_guild_a <- "pl"
  str_guild_b <- "pol"
  name_guild_a <- "Pollinator"
  name_guild_b <- "Plant"
  network_name <- strsplit(network_file,".csv")[[1]][1]
  slabels <- c("Plant", "Pollinator")
  if (grepl("_SD_",network_name)){
    str_guild_b <- "disp"
    name_guild_b <- "Dispersers"
  }

  if (nchar(label_strguilda)>0){
    slabels <- c(label_strguilda, label_strguildb)
    name_guild_a <- label_strguilda
    name_guild_b <- label_strguildb
  }

  result_analysis <- analyze_network(network_file, directory = directorystr, guild_a = str_guild_a,
                                     guild_b = str_guild_b, only_NODF = TRUE)


  calc_vals <- list("result_analysis" = result_analysis, "str_guild_a" = str_guild_a, "str_guild_b" = str_guild_b,
                    "name_guild_a" = name_guild_a, "name_guild_b" = name_guild_b,
                    "network_name" = network_name)
  return(calc_vals)
}

def_configuration <- function(paintlinks, print_to_file, plotsdir, orderkcoremaxby, style, 
                              guild_gap_increase, flip_results, aspect_ratio,
                              alpha_level, color_guild_a, color_guild_b,
                              color_link, alpha_link, size_link,
                              displace_y_b, displace_y_a, lsize_kcoremax, lsize_zig, lsize_kcore1,
                              lsize_legend, lsize_core_box, labels_color,
                              height_box_y_expand, kcore2tail_vertical_separation,  kcore1tail_disttocore,
                              innertail_vertical_separation ,
                              factor_hop_x, fattailjumphoriz, fattailjumpvert,
                              coremax_triangle_height_factor, coremax_triangle_width_factor,
                              paint_outsiders, displace_outside_component,
                              outsiders_separation_expand, outsiders_legend_expand, specialistskcore2_horizontal_dist_rootleaf_expand,
                              specialistskcore2_vertical_dist_rootleaf_expand , specialists_boxes_separation_count,
                              root_specialist_expand,hide_plot_border,rescale_plot_area,kcore1specialists_leafs_vertical_separation,
                              corebox_border_size, kcore_species_name_display,kcore_species_name_break,
                              shorten_species_name,exclude_species_number,
                              label_strguilda, label_strguildb, landscape_plot, backg_color, show_title,
                              use_spline, spline_points, file_name_append, svg_scale_factor, weighted_links, square_nodes_size_scale,
                              move_all_SVG_up, move_all_SVG_right, progress
                              )
{
  # ENVIRONMENT CONFIGURATION PARAMETERS
  bpp$paintlinks <- paintlinks
  bpp$print_to_file <- print_to_file
  bpp$plotsdir <- plotsdir
  bpp$orderkcoremaxby <- orderkcoremaxby
  bpp$style <- style
  bpp$guild_gap_increase <- guild_gap_increase
  bpp$flip_results <- flip_results
  bpp$alpha_level <- alpha_level
  bpp$color_guild_a <- color_guild_a
  bpp$color_guild_b <- color_guild_b
  bpp$color_link <- color_link
  bpp$alpha_link <- alpha_link
  bpp$size_link <- size_link
  bpp$displace_y_b <- displace_y_b
  bpp$displace_y_a <- -displace_y_a
  bpp$aspect_ratio <- aspect_ratio
  bpp$labels_size <- 3.5
  bpp$lsize_kcoremax <- lsize_kcoremax
  bpp$lsize_zig <- lsize_zig
  bpp$lsize_kcore1 <- lsize_kcore1
  bpp$lsize_legend <- lsize_legend
  bpp$lsize_core_box <- lsize_core_box
  bpp$labels_color <- labels_color
  bpp$height_box_y_expand <- height_box_y_expand
  bpp$kcore2tail_vertical_separation <- kcore2tail_vertical_separation                 # Vertical separation of orphan boxes linked to core 2 in number of heights_y
  bpp$kcore1tail_disttocore <- kcore1tail_disttocore                            # Horizontal & Vertical distances of edge/specialist tails linked to core 1 North & South
  bpp$innertail_vertical_separation <- innertail_vertical_separation                  # Vertical separation of orphan boxes linked to inner cores in number of heights_y
  #bpp$horiz_kcoremax_tails_expand <- horiz_kcoremax_tails_expand                  # horizontal separation of edge tails connected to kcoremax.
  bpp$factor_hop_x <- factor_hop_x
  #bpp$displace_legend <- displace_legend   DEPRECATED
  bpp$fattailjumphoriz <- fattailjumphoriz
  bpp$fattailjumpvert <- fattailjumpvert
  bpp$coremax_triangle_height_factor <- 3
  bpp$coremax_triangle_width_factor <- 3
  bpp$paint_outsiders <- paint_outsiders
  bpp$displace_outside_component <- displace_outside_component
  bpp$outsiders_separation_expand <- outsiders_separation_expand
  bpp$outsiders_legend_expand <- outsiders_legend_expand
  bpp$specialistskcore2_horizontal_dist_rootleaf_expand <- specialistskcore2_horizontal_dist_rootleaf_expand        # Controls the distance of specialist root leaves to partner in core 2
  bpp$specialistskcore2_vertical_dist_rootleaf_expand <- specialistskcore2_vertical_dist_rootleaf_expand
  bpp$specialists_boxes_separation_count <- specialists_boxes_separation_count                  # Separation of leaves of a specialist tail
  bpp$root_specialist_expand <- root_specialist_expand
  bpp$hide_plot_border <- hide_plot_border
  bpp$rescale_plot_area <- rescale_plot_area
  bpp$kcore1specialists_leafs_vertical_separation <- kcore1specialists_leafs_vertical_separation
  bpp$corebox_border_size <- corebox_border_size
  bpp$kcore_species_name_display <- kcore_species_name_display
  bpp$kcore_species_name_break <- kcore_species_name_break
  bpp$shorten_species_name <- shorten_species_name
  bpp$exclude_species_number <- exclude_species_number
  bpp$landscape_plot <- landscape_plot
  bpp$backg_color <- backg_color
  bpp$show_title <- show_title
  bpp$use_spline <- use_spline
  bpp$spline_points <- spline_points
  bpp$file_name_append <- file_name_append
  bpp$svg_scale_factor <- svg_scale_factor
  bpp$weighted_links <- weighted_links
  bpp$square_nodes_size_scale <- square_nodes_size_scale
  bpp$move_all_SVG_up <- move_all_SVG_up
  bpp$move_all_SVG_right <- move_all_SVG_right
  bpp$progress <- progress
}

init_working_values <- function()
{
  bpp$joinstr <- " "
  bpp$max_position_y_text_core <- 0
  bpp$rg <- V(bpp$result_analysis$graph)
  bpp$g <- bpp$rg[bpp$rg$kradius != Inf]
  bpp$outsider <- bpp$rg[bpp$rg$kradius == Inf]
  bpp$outsiders_a <- sort(bpp$outsider$name[grep(bpp$str_guild_a,bpp$outsider$name)])
  bpp$outsiders_b <- sort(bpp$outsider$name[grep(bpp$str_guild_b,bpp$outsider$name)])
  bpp$ind_cores <- rev(sort(unique(bpp$g$kcorenum)))
  bpp$kcoremax <- max(bpp$ind_cores)
  palcores <- colorRampPalette(c("salmon3","salmon4"))
  bpp$corecols <- palcores(bpp$kcoremax)
  bpp$last_xtail_a <- rep(NA,bpp$kcoremax)
  bpp$last_ytail_a <- rep(NA,bpp$kcoremax)
  bpp$last_xtail_b <- rep(NA,bpp$kcoremax)
  bpp$last_ytail_b <- rep(NA,bpp$kcoremax)
  species_guild_a <- rep(NA,bpp$kcoremax)
  species_guild_b <- rep(NA,bpp$kcoremax)
  num_species_guild_a <- rep(NA,bpp$kcoremax)
  num_species_guild_b <- rep(NA,bpp$kcoremax)
  bpp$df_cores <- data.frame(species_guild_a, species_guild_b, num_species_guild_a, num_species_guild_b)
  bpp$list_dfs_a <- list()
  bpp$list_dfs_b <- list()
  bpp$df_cores$num_species_guild_a <- 0
  bpp$df_cores$num_species_guild_b <- 0
  bpp$straight_links <- data.frame(x1=c(), x2 = c(), y1 = c(),  y2 = c(), weightlink = c())
  bpp$bent_links <- data.frame(x=c(), y = c(),  number = c(), weightlink = c() )
  bpp$count_bent_links <- 0
}


draw_bipartite_plot <- function(svg_scale_factor, progress)
{
  if (!is.null(progress)) 
    progress$inc(1/11, detail=strings$value("MESSAGE_ZIGGURAT_PROGRESS_PROCESSING_NODES"))
  zinit_time <- proc.time()
  for (i in bpp$ind_cores) {
    nodes_in_core_a <- bpp$g[(bpp$g$guild == bpp$str_guild_a)&(bpp$g$kcorenum == i)]$name
    nodes_in_core_b <- bpp$g[(bpp$g$guild == bpp$str_guild_b)&(bpp$g$kcorenum == i)]$name
    bpp$df_cores[i,]$species_guild_a <- list(unlist(lapply(nodes_in_core_a, function(x) strsplit(x,bpp$str_guild_a)[[1]][[2]])))
    bpp$df_cores[i,]$num_species_guild_a <- length(nodes_in_core_a)
    bpp$df_cores[i,]$species_guild_b <- list(unlist(lapply(nodes_in_core_b, function(x) strsplit(x,bpp$str_guild_b)[[1]][[2]])))
    bpp$df_cores[i,]$num_species_guild_b <- length(nodes_in_core_b)
  }
  bpp$num_a_coremax <- bpp$df_cores[bpp$kcoremax,]$num_species_guild_a
  base_width <- 2000
  bpp$ymax <- 1.75*base_width/bpp$aspect_ratio
  bpp$tot_width <- bpp$ymax

  bpp$species_in_core2_a <- sum(bpp$df_cores[2,]$num_species_guild_a)
  bpp$species_in_core2_b <- sum(bpp$df_cores[2,]$num_species_guild_b)
  bpp$species_in_almond_a <- sum(bpp$df_cores[2:(bpp$kcoremax-1),]$num_species_guild_a)
  bpp$species_in_almond_b <- sum(bpp$df_cores[2:(bpp$kcoremax-1),]$num_species_guild_b)
  bpp$height_y <- bpp$ymax/max(1.3,(1.3*max(bpp$species_in_almond_a,bpp$species_in_almond_b)))
  maxincore2 <- max(bpp$species_in_core2_a,bpp$species_in_core2_b)
  if (bpp$kcoremax < 4)
    if (bpp$species_in_core2_a+bpp$species_in_core2_b < 6)
      bpp$height_y <- (0.08)*bpp$ymax
  bpp$yoffset <- bpp$height_y*maxincore2*bpp$height_box_y_expand
  fmult <- (bpp$ymax+bpp$yoffset)/bpp$ymax
  bpp$ymax <- bpp$ymax + bpp$yoffset
  bpp$tot_width <- bpp$tot_width*fmult
  bpp$height_y <- bpp$height_y * fmult * bpp$height_box_y_expand
  bpp$yoffset <- bpp$height_y*maxincore2
  bpp$ymax <- bpp$ymax * (1+0.1*bpp$height_box_y_expand)
  for (i in seq(3,bpp$kcoremax-1)){
    bpp$displace_y_a[i] <- bpp$displace_y_a[i] + bpp$coremax_triangle_height_factor*bpp$species_in_core2_a*bpp$height_y/bpp$ymax
    bpp$displace_y_b[i] <- bpp$displace_y_b[i] + bpp$coremax_triangle_height_factor*bpp$species_in_core2_b*bpp$height_y/bpp$ymax
  }

  bpp$hop_x <- bpp$factor_hop_x*(bpp$tot_width)/max(1,(bpp$kcoremax-2))
  bpp$lado <- min(0.05*bpp$tot_width,bpp$height_y * bpp$aspect_ratio)
  bpp$basey <- (0.1+0.1*length(bpp$df_cores[bpp$kcoremax,]$num_species_guild_a))*bpp$ymax
  wcormax <- 1.2*bpp$hop_x*bpp$coremax_triangle_width_factor
  bpp$topxa <- 0.65*bpp$hop_x
  bpp$basex <- bpp$topxa - wcormax
  bpp$posic_zig <- 0
  bpp$posic_zig_a <- 0
  bpp$posic_zig_b <- 0
  bpp$toopy <- 0.3*bpp$ymax+bpp$basey
  bpp$strips_height <- 0.6*(bpp$ymax-bpp$yoffset)/max(1,(bpp$kcoremax-2))
  # Draw max core triangles
  svg <-SVG(svg_scale_factor, style = bpp$style)
  
  f <- handle_orphans(bpp$result_analysis$graph)
  bpp$mtxlinks <- f["mtxlinks"][[1]]
  bpp$orphans_a <- f["orphans_a"][[1]]
  bpp$orphans_b <- f["orphans_b"][[1]]
  bpp$df_orph_a <- f["df_orph_a"][[1]]
  bpp$df_orph_b <- f["df_orph_b"][[1]]
  
  sbip <- find_specialists_bip(specialists_a,specialists_b)
  
  if (!is.null(progress))
    progress$inc(1/11, detail=strings$value("MESSAGE_ZIGGURAT_PROGRESS_DRAWING_MAXCORE"))
  f <- draw_maxcore_bip(svg)
  p <- f["p"][[1]]
  svg <- f["svg"][[1]]
  if ((bpp$style=="legacy") || (bpp$style=="kcoreorder"))
    bpp$pos_tail_x <- min(bpp$list_dfs_a[[bpp$kcoremax]]$x1)  # Leftmost side of the leftmost node
  

  if (bpp$paintlinks) {
    z <- draw_inner_links(p, svg)
    p <- z["p"][[1]]
    svg <- z["svg"][[1]]
  }
  
  if (bpp$style=="chilopodograph"){
    bpp$posic_zig <- f["posic_zig"][[1]] 
    bpp$list_dfs_a <- f["list_dfs_a"][[1]]
    bpp$list_dfs_b <- f["list_dfs_b"][[1]] 
    bpp$last_xtail_a <-  f["last_xtail_a"][[1]] 
    bpp$last_ytail_a <- f["last_ytail_a"][[1]]
    bpp$last_xtail_b <- f["last_xtail_b"][[1]] 
    bpp$last_ytail_b <-  f["last_ytail_b"][[1]]
    bpp$topy <- f["topy"][[1]] 
    bpp$topxa <-f["topxa"][[1]] 
    bpp$topxb <- f["topxb"][[1]] 
    bpp$posic_zig <- f["posic_zig"][[1]] 
    bpp$list_dfs_a <- f["list_dfs_a"][[1]]
    bpp$list_dfs_b <- f["list_dfs_b"][[1]] 
    bpp$last_xtail_a <-f["last_xtail_a"][[1]] 
    bpp$last_ytail_a <- f["last_ytail_a"][[1]]
    bpp$last_xtail_b <- f["last_xtail_b"][[1]] 
    bpp$last_ytail_b <- f["last_ytail_b"][[1]] # Add kcore1 information
    if (!is.null(bpp$df_cores[1,])){
      if (bpp$df_cores[1,]$num_species_guild_a > 0)
        bpp$list_dfs_a[[1]] <- conf_kcore1_info(bpp$str_guild_a)
      if (bpp$df_cores[1,]$num_species_guild_b > 0)
        bpp$list_dfs_b[[1]] <- conf_kcore1_info(bpp$str_guild_b)
    }
    
    # Fat tails - nodes of core 1  linked to most generalist of opposite guild. Left side of panel 
    z <-  handle_fat_tails_bip(p, svg,style = bpp$style ) 
    p <- z["p"][[1]] 
    svg <- z["svg"][[1]]
    bpp$pos_tail_x <- z["pos_tail_x"][[1]] 
    
    # Hanlde orphans, species outside the ziggurat
    if (!is.null(progress))
      progress$inc(1/11,  detail=strings$value("MESSAGE_ZIGGURAT_PROGRESS_DRAWING_ORPHANS"))
    bpp$gap <- 4*bpp$height_y 
    if (!is.null(progress)) 
      progress$inc(1/11,detail=strings$value("MESSAGE_ZIGGURAT_PROGRESS_DRAWING_MAXCORE_TAILS")) 
    f <- draw_maxcore_tails_bip(p, svg) 
    p <- f["p"][[1]] 
    svg <- f["svg"][[1]]
    
    bpp$last_xtail_a <- f["last_xtail_a"][[1]] 
    bpp$last_ytail_a <-f["last_ytail_a"][[1]] 
    bpp$last_xtail_b <- f["last_xtail_b"][[1]]
    bpp$last_ytail_b <- f["last_ytail_b"][[1]] 
    # Nodes of core 1 linked to species in cores kcoremax-1 to core 2. 
    if (!is.null(progress)) 
      progress$inc(1/11,detail=strings$value("MESSAGE_ZIGGURAT_PROGRESS_DRAWING_INNER_ORPHANS")) 
    z <- draw_bipartite_leafs(p, svg) 
    p <- z["p"][[1]] 
    svg <- z["svg"][[1]]
  }

  # Legend, title and final annotations
  v <- write_annotations(p, svg)
  p <- v["p"][[1]]
  svg <- v["svg"][[1]]
  # Plot straight links
  if (!is.null(progress))
    progress$inc(1/11, detail=strings$value("MESSAGE_ZIGGURAT_PROGRESS_DRAWING_LINKS"))
  if (bpp$paintlinks){
    if (nrow(bpp$straight_links)>0) {
      p <- p+ geom_segment(data=bpp$straight_links, aes(x=x1, y=y1, xend=x2, yend=y2),
                           linewidth=bpp$straight_links$weightlink, color=bpp$color_link ,alpha=bpp$alpha_link)
      factormult <- 0.1*svg_scale_factor
      svg$segment(idPrefix="link", data=bpp$straight_links, mapping=aes(x=x1, y=y1, xend=x2, yend=y2),
                  alpha=bpp$alpha_link, color=bpp$color_link, size=bpp$straight_links$weightlink)
    }
    if (nrow(bpp$bent_links)>0) {
      p <- p + geom_path(data =bpp$bent_links,aes(x,y,group=number), linewidth=bpp$bent_links$weightlink,
                         color=bpp$color_link ,alpha=bpp$alpha_link)
      svg$path(idPrefix="link", data=bpp$bent_links, mapping=aes(x, y, group=number), alpha=bpp$alpha_link,
               color=bpp$color_link,size=bpp$bent_links$weightlink)
    }
  }
  if (is.null(progress))
    display_plot(p,bpp$print_to_file,bpp$flip_results, landscape = bpp$landscape_plot, fname_append = bpp$file_name_append)

  # Stores results
  bpp$plot  <- p
  bpp$svg   <- svg
  # 
  html<-svg$html()
  cat(html, file = "tmp.svg")
  
  return(bpp)

}
if (debugging)
  # bipartite_graph("data/","dattilo2014.csv", style="chilopodograph",orderkcoremaxby = "kdegree",
  #              guild_gap_increase = 1,weighted_links = "none",square_nodes_size_scale=1,backg_color = "white",
  #              hide_plot_border = FALSE, flip_results=FALSE)
  old_bipartite_graph("data/","RA_HP_042.csv",square_nodes_size_scale = 2,
                       style="chilopodograph",orderkcoremaxby = "kdegree",
                       guild_gap_increase = 1,weighted_links = "none",svg_scale_factor = 1,color_link = "#6d6d6e",

                       hide_plot_border = TRUE,move_all_SVG_right = 90)