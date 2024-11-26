rm(list = ls())
library(reshape2)
library(ggplot2)
library(kcorebip)
library(igraph)


plot_m <- function(longData,flip_results=FALSE,nname="",ncolor="green",links_weight=FALSE,
                   strA="",strB="",colorA="blue",colorB="red",lsize=8)
  {
    legends_text <- paste0(
    "<span style = 'text-align: left; color:black'>Network: ",nname,
    "</span><span style = 'text-align: right; color:white'>....</span><span style = 'text-align: right; color:",colorA,"'>",paste('&#9632;',strA),
    "</span> <span style = 'text-align: right;color:",colorB,"'>",paste('&nbsp; &#9632;',strB),"</span>")

    #mplots <- ggplot(longData, aes(x = speciesA, y = speciesB,fill=as.factor(value))) + 
      if (!flip_results)
        mplots<- ggplot(longData, aes(x = speciesA, y = species_b,fill=as.factor(value)))
      else
        mplots<- ggplot(longData, aes(x = rev(speciesB), y = rev(speciesA),fill=as.factor(value)))
    mplots <- mplots + geom_tile(color = ncolor,alpha=1) +
      scale_fill_manual(values=c("white",ncolor))+
      ylab("")+xlab("")+coord_fixed()
    if (!flip_results){
      mplots <- mplots +scale_x_discrete(position="top",labels=unique(longData$numA))+
        scale_y_discrete(labels=unique(longData$numB))
    } else {
      mplots <- mplots +scale_x_discrete(position="top",labels=rev(unique(longData$numB)))+
        scale_y_discrete(labels=rev(unique(longData$numA)))
    }
    
    mplots <- mplots+theme_void()+labs(caption=legends_text)+
      theme(legend.position="none",
            axis.text.x = element_text(size=lsize,hjust=0,vjust=1,angle=90,color=ifelse(!flip_results,colorA,colorB)),
            axis.text.y = element_text(size = lsize,hjust=1,vjust=0.5,color=ifelse(!flip_results,colorB,colorA)),
            plot.title = element_text(size=lsize+2,hjust=0.5),
            axis.title.x=element_text(size=18,face="bold",color="grey40",
                                      hjust=0.5),
            plot.subtitle = ggtext::element_markdown(hjust=0,vjust=0),
            plot.margin = unit(c(1,1,1,1), "cm"),
            plot.caption = ggtext::element_markdown(size=lsize+4,hjust=1,vjust=0))
    return(mplots)
    # if (!flip_results)
    #   mplots<- ggplot(longData, aes(x = speciesA, y = speciesB), fill=as.factor(value))
    # else
    #   mplots<- ggplot(longData, aes(x = rev(speciesB), y = rev(speciesA)),fill=as.factor(value))
    
    mplots<- ggplot(longData, aes(x = speciesA, y = speciesB), fill=as.factor(value))
    
    #if (!links_weight)
      mplots <- mplots+geom_tile(color =ncolor,alpha=1)+scale_fill_manual(values=c("white",ncolor))
    #else
      #mplots <- mplots+geom_tile()
      #geom_tile(color = ncolor,alpha=1) +  scale_fill_manual(values=c("white",ncolor))+
    mplots <- mplots +ylab("")+xlab("")+coord_fixed()
    if (!flip_results){
      mplots <- mplots +scale_x_discrete(position="top",labels=unique(longData$numA))+
                        scale_y_discrete(labels=unique(longData$numB))
    } else {
      mplots <- mplots +scale_x_discrete(position="top",labels=rev(unique(longData$numB)))+
                        scale_y_discrete(labels=rev(unique(longData$numA)))
    }
    
    mplots <- mplots+theme_void()+labs(caption=legends_text)+
    theme(legend.position="none",
          axis.text.x = element_text(size=lsize,hjust=0,vjust=1,angle=90,color=ifelse(!flip_results,colorA,colorB)),
          axis.text.y = element_text(size = lsize,hjust=1,vjust=0.5,color=ifelse(!flip_results,colorB,colorA)),
          plot.title = element_text(size=lsize+2,hjust=0.5),
          axis.title.x=element_text(size=18,face="bold",color="grey40",
                                    hjust=0.5),
          plot.subtitle = ggtext::element_markdown(hjust=0,vjust=0),
          plot.margin = unit(c(1,1,1,1), "cm"),
          plot.caption = ggtext::element_markdown(size=lsize+4,hjust=1,vjust=0))
  return(mplots)
}

create_labels <- function(M,nums,i,show_species=FALSE,flip_results=TRUE,guild="A"){
  if (!flip_results){
    if (guild=="A")
      return(paste0("   ",nums[i]," ",ifelse(show_species,colnames(M)[i],""),"   "))
    else
      return(paste0(" ",nums[i]," ",ifelse(show_species,paste0(rownames(M)[i],"")),"    "))
  } else {
    if (guild=="A")
      return(paste0(" ",nums[i]," ",ifelse(show_species,colnames(M)[i],""),"    "))
    else
      return(paste0(" ",nums[i]," ",ifelse(show_species,paste0(rownames(M)[i],"  "),"    ")))
  }
}

matrix_graph <-function(datadir,filename,
                         print_to_file = FALSE, plotsdir ="plot_results/", 
                         orderkcoremaxby = "kradius",
                         flip_results = FALSE, links_weight = FALSE,
                         alpha_level = 0.2, label_strguilda = "",
                         label_strguildb = "", show_species_names = FALSE,
                         color_guild_a = "#4169E1", 
                         color_guild_b = "#FF1808")
{
  mat_argg <- c(as.list(environment()))
  # Create global environment
  mat <<- new.env()
  f<-kcorebip:::read_and_analyze(datadir,filename,label_strguilda, label_strguildb)
  mat$result_analysis <- f["result_analysis"][[1]]
  mat$str_guild_a <- f["str_guild_a"][[1]]
  mat$str_guild_b <- f["str_guild_b"][[1]]
  mat$name_guild_a <- f["name_guild_a"][[1]]
  mat$name_guild_b <- f["name_guild_b"][[1]]
  mat$network_name <- f["network_name"][[1]]
  lnodes <- V(mat$result_analysis$graph)
  species_a <- data.frame("num"=seq(1:mat$result_analysis$num_guild_a),"name"=colnames(mat$result_analysis$matrix))
  species_a$kdegree <- lnodes$kdegree[1:mat$result_analysis$num_guild_a]
  species_a$kradius <- lnodes$kradius[1:mat$result_analysis$num_guild_a]
  if (sum(species_a$kradius==Inf)>0)
    species_a[species_a$kradius==Inf,]$kradius=100
  species_a$kshell <- lnodes$kcorenum[1:mat$result_analysis$num_guild_a]
  species_a$num <- seq(1:mat$result_analysis$num_guild_a)
  species_b <- data.frame("num"=seq(1:mat$result_analysis$num_guild_b),"name"=rownames(mat$result_analysis$matrix))
  species_b$kdegree <- lnodes$kdegree[seq(mat$result_analysis$num_guild_a+1,mat$result_analysis$num_guild_a+mat$result_analysis$num_guild_b)]
  species_b$kradius <- lnodes$kradius[seq(mat$result_analysis$num_guild_a+1,mat$result_analysis$num_guild_a+mat$result_analysis$num_guild_b)]
  if (sum(species_b$kradius==Inf)>0)
    species_b[species_b$kradius==Inf,]$kradius=100
  species_b$kshell <- lnodes$kcorenum[seq(mat$result_analysis$num_guild_a+1,mat$result_analysis$num_guild_a+mat$result_analysis$num_guild_b)]
  species_b$num <- seq(1:mat$result_analysis$num_guild_b)
  M<-mat$result_analysis$matrix
  binary_network = (sum(M[M>1])==sum(M))
  #if (!links_weight)
    M[M>1]=1
  
  species_a$degree <- colSums(M)
  species_b$degree <- rowSums(M)
  ordvectorB <- order(10000*species_b$kshell-(100*species_b$kradius-species_b$degree))
  ordvectorA <- rev(order(10000*species_a$kshell-(100*species_a$kradius-species_a$degree)))
  M<-M[ordvectorB,ordvectorA]
  num_b = species_b[ordvectorB,]$num
  num_a = species_a[ordvectorA,]$num
  longData<-melt(M)
  longData$numA <- 0
  longData$numB <- 0
  names(longData) = c("speciesB","speciesA", "value","numA","numB")
  for (i in 1:length(colnames(M)))
    longData[longData$speciesA==colnames(M)[i],]$numA = create_labels(M,num_a,i,show_species=show_species_names,flip_results=flip_results,guild="A")
  for (i in 1:length(rownames(M)))
    longData[longData$speciesB==rownames(M)[i],]$numB = create_labels(M,num_b,i,show_species=show_species_names,flip_results=flip_results,guild="B")

  ncolor <- "grey40"
  lsize <- 16 - round(log10(sqrt(nrow(longData))))
  p <- plot_m(longData,flip_results=flip_results,nname=mat$network_name,ncolor=ncolor,
              strA=mat$name_guild_a,strB=mat$name_guild_b,links_weight = (links_weight && !binary_network),
              colorA=color_guild_a,colorB=color_guild_b,lsize=lsize)
  return(p)
}

# 
# B <-unname(mat$result_analysis$matrix)
# B[B>1]=1
# B<-B[order(species_b$kdegree),rev(order(species_a$kdegree))]
# longData<-melt(B)
# ncolor <- "magenta"
# q <- plot_m(longData,title="kdegree")
# 
# 
# C <-unname(mat$result_analysis$matrix)
# C[C>1]=1
# C<-C[order(species_b$degree),rev(order(species_a$degree))]
# 
# longData<-melt(C)
# ncolor <- "green"
# r <- plot_m(longData,title="degree")

#pd <- ( p | q | r)
# pd <- pd +  plot_annotation(title = netw,theme = theme(plot.title = element_text(size = 20,face="bold",color="gray20",hjust=0.5)))


p <- matrix_graph("data/","RA_HP_042.csv",
                  print_to_file = FALSE, plotsdir ="plot_results/", 
                  orderkcoremaxby = "kradius",
                  flip_results = TRUE, links_weight = FALSE,
                  alpha_level = 0.2, label_strguilda = "",
                  label_strguildb = "", show_species_names = TRUE)

fscalegraph = 2
plsize=8*fscalegraph
ppi = 300
nfile <- paste0("MATRIX",".png")
png(nfile,width=plsize*ppi,height=plsize*ppi,res=ppi)
print(p)
dev.off()