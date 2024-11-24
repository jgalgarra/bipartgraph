library(reshape2)
library(ggplot2)
library(kcorebip)
library(patchwork)
library(igraph)

plot_m <- function(longData,title="",numa="",numb=""){
  mplots<- ggplot(longData, aes(x = speciesA, y = speciesB,fill=as.factor(value))) + 
    geom_tile(color = ncolor,alpha=1) +
    scale_fill_manual(values=c("white",ncolor))+
    ylab("")+xlab(title)+coord_fixed()+
    #scale_y_discrete()+
    
    # scale_x_discrete(breaks=seq(1:length(numa)))+
    #                                      scale_y_discrete()+
    # scale_y_discrete(breaks=seq(1,length(numb)),labels=numb)+
    theme_void()+
    theme(legend.position="none",
          axis.text.x = element_text(size=8,hjust=1,vjust=0.5,angle=90,color="red"),
          axis.text.y = element_text(size = 8,hjust=1,vjust=0.5),
          axis.line.x.top = element_line(linetype = "dashed", 
                                         color = "blue",
                                         size = 2,
                                         arrow = grid::arrow()),
          axis.title.x=element_text(size=18,face="bold",color="grey40",
                                    hjust=0),
  plot.margin = unit(c(1,1,1,1), "cm"))
  return(mplots)
}

label_strguilda ="A"
label_strguildb ="P"
#result_analysis <- analyze_network(directory = "data/", "RA_HP_042.CSV", guild_a = "A", guild_b = "P", only_NODF = TRUE)
f<-kcorebip:::read_and_analyze("data/","RA_HP_042.CSV",label_strguilda, label_strguildb)
lnodes <- V(f$result_analysis$graph)
species_a <- data.frame("num"=seq(1:f$result_analysis$num_guild_a),"name"=colnames(f$result_analysis$matrix))
species_a$kdegree <- lnodes$kdegree[1:f$result_analysis$num_guild_a]
species_a$kradius <- lnodes$kradius[1:f$result_analysis$num_guild_a]
species_a$num <- seq(1:f$result_analysis$num_guild_a)
species_b <- data.frame("num"=seq(1:f$result_analysis$num_guild_b),"name"=rownames(f$result_analysis$matrix))
species_b$kdegree <- lnodes$kdegree[seq(f$result_analysis$num_guild_a+1,f$result_analysis$num_guild_a+f$result_analysis$num_guild_b)]
species_b$kradius <- lnodes$kradius[seq(f$result_analysis$num_guild_a+1,f$result_analysis$num_guild_a+f$result_analysis$num_guild_b)]

A<-(f$result_analysis$matrix)
A[A>1]=1
species_a$degree <- colSums(A)
species_b$degree <- rowSums(A)
A<-A[rev(order(1000*species_b$kradius+species_b$kdegree)),
        (order(1000*species_a$kradius+species_a$kdegree))]
#A <- as.data.frame(A)

num_b = species_b[rev(order(species_b$kradius)),]$num
num_a = species_a[order(species_a$kradius),]$n

longData<-melt(A)
names(longData) = c("speciesB","speciesA", "value")

ncolor <- "blue"

p <- plot_m(longData,title="kradius",numa = num_a, numb = num_b)

B <-unname(f$result_analysis$matrix)
B[B>1]=1
B<-B[order(species_b$kdegree),rev(order(species_a$kdegree))]
longData<-melt(B)
ncolor <- "magenta"
q <- plot_m(longData,title="kdegree")


C <-unname(f$result_analysis$matrix)
C[C>1]=1
C<-C[order(species_b$degree),rev(order(species_a$degree))]

longData<-melt(C)
ncolor <- "green"
r <- plot_m(longData,title="degree")

pd <- ( p | q | r)
  # pd <- pd +  plot_annotation(title = netw,theme = theme(plot.title = element_text(size = 20,face="bold",color="gray20",hjust=0.5)))
  plsize=8
  ppi = 150
  nfile <- paste0("MATRIX",".png")
  png(nfile,width=plsize*ppi,height=plsize*ppi,res=ppi)
  print(p)
  dev.off()