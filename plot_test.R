# Auxiliar script to test if kcorebip was properly installed

library("kcorebip")
X11()
network_test <- data.frame(GuildB= c(),A1=c(),A2=c(),A3=c(),A4=c())
network_test <- rbind(network_test,data.frame(GuildB= "B1",A1=1,A2=1,A3=1,A4=0))
network_test <- rbind(network_test,data.frame(GuildB= "B2",A1=1,A2=1,A3=0,A4=0))
network_test <- rbind(network_test,data.frame(GuildB= "B3",A1=1,A2=1,A3=0,A4=1))
network_test <- rbind(network_test,data.frame(GuildB= "B4",A1=1,A2=0,A3=0,A4=0))
network_test <- rbind(network_test,data.frame(GuildB= "B5",A1=0,A2=0,A3=0,A4=1))
write.table(network_test,"ntest.csv",sep=",",row.names = FALSE)
ziggurat_graph("","ntest.csv",square_nodes_size_scale = 3, 
               displace_legend = c(-0.5,0.3), root_weird_expand = c(0.5,1))

while (!is.null(dev.list())) Sys.sleep(1)
file.remove("ntest.csv")