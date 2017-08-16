##Sanjay Dasari
# This code is used to extract the maximum cliques in a network
# After extracting the maximal cliques, the network is explored among maximal cliques.


library(igraph)
g_SAPSub_simpl1
#install.packages("egonet")
#library(egonet)
rm(list=ls()) 

#ego(g_SAPSub_simpl1,50, nodes = V(g_SAPSub_simpl1), mode = c("all", "out", "in"),mindist = 25)

#Y1 <-make_ego_graph(g_SAPSub_simpl1,25, nodes = V(g_SAPSub_simpl1), mode = c("all", "out", "in"),mindist = 22)


data1 <- read.csv("SAPFull_SubGraph_EdgeList.csv", header = T , sep = ',')


g1 <- graph.data.frame(data1,directed=F)
plot(g1)

# finding out the nodes involved in the maximal cliques
LC <- largest.cliques(g1)
par(mfrow=c(3, 2))
clique1 <- LC[[1]]
g2 <- induced.subgraph(g1,vids = clique1)
plot(g2)

clique1 <- LC[[2]]
g2 <- induced.subgraph(g1,vids = clique1)
plot(g2)

clique1 <- LC[[3]]
g2 <- induced.subgraph(g1,vids = clique1)
plot(g2)

clique1 <- LC[[4]]
g2 <- induced.subgraph(g1,vids = clique1)
plot(g2)

clique1 <- LC[[5]]
g5 <- induced.subgraph(g1,vids = clique1)
plot(g2)
class(LC)

U <- unique(LC)
D <- unlist(U)
 as.igraph(D)

table(D)
degree(g5, v= V(g5))

# Extracting the unique nodes in the cliques to deduce a sub-graph

#V123 <- c('592540','701187','1046999','2704623,983891,22328,3552437,3510478,623434,22328,702470,1043,983891)

