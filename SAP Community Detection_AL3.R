##Community Structures in the SAP Network
## Sanjay Dasari

#Set Working Directory and clear the environment
getwd()
setwd("C:/Users/Owner/Desktop/UIC MSBA/R Practice/SNAAL3")
rm(list=ls())
### Initialize the necessary packages
library(igraph)
edge_file <- "CollabNetEdgeListFilteredDec7_2012.csv"
node_file <- "NodesNetList_corrected_Feb19_2016 (1).csv"
#Reading Files and creating data frames for the igraph package
S_nodes <- read.csv(node_file,header = T, sep = ",")
S_edges <- read.csv(edge_file,header = T, sep = ",")
SAP <- graph.data.frame(S_edges,vertices=S_nodes,directed = F )
SAP1 <- graph.data.frame(S_edges,vertices=S_nodes,directed = F )
#Basic Network Characteristics
vcount(SAP1)
ecount(SAP1)
is.connected(SAP1)
is.simple(SAP1)
##Simplifying the graph
E(SAP1)$weight <- 1
E(SAP1)$weight
SAP1_simplify <-simplify(SAP1,edge.attr.comb =sum)
is.simple(SAP1_simplify)
vcount(SAP1_simplify)
ecount(SAP1_simplify)
E(SAP1_simplify)$weight
names(S_nodes)
names(S_edges)
##Vertex Attributes
SAP.country <- get.vertex.attribute(SAP1_simplify,"country")
SAP.points <-get.vertex.attribute(SAP1_simplify,"ln_points")
##Community Detection-Walktrap
SAP1_simplify_wt <- walktrap.community(SAP1_simplify,weights = E(SAP1_simplify)$weight)
class(SAP1_simplify_wt)
length(SAP1_simplify_wt)
c.m.w <- membership(SAP1_simplify_wt)
table(c.m.w, SAP.country, useNA = c("no"))
##Community Detection- Fast Greedy
SAP_fg <-fastgreedy.community(SAP1_simplify,weights = E(SAP1_simplify)$weight)
length(SAP_fg)
class(SAP_fg)
c.m.f <- membership(SAP_fg)
table(c.m.f, SAP.country, useNA = c("no"))
CF_SAP <- contract.vertices(SAP1_simplify,c.m.f)
length(CF_SAP)
plot(CF_SAP)


C_SAP <- contract.vertices(SAP1_simplify_wt,membership(SAP1_simplify_wt))
