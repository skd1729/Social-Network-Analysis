library(igraph)
library(Matrix)
infile_sub<-"SAPFull_SubGraph_EdgeList.csv"
SAP_AP <-read.csv(infile_sub, header = TRUE, sep = ",")
AP_SAPSub=graph.data.frame(SAP_AP, directed = TRUE, vertices= NULL)
E(AP_SAPSub)$weight <-1
AP_SAPSub_simpl<-simplify(AP_SAPSub, edge.attr.comb="sum")
class(AP_SAPSub_simpl)
A<- get.adjacency(AP_SAPSub_simpl)
image(A)
#V1_out <- order(degree(AP_SAPSub_simpl, mode = "out"))
V1_out <- (degree(AP_SAPSub_simpl))
vcount(AP_SAPSub_simpl)
length(V1_out)
V1_in <- order(degree(AP_SAPSub_simpl,mode = "in"))
image(A[i,V1_out])
image(A[i,i_in])

#neighbors(g_SAPSub_simpl, v=c('900'))

MN <- (knn(AP_SAPSub_simpl))
plot(V1_out,MN)

assortativity(AP_SAPSub_simpl)
