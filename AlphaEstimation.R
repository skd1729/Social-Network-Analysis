setwd("C:/Users/Owner/Desktop/UIC MSBA/R Practice/SNAAL4")
rm(list=ls())
library(igraph)
infile1 <- "Share_corp_alliance_EdgeList_2007.csv"
infile2 <- "Share_corp_alliance_EdgeList_2014.csv"
E7 <- read.csv(infile1, header = T, sep = ",")
E4 <- read.csv(infile2,header = T, sep = ",")
C7 <- graph.data.frame(E7,directed = F )
C4 <- graph.data.frame(E4,directed = F)
vcount(C7)
vcount(C4)
#For 2007
df_deg <- as.data.frame(table(degree(C7)))
colnames(df_deg) <- c('degree','count')
write.csv(df_deg, file = 'degree_dist7.csv',row.names = F)
infile3 <- "degree_dist7.csv"
x = read.csv(infile3,header=T,sep=",")
par(pch=21, col="blue")
par(mfrow=c(1,2))
plot(x$degree,x$count,type='o',main="Degree Distribution:2007",xlab="Degree",ylab="Frequency")
plot(log(x$degree),log(x$count),type= 'o',main="Log Degree Distribution:2007",xlab="Log Degree",ylab="Log Frequency")
plot(log(log(x$degree)),log(log(x$count)),type='o',main="Log of Log-Log Plot",xlab="Log-Log-Degree",ylab="Log-Log Frequency")

#For 2014
df_deg <- as.data.frame(table(degree(C4)))
colnames(df_deg) <- c('degree','count')
write.csv(df_deg, file = 'degree_dist4.csv',row.names = F)
infile4 <- "degree_dist4.csv"
x = read.csv(infile4,header=T,sep=",")
vcount(C4)
par(pch=22, col="brown")
par(mfrow=c(2,2))
plot(log(x$degree),log(x$count),type= 'o')
par(pch=21, col="blue")
par(mfrow=c(1,2))
plot(x$degree,x$count,type='o',main="Degree Distribution:2014",xlab="Degree",ylab="Frequency")
plot(log(x$degree),log(x$count),type= 'o',main="Log Degree Distribution:2014",xlab="Log Degree",ylab="Log Frequency")


#Function for calcuating the X and Y values
cd <- function(x,alpha){
  
  names(x)[2] = paste("freq")
  m = sum(x[,1]*x[,2])/(2*sum(x[,2]))  ## Average degree is sumproduct divided by total frequency
  # To get m, divide average degree by 2
  x$alpha = alpha
  sub = (2*m*alpha)/(1-alpha)
  x$sub = sub
  x$d_sub = x[,1]+sub 
  
  for(i in 1:nrow(x)){
    x$fd[i] = sum(x$freq[1:i])/sum(x$freq)
    x$fd_1[i] = 1-x$fd[i]
    i+1}
  x$X = log(x$d_sub)  ## X values for regression
  x$Y = log(x$fd_1) # Y values for regression
  return (x)
}

#Import the results to dataframe
#results = cd(x,.11)

#Function for getting regression coefficients

reg_results = function(results,alpha) {
  
  x = data.frame(coefficients(lm((results$Y[1:nrow(results)-1])~results$X[1:nrow(results)-1])))
  #Log 0 is not defined. Hence last row is not considered always.
  alpha_0 = 0
  Beta = 0
  Intercept = 0
  alpha_1 = 0
  y = data.frame(cbind(alpha_0,Beta,Intercept,alpha_1))
  y$alpha_0[1] = alpha
  y$Beta[1] = x[2,1]
  y$Intercept[1] = x[1,1]
  y$alpha_1[1] = 1+(2/y$Beta)
  
  return (y) 
}



# Insert alpha values in a dataframe
alpha_vals = data.frame(c(.01,.03,.05,.08,.1,
                          .12,.15,.18,.2,
                          .22,.25,.28,.3,
                          .33,.36,.4,
                          .45,.5,.55,.6,.65,.7,
                          .75,.78,.8,.83,.85,.88,
                          .91,.93,.95,.97,.998,.999))
#change the values in the vector , if you want results for different Alpha values
names(alpha_vals)[1] = paste("alpha")

#Function to generate the coefficents for all the desired alpha values
total = function(x,alpha_vals){
  total1 = data.frame()
  for (i in 1:nrow(alpha_vals)) {
    results = data.frame(cd(x,alpha_vals[i,1]))
    total = reg_results(results,alpha_vals[i,1])
    total1 = rbind(total1,total)
    i + 1
  }
  return(total1)
}

#Use the function to get the final coefficients 
final = total(x,alpha_vals)
final7=total(x,alpha_vals)


s <- seq(1,34,1)
plot(s,final$alpha_1,col="green",type="o",main="Alpha Convergence:2014",ylab="Alpha Values",xlab="Index")
lines(s,final$alpha_0,type="o")
legend("topleft",legend=c("Alpha_1","Alpha_0"),col=c("green","blue"),lty=1:2, cex=0.8)
write.csv(final, file = 'Alpha2014_V1.csv',row.names= F)


# Degree Distribution COde
par(pch=21, col="black")
#par(mfrow=c(1,2))
t7 <- degree_distribution(C7)
t4 <- degree.distribution(C4)
plot(t7,type="o",main="Cumulative Distribution Function :2007",xlab="Degree",ylab="")
plot(t4,type="o",main="Cumulative Distribution Function :2014",xlab="Degree",ylab="")

#Maximal Cliques
MC7 <-largest.cliques(C7)
MC14 <-largest.cliques(C4)

clique7 <- MC7[[1]]
clique41 <- MC14[[1]]
clique42 <- MC14[[2]]
PC7 <- induced.subgraph(graph=C7,vids=clique7)
plot(PC7,main = " Largest Component in 2007")
PC41 <-induced.subgraph(graph = C4, vids=clique41)
PC42 <- induced.subgraph(graph=C4,vids=clique42)
par(mfrow=c(1,1))
plot(PC41,main = " Largest Component 1 in 2014")
plot(PC42,main = "Largest Component 2 in 2014")
mtext("Largest Components in 2014",outer = TRUE, cex=1.5)

## Total Cliques
cn7 <- cliques(C7)
cn4 <- cliques(C4)
tc7 <- table(lengths(cn7))
tc4 <- table(lengths(cn4))

bb = as.data.frame(tc7,row.names = NULL)
cc = as.data.frame(tc4,row.names=NULL)
bb$Var1 <- as.numeric(bb$Var1)
cc$Var1 <- as.numeric(cc$Var1)
plot(cc$Var1,log(cc$Freq),main="Clique Distribution",type="o",ylim=c(1,11),col="green",cex=1,lwd=2.5,xlab="Clique Size",ylab="Log(Frequency)")
lines(bb$Var1,log(bb$Freq),type="o",col="brown",cex=1,lwd=2.5)
legend("topright",legend=c("2014 Cliques","2007 Cliques"),lty =1,col=c("green","brown"))
dd = cc$Freq[1:8]-bb$Freq
ee <- c(20,2)
ff=union(dd,ee)
gg = seq(1,10,1)
plot(gg,log(ff))




#Neighbors of High Degree Centrality Node
neighbors(C7,3)
V(C7)$name[max(degree)]


## Comparison on Network Measures

diameter(C7)
diameter(C4)
transitivity(C4)
assortativity(C4)
graph.density(C7)
graph.density(C4)
vcount(C4)
## Ego Graphs
par(mfrow=c(1,1))
set.seed(123)
ego4 <-make_ego_graph(C4,3,nodes="08987H")
ego7 <- make_ego_graph(C7,2,nodes= "08987H")

plot(ego7[[1]],main= "2007: III order neighborhood of 08987H")
plot(ego4[[1]],main = " 2014: III order neighborhood of 08987H")

v(EE)$color = ifelse(V(EE)== "00394A","green","brown")

n4 = neighbors(C4,"78031A")
n7 = neighbors(C7,"78031A")
degree(C4,"62517X")
largest.cliques(C7)

dg <- clusters(C4)
lengths(dg)
dg4 <- decompose.graph(C4)
dg7 <- decompose.graph(C7)
cd7 <- components(C4)
lengths(cd7)

dg4 <- plot.decompose()

