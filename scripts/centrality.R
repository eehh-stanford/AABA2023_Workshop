
## Read in Florentine marriages, convert to igraph
require(igraph)
flo <- read.table("../data/flo.txt", 
                  header=TRUE, row.names=1)
gflo <- graph.adjacency(as.matrix(flo), mode="undirected")
gflo


## Plot
plot(gflo, vertex.color="skyblue2",vertex.label.family="Helvetica")


## Is it connected
is.connected(gflo)
clusters(gflo)


## Remove the isolate
gflo1 <- decompose.graph(gflo)[[1]]
## could also do this: gflo1 <- induced_subgraph(gflo, subcomponent(gflo,1))
is.connected(gflo1)
plot(gflo1, vertex.color="skyblue2",vertex.label.family="Helvetica")


## Graph density
## note we're working with the matrix, not the graph object here
n <- dim(flo)[1]
2*sum(apply(flo,1,sum)/2)/(n*(n-1))


## Graph density with Pucci removed
n <- n-1
2*sum(apply(flo,1,sum)/2)/(n*(n-1))
graph.density(gflo1)


## Information centrality
infocentral <- function(X){
  ## assumes binary relation
  k <- dim(X)[1]
  A <- matrix(as.numeric(!X),nr=k,nc=k)
  diag(A) <- apply(X,1,sum)+1
  C <- solve(A)
  T <- sum(diag(C))
  R <- apply(C,1,sum)[1]
  ic <- 1/(diag(C) + (T - 2*R)/k)
  return(ic)
}


## Compare centralities
# need a matrix
# remove Pucci
flo1 <- flo[-12,-12]
ic <- infocentral(flo1)
CP <- abs(bonpow(gflo1))
CE <- eigen_centrality(gflo1)$vector

flo_measures <- cbind(CD=degree(gflo1),
                CB=round(betweenness(gflo1),1),
                CC=round(closeness(gflo1),2),
                CI=round(ic,2),
                CP=round(CP,2),
                CE=round(CE,2))
              
dimnames(flo_measures)[[1]] <- dimnames(flo1)[[1]]
flo_measures


## Plot using betweenness to scale vertex size
plot(gflo1, vertex.color="plum", vertex.size=flo_measures[,"CB"]+1, 
     vertex.label.family="Helvetica", vertex.label.cex=0.5)


## correlation matrix of the centralities
cor(flo_measures)
## vertex size proportional to eigenvalue centrality
plot(gflo1, vertex.size=flo_measures[,"CE"]*10, vertex.color="plum",
     vertex.label.family="Helvetica")


## Kapferer's tailor shop
A <- as.matrix(read.table("data/kapferer-tailorshop1.txt", 
                          header=TRUE, row.names=1))
G <- graph.adjacency(A, mode="undirected", diag=FALSE)
plot(G,vertex.shape="none", vertex.label.cex=0.75, 
     vertex.label.family="Helvetica", edge.color=grey(0.85))


## Compare centralities
c <- infocentral(A)
CP <- abs(bonpow(G))
CE <- eigen_centrality(G)$vector

kap_measures <- cbind(CD=degree(G),
                CB=round(betweenness(G),1),
                CC=round(closeness(G),2),
                CI=round(c,2),
                CP=round(CP,2),
                CE=round(CE,2))
dimnames(kap_measures)[[1]] <- dimnames(A)[[1]]
kap_measures
cor(kap_measures)


## Sade (1972) rhesus monkeys, grooming interactions
rhesus <- read.table("data/sade1.txt", header=FALSE)
rhesus <- as.matrix(rhesus)
rhesus
nms <- c("066", "R006", "CN", "ER", "CY", "EC", "EZ", "004", "065", "022", "076", "AC", "EK", "DL", "KD", "KE")
sex <- c(rep("M",7), rep("F",9))
dimnames(rhesus)[[1]] <- nms
dimnames(rhesus)[[2]] <- nms
grhesus <- graph.adjacency(rhesus, weighted=TRUE)
V(grhesus)$sex <- sex

rhesus.layout <- layout.kamada.kawai(grhesus)
plot(grhesus, 
     edge.width=log10(E(grhesus)$weight)+1, 
     edge.arrow.width=0.5,
     edge.color=grey(0.85),
     vertex.shape="none",
     vertex.label=V(grhesus)$name, 
     vertex.label.family="Helvetica",
     vertex.label.color=as.numeric(V(grhesus)$sex=="F")+5, 
     layout=rhesus.layout)


## Compare centralities
c <- infocentral(rhesus)
CP <- abs(bonpow(grhesus))
CE <- eigen_centrality(grhesus)$vector

rh_measures <- cbind(CD=degree(grhesus),
               CB=round(betweenness(grhesus),1),
               CC=round(closeness(grhesus),2),
               CI=round(c,2),
                CP=round(CP,2),
                CE=round(CE,2))
dimnames(rh_measures)[[1]] <- nms
rh_measures
cor(rh_measures)


## plot rhesus
plot(grhesus, edge.arrow.width=0.5, 
     edge.color=grey(0.85),
     vertex.size=rh_measures[,"CB"]/1.5, 
     vertex.color="plum", 
     vertex.frame.color="plum",
     vertex.label=NA, 
     layout=rhesus.layout)

