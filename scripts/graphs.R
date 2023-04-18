
## The simplest form of graph
require(igraph)
g <- graph(c(1,2), n=2, dir=FALSE)
plot(g, vertex.color="skyblue2",vertex.label.family="Helvetica")


## Triangles are really important for network theory
# generate a triangle
g <- graph( c(1,2, 2,3, 1,3), n=3, dir=FALSE)
# the way it gets plotted with probably not make you happy -- fix it
### do some stuff with tkplot() and get coords which we call tri.coords
tri.coords <- matrix( c(228,416, 436,0, 20,0), nr=3, nc=2, byrow=TRUE)
par(mfrow=c(1,2))
plot(g, vertex.color="skyblue2",vertex.label.family="Helvetica")
plot(g, layout=tri.coords, vertex.color="skyblue2",vertex.label.family="Helvetica")


## undirected triangle, using a provided layout
A <- matrix( c(0,1,1, 1,0,1, 1,1,0), nrow=3, ncol=3, byrow=TRUE)
g <- graph_from_adjacency_matrix(A, mode="undirected", diag=FALSE)
plot(g, layout=tri.coords, vertex.color="skyblue2",vertex.label.family="Helvetica")


## directed triangle using provided layout
A1 <- matrix( c(0,1,0, 0,0,1, 1,0,0), nrow=3, ncol=3, byrow=TRUE)
g1 <- graph_from_adjacency_matrix(A1, mode="directed", diag=FALSE)
plot(g1, layout=tri.coords, vertex.color="skyblue2",vertex.label.family="Helvetica")


## Read data: Kapferer's Tailor Shop
A <- as.matrix(read.table("../data/kapferer-tailorshop1.txt", 
                          header=TRUE, row.names=1))
G <- graph.adjacency(A, mode="undirected", diag=FALSE)
plot(G, vertex.color="skyblue2", vertex.label.family="Helvetica")


## Plot with just labels
plot(G,vertex.shape="none", vertex.label.cex=0.75, edge.color=grey(0.85),
     vertex.label.family="Helvetica")


## degree distribution
require(igraph)
sort(degree(G))
ego(G,order=1,nodes=c(10,20,22))
## not that enlightening
ego(G,order=2,nodes=c(10,20,22))


## Community detection
fg <- fastgreedy.community(G)
subg1 <- induced_subgraph(G, which(membership(fg)==4))
summary(subg1)
plot(subg1,vertex.shape="none", vertex.label.cex=0.75, edge.color=grey(0.85),vertex.label.family="Helvetica")


## 
subg2 <- induced_subgraph(G, which(membership(fg)==4 | membership(fg)==1))
## MATEO -- ABRAHAN: 9
## PAULOS -- ZULU: 39
## PAULOS -- LWANGA: 40
subg3 <- delete.edges(subg2, c(9, 39, 40))
par(mfrow=c(1,2))
plot(subg2, vertex.shape="none", vertex.label.cex=0.5, edge.color=grey(0.85),
     vertex.label.family="Helvetica")
plot(subg3, vertex.shape="none", vertex.label.cex=0.5, edge.color=grey(0.85),
     vertex.label.family="Helvetica")

## reset subplots
par(mfrow=c(1,1))

## Plot full graph
plot(graph.full(5), vertex.color="skyblue2")


## Show a vertex with degree=4
g <- make_graph( c(1,2, 1,3, 1,4, 1,5), n=5, dir=FALSE )
mki <- c("skyblue2",rep("white",4))
lay <- matrix(c(0,0, 1,0, 0,1, -1,0, 0,-1),nr=5,nc=2,byrow = TRUE)
vf <- c("black", rep("white",4))
plot(g, vertex.label=NA, vertex.color=mki, vertex.frame.color=vf, edge.width=3, layout=lay)


## What's the degree distribution of the Tailor shop?
degree(G)


## Degree distribution of a random graph
#g <- erdos.renyi.game(1000, 1/500) #old syntax!
g <- sample_gnp(1000,1/500)
dd <- degree_distribution(g)
lendd <- length(dd)
plot((1:lendd)-1,dd, type="h", lwd=20, lend=2, col="blue", 
     xlab="Degree (k)", ylab="Probability(K=k)")


## Plot a geodesic
d <- get.diameter(G)
E(G)$color <- "SkyBlue2"
E(G)$width <- 1
E(G, path=d)$color <- "red"
E(G, path=d)$width <- 2
V(G)$labelcolor <- V(G)$color  <- "blue"
V(G)[d]$labelcolor <- V(G)[d]$color <- "red"
plot(G, vertex.shape="none", vertex.label.cex=0.5,
     edge.color=E(G)$color, edge.width=E(G)$width,
     vertex.label.color=V(G)$labelcolor,
     vertex.size=3)


## Layout for the different types of dyads
g <- graph( c( 1,2, 3,4, 5,6, 6,5), n=8)
ccc <- cbind(c(300, 100, 100, 300, 100, 300, 100, 300),  
             c(300, 300, 230, 230, 160, 160, 90, 90))
plot(g, layout=ccc, vertex.color=grey(0.75), vertex.label.family="Helvetica")


## Transitive triangle (030t)
g030t <- graph( c(1,2, 2,3, 1,3), n=3)
plot(g030t, vertex.color="skyblue2")
#tkplot(g030t)
#tk_coords(1)
tri.coords <- matrix( c(228,416, 436,0, 20,0), nr=3, nc=2, byrow=TRUE)
plot(g030t, layout=tri.coords, vertex.color="skyblue2", 
     vertex.label.family="Helvetica")


## Random graph n=vertices, m=edges
## Triad census
g <- sample_gnm(15, 45, directed = TRUE)
plot(g,edge.arrow.width=0.5, vertex.color="skyblue2", 
     vertex.label.family="Helvetica")
triad_census(g)


## Where does transitivity come from?
## define triads
DD <- graph( c(3,1, 3,2), n=3, directed=TRUE)
DS <- graph( c(3,1, 2,1), n=3, directed=TRUE)
BDID <- graph( c(3,1, 2,3), n=3, directed=TRUE)
ISDB <- graph( c(3,1, 1,2), n=3, directed=TRUE)
labs <- c("B","C","A")


## Possible agonistic interactions
par(mfrow=c(2,2))
plot(DD, layout=tri.coords,
     vertex.label=labs,
     vertex.label.family="sans",
     vertex.label.color="black",
     vertex.color="skyblue2",
     vertex.frame.color="black",
     vertex.size=50,
     edge.width=2,
     edge.arrow.width=0.5,
     edge.color=grey(0.5))
title("DD")

plot(DS, layout=tri.coords,
     vertex.label=labs,
     vertex.label.family="sans",
     vertex.label.color="black",
     vertex.color="skyblue2",
     vertex.frame.color="black",
     vertex.size=50,
     edge.width=2,
     edge.arrow.width=0.5,
     edge.color=grey(0.5))
title("DS")

plot(BDID, layout=tri.coords,
     vertex.label=labs,
     vertex.label.family="sans",
     vertex.label.color="black",
     vertex.color="skyblue2",
     vertex.frame.color="black",
     vertex.size=50,
     edge.width=2,
     edge.arrow.width=0.5,
     edge.color=grey(0.5))
title("BDID")

plot(ISDB, layout=tri.coords,
     vertex.label=labs,
     vertex.label.family="sans",
     vertex.label.color="black",
     vertex.color="skyblue2",
     vertex.frame.color="black",
     vertex.size=50,
     edge.width=2,
     edge.arrow.width=0.5,
     edge.color=grey(0.5))
title("ISDB")

par(mfrow=c(1,1))

## Structural Balance (see Evans-Pritchard 1929)
eplabs <- c("M","S","F")
EP <- graph( c(1,2, 1,3, 2,3), n=3, directed=FALSE)
plot(EP, layout=tri.coords,
     vertex.label=eplabs,
     vertex.label.family="sans",
     vertex.label.color="black",
     vertex.color="white",
     vertex.frame.color="black",
     vertex.size=25,
     edge.width=5,
     edge.color=c("red","red","green"))


## Florentine marriages
flo <- read.table("../data/flo.txt", 
                  header=TRUE, row.names=1)
gflo <- graph_from_adjacency_matrix(as.matrix(flo), mode="undirected")
gflo
plot(gflo, vertex.color="skyblue2",vertex.frame.color="skyblue2",
     vertex.label.family="Helvetica")
plot(gflo, vertex.shape="none", vertex.label.family="Helvetica")


## Is the network connected?
is.connected(gflo)
clusters(gflo)


## Find the connected component
## note difference from above: could have used induced_subgraph()
gflo1 <- decompose.graph(gflo)[[1]]
lay <- layout_with_fr(gflo1)
plot(gflo1, vertex.color="skyblue2", vertex.label=NA, layout=lay)


## Calculate density
## note we're working with the matrix, not the graph object here
n <- dim(flo)[1]
2*sum(apply(flo,1,sum)/2)/(n*(n-1))


## just the connected component
## Can just use the built-in function
n <- n-1
2*sum(apply(flo,1,sum)/2)/(n*(n-1))
edge_density(gflo1)


## write a function for information centrality
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
## infocentral needs matrix, so trim off Pucci from flo
flo1 <- flo[-12,-12]
ic <- infocentral(flo1)
CE <- abs(power_centrality(gflo1))

measures <- cbind(CD=degree(gflo1),
              CB=round(betweenness(gflo1),1),
              CC=round(closeness(gflo1),2),
              CI=round(ic,2),
              CE=round(CE,2))
dimnames(measures)[[1]] <- dimnames(flo1)[[1]]
measures
# Plot the graph one last time with the vertices sized according to
# betweenness centrality
plot(gflo1, vertex.size=measures[,"CB"]+1, vertex.color="plum", vertex.label=NA, layout=lay)


## 
## correlation matrix of the centralities
cor(measures)
## vertex size proportional to eigenvalue centrality
plot(gflo1, vertex.size=measures[,"CE"]*10, vertex.color="plum", vertex.label=NA, layout=lay)


## rescale function -- useful for plotting with betweenness! 
rescale <- function(x,a,b,c,d) c + (x-a)/(b-a)*(d-c)
bb <- rescale(measures[,2], 0, 47.5, 1, 40)
plot(gflo1, vertex.size=bb, vertex.color="plum", vertex.label=NA, layout=lay)


## Plot subgraphs
require(igraph)
set.seed(8675309)
g1 <- sample_gnm(20,60)
g2 <- sample_gnm(200,500)
gg <- g1 %du% g2

## add links across communities
s1 <- sample(1:20,5,replace=FALSE)
s2 <- sample(21:220,5,replace=FALSE)
gg <- add_edges(gg,c(rbind(s1,s2)))

## only the connected component
gg <- induced_subgraph(gg, subcomponent(gg,1))

## plotting
cols <- c(rep("blue4",20), rep("magenta",200))
ecols <- rep("#A6A6A6",565)
lay <- layout_with_fr(gg)


plot(gg, vertex.size=5,
    vertex.color=cols,
#    vertex.shape="none",
    vertex.frame.color=cols,
    vertex.label=NA,
#    vertex.label.color=cols,
    edge.width=0.5,
    edge.color=ecols,
    layout=lay)


## highlight ego's 2nd degre ego network
# highlight ego network in minority group
E(gg)$id <- seq_len(ecount(gg))
V(gg)$vid <- seq_len(vcount(gg))
ego10 <- make_ego_graph(gg,order=2,nodes=10)
ego10 <- ego10[[1]]
##
cols1 <- cols
cols1[V(ego10)$vid] <- "blue"
cols1[10] <- "cyan"
ecols1 <- ecols
ecols1[E(ego10)$id] <- "blue"

## plotting
plot(gg, vertex.size=5,
   vertex.color=cols1,
   vertex.frame.color=cols1,
   vertex.label=NA,
   edge.width=0.5,
   edge.color=ecols1,
   layout=lay) 




## highlight a random majority-group member's ego network
# highlight corresponding ego network in majority group
ego153 <- make_ego_graph(gg,order=2,nodes=153)
ego153 <- ego153[[1]]

cols2 <- cols
cols2[V(ego153)$vid] <- "blue"
cols2[153] <- "cyan"
ecols2 <- ecols
ecols2[E(ego153)$id] <- "blue"
      
plot(gg, vertex.size=5,
     vertex.color=cols2,
     vertex.frame.color=cols2,
     vertex.label=NA,
     edge.width=0.5,
     edge.color=ecols2,
     layout=lay) 

