
require(igraph)
## make a simole graph
g <- make_graph( c(1,2, 1,3, 2,3, 2,4, 3,5, 4,5), n=5, dir=FALSE )
plot(g, vertex.color="skyblue2")


## graph from literal
g <- graph_from_literal(Fred-Daphne:Velma-Shaggy, Fred-Shaggy-Scooby)
plot(g, vertex.shape="none", vertex.label.color="black")


## Special graphs: empty, full, ring
# empty graph
g0 <- make_empty_graph(20)
plot(g0, vertex.color="skyblue2", vertex.size=10, vertex.label=NA)
# full graph
g1 <- make_full_graph(20)
plot(g1, vertex.color="skyblue2", vertex.size=10, vertex.label=NA)
# ring
g2 <- make_ring(20)
plot(g2, vertex.color="skyblue2", vertex.size=10, vertex.label=NA)


## Special graphs: lattice, tree, star
# lattice
g3 <- make_lattice(dimvector=c(10,10))
plot(g3, vertex.color="skyblue2", vertex.size=10, vertex.label=NA)
# tree
g4 <- make_tree(20, children = 3, mode = "undirected")
plot(g4, vertex.color="skyblue2", vertex.size=10, vertex.label=NA)
# star
g5 <- make_star(20, mode="undirected")
plot(g5, vertex.color="skyblue2", vertex.size=10, vertex.label=NA)


## Random graphs
# Erdos-Renyi Random Graph
g6 <- sample_gnm(n=100,m=50)
plot(g6, vertex.color="skyblue2", vertex.size=5, vertex.label=NA)
# Power Law
g7 <- sample_pa(n=100, power=1.5, m=1,  directed=FALSE)
plot(g7, vertex.color="skyblue2", vertex.size=5, vertex.label=NA)


## Combining graphs: Disjoint union
plot(g4 %du% g7, vertex.color="skyblue2", vertex.size=5, vertex.label=NA)


## Rewiring graphs
gg <- g4 %du% g7
gg <- rewire(gg, each_edge(prob = 0.3))
plot(gg, vertex.color="skyblue2", vertex.size=5, vertex.label=NA)
## retain only the connected component
gg <- induced_subgraph(gg, subcomponent(gg,1))
plot(gg, vertex.color="skyblue2", vertex.size=5, vertex.label=NA)


## Vertex & edge attributes
## look at the structure
g4
V(g4)$name <- LETTERS[1:20]
## see how it's changed
g4
## see what I did there?
## hexadecimal color codes
V(g4)$vertex.color <- "#4503fc"
E(g4)$edge.color <- "#abed8e"
g4
plot(g4, vertex.size=15, vertex.label=NA, vertex.color=V(g4)$vertex.color, 
     vertex.frame.color=V(g4)$vertex.color,
     edge.color=E(g4)$edge.color, edge.width=3)


## Adjacency matrices
kids <- as.matrix(
  read.table("data/strayer_strayer1976-fig2.txt",
                             header=FALSE)
  )
kid.names <- c("Ro","Ss","Br","If","Td","Sd","Pe","Ir","Cs","Ka",
                "Ch","Ty","Gl","Sa", "Me","Ju","Sh")
colnames(kids) <- kid.names
rownames(kids) <- kid.names
g <- graph_from_adjacency_matrix(kids, mode="directed", weighted=TRUE)
lay <- layout_with_fr(g)
plot(g, layout=lay, edge.arrow.size=0.5,
     vertex.color="skyblue2", vertex.label.family="Helvetica", 
     vertex.frame.color="skyblue2")


## Community structure
A <- as.matrix(
  read.table(file="data/kapferer-tailorshop1.txt", 
             header=TRUE, row.names=1)
  )
G <- graph.adjacency(A, mode="undirected", diag=FALSE)
fg <- fastgreedy.community(G)
cols <- c("blue","red","black","magenta")
plot(G, vertex.shape="none",
     vertex.label.cex=0.75, edge.color=grey(0.85), 
     edge.width=1, vertex.label.color=cols[fg$membership],
     vertex.label.family="Helvetica")
# another approach to visualizing
plot(fg,G,vertex.label=NA)


## Specifying graph layouts
g <- graph( c(1,2, 2,3, 1,3), n=3, dir=FALSE)
plot(g, 
     vertex.color="skyblue2", 
     vertex.frame.color="skyblue2", vertex.label.family="Helvetica")
#tkplot(g)
#tkplot.getcoords(1)
### do some stuff with tkplot() and get coords which we call tri.coords
## tkplot(g)
## tkplot.getcoords(1) ## the plot id may be different depending on how many times you've called tkplot()
##     [,1] [,2]
##[1,]  228  416
##[2,]  436    0
##[3,]   20    0
tri.coords <- matrix( c(228,416, 436,0, 20,0), nr=3, nc=2, byrow=TRUE)
par(mfrow=c(1,2))
plot(g, vertex.color="skyblue2",
     vertex.frame.color="skyblue2", 
     vertex.label.family="Helvetica")
plot(g, layout=tri.coords, 
     vertex.color="skyblue2", 
     vertex.frame.color="skyblue2", vertex.label.family="Helvetica")


## A lattice should be a lattice
plot(g3, vertex.color="skyblue2", 
     layout=layout_on_grid(g3,10,10), vertex.size=10, vertex.label=NA)


## Plotting affiliation graphs
davismat <- as.matrix(
  read.table(file="data/davismat.txt", 
            row.names=1, header=TRUE)
  )
southern <- graph_from_incidence_matrix(davismat) 
V(southern)$shape <- c(rep("circle",18), rep("square",14))
V(southern)$color <- c(rep("blue",18), rep("red", 14))
plot(southern, layout=layout.bipartite)
## not so beautiful
## did some tinkering using tkplot()...
x <- c(rep(23,18), rep(433,14))
y <- c(44.32432,   0.00000, 132.97297,  77.56757,  22.16216, 110.81081, 155.13514,
       199.45946, 177.29730, 243.78378, 332.43243, 410.00000, 387.83784, 354.59459,
       310.27027, 221.62162, 265.94595, 288.10811,   0.00000,  22.16216,  44.32432,
       66.48649,  88.64865, 132.97297, 166.21622, 199.45946, 277.02703, 365.67568,
       310.27027, 343.51351, 387.83784, 410.00000)
southern.layout <- cbind(x,y)
plot(southern, layout=southern.layout, vertex.label.family="Helvetica")


## Sociomatrix from incidence matrix
#Sociomatrix
(f2f <- davismat %*% t(davismat))
gf2f <- graph_from_adjacency_matrix(f2f, mode="undirected", diag=FALSE, add.rownames=TRUE)
gf2f <- simplify(gf2f)
plot(gf2f, vertex.color="skyblue2",vertex.label.family="Helvetica")
## who is the most central?
cb <- betweenness(gf2f)
#plot(gf2f,vertex.size=cb*10, vertex.color="skyblue2")
plot(gf2f,vertex.label.cex=1+cb/2, vertex.shape="none",vertex.label.family="Helvetica")


## Event matrix from incidence matrix
### this gives you the number of women at each event (diagonal) or mutually at 2 events
(e2e <- t(davismat) %*% davismat)
ge2e <- graph_from_adjacency_matrix(e2e, mode="undirected", diag=FALSE, add.rownames=TRUE)
ge2e <- simplify(ge2e)
plot(ge2e, vertex.size=20, vertex.color="skyblue2",vertex.label.family="Helvetica")

