
require(igraph)
library(knitr)

## construct a small edgelist
p1 <- c("Harry", "Harry", "Hermione")
p2 <- c("Hermione", "Ron", "Ron")
values <- c(5,6,5)

schoolmodel <- data.frame(p1, p2, values)
colnames(schoolmodel) <- c("Ego", "Alter", "Value")
## kable together a table
knitr::kable(schoolmodel)

## Anotha one
From <- c("Luna Lovegood", "Hermione Granger", "Harry James Potter") 
To <- c("Harry James Potter", "Harry James Potter", "Cho Chang")
Chapter <- c(10, 15, 18)
Page <- c(180, 292, 350)
supegs <- data.frame(From, To, Chapter, Page)
knitr::kable(supegs)


## load an existing edgelist
require(igraph)
hp5edges <- read.table("data/hp5edgelist.txt", sep="\t", header=TRUE)

#take a look
head(hp5edges)

#convert to an igraph network
hp5edgesmat <- as.matrix(hp5edges) #igraph wants our data in matrix format
hp5net <- graph_from_edgelist(hp5edgesmat, directed=TRUE)

#extract first names from list of names to make nicer labels. 
##FYI this is insane R notation to extract elements from a list, fear not if you don't get it
firsts <- unlist(lapply(strsplit(V(hp5net)$name,  " "), '[[', 1))

#let's take a look
plot(hp5net, vertex.shape="none", vertex.label.cex=0.6, edge.arrow.size=0.4, 
     vertex.label=firsts, layout=layout.kamada.kawai)

## simplify it

hp5netsimple <- simplify(hp5net)
plot(hp5netsimple, vertex.shape="none",vertex.label.cex=0.7, edge.arrow.size=0.4, 
     vertex.label=firsts, layout=layout.kamada.kawai)


## adding in the isolates (they matter for social structure too!)
book5isolates <- c("Lavender Brown", "Millicent Bulstrode", "Michael Corner", 
                   "Roger Davies", "Theodore Nott", "Zacharias Smith")

hp5netfull <- add_vertices(hp5netsimple, nv=length(book5isolates), attr=list(name=book5isolates))
#note that here we add as many vertices as in our list of isolates, and assign them the attribute "name" which is stored in our list of isolates

#regenerate our list of first names to update it to include these new characters
firsts <- unlist(lapply(strsplit(V(hp5netfull)$name,  " "), '[[', 1))

plot(hp5netfull, vertex.shape="none", vertex.label.cex=0.6, edge.arrow.size=0.4, 
     vertex.label=firsts, layout=layout.kamada.kawai)


## Removing duplicate ties
hp5newedges <- data.frame(rbind(hp5edges, c("Harry James Potter", "Ronald Weasley"), 
                                c("Ronald Weasley", "Harry James Potter"), 
                                c("Hermione Granger", "Ronald Weasley"), 
                                c("Hermione Granger", "Harry James Potter"), 
                                c("Hermione Granger", "Harry James Potter")))

#this takes a list of ones and takes the sum of them for each unique row in the edgelist
hp5edgeweights <- aggregate(list(count=rep(1,nrow(hp5newedges))), hp5newedges, FUN=sum)

#take a look at the edges that appear more than once
hp5edgeweights[which(hp5edgeweights$count>1),]

#create a network and assign the counts as an edge weight
hp5netweight <- graph_from_edgelist(as.matrix(hp5edgeweights[,1:2]), directed=TRUE)
E(hp5netweight)$weight <- hp5edgeweights$count


## loading individual attributes
attributes <- read.csv("data/hpindividuals.csv", header=TRUE)
head(attributes)


## Only include attributes of book-5 characters
book5students <- attributes[attributes$name %in% V(hp5netfull)$name,]

#then reorder the attributes to match the order that the individuals appear as network vertices
book5students <- book5students[match(V(hp5netfull)$name, book5students$name), ]

#now we can simply assign the dataframe columns as vertex attributes, e.g., 
V(hp5netfull)$house <- book5students$house

#but let's use a factor to get nice colours
housecolours <- c("firebrick", "darkgoldenrod", "darkslateblue", "darkgreen")
V(hp5netfull)$housecolours <-as.character(factor(book5students$house, levels=1:4, labels=housecolours))

plot(hp5netfull, vertex.shape="none", vertex.label.color=V(hp5netfull)$housecolours, vertex.label.cex=0.6, edge.arrow.size=0.3, vertex.label=firsts, layout=layout.kamada.kawai)



#read in data
#note that this file has no row or column names
hp5df <- read.table("data/hp5matrix.txt", sep="\t", header=FALSE) 

#inspect it
head(hp5df)
dim(hp5df)



#read in data
hp5df <- read.table("data/hp5matrix.txt", sep=" ", header=FALSE) 

#inspect
dim(hp5df) #much better




#start by adding row and column names to our sociomatrix
#we know from context that the students in this sociomatrix are ordered by ID number
studentsIDs <- sort(attributes$id) #get an ordered list of students IDS
colnames(hp5df) <- rownames(hp5df) <- studentsIDs #assign them as row/col names of the sociomatrix

#extract out the students based on our list of individuals from Book 5
hp5only <- hp5df[book5students$id, book5students$id]
#note that this operation conveniently put the students in the same order as our attributes table from before

#now we can make our network
hp5mat <- as.matrix(hp5only)
hp5net2 <- graph_from_adjacency_matrix(hp5mat)

#get first names again
firsts2 <- unlist(lapply(strsplit(as.character(book5students$name),  " "), '[[', 1))

plot(hp5net2, vertex.shape="none", vertex.label.cex=0.6, edge.arrow.size=0.3, 
     vertex.label=firsts2, layout=layout.kamada.kawai)


## Bipartite graphs
davismat <- as.matrix(read.table("data/davismat.txt",header=TRUE))
southern <- graph_from_incidence_matrix(davismat)
southern
V(southern)$type
V(southern)$shape <- c(rep("circle",18), rep("square",14))   
V(southern)$color <- c(rep("blue",18), rep("red", 14))
plot(southern, layout=layout.bipartite)



#Sociomatrix from incidence matrix
(f2f <- davismat %*% t(davismat))
gf2f <- graph_from_adjacency_matrix(f2f, mode="undirected", diag=FALSE, add.rownames=TRUE)
gf2f <- simplify(gf2f)
plot(gf2f, vertex.color="skyblue2")


# calculate betweenness centrality and scale vertices to this
cb <- igraph::betweenness(gf2f)
# simple function to rescale values in range [a b] to [c d]
# this is very useful for scaling vertex size for visualization
rescale <- function(x,a,b,c,d) c + (x-a)/(b-a)*(d-c)
plot(gf2f,vertex.size=rescale(cb,0, 1.38, 5, 20), vertex.color="skyblue2")


# Event matrix from incidence matrix
### this gives you the number of women at each event (diagonal) or mutually at 2 events
(e2e <- t(davismat) %*% davismat)
ge2e <- graph_from_adjacency_matrix(e2e, mode="undirected", diag=FALSE, add.rownames=TRUE)
ge2e <- simplify(ge2e)
plot(ge2e, vertex.color="skyblue2")


## A function that replicates some Matlab functionality for visualize sparse matrices
spyR <- function(X, pixcol=c("white","black"), xl="",yl="",
                 draw.labs=FALSE,labs=NA,box=TRUE,las=2){
        s <- dim(X)
        x <- 1:s[1]
        y <- 1:s[2]
        yr <- rev(y)
        Xr <- t(X)[,yr]
        image(x,y,Xr,col=pixcol,axes=FALSE,xlab=xl,ylab=yl,asp=NA)
        if(draw.labs){
                axis(1,at=labs,labels=labs,tick=FALSE)
                axis(3,at=labs,labels=rev(labs),tick=FALSE)
        }
        if(box) box()
}


## Nested incidence matrix
nest <- matrix( c(1,1,1,1,1,
                  1,1,1,1,0,
                  1,1,1,0,0,
                  1,0,0,0,0,
                  1,0,0,0,0), nr=5, nc=5, byrow=TRUE)
dimnames(nest)[[1]] <- c("1","2","3","4","5")
dimnames(nest)[[2]] <- c("A","B","C","D","E")

spyR(nest, pixcol=c("white","blue4"))
axis(3,at=1:5,labels=LETTERS[1:5],tick=FALSE)
axis(2,at=1:5,labels=5:1,tick=FALSE, las=2)


## Bipartite graph
gnest <- graph_from_incidence_matrix(nest,mode="all")
V(gnest)$vertex.color <- c(rep("cyan",5), rep("magenta",5))
plot(gnest, layout=layout_as_bipartite(gnest), 
     vertex.color=V(gnest)$vertex.color, vertex.size=20)



## Projected "sociomatrix"
nums <- nest %*% t(nest)

gnums <- graph_from_adjacency_matrix(nums, mode='undirected', diag=FALSE)
gnums <- simplify(gnums)
plot(gnums, vertex.color="cyan", vertex.size=20)


## Modular incidence matrix
mod  <- matrix( c(0,0,0,0,0,0,0,1,1,1,
                  0,0,0,0,0,0,0,1,1,1,
                  0,0,0,0,0,0,0,1,1,1,
                  0,0,0,0,0,1,1,0,0,0,
                  0,0,0,0,0,1,1,0,0,0,
                  1,1,1,1,1,0,0,0,0,0,
                  1,1,1,1,1,0,0,0,0,0,
                  1,1,1,1,1,0,0,0,0,0,
                  1,1,1,1,1,0,0,0,0,0,
                  1,1,1,1,1,0,0,0,0,0), nr=10, nc=10, byrow=TRUE)

spyR(mod, pixcol=c("white","blue4"))
axis(3,at=1:10,labels=LETTERS[1:10],tick=FALSE)
axis(2,at=1:10,labels=10:1,tick=FALSE, las=2)


## Bipartite graph
gmod <- graph_from_incidence_matrix(mod,mode="all")
V(gmod)$vertex.color <- c(rep("cyan",5), rep("magenta",5))
plot(gmod, layout=layout_as_bipartite(gmod), 
     vertex.color=V(gmod)$vertex.color, vertex.size=20)



## Projected "sociomatrix"
nums1 <- mod %*% t(mod)

gnums1 <- graph_from_adjacency_matrix(nums1, mode='undirected', diag=FALSE)
gnums1 <- simplify(gnums1)
plot(gnums1, vertex.color="cyan", vertex.size=20)


## Reading a network from a data frame
csel <- read.table("data/colorado_springs/edges.tsv",header=TRUE)
csppl <- read.table("data/colorado_springs/nodes.tsv", header=TRUE)
cs <- graph_from_data_frame(csel,vertices=csppl,directed=FALSE)
cs <- simplify(cs)
## extract just the giant component to plot
cs1 <- induced_subgraph(cs, subcomponent(cs,1))
plot(cs1, vertex.color="lightblue", vertex.size=2, vertex.label=NA, layout=layout_with_fr(cs1))

