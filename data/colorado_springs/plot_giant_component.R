setwd("/Users/jhj1/Teaching/social_networks/data/colorado_springs/")
csedges <- read.table("edges.tsv", header=TRUE)

csnet <- graph_from_edgelist(as.matrix(csedges), directed=FALSE)
csnet <- simplify(csnet)
csnet1 <- induced.subgraph(csnet, subcomponent(csnet,1))
lay <- layout_with_fr(csnet1)
lay1 <- layout_with_kk(csnet1)

pdf(file="cs-highrisk.pdf", height=21, width=21)
plot(csnet1, vertex.size=1, vertex.label=NA, 
     vertex.color="lightblue", layout=lay)
dev.off()

pdf(file="cs-highrisk-kk.pdf", height=21, width=21)
plot(csnet1, vertex.size=1, vertex.label=NA, 
     vertex.color="lightblue", 
     edge.color=rgb(200,200,200,100, maxColorValue = 255),
        layout=lay1)
dev.off()