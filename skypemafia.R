
#
# SkypeMafia social graph visualization
# created to celebrate 10th birthday of Skype on August 29th, 2013
# by Sten Tamkivi (about.me/sten)
# 
# Special thanks to Daizaburo Shizuka for his
# R SNA reference (https://sites.google.com/site/daishizuka/toolkits/sna)
#

library(igraph)

setwd("/Users/sten/Dropbox/git/SkypeMafia-visualization")

# read the Entity Type, Location, edge size attributes
attributes <- read.csv(file = "skypemafia_attributes.csv", sep=",", row.names=NULL, header = TRUE)
# attributes <- attributes[ order(Name), ]

# read the Person / Relationship Type / Company data on edges
edges <- read.csv ("skypemafia_edges.csv", sep=",", header=TRUE, check.names=FALSE)

# convert to matrix (needed for edgelist manipulation, not graph.data.frame!)
#  edgesmatrix <- as.matrix(edges)
# create a graph, without the relationship type attribute (middle column)
#  g <- graph.edgelist(edgesmatrix[,c(1,2)],directed=TRUE)
#  dump <- get.data.frame(g, what="both")

g <- graph.data.frame(edges, directed=TRUE, vertices=attributes)

# map edge properties (color & weight) based on Role
E(g)[ Role == 1 ]$color <- "red"       # founder
E(g)[ Role == 2 ]$color <- "grey"      # employee
E(g)[ Role == 3 ]$color <- "darkgreen" # investor/advisor

E(g)[ Role == 1 ]$width <- 1.5           # founder
E(g)[ Role == 2 ]$width <- 1           # employee
E(g)[ Role == 3 ]$width <- 1.2          # investor/advisor

# make edges curved
# E(g)$curved <- autocurve.edges(g)
E(g)$curved <- 0.15

# copy attributes to respective vertexes
V(g)$label  <- V(g)$name
V(g)$size   <- attributes$Size
V(g)$type   <- as.character(attributes$Type)

V(g)[ type == "Startup" ]$color <- "red"
V(g)[ type == "Person" ]$color   <- "black"
V(g)[ type == "Company" ]$color  <- "darkred"
V(g)[ type == "VC" ]$color       <- "darkgreen"
V(g)[ type == "Founder" ]$color   <- "blue"
V(g)[ type == "Skype" ]$color     <- "blue"

# adjust label alignment for different sized vertices
V(g)$label.dist <- 0.15
V(g)[ type == "Person" ]$label.dist <- 0.05

l <- layout.fruchterman.reingold(g,niter=500,area=vcount(g)^2.3,repulserad=vcount(g)^2.8)
# l <- layout.fruchterman.reingold.grid(g)
# l <- layout.graphopt(g)


####
#### Plot everything
####

pdf("skypemafia - full plot.pdf", family="sans")
# png(file="skypemafia plot.png", width=2000,height=2000)

plot.igraph(g, layout=l,
            vertex.label.color="black", vertex.label.degree=pi/2, vertex.label.family="sans",
            vertex.label.cex=0.3,
            vertex.frame.color="white", vertex.size=V(g)$size/5,
            edge.arrow.size=0.1, main="#SkypeMafia 2013")

# close the graphics output device
dev.off()

####
#### Plot the money flows around Skype
####

g_invest <- g
g_invest <- g_invest - edges(E(g_invest)[Role==2])
g_invest <- g_invest - edges(E(g_invest)[Role==1])
g_invest <- g_invest - vertices(V(g_invest)[Type=="Company"])
g_invest <- g_invest - path("Skype")

g_invest <- g_invest - vertices(degree(g_invest)==0 ) # remove isolates


pdf("skypemafia - money plot.pdf", family="sans")

plot.igraph(g_invest, layout=layout.fruchterman.reingold.grid(g_invest),
            vertex.label.color="black", vertex.label.degree=pi/2, vertex.label.family="sans",
            vertex.label.cex=0.3,
            vertex.frame.color="white", vertex.size=V(g_invest)$size/5,
            edge.arrow.size=0.1, main="#SkypeMafia Money Flow")

# close the graphics output device
dev.off()


####
#### Plot the founder activity around Skype
####

g_founder <- g

g_founder <- g_founder - edges(E(g_founder)[Role==2 & !to("Skype")])  # no work relationships outside Skype
g_founder <- g_founder - edges(E(g_founder)[Role==3])                 # no investing/advising
g_founder <- g_founder - vertices(V(g_founder)["Bill Draper"])  # to hide a father->son "founding event" :)
g_founder <- g_founder - vertices(V(g_founder)[Type=="VC"])
g_founder <- g_founder - vertices(V(g_founder)[Type=="Company"])
# g_founder <- g_founder - path("Skype")

g_founder <- g_founder - vertices(degree(g_founder)==0 ) # remove isolates
g_founder <- g_founder - vertices(degree(g_founder, mode="out")==1) # remove non-wounder employees (no other outbound connections but Skype)

pdf("skypemafia - founder plot.pdf", family="sans")

plot.igraph(g_founder, layout=layout.fruchterman.reingold.grid(g_founder),
            vertex.label.color="black", vertex.label.degree=pi/2, vertex.label.family="sans",
            vertex.label.cex=0.3,
            vertex.frame.color="white", vertex.size=V(g_founder)$size/5,
            edge.arrow.size=0.1, main="#SkypeMafia Founders")

# close the graphics output device
dev.off()