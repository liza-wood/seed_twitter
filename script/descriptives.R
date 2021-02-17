# SESYNC NOTES
## Here I am working with the almost full (2018-2020) network, without any weights. I could potentially get rid of specific tweets, and instead just weight them by retweet/mention interaction in total -- I think this will come for the analysis of the network generally, perhaps. Less about climate change...
## So this network will be about the whole network, where I can get general descriptives, not thinking about what they are saying but about who they are near

library(sna)
library(statnet)
library(dplyr)
library(reshape2)


# --- network package ----

## READING IN DATA ----

# EDGES/VERTICES 

# If your vetices are already in matrix form, you can start with a matrix of your edges
#mat <- matrixcsv
# Can make it a network object with just the function network(), and specify directed = T/F (in the network package)
#net <- network(mat)
## Can also use as.network.matrix() and you can specify the matrix type: "adjacency", "edgelist", "incidence" 

# If you are reading in an edgelist (two columns), add the matrix.type argument
edge.list <- read.csv("data/6.18.ASTAretweetandmention.edgelist.recent.csv", header = TRUE, na.strings = "NA")
edge.list <- edge.list %>% select(Source, Target, date, stripped_text, climate_tweet, breeding_tweet)

net.edgelist.large <- network(edge.list, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "text", "climate_tweet", "breeding_tweet"))

# Because this is a massive edgelist, I am going to randomly take 10000 of them
edge.list.short <- edge.list[sample(nrow(edge.list), 10000), ]
## Much smaller network
net.edgelist.short <- network(edge.attr.short, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "text", "climate_tweet", "breeding_tweet"))

## Generally, network objects looks like a list in the global environment (typeof is list) but if you explore it is a network object (class is network)
## Note: you don't need to it be a network object unless you are going to be doing ERGMs

# NODES 

# Import nodelist, and make sure node names match edge sides
node.info <- read.csv("data/6.18.ASTAretweetandmention.nodelist.recent.csv", header = TRUE, as.is = TRUE)
## For the sake of these data I am going to make sure I am carrying only unique nodes
nodes.info <- unique(node.info[c("Id", "Label")])

# For this sample, because I made the sample smaller
node.info.short <- edge.list.short %>% select(Target, Source) %>%  mutate(row = 1:nrow(edge.attr.abbr)) %>% melt(id.vars = "row", var = "name") %>% select(value) %>% unique() %>% rename("name" = "value")

## PLOTTING FOR A QUICK VIEW ----

# Rename just to make this easier
net <- net.edgelist.short

# Simply, you can just plot to see default
plot(net)
## Can add displaylabels=T, and mode = "" to specify what shapes/algorithms 

# You can also use various calls, such as coloring attibutes -- 
table(net%e%"climate_tweet")
edgeColors <- ifelse(net%e%"climate_tweet" == "TRUE", "hotpink", "dodgerblue")

plot(net, 
     displaylabels = F,
     edge.col = edgeColors,
     edge.lwd = 0.2,
     vertex.col = "grey",
     vertex.cex = 0.5)
#edge.lwd = net.edgelist.weights %e% "weight"*.5)
#edge.lwd means edge line width

## DESCRIPTIVES ----

# Want to check this out for the whole network
net <- net.edgelist.large

summary(net) # Get an overall summary
network.dyadcount(net) # How many dyads?
network.edgecount(net) # How many edges are present?
network.size(net)  # How large is the network?
as.sociomatrix(net) # Show it as a sociomatrix
net[,] # Another way to do it

# Can add node attributes this way

#net %v% "id" <- node.info$id # Add in our vertex attributes
#net %v% "type" <- node.info$type
#net %v% "location" <- node.info$location

# Listing attributes
list.vertex.attributes(net) # List all vertex attributes
list.network.attributes(net) # List all network attributes

#Retrieving attributes
#net %v% "type" # Retrieve vertex ages
#net %v% "location" # Retrieve vertex ids


# --- SNA package ----
# DESCRIPTIVES ----
# Different routines for matrices
net <- net.edgelist.large
## Whole network routines
gden(net)  # Density: 2.851148e-05 for large
connectedness(net) # Connectedness: 0.9981421 for large
gtrans(net)
#clique.census(net) -- counts up the cliques, but this is a long output

## Individual vertex routines
net <- net.edgelist.short
?degree
degree(net) -> deg #default is total, also called "freeman"
degree(net, cmode="indegree") -> ideg # in degree
degree(net, cmode="outdegree") -> odeg # out degree
betweenness(net) -> bw # Betweenness centrality for all nodes
closeness(net) -> close # closeness centrality
evcent(net) -> evc # Eigenvector centrality

## To explore more in sna
# Can explore more in the gclust... functions in sna, but leaving it for now
# gcov and gcor find covariation and correlations between graphs

# PLOTTING ----
## Can plot also with sna
gplot(net) # Requires sna
gplot(mat) # gplot Will work with a matrix object too

#Let's color the nodes in sex-stereotypic colors
edgeColors <- ifelse(net%e%"climate_tweet" == "TRUE", "hotpink", "dodgerblue")

gplot(net,
      gmode = "digraph",
      displaylabels = F,
      usearrows= F,
      edge.col = edgeColors,
      edge.lwd = 0.2,
      vertex.col = "grey",
      vertex.cex = deg/10,
      displayisolates=FALSE)

# usearrows = T/F
# gmode = is "graph" for not directional, or default to direction "digraph"
# mode is for layout: eg. fruchtermanreingold, eigen, spring, target, circle, adj, mds

#We can add labels to the vertices -- labels are just their name in the network, not from an attribute

#Other ways to specify the labeling
gplot(net,
      gmode = "graph",
      label = network.vertex.names(net),
      displaylabels = F,
      usearrows= F,
      edge.col = edgeColors,
      edge.lwd = 0.2)

#Here's an example of directed data|militarized interstate disputes (MIDs) for 1993 (diff network)
#By not including gmode = "graph", the defailt is "digraph" which assumed directedness


#When a layout is generated, the results can be saved for later reuse:
coords <- gplot(net,
                gmode="graph",
                displaylabels = F,
                usearrows= F,
                edge.col = edgeColors,
                edge.lwd = 0.2) # Capture the magic of the moment

coords # Show the vertex coordinates

#Saved (or a priori) layouts can be used via the coord argument

gplot(net,
      gmode="graph",
      displaylabels = F,
      usearrows= F,
      edge.col = edgeColors,
      edge.lwd = 0.2,
      coord = coords)

#When the default settings are insuficient, interactive mode allows for tweaking
coords <- gplot(net, 
                displaylabels = F,
                usearrows= F,
                edge.col = edgeColors,
                edge.lwd = 0.2,
                interactive = TRUE) # Modify and save

gplot(net, 
      displaylabels = F,
      usearrows= F,
      edge.col = edgeColors,
      edge.lwd = 0.2,
      coord = coords) # Modify and save



# Future things in sna
# block.model
# cug.test
# kcores 
# netlm: DV is a matrix
# netlogit: DV is dichotomous
# qaptest
# There is a plotting function for almost any advanced analysis

# EXPLORING DESCRIPTIVES ----

# Scatter plot of in and out degree
plot(ideg, 
     odeg, 
     xlab="Being tweeted or mentioned", 
     ylab="Re-tweeting") # Plot ideg by odeg
abline(0, 1, lty=3) # This is a line where y=x, so if you are above the line, the retweet more than you are tweeted or mentioned


text(jitter(ideg), 
     jitter(odeg), 
     network.vertex.names(net), 
     cex=0.75, 
     col=2)

## COMMUNITIES?
## Community routines
#ec <- equiv.clust(net) # Heirarchical clustering of network positions -- this took too long to see it through. Unclear what was happening, of if you could specify k clusters...
plot(ec)                        # Shows dendrogram
rect.hclust(ec$cluster,h=15)    # If we cut it at dist=15, what would happen?
rect.hclust(ec$cluster,k=3,border="blue")     # Split into 3 positions; what's the best cut?

# ---- igraph ----
library(igraph)

## READING in Data
# Data from an edgelist can be read in, and converted into a network object, with node attributes, all in one function
edges <- edge.list # This is an edgelist with attributes
nodes <- node.info
head(edges)

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T)  

# Exploring vertices
E(net)
E(net)$climate_tweet
# The methods used in sna do not work for igraph
net %e% "climate_tweet"
V(net)

# Set attributes before plotting ----

# Edge attributes
E(net)$color <- ifelse(E(net)$climate_tweet == TRUE, "blue", "gray50")
E(net)$width <- .5
# Set edge width based on weight:
#E(net)$width <- E(net)$weight/6
#E(net)$edge.color <- "gray80"
#colrs <- c("gray50", "tomato", "gold")
#V(net)$color <- colrs[V(net)$media.type]

# Vertex attributes
V(net)$color="gray50"
deg <- degree(net, mode="all")
V(net)$size <- deg/6

# Labels
# The labels are currently node IDs, Setting them to NA will render no labels:
#V(net)$label <- NA
# Or set only some labels
V(net)$label <- unname(ifelse(degree(net)[V(net)] > 30, names(V(net)), "")) 
V(net)$label.color="black"

# Arrows
E(net)$arrow.size <- .02

# Plotting in igraph
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net, displayisolates=FALSE)

# SUBSETTING A NETWORK
# Trying to reduce side -- removing those with less than 5 degrees
v.remove <- V(net)[which(degree(net) < 5)]
v.remove <- V(net)[which(degree(net, mode="in") < 4 & degree(net, mode="out") < 4)]

net2 <- delete.vertices(net, v.remove)
#graph_attr(net2, "layout") <- layout_with_lgl
plot(net2, displayisolates=FALSE)


# COMMUNITIES
clp <- cluster_optimal(net2)
class(clp)
plot(clp, net2)

# We can also plot the communities without relying on their built-in plot:
V(net2)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net2, vertex.color=colrs[V(net2)$community])

# LATER NOTES

#Setting edge values -- this is about weighting and I am going to leave it below for now
# minute 21:13 in the network intro video for review -- this seems super important for me
m<-nrelations[,] # copy over the relational structure
m[upper.tri(m)>0]<-rep(1:3,times=3) # give different edge values
library(sna)
m<-symmetrize(m,rule="upper") # This has to do with directionality
m
# SO we took the old adjacency matrix, and what we did above was to add weights. Separating out these netowkrs (nrelations and m), we can work with the same network.So if we have one with binary (yes they are in my network), versus saying their name multiple times. But how can you have weights but no tie. For example, if you aren't collaborating but there is an attribute that might make them close, such as geographic proximity. So that is why we need edges and edgeweight separately.
nrelations %e% "strength" <- m # Add the valued ties back in

#Retrieving edge values
list.edge.attributes(nrelations) # See whats available
nrelations %e% "strength" # Use the %e% operator
as.sociomatrix(nrelations,attrname="strength") # Can also do it this way
nrelations[,] # Original tie structure is preserved
