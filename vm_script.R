library(sna)
library(statnet)
library(dplyr)
library(reshape2)

# IN VIRTUAL MACHINE

# --- network package ----

## READING IN DATA ----

# If you are reading in an edgelist (two columns), add the matrix.type argument
edge.list <- read.csv("data/6.18.ASTAretweetandmention.edgelist.recent.csv", header = TRUE, na.strings = "NA")
edge.list <- edge.list %>% select(Source, Target, date, stripped_text, climate_tweet, breeding_tweet)

net.edgelist.large <- network(edge.list, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "text", "climate_tweet", "breeding_tweet"))

# Because this is a massive edgelist, I am going to randomly take 10000 of them
edge.list.short <- edge.list[sample(nrow(edge.list), 10000), ]
## Much smaller network
net.edgelist.short <- network(edge.attr.short, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "text", "climate_tweet", "breeding_tweet"))

# Import nodelist, and make sure node names match edge sides
node.info <- read.csv("data/6.18.ASTAretweetandmention.nodelist.recent.csv", header = TRUE, as.is = TRUE)
## For the sake of these data I am going to make sure I am carrying only unique nodes
nodes.info <- unique(node.info[c("Id", "Label")])

# For this sample, because I made the sample smaller
node.info <- edge.list.short %>% select(Target, Source) %>%  mutate(row = 1:nrow(edge.attr.abbr)) %>% melt(id.vars = "row", var = "name") %>% select(value) %>% unique() %>% rename("name" = "value")

# DESCRIPTIVES ----
# Different routines for matrices
net <- net.edgelist.large
## Whole network routines
gden(net)  # Density: 2.851148e-05 for large
connectedness(net) # Connectedness: 0.9981421 for large
gtrans(net)
#clique.census(net) -- counts up the cliques, but this is a long output

## Individual vertex routines
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

# Future things in sna
# block.model
# cug.test
# kcores 
# netlm: DV is a matrix
# netlogit: DV is dichotomous
# qaptest
# There is a plotting function for almost any advanced analysis

## COMMUNITIES?
## Community routines
ec <- equiv.clust(net) # Heirarchical clustering of network positions -- this took too long to see it through. Unclear what was happening, of if you could specify k clusters...
#plot(ec)                        # Shows dendrogram
#rect.hclust(ec$cluster,h=15)    # If we cut it at dist=15, what would happen?
#rect.hclust(ec$cluster,k=3,border="blue")     # Split into 3 positions; what's the best cut?

# ---- igraph ----
library(igraph)

## READING in Data
# Data from an edgelist can be read in, and converted into a network object, with node attributes, all in one function
edges <- net.edgelist.short # This is an edgelist with attributes
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
V(net)$label <- unname(ifelse(degree(net)[V(net)] > 50, names(V(net)), "")) 
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
plot(clp, net)



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
