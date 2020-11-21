library(data.table)
library(igraph)
library(sna) 
library(RColorBrewer)
library(dplyr)

# 5. Plotting
edges <- fread("~/Box/seed_twitter/data/10.31.ASTAretweetandmention.edgelist.recent.ag.cw.csv")
nodes <- fread("~/Box/seed_twitter/data/10.31.ASTAnode.names.ag.cw.bio.csv")
#nodes <- fread("~/Box/seed_twitter/data/10.31.ASTAnode.names.ag.cw.bio.id.csv")

nodes$community <- ifelse(nodes$community == 3, 1,
                   ifelse(nodes$community == 5, 2,
                   ifelse(nodes$community == 8, 3,
                   ifelse(nodes$community == 9, 4,
                   ifelse(nodes$community == 11, 5,
                   ifelse(nodes$community == 15, 6,
                   ifelse(nodes$community == 19, 7,
                   ifelse(nodes$community == 29, 8, NA))))))))

# Who are the higher degree nodes? ----
#net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T) 
#V(net)$degree <- igraph::degree(net, mode = "all")
#nodelist <- igraph::as_data_frame(net, what = "vertices")

# EXPORT TO GEPHI ----
Gephi.edges <- edges
Gephi.nodes <- nodes

colnames(Gephi.edges)[1:2] <- c("Source", "Target")
Gephi.nodes$Label <- Gephi.nodes$screen_name
colnames(Gephi.nodes)[1] <- "Id"
Gephi.nodes <- select(Gephi.nodes, Id, Label, name, location, community)

fwrite(Gephi.edges, "~/Box/seed_twitter/data/11.3.Gephi.edges.csv")
fwrite(Gephi.nodes, "~/Box/seed_twitter/data/11.3.Gephi.nodes.csv")

# Count up


# COMMUNITY EDGE AND NOSELIST CREATION ----

for(i in 5){
   nodes.c <- nodes %>% filter(community == i) %>% select(screen_name)
   edges$c <- ifelse(edges$from %in% nodes.c$screen_name & 
                        edges$to %in% nodes.c$screen_name, T, F)
   edges.c.long <- edges %>% filter(c == T) 
   edges.c <- edges.c.long %>% select(from, stripped_text)
}

nodes.c5 <- nodes.c
edges.c5.long <- edges.c.long
edges.c5 <- edges.c


# Vis Network ----
library(visNetwork) 
vis.nodes <- nodes.c7
vis.links <- edges.c7.long

vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- F # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$name # Text on click
vis.nodes$label  <-  "" #vis.nodes$type.label # Node label
vis.nodes$size   <- vis.nodes$deg/100 # Node size
vis.nodes$borderWidth <- 0 # Node border width

colrs <- brewer.pal(n = 8, name = "YlGnBu")
vis.nodes$color.background <- colrs[7]
vis.nodes$color.border <- colrs[7]
vis.nodes$color.highlight.background <- colrs[1]
vis.nodes$color.highlight.border <- colrs[1]

#visNetwork(vis.nodes, vis.links)


# SNA ----
# Whole network

net.g <- network(edges, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "stripped_text", "date","ag_tweet", "climate_tweet", "breeding_tweet"))

net.g%v%"community" <- nodes$community

colrs <- brewer.pal(n = 8, name = "YlGnBu")
net.g%v%"color" <- colrs[net.g%v%"community"]
net.g%v%"degree" <- degree(net.g)

gplot(net.g,
      gmode = "digraph",
      displaylabels = F,
      usearrows= F,
      #edge.col = edgeColors,
      edge.lwd = 0.2,
      vertex.col = net.g%v%"color",
      vertex.cex = degree(net.g)/150,
      displayisolates=FALSE)

# Indivifual networks ----
# plotting in gplot
net.g.c1 <- network(edges.c1, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "stripped_text", "date", "hashtags", "ag_tweet", "climate_tweet", "breeding_tweet"))
net.g.c1%v%"degree" <- degree(net.g.c1)

net.g.c2 <- network(edges.c2, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "stripped_text", "date", "hashtags", "ag_tweet", "climate_tweet", "breeding_tweet"))
net.g.c2%v%"degree" <- degree(net.g.c2)

net.g.c3 <- network(edges.c3, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "stripped_text", "date", "hashtags", "ag_tweet", "climate_tweet", "breeding_tweet"))
net.g.c3%v%"degree" <- degree(net.g.c3)

net.g.c4 <- network(edges.c4, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "stripped_text", "date", "hashtags", "ag_tweet", "climate_tweet", "breeding_tweet"))
net.g.c4%v%"degree" <- degree(net.g.c4)

net.g.c5 <- network(edges.c5, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "stripped_text", "date", "hashtags", "ag_tweet", "climate_tweet", "breeding_tweet"))
net.g.c5%v%"degree" <- degree(net.g.c5)

net.g.c6 <- network(edges.c6, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "stripped_text", "date", "hashtags", "ag_tweet", "climate_tweet", "breeding_tweet"))
net.g.c6%v%"degree" <- degree(net.g.c6)

net.g.c7 <- network(edges.c7, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "stripped_text", "date", "hashtags", "ag_tweet", "climate_tweet", "breeding_tweet"))
net.g.c7%v%"degree" <- degree(net.g.c7)

net.g.c8 <- network(edges.c8, matrix.type="edgelist", ignore.eval=F, names.eval=c("date", "stripped_text", "date", "hashtags", "ag_tweet", "climate_tweet", "breeding_tweet"))
net.g.c8%v%"degree" <- degree(net.g.c8)

gplot(net.g.c8,
      gmode = "digraph",
      displaylabels = F,
      usearrows= F,
      edge.col = "grey80",
      edge.lwd = 0.1,
      vertex.col = colrs[8],
      vertex.border = "grey60",
      vertex.cex = net.g.c8%v%"degree"/80,
      displayisolates=FALSE)

# usearrows = T/F
# gmode = is "graph" for not directional, or default to direction "digraph"
# mode is for layout: eg. fruchtermanreingold, eigen, spring, target, circle, adj, mds

#We can add labels to the vertices -- labels are just their name in the network, not from an attribute

# IGRAPH --------

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T)  
net
# Set attributes before plotting ----
# Edge attributes
E(net)$color <- ifelse(E(net)$climate_tweet == TRUE, "gold", "gray50")
E(net)$width <- .1
# Set edge width based on weight:
#E(net)$width <- E(net)$weight/6
#E(net)$edge.color <- "gray80"

# Vertex attributes
colrs <- brewer.pal(n = 8, name = "YlGnBu")
V(net)$color <- colrs[V(net)$community]
deg <- igraph::degree(net, mode = "all")
V(net)$degree <-igraph::degree(net, mode = "all")
V(net)$size <- deg/100


# Labels
# The labels are currently node IDs, Setting them to NA will render no labels:
#V(net)$label <- NA
# Or set only some labels
V(net)$label <- ifelse(V(net)$degree > 2000, V(net)$name, "")
V(net)$label.color="black"
# Arrows
E(net)$arrow.size <- .01

# Plotting in igraph
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net, displayisolates=FALSE)

# COMMUNITY 1
v.remove <- V(net)[which(community != 3)]
v.remove <- V(net)[community != 3]
V(net2)[!(V(net2)$community %in% comm)]
net.c1 <- delete.vertices(net, v.remove)

colrs <- brewer.pal(n = 5, name = "YlOrRd")
V(net)$color <- colrs[V(net)$community]

plot(c1)
