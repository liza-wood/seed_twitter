library(igraph)
library(dplyr)
library(netdiffuseR)
library(ggraph)

# Read in the total edgelist and nodelist between 2018-2020 -- these include binaries about whether they include the word climate
nodes = read.csv("data/6.18.ASTAretweetandmention.nodelist.recent.csv", header = TRUE, as.is = TRUE)
edges = read.csv("data/6.18.ASTAretweetandmention.edgelist.recent.csv", header = TRUE, row.names = 1, na.strings = "NA")

# WHOLE 2018-2020 NETWORK -----
# Set so that we are just dealing with climate Tweets until we can refine this a little more
edges = edges %>% select(Source, Target)
# wanting to eliminate repeats
nodes2 = unique(nodes[c("Label", "text", "climate_tweet", "breeding_tweet", "hashtags")])

# This is for Gephi
#nodes3 =unique(nodes["Label"])
#nodes3$Id = nodes3$Label
#write.csv(nodes3, "data/6.18.ASTA.nodelist.recent.simple.csv", row.names = F)

#links are used to create an adjacency matrix for a two-node network
mymat = edgelist_to_adjmat(edges)

#creating a network object by now reading edges into a graph -- this is an incidence matrix as a bipartite matrix... a one-mode matrix is an adjacency matric
net = graph_from_adjacency_matrix(mymat, mode = "directed")

# Twitter site https://rud.is/books/21-recipes/visualizing-a-graph-of-retweet-relationships.html

V(net)$node_label <- unname(ifelse(degree(net)[V(net)] > 1000, names(V(net)), "")) 
V(net)$node_size <- unname(ifelse(degree(net)[V(net)] > 1000, degree(net), 0)) 
table(V(net)$node_label)

# This, for the whole network, is seeming to take multiple hours... I stopped it at 2
ggraph(net, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(edge_width=0.01, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="springgreen",
                  color="slateblue", repel=TRUE, fontface="bold") +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Retweet Relationships", subtitle="Most retweeted users") +
  theme_graph() +
  theme(legend.position="none")

# ----- POLNET tutorial -----------
# https://kateto.net/network-visualization
# -- network, basic image
l <- layout_with_mds(net)
#l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)


plot(net, edge.arrow.size=.02, edge.color="gray",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=NA, 
     vertex.label.color="black", vertex.size=.5,
     layout=l*1.0)

# CLIMATE subset, all time -----
# Set so that we are just dealing with climate Tweets until we can refine this a little more
edgesclimate = edges %>% filter(climate_tweet == TRUE)
nodesclimate = nodes %>% filter(climate_tweet == TRUE)
edgesclimate = edgesclimate %>% select(Source, Target)

#links are used to create an adjacency matrix for a two-node network
mymatclimate = edgelist_to_adjmat(edgesclimate)

#creating a network object by now reading edges into a graph -- this is an incidence matrix as a bipartite matrix... a one-mode matrix is an adjacency matric
netclimate = graph_from_adjacency_matrix(mymatclimate, mode = "directed")

# Twitter site https://rud.is/books/21-recipes/visualizing-a-graph-of-retweet-relationships.html

V(netclimate)$node_label <- unname(ifelse(degree(netclimate)[V(netclimate)] > 100, names(V(netclimate)), "")) 
V(netclimate)$node_size <- unname(ifelse(degree(netclimate)[V(netclimate)] > 100, degree(netclimate), 0)) 
table(V(net)$node_label)

ggraph(netclimate, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(edge_width=0.01, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="springgreen",
                  color="slateblue", repel=TRUE, fontface="bold") +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Retweet Relationships", subtitle="Most retweeted users") +
  theme_graph() +
  theme(legend.position="none")

# BREEDING subset, all time -----
# Set so that we are just dealing with climate Tweets until we can refine this a little more
edgesbreeding = edges %>% filter(breeding_tweet == TRUE)
nodesbreeding = nodes %>% filter(breeding_tweet == TRUE)
edgesbreeding = edgesbreeding %>% select(Source, Target)

#links are used to create an adjacency matrix for a two-node network
mymatbreeding = edgelist_to_adjmat(edgesbreeding)

#creating a network object by now reading edges into a graph -- this is an incidence matrix as a bipartite matrix... a one-mode matrix is an adjacency matric
netbreeding = graph_from_adjacency_matrix(mymatbreeding, mode = "directed")

# Twitter site https://rud.is/books/21-recipes/visualizing-a-graph-of-retweet-relationships.html

V(netbreeding)$node_label <- unname(ifelse(degree(netbreeding)[V(netbreeding)] > 100, names(V(netbreeding)), "")) 
V(netbreeding)$node_size <- unname(ifelse(degree(netbreeding)[V(netbreeding)] > 100, degree(netbreeding), 0)) 


ggraph(netbreeding, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(edge_width=0.01, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="springgreen",
                  color="slateblue", repel=TRUE, fontface="bold") +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Retweet Relationships", subtitle="Most retweeted users") +
  theme_graph() +
  theme(legend.position="none")

# 

# ----- POLNET tutorial -----------
# https://kateto.net/network-visualization
# -- network, basic image
l <- layout_with_mds(net)
#l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)


plot(net, edge.arrow.size=.02, edge.color="gray",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=NA, 
     vertex.label.color="black", vertex.size=.5,
     layout=l*1.0)

# This might be a smaller thing but it was taking too long
#netclimate_simple = simplify(net, edge.attr.comb=list(Weight="sum","ignore"))

# Generate colors based on org type:
colrsorg = c("gold", "skyblue")
V(net2)$color = colrsorg[nodesorg$org_type]

shape = c("square", "circle")
V(net2)$shape = shape[V(net2)$type+1]
plot(net2)

#what about colors based on province, #tomato is Yasothin, gold it Surin, pruple is Kalasin
colrs = c("tomato", "gold", "purple", "blue", "green", "gray")
V(net2)$color = colrs[nodesorg$province]
plot(net2)

plot(net2, vertex.size=5, vertex.label=nodesorg$org_name)

sort(degree(net2))
#g03 has most, g13 has a lot, as does g04 and g02
sort(strength(net2))


# can I make a subgraph of Surin
subv = nodesorg$province[nodesorg$province == "1"]
subv


Surin <- induced.subgraph(graph=net2,vids=subv)
plot(Surin)

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net2, mode="all")
V(net2)$size <- deg*1
plot(net2, vertex.label=nodesorg$org_name)


#add legend
legend(x=-1.5, y=-1.1, c("NGO","GO"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

plot(net2)

# -- network, basic image
plot(net5, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=nodessupply$org_name, vertex.label.color="black")

# -- add in all of these attributes and just do a plot(net2) at the end... but I have not figured out how to keep labels, or do much of this formatting

# Generate colors based on org type:
colrs = c("gray", "skyblue", "gold")
V(net5)$color = colrs[nodessupply$formality]
plot(net5)

#what about colors based on province
colrs = c("tomato", "gold", "purple", "blue", "green", "gray")
V(net5)$color = colrs[nodessupply$province]
plot(net5)

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net4, mode="all")
V(net5)$size <- deg*1
plot(net5)

plot(net5, vertex.label=nodesorg$org_name)

# Or instead put node size as varieties...? can't do this. How do I add node attributes by size?
V(net4)$size = nodesorg$numb_var_total
plot(net4)

#Adding labels to the net
V(net4)$label = NA
plot(net4)


# Generate colors based on formality
levels(nodessupply3$formality_shades) = c("formal seed source", "formal organization", "formal mill", "semiformal seed source", "semiformal organization", "semiformal mill", "informal seed source", "informal mill", "mixed seed source")
colrs = c("cadetblue2", "cadetblue3", "cadetblue4", "gold1", "gold2", "gold3", "gray77", "gray72", "olivedrab2")
V(net6)$color = colrs[nodessupply3$formality_shades]
plot(net6)

shape = c("square", "circle")
V(net6)$shape = shape[V(net6)$type+1]
plot(net6)

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net6, mode="all")
V(net6)$size <- deg*1
plot(net6)
plot(net6, vertex.label=NA)

plot(net6, vertex.label=nodessupply3$org_name, vertex.size=4)
plot(net6, vertex.label=NA, vertex.size=4)

legend(x=-3, y=-1, c("formal seed source", "formal organization", "formal mill", "semiformal seed source", "semiformal organization", "semiformal mill", "informal seed source", "informal mill", "mixed seed source"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.7, bty="n", ncol=3)

legend(x = -4.2, y = -1, legend=levels(nodessupply3$formality_shades)  , col = colrs , bty = "n", pch=20 , pt.cex = 1.5, cex = .75, text.col= "#777777" , horiz = FALSE, inset = c(0.1, 0.1), ncol=3)


# Or instead put node size as varieties...? can't do this. How do I add node attributes by size?
V(net4)$size = nodesorg$numb_var_total
plot(net4)

#Adding labels to the net
V(net4)$label = NA
plot(net4)


#add legend
legend(x=-1.5, y=-1.1, c("NGO","GO"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

plot(net2)
# ------ PEDRO J instructions https://pedroj.github.io/bipartite_plots/ -----------

install.packages("ggnetwork")
install.packages("ergm")
install.packages("network")
install.packages("intergraph")
install.packages("RColorBrewer")
install.packages("sna")
install.packages("ggnet") # not available for R 3.5.1
install.packages("ggbipart") # not available for R 3.5.1


devtools::install_github("pedroj/bipartite_plots")
library(devtools)
library(ggbipart)

library(GGally)
library(ggplot2)
library(ggnetwork)
library(sna)
library(ergm)
library(network)
library(intergraph)
library(RColorBrewer)
library(dplyr)
library(igraph)

# Okay so I created a network object using as.network, which worked, then it actually worked...

# SO, this is how I got my network object
net <- as.network(x = mymat, directed = T, loops = F, matrix.type = "adjacency", bipartite = F)
net

#this issue is that he wants me to use bip_init_network to create a network object and I am having a hard time, so...

#Here I am trying to visualize

#Binplot gives me lbels and some sense of things
bip_binplot(mymat, net = net, usearrows = T, mode = nodes2, displaylabels = T, vertex.color = nodes2$province)

#ggnet
ggnet(net, weight.method = nodes2$numb_var_total, vertex.shape = "square") + geom_text(aes(label = network.vertex.names(net)), color= "black", size= 3.5) + guides(color= TRUE)
