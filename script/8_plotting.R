library(data.table)
library(igraph)
library(sna) 
library(RColorBrewer)
library(dplyr)
library(ggraph)
source("script/functions.R")

# 5. Plotting
edges <- fread("~/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.ag.cw.names.csv")
#nodes <- fread("~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.location.csv")
nodes <- fread("~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.location.unfinished.csv")
nodes <- nodes %>% select(screen_name, name, user_id, bio, community, id_, location_gen)

edges$from_user_id <- as.character(edges$from_user_id)
edges$to_user_id <- as.character(edges$to_user_id)
nodes$user_id <- as.character(nodes$user_id)

replaceNA <- function(x){ifelse(x == "", NA, x)}
nodes <- data.frame(lapply(nodes[,c(1:7)], replaceNA))
edges <- data.frame(lapply(edges[,c(1:16)], replaceNA))

# If screen name isn't there, reaplce with the user id
edges$Source <- ifelse(is.na(edges$Source), 
                       edges$from_user_id, edges$Source)
edges$Target <- ifelse(is.na(edges$Target), 
                       edges$to_user_id, edges$Target)
nodes$screen_name <- ifelse(is.na(nodes$screen_name), 
                            nodes$user_id, nodes$screen_name)

# Some edges not in node list any more. Curious..
nodes2 <- edges %>% select(Target, Source) %>%  mutate(row = 1:nrow(edges)) %>% melt(id.vars = "row", var = "name") %>% select(value) %>% unique() %>% rename("name" = "value")

nodes1 <- data.frame(nodes$screen_name)
anti <- anti_join(nodes2, nodes1, by = c("name" = "nodes.screen_name"))
colnames(anti) = "screen_name"
anti <- data.frame(anti,
                   name = NA,
                   user_id = NA,
                   bio = NA,
                   community = NA,
                   id_ = NA,
                   location_gen = NA)

nodes <- rbind(nodes, anti)


# EXPORT TO GEPHI ----
Gephi.edges <- edges
Gephi.nodes <- nodes

colnames(Gephi.edges)[1:2] <- c("Source", "Target")
Gephi.nodes$Label <- Gephi.nodes$screen_name
colnames(Gephi.nodes)[1] <- "Id"
Gephi.nodes <- select(Gephi.nodes, Id, Label, name, location_gen, id_, community)

fwrite(Gephi.edges, "~/Box/seed_twitter/data/11.3.Gephi.edges.csv")
fwrite(Gephi.nodes, "~/Box/seed_twitter/data/11.3.Gephi.nodes.csv")

# COMMUNITY EDGE AND NOSELIST CREATION ----

# Just plugging in community number
extract_community(1)
nodes.c1 <- nodes.c; edges.c1.long <- edges.c.long; edges.c1 <- edges.c
extract_community(2)
nodes.c2 <- nodes.c; edges.c2.long <- edges.c.long; edges.c2 <- edges.c
extract_community(3)
nodes.c3 <- nodes.c; edges.c3.long <- edges.c.long; edges.c3 <- edges.c
extract_community(4)
nodes.c4 <- nodes.c; edges.c4.long <- edges.c.long; edges.c4 <- edges.c
extract_community(5)
nodes.c5 <- nodes.c; edges.c5.long <- edges.c.long; edges.c5 <- edges.c
extract_community(6)
nodes.c6 <- nodes.c; edges.c6.long <- edges.c.long; edges.c6 <- edges.c
extract_community(7)
nodes.c7 <- nodes.c; edges.c7.long <- edges.c.long; edges.c7 <- edges.c
extract_community(8)
nodes.c8 <- nodes.c; edges.c8.long <- edges.c.long; edges.c8 <- edges.c
extract_community(9)
nodes.c9 <- nodes.c; edges.c9.long <- edges.c.long; edges.c9 <- edges.c
extract_community(10)
nodes.c10 <- nodes.c; edges.c10.long <- edges.c.long; edges.c10 <- edges.c
extract_community(11)
nodes.c11 <- nodes.c; edges.c11.long <- edges.c.long; edges.c11 <- edges.c
extract_community(12)
nodes.c12 <- nodes.c; edges.c12.long <- edges.c.long; edges.c12 <- edges.c

i = 6
nodes.c <- nodes %>% filter(community == i) %>% 
    select(screen_name, community, id_, location_gen, bio)
edges$c <- ifelse(edges$Source %in% nodes.c$screen_name & 
                      edges$Target %in% nodes.c$screen_name, T, F)
edges.c.long <- edges %>% filter(c == T) 
edges.c <- edges.c.long %>% select(Source, stripped_text)

nodes.c6 <- nodes.c
edges.c6.long <- edges.c.long

# GRAPH EACH COMMUNITY
lightred <- "#E74C3C" ; darkred <- "#A93226"; lightor <- "#E67E22"; darkor <- "#BA4A00"; yellow <- "#D4AC0D"; darkgrn <- "#1E8449"; medgrn <- "#28B463"; lightgrn <- "#58D68D"; darkbl <- "#1F618D"; lightbl <- "#2E86C1"; lightprpl <- "#AF7AC5"; darkgry <- "#707B7C"; lightgry <- "#BFC9CA"

# ROle color

color.df <- data.frame(color = c(lightred, darkred, lightor, 
                                 darkor, yellow, lightgrn, medgrn, darkgrn,
                                 lightbl, darkbl, lightprpl, darkgry,"#FCF3CF"),
                       role = c("company", "company emp.", "govt", "govt emp.", 
                                "farmer", "university", "university emp.",
                                "expert", "media", "journalist", "organization", 
                                "civil society", "unknown"),
                       x = c(1:13),
                       y = 1)

color.df$role <- factor(color.df$role, levels = c("company", "company emp.", 
                                                  "govt", "govt emp.", "farmer",
                                                  "university", "university emp.",
                                                  "expert", "media", "journalist",
                                                  "organization", "civil society",
                                                  "unknown"))

ggplot(color.df, aes(reorder(role, desc(role)), y)) +
  geom_point(color = color.df$color, size = 7) +
  theme_classic(base_size = 16) +
  coord_flip() +
  ylim(.75, 1.25)

# Location color 

color.df <- data.frame(color = c(darkred, darkgrn, yellow, darkbl,
                                 "#616A6B", "#707B7C", "#7F8C8D", 
                                 "#99A3A4", "#B2BABB", "#CCD1D1", "#FCF3CF"),
                       role = c("US South", "US Midwest", "US West", "US Northeast", 
                                "Canada", "Europe", "Oceania",
                                "Africa", "Asia", "South America", "unknown"),
                       x = c(1:11),
                       y = 1)

color.df$role <- factor(color.df$role, levels = c("US South", "US Midwest", "US West",
                                                  "US Northeast", "Canada", "Europe",
                                                  "Oceania","Africa", "Asia", 
                                                  "South America", "unknown"))

ggplot(color.df, aes(reorder(role, desc(role)), y)) +
  geom_point(color = color.df$color, size = 7) +
  theme_classic(base_size = 16) +
  coord_flip() +
  ylim(.75, 1.25)


net <- igraph::graph_from_data_frame(d=edges.c10.long, vertices=nodes.c10, directed=T) 
isolated <- which(igraph::degree(net)==0)
net <- igraph::delete.vertices(net, isolated)

V(net)$id_color <- ifelse(is.na(V(net)$id_), "#FCF3CF",
                   ifelse(V(net)$id_ == "company", lightred,
                   ifelse(V(net)$id_ == "company_employee", darkred,
                   ifelse(V(net)$id_ == "govt", lightor,
                   ifelse(V(net)$id_ == "govt_employee", darkor,
                   ifelse(V(net)$id_ == "farmer", yellow,
                   ifelse(V(net)$id_ == "university", lightgrn,
                   ifelse(V(net)$id_ == "university_employee", medgrn,
                   ifelse(V(net)$id_ == "expert", darkgrn,
                   ifelse(V(net)$id_ == "media", lightbl,
                   ifelse(V(net)$id_ == "media_employee", darkbl,
                   ifelse(V(net)$id_ == "org", lightprpl,
                   ifelse(V(net)$id_ == "civil_society", darkgry,
                    "lightgry")))))))))))))



V(net)$geo_color <- ifelse(is.na(V(net)$location_gen), "#FCF3CF",
                    ifelse(V(net)$location_gen == "Northeast", darkbl,
                    ifelse(V(net)$location_gen == "South", darkred,
                    ifelse(V(net)$location_gen == "Midwest", darkgrn,
                    ifelse(V(net)$location_gen == "West", yellow,
                    ifelse(V(net)$location_gen == "North America", "#616A6B",
                    ifelse(V(net)$location_gen == "Europe", "#707B7C",
                    ifelse(V(net)$location_gen == "Oceania", "#7F8C8D",
                    ifelse(V(net)$location_gen == "Africa", "#99A3A4",
                    ifelse(V(net)$location_gen == "Asia", "#B2BABB",
                    ifelse(V(net)$location_gen == "South America", "#CCD1D1",
                     "#FCF3CF")))))))))))


indeg <- igraph::degree(net, mode="in")
outdeg <- igraph::degree(net, mode="out")
deg <- igraph::degree(net, mode="total")

V(net)$size <- log(deg)

V(net)$label <- unname(ifelse(igraph::degree(net, mode = "in")[V(net)] > 5000, names(V(net)), "")) 
V(net)$label.color = "black"
V(net)$label.size = 3

p <- ggraph(net, layout = 'lgl') +
  geom_edge_link(color = "black", alpha = 0.2) +
  geom_node_point(size= V(net)$size) +
  theme_void() +
  labs(title = "C10") ; p

p + 
  geom_node_point(color = V(net)$id_color, size= V(net)$size) +
  labs(title = "C10 Roles") 

p + 
  geom_node_point(color = V(net)$geo_color, size= V(net)$size) +
  labs(title = "C10 Location")

# Science communication in C10 ----
non_sci_comp <- which(!(V(net)$id_ %in% c("expert", "university_employee", "university", "company", "company_employee")))
sci_comp_net <- igraph::delete.vertices(net, non_sci_comp)
isolated <- which(igraph::degree(sci_comp_net)==0)
sci_comp_net <- igraph::delete.vertices(sci_comp_net, isolated)

deg <- igraph::degree(sci_comp_net, mode="total")
V(sci_comp_net)$size <- log(deg)

# Need to extract the largest component
V(sci_comp_net)$comp <- igraph::components(sci_comp_net)$membership
main <- induced_subgraph(sci_comp_net,V(sci_comp_net)$comp==1)

p <- ggraph(main, layout = 'lgl') +
  geom_edge_link(color = "black", alpha = 0.2) +
  geom_node_point(size= V(main)$size) +
  theme_void() +
  labs(title = "C10 sci comp") ; p

p + 
  geom_node_point(color = V(main)$id_color, 
                  size= V(main)$size) +
  labs(title = "C10 Roles sci comp") 

sci.comp.df <- data.frame(
names <- V(main)$name,
role <- V(main)$id_,
size <- V(main)$size)

# DESCRIPTIVE TABLES OF EACH COMMUNITY

prop_id <- function(d){
  d %>% group_by(id_) %>% 
    count() %>% ungroup() %>% 
    mutate(prop = n/sum(n)) %>% 
    arrange(desc(prop))
}

prop_loc <- function(d){
  d %>% group_by(location_gen) %>% 
    count() %>% ungroup() %>% 
    mutate(prop = n/sum(n)) %>% 
    arrange(desc(prop))
}

c1_id <- prop_id(nodes.c1); c2_id <- prop_id(nodes.c2); c3_id <- prop_id(nodes.c3); c4_id <- prop_id(nodes.c4); c5_id <- prop_id(nodes.c5); c6_id <- prop_id(nodes.c6); c7_id <- prop_id(nodes.c7); c8_id <- prop_id(nodes.c8); c9_id <- prop_id(nodes.c9); c10_id <- prop_id(nodes.c10); c11_id <- prop_id(nodes.c11); c12_id <- prop_id(nodes.c12)

c1_loc <- prop_loc(nodes.c1); c2_loc <- prop_loc(nodes.c2); c3_loc <- prop_loc(nodes.c3); c4_loc <- prop_loc(nodes.c4); c5_loc <- prop_loc(nodes.c5); c6_loc <- prop_loc(nodes.c6); c7_loc <- prop_loc(nodes.c7); c8_loc <- prop_loc(nodes.c8); c9_loc <- prop_loc(nodes.c9); c10_loc <- prop_loc(nodes.c10); c11_loc <- prop_loc(nodes.c11); c12_loc <- prop_loc(nodes.c12)

# What about top 10 degree from each network
net1 <- graph_from_data_frame(d=edges.c1.long, vertices=nodes.c1, directed=T) 
net2 <- graph_from_data_frame(d=edges.c2.long, vertices=nodes.c2, directed=T) 
net3 <- graph_from_data_frame(d=edges.c3.long, vertices=nodes.c3, directed=T) 
net4 <- graph_from_data_frame(d=edges.c4.long, vertices=nodes.c4, directed=T) 
net5 <- graph_from_data_frame(d=edges.c5.long, vertices=nodes.c5, directed=T) 
net6 <- graph_from_data_frame(d=edges.c6.long, vertices=nodes.c6, directed=T) 
net7 <- graph_from_data_frame(d=edges.c7.long, vertices=nodes.c7, directed=T) 
net8 <- graph_from_data_frame(d=edges.c8.long, vertices=nodes.c8, directed=T) 
net9 <- graph_from_data_frame(d=edges.c9.long, vertices=nodes.c9, directed=T) 
net10 <- graph_from_data_frame(d=edges.c10.long, vertices=nodes.c10, directed=T) 
net11 <- graph_from_data_frame(d=edges.c11.long, vertices=nodes.c11, directed=T) 
net12 <- graph_from_data_frame(d=edges.c12.long, vertices=nodes.c12, directed=T) 


nodes.c1$deg <- igraph::degree(net1, mode = "total")
nodes.c2$deg <- igraph::degree(net2, mode = "total")
nodes.c3$deg <- igraph::degree(net3, mode = "total")
nodes.c4$deg <- igraph::degree(net4, mode = "total")
nodes.c5$deg <- igraph::degree(net5, mode = "total")
nodes.c6$deg <- igraph::degree(net6, mode = "total")
nodes.c7$deg <- igraph::degree(net7, mode = "total")
nodes.c8$deg <- igraph::degree(net8, mode = "total")
nodes.c9$deg <- igraph::degree(net9, mode = "total")
nodes.c10$deg <- igraph::degree(net10, mode = "total")
nodes.c11$deg <- igraph::degree(net11, mode = "total")
nodes.c12$deg <- igraph::degree(net12, mode = "total")

top_deg <- function(d){
  d %>% 
    arrange(desc(deg)) %>% 
    top_n(20) %>% 
    select(community, screen_name, location_gen, id_, deg)
}

c1_deg <- top_deg(nodes.c1); c2_deg <- top_deg(nodes.c2); c3_deg <- top_deg(nodes.c3); c4_deg <- top_deg(nodes.c4); c5_deg <- top_deg(nodes.c5); c6_deg <- top_deg(nodes.c6); c7_deg <- top_deg(nodes.c7); c8_deg <- top_deg(nodes.c8); c9_deg <- top_deg(nodes.c9); c10_deg <- top_deg(nodes.c10); c11_deg <- top_deg(nodes.c11); c12_deg <- top_deg(nodes.c12)

c2_deg <- c2_deg[c(1:20),]
c5_deg <- c5_deg[c(1:20),]
# Putting these community summaries together

roles <- left_join(c3_id, c2_id, by = "id_"); roles <- left_join(roles, c1_id, by = "id_"); roles <- left_join(roles, c4_id, by = "id_"); roles <- left_join(roles, c5_id, by = "id_"); roles <- left_join(roles, c6_id, by = "id_"); roles <- left_join(roles, c7_id, by = "id_"); roles <- left_join(roles, c8_id, by = "id_"); roles <- left_join(roles, c9_id, by = "id_"); roles <- left_join(roles, c10_id, by = "id_"); roles <- left_join(roles, c11_id, by = "id_"); roles <- left_join(roles, c12_id, by = "id_")

colnames(roles) <- c("role", "C3 (n)", "C3 (prop)", "C2 (n)", "C2 (prop)", "C1 (n)", "C1 (prop)", "C4 (n)", "C4 (prop)", "C5 (n)", "C5 (prop)", "C6 (n)", "C6 (prop)", "C7 (n)", "C7 (prop)", "C8 (n)", "C8 (prop)", "C9 (n)", "C9 (prop)", "C10 (n)", "C10 (prop)", "C11 (n)", "C11 (prop)", "C12 (n)", "C12 (prop)")


locations <- left_join(c1_loc, c2_loc, by = "location_gen"); locations <- left_join(locations, c3_loc, by = "location_gen"); locations <- left_join(locations, c4_loc, by = "location_gen"); locations <- left_join(locations, c5_loc, by = "location_gen"); locations <- left_join(locations, c6_loc, by = "location_gen"); locations <- left_join(locations, c7_loc, by = "location_gen"); locations <- left_join(locations, c8_loc, by = "location_gen"); locations <- left_join(locations, c9_loc, by = "location_gen"); locations <- left_join(locations, c10_loc, by = "location_gen"); locations <- left_join(locations, c11_loc, by = "location_gen"); locations <- left_join(locations, c12_loc, by = "location_gen")

colnames(locations) <- c("location", "C1 (n)", "C1 (prop)", "C2 (n)", "C2 (prop)", "C3 (n)", "C3 (prop)", "C4 (n)", "C4 (prop)", "C5 (n)", "C5 (prop)", "C6 (n)", "C6 (prop)", "C7 (n)", "C7 (prop)", "C8 (n)", "C8 (prop)", "C9 (n)", "C9 (prop)", "C10 (n)", "C10 (prop)", "C11 (n)", "C11 (prop)", "C12 (n)", "C12 (prop)")

degrees <- cbind(c1_deg, c2_deg, c3_deg, c4_deg, c5_deg, c6_deg, c7_deg, c8_deg, c9_deg, c10_deg, c11_deg, c12_deg)

colnames(locations) <- c("location", "C1 (n)", "C1 (prop)", "C2 (n)", "C2 (prop)", "C3 (n)", "C3 (prop)", "C4 (n)", "C4 (prop)", "C5 (n)", "C5 (prop)", "C6 (n)", "C6 (prop)", "C7 (n)", "C7 (prop)", "C8 (n)", "C8 (prop)", "C9 (n)", "C9 (prop)", "C10 (n)", "C10 (prop)", "C11 (n)", "C11 (prop)", "C12 (n)", "C12 (prop)")


fwrite(locations, "~/Box/seed_twitter/data/community.locations.csv")
fwrite(roles, "~/Box/seed_twitter/data/community.roles.csv")
fwrite(degrees, "~/Box/seed_twitter/data/community.degrees.csv")










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
