library(data.table)
library(igraph) 

# COMMUNITY DETECTION

edge.list <- fread("~/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.attr.csv", header = TRUE, na.strings = "NA")
colnames(edge.list)

# Make edgelist specialized
edges <- edge.list %>% select(Source, Target, stripped_text, date, hashtags, ag_tweet, climate_tweet, weather_tweet, breeding_tweet, user_screename) %>% filter(ag_tweet == T)

# Create intermin nodelist just with names
nodes <- edges %>% select(Target, Source) %>%  mutate(row = 1:nrow(edges)) %>% melt(id.vars = "row", var = "name") %>% select(value) %>% unique() %>% rename("name" = "value")


# Create igraph object
net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T)  


# COMMUNITIES
# Bastos: These ten large modules account for 80% of the graph (32,152 users) and the remaining, more sparsely connected nodes,are not considered in the following analyses.
total.n <- 102901

# 4 steps = 947 communities, communities > 500 are 20, and account for only 56% of the network

# 6 steps = 849 communities, communities > 500 are 18, and account for only 60% of the network
#cw6 <- cluster_walktrap(net,  steps = 6, merges = TRUE, modularity = TRUE, membership = TRUE)
comm.count6 <- data.table(table(cw6$membership))
colnames(comm.count6) <- c("community", "n")

refined6 <- comm.count6 %>% filter(n > 500) # 18 communities
refined6.n <- sum(refined6$n) 
refined6.n/total.n # only 60%

# 10 steps = 889 communities, communities > 500 are 20, and account for only 63% of the network
#cw10 <- cluster_walktrap(net,  steps = 10, merges = TRUE, modularity = TRUE, membership = TRUE)
comm.count10 <- data.table(table(cw10$membership))
colnames(comm.count10) <- c("community", "n")

refined10 <- comm.count10 %>% filter(n > 500) # 20 communities
refined10.n <- sum(refined10$n) 
refined10.n/total.n # only 63%

# This is not working with the whole network, so I will make it smaller

# The original specialized network is 102901
# The numer of vertices that do not have at least 1 retweet and 1 mention are: 59,534 (58%)
v.remove <- V(net)[which(igraph::degree(net, mode="in") < 2 & igraph::degree(net, mode="out") < 2)]
net2 <- igraph::delete.vertices(net, v.remove)


# 6 steps, 585 communities, communities > 500 are 11, and account for only 68% of the network
#cw6.2 <- cluster_walktrap(net2,  steps = 6, merges = TRUE, modularity = TRUE, membership = TRUE)
comm.count6.2 <- data.table(table(cw6.2$membership)) #585 communities
colnames(comm.count6.2) <- c("community", "n")
total.n <- sum(comm.count6.2$n)

refined6.2 <- comm.count6.2 %>% filter(n > 400) # 11 communities
refined6.n.2 <- sum(refined6.2$n) 
refined6.n.2/total.n # only 68%

# 8 steps, 432 communities, communities > 500 are 8, and account for only 74% of the network
#cw8.2 <- cluster_walktrap(net2,  steps = 8, merges = TRUE, modularity = TRUE, membership = TRUE)
comm.count8.2 <- data.table(table(cw8.2$membership)) #432 communities
colnames(comm.count8.2) <- c("community", "n")
total.n <- sum(comm.count8.2$n)

refined8.2 <- comm.count8.2 %>% filter(n > 350) # 8 communities
refined8.n.2 <- sum(refined8.2$n) 
refined8.n.2/total.n # 74%

# 10 steps, 445 communities, communities > 500 are 8, and account for 75% of the network
cw10.2 <- cluster_walktrap(net2,  steps = 10, merges = TRUE, modularity = TRUE, membership = TRUE)
comm.count10.2 <- data.table(table(cw10.2$membership)) #445 communities (410 with .20)
colnames(comm.count10.2) <- c("community", "n")
total.n <- sum(comm.count10.2$n)

refined10.2 <- comm.count10.2 %>% filter(n > 500) #  8 communities (9 with .20)
refined10.n.2 <- sum(refined10.2$n) 
refined10.n.2/total.n # 75% (77% with .2)

# 10 steps is the winner, then.
# Assignment membership attribute to the network
V(net2)$community <- cw10.2$membership

# 32685 nodes in the refined community group
# Select only the largest communities
# Subsetting network to top 8 communities
comm <- refined10.2$community
v.remove <- V(net2)[!(V(net2)$community %in% comm)]
net.top8 <- igraph::delete.vertices(net2, v.remove)

V(net.top8)$community

# Change community names
V(net.top8)$community <- ifelse(V(net.top8)$community == 6, 1,
                         ifelse(V(net.top8)$community == 13, 2,
                         ifelse(V(net.top8)$community == 14, 3,
                         ifelse(V(net.top8)$community == 15, 4,
                         ifelse(V(net.top8)$community == 21, 5,
                         ifelse(V(net.top8)$community == 23, 6,
                         ifelse(V(net.top8)$community == 25, 7,
                         ifelse(V(net.top8)$community == 27, 8, 
                         ifelse(V(net.top8)$community == 35, 9,  NA)))))))))
#Convert back to edgelist and nodelist once 
edgelist <- igraph::as_data_frame(net.top8, what = "edges")
nodelist <- igraph::as_data_frame(net.top8, what = "vertices")
rownames(nodelist) <- NULL

# Unique tweets for whole network 1320959
length(unique(edge.list$stripped_text))
# Unique tweets for specialized network 350714
length(unique(edges$stripped_text))
# Uniqye tweets for community refined network (top 9) 268348
length(unique(edgelist$stripped_text))

fwrite(edgelist, "~/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.ag.cw.csv")
fwrite(nodelist, "~/Box/seed_twitter/data/10.31.20.ASTAnode.names.ag.cw.csv")


