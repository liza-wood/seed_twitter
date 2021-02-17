library(data.table)
library(igraph) 
library(dplyr)
library(textcat)
library(cld3)

# COMMUNITY DETECTION

edge.list <- fread("~/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.attr.csv", header = TRUE, na.strings = "NA")


# Make edgelist specialized
edges <- edge.list %>% select(Source, Target, stripped_text, text, date, hashtags, url, ag_tweet, climate_tweet, weather_tweet, breeding_tweet, user_screename) %>% filter(ag_tweet == T)

edges$language = detect_language(edges$stripped_text)
table(edges$language)


englishinde = c(163, 485, 3264, 6648, 9626, 9627, 10385:10387, 18609:18611, 18968, 18969, 22055, 22056, 22193, 22194, 22423, 28812, 43565:43567, 46453, 63341, 68926:68928, 68946:68948, 71815, 71816, 77448, 77449, 93130, 94006:94008, 97079, 97402, 97405, 102158, 111262, 115566, 117347, 119932, 120599:120601, 120616:120618, 122931, 122932, 123480, 123598, 131498, 133173:133179, 143721, 154428, 154429, 164376, 164377, 166602, 166603, 166705, 166706, 167165, 167166, 167540, 168160, 169975:169977, 170145, 170146, 173910, 200696, 200725, 200726, 200772, 212125, 213326, 218343, 224486, 229769, 236409, 248446, 253353, 256289, 267024, 293084, 295291, 303010:303012, 305517, 319338, 329067:329075, 337007, 345251, 350491, 350492, 350493, 361590, 361590, 365183, 371398, 381343, 381568, 399762, 411298, 411854, 412009, 412219, 412604:412608, 416610:416611, 417595, 422071, 422939, 429796, 432008, 437557, 441047, 443332, 445402, 447965, 450669, 464531, 467115, 468112, 471175, 471301, 471371, 471477, 497973, 500145, 510514, 517089, 523411, 530685, 533510, 539332, 539453, 540315, 540322, 542620, 556815, 562223, 566719, 568111, 568146, 568536, 571352, 574194, 579850, 582668, 583236, 586653, 602153, 603854, 604632, 605119, 605652, 609158, 609666, 611068, 624236, 625944, 627179, 631202, 633155, 633639, 633641, 635468, 635676, 641179, 644853, 655879, 655903, 655904, 655912, 655917, 655924, 655927, 655934, 657250, 661049, 661092, 665191, 674178, 674614, 676457, 677438, 68213, 683528, 685214, 685237, 685594)

maybede <- edges %>% filter(rownames(edges) %in% englishinde)

notenglish <- c(12, 13, 9771, 13195, 13196, 13562:13565, 143168, 143169, 211444, 309036, 446009, 453208, 453361, 453429, 453533, 453621, 453626, 453688, 453955, 453957, 496050, 509211, 509232, 90329, 493904, 493909, 561020, 603972, 616319, 617331, 35244, 497304, 67409, 508284, 508313, 508437, 508438, 508439, 508453, 508472, 508473, 508517, 508694, 508731, 508750, 508774, 508779, 508780, 508781, 508782, 508826, 508838, 508873, 508906, 508908, 509031:509034, 509063, 509064, 509146, 509147, 509179, 509197, 509213, 509228:509230, 541227, 564292, 571147, 571197)

edges$language <- ifelse(rownames(edges) %in% englishinde, 'en', edges$language)
table(edges$language)

# Remove certain lanuages
remove <- c("am", "ar", "az", "be", "bn", "de")
edges$rm <- ifelse(edges$language %in% remove, T,
            ifelse(rownames(edges) %in% notenglish, T, F))
table(edges$rm)

edges <- edges %>% filter(rm == F)

# Unique tweet number
edges %>% select(stripped_text) %>% unique %>% nrow

# Create intermin nodelist just with names (unique user number)
nodes <- edges %>% select(Target, Source) %>%  mutate(row = 1:nrow(edges)) %>% melt(id.vars = "row", var = "name") %>% select(value) %>% unique() %>% rename("name" = "value")

# Create igraph object
net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T)  


# COMMUNITIES
# Bastos: These ten large modules account for 80% of the graph (32,152 users) and the remaining, more sparsely connected nodes,are not considered in the following analyses.
total.n <- nrow(nodes)

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

v.remove <- V(net)[which(igraph::degree(net, mode="in") < 2 & igraph::degree(net, mode="out") < 2)]
net2 <- igraph::delete.vertices(net, v.remove)

# The original specialized network is 98955
# The number of vertices that do not have at least 1 retweet and 1 mention are: 54284
1-length(v.remove)/total.n
# This leaves 43% of the netwwork

# 6 steps, 585 communities, communities > 500 are 11, and account for only 68% of the network
#cw6.2 <- cluster_walktrap(net2,  steps = 6, merges = TRUE, modularity = TRUE, membership = TRUE)
comm.count6.2 <- data.table(table(cw6.2$membership)) #585 communities
colnames(comm.count6.2) <- c("community", "n")
total.n <- sum(comm.count6.2$n)

refined6.2 <- comm.count6.2 %>% filter(n > 400) # 11 communities
refined6.n.2 <- sum(refined6.2$n) 
refined6.n.2/total.n # only 68%

# 8 steps
cw8.2 <- cluster_walktrap(net2,  steps = 8, merges = TRUE, modularity = TRUE, membership = TRUE)
comm.count8.2 <- data.table(table(cw8.2$membership)) #563 communities
colnames(comm.count8.2) <- c("community", "n")
total.n <- sum(comm.count8.2$n)

refined8.2 <- comm.count8.2 %>% filter(n > 300) # 14 communities
refined8.n.2 <- sum(refined8.2$n) 
refined8.n.2/total.n # 72%

# 10 steps
cw10.2 <- cluster_walktrap(net2,  steps = 10, merges = TRUE, modularity = TRUE, membership = TRUE)
comm.count10.2 <- data.table(table(cw10.2$membership)) #423 communities
colnames(comm.count10.2) <- c("community", "n")
total.n <- sum(comm.count10.2$n)

refined10.2 <- comm.count10.2 %>% filter(n > 250) # 12
refined10.n.2 <- sum(refined10.2$n) 
refined10.n.2/total.n # 80%

# 10 steps is the winner, then.
# Assignment membership attribute to the network
V(net2)$community <- cw10.2$membership

# Select only the largest communities
# Subsetting network to top 9 communities
comm <- refined10.2$community
v.remove <- V(net2)[!(V(net2)$community %in% comm)]
net.top12 <- igraph::delete.vertices(net2, v.remove)

V(net.top12)$community

# Change community names
V(net.top12)$community <- ifelse(V(net.top12)$community == 1, 1,
                         ifelse(V(net.top12)$community == 3, 2,
                         ifelse(V(net.top12)$community == 6, 3,
                         ifelse(V(net.top12)$community == 8, 4,
                         ifelse(V(net.top12)$community == 9, 5,
                         ifelse(V(net.top12)$community == 11, 6,
                         ifelse(V(net.top12)$community == 13, 7,
                         ifelse(V(net.top12)$community == 15, 8, 
                         ifelse(V(net.top12)$community == 16, 9,
                         ifelse(V(net.top12)$community == 23, 10,
                         ifelse(V(net.top12)$community == 31, 11,
                         ifelse(V(net.top12)$community == 180, 12,
                                NA))))))))))))
#Convert back to edgelist and nodelist once 
edgelist <- igraph::as_data_frame(net.top12, what = "edges")
nodelist <- igraph::as_data_frame(net.top12, what = "vertices")
rownames(nodelist) <- NULL

# Unique tweets for whole network 2596893; 1320900
length(edge.list$stripped_text)
length(unique(edge.list$stripped_text))
# Unique tweets for specialized network 684687; 336417
length(edges$stripped_text)
length(unique(edges$stripped_text))
# Unique tweets for community refined network (top 12) 544981; 267483
length(edgelist$stripped_text)
length(unique(edgelist$stripped_text))

fwrite(edgelist, "~/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.ag.cw.csv")
fwrite(nodelist, "~/Box/seed_twitter/data/10.31.20.ASTAnode.names.ag.cw.csv")


