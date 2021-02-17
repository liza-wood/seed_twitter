library(stm)

edges <- fread("~/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.ag.cw.names.csv")
#nodes <- fread("~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.location.csv")
nodes <- fread("~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.location.unfinished.csv")

# Need unit of analysis to be the row, so should compile each user's tweets into a unit. But keep the ones they are from and to
edges.l <- edges %>% pivot_longer(cols = c(Source, Target), values_to = "")

edges.l <- edges.l %>% select(user, stripped_text, user_id)

nodes <- nodes %>% select(user_id, community, id_, location_gen)


df <- left_join(edges.l, nodes, )

replaceNA <- function(x){ifelse(x == "", NA, x)}
df2 <- data.frame(lapply(df2[,c(1:6)], replaceNA))

table(is.na(df2$community))
table(is.na(df2$id_))
49344/(49344+495637) #10% no id
table(is.na(df2$location_gen))
277663/(277663+267318) #51% no location





processed <- textProcessor(data$documents, metadata = data)
out <- prepDocuments(processed$user_id, processed$stripped_text,
                     + processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta