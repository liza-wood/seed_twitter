# Topic model, then test to see if community, bio_id, or top hashtag account for it

# Improvements: remove foreign language, combine some words, remove 

# Can look into quanteda
library(tidytext)
library(data.table)
library(dplyr)
library(topicmodels)
library(textstem)
library(ldatuning)
library(ggplot2)
library(stringr)
library(lme4)

edges <- fread("~/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.ag.cw.names.csv")
#nodes <- fread("~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.location.csv")
nodes <- fread("~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.location.unfinished.csv")

# USER AS UNIT ----
# DOCUMENT TERM MATRIX ----
ag.stopwords <- c("ag", "agriculture", "farm", "farmer", "food", "agricultural")

# Get grams
unigrams <- edges %>% unnest_tokens(word, stripped_text) 
unigrams <- unigrams %>% dplyr::filter(!str_detect(word, "[:digit:]")) %>%
  dplyr::filter(!word %in% stop_words$word) %>% 
  dplyr::filter(!word %in% ag.stopwords)

#bigrams <- edges %>% unnest_tokens(gram, stripped_text, token = "ngrams", n = 2) 
#bigrams <- tokens %>% filter(!str_detect(gram, "[:digit:]")) %>%
#  tidyr::separate(gram, c("w1", "w2"), sep = " ") %>% 
#  dplyr::filter(!w1 %in% stop_words$word & !w2 %in% stop_words$word) 
#bigrams$gram <- paste(bigrams$w1, bigrams$w2)

lemmas <- tibble(word = unique(unigrams$word))
lemmas$lemma <- lemmatize_words(lemmas$word)
unigrams <- left_join(unigrams, lemmas) 
unigrams <- unigrams %>%
  dplyr::filter(!lemma %in% ag.stopwords)

# # Tyler said he got rid of top 40 and bottom 40, because the most common words start to lose meaning; here I remove the top 10, and those that are only 1
nwords <- unigrams %>% group_by(lemma) %>% count(lemma)
remove <- nwords %>% 
  filter(n == 1)

# RM is better
unigrams.rm <- unigrams %>% filter(!unigrams$lemma %in% remove$lemma) # 6.6M

# Create dtm  -- have run this with .rm and not
c_dtm <- unigrams.rm %>% group_by(Source) %>% count(lemma) %>% cast_dtm(Source, lemma, n)
# Need to remove any empty cells
rowTotals = apply(c_dtm, 1, sum) #Find the sum of words in each Document
c_dtm = c_dtm[rowTotals> 0, ] #remove all tweets without words


# Find the potentially ideal number of topics ----
control_list <- list(alpha = 1)
topic_number <- FindTopicsNumber(c_dtm, 
                #topics = c(seq(from = 5, to = 100, by = 1), 
                #         seq(10, 25, 2), seq(20, 25, 5)),
                topics = seq(from = 5, to = 50, by = 5),
                metrics = c("Griffiths2004", "CaoJuan2009", 
                           "Arun2010", "Deveaud2014"),
                method = "Gibbs",
                control = control_list,
                mc.cores = NA,
                verbose = TRUE)
saveRDS(topic_number, "topic_number.RDS")
#topic_number <- readRDS("topic_number.RDS")
FindTopicsNumber_plot(topic_number)

png("topic_number.png", height = 5, width = 10, units = "in", res = 100)
FindTopicsNumber_plot(topic_number)
dev.off()

# Topic models ----

lda_k50_a.1 <- LDA(c_dtm, k = 50, control = list(alpha = .1))
saveRDS(lda_k50_a.1, "lda_k50_a.1.RDS")

topics_n(lda_k50_a.1, 5)


# Take a look at beta probabilities for each topic
#`sotu_topics` indicates the probability distribution of words over topics; `sotu_docs` indicates the proportion of each document (paragraph) attributed to each topic.
tm <- readRDS("lda_k50_a.1.RDS")
topics <- tidy(tm, matrix = "beta")
# See these are probabilities -- all words have some percentage of being in a topic, and add up to one
topics %>% group_by(topic) %>% summarize(total = sum(beta))

top10words <- topics %>% 
  group_by(topic) %>% 
  arrange(desc(beta)) %>% top_n(20)

users <- tidy(tm, matrix = "gamma") 
users %>% group_by(document) %>% summarize(total = sum(gamma))

top10users <- users %>% 
  group_by(document) %>% 
  filter(gamma > 0.975) %>% 
  arrange(desc(gamma)) 

toptopics.in.comm <- left_join(nodes, top10users, by = c("screen_name" = "document"))
toptopics.in.comm <- toptopics.in.comm %>% filter(!is.na(gamma))

c1.topuserstpics <- toptopics.in.comm %>% filter(community == 1) %>% 
  group_by(topic) %>% count()
c2.topuserstpics <- toptopics.in.comm %>% filter(community == 2) %>% 
  group_by(topic) %>% count()
c3.topuserstpics <- toptopics.in.comm %>% filter(community == 3) %>% 
  group_by(topic) %>% count()
c4.topuserstpics <- toptopics.in.comm %>% filter(community == 4) %>% 
  group_by(topic) %>% count()
c5.topuserstpics <- toptopics.in.comm %>% filter(community == 5) %>% 
  group_by(topic) %>% count()
c6.topuserstpics <- toptopics.in.comm %>% filter(community == 6) %>% 
  group_by(topic) %>% count()
c7.topuserstpics <- toptopics.in.comm %>% filter(community == 7) %>% 
  group_by(topic) %>% count()
c8.topuserstpics <- toptopics.in.comm %>% filter(community == 8) %>% 
  group_by(topic) %>% count()
c9.topuserstpics <- toptopics.in.comm %>% filter(community == 9) %>% 
  group_by(topic) %>% count()
c10.topuserstpics <- toptopics.in.comm %>% filter(community == 10) %>% 
  group_by(topic) %>% count()
c11.topuserstpics <- toptopics.in.comm %>% filter(community == 11) %>% 
  group_by(topic) %>% count()
c12.topuserstpics <- toptopics.in.comm %>% filter(community == 12) %>% 
  group_by(topic) %>% count()


# MODEL-----
#combine users/gamma with the user data, then run a model 

topic18 <- users %>% filter(topic == 18, document != "")
df.18 <- left_join(topic18, nodes, by = c("document" = "screen_name"))

fit18 <- lmer(topic18$gamma ~ (1|community) + (1|id_) + (1|location_gen), data = df.18)
summary(fit18) # community explains most of the variance; fixef only .029
coef(fit18) # community 3, 4 and 6; europe; even across types
ranef(fit18)

topic19 <- users %>% filter(topic == 19, document != "")
df.19 <- left_join(topic19, nodes, by = c("document" = "screen_name"))

fit19 <- lmer(topic19$gamma ~ (1|community) + (1|id_) + (1|location_gen), data = df.19)
summary(fit19) # community explaining much more of the variance here; a little more populat fixef .09
coef(fit19) # media much more likely to be talking about this kind of cc; community 4 and 6; northeast and west
ranef(fit19)

topic38 <- users %>% filter(topic == 38, document != "")
df.38 <- left_join(topic38, nodes, by = c("document" = "screen_name"))

fit38 <- lmer(topic38$gamma ~ (1|community) + (1|id_) + (1|location_gen), data = df.38)
summary(fit38) # int is .02; community still most explanatory, then location then id
coef(fit38) # community 11
ranef(fit38)

# Diversity: 16, 35, 47

topic16 <- users %>% filter(topic == 16, document != "")
df.16 <- left_join(topic16, nodes, by = c("document" = "screen_name"))

fit16 <- lmer(topic16$gamma ~ (1|community) + (1|id_) + (1|location_gen), data = df.16)
summary(fit16) 
coef(fit16) 
ranef(fit16)

topic35 <- users %>% filter(topic == 35, document != "")
df.35 <- left_join(topic35, nodes, by = c("document" = "screen_name"))

fit35 <- lmer(topic35$gamma ~ (1|community) + (1|id_) + (1|location_gen), data = df.35)
summary(fit35) 
coef(fit35) 
ranef(fit35)

topic47 <- users %>% filter(topic == 47, document != "")
df.47 <- left_join(topic47, nodes, by = c("document" = "screen_name"))

fit47 <- lmer(topic47$gamma ~ (1|community) + (1|id_) + (1|location_gen), data = df.47)
summary(fit47) # int is .02; community still most explanatory, then location then id
coef(fit47) # community 11
ranef(fit47)


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

# NEED TO TAKE INTO CONSIDERATION THE DUPLICATED EDGES??

## ******* SHOULD LEMMATIVE HASHTAGS FOR THE FEW THAT ARE THE SAME SOYBEAN, SOYBEANS

# HASHTAGS ----
hashtag_count(edges.c1.long)
c1_hash_count <- df_hash_count

# I wrote a function for it
hash_count <- c8_hash_count %>% filter(!is.na(word))
ordered <- hash_count %>% arrange(desc(n))
c8_top20 <- ordered[1:20,]

top_hashtags <- cbind(c1_top20, c2_top20, c3_top20, c4_top20, c5_top20, c6_top20, c7_top20, c8_top20)
colnames(top_hashtags) <- c("c1", "n", "c2", "n", "c3", "n", "c4", "n", "c5", "n", "c6", "n", "c7", "n", "c8", "n" )

# HASHTAGS UNIQUE ----
edges.long.unique <- edges.c8.long %>% select(stripped_text, hashtags)
edges.long.unique <- as.data.frame(unique(edges.long.unique))
# I wrote a function for it
hashtag_count(edges.long.unique)
c8_hash_count <- df_hash_count

hash_count <- c8_hash_count %>% filter(!is.na(word))
ordered <- hash_count %>% arrange(desc(n))
c8_top20_unique <- ordered[1:20,]

top_hashtags_unique <- cbind(c1_top20_unique, c2_top20_unique, c3_top20_unique, c4_top20_unique, c5_top20_unique, c6_top20_unique, c7_top20_unique, c8_top20_unique)
colnames(top_hashtags_unique) <- c("c1", "n", "c2", "n", "c3", "n", "c4", "n", "c5", "n", "c6", "n", "c7", "n", "c8", "n" )

write.csv(top_hashtags, "top_hastags.csv")
write.csv(top_hashtags_unique, "top_hastags_unique.csv")


# Bastos:  Consistent with pre-vious results (Bastos et al., 2013), we found that the communitiestweeted dominant hashtags that could be leveraged to distinguishsubstantive thematic communities.