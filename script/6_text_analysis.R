# Compare top hashtags total and top hashtags unique; to top 3 topics total, and top 3 topics unique



# Can look into quanteda
library(tidytext)
library(topicmodels)
library(textstem)
library(ldatuning)
library(ggplot2)
library(stringr)
edges <- fread("~/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.ag.cw.names.csv")
nodes <- fread("~/Box/seed_twitter/data/10.31.20.ASTAnode.names.ag.cw.bio.csv")


# COMMUNITY EDGE AND NOSELIST CREATION ----

# I just plug this in repeating this on my own, which is not sustainable but I didn't really want to find a way to iterate
i = 1
nodes.c <- nodes %>% filter(community == i) %>% select(screen_name)
edges$c <- ifelse(edges$from %in% nodes.c$screen_name & 
                     edges$to %in% nodes.c$screen_name, T, F)
edges.c.long <- edges %>% filter(c == T) 
edges.c <- edges.c.long %>% select(from, stripped_text)


nodes.c8 <- nodes.c
edges.c8.long <- edges.c.long
edges.c8 <- edges.c

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

# DOCUMENT TERM MATRIX ----

# Removing very common words, and a stopword, that will definitely be in there
#c1_words = edges.c1 %>% unnest_tokens(gram, stripped_text) %>% anti_join(stop_words)

edges.c <- edges.c8

# Get bigrams
words = edges.c %>% unnest_tokens(gram, stripped_text, token = "ngrams", n = 2) 
bigrams <- words %>% filter(!str_detect(gram, "[:digit:]")) %>%
  tidyr::separate(gram, c("w1", "w2"), sep = " ") %>% 
  dplyr::filter(!w1 %in% stop_words$word & !w2 %in% stop_words$word) 
bigrams$gram <- paste(bigrams$w1, bigrams$w2)

#c1_lemmas = tibble(word = unique(c1_words$word))
#c1_lemmas$lemma <- lemmatize_words(c1_lemmas$word)
#c1_words = left_join(c1_words, c1_lemmas) 

# Create dtm 
c_dtm = bigrams %>% group_by(from) %>% count(gram) %>% cast_dtm(from, gram, n)
# Need to remove any empty cells
rowTotals = apply(c_dtm , 1, sum) #Find the sum of words in each Document
c_dtm = c_dtm[rowTotals> 0, ] #remove all docs without words

# Find the potentially ideal number of topics ----
#control_list <- list(alpha = 1)
#topic_number <- FindTopicsNumber(c_dtm, 
#                topics = c(seq(from = 5, to = 25, by = 1), 
#                         seq(10, 20, 2), seq(20, 25, 5)),
#                metrics = c("Griffiths2004", "CaoJuan2009", 
#                           "Arun2010", "Deveaud2014"),
#                method = "Gibbs",
#                control = control_list,
#                mc.cores = NA,
#                verbose = TRUE)
#?FindTopicsNumber
#saveRDS(topic_number, "topic_number_c1.RDS")
#topic_number <- readRDS("topic_number_c1.RDS")
#FindTopicsNumber_plot(topic_number)
#
#png("topic_number.png", height = 5, width = 10, units = "in", res = 100)
#FindTopicsNumber_plot(topic_number)
#dev.off()

# Topic models ----

c8_lda_k3_a.5 <- LDA(c_dtm, k = 3, control = list(alpha = .5))
topics_10(c8_lda_k3_a.5)

# From topic models it looks like 
# c1 = climate change, food system, farm bill, food movement | hashtags = organice, farmbill, foodtank, cc, nosb, health...
# c2 = breeding, planct science, cc? genetics | breedings, innovation, cc, dyk, eu, crispr, corteva
# c3 = winter wheat, seed industry, cover crop, canadian seed? | canadian seed things, innovation, specific crops, harvest 19
# c4 = food security, gm, regulation, pesticides | gmo, agtech, crispr, edeediting, innovation, organic?
# c5 = climate change, small holder, food systems, international (?) | nutrition, biodiversity |zerohunger
# c6 = farm bill, farm bureau, cover crops rural, farmers | very farmer heavy
# c7 = specific crops, soils, grains
# c8 = media

# UNIQUE TWEETS TOPIC MODEL ----
edges.c <- edges.c1

words = edges.c %>% unnest_tokens(gram, stripped_text, token = "ngrams", n = 2) 
bigrams <- words %>% filter(!str_detect(gram, "[:digit:]")) %>%
  tidyr::separate(gram, c("w1", "w2"), sep = " ") %>% 
  dplyr::filter(!w1 %in% stop_words$word & !w2 %in% stop_words$word) 
bigrams$gram <- paste(bigrams$w1, bigrams$w2)

#c1_lemmas = tibble(word = unique(c1_words$word))
#c1_lemmas$lemma <- lemmatize_words(c1_lemmas$word)
#c1_words = left_join(c1_words, c1_lemmas) 

# Create dtm ----
c_dtm = bigrams %>% group_by(from) %>% count(gram) %>% cast_dtm(from, gram, n)
# Need to remove any empty cells
rowTotals = apply(c_dtm , 1, sum) #Find the sum of words in each Document
c_dtm = c_dtm[rowTotals> 0, ] #remove all docs without words

# Find the potentially ideal number of topics ----
#control_list <- list(alpha = 1)
#topic_number <- FindTopicsNumber(c_dtm, 
#                topics = c(seq(from = 5, to = 25, by = 1), 
#                         seq(10, 20, 2), seq(20, 25, 5)),
#                metrics = c("Griffiths2004", "CaoJuan2009", 
#                           "Arun2010", "Deveaud2014"),
#                method = "Gibbs",
#                control = control_list,
#                mc.cores = NA,
#                verbose = TRUE)
#?FindTopicsNumber
#saveRDS(topic_number, "topic_number_c1.RDS")
#topic_number <- readRDS("topic_number_c1.RDS")
#FindTopicsNumber_plot(topic_number)
#
#png("topic_number.png", height = 5, width = 10, units = "in", res = 100)
#FindTopicsNumber_plot(topic_number)
#dev.off()

# Topic models, 16 or 20 or 5 ----

c5_lda_k3_a.8 <- LDA(c_dtm, k = 3, control = list(alpha = .5))
topics_10(c8_lda_k3_a.5)



# HASTAG REVIEW ----

# Bastos:  Consistent with pre-vious results (Bastos et al., 2013), we found that the communitiestweeted dominant hashtags that could be leveraged to distinguishsubstantive thematic communities.