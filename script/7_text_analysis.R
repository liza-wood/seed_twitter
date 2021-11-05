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
topic_number <- readRDS("topic_number.RDS")
FindTopicsNumber_plot(topic_number)

png("topic_number.png", height = 5, width = 10, units = "in", res = 100)
FindTopicsNumber_plot(topic_number)
dev.off()

# Topic models ----

lda_k50_a.1 <- LDA(c_dtm, k = 50, control = list(alpha = .1))
saveRDS(lda_k50_a.1, "lda_k50_a.1.RDS")

tm <- readRDS("lda_k50_a.1.RDS")

tm %>% filter()
topics_n(tm, 10, c(1:50))
topics <- tidy(tm, matrix = "beta")

topics$keyword <- str_detect(topics$term, "climate.*|adapt.*|.*diversity")
topic.ofinterest <- topics %>% filter(keyword == T, beta > 0.008)

topics <- topics %>% filter(topic %in% c(16, 18, 19, 35, 47))
users <- tidy(tm, matrix = "gamma")

# Can look at the intersection of a user's words and assigned topics
topics_n <- function(lda, n, topic.vector) {
  tidy(lda, matrix = "beta") %>% 
    filter(topic %in% topic.vector) %>% 
    group_by(topic) %>% top_n(n, beta) %>%
    ggplot(aes(x = reorder(term, beta), y = beta, fill = topic)) + #fill = term
    geom_col() + coord_flip() + guides(fill = FALSE) +
    scale_color_brewer() + 
    facet_wrap(vars(topic), ncol = 5, scales = "free") +
    theme_minimal(base_size = 12) +
    scale_y_continuous(labels = c()) +
    labs(title = "Top words by topic", 
         x = "Word", y = "Beta")
}



topics_n(tm, 10, c(16, 18, 19, 35, 47))

library(tidyverse)
users.wide <- users %>% pivot_wider(id_cols = document, names_from = topic, values_from = gamma)
colnames(users.wide) <- c("name", paste("topic", seq(1,(ncol(users.wide)-1),1), sep = ""))

# Write to add on as an attribute for plotting in 8
fwrite(users.wide, "~/Box/seed_twitter/data/users.wide.csv")

# Take a look at beta probabilities for each topic
#`sotu_topics` indicates the probability distribution of words over topics; `sotu_docs` indicates the proportion of each document (paragraph) attributed to each topic.


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