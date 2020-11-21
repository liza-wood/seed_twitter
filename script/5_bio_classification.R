library(tidytext)
library(l)

# Take a look at these to see high degrees for choosin classifying, though I already did this with Gephi so
fread("~/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.ag.cw.names.csv")

# Combine the classifications I have already done, with the new 
df.old = fread("~/Box/seed_twitter/data/10.31.ASTAnode.names.ag.cw.bio.id.csv")
colnames(df.old)
df.old <- df.old %>% select(screen_name, id)
df = fread("~/Box/seed_twitter/data/10.31.20.ASTAnode.names.ag.cw.bio.csv")

df.new <- left_join(df, df.old, by = "screen_name")

# What about what was lost between the two?
anti <- anti_join(df.old, df, by = "screen_name")

# It looks like I only lost 13 -- not bad
table(df.old$id == "")
table(df.new$id == "")

table(df.new$id)

df.new$id <- ifelse(df.new$id == "cause", "org", df.new$id)




df = df.new %>% select(screen_name, bio, id)
df$bio = as.character(df$bio)

df_tokenized <- df %>% unnest_tokens(word, bio) %>%
  anti_join(stop_words) 
df_lemmas = tibble(word = unique(df_tokenized$word))
df_lemmas$lemma <- lemmatize_words(df_lemmas$word)
df_words <- left_join(df_tokenized, df_lemmas) 
names(df_words) = c("screen_name","id", "word", "lemma")

# id3 (8 IDs): create dtm   ----
# Identify the top 1000 words used in all bios
top_fifty <- df_words %>% dplyr::count(lemma) %>% top_n(26)
dtm_id3 <- df_words %>% filter(lemma %in% top_fifty$lemma) %>%    
  group_by(screenname, id3) %>% dplyr::count(lemma) %>% mutate(tf = n/sum(n)) %>% 
  rename(screenname_ = screenname, id3_ = id3) %>% 
  select(-n) %>% spread(lemma, tf)
#Substitute zero for missing values
dtm_id3[is.na(dtm_id3)] <- 0

dtm_id3 = dtm_id3 %>% select(-"__")

# id3 (8 IDs): Training and classification assignment ----
# Divide the dtm into a training set and a classification set
train3 <- dtm_id3[dtm_id3$id3_ != "Unknown", ]
class3 <- dtm_id3[dtm_id3$id3_ == "Unknown", ]
table(train3$id3_)

# id3 (8 IDs): KNN  ----
pred_knn3 = knn(train3[, 3:ncol(train3)], class3[, 3:ncol(class3)], factor(train3$id3_), k = 5)
predict_knn3 = tibble(knn(train3[, 3:ncol(train3)], class3[, 3:ncol(class3)], factor(train3$id3_), k = 5))

# Assign IDs
predicted_knn3 <- class3[ , 1] %>% mutate(prediction = pred_knn3) %>% 
  group_by(screenname_) %>% dplyr::count(prediction) %>% spread(prediction, n)

table(predict_knn3)

# CROSS-VALIDATE

loo_knn3 <- function(k) {
  predictions <- data.frame()
  for(i in 1:nrow(train3)) {
    pred <- knn(train3[-i, 3:ncol(train3)], 
                train3[i, 3:ncol(train3)], 
                factor(train3[-i, ]$id3_), k = k)
    predict <- train3[i, 1:2] %>% ungroup %>% mutate(prediction = pred)
    predictions <- rbind(predictions, predict)
  }
  print(table(predictions$id3_, predictions$prediction))
}

loo_knn3(3) # 125/402 = 31% when 1/8 = 13% would be random 
loo_knn3(5) # 130/402 = 32% (2.6x better than random)

loo_knn3(3) # using top 50 instead of 1000, 113/338 = 33%
loo_knn3(5) # using top 50 instead of 1000, 127/338 = 37%

loo_knn3(3) # using top 25 instead of 1000, 106/305 = 35%
loo_knn3(5) # using top 55 instead of 1000, 101/305 = 33%
