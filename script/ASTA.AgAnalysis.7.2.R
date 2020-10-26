library(dplyr)

# GET TWEETS FROM ORIGINAL NETWORK THAT ARE SPECIALIZED ----
agtweets = read.csv("data/agtweets.7.16.csv")
#order so that I don't lose description
agtweets = agtweets %>% select(screen_name, name, is_retweet, retweet_screen_name, retweet_name, text, stripped_text, created_at, hashtags, description, mentions)

# GET THEIR TWEETING AND RETWEETING NETWORK to make edge and node lists -----
# Use the separate function to piece apart the list of mentions into different columns
tweets_long = tidyr::separate(agtweets, mentions, c("mention1", "mention2", "mention3", "mention4", "mention5", "mention6", "mention7", "mention8", "mention9", "mention10", "mention11", "mention12", "mention13", "mention14", "mention15", "mention16", "mention17", "mention18", "mention19", "mention20", "mention21", "mention22", "mention23", "mention24", "mention25", "mention26", "mention27", "mention28", "mention29", "mention30", "mention31", "mention32", "mention33", "mention34", "mention35", "mention36", "mention37", "mention38", "mention39", "mention40", "mention41", "mention42", "mention43", "mention44", "mention45", "mention46", "mention47", "mention48", "mention49", "mention50"), sep = " ", remove = TRUE, convert = FALSE, extra = "warn", fill = "warn")


# Identify who is being retweeted and who is being mentioned. To do this, if reweet == True, the the first name in mentions is actually the retweeter. Thus, if is_retweet == T, then delete mention1; for the mentions, I want to filter out those where there are no mentions -- these get booted out of the sample.
df_retweet = tweets_long %>% filter(is_retweet == TRUE) 
df_mention = tweets_long %>% filter(is_retweet == FALSE) %>% filter(mention1 > 0)

# Use the gather/pivot function in dplyr to convert the series of columns into an edgelist; including na.rm = T ensures that I remove tweets that are neither mentions or retweets

# For this first pivot, I am looking for the mentions that occur in tweets that are retweets. This means I am only interested in the mentions beyond mention1, as mention1 is the retweet. This connection will be pieced apart in pivot 3.

pivot1 = df_retweet %>%
  pivot_longer(
    cols = mention2:mention50,
    names_to = "mention_number",
    values_to = "mention_name",
    values_drop_na = TRUE)
# using select to put them in the proper order
pivot1order = pivot1 %>% select(screen_name, mention_name, text, created_at, stripped_text, hashtags, is_retweet, description)
# renaming columns to fit with Gephi
colnames(pivot1order) = c("Source", "Target", "text", "date", "stripped_text", "hashtags", "is_retweet", "description")

# For the second pivot, I am looking for the mentions that came out of non-retweets. This then calls on columns of mentions 1-30 and repeats the remaining processes
pivot2 = df_mention %>%
  pivot_longer(
    cols = mention1:mention50,
    names_to = "mention_number",
    values_to = "mention_name",
    values_drop_na = TRUE)
# using select to put them in the proper order
pivot2order = pivot2 %>% select(screen_name, mention_name, text, created_at, stripped_text, hashtags, is_retweet, description)
# renaming columns to fit with Gephi
colnames(pivot2order) = c("Source", "Target", "text", "date", "stripped_text", "hashtags", "is_retweet", "description")

# Pivot 3 is altered because I am only isolating retweets here. Use only the retweet_screen_name (which matches mention1), but switch the directionality. For this method, a retweet of A retweeting B is in the direction of B --> A, while A mentioning B is A --> B. 

pivot3order = df_retweet %>% select(retweet_screen_name, screen_name, text, created_at, stripped_text, hashtags, is_retweet, description)
# renaming columns to fit with Gephi
colnames(pivot3order) = c("Source", "Target", "text", "date", "stripped_text", "hashtags", "is_retweet", "description")


# Putting them all together for a final edgelist that can be uploaded into Gephi
edgelist = rbind(pivot1order, pivot2order, pivot3order)

write.csv(edgelist, "data/7.16.ASTAretweetandmention.edgelist.recent.specialized.csv", row.names = F)

# NODELIST -------

edgelist = read.csv("data/7.16.ASTAretweetandmention.edgelist.recent.specialized.csv")


# Create a node list by focusing only on Sources (since we only have information about their tweets, specifically) to create a nodelist with attributes
nodelist1 = edgelist %>% select(-Target)
nodelist1$Label = nodelist1$Source
nodelist1 = nodelist1 %>% select(Source, Label)
colnames(nodelist1) = c("Id", "Label")
nodelist1 = distinct(nodelist1)

nodelist2 = edgelist %>% select(-Source)
nodelist2$Label = nodelist2$Target
nodelist2 = nodelist2 %>% select(Target, Label)
colnames(nodelist2) = c("Id", "Label")
nodelist2 = distinct(nodelist2)

nodelist = rbind(nodelist1, nodelist2)
nodelist = distinct(nodelist)


write.csv(nodelist, "data/7.16.ASTAretweetandmention.nodelist.recent.specialized.csv", row.names = FALSE)

# TRYING TO FILTER TO ELIMINATE degree = 1, but I don't know how many unique users there are, it is confusing -----
tweets = edgelist %>% select(text)
unique.tweets = distinct(tweets)

users1 = edgelist %>% select(Target)
users2 = edgelist %>% select(Source)
colnames(users2) = "Target"
users = rbind(users1, users2)
unique.users = distinct(users)

colnames(edgelist)
# You need to have more than one Source listing
nosolo1 = edgelist %>% count(Source)
# You need to have more than one Target listing
nosolo2 = edgelist %>% count(Target) 
colnames(nosolo2) = c("Source", "n2")
# Combining those with more than 1 in at least one of the categories. So minimum degree here should be 2
nosolo = full_join(nosolo1, nosolo2)

nosolo$n.na = is.na(nosolo$n)
nosolo$n2.na = is.na(nosolo$n2)
nosolo$n1 = ifelse(nosolo$n.na == T, 0, nosolo$n)
nosolo$n2 = ifelse(nosolo$n2.na == T, 0, nosolo$n2)
nosolo = nosolo %>% select(Source, n1, n2)

nosolo$sum = apply(nosolo[,c(2,3)], 1, sum, na.rm = T) 
nosolo = filter(nosolo, sum > 1)

edgelist.reduced = inner_join(edgelist, nosolo)
users1 = edgelist.reduced %>% select(Target)
users2 = edgelist.reduced %>% select(Source)
colnames(users2) = "Target"
users = rbind(users1, users2)
unique.users = distinct(users)

# This is eliminating users (and their tweets) that had less than 1 mention or retweet. Basically making the minumum degree to 2
write.csv(edgelist.reduced, "data/7.16.ASTAretweetandmention.edgelist.recent.specialized.reduced.csv", row.names = F)

nosolo$Label = nosolo$Source
colnames(nosolo) = c("Id", "Label")

write.csv(nosolo, "data/7.16.ASTAretweetandmention.nodelist.recent.specialized.reduced.csv", row.names = F)




# GETTING DESCRIPTIONS ----
# ** SHOULD BE EDITING THIS DOCUMENT WITH IDS?

nodes = read.csv("data/7.16.ASTAretweetandmention.nodelist.recent.specialized.reduced.csv")

## select one or more twitter users to lookup
users = as.vector(nodes$Id)

## get users data
#usr_df = lookup_users(users)
colnames(usr_df)
userdf = usr_df %>% select(screen_name, name, description, location, followers_count:account_created_at)
userdf$description = as.character(userdf$description)

# I have combined methods guided by tidytext cleaning (https://www.tidytextmining.com/twitter.html) as well as online blogs (e.g. https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/ AND https://gist.github.com/CateGitau/05e6ff80b2a3aaa58236067811cee44e)

# Pre-processing tweet text by removing many of the text parts that cloud NLP, assisted greatly by the grep and gsub functions (great cheat sheet is here: https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf))

# Rename the cleaned tweet text to stripped text
userdf$stripped_description = gsub('http\\S+\\s*', '', userdf$description) # Remove URLs
userdf$stripped_description = gsub("http.*","",  userdf$stripped_description) # Remove URLs
userdf$stripped_description = gsub("https.*","",  userdf$stripped_description) # Remove URLs
userdf$stripped_description = gsub('@', '', userdf$stripped_description) # Remove mentions
userdf$stripped_description = gsub('[[:cntrl:]]', '', userdf$stripped_description) # Remove Controls and special characters
userdf$stripped_description = gsub("\\d", '', userdf$stripped_description) # Remove Controls and special characters
userdf$stripped_description = gsub('[[:punct:]]', '', userdf$stripped_description) # Remove Punctuations
userdf$stripped_description = gsub("^[[:space:]]*","",userdf$stripped_description) # Remove leading whitespaces
userdf$stripped_description = gsub("[[:space:]]*$","",userdf$stripped_description) # Remove trailing whitespaces
userdf$stripped_description = gsub(' +',' ',userdf$stripped_description) # Remove extra whitespaces
userdf$stripped_description = gsub("amp","",  userdf$stripped_description) # Remove amps

# this is also helpful for grep and gub functions https://compsocialscience.github.io/summer-institute/2018/materials/day3-text-analysis/basic-text-analysis/rmarkdown/Basic_Text_Analysis_in_R.html

userdf$Label = userdf$screen_name
colnames(userdf) = c("Id", "name", "description", "location", "followers_count", "friends_count", "listed_count","statuses_count", "favourites_count", "account_created_at", "stripped_description", "Label")
userdf = userdf %>% select(Id, Label, name, description, location, followers_count, friends_count, listed_count,statuses_count, favourites_count, account_created_at, stripped_description)

write.csv(userdf, "data/7.3.ASTAretweetandmention.nodelist.w.attributes")


# THIS IS CLASSIFICATION -- Combining dfs to get all of the nodes of interest
attributes = read.csv("data/7.3.ASTAretweetandmention.nodelist.w.attributes")
attributes = attributes %>% select(-X, -Id, -name, -description, -listed_count)
colnames(attributes)
colnames(attributes) = c("screen_name", "location","followers_count","friends_count","statuses_count","favourites_count","account_created_at", "stripped_description")
df = read.csv("data/7.2.user.descriptions.agtweets_id.csv")

df = left_join(df, attributes)

# Going to see how this works on only the ones I know

df$stripped_description = as.character(df$stripped_description)

df_tokenized <- df %>% unnest_tokens(word, stripped_description) %>%
  anti_join(stop_words) 
df_lemmas = tibble(word = unique(df_tokenized$word))
df_lemmas$lemma <- lemmatize_words(df_lemmas$word)
df_words <- left_join(df_tokenized, df_lemmas) 
names(df_words) = c("screenname","id", "word", "lemma")

# id3 (8 IDs): create dtm   ----
# Identify the top 1000 words used in all bios
top_fifty <- df_words %>% dplyr::count(lemma) %>% top_n(300)
dtm_id <- df_words %>% filter(lemma %in% top_fifty$lemma) %>%    
  group_by(screenname, id) %>% dplyr::count(lemma) %>% mutate(tf = n/sum(n)) %>% 
  rename(screenname_ = screenname, id_ = id) %>% 
  select(-n) %>% spread(lemma, tf)
#Substitute zero for missing values
dtm_id[is.na(dtm_id)] <- 0

colnames(dtm_id)


# id3 (8 IDs): Training and classification assignment ----
# Divide the dtm into a training set and a classification set
train <- dtm_id[dtm_id$id_ != "Unknown", ]
class <- dtm_id[dtm_id$id_ == "Unknown", ]
table(train$id_)

# id3 (8 IDs): KNN  ----
pred_knn = knn(train[, 3:ncol(train)], class[, 3:ncol(class)], factor(train$id_), k = 5)
predict_knn = tibble(knn(train[, 3:ncol(train)], class[, 3:ncol(class)], factor(train$id_), k = 5))

# Assign IDs
predicted_knn <- class3[ , 1] %>% mutate(prediction = pred_knn) %>% 
  group_by(screenname_) %>% dplyr::count(prediction) %>% spread(prediction, n)

table(predict_knn)

# CROSS-VALIDATE

loo_knn <- function(k) {
  predictions <- data.frame()
  for(i in 1:nrow(train)) {
    pred <- knn(train[-i, 3:ncol(train)], 
                train[i, 3:ncol(train)], 
                factor(train[-i, ]$id_), k = k)
    predict <- train[i, 1:2] %>% ungroup %>% mutate(prediction = pred)
    predictions <- rbind(predictions, predict)
  }
  print(table(predictions$id_, predictions$prediction))
}

loo_knn(3) # 170/411 = 41% when 1/8 = 13% would be random 
loo_knn(5) # 175/411 = 42% 


