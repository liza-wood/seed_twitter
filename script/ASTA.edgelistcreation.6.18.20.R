# This is a revisiting of my CSS and STS projects combined. I am using the ASTA data pulled in the Winter term 
## ------------------------------------------------------------
## --------- DATA COLLECTION FROM CSS FALL 2019 ---------------
## ------------------------------------------------------------

# PACKAGE INSTALLATION -----

# Main packages for scraping and data wrangling
library("rtweet")
library("twitteR")
library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# Additional packages for text analysis
library("tidytext")
library("tm")
library("tau")
library(plyr)
library("plotly")
library(stringr)
library(lubridate)
library("textcat")
library("cld3")

# classification etc.
library("topicmodels")
library(textstem)
library(tidyr)
library(tidyverse)
library(tidytext)
library(tm)
library(dplyr)
library(cld3)
library(class)
library(e1071)
library("ldatuning")
library(wordcloud)



# 1. TWITTER REST API KEY SET UP ------

# This is the set-up for the twitteR package, which I found more useful for getting friends and followers list
consumer_key <- "5Ksu3KltXDdNMYPeeauKQZ21H"
consumer_secret <-"lG0CEPzZLN55yMHuy6e5pWRiR6CdEAh6dWBIo2hGjo299NMh58"
access_token <- "2737151198-EPbvpcB9TstDRAdw3HyuVNW1CWsv6lJxUAjbABu"
access_secret <- "t566GrMlfu34qNM6goEuUctlHN5rFmvcb2V8MgAC0Gs1S" 
token_twitteR = setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# This is the set-up for the rtweet package, which is just slightly different, which I found more useful for basically all other scraping functions
app_name = "agCSS" # this is the app name in Twitter under my Twitter developer account
token_rtweet <- create_token(app_name, consumer_key, consumer_secret, access_token, access_secret)

# 2. FOLLOWING LIST  TO DATAFRAME --------
# Guided by instructions from: https://www.r-bloggers.com/mapping-twitter-followers-in-r/

# Each of these blocks grabs the friends (who the user is following) and puts them into a dataframe. This was done individually to make sure each user had the adeuqate number of friends grabbed -- the API seems to always be grabbing a few less friends than I can view on the actual Twitter interface. Here I specify an generally high number of friends in the getFriends command (n=5000) but increase that number for particularly popular users. I also include a retryonratelimit command, as the Twitter API sometimes times out.

# I will annotate only the first of these.

# Assign the user to an object
th <- getUser('Better_Seed')
# Create a list of friends that twitteR outputs -- this cannot be coerced into a dataframe
following = th$getFriends(n = 5000, retryOnRateLimit=3)
# Get the list into dataframe form
following_df1 = bind_rows(lapply(following, as.data.frame))
# Add a row that labels the "source", which in this case is the user who is following others
following_df1$source = "Better_Seed"

write.csv(following_df1, file = "data/1.26_edgelistfollowing_ASTA.csv",row.names = F)


# 2. FOLLOWERS LIST TO DATAFRAME -------
# Here I am following the same code pattern to get an edgelist of 

followers = th$getFollowers(retryOnRateLimit=5000) # 98
followers_df1 = bind_rows(lapply(followers, as.data.frame))
followers_df1$target = "Better_Seed" 

write.csv(followers_df1, file = "data/1.26_edgelistfollowers_ASTA.csv",row.names = F)

# 3. COMBINING THE TWO DFS INTO LIST OF USERS -------

# Assign each of the dataframes to an object
followingdf <- read_csv("data/1.26_edgelistfollowing_ASTA.csv")
followersdf <- read_csv("data/1.26_edgelistfollowers_ASTA.csv")

# Organize the dataframes so that they are sorted and labelled the same
followingdf = followingdf %>% select(screenName, description, protected, location)
colnames(followingdf) = c("screenname", "description", "protected", "location")

followersdf = followersdf %>% select(screenName, description, protected, location)
colnames(followersdf) = c("screenname", "description", "protected", "location")

# Combine the two dataframes
df = rbind(followersdf, followingdf)

# Remove any duplicates -- this is the new sample
distinct = distinct(.data = df)
# I only want people with descriptions
distinct2 = distinct %>% filter(!(description == ""))
# and I only want English
distinct2$language = detect_language(distinct2$description)
# I looked through these by hand because the detect language function kind of sucks
df <- distinct2[order(distinct2$language),]
nonenglish = c(10:24,27,29:30,50:53,55,57:62,2597:2603,2605:2673,2675:2704,2706:2721,2726,2728:2738,2741:2745,2755,2761:2767,2769:2771,2775:2776,2780:2781,2783:2788,2790,2798:2801,2825,2829,2831:2840,2859,2862:2867,2870)
# Here is me grabbing just what looks like english descriptions
df2 = df[-nonenglish, ]

rtweet::write_as_csv(df2, "data/1.26_ASTA_users.csv")


# ------
# The end result is having a list of distinct users in the ASTA network, refined by having a biography and English, and then based on this list, I have pulled all of their Tweets 

## ------------------------------------------------------------
## ------ USER CLASSIFICATION FROM STS WINTER 2020 ------------
## ------------------------------------------------------------

# This subset of work uses the 1.26_ASTA_users.csv because it has all of the users of interest based on having a bio and on having readable bios


# Stripping text ---
users$stripped_text = gsub('http\\S+\\s*', '', users$description) # Remove URLs
users$stripped_text = gsub("http.*","",  users$stripped_text) # Remove URLs
users$stripped_text = gsub("https.*","",  users$stripped_text) # Remove URLs
users$stripped_text = gsub('[[:cntrl:]]', '', users$stripped_text) # Remove Controls and special characters
users$stripped_text = gsub("\\d", '', users$stripped_text) # Remove Controls and special characters
users$stripped_text = gsub('[[:punct:]]', '', users$stripped_text) # Remove Punctuations
users$stripped_text = gsub("^[[:space:]]*","",users$stripped_text) # Remove leading whitespaces
users$stripped_text = gsub("[[:space:]]*$","",users$stripped_text) # Remove trailing whitespaces
users$stripped_text = gsub(' +',' ',users$stripped_text) # Remove extra whitespaces
users$stripped_text = gsub("amp","",  users$stripped_text) # Remove amps
users$stripped_text = gsub("@","",  users$stripped_text) # Remove @ sign

# Language identification ---
# This is not great but it can help identify some trends
users$language = detect_language(as.character(users$stripped_text))

# Manually excluding row numbers because the language identification tool is lousy
exclude <- c(2750, 3001, 2576, 2642, 2584, 2604, 3158, 3035, 2639, 2601, 34, 2625, 2624, 1065, 2656, 3088, 445, 2661, 2667, 2639:2696, 2713, 2751, 2802, 2895, 2897, 2948, 2697, 3165, 3186, 3138, 2995, 3025, 2807, 3802, 2969, 2761, 2567, 2980, 2773, 2725, 2905, 3156, 2989, 3032, 3225, 2913, 3154, 3074, 2758, 2994, 3066, 3130)
users_en <- users[-exclude,]
# And manually filter out trends of language issues
users_en$tr = if_else(users_en$language == "tr", 1, 0)
users_en[is.na(users_en)] = 0
users_en = users_en %>% filter(tr == 0) 

users_en2 = users_en %>% select(screenname, stripped_text)


# ----
# End result is a df that I wrote and then manually coded for classification of IDs
# write.csv(users_en2, "data/11.17.ASTA.users_en2.csv") but renamed 11.17.ASTA.users_en2.csv


# CLASSIFICATION ----
# Uploading the manually classified version of this, which has "_id" on the end
df = read.csv("data/11.17.ASTA.users_en2_id.csv")
table(df$id2)
df = df %>% select(screenname, stripped_text, id3)

df$stripped_text = as.character(df$stripped_text)

df_tokenized <- df %>% unnest_tokens(word, stripped_text) %>%
  anti_join(stop_words) 
df_lemmas = tibble(word = unique(df_tokenized$word))
df_lemmas$lemma <- lemmatize_words(df_lemmas$word)
df_words <- left_join(df_tokenized, df_lemmas) 
names(df_words) = c("screenname","id3", "word", "lemma")

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

## ------------------------------------------------------------
## ------ TWEET ANALYSIS FROM CSS FALL 2019 & STS -------------
## ------------------------------------------------------------

# 4. USING RTWEET TO GET THE TWEETS ------

# Use the function user_timelines to get tweets of these users developed in the previous step. However, due to limitations of time and computer space, I will be running the remainder of the code on a random sample of 50. Though the results of this will now be meaningless based on such a limitated sample, the main purpose is to ensure that my code works for these kinds of data. I am currently researching ways to run the whole sample on AWS cloud services, but this will not be complete by the time of submission. 

users1 = subset(df2[1:500,1])
users2 = subset(df2[501:1000,1])
users3 = subset(df2[1001:1500,1])
users4 = subset(df2[1501:2000,1])
users5 = subset(df2[2001:2500,1])
users6 = subset(df2[2501:3000,1])
users7 = subset(df2[3001:3295,1])


users1.vector = unlist(users1, recursive = T)
users2.vector = unlist(users2, recursive = T)
users3.vector = unlist(users3, recursive = T)
users4.vector = unlist(users4, recursive = T)
users5.vector = unlist(users5, recursive = T)
users6.vector = unlist(users6, recursive = T)
users7.vector = unlist(users7, recursive = T)

tweets1 = get_timelines(users1.vector, n = 3200)
tweets2 = get_timelines(users2.vector, n = 3200)
tweets3 = get_timelines(users3.vector, n = 3200)
tweets4 = get_timelines(users4.vector, n = 3200)
tweets5 = get_timelines(users5.vector, n = 3200)
tweets6 = get_timelines(users6.vector, n = 3200)
tweets7 = get_timelines(users7.vector, n = 3200)

rtweet::write_as_csv(tweets1, "data/1.26_ASTA_network_tweets1.csv")
rtweet::write_as_csv(tweets2, "data/1.26_ASTA_network_tweets2.csv")
rtweet::write_as_csv(tweets3, "data/1.26_ASTA_network_tweets3.csv")
rtweet::write_as_csv(tweets4, "data/1.26_ASTA_network_tweets4.csv")
rtweet::write_as_csv(tweets2, "data/1.26_ASTA_network_tweets5.csv")
rtweet::write_as_csv(tweets3, "data/1.26_ASTA_network_tweets6.csv")
rtweet::write_as_csv(tweets4, "data/1.26_ASTA_network_tweets7.csv")

# Assign this dataframe to an object, and note that it is the "flat" version (The flat version output mentions and hashtags them with spaces between them in one column, rather than as a vectored list within a column. This allows me to use the separate function more easily to break apart the mentions and hashtags)

tweetsdf_flat1 = read_twitter_csv("data/1.26_ASTA_network_tweets1.csv")
tweetsdf_flat2 = read_twitter_csv("data/1.26_ASTA_network_tweets2.csv")
tweetsdf_flat3 = read_twitter_csv("data/1.26_ASTA_network_tweets3.csv")
tweetsdf_flat4 = read_twitter_csv("data/1.26_ASTA_network_tweets4.csv")
tweetsdf_flat5 = read_twitter_csv("data/1.26_ASTA_network_tweets5.csv")
tweetsdf_flat6 = read_twitter_csv("data/1.26_ASTA_network_tweets6.csv")
tweetsdf_flat7 = read_twitter_csv("data/1.26_ASTA_network_tweets7.csv")

totaltweets = rbind(tweetsdf_flat1, tweetsdf_flat2, tweetsdf_flat3, tweetsdf_flat4, tweetsdf_flat5, tweetsdf_flat6, tweetsdf_flat7)

write_as_csv(totaltweets, "data/1.26_tweets_ASTA_follow_network.csv")




# ---- LOADING THE ENTIRE DATAFRAME -----

tweetsdf_flat = read_twitter_csv("data/1.26_tweets_ASTA_follow_network.csv") # this now takes like 10 minutes

# CLIMATE TWEETS -------
grepdf = as.data.frame(grepl("climate | climatechange", tweetsdf_flat$text)) #this should take 5 minutes
cctweets = cbind(tweetsdf_flat,grepdf) 

cctweets2 = cctweets %>% select(screen_name, name, mentions_screen_name, is_retweet, retweet_screen_name, retweet_name, text, created_at, `grepl("climate | climatechange", tweetsdf_flat$text)`, hashtags, description)
colnames(cctweets2) = c("screen_name", "name", "mentions", "is_retweet", "retweet_screen_name", "retweet_name", "text", "date", "climate_tweet", "hashtags", "description")

# Isolate only the cc tweets for future text analysis
cctweets_only = cctweets2 %>% filter(climate_tweet == TRUE)

write_as_csv(cctweets_only, "data/cctweets2.csv")

# BREEDERS TWEETS -------
grepdf2 = as.data.frame(grepl("breed | breeding | breeder | breeds", tweetsdf_flat$description)) 
breedertweets = cbind(tweetsdf_flat,grepdf2) 

breedertweets = breedertweets %>% select(screen_name, name, mentions_screen_name, is_retweet, retweet_screen_name, retweet_name, text, created_at, `grepl("breed | breeding | breeder | breeds", tweetsdf_flat$description)`, hashtags, description)
colnames(breedertweets) = c("screen_name", "name", "mentions", "is_retweet", "retweet_screen_name", "retweet_name", "text", "date", "breeder_tweet", "hashtags", "description")

# Isolate only the cc tweets for future text analysis
breedertweets_only = breedertweets %>% filter(breeder_tweet == TRUE)

write_as_csv(breedertweets_only, "data/breedertweets.csv")


# 3. SUBSETTING A PARTICULAR TIMELINE  -----
tweetsdf = tweetsdf_flat %>% group_by(screen_name) %>%
  dplyr::summarise(NumTweets = n()) %>%
  full_join(tweetsdf_flat, by="screen_name")

# I can create a variable to tell me whether or not a user has maxed out their tweets
tweetsdf$maxedout = if_else(tweetsdf$NumTweets == 3200, 1, 0)
table(tweetsdf$maxedout)

#from this subset it looks like only 1 was maxed out, but this is more important in the future

# I want to explore a timeline of the different tweets to see where I can subset a timeline based on being maxed out, and the ts_plot function on rtweet is useful for this

tweetsdf$maxedout = as.factor(tweetsdf$maxedout)

tweetsdf %>%
  dplyr::group_by(maxedout) %>%
  ts_plot("months", trim = 1L)
# Here is looks like the users who maxed out have been very active in the past year or two

# Here is a more advanced code to visualize, using code from https://rtweet.info/
tweetsdf %>% 
  dplyr::group_by(maxedout) %>%
  ts_plot("months", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by truncated vs. non-truncated users",
    subtitle = "Twitter status (tweet) counts",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# If you then wanted to visualize the spread based on the cutoff point, which is the start of 2018
tweetsdf %>% dplyr::filter(created_at > "2018-01-01") %>%
  dplyr::group_by(maxedout) %>%
  ts_plot("months", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by Truncated vs. Non-truncated users",
    subtitle = "Twitter status (tweet) counts",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

tweets.recent = tweetsdf %>%dplyr::filter(created_at > "2018-01-01")
write_as_csv(tweets.recent, "data/6.18.recent.ASTA.tweets.csv")
# 4. PRE-PROCESSING FOR CLEANER TEXTS -------

textdf = read.csv("data/6.18.recent.ASTA.tweets.csv")
# convert the text into characters to assist with tidytext cleaning

textdf$text = as.character(textdf$text)

# I have combined methods guided by tidytext cleaning (https://www.tidytextmining.com/twitter.html) as well as online blogs (e.g. https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/ AND https://gist.github.com/CateGitau/05e6ff80b2a3aaa58236067811cee44e)

# Pre-processing tweet text by removing many of the text parts that cloud NLP, assisted greatly by the grep and gsub functions (great cheat sheet is here: https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf))

# Rename the cleaned tweet text to stripped text
textdf$stripped_text = gsub('http\\S+\\s*', '', textdf$text) # Remove URLs
textdf$stripped_text = gsub("http.*","",  textdf$stripped_text) # Remove URLs
textdf$stripped_text = gsub("https.*","",  textdf$stripped_text) # Remove URLs
textdf$stripped_text = gsub('@\\S+', '', textdf$stripped_text) # Remove mentions
textdf$stripped_text = gsub('[[:cntrl:]]', '', textdf$stripped_text) # Remove Controls and special characters
textdf$stripped_text = gsub("\\d", '', textdf$stripped_text) # Remove Controls and special characters
textdf$stripped_text = gsub('[[:punct:]]', '', textdf$stripped_text) # Remove Punctuations
textdf$stripped_text = gsub("^[[:space:]]*","",textdf$stripped_text) # Remove leading whitespaces
textdf$stripped_text = gsub("[[:space:]]*$","",textdf$stripped_text) # Remove trailing whitespaces
textdf$stripped_text = gsub(' +',' ',textdf$stripped_text) # Remove extra whitespaces
textdf$stripped_text = gsub("amp","",  textdf$stripped_text) # Remove amps

# this is also helpful for grep and gub functions https://compsocialscience.github.io/summer-institute/2018/materials/day3-text-analysis/basic-text-analysis/rmarkdown/Basic_Text_Analysis_in_R.html


# Isolating tweets that relate to climate

# Identifying some key words for grep to detect a pattern in the stripped text and pull out these users
# CLIMATE TWEETS 1 -------
grepdf = as.data.frame(grepl("climate | climatechange", textdf$stripped_text)) 
cctweets = cbind(textdf,grepdf) 

cctweets = cctweets %>% select(screen_name, name, mentions_screen_name, is_retweet, retweet_screen_name, retweet_name, text, stripped_text, created_at, `grepl("climate | climatechange", textdf$stripped_text)`, hashtags, description)
colnames(cctweets) = c("screen_name", "name", "mentions", "is_retweet", "retweet_screen_name", "retweet_name", "text", "stripped_text", "date", "climate_tweet", "hashtags", "description")

# BREEDING TWEETS 1 -------
grepdf2 = as.data.frame(grepl("breed | breeding | breeder | breeds", textdf$stripped_text)) 
breedingtweets = cbind(cctweets,grepdf2) 

tweets = breedingtweets %>% select(screen_name, name, mentions, is_retweet, retweet_screen_name, retweet_name, text, stripped_text, date, climate_tweet, `grepl("breed | breeding | breeder | breeds", textdf$stripped_text)`, hashtags, description)
colnames(tweets) = c("screen_name", "name", "mentions", "is_retweet", "retweet_screen_name", "retweet_name", "text", "stripped_text", "date", "climate_tweet","breeding_tweet", "hashtags", "description")



# AG TWEETS -------

grepdf = as.data.frame(grepl("[Aa]erat[a-z]+ | [Aa]gri[a-z]+ | [Aa]g | [Cc]rop[a-z]+ | [Cc]ultiva[a-z]+ | [Dd]rought | [Ff]arm[a-z]+ | [Ff]ertilize[a-z]+ | [Ff]ood[a-z]+ | [Gg][Mm][Oo] | [Gg]rain | [Gg]roundwater | [Gg]rower | [Hh]erbicide | [Hh]orticulture | [Hh]usbandry | [Ii]rrigat[a-z]+ | [Ll]and[a-z]+ | [Ll]ivestock | [Mm]anure | [Mm]echaniz[a-z]+ | [Mm]onoculture[s]+ | [Nn]itrat[a-z]+ | [Nn]utrient[s]+ | [Pp]est[a-z]+ | [Rr]ear[a-z]+ | [Rr]ural | [Ss]eed[s]+ | [Ss]oil[s]+ | [Tt]ill[a-z]+| [Uu][Ss][Dd][Aa] | [Ww]eed[s]+ | [Aa]groeco[a-z]+ | [Bb]iocide[s]+ | [Bb]iodivers[a-z]+ | [Bb]iological | [Cc]limate | [Cc]limatechange | [Cc]ompost[s]+ | [Cc]onservation | [Cc]ooperative | [Ee]colog[a-z]+ | [Ee]cosystem[s]+ | [Ee]mission[s]+ | [Ee]nvironment[a-z]+ | [Ff]orest[s]+ | [Ff]uel[s]+ | [Hh]abitat[s]+ | [Ll]ocally-sourced| [[Nn]atural| [Oo]rganic[s]+ | [Oo]verpopulation | [Pp]ackaging | [Pp]ollution | [Rr]ecycl[a-z]+ | [Ss]ustainab[a-z]+| [Ww]aste | [Ww]ildlife| [Rr]ain | [Dd]rought | [Dd]ry", textdf$stripped_text))
agtweets = cbind(textdf,grepdf) 
colnames(agtweets)

agtweets2 = agtweets %>% select(screen_name, name, is_retweet, retweet_screen_name, retweet_name, text, stripped_text, created_at, mentions_screen_name, `grepl(\"[Aa]erat[a-z]+ | [Aa]gri[a-z]+ | [Aa]g | [Cc]rop[a-z]+ | [Cc]ultiva[a-z]+ | [Dd]rought | [Ff]arm[a-z]+ | [Ff]ertilize[a-z]+ | [Ff]ood[a-z]+ | [Gg][Mm][Oo] | [Gg]rain | [Gg]roundwater | [Gg]rower | [Hh]erbicide | [Hh]orticulture | [Hh]usbandry | [Ii]rrigat[a-z]+ | [Ll]and[a-z]+ | [Ll]ivestock | [Mm]anure | [Mm]echaniz[a-z]+ | [Mm]onoculture[s]+ | [Nn]itrat[a-z]+ | [Nn]utrient[s]+ | [Pp]est[a-z]+ | [Rr]ear[a-z]+ | [Rr]ural | [Ss]eed[s]+ | [Ss]oil[s]+ | [Tt]ill[a-z]+| [Uu][Ss][Dd][Aa] | [Ww]eed[s]+ | [Aa]groeco[a-z]+ | [Bb]iocide[s]+ | [Bb]iodivers[a-z]+ | [Bb]iological | [Cc]limate | [Cc]limatechange | [Cc]ompost[s]+ | [Cc]onservation | [Cc]ooperative | [Ee]colog[a-z]+ | [Ee]cosystem[s]+ | [Ee]mission[s]+ | [Ee]nvironment[a-z]+ | [Ff]orest[s]+ | [Ff]uel[s]+ | [Hh]abitat[s]+ | [Ll]ocally-sourced| [[Nn]atural| [Oo]rganic[s]+ | [Oo]verpopulation | [Pp]ackaging | [Pp]ollution | [Rr]ecycl[a-z]+ | [Ss]ustainab[a-z]+| [Ww]aste | [Ww]ildlife| [Rr]ain | [Dd]rought | [Dd]ry\",      textdf$stripped_text)`, hashtags, description)
colnames(agtweets2) = c("screen_name", "name", "is_retweet", "retweet_screen_name", "retweet_name", "text", "stripped_text", "created_at", "mentions","ag_tweet", "hashtags", "description")

# Isolate only the cc tweets for future text analysis
agtweets_only = agtweets2 %>% filter(ag_tweet == TRUE)

write_as_csv(agtweets_only, "data/agtweets.csv")


### --------------------------------------------------- ###
### --------------------------------------------------- ###
### --------------------------------------------------- ###

# Starting on June 18, working with Tweets df_flat to get the retweets
textdf = read_twitter_csv("data/1.26_tweets_ASTA_follow_network.csv") # this now takes like 5 minutes


# EDGELIST -------
# Here I will draw on the english list of tweets to create an edgelist of users and their retweets/mentions to put into network form.

# Use the separate function to piece apart the list of mentions into different columns
tweets_long = tidyr::separate(tweets, mentions, c("mention1", "mention2", "mention3", "mention4", "mention5", "mention6", "mention7", "mention8", "mention9", "mention10", "mention11", "mention12", "mention13", "mention14", "mention15", "mention16", "mention17", "mention18", "mention19", "mention20", "mention21", "mention22", "mention23", "mention24", "mention25", "mention26", "mention27", "mention28", "mention29", "mention30", "mention31", "mention32", "mention33", "mention34", "mention35", "mention36", "mention37", "mention38", "mention39", "mention40", "mention41", "mention42", "mention43", "mention44", "mention45", "mention46", "mention47", "mention48", "mention49", "mention50"), sep = " ", remove = TRUE, convert = FALSE, extra = "warn", fill = "warn")


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
pivot1order = pivot1 %>% select(screen_name, mention_name, text, date, stripped_text, hashtags, is_retweet, climate_tweet, breeding_tweet)
# renaming columns to fit with Gephi
colnames(pivot1order) = c("Source", "Target", "text", "date", "stripped_text", "hashtags", "is_retweet", "climate_tweet", "breeding_tweet")

# For the second pivot, I am looking for the mentions that came out of non-retweets. This then calls on columns of mentions 1-30 and repeats the remaining processes
pivot2 = df_mention %>%
  pivot_longer(
    cols = mention1:mention50,
    names_to = "mention_number",
    values_to = "mention_name",
    values_drop_na = TRUE)
# using select to put them in the proper order
pivot2order = pivot2 %>% select(screen_name, mention_name, text, date, stripped_text, hashtags, is_retweet, climate_tweet, breeding_tweet)
# renaming columns to fit with Gephi
colnames(pivot2order) = c("Source", "Target", "text", "date", "stripped_text", "hashtags", "is_retweet", "climate_tweet", "breeding_tweet")

# Pivot 3 is altered because I am only isolating retweets here. Use only the retweet_screen_name (which matches mention1), but switch the directionality. For this method, a retweet of A retweeting B is in the direction of B --> A, while A mentioning B is A --> B. 

pivot3order = df_retweet %>% select(retweet_screen_name, screen_name, text, date, stripped_text, hashtags, is_retweet, climate_tweet, breeding_tweet)
# renaming columns to fit with Gephi
colnames(pivot3order) = c("Source", "Target", "text", "date", "stripped_text", "hashtags", "is_retweet", "climate_tweet", "breeding_tweet")


# Putting them all together for a final edgelist that can be uploaded into Gephi
edgelist = rbind(pivot1order, pivot2order, pivot3order)

#write.csv(edgelist, "data/6.18.ASTAretweetandmention.edgelist.csv")
write.csv(edgelist, "data/6.18.ASTAretweetandmention.edgelist.recent.csv")
# Have we gained or lost any edges based on this limitation?


users_limited = edgelist %>% select(text)
lets_see = distinct(users_limited)
# We have 2,710,620 unique tweets, but 4,571,468 edges for all tweets

users_limited = edgelist %>% select(text)
lets_see = distinct(users_limited)
# We have 1,431,563 unique tweets, but 2,593,203 edges for recent tweets


# NODELIST -------

# Create a node list by focusing only on Sources (since we only have information about their tweets, specifically) to create a nodelist with attributes
nodelist1 = edgelist %>% select(-Target)
nodelist1$Label = nodelist1$Source
nodelist1 = nodelist1 %>% select(Source, Label, text, date, stripped_text, climate_tweet, breeding_tweet, hashtags)
colnames(nodelist1) = c("Id", "Label", "text", "date", "stripped_text", "climate_tweet", "breeding_tweet", "hashtags")

nodelist2 = edgelist %>% select(-Source)
nodelist2$Label = nodelist2$Target
nodelist2 = nodelist2 %>% select(Target, Label, text, date, stripped_text, climate_tweet, breeding_tweet, hashtags)
colnames(nodelist2) = c("Id", "Label", "text", "date", "stripped_text", "climate_tweet", "breeding_tweet", "hashtags")

nodelist = rbind(nodelist1, nodelist2)

write.csv(nodelist, "data/6.18.ASTAretweetandmention.nodelist.csv", row.names = FALSE)
write.csv(nodelist, "data/6.18.ASTAretweetandmention.nodelist.recent.csv", row.names = FALSE)



