
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
#library(plyr)
library("plotly")
library(stringr)
library(lubridate)
library("textcat")
library("cld3")



# ---- LOADING THE ENTIRE DATAFRAME -----

tweetsdf_flat = read_twitter_csv("data/1.26_tweets_ASTA_follow_network.csv") # this now takes like 10 minutes

# 5. SUBSETTING A PARTICULAR TIMELINE  -----
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
#write_as_csv(tweets.recent, "data/6.18.recent.ASTA.tweets.csv")

# 6. PRE-PROCESSING FOR CLEANER TEXTS -------

textdf = fread("/Users/lizawood/Box/seed_twitter/data/6.18.recent.ASTA.tweets.csv")
# convert the text into characters to assist with tidytext cleaning

textdf$text = as.character(textdf$text)

length(unique(textdf$user_id))

# I have combined methods guided by tidytext cleaning (https://www.tidytextmining.com/twitter.html) as well as online blogs (e.g. https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/ AND https://gist.github.com/CateGitau/05e6ff80b2a3aaa58236067811cee44e)

# Pre-processing tweet text by removing many of the text parts that cloud NLP, assisted greatly by the grep and gsub functions (great cheat sheet is here: https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf))

# I 


# Rename the cleaned tweet text to stripped text
textdf$stripped_text = gsub("http.*","",  textdf$text) # Remove URLs
textdf$stripped_text = gsub('&gt;', '', textdf$stripped_text) # This is html for >
textdf$stripped_text = gsub('@\\S+', '', textdf$stripped_text) # Remove mentions
textdf$stripped_text = gsub('[[:cntrl:]]', '', textdf$stripped_text) # Remove Controls and special characters
textdf$stripped_text = gsub("\\d", '', textdf$stripped_text) # Remove digits
textdf$stripped_text = gsub('[[:punct:]]', '', textdf$stripped_text) # Remove Punctuations
textdf$stripped_text = gsub("^[[:space:]]*","",textdf$stripped_text) # Remove leading whitespaces
textdf$stripped_text = gsub("[[:space:]]*$","",textdf$stripped_text) # Remove trailing whitespaces
textdf$stripped_text = gsub('\\s+',' ',textdf$stripped_text) # Remove extra whitespaces
textdf$stripped_text = gsub("amp","",  textdf$stripped_text) # Remove amps


tweets <- textdf %>% select(screen_name, text, stripped_text, created_at, is_retweet, hashtags, urls_url, mentions_screen_name, retweet_screen_name, user_id, mentions_user_id, retweet_user_id)

# EDGELIST -------
# Here I will draw on the english list of tweets to create an edgelist of users and their retweets/mentions to put into network form.

# Use the separate function to piece apart the list of mentions into different columns

tweets_long = tidyr::separate(tweets, mentions_user_id, c("mention1", "mention2", "mention3", "mention4", "mention5", "mention6", "mention7", "mention8", "mention9", "mention10", "mention11", "mention12", "mention13", "mention14", "mention15", "mention16", "mention17", "mention18", "mention19", "mention20", "mention21", "mention22", "mention23", "mention24", "mention25", "mention26", "mention27", "mention28", "mention29", "mention30", "mention31", "mention32", "mention33", "mention34", "mention35", "mention36", "mention37", "mention38", "mention39", "mention40", "mention41", "mention42", "mention43", "mention44", "mention45", "mention46", "mention47", "mention48", "mention49", "mention50"), sep = " ", remove = TRUE, convert = FALSE, extra = "warn", fill = "warn")

# Identify who is being retweeted and who is being mentioned. To do this, if reweet == True, the the first name in mentions is actually the retweeter. Thus, if is_retweet == T, then delete mention1; for the mentions, I want to filter out those where there are no mentions -- these get booted out of the sample.
df_retweet = tweets_long %>% filter(is_retweet == TRUE) 
df_mention = tweets_long %>% filter(is_retweet == FALSE) %>% filter(mention1 > 0)

# Use the gather/pivot function in dplyr to convert the series of columns into an edgelist; including na.rm = T ensures that I remove tweets that are neither mentions or retweets

# For this first pivot, I am looking for the mentions that occur in tweets that are retweets. This means I am only interested in the mentions beyond mention1, as mention1 is the retweet. This makes it so that users(A) is mentioning --> B This connection will be pieced apart in pivot 3.

pivot1 = df_retweet %>%
  pivot_longer(
    cols = mention2:mention50,
    names_to = "mention_number",
    values_to = "mention_id",
    values_drop_na = TRUE)
# using select to put them in the proper order
mention.list = pivot1 %>% select(user_id, mention_id, text, stripped_text, created_at, hashtags, urls_url, is_retweet, screen_name)
# renaming columns to fit with Gephi
colnames(mention.list) = c("Source", "Target", "text", "stripped_text", "date", "hashtags", "url", "is_retweet", "user_screename")

# For the second pivot, I am looking for the mentions that came out of non-retweets. This then calls on columns of mentions 1-30 and repeats the remaining processes

pivot2 = df_mention %>%
  pivot_longer(
    cols = mention1:mention50,
    names_to = "mention_number",
    values_to = "mention_id",
    values_drop_na = TRUE)
# using select to put them in the proper order
mention.list2 = pivot2 %>% select(user_id, mention_id, text, stripped_text, created_at, hashtags, urls_url, is_retweet, screen_name)
# renaming columns to fit with Gephi
colnames(mention.list2) = c("Source", "Target", "text", "stripped_text", "date", "hashtags", "url", "is_retweet", "user_screename")

# Pivot 3 is altered because I am only isolating retweets here. Use only the retweet_screen_name (which matches mention1), but switch the directionality. For this method, a retweet of A retweeting B is in the direction of B --> A, while A mentioning B is A --> B. 

retweet.list = df_retweet %>% select(retweet_user_id, user_id, text, stripped_text, created_at, hashtags, urls_url, is_retweet, screen_name)
# renaming columns to fit with Gephi
colnames(retweet.list) = c("Source", "Target", "text", "stripped_text", "date", "hashtags", "url", "is_retweet", "user_screename")

# Putting them all together for a final edgelist that can be uploaded into Gephi
edgelist = rbind(mention.list,mention.list2, retweet.list)
edgelist %>% select(Source, Target) %>% pivot_longer(cols = Source:Target) %>% select(-name) %>% unique() %>% nrow()

# I added the .20 actually in November because I added user_id
fwrite(edgelist, "/Users/lizawood/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.csv")



