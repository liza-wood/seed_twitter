
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




# 1. TWITTER REST API KEY SET UP ------

# This is the set-up for the twitteR package, which I found more useful for getting friends and followers list
consumer_key <- "5Ksu3KltXDdNMYPeeauKQZ21H"
consumer_secret <-"lG0CEPzZLN55yMHuy6e5pWRiR6CdEAh6dWBIo2hGjo299NMh58"
access_token <- "2737151198-EPbvpcB9TstDRAdw3HyuVNW1CWsv6lJxUAjbABu"
access_secret <- "t566GrMlfu34qNM6goEuUctlHN5rFmvcb2V8MgAC0Gs1S" 
token_twitteR = setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# This is the set-up for the Rtweet package, which is just slightly different, which I found more useful for basically all other scraping functions
app_name = "agCSS" # this is the app name in Twitter under my Twitter developer account
token_rtweet <- create_token(app_name, consumer_key, consumer_secret, access_token, access_secret)

# 2a. FOLLOWING LIST  TO DATAFRAME --------
# Guided by instructions from: https://www.r-bloggers.com/mapping-twitter-followers-in-r/
# Each of these blocks grabs the friends (who the user is following) and puts them into a dataframe. This was done individually to make sure each user had the adequate number of friends grabbed -- the API seems to always be grabbing a few less friends than I can view on the actual Twitter interface. Here I specify an generally high number of friends in the getFriends command (n=5000) but increase that number for particularly popular users. I also include a retryonratelimit command, as the Twitter API sometimes times out.

# Assign the user to an object
th <- getUser('Better_Seed')
# Create a list of friends that twitteR outputs -- this cannot be coerced into a dataframe
following = th$getFriends(n = 5000, retryOnRateLimit=3)
# Get the list into dataframe form
following_df = bind_rows(lapply(following, as.data.frame))
# Add a row that labels the "source", which in this case is the user who is following others
following_df$source = "Better_Seed"

#write.csv(following_df, file = "data/1.26_edgelistfollowing_ASTA.csv",row.names = F)

# 2b. FOLLOWERS LIST TO DATAFRAME -------
# Here I am following the same code pattern as before:
followers = th$getFollowers(retryOnRateLimit=5000) 
followers_df = bind_rows(lapply(followers, as.data.frame))
followers_df$target = "Better_Seed" 

#write.csv(followers_df, file = "data/1.26_edgelistfollowers_ASTA.csv",row.names = F)

# 3. COMBINING THE TWO DFS INTO LIST OF USERS -------

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

#rtweet::write_as_csv(df2, "data/1.26_ASTA_users.csv")

# The end result is having a list of distinct users in the ASTA network, refined by having a biography and English

# 4. USING RTWEET TO GET THE TWEETS ------

# Use the function user_timelines to get tweets of these ASTA use list. I am doing them in smaller chunks based on computational limits.

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

#rtweet::write_as_csv(tweets1, "data/1.26_ASTA_network_tweets1.csv")
#rtweet::write_as_csv(tweets2, "data/1.26_ASTA_network_tweets2.csv")
#rtweet::write_as_csv(tweets3, "data/1.26_ASTA_network_tweets3.csv")
#rtweet::write_as_csv(tweets4, "data/1.26_ASTA_network_tweets4.csv")
#rtweet::write_as_csv(tweets2, "data/1.26_ASTA_network_tweets5.csv")
#rtweet::write_as_csv(tweets3, "data/1.26_ASTA_network_tweets6.csv")
#rtweet::write_as_csv(tweets4, "data/1.26_ASTA_network_tweets7.csv")

# Note that it is the "flat" version (The flat version output mentions and hashtags them with spaces between them in one column, rather than as a vectored list within a column. This allows me to use the separate function more easily to break apart the mentions and hashtags)

tweetsdf_flat1 = read_twitter_csv("data/1.26_ASTA_network_tweets1.csv")
tweetsdf_flat2 = read_twitter_csv("data/1.26_ASTA_network_tweets2.csv")
tweetsdf_flat3 = read_twitter_csv("data/1.26_ASTA_network_tweets3.csv")
tweetsdf_flat4 = read_twitter_csv("data/1.26_ASTA_network_tweets4.csv")
tweetsdf_flat5 = read_twitter_csv("data/1.26_ASTA_network_tweets5.csv")
tweetsdf_flat6 = read_twitter_csv("data/1.26_ASTA_network_tweets6.csv")
tweetsdf_flat7 = read_twitter_csv("data/1.26_ASTA_network_tweets7.csv")

totaltweets = rbind(tweetsdf_flat1, tweetsdf_flat2, tweetsdf_flat3, tweetsdf_flat4, tweetsdf_flat5, tweetsdf_flat6, tweetsdf_flat7)

#write_as_csv(totaltweets, "data/1.26_tweets_ASTA_follow_network.csv")
