# NODELIST
library(rtweet)
library(twitteR)
library(network)
library(stringr)

# 1. SETUP API KEYS AGAIN
# This is the set-up for the twitteR package, which I found more useful for getting friends and followers list
consumer_key <- "5Ksu3KltXDdNMYPeeauKQZ21H"
consumer_secret <-"lG0CEPzZLN55yMHuy6e5pWRiR6CdEAh6dWBIo2hGjo299NMh58"
access_token <- "2737151198-EPbvpcB9TstDRAdw3HyuVNW1CWsv6lJxUAjbABu"
access_secret <- "t566GrMlfu34qNM6goEuUctlHN5rFmvcb2V8MgAC0Gs1S" 
token_twitteR = setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# This is the set-up for the Rtweet package, which is just slightly different, which I found more useful for basically all other scraping functions
app_name = "agCSS" # this is the app name in Twitter under my Twitter developer account
token_rtweet <- create_token(app_name, consumer_key, consumer_secret, access_token, access_secret)

# Take the nodelist I previously made
nodes <- fread("~/Box/seed_twitter/data/10.31.20.ASTAnode.names.ag.cw.csv")
colnames(nodes)[1] <- "user_id"
nodes$user_id <- str_remove(nodes$user_id, "x")

# this pulled only like ~ 24K then started pulling fewer. 
#userinfo <- lookup_users(nodes$user_id) 

# Doing this piecemeal for potentially better results
userinfo1 <- lookup_users(nodes$user_id[1:10000]) # Pulled only 9822
userinfo2 <- lookup_users(nodes$user_id[10001:20000]) # Pulled only 9766
userinfo3 <- lookup_users(nodes$user_id[20001:30000]) # Pulled 9692
userinfo4 <- lookup_users(nodes$user_id[30001:32579]) # Pulled 2521

userinfo <- rbind(userinfo1, userinfo2, userinfo3, userinfo4)
nrow(nodes) - nrow(userinfo)
# 778 excluded. Maybe because of protected accounts or something. 

#Let's make sure I can't get them.
nodes$leftbehind <- !(nodes$user_id %in% userinfo$user_id)
nodes2 <- nodes %>% filter(leftbehind == T)
userinfo.left <- lookup_users(nodes2$user_id)

# One more time
nodes2$leftbehind <- !(nodes2$user_id %in% userinfo.left$user_id)
nodes2 <- nodes2 %>% filter(leftbehind == T)
userinfo.left2 <- lookup_users(nodes2$user_id)
# Nothing

# Nothing
#userinfo <- rbind(userinfo, userinfo.left)
#nrow(nodes)-nrow(userinfo)

userinfo <- userinfo %>% select(screen_name, name, location, description, user_id)
colnames(userinfo)[4] <- "bio"

# Now match up to nodes
nodes <- nodes %>% select(-leftbehind)
nodelist <- full_join(userinfo, nodes, by = "user_id")

fwrite(nodelist, "~/Box/seed_twitter/data/10.31.20.ASTAnode.names.ag.cw.bio.csv")
# Never open this in Excel or else the user ids will fail

#nodelist <- fread("~/Box/seed_twitter/data/10.31.20.ASTAnode.names.ag.cw.bio.csv")
# Combine user_ids to names
edgelist <- fread( "~/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.ag.cw.csv")
edgelist$from <- str_remove(edgelist$from, "x")
edgelist$to <- str_remove(edgelist$to, "x")

nodelist.names <- nodelist %>% select(screen_name, name, user_id)
nodelist.names$user_id <- as.character(nodelist.names$user_id)

from.df <- edgelist %>% rename("user_id" = "from")
to.df <- edgelist %>% rename("user_id" = "to")

join1 <- left_join(from.df, nodelist.names, by = "user_id")
join1 <- join1 %>% rename("from_screen_name" = "screen_name", "from_name" = "name")
join2 <- left_join(to.df, nodelist.names, by = "user_id")
join2 <- join2 %>% rename("to_screen_name" = "screen_name", "to_name" = "name")

join <- cbind(join1, join2$to_screen_name, join2$to_name)
join <- join %>% rename("to_screen_name" = "V2", "to_name" = "V3")
join <- join %>% rename("from_user_id" = "user_id", "to_user_id" = "to")
join$Source <- join$from_screen_name
join$Target <- join$to_screen_name
colnames(join)
edgelist <- join %>% select(Source, Target, stripped_text, date, hashtags, ag_tweet, climate_tweet, weather_tweet, breeding_tweet, user_screename, from_screen_name, from_name,from_user_id, to_screen_name, to_name, to_user_id)

fwrite(edgelist, "~/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.ag.cw.names.csv")

