# TOPIC MODELING TWEETS ----
# Break out communities
extract_community <- function(ncomm) {
  nodes.c <<- nodes %>% filter(community == ncomm) %>% 
    select(screen_name, community, id_, location_gen, bio, topic18, topic19, topic35, topic47)
  edges$c <- ifelse(edges$Source %in% nodes.c$screen_name & 
                      edges$Target %in% nodes.c$screen_name, T, F)
  edges.c.long <<- edges %>% filter(c == T) 
  edges.c <<- edges.c.long %>% select(Source, stripped_text)
}
  
  


# Hashtags
hashtag_count <- function(df){
df$hashtags <- ifelse(df$hashtags == "", NA, df$hashtags)
na.list <- is.na(df$hashtags)
percent.hashtags <- length(na.list[na.list == F])/nrow(df)
df_hashtags <- df %>% unnest_tokens(word, hashtags) %>% anti_join(stop_words) 
df_hash_count <<- df_hashtags %>% group_by(word) %>% count(word)
return(percent.hashtags)
}


# Topic plotting
topics_10 <- function(lda) {
  tidy(lda, matrix = "beta") %>% 
    group_by(topic) %>% top_n(10, beta) %>%
    ggplot(aes(x = reorder(term, beta), y = beta, fill = term)) + 
    geom_col() + coord_flip() + guides(fill = FALSE) +
    facet_wrap(vars(topic), ncol = 5, scales = "free") +
    theme_minimal(base_size = 12) +
    scale_y_continuous(labels = c()) +
    labs(title = "Top 10 words by topic", 
         x = "Word", y = "Beta")
}

topics_n <- function(lda, n) {
  tidy(lda, matrix = "beta") %>% 
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


