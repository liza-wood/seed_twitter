# 9. Modelling
library(data.table)
nodes <- fread("~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.location.unfinished.csv")
library(tidytext)
library(dplyr)
tm <- readRDS("lda_k50_a.1.RDS")
topics <- tidy(tm, matrix = "beta")
# See these are probabilities -- all words have some percentage of being in a topic, and add up to one
topics %>% group_by(topic) %>% summarize(total = sum(beta))

climate <- topics %>% filter(term == "climate")
diversity <- topics %>% filter(term == "biodiversity")


# Can look at the intersection of a user's words and assigned topics
aug <- augment(tm, data = c_dtm)
?augment
climate <- aug %>% filter(term == "climate")
cgiarclimate <- aug %>% filter(document == "CGIARclimate")

top10words <- topics %>% 
  group_by(topic) %>% 
  arrange(desc(beta)) %>% top_n(20)

users <- tidy(tm, matrix = "gamma") 
users %>% group_by(document) %>% summarize(total = sum(gamma))

avg_gamma <- users %>% group_by(topic) %>% 
  summarize(mean(gamma))

post.tm <- posterior(tm)

# Theat, according to what we looked at, looks like gamma
theta <- post.tm$topics
dim(theta)


interest <- users %>% filter(topic %in% c(16, 18, 19, 35, 47)) %>% 
  group_by(topic) %>% 
  top_n(5)


top10users <- users %>% 
  group_by(document) %>% 
  filter(gamma > 0.95) %>% 
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

replaceNA <- function(x){ifelse(x == "", NA, x)}
nodes <- data.frame(lapply(nodes[,c(1:15)], replaceNA))

relevant.nodes <- nodes %>% filter(community %in% c(3,4,6,7,9,10))
relevant.nodes <- relevant.nodes %>% mutate(location_us = case_when(
  country == "United States" ~ region,
  country != "United States" ~ "Non-US",
  is.na(location_gen) ~ NA_character_,
  T ~ NA_character_
))

# Setting some baseline levels for overall int
relevant.nodes$community.f <- factor(relevant.nodes$community, levels = c("10", "9", "7", "6", "4", "3"))
levels(relevant.nodes$community.f)
relevant.nodes$location.f <- factor(relevant.nodes$location_us, levels = c("Non-US", "Midwest", "Northeast", "South", "West"))
levels(relevant.nodes$location.f)


users.reduced <- users %>% filter(document %in% relevant.nodes$screen_name)



topic18 <- users.reduced %>% filter(topic == 18, document != "")
df.18 <- left_join(topic18, relevant.nodes, by = c("document" = "screen_name"))
df.18$gamma.10 <- df.18$gamma*1
fit18 <- lmer(df.18$gamma.10 ~ 1+ (1|community) + (1|id_) + (1|location_us), data = df.18)
fit18
sum18 <- summary(fit18) # community explains most of the variance; fixef only .029

ran18 <- ranef(fit18)
coef(fit18)

hist(topic18$gamma)
hist(topic18$gamma, ylim = c(0,500))

topic19 <- users.reduced  %>% filter(topic == 19, document != "")
df.19 <- left_join(topic19, relevant.nodes, by = c("document" = "screen_name"))
df.19$gamma.10 <- df.19$gamma*1
fit19 <- lmer(df.19$gamma.10 ~ (1|community) + (1|id_) + (1|location_us), data = df.19)
fit19
sum19 <- summary(fit19) # community explaining much more of the variance here; a little more populat fixef .09
coef(fit19) # media much more likely to be talking about this kind of cc; community 4 and 6; northeast and west
ran19 <- ranef(fit19)

hist(topic19$gamma, ylim = c(0,500))
# Diversity: 16, 35, 47

topic35 <- users.reduced %>% filter(topic == 35, document != "")
df.35 <- left_join(topic35, relevant.nodes, by = c("document" = "screen_name"))
df.35$gamma.10 <- df.35$gamma*1
fit35 <- lmer(df.35$gamma.10 ~ (1|community) + (1|id_) + (1|location_us), data = df.35)
sum35 <- summary(fit35) 
#coef(fit35) 
ran35 <- ranef(fit35)

topic47 <- users.reduced %>% filter(topic == 47, document != "")
df.47 <- left_join(topic47, relevant.nodes, by = c("document" = "screen_name"))
df.47$gamma.10 <- df.47$gamma*1
fit47 <- lmer(df.47$gamma.10 ~ (1|community) + (1|id_) + (1|location_us), data = df.47)
sum47 <- summary(fit47) # int is .02; community still most explanatory, then location then id
#coef(fit47) # community 11
ran47 <- ranef(fit47)


var18 <- data.frame(sum18$varcor)[c(1,4,5)]
var19 <- data.frame(sum19$varcor)[c(4,5)]
var35 <- data.frame(sum35$varcor)[c(4,5)]
var47 <- data.frame(sum47$varcor)[c(4,5)]

# intraclass correlation = variance/total variance + residual
total.var <- var18[1,2]+var18[2,2]+ var18[3,2]+var18[4,2]
var18[1,2]/(total.var)
var18[2,2]/(total.var)
var18[3,2]/(total.var)
var18[4,2]/(total.var)

total.var <- var19[1,1]+var19[2,1]+ var19[3,1]+var19[4,1]
var19[1,1]/(total.var)
var19[2,1]/(total.var)
var19[3,1]/(total.var)
var19[4,1]/(total.var)

total.var <- var35[1,1]+var35[2,1]+ var35[3,1]+var35[4,1]
var35[1,1]/(total.var)
var35[2,1]/(total.var)
var35[3,1]/(total.var)
var35[4,1]/(total.var)

total.var <- var47[1,1]+var47[2,1]+ var47[3,1]+var47[4,1]
var47[1,1]/(total.var)
var47[2,1]/(total.var)
var47[3,1]/(total.var)
var47[4,1]/(total.var)


raneff <- cbind(var18, var19, var35, var47)
raneff[2:9] <- round(raneff[2:9], 4)

write.csv(raneff, "../../../../Desktop/overallvar.csv")

overall.int18 <- data.frame(sum18$coefficients)
rownames(overall.int18) <- "GHG emissions"
overall.int19 <- data.frame(sum19$coefficients)
rownames(overall.int19) <- "Climate policy"
overall.int35 <- data.frame(sum35$coefficients)
rownames(overall.int35) <- "Ag-system health"
overall.int47 <- data.frame(sum47$coefficients)
rownames(overall.int47) <- "Biotech & breeding"

fixed.int <- rbind(overall.int18, overall.int19, overall.int35, overall.int47)
write.csv(fixed.int, "../../../../Desktop/fixed.int.csv")

comm18 <- ran18$community 
comm19 <- ran19$community 
comm35 <- ran35$community 
comm47 <- ran47$community 

commeff <- cbind(comm18, comm19, comm35, comm47)
rownames(commeff) <- c("C3", "C4", "C6", "C7", "C9", "C10")
colnames(commeff) <- c("GHG emissions", "Climate policy", "Ag-system health", "Biotech & breeding")
write.csv(commeff, "../../../../Desktop/commeff.csv")

###
location18 <- ran18$location_us
location19 <- ran19$location_us 
location35 <- ran35$location_us 
location47 <- ran47$location_us 

commeff <- cbind(comm18, comm19, comm35, comm47)
rownames(commeff) <- c("C3", "C4", "C6", "C7", "C9", "C10")
colnames(commeff) <- c("GHG emissions", "Climate policy", "Ag-system health", "Biotech & breeding")
write.csv(commeff, "../../../../Desktop/commeff.csv")



topic18 <- users %>% filter(topic == 18, document != "")
df.18 <- left_join(topic18, nodes, by = c("document" = "screen_name"))
df.18$gamma.10 <- df.18$gamma*1
fit18 <- lmer(df.18$gamma.10 ~ 1+ (1|community) + (1|id_) + (1|location_gen), data = df.18)
fit18
sum18 <- summary(fit18) # community explains most of the variance; fixef only .029

ran18 <- ranef(fit18)
coef(fit18)

hist(topic18$gamma)
hist(topic18$gamma, ylim = c(0,500))

topic19 <- users  %>% filter(topic == 19, document != "")
df.19 <- left_join(topic19, nodes, by = c("document" = "screen_name"))
df.19$gamma.10 <- df.19$gamma*1
fit19 <- lmer(df.19$gamma.10 ~ (1|community) + (1|id_) + (1|location_gen), data = df.19)
fit19
sum19 <- summary(fit19) # community explaining much more of the variance here; a little more populat fixef .09
coef(fit19) # media much more likely to be talking about this kind of cc; community 4 and 6; northeast and west
ran19 <- ranef(fit19)

hist(topic19$gamma, ylim = c(0,500))
# Diversity: 16, 35, 47

topic35 <- users %>% filter(topic == 35, document != "")
df.35 <- left_join(topic35, nodes, by = c("document" = "screen_name"))
df.35$gamma.10 <- df.35$gamma*1
fit35 <- lmer(df.35$gamma.10 ~ (1|community) + (1|id_) + (1|location_gen), data = df.35)
sum35 <- summary(fit35) 
#coef(fit35) 
ran35 <- ranef(fit35)

topic47 <- users %>% filter(topic == 47, document != "")
df.47 <- left_join(topic47, nodes, by = c("document" = "screen_name"))
df.47$gamma.10 <- df.47$gamma*1
fit47 <- lmer(df.47$gamma.10 ~ (1|community) + (1|id_) + (1|location_gen), data = df.47)
sum47 <- summary(fit47) # int is .02; community still most explanatory, then location then id
#coef(fit47) # community 11
ran47 <- ranef(fit47)


var18 <- data.frame(sum18$varcor)[c(1,4,5)]
var19 <- data.frame(sum19$varcor)[c(4,5)]
var35 <- data.frame(sum35$varcor)[c(4,5)]
var47 <- data.frame(sum47$varcor)[c(4,5)]

# intraclass correlation = variance/total variance + residual
total.var <- var18[1,2]+var18[2,2]+ var18[3,2]+var18[4,2]
var18[1,2]/(total.var)
var18[2,2]/(total.var)
var18[3,2]/(total.var)
var18[4,2]/(total.var)

total.var <- var19[1,1]+var19[2,1]+ var19[3,1]+var19[4,1]
var19[1,1]/(total.var)
var19[2,1]/(total.var)
var19[3,1]/(total.var)
var19[4,1]/(total.var)

total.var <- var35[1,1]+var35[2,1]+ var35[3,1]+var35[4,1]
var35[1,1]/(total.var)
var35[2,1]/(total.var)
var35[3,1]/(total.var)
var35[4,1]/(total.var)

total.var <- var47[1,1]+var47[2,1]+ var47[3,1]+var47[4,1]
var47[1,1]/(total.var)
var47[2,1]/(total.var)
var47[3,1]/(total.var)
var47[4,1]/(total.var)


raneff <- cbind(var18, var19, var35, var47)
raneff[2:9] <- round(raneff[2:9], 4)

write.csv(raneff, "../../../../Desktop/overallvar.csv")

overall.int18 <- data.frame(sum18$coefficients)
rownames(overall.int18) <- "GHG emissions"
overall.int19 <- data.frame(sum19$coefficients)
rownames(overall.int19) <- "Climate policy"
overall.int35 <- data.frame(sum35$coefficients)
rownames(overall.int35) <- "Ag-system health"
overall.int47 <- data.frame(sum47$coefficients)
rownames(overall.int47) <- "Biotech & breeding"

fixed.int <- rbind(overall.int18, overall.int19, overall.int35, overall.int47)
write.csv(fixed.int, "../../../../Desktop/fixed.int.csv")

comm18 <- ran18$community 
comm19 <- ran19$community 
comm35 <- ran35$community 
comm47 <- ran47$community 

commeff <- cbind(comm18, comm19, comm35, comm47)
rownames(commeff) <- c("C3", "C4", "C6", "C7", "C9", "C10")
colnames(commeff) <- c("GHG emissions", "Climate policy", "Ag-system health", "Biotech & breeding")
write.csv(commeff, "../../../../Desktop/commeff.csv")

###
location18 <- ran18$location_us
location19 <- ran19$location_us 
location35 <- ran35$location_us 
location47 <- ran47$location_us 

commeff <- cbind(comm18, comm19, comm35, comm47)
rownames(commeff) <- c("C3", "C4", "C6", "C7", "C9", "C10")
colnames(commeff) <- c("GHG emissions", "Climate policy", "Ag-system health", "Biotech & breeding")
write.csv(commeff, "../../../../Desktop/commeff.csv")


