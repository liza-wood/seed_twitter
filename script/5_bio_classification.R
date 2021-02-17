library(tidytext)
library(dplyr)
library(tidyverse)
library(textstem)
library(class)


# Take a look at these to see high degrees classification in Gephi
# 10.31.20.ASTAretweetandmention.edgelist.recent.ag.cw.names.csv

# Combine the classifications I have already done, with the new -- this is an interim step because I ran this twice...
df.old <- fread("~/Box/seed_twitter/data/10.31.ASTAnode.names.ag.cw.bio.id.csv")
colnames(df.old)
df.old <- df.old %>% select(screen_name, id)
table(df.old$id)
df <- fread("~/Box/seed_twitter/data/10.31.20.ASTAnode.names.ag.cw.bio.csv")
colnames(df)[1] <- "screen_name"

# Community 2 ids ----
df$id[df$screen_name == "farukshaikh"] <- "civil_society"
df$id[df$screen_name == "rajukap"] <- "expert"
df$id[df$screen_name == "pssaini1"] <- "expert"
df$id[df$screen_name == "agritimesin"] <- NA
df$id[df$screen_name == "MSGlobalgap"] <- "company_employee"
df$id[df$screen_name == "RadhamohanBJP"] <- "govt_employee"
df$id[df$screen_name == "OpenBlueCobia"] <- "company"
df$id[df$screen_name == "ET_Edge"] <- "company"
df$id[df$screen_name == "AavishkaarVC"] <- "company"
df$id[df$screen_name == "SankalpForum"] <- "company"
df$id[df$screen_name == "AL3BoerderySA"] <- "farm"
df$id[df$screen_name == "PMOIndia"] <- "govt"
df$id[df$screen_name == "KDAHMumbai"] <- "org"
df$id[df$screen_name == "JnathanParedesM"] <- "company_employee"
df$id[df$screen_name == "SkymetWeather"] <- "org"
df$id[df$screen_name == "nstomar"] <- "govt_employee"
df$id[df$screen_name == "JATINSKYMET"] <- "org_employee"
df$id[df$screen_name == "PRupala"] <- "govt_employee"

# Community 3 ids ----
df$id[df$screen_name == "CIAT_"] <- "org"
df$id[df$screen_name == "dejirauf"] <- "farmer"

# Community 4 ids ----
df$id[df$screen_name == "HRPrendeville"] <- "govt_employee"

# Community 5 ids ----
df$id[df$screen_name == "_seedinnovation"] <- "org"
df$id[df$screen_name == "PlantDiagnostic"] <- "company"

# Community 6 ids ----
df$id[df$screen_name == "TiffanyStecker"] <- "media_employee"
df$id[df$screen_name == "LFFriedman"] <- "media_employee"
df$id[df$screen_name == "environment"] <- "media"
df$id[df$screen_name == "chriscmooney"] <- "media"
df$id[df$screen_name == "POLITICOPro"] <- "media"
df$id[df$screen_name == "RepGaramendi"] <- "govt_employee"
df$id[df$screen_name == "deantscott"] <- "media_employee"
df$id[df$screen_name == "patrizzuto"] <- "media_employee"
df$id[df$screen_name == "jswatz"] <- "media_employee"
df$id[df$screen_name == "brady_dennis"] <- "media_employee"
df$id[df$screen_name == "eilperin"] <- "media_employee"
df$id[df$screen_name == "Flipboard"] <- "company"
df$id[df$screen_name == "StevenMufson"] <- "media_employee"
df$id[df$screen_name == "EnergyCommerce"] <- "govt"
df$id[df$screen_name == "aallington"] <- "media_employee"
df$id[df$screen_name == "bobbymagill"] <- "media_employee"
df$id[df$screen_name == "CoralMDavenport"] <- "media_employee"
df$id[df$screen_name == "capitalweather"] <- "media"
df$id[df$screen_name == "HirokoTabuchi"] <- "media_employee"
df$id[df$screen_name == "AnthonyAdragna"] <- "media_employee"

# Community 8 ids ----
df$id[df$screen_name== "Seeds_Canada"] <- "org"

# Community 9 ids ----
df$id[df$screen_name== "SonnyPerdue"] <- "govt_employee"

# Community 10 ids ----
df$id[df$screen_name== "ChannelSeed"] <- "company"
df$id[df$screen_name== "MachineryPete"] <- "company"

# Community 11 ids ---
df$id[df$screen_name == "MushaMukadzi1"] <- "expert"
df$id[df$screen_name == "amiran_kenya"] <- "company"
df$id[df$screen_name == "mkulimayoung"] <- "company"
df$id[df$screen_name == "corporatefarmg1"] <- "company"
df$id[df$screen_name == "IrriHub"] <- "company"
df$id[df$screen_name == "ezra_muinde_"] <- "company_employee"
df$id[df$screen_name == "rodgers_kirwa"] <- "farmer"
df$id[df$screen_name == "Mashaliah"] <- "company_employee"
df$id[df$screen_name == "Agri_FinanceKe"] <- "govt"
df$id[df$screen_name == "basera_john"] <- "govt_employee"
df$id[df$screen_name == "OfficialBizNez"] <- ""
df$id[df$screen_name == "agribusinesszw"] <- "media"
df$id[df$screen_name == "Niran_Ag"] <- "expert"
df$id[df$screen_name == "AgricultureData"] <- "company"
df$id[df$screen_name == "zimagricsociety"] <- "company"
df$id[df$screen_name == "wakulima"] <- "company"
df$id[df$screen_name == "OxfarmAg"] <- "farm"
df$id[df$screen_name == "Paulondeng"] <- "expert"
df$id[df$screen_name == "michaelondialla"] <- "farmer"


# Community 12 ids ---
df$id[df$screen_name == "UNL_CropWatch"] <- "govt"
df$id[df$screen_name == "unlagrohort"] <- "govt"
df$id[df$screen_name == "UNL_IANR"] <- "university"
df$id[df$screen_name == "croptechcafe"] <- "govt_employee"
df$id[df$screen_name == "UNLExtension"] <- "university"
df$id[df$screen_name == "NebraskaWheat"] <- "org"
df$id[df$screen_name == "AmitUNL"] <- "university_employee"
df$id[df$screen_name == "BYFUNL"] <- "university"
df$id[df$screen_name == "UNLAgEcon"] <- "university"
df$id[df$screen_name == "UNLincoln"] <- "university"
df$id[df$screen_name == "jenreesources"] <- "govt_employee"
df$id[df$screen_name == "UNLSNR"] <- "university"
df$id[df$screen_name == "glesoing2"] <- "govt_employee"
df$id[df$screen_name == "UNLAgHortOnline"] <- "university"
df$id[df$screen_name == "marketjournal"] <- "media"
df$id[df$screen_name == "NebENREC"] <- "govt"
df$id[df$screen_name == "UNL_PSEP"] <- "govt"
df$id[df$screen_name == "UNL_CASNR"] <- "university"
df$id[df$screen_name == "mshulski3"] <- "govt_employee"
df$id[df$screen_name == "tylerw_bayer"] <- "company_employee"



df.new <- left_join(df, df.old, by = "screen_name")

replaceNA <- function(x){ifelse(x == "", NA, x)}
df.new <- data.frame(lapply(df.new[,c(1:8)], replaceNA))

df.new$id <- ifelse(is.na(df.new$id.x) & !is.na(df.new$id.y), df.new$id.y,
             ifelse(!is.na(df.new$id.x) & is.na(df.new$id.y), df.new$id.x,df.new$id.x))

df.new <- df.new %>% select(-id.x, - id.y)
table(is.na(df.new$id))
# What about what was diff between the two? Quite a bit, but this makes sense since I've changed a lot
anti <- anti_join(df.old, df, by = "screen_name")

# It looks like I only lost 63 -- not bad
table(df.old$id == "")
table(is.na(df.new$id))


fwrite(df.new, "~/Box/seed_twitter/data/10.31.20.ASTAnode.names.ag.cw.bio.w.old.csv")

# Without doing the old id things, just do this:
df <- df.new
replaceNA <- function(x){ifelse(is.na(x), "", x)}
df <- data.frame(lapply(df.new[,c(1:7)], replaceNA))
df$bio <- as.character(df$bio)

# Let's first clean some words
df$bio.stripped <- gsub("http.*","",  df$bio) # Remove URLs
df$bio.stripped <- gsub("\\bt\\.co.*","",  df$bio.stripped) # Remove URLs
df$bio.stripped <- gsub("\\d*", "", df$bio.stripped) # remove numbers

df_tokenized <- df %>% unnest_tokens(word, bio.stripped) %>%
  anti_join(stop_words) 
# Maybe not lemmatize for now
# df_lemmas = tibble(word = unique(df_tokenized$word))
# df_lemmas$lemma <- lemmatize_words(df_lemmas$word)
# df_words <- left_join(df_tokenized, df_lemmas) 
# names(df_words) = c("screen_name","id", "word", "lemma")



# Identify the top 50 words used in all bios
# And create a dtm

topn <- function(topn){
   top.words <- data.frame()
   dtm_id <- data.frame()
   top_words <- df_tokenized %>% dplyr::count(word) %>% top_n(topn)
   assign("top_words", top_words, envir = .GlobalEnv)
   dtm_id <- df_tokenized %>% filter(word %in% top_words$word) %>%    
     group_by(screen_name, id) %>% dplyr::count(word) %>% mutate(tf = n/sum(n)) %>% 
     rename(screen_name_ = screen_name, id_ = id) %>% 
     select(-n) %>% tidyr::spread(word, tf)
   #Substitute zero for missing values
   dtm_id[,-c(1,2)][is.na(dtm_id[,-c(1,2)])] <- 0
   assign("dtm_id", dtm_id, envir = .GlobalEnv)
}

# Training and classification assignment ----
# Divide the dtm into a training set and a classification set
train <- dtm_id[dtm_id$id_ != "", ]
train <- train %>% filter(!is.na(id_))
class <- dtm_id[dtm_id$id_ == "", ]
class <- class %>% filter(!is.na(id_), !is.na(screen_name_))
# CROSS-VALIDATE
# We can look at our training dataset and see if it any good at training, before assigning IDs

## Leave one out in this function, and assign different knn values
loo_knn <- function(k) {
  predictions <- data.frame()
  for(i in 1:nrow(train)) {
    pred <- knn(train[-i, 3:ncol(train)], 
                train[i, 3:ncol(train)], 
                factor(train[-i, ]$id_), k = k)
    predict <- train[i, 1:2] %>% ungroup %>% mutate(prediction = pred)
    predictions <- rbind(predictions, predict)
  }
  table(predictions$id_, predictions$prediction)
  assign("predictions", predictions, envir = .GlobalEnv)
}
confusion.mat <- function(){
  total <- nrow(predictions)
  df <- data.frame()
  df <- predictions
  
  df <- df %>% group_by(id_, prediction) %>% count() %>% 
    pivot_wider(names_from = id_, values_from = n) %>% 
    replace(is.na(.), 0) 
  
  df <- select(df, df$prediction)
  
  df$row.sum <- apply(df[,c(2:12)], 1, sum)
  df$accuracy <- ""
  df$accuracy[1] <- df[1,2]/df[1,13]
  df$accuracy[2] <- df[2,3]/df[2,13]
  df$accuracy[3] <- df[3,4]/df[3,13]
  df$accuracy[4] <- df[4,5]/df[4,13]
  df$accuracy[5] <- df[5,6]/df[5,13]
  df$accuracy[6] <- df[6,7]/df[6,13]
  df$accuracy[7] <- df[7,8]/df[7,13]
  df$accuracy[8] <- df[8,9]/df[8,13]
  df$accuracy[9] <- df[9,10]/df[9,13]
  df$accuracy[10] <- df[10,11]/df[10,13]
  df$accuracy[11] <- df[11,12]/df[11,13]
  assign("df", df, envir = .GlobalEnv)
  # Overall (I am sure there is matrix algebra to deal with this but)
  accuracy <- sum(df[1,2],df[2,3],df[3,4],df[4,5],df[5,6],df[6,7],df[7,8],df[8,9],df[9,10],df[10,11],df[11,12]) / total
  assign("accuracy", accuracy, envir = .GlobalEnv)
}

# Now let's run through a few tries, n between 250 and 750, by increments of 50, and values 1-7 of k


output.df <- data.frame()
for(i in seq(250,750,50)){
  for(k in c(1,3,5,7)){
  topn(i) # pick n of tokens in the dtm
    # define training and classifying sets of the data
  train <- dtm_id[dtm_id$id_ != "", ]
  train <- train %>% filter(!is.na(id_))
  class <- dtm_id[dtm_id$id_ == "", ]
  class <- class %>% filter(!is.na(id_), !is.na(screen_name_))
  loo_knn(k) # pick k for the k-nearest neighbors cross-validation
  confusion.mat() # look accuracy of predictors
  output <- data.frame(i, k, accuracy)
  output.df <- rbind(output.df, output)
  }
}

# Okay, let's go with 500 and 7
topn(500) # pick n of tokens in the dtm
# define training and classifying sets of the data
train <- dtm_id[dtm_id$id_ != "", ]
train <- train %>% filter(!is.na(id_))
class <- dtm_id[dtm_id$id_ == "", ]
class <- class %>% filter(!is.na(id_), !is.na(screen_name_))
loo_knn(5)
confusion.mat()
accuracy

# KNN to assign   ----

# This assigns values to the classification dataset based on the training dataset
pred_knn = tibble(knn(train[, 3:ncol(train)], class[, 3:ncol(class)], factor(train$id_), k = 7))
colnames(pred_knn) <- "prediction"

# Putting the screen_name from the classification set next to its predictons
predicted_class_ids <- class[,1]
predicted_class_ids$prediction <- pred_knn$prediction
#predicted_class_ids.matrix <- predicted_class_ids %>% group_by(screen_name_) %>% 
#  dplyr::count(prediction) %>% spread(prediction, n)

# Why this did not work is beyond me:
#predicted_knn <- class[ ,1] %>% mutate(prediction = pred_knn$prediction) %>% 
#  group_by(screen_name_) %>% dplyr::count(prediction) %>% tidyr::spread(prediction, n)


# Now what?
colnames(predicted_class_ids)[2] <- "id_"
new.ids <- rbind(predicted_class_ids, train[,c(1,2)])

# bring this back again
df <- fread("~/Box/seed_twitter/data/10.31.20.ASTAnode.names.ag.cw.bio.w.old.csv")

# join and write...

total.df <- full_join(df, new.ids, by = c("screen_name" = "screen_name_"))
total.df$id_ <- ifelse(is.na(total.df$id_) & total.df$id != "", total.df$id, total.df$id_)

# I think those that have NA as id either did not have a bio, but also if none of their words were in the top 500. This is probably fine...
table(is.na(total.df$id_))
fwrite(total.df, "~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.csv")

      
      