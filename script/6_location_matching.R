library(tools)
library(dplyr)

nodes <- fread("~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.csv")

# Clean up location
# get list of US cities, states
# get list of Canadian cities and provinces
# List of UK cities
nodes$row <- seq(1, nrow(nodes))
nodes$location <- trimws(nodes$location)

# If these things, then just delete
remove <- c("[Ee]verywhere", "[Ee]arth", "^Global$", "^International$", "^[Ii]n\\s", "^Nationwide$", "^[Oo]n\\b", "[Pp]ale [Bb]lue [Dd]ot", "^Somewhere", "[Tt]he [Ww]orld", "^Home$", "^Wherever", "^Probably", "^World$", "^Worldwide$")
remove <- paste(remove, collapse = "|")
nodes$location <- ifelse(str_detect(nodes$location, remove), "", nodes$location)

nodes$location <- toTitleCase(nodes$location)


# Maybe do just states first to get over abbreviations, then do the master list?
continent <- read.csv("~/Box/seed_twitter/data/continent.csv") # Some continents --
country <- read.csv("~/Box/seed_twitter/data/countries.csv") # All countries in the world, and some with different spellings
country$detect <- paste0('\\b', country$detect)
country$detect <- paste0(country$detect, '\\b')
state <- read.csv("~/Box/seed_twitter/data/states.csv") # These include US states, Canadian provinces, and Australian regions, regions of the UK, and 28 indian states
city <- read.csv("~/Box/seed_twitter/data/cities.csv") # These include US, Canadian, and European cities with > 100,000 and cities with LGUs, 25 largest cities in the African continent, 5 largest cities in India + the one mentioned a lot, 7 largest in Australia, Bangkok, Hong Kong and Beijing
city$detect.city <- paste0('\\b', city$detect.city)
city$detect.city <- paste0(city$detect.city, '\\b')
city$detect.city <- ifelse(city$detect.city == '\\b\\b', "", city$detect.city)
#region <- read.csv("~/Box/seed_twitter/data/regions.csv") 
region <- data.frame(
  detect = c("^Pacific Northwest$",	"^PNW$", "^Northeast$", "^Midwest$"),
  replace = c("West", "West", "Northeast", "Midwest"))

# Trimws
continent[,1:2] <- lapply(continent[,1:2], trimws)
country[,1:2] <- lapply(country[,1:2], trimws)
state[,1:2] <- lapply(state[,1:2], trimws)
city[,1:5] <- lapply(city[,1:5], trimws)
city[,3] <- gsub("^[[:space:]]*","",city[,3]) # Remove leading whitespaces


# Then assign each level...
continent.nodes <- data.table()
for (i in 1:nrow(continent)){
  continent.match <- data.table(grep(continent$detect[i], nodes$location))
  colnames(continent.match) <- "row"
  continent.match$continent <- continent$replace[i]
  #author.match$agency.row <- i
  continent.nodes <- rbind(continent.match, continent.nodes, fill=T)
}
continent.nodes <- unique(continent.nodes)

country.nodes <- data.table()
for (i in 1:nrow(country)){
     country.match <- data.table(grep(country$detect[i], nodes$location))
     colnames(country.match) <- "row"
     country.match$country <- country$replace[i]
     #author.match$agency.row <- i
     country.nodes <- rbind(country.match, country.nodes, fill=T)
}
country.nodes <- unique(country.nodes)

state.nodes <- data.table()
for (i in 1:nrow(state)){
  state.match <- data.table(grep(state$detect[i], nodes$location))
  colnames(state.match) <- "row"
  state.match$state <- state$replace[i]
  #author.match$agency.row <- i
  state.nodes <- rbind(state.match, state.nodes, fill=T)
}
state.nodes <- unique(state.nodes)

city.nodes <- data.table()
city.only <- city %>% filter(detect.city != "")
for (i in 1:nrow(city.only)){
  city.match <- data.table(grep(city.only$detect.city[i], nodes$location))
  colnames(city.match) <- "row"
  city.match$city <- city.only$replace.city[i]
  city.nodes <- rbind(city.match, city.nodes, fill=T)
}

city.nodes <- unique(city.nodes)

region.nodes <- data.table()
for (i in 1:nrow(region)){
  region.match <- data.table(grep(region$detect[i], nodes$location))
  colnames(region.match) <- "row"
  region.match$region <- region$replace[i]
  region.nodes <- rbind(region.match, region.nodes, fill=T)
}
region.nodes <- unique(region.nodes)


nodes.location <- left_join(nodes, continent.nodes, by = "row")
nodes.location <- left_join(nodes.location, country.nodes, by = "row")
nodes.location <- left_join(nodes.location, state.nodes, by = "row")
nodes.location <- left_join(nodes.location, city.nodes, by = "row")
nodes.location <- left_join(nodes.location, region.nodes, by = "row")


nodes.location <- unique(nodes.location)

# We have 1571 duplicates it seems
nrow(nodes.location) - nrow(nodes)

nolocation <- nodes.location %>% 
  filter(location == "")
# 6578 don't have location listed
nrow(nodes) - nrow(nolocation) #26001 have locations

nodetection <- nodes.location %>% 
  filter(is.na(country) & is.na(state) & 
           is.na(city) & is.na(continent) & location != "")
# Was have not been able to detect 2699 locations
nrow(nodetection)/(nrow(nodes) - nrow(nolocation))
# Cannot detect 10%


# Could improve upon: Universities, some cities (Peru, Sao Paolo, The Hague, Potsdam)

# Then use the rest of cities to really give detail across
## If you have this city, then have this state, country, continent

# Re-row number, in order to prevent merging problems later
nodes.location$row2 <- seq(1, nrow(nodes.location))

city.nodes2 <- data.table()
for(i in 1:nrow(city.only)){
    city.match <- data.table(grep(city.only$replace.city[i], nodes.location$city))
    colnames(city.match) <- "row2"
    for(j in city.match$row){
    if(nrow(city.match) >= 1 & is.na(nodes.location$state[j]) &
        is.na(nodes.location$country[j]) & is.na(nodes.location$continent[j])){
    city.match$city <- city.only$replace.city[i]
    city.match$state <- city.only$state[i]
    city.match$country <- city.only$country[i]
    city.match$continent <- city.only$continent[i]
    city.nodes2 <- rbind(city.match, city.nodes2, fill=T)
    }
    }
}
# This duplicates a lot and I am not sure why
city.nodes2 <- unique(city.nodes2)
  
# If you don't have a city but you have a state, then fill in the rest...
state.nodes2 <- data.table()
state.only <- city %>% select(-detect.city, -replace.city) %>% 
  filter(state != "") %>% unique()
for(i in 1:nrow(state.only)){
  state.match <- data.table(grep(state.only$state[i], nodes.location$state))
  colnames(state.match) <- "row2"
  for(j in state.match$row){
    if(nrow(state.match) >= 1 & is.na(nodes.location$city[j]) & 
       is.na(nodes.location$country[j]) & is.na(nodes.location$continent[j])){
      state.match$state <- state.only$state[i]
      state.match$country <- state.only$country[i]
      state.match$continent <- state.only$continent[i]
      state.nodes2 <- rbind(state.match, state.nodes2, fill=T)
  }
  }
}

# This duplicates a lot and I am not sure why
state.nodes2 <- unique(state.nodes2)

#If you have this country, but you have a state, then fill in the rest...
country.nodes2 <- data.table()
country.only <- city %>% select(-detect.city, -replace.city, -state, - USdistrict, -Usregion) %>% 
  filter(country != "") %>% unique()

for(i in 1:nrow(country.only)){
  country.match <- data.table(grep(country.only$country[i], nodes.location$country))
  colnames(country.match) <- "row2"
  for(j in country.match$row){
    if(nrow(country.match) >= 1 & is.na(nodes.location$city[j]) & 
       is.na(nodes.location$state[j]) & is.na(nodes.location$continent[j])){
      country.match$country <- country.only$country[i]
      country.match$continent <- country.only$continent[i]
      country.nodes2 <- rbind(country.match, country.nodes2, fill=T)
    }
  }
}
# This duplicates a lot and I am not sure why
country.nodes2 <- unique(country.nodes2)

# I reversed the order to improve something about this, but we still have 2300 more, but I think these are probably York or multiple cities listed


nodes.location2 <- left_join(nodes.location, country.nodes2, by = "row2")
nodes.location2$country <- ifelse(is.na(nodes.location2$country.x), 
                                  nodes.location2$country.y, nodes.location2$country.x)
nodes.location2$continent <- ifelse(is.na(nodes.location2$continent.x), 
                                    nodes.location2$continent.y, nodes.location2$continent.x)
nodes.location2 <- nodes.location2 %>% select(-continent.x, -country.x, -continent.y, -country.y)

nodes.location2 <- unique(nodes.location2)


# Keep repeating but may have to modify code just a little bit from above                         
nodes.location2 <- left_join(nodes.location2, state.nodes2, by = "row2")

nodes.location2$state <- ifelse(is.na(nodes.location2$state.x), 
                                nodes.location2$state.y, nodes.location2$state.x)
nodes.location2$country <- ifelse(is.na(nodes.location2$country.x), 
                                  nodes.location2$country.y, nodes.location2$country.x)
nodes.location2$continent <- ifelse(is.na(nodes.location2$continent.x), 
                                    nodes.location2$continent.y, nodes.location2$continent.x)
nodes.location2 <- nodes.location2 %>% select(-state.y, -continent.x, -country.x, -continent.y, -country.y)


nodes.location2 <- left_join(nodes.location2, city.nodes2, by = "row2")
nodes.location2$city <- nodes.location2$city.x

nodes.location2$state <- ifelse(is.na(nodes.location2$state.x), 
                                nodes.location2$state.y, nodes.location2$state.x)
nodes.location2$country <- ifelse(is.na(nodes.location2$country.x), 
                                  nodes.location2$country.y, nodes.location2$country.x)
nodes.location2$continent <- ifelse(is.na(nodes.location2$continent.x), 
                                  nodes.location2$continent.y, nodes.location2$continent.x)

# If row2 repeats, put x into y; or if row2 is duplicate but row is not...

nodes.location2 <- nodes.location2 %>% select(-state.x, -city.x, -city.y, -state.x.x, -state.y, -continent.x, -country.x, -continent.y, -country.y)
        
nodes.location2 <- unique(nodes.location2)

duplicates.row1 <- nodes.location2 %>%
  group_by(row) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(desc(n))
#1654

duplicates.row2 <- nodes.location2 %>%
  group_by(row2) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(desc(n))
#489


nodes.location2$duplicate <- ifelse(nodes.location2$row2 %in% duplicates.row2$row2, T, F)
table(nodes.location2$duplicate)

# If it is Cambridge UK but this system has tried to fill in Ontario or Mass as states, remove them
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                  nodes.location2$country == "United Kingdom" &
                                  nodes.location2$city == "Cambridge", "", nodes.location2$state)

# Remove the duplicates with Australia as continent be

nodes.location2$continent <- ifelse(nodes.location2$duplicate == T & 
                                  nodes.location2$continent == "Australia", "Oceania", 
                                  nodes.location2$continent)
nodes.location2 <- unique(nodes.location2)

duplicates.row2 <- nodes.location2 %>%
  group_by(row2) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(desc(n))
#245
nodes.location2$duplicate <- ifelse(nodes.location2$row2 %in% duplicates.row2$row2, T, F)

nodes.location.holder <- nodes.location2




nodes.location2 <- nodes.location.holder 
# If location is  "South Bend, in" then change state to Indiana
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                nodes.location2$location == "South Bend, in", 
                                "Indiana", nodes.location2$state)

# If location is  "Columbia, Mo\\. | Mizzou! Midwest! Columbia! Bk!|Columbia Mo\\.| Springfield, Mo" then change state to Missouri
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                nodes.location2$location %in% c("Columbia, Mo.","Mizzou! Midwest! Columbia! Bk!","Columbia Mo.","Springfield, Mo"), 
                                "Missouri", nodes.location2$state)

# If location is  "Columbus, OHIO" then change state to Ohio
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                nodes.location2$location == "Columbus, OHIO", 
                                "Ohio", nodes.location2$state)

# If state is Texas and country and United Kingdom, make state Scotland
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                nodes.location2$state == "Texas" &
                                nodes.location2$country == "United Kingdom", 
                                "Scotland", nodes.location2$state)

# If state is Alabama and country and United Kingdom, make state England
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                  nodes.location2$state == "Alabama" &
                                  nodes.location2$country == "United Kingdom", 
                                "England", nodes.location2$state)

# If country is sweden, continent is Europe
nodes.location2$continent <- ifelse(nodes.location2$duplicate == T & 
                                  nodes.location2$country == "Sweden", 
                                "Europe", nodes.location2$continent)

# if duplicate and New York City and United Kingdom are city and county, filter
nodes.location2 <- nodes.location2 %>% 
  filter(!(duplicate == T & city == "New York City" & country == "United Kingdom"))

# If country is UK and state and city are NA, filer out
nodes.location2 <- nodes.location2 %>% 
  filter(!(duplicate == T & is.na(city) & country == "United Kingdom"))

# If city is vancouver and state is washington, remove
nodes.location2 <- nodes.location2 %>% 
  filter(!(duplicate == T & city == "Vancouver" & state == "Washington"))

# If location is Brussels, Athens, Thessaloniki, and state is GA, remove
nodes.location2 <- nodes.location2 %>% 
  filter(!(duplicate == T & location == "Brussels, Athens, Thessaloniki" & state == "Georgia"))

nodes.location2 <- unique(nodes.location2)

duplicates.row2 <- nodes.location2 %>%
  group_by(row2) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(desc(n))
#102
nodes.location2$duplicate <- ifelse(nodes.location2$row2 %in% duplicates.row2$row2, T, F)

# Make any duplicates into Edinburgh then make it the UK one
nodes.location2 <- nodes.location2 %>% 
  filter(!(duplicate == T & city == "Edinburgh" & state == "Texas"))

# Sydney and New South Whales and UK, remove
nodes.location2 <- nodes.location2 %>% 
  filter(!(duplicate == T & city == "Sydney" & country == "United Kingdom"))

nodes.location2 <- nodes.location2 %>% 
  filter(!(duplicate == T & city == "Orange" & country == "United Kingdom"))

nodes.location2 <- nodes.location2 %>% 
  filter(!(duplicate == T & city == "New South Wales" & country == "United Kingdom"))
# Richmond
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                nodes.location2$location == "Richmond, North Yorkshire", 
                                "England", nodes.location2$state)
nodes.location2$country <- ifelse(nodes.location2$duplicate == T & 
                                  nodes.location2$location == "Richmond, North Yorkshire", 
                                "United Kingdom", nodes.location2$country)
nodes.location2$continent <- ifelse(nodes.location2$duplicate == T & 
                                    nodes.location2$location == "Richmond, North Yorkshire", 
                                  "Europe", nodes.location2$continent)
# Durham Region -- this is Canada; just remove it all
nodes.location2 <- nodes.location2 %>% 
  filter(!(duplicate == T & location == "Durham Region"))

# Athens, Ga. make Georgia
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                  nodes.location2$location == "Athens, Ga.", 
                                "Georgia", nodes.location2$state)
# Columbia, S.C. Make South Carolina
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                  nodes.location2$location == "Columbia, S.C.", 
                                "South Carolina", nodes.location2$state)
# Durham, N.C. make north carolina
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                  nodes.location2$location == "Durham, N.C.", 
                                "North Carolina", nodes.location2$state)
# District of Columbia, USA
nodes.location2$city <- ifelse(nodes.location2$duplicate == T & 
                                  nodes.location2$location == "District of Columbia, USA", 
                                "DC", nodes.location2$city)
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                  nodes.location2$location == "District of Columbia, USA", 
                                "District of Columbia", nodes.location2$state)

# Columbia City, in Alexandria, in both Indiana
nodes.location2$state <- ifelse(nodes.location2$duplicate == T & 
                                nodes.location2$location %in%c("Columbia City, in","Alexandria, in"), 
                                "Indiana", nodes.location2$state)

nodes.location2 <- unique(nodes.location2)

duplicates.row2 <- nodes.location2 %>%
  group_by(row2) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(desc(n))
#48
nodes.location2$duplicate <- ifelse(nodes.location2$row2 %in% duplicates.row2$row2, T, F)


# If you detect NEC Birmingham in location and state is Alabama, remove

# Lincoln/Aurora/Lindsay, Ne -- no city, Nebraska


fwrite(nodes.location2, "~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.location.csv")

#####################
# I AM ALMOST DONE CLEANING, BUT FOR NOW I SAVED MY PROGRESS ABOVE, AND I WANT TO STOP AND JUST REMOVE LOCATIONS FOR THOSE THAT I HAVEN'T FIGURED OUT YET
# If duplicate, then choose just the first one for now?

df <- fread("~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.location.csv")

duplicates.row1 <- df %>%
  group_by(row) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

df$duplicate <- ifelse(df$row %in% duplicates.row1$row, T, F)

df$city <- ifelse(df$duplicate == T, "", df$city)
df$state <- ifelse(df$duplicate == T, "", df$state)
df$country <- ifelse(df$duplicate == T, "", df$country)
df$continent <- ifelse(df$duplicate == T, "", df$continent)

df <- df %>% select(-row2, -duplicate) %>% unique()

regions <- city %>% 
  select(state, Usregion) %>% 
  filter(state != "", Usregion != "") %>% 
  unique() %>% 
  rename("region" = "Usregion")

df2 <- left_join(df, regions, by = "state")
df2$region <- ifelse(is.na(df2$region.y) & !is.na(df2$region.x), 
                    df2$region.x, df2$region.y)

df2 <- df2 %>% select(-region.x, -region.y)

df2$continent[df2$continent == "Africe"] <- "Africa"
df2$continent[df2$continent == "Australia"] <- "Oceania"

df2$location_gen <- ifelse(is.na(df2$region), df2$continent,
                    ifelse(df2$region == "", df2$continent, df2$region))
table(df2$location_gen)
fwrite(df2, "~/Box/seed_twitter/data/1.22.21.ASTA.node.names.ag.ids.location.unfinished.csv")
