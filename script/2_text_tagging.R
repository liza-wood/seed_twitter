library(data.table)
library(dplyr)
library(stringr)

tweets <- fread("/Users/lizawood/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.csv")

# AG TWEETS -------

# These include more classic ag words, sustainable ag words, and weather related words
ag.words <- c("\\b[Aa]erat[a-z]+\\b", "\\b[Aa]gri[a-z]+\\b", "\\b[Bb]reed[a-z]*\\b", "\\b[Cc]rop[a-z]*\\b", "\\b[Cc]ultiva[a-z]+", "\\b[Ff]arm[a-z]*\\b", "\\b[Ff]ertiliz[a-z]+\\b", "\\b[Ff]ood[a-z]*\\b", "\\b[Gg]ermplasm\\b", "\\b[Gg]ene[a-z]*\\b", "\\b[Gg][Mm][Oo]\\b", "\\b[Gg]rain[a-z]*\\b", "\\b[Gg]roundwater\\b", "\\b[Gg]rower\\b", "\\b[Hh]erbicide\\b", "\\b[Hh]orticultur[a-z]+\\b", "\\b[Hh]usbandry\\b", "[Ii]rrigat[a-z]+\\b", "\\b[Ll]and[a-z]+", "\\b[Ll]ivestock", "\\b[Mm]anure\\b", "\\b[Mm]echaniz[a-z]+\\b", "\\b[Mm]onoculture[s]*\\b", "\\b[Nn]itrat[a-z]+\\b", "\\b[Nn]utrient[s]*\\b", "\\b[Pp]est[a-z]*\\b", "\\b[Rr]ural[a-z]*\\b", "\\b[Ss]eed[s]*\\b", "\\b[Ss]oil[s]*\\b", "\\b[Tt]ill[a-z]*\\b", "\\b[Uu][Ss][Dd][Aa]\\b", "\\b[Vv]ariet[a-z]+\\b", "\\b[Ww]eed[s]*\\b", "\\b[Aa]groeco[a-z]*\\b", "\\b[Bb]iocide[s]*\\b", "\\b[Bb]iodivers[a-z]+\\b", "\\b[Bb]iological\\b", "\\b[Cc]limat[a-z]+\\b", "\\b[Cc]ompost[s]*\\b", "\\b[Cc]onservation[a-z]*\\b", "\\b[Cc]ooperative[a-z]*\\b",  "\\b[Ee]cosystem[s]*\\b", "\\b[Ee]mission[s]*\\b", "\\b[Ee]nvironment[a-z]*\\b", "\\b[Ff]orest[s]*\\b", "\\b[Hh]abitat[s]*\\b", "\\b[Ll]ocal[a-z]*-source[a-z]*\\b", "\\b[Nn]atural\\b", "\\b[Oo]rganic[s]*\\b", "\\b[Pp]ollut[a-z]+\\b", "\\b[Ss]ustainab[a-z]+\\b", "\\b[Ww]ildlife\\b", "\\b[Dd]rought[s]*\\b", "\\b[Dd]ry", "\\b[Ff]lood[a-z]*\\b", "\\b[Hh]eat[a-z]*\\b", "\\b[Hh]urrican[s]*\\b", "\\b[Ff]reez[a-z]*\\b", "[Ff]ire[s]*\\b", "\\b[Rr]ain[a-z]*\\b", "\\b[Ss]torm[s]*\\s")
ag.words <- paste(ag.words, collapse = "|")

tweets$ag_tweet <- str_detect(tweets$stripped_text, ag.words)
table(tweets$ag_tweet)

climate.word <- "\\b[Cc]limat[a-z]+\\b"
tweets$climate_tweet <- str_detect(tweets$stripped_text, climate.word)
table(tweets$climate_tweet)

weather.words <- c("\\b[Rr]ain[a-z]*\\b", "\\b[Dd]rought[s]*\\b", "\\b[Dd]ry", "\\b[Ff]lood[a-z]*\\b", "\\b[Hh]eat[a-z]*\\b", "\\b[Ff]reez[a-z]*\\b", "[Ff]ire[s]*\\b", "\\b[Ss]torm[s]*\\s", "\\b[Hh]urrican[s]*\\b")
weather.words <- paste(weather.words, collapse = "|")
tweets$weather_tweet <- str_detect(tweets$stripped_text, weather.words)
table(tweets$weather_tweet)

breeding.words <- c("\\b[Bb]reed[a-z]*\\b", "\\b[Ss]eed[s]*\\b", "\\b[Gg]ermplasm\\b", "\\b[Gg]ene[a-z]*\\b", "\\b[Vv]ariet[a-z]+\\b")
breeding.words <- paste(breeding.words, collapse = "|")
tweets$breeding_tweet <- str_detect(tweets$stripped_text, breeding.words)
table(tweets$breeding_tweet)

#Only 3,238 that include climate and breeding comments
table(tweets$breeding_tweet, tweets$climate_tweet)

# Added .20
fwrite(tweets, "/Users/lizawood/Box/seed_twitter/data/10.31.20.ASTAretweetandmention.edgelist.recent.attr.csv")










