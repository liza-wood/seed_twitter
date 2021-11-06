A repository outlining the steps in scraping tweets from the Twitter API, plotting and analyzing networks based on retweets and mentions, and analyzing text data.

**1_edgelist_creation.R**

Identifies followers and followees of ASTA (@better_seed) to create a list of 3,295 accounts, which is then used to pull all tweets from those users on January 26, 2020. These Tweets are then limited to be between 2018-2020 for full Twitter histories of users (given Twitter API's limit of 3200 tweets per user), and then the text of the tweets was cleaned. An edgelist was then created using the tweet and mention data, resulting in `10.31.20.ASTAretweetandmention.edgelist.recent.csv`

**2_text_tagging.R**

This creates a regex dictionary for different themed words for agriculture, weather, climate, and breeding. It then uses stringr to detect these presence of any of these words, and create a T/F column for each subject as a tweet attribute. `10.31.20.ASTAretweetandmention.edgelist.recent.attr.csv`

**3_community_detect.R**

Filters for only the agriculture related tweets, and then tests the community walktrap function in igraph to identify communities. I identify 8 communities once removing those with less than 1 tweet or retweet. I can then get 75% of the sample into communities. I create a `10.31.20.ASTAretweetandmention.edgelist.recent.ag.cw.csv` edgelist, which now has refined the group down to the 8 communities from the community walktrap.

**4_nodelist_creation.R**

Because the edgelist is made up of many more users than the original sample, I need to again pull their information to get the bios and location information of the users. Once I have this information, I can assign names to the user ids based on the list, which I can then use to overwrite future lists for ease. 

**5_bio_classification.R**

I read in the csv with the assigned IDs. In this case, because I assigned IDs from an old list, I will join it with my new list. In this process, there is only a very small difference. I am then able to run the knn algorithm to try to train my assignments. This classification approach is in process.

**6_location_matching.R**

This script takes the location names identified in Twitter profiles and iteratively assigns locations from dictionaries of major cities and countries around the world.

**7_text_analysis.R**
