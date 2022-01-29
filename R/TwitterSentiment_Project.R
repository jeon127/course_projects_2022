# load twitter library
# plotting and pipes - tidyverse
library(ggplot2)
library(dplyr)
library(tidytext) ######################################################################################### #Authenticating and connecting Twitter API
appname <- "text_mining_hult"
#API key
key <- "mLop2zq6NFeqny1eD2IFjEr6X"
#API secret
secret <- "MpDWuQQa00rU4dAV1arZljdHLo6o7mJofxF2BXGenYQrYmIFAd"
#Access token and secret
access_token <- "1461845253535588355-JfbkKOeuOKbcmNkSQtvSDxIiUj0wa0" access_secret <- "7H5BgdeyswnelD6F9sG2DYwoquLPcFn8CaZvIvJHjY05X"
#create token named "twitter_token" twitter_token <- create_token(
app = appname, consumer_key = key, consumer_secret = secret,

access_token = access_token, access_secret = access_secret)
######################################################################################### #check to see if the token is loaded
library(rtweet)
get_token() ######################################################################################### #posting a tweet from R for fun
post_tweet("Tweeting from #R for fun") #your tweet has been posted!
######################################################################################### #EXTRACTING TWEETS FOR RUNNING
#search for 500 tweets using the #running hashtag running_tweets <- search_tweets(q = "#running",
n = 500, lang = "en", include_rts = FALSE) #view the first 3 rows of the dataframe
#head(running_tweets, n = 3)
#see the length of the tweets in the running_tweets datframe #n.tweet <- length(running_tweets)
######################################################################################### #EXTRACTING TWEETS FOR CYCLING
#search for 500 tweets using the #cycling hashtag cycling_tweets <- search_tweets(q = "#cycling",
n = 500, lang = "en", include_rts = FALSE) #view the first 3 rows of the dataframe
#head(cycling_tweets, n = 3)
######################################################################################### #EXTRACTING TWEETS FOR SWIMMING
#search for 500 tweets using the #swimming hashtag swimming_tweets <- search_tweets(q = "#swimming",
n = 500, lang = "en", include_rts = FALSE) #view the first 3 rows of the dataframe
#head(swimming_tweets, n = 3)
#########################################################################################
#STARTING "SENTIMENT ANALYSIS" and CLEANING THE DATA: PROCESS EACH SET OF TWEETS INTO TIDY TEXT (OR CORPUS OBJECTS)
library(tidyr)
tweets.running_tweets = running_tweets %>% select(screen_name, text) tweets.running_tweets
tweets.cycling_tweets = cycling_tweets %>% select(screen_name, text) tweets.cycling_tweets
tweets.swimming_tweets = swimming_tweets %>% select(screen_name, text)
tweets.swimming_tweets #########################################################################################

#USE PRE-PROCESSING TEXT TRANSFORMATIONS TO CLEAN UP TWEETS (changing the case, removing links, punctuations, stop words, etc)
#STEP 1) PRE-PROCESSING TEXT FOR RUNNING_TWEETS head(tweets.running_tweets$text)
#removing http elements (link to web pages) manually
tweets.running_tweets$stripped_text1 <- gsub("https://t.co/","",tweets.running_tweets$text)
#using unnest_tokens to convert to remove punctuation, change to lowercase, and add an id for tweets tweets.running_tweets_stem <- tweets.running_tweets %>%
select(stripped_text1) %>% unnest_tokens(word, stripped_text1)
head(tweets.running_tweets_stem)
#removing stop words from list of words and naming it with "cleaned_tweets" cleaned_tweets.running_tweets <- tweets.running_tweets_stem %>%
anti_join(stop_words) head(cleaned_tweets.running_tweets)
head(tweets.running_tweets$text)
#STEP 2) PRE-PROCESSING TEXT FOR CYCLING_TWEETS head(tweets.cycling_tweets$text)
#removing http elements (link to web pages) manually tweets.cycling_tweets$stripped_text1 <- gsub("https://t.co/","",tweets.cycling_tweets$text)
#using unnest_tokens to convert to remove punctuation, change to lowercase, and add an id for tweets tweets.cycling_tweets_stem <- tweets.cycling_tweets %>%
select(stripped_text1) %>% unnest_tokens(word, stripped_text1)
head(tweets.cycling_tweets_stem)
#removing stop words from list of words and naming it with "cleaned_tweets" cleaned_tweets.cycling_tweets <- tweets.cycling_tweets_stem %>%
anti_join(stop_words) head(cleaned_tweets.cycling_tweets)
head(tweets.cycling_tweets$text)
#STEP 3) PRE-PROCESSING TEXT FOR SWIMMING_TWEETS head(tweets.swimming_tweets$text)
#removing http elements (link to web pages) manually
tweets.swimming_tweets$stripped_text1 <- gsub("https://t.co/","",tweets.swimming_tweets$text)
#using unnest_tokens to convert to remove punctuation, change to lowercase, and add an id for tweets tweets.swimming_tweets_stem <- tweets.swimming_tweets %>%
select(stripped_text1) %>% unnest_tokens(word, stripped_text1)
head(tweets.swimming_tweets_stem)

#removing stop words from list of words and naming it with "cleaned_tweets" cleaned_tweets.swimming_tweets <- tweets.swimming_tweets_stem %>%
anti_join(stop_words) head(cleaned_tweets.swimming_tweets)
head(tweets.swimming_tweets$text)
######################################################################################### #WORD FREQUENCY: FINDING THE TOP 10 COMMONLY USED WORDS IN EACH SET OF TWEETS
#Top 10 words in #running tweets cleaned_tweets.running_tweets %>%
count(word, sort = TRUE) %>% top_n(10) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() +
  xlab(NULL) + coord_flip() + theme_classic() +
  labs(x = "Unique words",
       y = "Count",
       title = "Unique word counts found in #running tweets")
#Top 10 words in #cycling tweets cleaned_tweets.cycling_tweets %>%
count(word, sort = TRUE) %>% top_n(10) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() +
  xlab(NULL) + coord_flip() + theme_classic() +
  labs(x = "Unique words",
       y = "Count",
       title = "Unique word counts found in #cycling tweets")
#Top 10 words in #swimming tweets cleaned_tweets.swimming_tweets %>%
count(word, sort = TRUE) %>% top_n(10) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() +
  xlab(NULL) + coord_flip() + theme_classic() +
  labs(x = "Unique words",
       y = "Count",
       title = "Unique word counts found in #swimming tweets")

######################################################################################### #CONTINUING SENTIMENT ANALYSIS TO EVALUATE EMOTION IN TEXT
library(textdata)
#ATTEMPTING "BING SENTIMENT ANALYSIS" get_sentiments("bing") %>% filter(sentiment=="positive") get_sentiments("bing") %>% filter(sentiment=="negative")
#ATTEMPTING "AFINN SENTIMENT ANALYSIS" get_sentiments("afinn") %>% filter(value=="3") get_sentiments("afinn") %>% filter(value=="-3")
#BING SENTIMENT ANALYSIS CONTINUED: THE RESULT RETURNS A TIBBLE FOR CLEANED RUNNING TWEETS bing_running_tweets = cleaned_tweets.running_tweets %>%
inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()
#BING SENTIMENT ANALYSIS CONTINUED: THE RESULT RETURNS A TIBBLE FOR CLEANED CYCLING TWEETS bing_cycling_tweets = cleaned_tweets.cycling_tweets %>%
inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()
#BING SENTIMENT ANALYSIS CONTINUED: THE RESULT RETURNS A TIBBLE FOR CLEANED SWIMMING TWEETS bing_swimming_tweets = cleaned_tweets.swimming_tweets %>%
inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()
######################################################################################### #CREATING VISUALIZATION FOR A SIDE BY SIDE COMPARISON OF POSITVE AND NEGATIVE EMOTION IN TWEETS FOR SENTIMENT ANALYSIS
#Comparative visualization of emotions using the tweet hashtag #running bing_running_tweets %>%
group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + labs(title = "Tweets containing '#running'",
                                                                                                                                                                y = "Contribution to sentiment",
                                                                                                                                                                x = NULL) +
  coord_flip() + theme_bw()
#Comparative visualization of emotions using the tweet hashtag #swimming bing_swimming_tweets %>%
group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +
  
  labs(title = "Tweets containing '#swimming'", y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()
#Comparative visualization of emotions using the tweet hashtag #cycling bing_cycling_tweets %>%
group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + labs(title = "Tweets containing '#cycling'",
                                                                                                                                                                y = "Contribution to sentiment",
                                                                                                                                                                x = NULL) +
  coord_flip() + theme_bw()
######################################################################################### #N-GRAMS USING BIGRAM ANALYSIS AND NETWORK DEFINITION
#Creating a bigram for '#running' tweets bi.gram.words.running <- tweets.running_tweets %>%
unnest_tokens(
  input = stripped_text1, output = bigram, token = 'ngrams',
  n=2
) %>%
  filter(! is.na(bigram))
bi.gram.words.running %>% select(bigram) %>% head(10)
#Creating a bigram for '#cycling' tweets bi.gram.words.cycling <- tweets.cycling_tweets %>%
unnest_tokens(
  input = stripped_text1, output = bigram, token = 'ngrams',
  n=2
) %>%
  filter(! is.na(bigram))
bi.gram.words.cycling %>% select(bigram) %>% head(10)
#Creating a bigram for '#swimming' tweets bi.gram.words.swimming <- tweets.swimming_tweets %>%
unnest_tokens(
  
  input = stripped_text1, output = bigram, token = 'ngrams',
  n=2
) %>%
  filter(! is.na(bigram))
bi.gram.words.swimming %>% select(bigram) %>%
  head(10)
######################################################################################### #SEEING IF OUR BIGRAMS HAVE STOP WORDS SO WE CAN REMOVE THEM
bi.gram.words.running %>%
  count(bigram, sort = TRUE)
bi.gram.words.cycling %>% count(bigram, sort = TRUE)
bi.gram.words.swimming %>% count(bigram, sort = TRUE)
######################################################################################### #REMOVING STOP WORDS FROM OUR '#RUNNING' BIGRAM
library(tidyr)
bigrams_separated_running <- bi.gram.words.running %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered_running <- bigrams_separated_running%>% filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
#CREATING NEW BIGRAM WITHOUT STOP WORDS bigram_counts_running <- bigrams_filtered_running %>%
count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_running ######################################################################################### #REMOVING STOP WORDS FROM OUR '#CYCLING' BIGRAM
library(tidyr)
bigrams_separated_cycling <- bi.gram.words.cycling %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered_cycling <- bigrams_separated_cycling %>% filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
#CREATING NEW BIGRAM WITHOUT STOP WORDS bigram_counts_cycling <- bigrams_filtered_cycling %>%
count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_cycling ######################################################################################### #REMOVING STOP WORDS FROM OUR '#SWIMMING' BIGRAM
library(tidyr)
bigrams_separated_swimming <- bi.gram.words.swimming %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_swimming <- bigrams_separated_swimming %>% filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
#CREATING NEW BIGRAM WITHOUT STOP WORDS bigram_counts_swimming <- bigrams_filtered_swimming %>%
count(word1, word2, sort = TRUE) #want to see the new bigrams bigram_counts_swimming
######################################################################################### ######################################################
####### VISUALISING OUR BIGRAM NETWORKS ################# ######################################################
#install.packages("igraph")
library(igraph)
bigram_graph_running<- bigram_counts_running %>%
  filter(n > 10) %>%
  graph_from_data_frame() bigram_graph_running
#install.packages("ggraph")
library(ggraph) ggraph(bigram_graph_running, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+ geom_node_text(aes(label=name), vjust =1, hjust=1)
######################################################
#install.packages("igraph")
library(igraph)
bigram_graph_cycling<- bigram_counts_cycling %>%
  filter(n > 10) %>%
  graph_from_data_frame() bigram_graph_cycling
#install.packages("ggraph")
library(ggraph) ggraph(bigram_graph_cycling, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+ geom_node_text(aes(label=name), vjust =1, hjust=1)
######################################################
#install.packages("igraph")
library(igraph)
bigram_graph_swimming<- bigram_counts_swimming %>%
  filter(n > 10) %>%
  graph_from_data_frame() bigram_graph_swimming
#install.packages("ggraph") library(ggraph)

ggraph(bigram_graph_swimming, layout = "fr") + geom_edge_link()+
  geom_node_point()+ geom_node_text(aes(label=name), vjust =1, hjust=1)