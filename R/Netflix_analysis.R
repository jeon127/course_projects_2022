# Downloading Netflix data from the tidytuesdayR package
#install.packages("tidytuesdayR")
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
write.csv(netflix, "/Users/kaitlingarcia/OneDrive - Hult Students/Hult MBAN Programme/Text Analytics/Datasets/netflix_data.csv")

library(dplyr)
library(stringr)

#let's look at the data
View(netflix[1:40,])
head(netflix)
unique(netflix$country)
unique(netflix$title)
names(netflix)

# renaming the columns - description to text
colnames(netflix)[12] <- "text"

# ready for tokenization
# View(netflix)
library(tidytext)
tidy_netflix <- netflix %>% unnest_tokens(word, text)
# View(netflix)
# View(tidy_netflix)

# removing stop words
data("stop_words")
netflix_no_stop <- tidy_netflix %>% anti_join(stop_words) #tokenizing the unstructured data
netflix_no_stop %>% count(word, sort = TRUE) #counting remaining words

# plotting the token frequencies
library(ggplot2)
freq_hist <- netflix_no_stop %>% 
  count(word, sort = T) %>% 
  filter(n>200) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n)) + geom_col() + xlab(NULL) + coord_flip()

print(freq_hist)

##################################################################################################################################
#comparing tokens from India, Brazil with United States

usa <- netflix %>%
  filter(country== "United States")

data("stop_words")
tidy_usa <- usa %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# print(tidy_usa)

### creating a tidy format for Brazil movies
brazil <- netflix %>%
  filter(country== "Brazil")

tidy_brazil <- brazil %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_brazil)

### creating a tidy format for India movies
india <- netflix %>%
  filter(country== "India")

tidy_india  <- india %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_india)

##################################################################################################################################
# We want to combine all the datasets and do frequencies 

library(tidyr)
frequency <- bind_rows(mutate(tidy_usa, author="United States"),
                       mutate(tidy_brazil, author= "Brazil"),
                       mutate(tidy_india, author="India")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Brazil`, `India`)

# let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`United States`, 
                      color = abs(`United States`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.2, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "red", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "United States", x=NULL)

# performing the correlation test
# can use this code to compare the effect of different platforms 

cor.test(data=frequency[frequency$author == "Brazil",],
         ~proportion + `United States`)

cor.test(data=frequency[frequency$author == "India",],
         ~proportion + `United States`)

##################################################################################################################################
# Sentiment analysis

library(textdata)
library(dplyr)

afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

table(afinn$value)
View(afinn %>% group_by(word) %>% filter(value == "-5"))  
nrc %>% filter(word == "abandonment") %>% group_by(sentiment)
view(unique(bing$sentiment))
# bing %>% group_by(word) %>%  filter(sentiment == "positive")
# bing %>% group_by(word) %>%  filter(sentiment == "negative")

# combining all the lexixons to make one single sentiment dataframe
sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon="nrc"),
                        mutate(bing, lexicon="bing"))

View(sentiments) # has NAs
cbind(lapply(lapply(sentiments, is.na), sum)) # value & sentiment cols have NAs
# NAs can remain in this case - they do not affect the analysis!!

# Sentiment analysis on Netflix Data
netflix_token <- netflix %>%
  unnest_tokens(word, text)

# use get_sentiments to load the specific sentiment library - NRC & surprise sentiments only
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

# inner joining the India movies and the surprise sentiments
netflix_token %>%
  filter(country == "India") %>%
  inner_join(nrcsurprise) %>% # combining the surprise with un-nested tokenised netlfix data
  count(word, sort=T)

unique(nrcsurprise)

# Comparing different sentiment libraries on Netflix 
india <- netflix_token %>%
  filter(country == "India")

afinn_india <- india %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN") 

# india %>%
#   inner_join(get_sentiments("afinn"))%>%
#   mutate(method="AFINN") %>% 
#   count(word, sort = T)

# Insights from AFINN
# matched words are mostly negative words? afinn value is -327 

# combing both bing & nrc libraries together with India data
bing_and_nrc <- bind_rows(
  india %>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  india %>%
    inner_join(get_sentiments("nrc") %>%
    filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
    count(method,  sentiment) %>%
    spread(sentiment, n, fill=0) %>%
    mutate(sentiment = positive-negative)
View(bing_and_nrc)

# rbind(afinn_india, bing_and_nrc)

bind_rows(afinn_india, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# both AFINN & BING indicated most common sentiment around movies negative, but bing is more negative because of the 0-1 scale
# NRC is positive, NRC is the biggest lexicon - It's generally positive because we are only considering the positive and negative sentiments

# Plotting the most common positive and negative words
bing_counts <- india %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
# bing_counts
india %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
india %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort=T) %>%
  ungroup()

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

##################################################################################################################################
# TF-IDF - helps you check for most unique and most frequent tokens in a collection of documents
# TF_IDF ON NETFLIX  - First group by the country 

netflix_token_country <- netflix %>%
  unnest_tokens(word, text) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words <- netflix_token_country %>% 
  group_by(country) %>%
  summarise(total=sum(n))
# unique(total_words$country)

netflix_words <- left_join(netflix_token_country, total_words)%>%
  filter(country %in% c("United States", "Mexico", "India"))

print(netflix_words)

library(ggplot2)
ggplot(netflix_words, aes(n/total, fill = country))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")

# the tails represent high proportions are stop words
# the lowest proportions highest count of frequency indicate unique words
# we are really interested in the not so common words i.e. the tall bars

##################################################################################################################################
# ZIPF's law 

freq_by_rank <- netflix_words %>%
  group_by(country) %>%
  mutate(rank = row_number(), # because the data is already ordered sorted in descending
         `term frequency` = n/total)

freq_by_rank %>% group_by(rank) %>% arrange(desc(rank))


#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=country))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = T)+
  scale_x_log10()+
  scale_y_log10()

# upper left is most commonly repeated words
# lower right hand corner is the highest rank and least repeated words

##################################################################################################################################
# TF_IDF 
# TF_IDF 

country_words <- netflix_words %>%
  bind_tf_idf(word, country, n)

country_words %>% filter(word == "mumbai") # we get all the zeros because we are looking at stop words ... too common

# sort tf_idf column by descending to see the most unique tokens
country_words %>%
  arrange(desc(tf_idf)) %>% 
  filter(country == "Mexico") 

country_words %>% filter(word == "father")
country_words %>% arrange(desc(tf_idf))
# the higher the tf_idf value the more important, unique the word
# but if n is also high then you need to be careful - they might obvious or stop words 
# for example: mexico, mexican having high tf_idf  and n means that they are not that important


# looking at the graphical approach:
country_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()

##################################################################################################################################
# DTM 

library(tidyr)
library(magrittr)
library(tidyverse)
library(tidytext)

dtm_netflix <- netflix %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(country, word) %>% 
  cast_dtm(country, word, n) # to make it all numeric/ 100% sparsity - is good - THIS IS A DTM

view(unlist(dtm_netflix$dimnames)) 

dtm_netflix_review <- netflix %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, country) %>% 
  cast_dtm(country, word, n) %>% 
  as.matrix()
  
dtm_netflix_review[1:4, 1:10]
##################################################################################################################################
# Latent Dirchlet Algorithm - LDA

# There are two principles:
#1. Every document is a combination of multiple topics
#2. Every topic is a combination of multiple words
#3. Input of LDA is a DTM


library(topicmodels)
netflix_lda <- LDA(dtm_netflix_review, k=2, control = list(seed=123))
glimpse(netflix_lda) # but this is difficult to comprehend, so we need to use the tidy() function

library(tidytext)
netflix_tidy <- tidy(netflix_lda, matrix="beta")
netflix_tidy

# probability of 0 being topic 1 or topic 2 
# 0 is more likely to be in topic 1

library(ggplot2)
library(dplyr)
library(tidyr)

top_terms_netlfix <- netflix_tidy %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_netlfix

#lets plot the term frequencies by topic
# most probable tokens per topic 
# topic 1 speaks about financial market
# topic 2 speaks about politics and political power play perhaps

top_terms_netlfix %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales =   "free") +
  coord_flip()

#lets calculate the relative difference between the betas for words in topic 1 and words in topic 2

beta_spread_netflix <- netflix_tidy %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1> 0.001 | topic2 > 0.001) %>%
  mutate(log_rate = log2(topic2/topic1)) # helps you find those token that have a big diff. between topic 1 & topic 2

beta_spread_netflix
# ?paste0

# helps you the probability of a topic being in document 
netflix_tidy_gamma <- tidy(netflix_lda, matrix="gamma")

gamma_spread_netflix <- netflix_tidy_gamma %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, gamma) %>%
  filter(topic1> 0.001 | topic2 > 0.001) %>%
  mutate(log_rate = log2(topic2/topic1))
gamma_spread_netflix
# higher the -ve value topic 1 and vice-versa

##################################################################################################################################
# n-grams

library(dplyr)
library(tidytext)
library(tidyr)
library(tidytuesdayR)

netflix_bigrams <- netflix %>% unnest_tokens(bigram, text, token = "ngrams", n=2)

View(netflix_bigrams)  #We want to see the bigrams (words that appear together, "pairs")

netflix_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- netflix_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# removing stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_netflix_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
View(bigram_netflix_counts) 
names(bigrams_filtered)

bigrams_filtered %>% group_by(country) %>% count(word1, word2, sort = T) # same result
bigrams_filtered  %>% count(country, word1, word2, sort = T) # same result

###########################################################
###### What if we are interested in the most common #######
################ 4 consecutive words - quadro-gram ########
###########################################################
quadrogram_netflix <- netflix %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

View(quadrogram_netflix)  

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

bigram_united <- bigrams_separated %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(country, bigram) %>%
  bind_tf_idf(bigram, country, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

##### lets do the same for a quadrogram

quadrogram_united <- quadrogram_netflix %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  count(country, quadrogram) %>%
  bind_tf_idf(quadrogram, country, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf

######################################################
######## visualising negated words ###################
###### negated words in sentiment analysis ###########
######################################################

negation_tokens <- c("no", "never", "without", "not")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(afinn, by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

negated_words

##############################################
#### we can visuals the negated words ####
negated_words_plot <- function(x){
  negated_words %>%
    filter(word1 == x) %>%
    mutate(contribution = n* value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*value, fill = n*value >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment score* number of occurences")+
    coord_flip()
}#closing the negated_words_plot function

negated_words_plot(x="not")
negated_words_plot(x="no")
negated_words_plot(x="without")

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
library(igraph)
bigram_netflix_graph <- bigram_netflix_counts %>%
  filter(n>10) %>%
  graph_from_data_frame()

bigram_netflix_graph

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_netflix_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)












