#######################################
####### MBAN2 HULT 2021


library(mongolite)
library(tidytext)
library(tidyverse)
library(tidyr)
library(tidytuesdayR)
library(stringr)
library(textreadr)
library(pdftools)
library(textshape)
library(twitteR)
library(tm)
library(ggplot2)
library(scales)
library(magrittr) 
library(dplyr)
library(gutenbergr)
library(Matrix)
library(textdata)
library(igraph)
library(ggraph)
library(widyr)
library(topicmodels)
library(gutenbergr)
library(quanteda)
library(quanteda.textmodels)
library(RColorBrewer)

# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://Khizer_sultan:khizersultan@cluster0.g4nv9.mongodb.net/myFirstDatabase?retryWrites=true&w=majority'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url =connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()

new_df <- c((airbnb_all$room_type),(airbnb_all$property_type))

################################
# N grams
###############################

airbnb_bigram <- airbnb_all %>%
  unnest_tokens(bigram, summary, token = "ngrams", n=2)

airbnb_bigram

airbnb_bigram %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

airbnb_bigrams_separated <- airbnb_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

airbnb_bigrams_filtered <- airbnb_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
airbnb_bigram_count <- airbnb_bigrams_filtered %>%
  count(word1, word2, property_type, sort = TRUE)
#want to see the new bigrams
airbnb_bigram_count

library("writexl")
write_xlsx(airbnb_bigram_count,"/Users/khizersultan/Desktop/new\\data.xlsx")

Apartment <- subset(airbnb_bigram_count, property_type == "Apartment")
House <-subset(airbnb_bigram_count, property_type == "House")
Condominium <- subset(airbnb_bigram_count, property_type == "Condominium")

filter(Apartment, n > 50)
filter(House, n > 6)
filter(Condominium, n > 6)


###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram #################
###########################################################

bigram_united <- airbnb_bigrams_separated %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(room_type, bigram) %>%
  bind_tf_idf(bigram, room_type, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

#lets plot bigram graph
airbnb_graph <- airbnb_bigram_count %>%
  filter(n > 50) %>%
  graph_from_data_frame()

airbnb_graph

ggraph(airbnb_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#sentiment analysis for airbnb room_types

airbnb_token <- airbnb_all %>%
  unnest_tokens(word, summary)

nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

#inner joining the joy sentiments
privateroom <- airbnb_token %>%
  filter(room_type == "Private room") %>%
  inner_join(nrcjoy) %>%
  count(word, sort=T)

#exporting file to excel to create tableau visuals
library("writexl")
write_xlsx(privateroom,"/Users/khizersultan/Desktop/new\\data.xlsx")

##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts <- airbnb_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

#exporting file to excel to create tableau visuals
library("writexl")
write_xlsx(bing_counts,"/Users/khizersultan/Desktop/new\\data.xlsx")

#exporting file to excel to create tableau visuals
### do for nrc
nrc_counts <- airbnb_token %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

nrc_counts

library("writexl")
write_xlsx(nrc_counts,"/Users/khizersultan/Desktop/new\\data.xlsx")

#visualizing top 10 negative and positive words
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

#visualizing nrc sentiments
nrc_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#for tableau use only

df <- airbnb_all %>%
  unnest_tokens(word, summary) %>%
  anti_join(stop_words) %>%
  count(word, room_type) %>%
  inner_join(get_sentiments("bing"))

library("writexl")
write_xlsx(df,"/Users/khizersultan/Desktop/new\\data.xlsx")

#correlograms to search for correlation between private rooms with shared and enitre apartments
#############################################
### creating a tidy format for private rooms
#############################################
private_tidy <- airbnb_all %>%
  filter(room_type == "Private room")

tidy_private <- private_tidy %>%
  unnest_tokens(word, summary) %>%
  anti_join(stop_words)
print(tidy_private)

### creating a tidy format for Entire home/apt
private_apartment <- airbnb_all %>%
  filter(room_type == "Entire home/apt")

apartment_tidy <- private_apartment %>%
  unnest_tokens(word, summary) %>%
  anti_join(stop_words)
print(apartment_tidy)

### creating a tidy format for Shared room
shared_tidy <- airbnb_all %>%
  filter(room_type == "Shared room")

tidy_shared <- shared_tidy %>%
  unnest_tokens(word, summary) %>%
  anti_join(stop_words)
print(tidy_shared)

#############################################
####We want to combine all the datasets and do frequencies 
#############################################
library(tidyr)
frequency <- bind_rows(mutate(tidy_private, room_type = "Private room"),
                       mutate(apartment_tidy, room_type = "Entire home/apt"),
                       mutate(tidy_shared, room_type= "Shared room")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(room_type, word) %>%
  group_by(room_type) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(room_type, proportion) %>%
  gather(room_type, proportion, `Entire home/apt`, `Shared room`)

#let's plot the correlograms:
ggplot(frequency, aes(x=proportion, y=`Private room`, 
                      color = abs(`Private room`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~room_type, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Private room", x=NULL)

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=frequency[frequency$room_type == "Entire home/apt",],
         ~proportion + `Private room`)

cor.test(data=frequency[frequency$room_type == "Shared room",],
         ~proportion + `Private room`)

