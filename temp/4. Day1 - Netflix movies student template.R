#########################################
### Example with Netflix data######
#########################################
install.packages("tidytuesdayR")
library(tidytuesdayR)
#tuesdata <- tidytuesdayR::tt_load('2021-04-20')
#netflix <- tuesdata$netflix_titles

library(dplyr)
library(stringr)

#let's look at the data
View(netflix[1:40,])
unique(netflix$title)
unique(netflix$country)
##############################
##############################
#need to rename the description variables as "text"
#this is good coding practice
colnames(netflix)[13] <- "text"
#We need one token per row , 
#so that the structure is simialar from our bacon tokenizing script.

library(tidytext)
tidy_netflix <- netflix %>%
                  unnest_tokens(word, text)
print(tidy_netflix)

 #removing stop words
data(stop_words)
netflix_no_stop <- tidy_netflix %>%
                anti_join(stop_words)
print(netflix_no_stop)
#printing the count frequencies for each token without stop words
netflix_no_stop %>%
  count(word, sort=TRUE)

#plotting the token frequencies:
library(ggplot2)
freq_hist <-netflix_no_stop %>%
  count(word, sort=TRUE) %>%
  filter(n>200) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

print(freq_hist)
