#Downloading Netflix data from the tidytuesdayR package

install.packages("tidytuesdayR")
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
#write.csv(netflix, "/Users/kaitlinkarbelk/Desktop/Text Analytics_21/netflix.csv")
