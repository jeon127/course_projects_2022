#install.packages("rtweet")
library(rtweet)
rt <- search_tweets(
  "#china #economy", n = 18000, include_rts = FALSE
)
