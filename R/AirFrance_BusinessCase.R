############MSBA1 HULT 2021-2022
######### BUSINESS CASE ANALYSIS - AIR FRANCE
#### DATE: 10.26.2021
#Installing packages #install.packages("lmtest") #install.packages("dplyr") #install.packages("plotrix") #install.packages("ggplot2") #install.packages("plyr") library(lmtest)
library(dplyr) library(plotrix) library(ggplot2) library(plyr) library(plotly) library(lattice) library(udpipe) library(textrank) library(caret)
####importing Air France Case Spreadsheet frame from local drive
library(readxl)
air_france <- read_excel("Desktop/R/Data Set/Air France Case Spreadsheet Supplement.xls",
                         sheet = "DoubleClick")
air_france
#####Massaging the data
## Removing all the unnecessary signs from the columns to make them look nicer for(i in 1:ncol(air_france)){
names(air_france)[i] <- tolower(names(air_france)[i]) names(air_france)[i] <- gsub("%", "", names(air_france)[i]) names(air_france)[i] <- gsub("/", "", names(air_france)[i]) names(air_france)[i] <- gsub("\\.", "", names(air_france)[i]) names(air_france)[i] <- gsub(" ", "_", names(air_france)[i])
}
## relable the NA's with an empty cell
air_france$match_type <- gsub("N/A", "", air_france$match_type) ######
#####Descriptive Analysis
###Finding out which are the most important variables to explain the bookings

###First, converting bookings into a binary of business success/failure
#Booking >20 = success #Saying 20 booking is a business success is very close to 0 #Booking <1 =failure
air_france$booking_success <- ifelse(air_france$total_volume_of_bookings >= 10,1,0) ###Second, making a random sampling
index <- sample(1:nrow(air_france), size=0.8*nrow(air_france))
air_france_train <- air_france[index,]
air_france_test <- air_france[-index,]
####Third, normalizing the data to be able to compare the variables
normal <- function(var1){
  my_normal <- (var1 - min(var1)) / (max(var1)- min(var1))
  return (my_normal)
}#closing the normal UDF
air_france$search_engine_norm <- normal(air_france$search_engine_bid) air_france$clicks_charges_norm <- normal(air_france$click_charges) air_france$clicks_norm <- normal(air_france$clicks)
air_france$avg_cost_norm <- normal(air_france$avg_cost_per_click) air_france$impressions_norm <- normal(air_france$impressions) air_france$engine_click_norm <- normal(air_france$engine_click_thru_) air_france$avg_pos_norm <- normal(air_france$avg_pos) air_france$total_cost_trans_norm <- normal(air_france$total_cost_trans) air_france$amount_norm <- normal(air_france$amount)
air_france$total_cost_norm <- normal(air_france$total_cost)
#Running the regression
air_regression <- glm (booking_success ~ search_engine_norm+clicks_charges_norm+
                         clicks_norm+avg_pos_norm+avg_cost_norm+impressions_norm+ engine_click_norm+total_cost_trans_norm+amount_norm+ total_cost_norm, data=air_france)
summary(air_regression)
#Variables that are not significant to explain bookings at a level of 0.05 are:engine_click_norm #and search_engine_norm
#Running again the regression without this variables
air_regression_fix <- glm (booking_success ~ clicks_charges_norm+
                             clicks_norm+avg_pos_norm+avg_cost_norm+impressions_norm+ +total_cost_trans_norm+amount_norm+
                             total_cost_norm, data=air_france)
summary(air_regression_fix)
###Now all variables are significant
###We can conclude the more important variables to explain bookings are clicks charges #and amounts
######Analyzing Clicks and Impressions
#We believe that the more Impressions the more clicks Air France will get

plot(air_france$clicks_norm,air_france$impressions_norm, type="p")
#We can see that when you have almost non impressions you also have almost non #clicks.
#Using a ggplot to be able to zoom on it-
ggplot(data=air_france, aes(x=impressions_norm, y=clicks_norm))+ geom_point() #Running a Linear Regression between Clicks and Impressions being Clicks as the reg_clicks_impresions<- lm(clicks ~ impressions , data=air_france_train) summary(reg_clicks_impresions)
#The p-value of the Impression variable is extreamly low (2e-16)
#P-value < than the significant value 0.05 shows the variable is significant to #explain the relationship with Clicks
######
## Analyzing with a linear regerssion how many clicks and impressions lead to a booking success
my_logit_bookings_success <- glm(booking_success ~ clicks_norm + impressions_norm, data=air_france, family = "binomial")
summary(my_logit_bookings_success)
#Both variables seem to be equally significant to explain the bookings since their p values #are both lower than 0.01
#Since the data is normalize we are able also to compare the variables coefficient side #by side
#We can see clicks have a positive relationship regarding the bookings.
#While when we have an impression the chances of having a booking decreases
####KPI Analysis
###CTR- Click Trough Rate
#CTR:the number of clicks that your ad receives divided by the number of times
#your ad is shown. It is a good way of analyzing the quality of advertising
#We are going to create a new variable "Click_Trough_Rate" where clicks % impresiones air_france$ctr <- air_france$clicks / air_france$impressions
#Analyzing the min, mean and max values for the CTR
ctr_min <- min(air_france$ctr, na.rm =TRUE) ctr_mean <- mean(air_france$ctr, na.rm =TRUE) ctr_max <- max(air_france$ctr, na.rm =TRUE)
#The min of the Click Trough Rate 0.0001286
#The mean of the Click Trough Rate is 27
#The mean value of the CTR is pretty low which indicate that 1 out of 9 people that saw the #add (impression) make a click
#The max of the Click Trough Rate is 2
sum_clicks <- sum(air_france$clicks)

sum_impressions <- sum(air_france$impressions)
### CTR by Publisher
ctr_publisher <- group_by(air_france, publisher_name) ctr_publisher <- summarise(ctr_publisher,
                                                                                 clicks = sum(clicks),
                                                                                 impressions = sum(impressions), ctr = sum(clicks)/sum(impressions))
ggplot(ctr_publisher, aes(x=reorder(publisher_name, ctr), y=ctr)) + geom_bar(stat = "identity",fill="blue") +
  coord_flip()
### ROA: Return Through Advertisment
## Creating a new grouped datatable which combines the publisher_name grouped <- group_by(air_france, publisher_name)
totals_grouped <- summarise(grouped, clicks = sum(clicks),
                            media_cost = sum(total_cost),
                            total_bookings = sum(total_volume_of_bookings),
                            avg_ticket = sum(amount)/sum(total_volume_of_bookings), total_revenue = sum(amount),
                            net_revenue = sum(amount) - sum(total_cost), total_impressions = sum(impressions),
                            avg_ctr = mean(ctr, na.rm = TRUE),
                            profit_cost = (sum(amount) - sum(total_cost)) / sum(total_cost), conv_rate = sum(total_volume_of_bookings) / sum(clicks) * 100, ROA = sum(amount) / sum(total_cost)
)
## Using the rbind function to add Kayak values to the grouped dataset - in order to be able to compare
totals_grouped <- rbind(totals_grouped,
                        c("Kayak", 2939, 3567.13, 208, 1123.53, 233694, 230126.87, NA, NA, 230126.87 / 3567.13, (208/2939 * 100), 233694 / 3567.13))
## Transforming all the data to numeric values
totals_grouped$clicks <- as.numeric(totals_grouped$clicks) totals_grouped$media_cost <- as.numeric(totals_grouped$media_cost) totals_grouped$total_bookings <- as.numeric(totals_grouped$total_bookings) totals_grouped$avg_ticket <- as.numeric(totals_grouped$avg_ticket) totals_grouped$total_revenue <- as.numeric(totals_grouped$total_revenue) totals_grouped$net_revenue <- as.numeric(totals_grouped$net_revenue) totals_grouped$total_impressions <- as.numeric(totals_grouped$total_impressions) totals_grouped$avg_ctr <- as.numeric(totals_grouped$avg_ctr) totals_grouped$ROA <- as.numeric(totals_grouped$ROA) totals_grouped$profit_cost <- as.numeric(totals_grouped$profit_cost) totals_grouped$conv_rate <- as.numeric(totals_grouped$conv_rate)

## Creating a new column to show the booking percentage totals_grouped$percentage_booking <- 0.0
#totals_grouped$net_rev_avg_cost <- (totals_grouped$total_revenue / totals_grouped$media_cost)
## In the line above we created the column to see where we got the most bookings come from percentage wise and then in the next lines we assign the values to it with a for loop sum_booking <- 0
for(i in totals_grouped$total_bookings){
  sum_booking <- sum_booking + i }
sum_booking
for(i in 1:nrow(totals_grouped)){
  res <- (totals_grouped[i, 4] / sum_booking)
  totals_grouped[i, 13] <- res * 100 }
## checking the min's and max's for the net revenue & clicks & ROA totals_grouped[which.max(totals_grouped$net_revenue), ] totals_grouped[which.min(totals_grouped$net_revenue), ] totals_grouped[which.max(totals_grouped$clicks), ] totals_grouped[which.min(totals_grouped$clicks), ] totals_grouped[which.max(totals_grouped$ROA), ] totals_grouped[which.min(totals_grouped$ROA), ]
#################VISUALIZATION
## Creating a bar chart to show which engine had the biggest ROA
?barplot
barplot(totals_grouped$ROA, main = "Return on Advertisement", names.arg = totals_grouped$publisher_name, xlab = "Search Engnies", ylab = "ROA" )
#ggplot(totals_grouped, aes(x = publisher_name, y = ROA)) didn't work as I wanted it to ## showing the ROA in a pie chart | maybe not the best option but just to try it out labels_ROA <- unlist(totals_grouped[, 1])
x_values_ROA <- unlist(totals_grouped[, 12])
labels_ROA <- paste(labels_ROA, round(x_values_ROA), " $") pie(x_values_ROA, labels = labels_ROA, main = "ROA")
## using a ggplot to visualize the ROA in a bar chart ?ggplot
##creating an unorderd bar chart
ggplot(totals_grouped, aes( x= publisher_name, y=ROA)) +
  geom_bar(stat = "identity",color="blue",fill=rgb(0.1,0.4,0.5,0.7)) +
  
  coord_flip()
## reordering the bars | nevertheless had some minor issues in final outcome (ylabel wasn't really showing what
## we want to)
ggplot(totals_grouped, aes(x=reorder(publisher_name, ROA), y=ROA)) +
  geom_bar(stat = "identity",color="blue",fill=rgb(0.1,0.4,0.5,0.7)) + coord_flip()
## showing the percentage of booking - with the added percentages labels_perc_book <- unlist(totals_grouped[, 1])
x_values_perc_book <- unlist(totals_grouped[, 13])
pct_book <- round(x_values_perc_book/sum(x_values_perc_book)*100) labels_perc_book <- paste(labels_perc_book, pct_book) labels_perc_book <- paste(labels_perc_book, " %", sep = " ")
pie(x_values_perc_book, labels = labels_perc_book, main = "Percentage of Bookings")
### visualizing the net revenue in a pie chart
labels_net_rev <- unlist(totals_grouped[,1])
x_variable_net_revenue <- unlist(totals_grouped[, 7])
pct_net_rev <- round(x_variable_net_revenue/sum(x_variable_net_revenue)*100) labels_perc_net_rev <- paste(labels_net_rev, pct_net_rev)
labels_perc_net_rev <- paste(labels_perc_net_rev, " %", sep = " ") pie(x_variable_net_revenue, labels = labels_perc_net_rev, main = "Net revenue by Engine") ############# Analyzing Clicks and Campaign #########
air_france$clicks <- gsub("[^[:alnum:]]", "",air_france$clicks) air_france$clicks = as.numeric(air_france$clicks) air_france$clicks[is.na(data$clicks)] <- 0
air_france$cost_clicks <- gsub("[$]", "",air_france$avg_cost_per_click) air_france$cost_clicks = as.numeric(air_france$cost_clicks)
df1 <- summarise(group_by(air_france, campaign), total_clicks = sum(clicks),
                 average_cost_clicks = mean(cost_clicks)
)
df1 <- df1[order(-df1$total_clicks),]
ggplot(df1, aes(x=reorder(campaign,total_clicks), y=total_clicks)) +
  geom_bar(stat = "identity",color="blue",fill=rgb(0.1,0.4,0.5,0.7)) +
  coord_flip()
ggplot(df1, aes(x=reorder(campaign,average_cost_clicks), y=average_cost_clicks)) +
  geom_bar(stat = "identity",color="blue",fill=rgb(0.1,0.4,0.5,0.7)) + coord_flip()

#####Making other regressions
###Seeing which campaign is having greater cost
new <- predict(my_logit_bookings_success, test_data, type="response")
new
my_logit <- glm(total_cost ~ campaign , data=air_france)
summary(my_logit, options=options(max.print=100000))
###The more significant campaigns at a level of 0.05 are: Air France Branded, Paris
#& France Terms, Targeted New York, Targeted Los Angeles, Targeted DC, Targeted Boston,
####Seing which campaigns produce a greater amount of money
my_logit <- glm(amount ~ campaign , data=air_france)
summary(my_logit, options=options(max.print=100000))
###The more significant campaigns at a level of 0.05 are_ Western Europe Destinations, #Google_Yearlong 2006, Targeted New York, Air France Branded and some campaigns that are not assigned
#So we can conclude that although the company is spending significant amount of #money on Targeted New York and Air France Branded campaigns, those produce also #a huge amount of money to the company
#######KEYWORD ANALYSIS
##Before starting we need to clean the Keyword column from all special characters air_france$keyword <- as.character(gsub("[^[:alnum:]]", " ", air_france$keyword))
#Let ́s see the frequency of the keywords:
#install.packages("udpipe")
#install.packages("texttrank")
key_freq <- txt_freq(x = air_france$keyword)
#We know created another data set with they keyword, the frequency of the keyword
#and the frequency percentage of the keyword
#Finding the mean of the frecuency percentage of the keyword
mean_freq<- mean(key_freq$freq)
#The mean of the freq is 2.17. Let ́s see which are the keywords that have a frequency
#above the mean
print(key_freq$freq > mean_freq)
#Printing the frequent keywords in a barchart
barchart(key ~ freq, data = head(key_freq,10), fill="blue", col=rgb(0.1,0.4,0.5,0.7), main = "Most Frequent Keywords", xlab = "Freq")
ggplot(data = head(key_freq,10), aes(x=reorder(key,freq), y=freq)) +
  geom_bar(stat = "identity",fill="#598E9F", col=rgb(0.1,0.4,0.5,0.7)) +
  coord_flip()
#We can visualize that the most repetitive keyword is paris airline, followed by paris flight and paris ticket
#From the 3 more frequent Keywords, let ́s see from which Publisher are they coming from

#Keyword = Paris Airline
paris_airline <- filter(air_france, keyword == "paris airline") publish_freq_paris_airline <- txt_freq(x = paris_airline$publisher_name) #Mainly from Google
#Keyword = paris flight
paris_flight <- filter(air_france, keyword == "paris flight") publish_freq_paris_flight <- txt_freq(x = paris_flight$publisher_name) #Mainly from Google
#Keyword = Paris ticket
paris_ticket<- filter(air_france, keyword == "paris ticket") publish_freq_paris_ticket <- txt_freq(x = paris_ticket$publisher_name) #Mainly from Google
###Trying to do a wordcloud of the keyword #install.packages("wordcloud")
library(wordcloud)
wordcloud(words = key_freq$key, freq = key_freq$freq)
#We also want to find out which is are the keywords that are significant to explain #the bookings
#Analizing which keywords have a significance to explain the amount of clicks clicks_keyword_relation <- glm(booking_success ~ keyword, data=air_france) summary(clicks_keyword_relation)
#The two most significant keywords are "france" and "airfare" since both have a small #p value. P values of 2.12e-13 and 6.18e-12 are smaller than the significant value of #0.001
#The keyword "line ticket" is also significant at a level of 0.1

