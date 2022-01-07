############MSBA1 HULT 2021-2022
######### BUSINESS CASE ANALYSIS - AIR FRANCE
##### CREATED BY: TEAM 2
#### DATE: 10.26.2021

#Installing packages
install.packages("lmtest")
install.packages("dplyr")
install.packages("plotrix")
library(lmtest)
library(dplyr)
library(plotrix)

####importing Air France Case Spreadsheet frame from local drive
library(readxl)
air_france <- read_excel("R/Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "DoubleClick")
View(air_france)

## removing all the unnecessary signs from the columns to make them look nicer
for(i in 1:ncol(air_france)){
  names(air_france)[i] <- tolower(names(air_france)[i])
  names(air_france)[i] <- gsub("%", "", names(air_france)[i])
  names(air_france)[i] <- gsub("/", "", names(air_france)[i])
  names(air_france)[i] <- gsub("\\.", "", names(air_france)[i])
  names(air_france)[i] <- gsub(" ", "_", names(air_france)[i])
}

## Calculating the Click Through Rate
#CTR:the number of clicks that your ad receives divided by the number of times 
#your ad is shown. It is a good way of analyzing the quality of advertising
#We are going to create a new variable "Click_Trough_Rate" where clicks % impresiones
air_france$ctr <- air_france$clicks / air_france$impressions
#Analyzing the min, mean and max values for the CTR
ctr_stats <- function (var1) {
  ctr <- var1
  ctr_min <- min(var1, na.rm =TRUE)
  ctr_mean <- mean(var1, na.rm =TRUE)
  ctr_max <- max(var1, na.rm =TRUE) 
  return (c(ctr_min, ctr_mean, ctr_max))  
}#closing for loops
call_ctr <- ctr_stats(air_france$ctr)
#The min of the Click Trough Rate 0.0001286
#The mean of the Click Trough Rate is 0.11
#The mean value of the CTR is pretty low which indicate that 1 out of 9 people that saw the 
#add (impression) make a click
#The max of the Click Trough Rate is 2

## relable the NA's with an empty cell
air_france$match_type <- gsub("N/A", "", air_france$match_type)

## creating a new grouped datatable which combines the publisher_name
grouped <- group_by(air_france, publisher_name)
totals_grouped <- summarise(grouped, clicks = sum(clicks), 
                            media_cost = sum(total_cost),
                            total_bookings = sum(total_volume_of_bookings),
                            avg_ticket = sum(amount)/sum(total_volume_of_bookings),
                            total_revenue = sum(amount),
                            net_revenue = sum(amount) - sum(total_cost),
                            total_impressions = sum(impressions),
                            avg_ctr = mean(ctr, na.rm = TRUE),
                            profit_cost = (sum(amount) - sum(total_cost)) / sum(total_cost),
                            conv_rate = sum(total_volume_of_bookings) / sum(clicks) * 100,
                            ROA = sum(amount) / sum(total_cost)
)
## using the rbind function to add Kayak values to the grouped dataset - in order to be able to compare
totals_grouped <- rbind(totals_grouped,  
                        c("Kayak", 2939, 3567.13, 208, 1123.53, 233694, 230126.87, NA, NA, 230126.87 / 3567.13, (208/2939 * 100), 233694 / 3567.13))

## transforming all the data to numeric values
totals_grouped$clicks <- as.numeric(totals_grouped$clicks)
totals_grouped$media_cost <- as.numeric(totals_grouped$media_cost)
totals_grouped$total_bookings <- as.numeric(totals_grouped$total_bookings)
totals_grouped$avg_ticket <- as.numeric(totals_grouped$avg_ticket)
totals_grouped$total_revenue <- as.numeric(totals_grouped$total_revenue)
totals_grouped$net_revenue <- as.numeric(totals_grouped$net_revenue)
totals_grouped$total_impressions <- as.numeric(totals_grouped$total_impressions)
totals_grouped$avg_ctr <- as.numeric(totals_grouped$avg_ctr)
totals_grouped$ROA <- as.numeric(totals_grouped$ROA)
totals_grouped$profit_cost <- as.numeric(totals_grouped$profit_cost)
totals_grouped$conv_rate <- as.numeric(totals_grouped$conv_rate)
## creating a new column to show the booking percentage 
totals_grouped$percentage_booking <- 0.0

## in the line above we created the column to see wehre we got the most bookings come 
#from percentage wise and then in the next lines we assign the values to it with a for loop
sum_booking <- 0
for(i in totals_grouped$total_bookings){
  sum_booking <- sum_booking + i
}

sum_booking
for(i in 1:nrow(totals_grouped)){
  res <- (totals_grouped[i, 4] / sum_booking)
  totals_grouped[i, 13] <- res * 100
}

#totals_grouped$net_rev_avg_cost <- (totals_grouped$total_revenue / totals_grouped$media_cost)

## checking the min's and max's for the net revenue & clicks & ROA
totals_grouped[which.max(totals_grouped$net_revenue), ]
totals_grouped[which.min(totals_grouped$net_revenue), ]
totals_grouped[which.max(totals_grouped$clicks), ]
totals_grouped[which.min(totals_grouped$clicks), ]
totals_grouped[which.max(totals_grouped$ROA), ]
totals_grouped[which.min(totals_grouped$ROA), ]

## Creating a new colum in the air_france dataset to see which click lead to a booking success
#Converting booking into a binomial variable
#Booking >1 = success
#Booking <1 =failure
air_france$booking_success <- ifelse(air_france$total_volume_of_bookings >= 1,1,0)
## 
my_logit_bookings_success <- glm(booking_success ~ campaign, data=air_france)
summary(my_logit_bookings_success)
#We can se by analyzing the pvalues there are some campaigns that are not significant 
#to explain the booking success. 
#This are: Air France Global Campaign, Paris & France Terms

######Analyzing Clicks and Impressions
test_data <- data.frame(
  clicks = c(90),
  impressions = c(500)
)
#We believe that the more Impressions the more clicks Air France will get
#Before starting we are going to normalize the data so we are able to compare
#both variables
normal <- function(var1){
  my_normal <- (var1 - min(var1)) / (max(var1)- min(var1))
  return (my_normal)
}#closing the normal UDF

air_france$clicks_norm <- normal(air_france$clicks)
air_france$impressions_norm <- normal(air_france$impressions)

plot(air_france$clicks_norm,air_france$impressions_norm, type="p")
#We can see that when you have almost non impressions you also have almost non
#clicks. Thinking about taking out the 0 impressions and running the funcion again. 
#Using a ggplot to be able to zoom on it-
install.packages("ggplot2")
library(ggplot2)
ggplot(data=air_france, aes(x=impressions_norm, y=clicks_norm))+ geom_point()

#Random Sampling
index <- sample(1:nrow(air_france), size=0.8*nrow(air_france))
air_france_train <- air_france[index,] 
air_france_test <- air_france[-index,] 

#Running a Linear Regression between Clicks and Impressions being Clicks as the
reg_clicks_impresions<- lm(clicks ~ impressions  , data=air_france_train)
summary(reg_clicks_impresions)

Call:
  lm(formula = clicks ~ impressions, data = air_france_train)

Residuals:
  Min       1Q   Median       3Q      Max 
-16298.9    -95.9    -93.4    -81.3  29849.7 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 9.689e+01  1.793e+01   5.402 7.01e-08 ***
  impressions 2.502e-03  9.503e-05  26.331  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1076 on 3606 degrees of freedom
Multiple R-squared:  0.1613,	Adjusted R-squared:  0.161 
F-statistic: 693.3 on 1 and 3606 DF,  p-value: < 2.2e-16

#The p-value of the Impression variable is extreamly low (2e-16)
#P-value < than the significant value 0.05 shows the variable is significant to
#explain the relationship with Clicks

## Analyzing with a linear regerssion how many clicks and impressions lead to a booking success 
my_logit_bookings_success <- glm(booking_success ~ clicks_norm + impressions_norm, data=air_france, family = "binomial")
summary(my_logit_bookings_success)

Call:
  glm(formula = booking_success ~ clicks_norm + impressions_norm, 
      family = "binomial", data = air_france)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-4.7886  -0.3018  -0.2930  -0.2908   2.5265  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)       -3.14950    0.07581 -41.545   <2e-16 ***
  clicks_norm      261.53790   16.51803  15.833   <2e-16 ***
  impressions_norm -30.96911    3.71279  -8.341   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2549.5  on 4509  degrees of freedom
Residual deviance: 1795.5  on 4507  degrees of freedom
AIC: 1801.5

Number of Fisher Scoring iterations: 8

#Both variables seem to be equally significant to explain the bookings since their p values
#are both lower than 0.01
#Since the data is normalize we are able also to compare the variables coefficient side 
#by side
#We can see clicks have a positive relationship regarding the bookings. As a user make a click
#the chances of having a booking (business success) is of 261
#While when we have an impression the chances of having a booking decreases in 30. 

#We also want to find out which is are the keywords that are significant to explain
#the bookings
#Analizing which keywords have a significance to explain the amount of clicks 
clicks_keyword_relation <- glm(clicks ~ keyword, data=air_france)
summary(clicks_keyword_relation)
#The two most significant keywords are "france" and "airfare" since both have a small 
#p value. P values of 2.12e-13 and 6.18e-12 are smaller than the significant value of 
#0.001
#The keyword "line ticket" is also significant at a level of 0.1

#####Making other regressions
###Seeing which campaign is having greater cost
new <- predict(my_logit_bookings_success, test_data, type="response")
new
my_logit <- glm(total_cost ~ campaign , data=air_france)
summary(my_logit, options=options(max.print=100000))
###The more significant campaigns at a level of 0.05 are: Air France Branded, Paris
#& France Terms, Targeted New York, Targeted Los Angeles, Targeted DC, Targeted Boston, 

###Seing which campaigns produce a greater amount of money
my_logit <- glm(amount ~ campaign , data=air_france)
summary(my_logit, options=options(max.print=100000))
###The more significant campaigns at a level of 0.05 are_ Western Europe Destinations, 
#Google_Yearlong 2006, Targeted New York, Air France Branded and some campaigns that are not assigned
#So we can conclude that although the company is spending significant amount of 
#money on Targeted New York and Air France Branded campaigns, those produce also
#a huge amount of money to the company

#######Keyword analysis 
##Before starting we need to clean the Keyword column from all special characters
air_france$keyword <- as.character(gsub("[^[:alnum:]]", "_", air_france$keyword))
#Let´s see the frequency of the keywords: 
install.packages("udpipe")
install.packages("texttrank")
library(udpipe)
library(textrank)
key_freq <- txt_freq(x = air_france$keyword)
#We know created another data set with they keyword, the frequency of the keyword 
#and the frequency percentage of the keyword
#Finding the mean of the frecuency percentage of the keyword 
mean_freq<- mean(key_freq$freq)
#The mean of the freq is 2.17. Let´s see which are the keywords that have a frequency 
#above the mean
print(key_freq$freq > mean_freq)
#Printing the frecuent keywords in a barchart
library(lattice)
barchart(key ~ freq, data = head(key_freq,10), col = "cadetblue", main = "Most Frequent Keywords", xlab = "Freq")
#We can visualize that the most repetitive keyword is paris airline

#Getting to know from which publisher are the ones where most type keywords coming from 
#Creating a new data frame for publisher and keyword variables
key_publisher <-group_by(air_france, keyword)
key_freq <- txt_freq(x = grouped$keyword)

keyword_coming <- glm( freq ~ publisher_name, data=air_france)
summary(keyword_coming options=options(max.print=10))

###Trying to do a wordcloud of the keyword
install.packages("wordcloud")
library(wordcloud)
wordcloud(words = key_freq$key, freq = key_freq$freq)


#################VISUALIZATION
## Creating a bar chart to show which engine had the biggest ROA
?barplot
barplot(totals_grouped$ROA, main = "Return on Advertisement", names.arg = totals_grouped$publisher_name, xlab = "Search Engnies", ylab = "ROA" )

#ggplot(totals_grouped, aes(x = publisher_name, y = ROA)) didn't work as I wanted it to 

## showing the ROA in a pie chart | maybe not the best option but just to try it out
labels_ROA <- unlist(totals_grouped[, 1])
x_values_ROA <- unlist(totals_grouped[, 12])
labels_ROA <- paste(labels_ROA, round(x_values_ROA), " $")
pie(x_values_ROA, labels = labels_ROA, main = "ROA")

## showing the percentage of booking - with the added percentages 
labels_perc_book <- unlist(totals_grouped[, 1])
x_values_perc_book <- unlist(totals_grouped[, 13])
pct_book <- round(x_values_perc_book/sum(x_values_perc_book)*100)
labels_perc_book <- paste(labels_perc_book, pct_book)
labels_perc_book <- paste(labels_perc_book, " %", sep = " ")

pie(x_values_perc_book, labels = labels_perc_book, main = "Percentage of Bookings")

### visualizing the net revenue in a pie chart 
labels_net_rev <- unlist(totals_grouped[,1])
x_variable_net_revenue <- unlist(totals_grouped[, 7])
pct_net_rev <- round(x_variable_net_revenue/sum(x_variable_net_revenue)*100)
labels_perc_net_rev <- paste(labels_net_rev, pct_net_rev)
labels_perc_net_rev <- paste(labels_perc_net_rev, " %", sep = " ")

pie(x_variable_net_revenue, labels = labels_perc_net_rev, main = "Net revenue by Engine")

############# analyzing clicks and campaign #########
library(dplyr)
library(ggplot2)
air_france <- read.csv('/Users/kaitlinkarbelk/Desktop/airfrance.csv')

air_france$Clicks <- gsub("[^[:alnum:]]", "",air_france$Clicks)
air_france$Clicks = as.numeric(air_france$Clicks)
air_franceClicks[is.na(data$Clicks)] <- 0

air_france$Cost_Clicks <- gsub("[$]", "",air_france$Avg..Cost.per.Click)
air_franceCost_Clicks = as.numeric(air_france$Cost_Clicks)


df1 <- summarise(group_by(air_france, Campaign),
                 total_clicks = sum(Clicks),
                 average_cost_clicks = mean(Cost_Clicks)
)

df1 <- df1[order(-df1$total_clicks),]


barplot(df1$total_clicks,names.arg=df1$Campaign, xlab="Campaign",ylab="Clicks",col="blue",
        main="Total Clicks",border="red", las=2,cex.names = 1)


barplot(df1$average_cost_clicks,names.arg=df1$Campaign, xlab="Campaign",ylab="Avg Cost Clicks",col="grey",
        main="Avg Cost Clicks",border="red", las=2,cex.names = 1)




