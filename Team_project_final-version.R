####MSBA1 HULT 2021-2022
### Data Analysis and Visualization
###Creadted by: Team 2
##Date: 11-12-2021
###
library(readxl)
mkt_ds <- read_excel("/Users/felixboelck/Library/Mobile Documents/com~apple~CloudDocs/A_HULT/Data_Analytics /Final Team Project/datasets_marketing_campaign_SF.xlsx")
View(mkt_ds)

#kaitlin's file path for the dataset, use: "/Users/kaitlinkarbelk/R_FinalProject/datasets_marketing_campaign_SF.xlsx")



###libraries used:
library(udpipe)
library(textrank)
library(dplyr)
library(ggplot2)

#Massasging the data 
#Cleaning dataset from N/A
my_udf_datasetname<- function (df, col_idx) { 
  new_df<- df
  for(i in 1:col_idx) {
    if (length(which(is.na(new_df[,i]))) > 1){
      new_df <- new_df[-which(is.na(new_df[,i])),]
    } 
  }
  return(new_df)
}
mkt_ds <- my_udf_datasetname(mkt_ds, ncol(mkt_ds))
mkt_ds

#Understanding how is the dataset conform
sapply(mkt_ds, function(x) length(unique(x)))
#Checking for uniqueness in the id column. 
length(unique(mkt_ds$ID))
#There are not duplicated ID´s

################################################
################ PART 1 / a) ###################
################################################

####Analyzing web purchases
#Understading how NumWebPurchases is conform
min(mkt_ds$NumWebPurchases)
mean(mkt_ds$NumWebPurchases) #The mean of web purchases is approximately 4
max(mkt_ds$NumWebPurchases)
hist(mkt_ds$NumWebPurchases)

###Working with categorical variables: converting them into numeric
#How is Marital Status compose
marital_status_freq <- txt_freq(x = mkt_ds$Marital_Status)
#Deleting Absurd and YOLO status 
#Converting the variables to " "
mkt_ds$Marital_Status <- gsub("YOLO", " ", mkt_ds$Marital_Status)
which(mkt_ds$Marital_Status=="YOLO") #checking they were erase
mkt_ds$Marital_Status <- gsub("Absurd", " ", mkt_ds$Marital_Status)
which(mkt_ds$Marital_Status=="Absurd")
#Cleaning the ds from the ""
mkt_ds[mkt_ds==" "] <- NA
clean <- na.omit(mkt_ds)
marital_status_freq <- txt_freq(x = mkt_ds$Marital_Status) #It´s already clean

#Convining the status to numeric
mkt_ds$Marital_Status_Factor <- as.factor(mkt_ds$Marital_Status)
mkt_ds$Marital_Status_Num <- as.numeric(mkt_ds$Marital_Status_Factor)

#How is education compose
eductaion_freq <- txt_freq(x = mkt_ds$Education)
#Converting the education to numeric
mkt_ds$Education_Factor <- as.factor(mkt_ds$Education)
mkt_ds$Education_Num <- as.numeric(mkt_ds$Education_Factor)

###Linear Regression###
#Checking for correlation between the variables and web_purchases
#Creating a ds for the matrix
matrix_cor <-  cbind(Id=mkt_ds$ID,Year_Birth=mkt_ds$Year_Birth,
                     Education=mkt_ds$Education_Num, 
                     Income=mkt_ds$Income,Kid_home=mkt_ds$Kidhome,
                     Teen_home = mkt_ds$Teenhome,Recency =mkt_ds$Recency, 
                     Mnt_Wines=mkt_ds$MntWines, Mnt_Fruits=mkt_ds$MntFruits, 
                     Mnt_Meat=mkt_ds$MntMeatProducts, Mnt_Fish=mkt_ds$MntFishProducts,
                     Mnt_Sweet =mkt_ds$MntSweetProducts, Mnt_Gold=mkt_ds$MntGoldProds,
                     Catalog_Purchases=mkt_ds$NumCatalogPurchases, 
                     Store_Purchases=mkt_ds$NumStorePurchases, 
                     Web_Visits=mkt_ds$NumWebVisitsMonth, 
                     AcceptedCmp1=mkt_ds$AcceptedCmp1,AcceptedCmp2=mkt_ds$AcceptedCmp2,
                     AcceptedCmp3=mkt_ds$AcceptedCmp3,AcceptedCmp4=mkt_ds$AcceptedCmp4,
                     AcceptedCmp5=mkt_ds$AcceptedCmp5,Complain=mkt_ds$Complain,
                     Response=mkt_ds$Response, Web_Purchases = mkt_ds$NumWebPurchases)
cor <- cor(matrix_cor, y = NULL, use = "everything",
           method = c("pearson", "kendall", "spearman"))
####There is no strong correlationship between the variables
##Saying the correlationship that is significant to run a linear regression is the one that 
#have a cor +/- 0.1

my_linear <- lm(NumWebPurchases~Year_Birth+Income+Kidhome+Teenhome+MntWines
                +MntFruits+MntMeatProducts+MntFishProducts+MntSweetProducts+
                  MntGoldProds+NumCatalogPurchases+NumStorePurchases+AcceptedCmp1+
                  AcceptedCmp4+AcceptedCmp5+Response, data=mkt_ds)
summary(my_linear)
#We can see by analyzing the pvalues that the significant variables to explain web purchases
#at a level of significance of 0.1 are:Income, Teenhome, MntWines, MntMeatProducts, MntSweetProducts,
#MntGoldProds, NumStorePurchases, AcceptedCmp5, Response
##Multiple R^2: 0.4262 
R_Square <- 0.4262*0.4262
#R_Square is 0.18 - these means 18% of web purchases can be explain by the variables
#above
#H0: variables are independent
#H1: variables are dependent
#Since the pvalue of the F-statistics is smaller than the level of significance of 0.01
#We reject H0 and say the variables explain statitstically significant variation in web purchases

#Running the regression with the significant variabless
my_linear_fix <- lm(NumWebPurchases~Income+Teenhome+MntWines
                    +MntMeatProducts+MntSweetProducts+
                      MntGoldProds+NumStorePurchases+AcceptedCmp5+Response, data=mkt_ds)
summary(my_linear_fix)
#Although we run it again with the significant variables to explain web purchases
#The R^2 didn´t improve 

################################################
################ PART 1 / b) ###################
################################################
mkt_ds$total_purchase <- mkt_ds$NumDealsPurchases+mkt_ds$NumWebPurchases+
  mkt_ds$NumCatalogPurchases+mkt_ds$NumStorePurchases
#Converting country into a binary US/Row (Rest of the Worlds)
mkt_ds$US <- ifelse(mkt_ds$Country == "US",1,0)
mkt_ds$Row <- ifelse(mkt_ds$Country != "US",1,0)


#Running the linear regression between total purchases and Country as a binary
country_purchase <- lm (total_purchase ~ US, data=mkt_ds)
summary(country_purchase)
#US as a country doesn´t hae a significant implication on the total purchases
#p value is bigger than the level of significance 0.05
R_Square_Country <- 0.001712*0.001712
#Also R^2 is super small so it doesn´t show the country explains the total 
#purchases 

#Also checking this on a Frequency table
#Total Purchase: web purchase + catalog purchase + store purchase
#Total Purchase by Country
country_grouped <- group_by(mkt_ds, Country)
total_grouped <- summarise(country_grouped, 
                           total_purchase= sum(NumDealsPurchases)+sum(NumWebPurchases)+ 
                             sum(NumStorePurchases)+ 
                             sum(NumCatalogPurchases))
sum(total_grouped$total_purchase)
#The total purchases arround the world were 32,976
total_grouped<- as.data.frame(total_grouped)  
Row_Sum_Abroad <- sum(total_grouped$total_purchase[1:7])
Row_Sum_US <- sum(total_grouped$total_purchase[8])
#From the 32,976 purchases, 31,233 came from abroad the US
#However this is no significant since we have less observation of US than the other
#countries
country_purchase_freq <- txt_freq(x = mkt_ds$Country)
Freq_Abroad <- sum(country_purchase_freq$freq_pct[1:6]) + (country_purchase_freq$freq_pct[8])
#Row represent 95.17 of the observations 
Freq_US <- country_purchase_freq$freq_pct[7]
#While US only represents 4.83

################################################
################ PART 1 / c) ###################
################################################

summary(mkt_ds$MntGoldProds)
mean_gold <- mean(mkt_ds$MntGoldProds)
var(mkt_ds$MntGoldProds)
sd(mkt_ds$MntGoldProds)

#Create dummy variable
mkt_ds$Abov_Avgspent <- ifelse(mkt_ds$MntGoldProds>mean_gold, "Conservative", "LessConservative")
mkt_ds
mkt_ds$Abov_Avgspent_Binary <- ifelse(mkt_ds$Abov_Avgspent=="Conservative", 1, 0)

sum(mkt_ds$Abov_Avgspent_Binary)

#Get the summary stats for the binary outcomes
summary(mkt_ds[which(mkt_ds$Abov_Avgspent =="Conservative"), ]) #returns descriptive statistics for all the variables for the conservative customers
summary(mkt_ds[which(mkt_ds$Abov_Avgspent =="LessConservative"), ]) #returns descriptive statistics for all the variables for the csutomers who 

mean(mkt_ds$NumStorePurchases)

library(ggplot2)
ggplot(data = mkt_ds, aes(x =Abov_Avgspent_Binary , y =NumStorePurchases)) + geom_bar(stat="identity")
#The Less Conservative persons are making more sotore purchases

################################################
################ PART 1 / d) ###################
################################################
#How is Marital_status & Education affecting the amount of spent on fish
#"Married PhD candidates" have a significant relation with amount spent on fish?
#Creating dummy variables for Marital_status=Married & 
mkt_ds$Dummy_married<- ifelse(mkt_ds$Marital_Status=="Married",1,0)
mkt_ds$Dummy_phD <- ifelse(mkt_ds$Education=="PhD",1,0)
#checking
table(mkt_ds$Dummy_married)
table(mkt_ds$Dummy_phD)
mkt_ds$Interaction <- mkt_ds$Dummy_married*mkt_ds$Dummy_phD

#Running the regression
fish_lm <-lm(MntFishProducts~Dummy_married+Dummy_phD+Interaction+Income+MntFruits
             +MntMeatProducts+MntSweetProducts+MntGoldProds+NumCatalogPurchases,
             data=mkt_ds)
summary(fish_lm)
#phD is statistically significant but shows a negative relationship with buying fish
#There are other products that are significant

################################################
################ PART 1 / e) ###################
################################################
####Analizing which product is generating more purchases
mkt_ds$total_purchases <- mkt_ds$NumDealsPurchases+mkt_ds$NumWebPurchases+
  mkt_ds$NumCatalogPurchases+mkt_ds$NumStorePurchases
mean(mkt_ds$total_purchase)
mkt_ds$total_business_success <- ifelse(mkt_ds$total_purchase > 9,1,0)
matrix_cor_business <- cbind(Id=mkt_ds$ID,Year_Birth=mkt_ds$Year_Birth,
                             Marital_Status=mkt_ds$Marital_Status_Num, 
                             Education=mkt_ds$Education_Num, 
                             Income=mkt_ds$Income,Kid_home=mkt_ds$Kidhome,
                             Teen_home = mkt_ds$Teenhome,Recency =mkt_ds$Recency, 
                             Mnt_Wines=mkt_ds$MntWines, Mnt_Fruits=mkt_ds$MntFruits, 
                             Mnt_Meat=mkt_ds$MntMeatProducts, Mnt_Fish=mkt_ds$MntFishProducts,
                             Mnt_Sweet =mkt_ds$MntSweetProducts, Mnt_Gold=mkt_ds$MntGoldProds,
                             Catalog_Purchases=mkt_ds$NumCatalogPurchases, 
                             Store_Purchases=mkt_ds$NumStorePurchases, 
                             Web_Visits=mkt_ds$NumWebVisitsMonth, 
                             AcceptedCmp1=mkt_ds$AcceptedCmp1,AcceptedCmp2=mkt_ds$AcceptedCmp2,
                             AcceptedCmp3=mkt_ds$AcceptedCmp3,AcceptedCmp4=mkt_ds$AcceptedCmp4,
                             AcceptedCmp5=mkt_ds$AcceptedCmp5,Complain=mkt_ds$Complain,
                             Response=mkt_ds$Response, Web_Purchases = mkt_ds$NumWebPurchases, 
                             Total_Business_success=mkt_ds$total_business_success)
cor_business_success <- cor(matrix_cor_business, y = NULL, use = "everything",
                            method = c("pearson", "kendall", "spearman"))
#The product that has a stronger correlation with the total purchases are the Wines
#This makes sense since the product that is more time sell on avrage is wine
mean(mkt_ds$MntWines) #305
mean(mkt_ds$MntMeatProducts) #166 
mean(mkt_ds$MntGoldProds) #43
mean(mkt_ds$MntFishProducts)#37
mean(mkt_ds$MntSweetProducts) #27
mean(mkt_ds$MntFruits) #26

ggplot(data=mkt_ds, aes(x=MntWines, y=total_business_success)) + 
  geom_point() 
#This plot shows the higher business
ggplot(data=mkt_ds, aes(x=MntMeatProducts, y=total_business_success)) + 
  geom_point() 
ggplot(data=mkt_ds, aes(x=MntGoldProds, y=total_business_success)) + 
  geom_point() 

#Let´s see what drives Wine purchases
#By knowing this, we can target our desire clients
wine_reasons <- lm(MntWines~Income+Marital_Status+Education, data=mkt_ds)
summary(wine_reasons)
#People that by more wines seem to be the ones that have higher income
#The marital status doesn´t appear to be significant to explain wine purchase
#The education level seems to be significant to explain wine purchase 
#As the level of education increases, so does the coefficients

#Where do clients prefer to but wine from?
cor_wine_catalog <- cor(mkt_ds$NumCatalogPurchases,y=mkt_ds$MntWines) #0.63
cor_wine_store <- cor(mkt_ds$NumStorePurchases,y=mkt_ds$MntWines) #0.64
cor_wine_web <- cor(mkt_ds$NumWebPurchases,y=mkt_ds$MntWines) #0.55
wine_purchase <- lm(MntWines~NumCatalogPurchases+NumStorePurchases+
                      NumWebPurchases,data=mkt_ds)
summary(wine_purchase)
#Catalog seems to be the most important chanell to sell wine






################################## Q1 #######################################


library(rpart)
library(rpart.plot)

mean(mkt_ds$NumWebPurchases)
##Converting web purchases into a business success/failure variable
#>1 = business success
#<0 = business failure
is.numeric(mkt_ds$NumWebPurchases)
mkt_ds$Business_Success <- ifelse(mkt_ds$NumWebPurchases>=1,1,0)

my_logit_Q1 <- glm(Business_Success ~ Income+Kidhome+Teenhome+Recency+MntWines+MntFruits+
                     MntMeatProducts+MntFishProducts+MntSweetProducts+MntGoldProds+
                     NumDealsPurchases+NumCatalogPurchases+NumWebVisitsMonth,
                   data=mkt_ds)
summary(my_logit_Q1)

my_logit_Q1 <- glm(Business_Success~Kidhome+MntWines+
                     MntMeatProducts+MntFishProducts+MntGoldProds+NumCatalogPurchases+Education_Num,
                   data=mkt_ds)
summary(my_logit_Q1)

my_tree <- rpart(Business_Success~Kidhome+MntWines+
                   MntMeatProducts+MntFishProducts+MntGoldProds+NumCatalogPurchases+Education_Num,
                 data = mkt_ds, method = "class", cp = 0.025)

rpart.plot(my_tree, type = 1, extra = 1)

## As we can see from the gini tree, the amount of meat products has the highest splitting power,
## followed by amount of wines and amount of fish products. We can conclude that people who shop online
## prefer to buy bigger amounts. The most bought products are meat, wine and fish.



################################## Q2 #######################################



cor1 <- cor(mkt_ds$NumWebPurchases, y = mkt_ds$NumWebVisitsMonth,
            method = c("pearson", "kendall", "spearman"))
cor1

my_linear <- lm(NumWebPurchases ~ NumWebVisitsMonth, data = mkt_ds)
summary(my_linear)

ggplot(data = mkt_ds, aes(x = NumWebPurchases, y = NumWebVisitsMonth, color = Business_Success)) + 
  geom_jitter() + geom_point() + geom_smooth(method = "lm")

## We wanted to analyze if there is a relationship between web purchases and web visits. We ran a 
## correlation matrix and visualized it using a scatter plot with a trend line. As we can see in the plot,
## we have a weak negative correlation of -0.06. Which means the more people buy, the less they are on our
## website. 


################################## Q3 #######################################


mkt_ds$sum_purchases <- mkt_ds$NumDealsPurchases + mkt_ds$NumWebPurchases + 
  mkt_ds$NumCatalogPurchases + mkt_ds$NumStorePurchases
##calculatin mean and median
mean(mkt_ds$sum_purchases)
median(mkt_ds$sum_purchases)

## defining business success as more than 15 purchases (median)
mkt_ds$BS_total_purchase <- ifelse(mkt_ds$sum_purchases > 15,1,0)

## comparing campaign 1 to countries ##
ggplot(data = mkt_ds, aes(x = Country, y = AcceptedCmp1, color = BS_total_purchase)) + 
  geom_jitter()

## comparing campaign 2 to countries ##
ggplot(data = mkt_ds, aes(x = Country, y = AcceptedCmp2, color = BS_total_purchase)) + 
  geom_jitter()

## comparing campaign 3 to countries ##
ggplot(data = mkt_ds, aes(x = Country, y = AcceptedCmp3, color = BS_total_purchase)) + 
  geom_jitter()

## comparing campaign 4 to countries ##
ggplot(data = mkt_ds, aes(x = Country, y = AcceptedCmp4, color = BS_total_purchase)) + 
  geom_jitter()

## comparing campaign 5 to countries ##
ggplot(data = mkt_ds, aes(x = Country, y = AcceptedCmp5, color = BS_total_purchase)) + 
  geom_jitter()

## Here we wanted to analyze if there is a relation between geographical region and the acceptance
## of a campaign.
## As we can see from this graphs is that the acceptance of our campaigns in general 
## is not successful. 
## On the x Axis we can see each country. We defined business success when our customres bought more than
## the average which was 15. 
## We would recommend coming up with a new marketing strategy. 



################################## Q4 #######################################


mean(mkt_ds$MntFruits)
median(mkt_ds$MntFruits)

#boxplot(mkt_ds$MntFruits,data=mkt_ds, main="Amount spent on fruits", horizontal = TRUE, varwidth = TRUE)

#install.packages("PerformanceAnalytics")    
library(PerformanceAnalytics)
chart.Boxplot(mkt_ds$MntFruits)

## Here we wanted to see what the average amount spend on fruits is. Depending on how you define average,
## we can either refer to the median = 8 or the mean = 26.36. We would normally recommend using the median.
## Because it is not influenced by outliers. But since we so many in this boxplot, we would recommend using,
## an average somewhere between median and mean. 



################################## Q5 #######################################



##could not find quesiotn 5##




################################## Q6 #######################################


mkt_ds$count <- 1
df_fish <- summarise(group_by(mkt_ds, Education),fish_purchase = sum(MntFishProducts), freq = sum(count))
df_fish[1, 1] <- "2nd Cycle"
df_fish$ratio <- round((df_fish$fish_purchase/df_fish$freq), digits = 0)

bdg1 <- ggplot(data = df_fish, aes(x = reorder(Education, -ratio),
                                   y = ratio)) + geom_bar(position = "dodge", stat = "identity", fill = "orange") +
  geom_text(aes(label = ratio), position = position_dodge(width = 0.5), vjust = -0.15)


bdg1 <- bdg1 + labs(title = "Education & consumption of Omega 3", x = "Education level",
                    y = "Amount of purchased fish in relation to observations") 

bdg1

## Here we wanted to analyze if people with more advanced degrees purchase more fish than others. We
## used a bar chart to display the different proportions. We can see that a higher education level,
## doesn't necessarily mean that people buy more fish. People who bought the most fish where in there 2nd Cycle,
## whereas people with the highest education where in the penultimate place.



################################## Q7 #######################################



## first we create a new data frame with the observations we want to analyze.
## in our case we wanted to analzye which food/product, families with kids spent most on.
df_teen1 <- summarise(group_by(mkt_ds, Teenhome), wine = sum(MntWines), fruits = sum(MntFruits), 
                      meat = sum(MntMeatProducts), fish = sum(MntFishProducts), freq = sum(count))

## after creating a new df we had to divide the total amount by the frequency to be able to actually 
## compare them
df_teen1$wine <- round(df_teen1$wine/df_teen1$freq)
df_teen1$fruits <- round(df_teen1$fruits/df_teen1$freq)
df_teen1$meat <- round(df_teen1$meat/df_teen1$freq)
df_teen1$fish <- round(df_teen1$fish/df_teen1$freq)

print(df_teen1)

ggplot(data = df_teen1, aes(x = Teenhome,
                            y = wine)) + geom_bar(position = "dodge", stat = "identity", fill = "darkred") +
  geom_text(aes(label = wine), position = position_dodge(width = 1), vjust = -0.15)

ggplot(data = df_teen1, aes(x = Teenhome,
                            y = fruits)) + geom_bar(position = "dodge", stat = "identity", fill = "orange") +
  geom_text(aes(label = fruits),  position = position_dodge(width = 1), vjust = -0.15)

ggplot(data = df_teen1, aes(x = Teenhome,
                            y = meat)) + geom_bar(position = "dodge", stat = "identity", fill = "red3") +
  geom_text(aes(label = meat),  position = position_dodge(width = 1), vjust = -0.15)

ggplot(data = df_teen1, aes(x = Teenhome,
                            y = fish)) + geom_bar(position = "dodge", stat = "identity", fill = "pink") +
  geom_text(aes(label = fish),  position = position_dodge(width = 1), vjust = -0.15)

## Here we wanted to see which food families with teenagers spent most on. 
## We can see that people with more teenagers at home spend the most on wine. We conclude that dealing 
## with teenagers can be really exhausting. Therefore parents need a break from time to time. 
## And wine is said to be great to calm you nreves. 



################################## Q8 #######################################



#Which marketing campaign is most successful?
#install.packages("tidyr")
library(tidyr)
#Which campaign is more successfull
Cmp1 <- sum(mkt_campaigns$Cmp1) #142
Cmp2 <- sum(mkt_campaigns$Cmp2) #30
Cmp3 <- sum(mkt_campaigns$Cmp3) #163
Cmp4 <- sum(mkt_campaigns$Cmp4) #164*
Cmp5 <- sum(mkt_campaigns$Cmp5) #162
mkt_campaigns <- cbind(Cmp1,Cmp2,Cmp3,Cmp4,Cmp5)
mkt_campaigns <- as.data.frame(mkt_campaigns)
#rearranging the data to be able to run ggplot with multiple x
campaigns_mkt <- mkt_campaigns%>%pivot_longer(1:5, names_to = "campaign", values_to = "accepted")
campaigns_mkt <- as.data.frame(campaigns_mkt)
#Printing the ggplot
ggplot(data=campaigns_mkt,aes(y =accepted, x =campaign )) +
  geom_bar(color="blue",fill="blue", stat="identity") +
  geom_text(data=campaigns_mkt,aes(x=campaign,y=accepted,label=accepted),vjust=-0.15,size=3)



##We looked at the campaign which had the highest acceptance and compared the totals of each marketing
## campaigns. Our results shows Campaign 4 having the highest marketing campaign since these customers 
## accepted the most offers in that campaign.


################################## Q9 #######################################

#Create dummy variable
mkt_ds$Complaints <- ifelse(mkt_ds$Complain>0, 1, 0)
mkt_ds
#mkt_ds$Abov_Avgspent_Binary <- ifelse(mkt_ds$Abov_Avgspent=="Conservative", 1, 0)
mkt_ds$Complaints <- ifelse(mkt_ds$Complain>0, 1, 0)
mkt_ds
#Creates new dataframe with complaints
Absolute_Complaints <- filter(mkt_ds, Complaints==1)
#gives total number of complaints
sum(Absolute_Complaints$Complaints)
#checking to see age range of people who have complaints
#get the max year of birth where customers have complaints
max(Absolute_Complaints$Year_Birth)
#get the min year of birth where customers have complaints
min(Absolute_Complaints$Year_Birth)
#summary stats on the birth year of people who have complaints
summary(Absolute_Complaints$Year_Birth)
sum(Absolute_Complaints$Dummy_graduation)
sum(Absolute_Complaints$Dummy_divorced)
sum(Absolute_Complaints$Dummy_single)
sum(Absolute_Complaints$Dummy_married)
#Creates new dataframe with complaints
Absolute_Complaints <- filter(mkt_ds, Complaints==1)
#gives total number of complaints
sum(Absolute_Complaints$Complaints)
#checking to see age range of people who have complaints
#get the max year of birth where customers have complaints
max(Absolute_Complaints$Year_Birth)
#get the min year of birth where customers have complaints
min(Absolute_Complaints$Year_Birth)
#summary stats on the birth year of people who have complaints
summary(Absolute_Complaints$Year_Birth)

## For question 9 we filtered on customers who had complaints and saw that their date of birth was between 
## the year 1900 and 1995.  Then we looked at other information about those persons with 
## complaints: graduation, divorced, single, married.



################################## Q10 #######################################




age <- 2021-mkt_ds$Year_Birth
age
mean_age <- mean(age)
summary(age)
mkt_ds$age <- age
install.packages("plyr")
library(plyr)
freq_age <- count(mkt_ds, 'Year_Birth')
install.packages('epiDisplay')
library(epiDisplay)
age_all <- tab1(mkt_ds$Year_Birth, sort.group = "decreasing", cum.percent = TRUE)
age_all

## CONCLUSION IS MISSING !!!

