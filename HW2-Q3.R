#Nicole Ng
#HW2

#Clear environment
remove(list = ls())

#load general libraries
library(dplyr)
library(ggplot2)
library(caret)

#load visualization libraries
library(tidyverse)
library(sf)

#load pair wise correlation libraries
library("PerformanceAnalytics")

#load prediction evaluation libraries
library('DMwR')

#Set working directory
setwd("/Users/nicoleng/Dropbox/SMU/MASDA/STAT 6324/data")

#Read csv data
airbnb <- read.csv("AB_NYC_2019.csv")

#Check for missing data
data <- subset(airbnb, number_of_reviews==0)
missingdata <- sum(is.na(airbnb$reviews_per_month))

#Replacing NAs in reviews_per_month with 0
airbnb$reviews_per_month[is.na(airbnb$reviews_per_month)] <- 0

#Check for missing values for each variable
colSums(is.na(airbnb))

#List structure for data
str(airbnb)

#extreme values
summary(airbnb$price)
extremeprice <- subset(airbnb,price>3926) 
 
#using median price to control for extreme prices
lowprice <- subset(airbnb,price==0) #to check for too low prices

airbnb_old1 <- airbnb #make a copy to compare with new version of df
airbnb <- subset(airbnb,price>0) #removes the one extreme value

airbnb <- subset(airbnb,price<3926) #removes data more than what airbnb has for their most expensive in NY

summary(airbnb$reviews_per_month)
extremereviews <- subset(airbnb,reviews_per_month>30) 

# only one extreme value, need to remove it because it doesn't make sense
airbnb_old2 <- airbnb #make a copy to compare with new version of df
airbnb <- subset(airbnb,reviews_per_month<32) #removes the one extreme value

# attempt to remove all listing with no review on the theory that those are noise
airbnb <- subset(airbnb,reviews_per_month>0)

#Converting chr and some int into factors
airbnb$room_type <- as.factor(airbnb$room_type)
airbnb$neighbourhood <- as.factor(airbnb$neighbourhood)

str(airbnb)

unique(airbnb$neighbourhood)
unique(airbnb$neighbourhood_group)

listing_neighborhood <- (   airbnb
                             %>% group_by(neighbourhood)
                             %>% count()
                             %>% as.data.frame
)

names(listing_neighborhood)[names(listing_neighborhood) == "n"] <- "listing"

neighborhood_summary <-(   airbnb
                                   %>% group_by(neighbourhood)
                                   %>% summarize(
                                      median_price = median(price, na.rm=TRUE),
                                      min_price =min(price, na.rm=TRUE),
                                      max_price=max(price, na.rm=TRUE),
                                      latitude = median(latitude, na.rm=TRUE), #median was used instead of mean to be resistant to errors
                                      longitude = median(longitude, na.rm=TRUE), #median was used instead of mean to be resistant to errors
                                      reviews_per_month = mean(reviews_per_month, na.rm=TRUE)
                                      )
                                   %>% as.data.frame
)



enthome_neighborhood <- (   airbnb
                            %>% group_by(neighbourhood)
                            %>% filter(room_type == "Entire home/apt")
                            %>% count()
                            %>% as.data.frame
)
names(enthome_neighborhood)[names(enthome_neighborhood) == "n"] <- "homes_apt"


#merge 
neighborhood_summary = merge(neighborhood_summary, listing_neighborhood, by.x=c("neighbourhood"), by.y=c("neighbourhood"))
#need to merge counts of home_apt, pvtroom, sharedroom into the df

#clean merged data
neighborhood_smr_old <- neighborhood_summary #to observe how many rows were removed
neighborhood_summary <- subset(neighborhood_smr_old,listing>4)
summary(neighborhood_summary$median_price)

#Formula to calculate distance from Tribeca latitude  40.71810 longitude -74.00726 
# d = 3963.0 * arccos[(sin(lat1) * sin(lat2)) + cos(lat1) * cos(lat2) * cos(long2 – long1)]
# add distance into the models

airbnb$d <- 3963 * acos((sin(airbnb$latitude)*sin(40.71810))+cos(airbnb$latitude)*cos(40.71810)*cos(airbnb$longitude-(-74.00726)))

#normality and linear relationships

#transforming to make them normal 
airbnb$lgprice <- log(airbnb$price)
airbnb$lgmin_nights <- log(airbnb$minimum_nights)
airbnb$lgnumber_of_reviews <- log(airbnb$number_of_reviews)
airbnb$lgreviews_per_month <- log(airbnb$reviews_per_month)
airbnb$lgcalculated_host_listings_count <- log(airbnb$calculated_host_listings_count)
airbnb$lgavailability_365 <- log(airbnb$availability_365+1)
airbnb$lgd <- log(airbnb$d)

#merge neighborhood level data to listing level data
df <- merge(airbnb, neighborhood_smr_old, by.x=c("neighbourhood"), by.y=c("neighbourhood"))
names(df)[names(df) == "reviews_per_month.x"] <- "reviews_per_month"

#log transformation of merged data
df$lglisting <- log(df$listing)
df$lgmedian_price <- log(df$median_price)
df$lgmin_price <- log(df$min_price)
df$lgmax_price <- log(df$max_price)

#select variables for modeling
selectkeep <- c("room_type","lgprice","lgmedian_price","availability_365","lgavailability_365","lgmin_nights","d","lgd","neighbourhood_group","lglisting","min_price","lgmin_price","max_price","lgmax_price")
df <- subset(df,select=selectkeep)
str(df)

#split train and validate
##Sample the dataset. The return for this is row nos.
set.seed(1)
row.number <- sample(1:nrow(df), 0.7*nrow(df))
train = df[row.number,]
test = df[-row.number,]
dim(train)
dim(test)

#standardize training set and test set 
train_old <- train #archive for comparison
test_old <- test #archive for comparison

train2 <- train_old
test2 <- test_old

#standardize training set
train <- train_old %>% mutate_each_(list(~scale(.) %>% as.vector),
                                    vars = c("lgprice","lgmedian_price","availability_365","lgavailability_365","lgmin_nights","d","lgd","lglisting","min_price","lgmin_price","max_price","lgmax_price"))
#standardize test set
train3 <- preProcess(train2,method="center")
test <- predict(train3, test2)

#visualize correlation and check for normality
train_pre <- train[, c(2,6,8,12,14)]
chart.Correlation(train_pre, histogram=TRUE, pch=19) #comment out to disable and increase runtime speed

model2 = lm(lgprice~neighbourhood_group+room_type+lgmin_nights+lgd+lgmin_price+lgmax_price+lgd*lgmin_price, data=train)
summary(model2)
par(mfrow=c(2,2))
plot(model2)

#check for autocorrelation
acf(model2$residuals, type = "correlation")

#prediction
test2<- test

prediction2 <- predict(model2, test2)
actuals_preds2 <- data.frame(cbind(actuals=test$lgprice, predicteds=prediction2))

# R Squared error metric custom function
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}
LR_R = RSQUARE(test2$lgprice,prediction2)
print("R-Square: ")
LR_R

DMwR::regr.eval(actuals_preds2$actuals, actuals_preds2$predicteds)

# plot predicted values and actual values
plot(actuals_preds2$predicteds, actuals_preds2$actuals,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0, b = 1, lwd=2,
       col = "green")

