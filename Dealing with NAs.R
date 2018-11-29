#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggfortify)
library(vars)
library(forecast)
library(lubridate)
library(scales)
library(lemon)

#Load data set
power2 <- readRDS("~/Ubiqum/Project 4/Task 1/Original Data Set/Power2.rds")
power2Tidy <- readRDS("~/Ubiqum/Project 4/Task 1/Original Data Set/power2Tidy.rds")

#find all NA's and what date(s) and duration(s)
NAsS1 <- filter(power2, is.na(power2$S1_Kitchen))
NAsS2 <- filter(power2, is.na(power2$S2_Laundry))
NAsS3 <- filter(power2, is.na(power2$S3_WH_AC))
NAsS4 <- filter(power2, is.na(power2$S4_Rest))

#All NAs are on same dates/times
summary(NAsS1)
summary(NAsS2)
summary(NAsS3)
summary(NAsS4)

#create full list of NA's from all submeters
allNAs <- filter(power2, is.na(power2$S1_Kitchen) | 
                   is.na(power2$S2_Laundry) |
                   is.na(power2$S3_WH_AC) |
                   is.na(power2$S4_Rest))

#plot the NAs
ggplot(allNAs, aes(x=DateTime)) +
  geom_bar()
#result is too hard to see all NA's

#plot the NA's by year to vizualize
n1 <- allNAs %>%
  filter(DateTime < "2008-1-1 00:00:00") %>%
  ggplot(aes(x=DateTime)) +
  geom_bar() +
  scale_x_datetime(breaks = date_breaks("1 month"), 
                   labels = date_format("%b %Y")) +
  labs(x=NULL)

n2 <- allNAs %>%
  filter(DateTime < "2009-1-1 00:00:00" & DateTime >= "2008-1-1 00:00:00") %>%
  ggplot(aes(x=DateTime)) +
  geom_bar() +
  scale_x_datetime(breaks = date_breaks("1 month"), 
                   labels = date_format("%b %Y")) +
  labs(x=NULL)

n3 <- allNAs %>%
  filter(DateTime < "2010-1-1 00:00:00" & DateTime >= "2009-1-1 00:00:00") %>%
  ggplot(aes(x=DateTime)) +
  geom_bar() +
  scale_x_datetime(breaks = date_breaks("1 month"), 
                   labels = date_format("%b %Y")) +
  labs(x=NULL)

n4 <- allNAs %>%
  filter(DateTime > "2010-1-1 00:00:00") %>%
  ggplot(aes(x=DateTime)) +
  geom_bar() +
  scale_x_datetime(breaks = date_breaks("1 month"), 
                   labels = date_format("%b %Y")) +
  labs(x=NULL)

grid.arrange(n1, n2, n3, n4, ncol = 1, top = "All NA's in Data Set")

#still not showing all NAs, need to show by month
allNAs$month <- month(allNAs$DateTime, label = TRUE, abbr = TRUE)
allNAs$month <- as.factor(allNAs$month)

allNAs$year <- year(allNAs$DateTime)
allNAs$year <- as.factor(allNAs$year)

#try summarizing instead
SumNAs <- allNAs %>%
  group_by(year, month) %>%
  summarize(Missing = sum(is.na(S4_Rest)))
#check to be sure we have all NAs accounted for
sum(SumNAs$Missing) #result is 25979!

#plot the summed table of missing data
ggplot(SumNAs, aes(x=month, y=Missing)) +
  geom_col(aes(fill = month)) +
  scale_y_sqrt() +
  facet_rep_wrap(~year, repeat.tick.labels = TRUE, scales = ) +
  labs(y="Number of Minutes of Missing Submeter Data", x="Month of Year",
       title="Missing Submeter Data by Year")

#subset April 2007 bc of large chunk of missing data
April07 <- filter(allNAs, month == "Apr" & year == "2007")
summary(April07)
#it was hotter than average that day, but weather history doesn't show
#a storm that might have caused power outage

#subset August 2010 bc of large chunk of missing data
Aug10 <- filter(allNAs, month == "Aug" & year == "2010")
summary(Aug10)
Aug10$day <- day(Aug10$DateTime)
hist(Aug10$day)
#missing data bewteen 8/17 and 8/22

#subset August 2010 bc of large chunk of missing data
Sep10 <- filter(allNAs, month == "Sep" & year == "2010")
summary(Sep10)
Sep10$day <- day(Sep10$DateTime)
hist(Sep10$day)
#missing data bewteen 9/25-28

#June 2009: 13, 14,15
Jun09 <- filter(allNAs, month == "Jun" & year == "2009")
summary(Jun09)
Jun09$day <- day(Jun09$DateTime)
hist(Jun09$day)

#August 2009: August 13 
Aug09 <- filter(allNAs, month == "Aug" & year == "2009")
summary(Aug09)
Aug09$day <- day(Aug09$DateTime)
hist(Aug09$day)

#January 2010: 12, 13, 14
Jan10 <- filter(allNAs, month == "Jan" & year == "2010")
summary(Jan10)
Jan10$day <- day(Jan10$DateTime)
hist(Jan10$day)

#March 2010: 20, 21
Mar10 <- filter(allNAs, month == "Mar" & year == "2010")
summary(Mar10)
Mar10$day <- day(Mar10$DateTime)
hist(Mar10$day)


#Look for other pattersn in NAs
#day of week?
allNAs$DayofWeek <- weekdays(allNAs$DateTime)
ggplot(allNAs, aes(x=DayofWeek, fill=DayofWeek)) +
  geom_bar() +
  labs(x="Day of the Week", y="Number of Minutes Missing",
       title = "Missing Submeter Data by Day of Week")
  
#mostly Saturdays and Sundays in the missing data

#time of day?
allNAs$HourofDay <- hour(allNAs$DateTime)
allNAs$HourofDay <- as.factor(allNAs$HourofDay)
ggplot(allNAs, aes(x=HourofDay, fill=HourofDay)) +
  geom_bar() +
  labs(x="Hour of the Day", y="Number of Minutes Missing",
       title = "Missing Submeter Data by Hour of Day")

#Day of month
allNAs$DayofMonth <- day(allNAs$DateTime)
allNAs$HourofDay <- as.factor(allNAs$HourofDay)
ggplot(allNAs, aes(x=DayofMonth)) +
  geom_bar(fill = "indianred") +
  scale_x_continuous(breaks = seq(1, 31, 1)) +
  labs(x="Day of the Month", y="Number of Minutes Missing",
       title = "Missing Submeter Data by Day of Month")


saveRDS(allNAs, file="allNAs.rds")
