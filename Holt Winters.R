#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggfortify)
library(vars)
library(forecast)
library(fpp2)
library(lubridate)
library(scales)
library(lemon)
options(scipen=999)
library(GGally)
library(plotly)
library(seasonal)

setwd("~/Ubiqum/Project 4/Task 1/Original Data Set")
power2 <- readRDS("~/Ubiqum/Project 4/Task 1/Original Data Set/power2.rds")

#create daily total for each submeter
pDay <- power2 %>%
  group_by(day=floor_date(DateTime, "day")) %>%
  summarize_at(vars(S1_Kitchen:S4_Rest), sum, na.rm = TRUE) %>%
  mutate(total = S1_Kitchen + S2_Laundry + S3_WH_AC + S4_Rest)


#create non seasonal time series data for Holt Winters
DaySample <- filter(pDay, day < "2009-05-01", day > "2009-02-01")
DaySampleTS <- ts(DaySample[,6], frequency = 30, start = c(2, 1))

autoplot(DaySampleTS)+
  scale_x_continuous(labels = c("Feb 2009", "Mar 2009", "Apr 2009", "May 2009"))

DaySampleTS %>% decompose %>%
  autoplot() + xlab("Month") +
  ggtitle("Decomposed Daily Electric Consumption")
  
ggAcf(DaySampleTS)


#create weekly sampled data
weekTotal <- power2 %>%
  group_by(week=floor_date(DateTime, "week")) %>%
  summarize_at(vars(S1_Kitchen:Total), sum, na.rm = TRUE)

weekTS <- ts(weekTotal[,6], frequency = 52, start = c(2006, 50))
autoplot(weekTS) +
  xlab("Year") +
  ylab("Total Watt-Hours") +
  ggtitle("Weekly Household Electric Consumption")

#zoom in to just 2007
autoplot(weekTS) +
  xlab("Year") +
  ylab("Total Watt-Hours") +
  ggtitle("Weekly Household Electric Consumption") +
  scale_x_continuous(limits = c(2007,2008))

#create ts object for just first few weeks in 2007
week07 <- weekTotal[5:20, 6]
week07TS <- ts(week07, frequency = 52, start = c(2007, 1))

autoplot(week07TS)
ggAcf(week07TS)

#Creat model and forecast using holt winters for next 4 weeks
week07HW <- HoltWinters(week07TS, gamma = FALSE, beta = FALSE)
week07HWf <- forecast(week07HW, h=4)

autoplot(week07HWf) +
  xlab("Year") +
  ylab("Consumption in Watt Hours") +
  ggtitle("Weekly Consumption Forecast using Holt Winters")

summary(week07HWf)
