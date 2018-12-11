#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
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

#create a daily total for each submeter
pweek <- power2 %>%
  group_by(week=floor_date(DateTime, "week")) %>%
  summarize_at(vars(S1_Kitchen:S4_Rest), sum, na.rm = TRUE) %>%
  mutate(total = S1_Kitchen + S2_Laundry + S3_WH_AC + S4_Rest)

#store as ts object
pweekTS <- ts(pweek, frequency = 52, start = c(2006, 50))

autoplot(pweekTS[,"total"]) +
  ggtitle("Household Total") +
  xlab("Year") +
  ylab("Watt Hours")

#make a seasonal plot
ggseasonplot(pweekTS[,"total"], year.labels=TRUE) +
  ylab("Watt Hours") +
  ggtitle("Total Household Consumption")

#create lag plots
gglagplot(pweekTS[,"total"])

#create acf plots
ggAcf(pweekTS[,"total"]) 


#use tsclean function to replace outliers with suggestions
pweekTS[,"total"] <- tsclean(pweekTS[,"total"])

#plot the decomposition
pweekTS[,"total"] %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Decomposed Weekly Electric Consumption in Watt-Hours") 


#creating multiple seasonality ts object with weekly and annual frequency
pwkMSTS <- msts(pweekTS[,"total"], 52, start = c(2006, 50))
pwkMSTS %>% mstl() %>%
  autoplot() + xlab("year")

pwkMSTS %>% stlf(h=12) %>% autoplot()


fcastwk <- stlf(pwkMSTS, h=12)
autoplot(fcastwk)

checkresiduals(fcastwk)
summary(fcastwk)
accuracy(fcastwk)

hw()

#super simple seasonal naive forecast
simpleWK <- snaive(pwkMSTS)
WKsimple <- forecast(simpleWK, h=12)
autoplot(WKsimple)
checkresiduals(WKsimple)
summary(WKsimple)
accuracy(simpleWK)

#plot the seasonal naive along with the STL
autoplot(pwkMSTS) +
  autolayer(WKsimple, series = "Seas. Naive", PI = FALSE) +
  autolayer(fcastwk, series = "STL", PI = FALSE) +
  scale_x_continuous(limits = c(2010.5, 2011.2)) +
  xlab("Week") + ylab("Consumption in Watt-Hours") +
  ggtitle("Weekly Forecast Comparison for Next 12 Weeks")

