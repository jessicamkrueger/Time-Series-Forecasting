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

#create a monthly total for each submeter
pMonth <- power2 %>%
  group_by(month=floor_date(DateTime, "month")) %>%
  summarize_at(vars(S1_Kitchen:S4_Rest), sum, na.rm = TRUE) %>%
  mutate(total = S1_Kitchen + S2_Laundry + S3_WH_AC + S4_Rest)

#store as ts object
pMonthTS <- ts(pMonth, frequency = 12, start = c(2006, 12))

autoplot(pMonthTS[,"total"]) +
  ggtitle("Household Total") +
  xlab("Year") +
  ylab("Watt Hours")

#make a seasonal plot
ggseasonplot(pMonthTS[,"total"], year.labels=TRUE) +
  ylab("Watt Hours") +
  ggtitle("Total Household Consumption")

#create lag plots
gglagplot(pMonthTS[,6])

#create acf plots
ggAcf(pMonthTS[,6]) 


#outliers?
tsoutliers(pMonthTS[,6])

#use tsclean function to replace outliers with suggestions
pMonthClean <- tsclean(pMonthTS[,6])

# Set training data from Dec 2006 to Jan 2010
test1 <- window(pMonthClean,start=c(2006,12), end=c(2010,1))
rest1 <- window(pMonthClean, start=c(2010,1))


#try a tslm model on monthly data using trend and season
#training set
LMMonth <- tslm(test1 ~ trend + season)
summary(LMMonth)

#plot the model and the training set
autoplot(test1, series="Data") +
  autolayer(fitted(LMMonth), series="Fitted") +
  xlab("Year") + 
  ylab("Watt Hours") +
  ggtitle("Montly Electric Consumption")

#plot the residuals 
checkresiduals(LMMonth)

#plot of errors
cbind(Data=pMonthClean, Fitted=fitted(LMMonth)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted)) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Monthly Consumption - Linear Model Errors") +
  geom_abline(intercept=0, slope=1)

#use linear model to forecast
LMfcast <- forecast(LMMonth, h=13)

autoplot(LMfcast) +
  ggtitle("Regression Forecast with Cleaned Data") +
  xlab("Year") + ylab("Watt Hours")



summary(LMfcast)
accuracy(LMfcast)
checkresiduals(LMfcast)


#decompose the monthly time series 
pMonthD <- decompose(pMonthClean)

#plot the decomposition
pMonthClean %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Decomposed Monthly Electric Consumption in Watt-Hours") 

#summarize results
summary(pMonthD)

#plot the monthly X11 decomposition
pMonthClean %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 Decomposed Monthly Electric Consumption")






