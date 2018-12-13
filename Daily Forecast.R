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
pday <- power2 %>%
  group_by(day=floor_date(DateTime, "day")) %>%
  summarize_at(vars(S1_Kitchen:S4_Rest), sum, na.rm = TRUE) %>%
  mutate(total = S1_Kitchen + S2_Laundry + S3_WH_AC + S4_Rest) %>%
  mutate(day_type = wday(day, label = TRUE))

#creates dummy varible (1= weekend, 0=weekday)
pday$day_type <- recode(pday$day_type, "Sat" = 1, "Sun"= 1)
pday[is.na(pday)] <- 0

#store as ts object
pdayTS <- ts(pday, frequency = 365, start = c(2006, 350))

autoplot(pdayTS[,"total"]) +
  ggtitle("Household Total") +
  xlab("Year") +
  ylab("Watt Hours")

#make a seasonal plot
ggseasonplot(pdayTS[,"total"], year.labels=TRUE) +
  ylab("Watt Hours") +
  ggtitle("Total Household Consumption")

#create lag plots
gglagplot(pdayTS[,"total"])

#create acf plots
ggAcf(pdayTS[,"total"]) 


#use tsclean function to replace outliers with suggestions
pdayTS[,"total"] <- tsclean(pdayTS[,"total"])

# Set training and test set
trainDay <- window(pdayTS,start=c(2006,350), end=c(2010,43))
testDay<- window(pdayTS, start=c(2010,44))


#try a tslm model on daily data using trend and season
#training set
LMDay <- tslm(trainDay ~ trend + season)
summary(LMDay)
checkresiduals(LMDay)

#try holt winters -- frequency too high
fit7 <- hw(trainDay[,6],seasonal="additive")
fit8 <- hw(trainDay[,6],seasonal="multiplicative")

#plot the model and the training set
autoplot(trainDay, series="Data") +
  autolayer(fitted(LMDay), series="Fitted") +
  xlab("Year") + 
  ylab("Watt Hours") +
  ggtitle("Daily Electric Consumption") 


#plot of errors
cbind(Data=pDayClean, Fitted=fitted(LMDay)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted)) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Daily Consumption - Linear Model Errors") +
  geom_abline(intercept=0, slope=1)


#decompose the monthly time series 
pDayD <- decompose(pDayClean)

#plot the decomposition
pDayClean %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Decomposed Daily Electric Consumption in Watt-Hours") 

#try adding another variable into model
fit6 <- auto.arima(pdayTS[,"total"],
                  xreg=pdayTS[,"day_type"])
checkresiduals(fit6)
summary(fit6)

autoplot(trainDay[,6]) +
  autolayer(fitted(fit6), series = "fitted")


#create forecast using the next 288 days values for day_type
fcast6 <- forecast(fit6, xreg = f6reg)
autoplot(testDay[,6])

autoplot(fcast6)

#creating multiple seasonality ts object with weekly and annual frequency
pdayMSTS <- msts(pday, c(365, 7), start = c(2006, 350, 7))
pdayMSTS[,6] %>% mstl() %>%
  autoplot() + xlab("year")

pdayMSTS[,6] %>% stlf(h=30) %>% autoplot() + 
  scale_x_continuous(limits=c(2010.75, 2011.25))


fcast7 <- stlf(pdayMSTS[,6], h=30)
autoplot(fcast7) +
  scale_x_continuous(limits=c(2010.75, 2011))

checkresiduals(fcast7)
summary(fcast7)


#try using auto-arima
bestfit <- list(aicc=Inf)
for(K in seq(3)) {
  fit <- auto.arima(pdayMSTS[,6], xreg=fourier(pdayMSTS[,6], K=c(K,K)),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}               


fcast9 <- forecast(bestfit),
               xreg=fourier(pdayMSTS[,6], K=c(bestK, bestK), h=90))
autoplot(fcast9)
checkresiduals(fcast9)

#super simple seasonal naive forecast
simple <- snaive(pdayMSTS[,6])
fsimple <- forecast(simple, h=90)
autoplot(fsimple)
checkresiduals(fsimple)
summary(fsimple)
accuracy(fsimple)
accuracy(fcast9)
accuracy(fcast7)
