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
power2Tidy <- readRDS("~/Ubiqum/Project 4/Task 1/Original Data Set/power2Tidy.rds")

powerts <- ts(power2, frequency = 525600, start = 503604)
autoplot(powerts[,"S1_Kitchen"])

#create hourly total for each submeter
pHour <- power2 %>%
  group_by(hour=floor_date(DateTime, "hour")) %>%
  summarize_at(vars(S1_Kitchen:S4_Rest), sum, na.rm = TRUE) %>%
  mutate(total = S1_Kitchen + S2_Laundry + S3_WH_AC + S4_Rest)



#create daily total for each submeter
pDay <- power2 %>%
  group_by(day=floor_date(DateTime, "day")) %>%
  summarize_at(vars(S1_Kitchen:S4_Rest), sum, na.rm = TRUE) %>%
  mutate(total = S1_Kitchen + S2_Laundry + S3_WH_AC + S4_Rest)

pDayTS <- ts(pDay, frequency = 365, start = c(2006, 350))
autoplot(pDayTS[,"S1_Kitchen"])


#create a monthly total for each submeter
pMonth <- power2 %>%
  group_by(month=floor_date(DateTime, "month")) %>%
  summarize_at(vars(S1_Kitchen:S4_Rest), sum, na.rm = TRUE) %>%
  mutate(total = S1_Kitchen + S2_Laundry + S3_WH_AC + S4_Rest)

#store as ts object
pMonthTS <- ts(pMonth, frequency = 12, start = c(2006, 12))

a1 <- autoplot(pMonthTS[,"S1_Kitchen"]) +
  ggtitle("Kitchen") +
  xlab("Year") +
  ylab("Watt Hours")

a2 <- autoplot(pMonthTS[,"S2_Laundry"]) +
  ggtitle("Laundry Room") +
  xlab("Year") +
  ylab("Watt Hours")

a3 <- autoplot(pMonthTS[,"S3_WH_AC"]) +
  ggtitle("Water Heater & A/C") +
  xlab("Year") +
  ylab("Watt Hours")

a4 <- autoplot(pMonthTS[,"S4_Rest"]) +
  ggtitle("Rest of House") +
  xlab("Year") +
  ylab("Watt Hours")

a5 <- autoplot(pMonthTS[,"total"]) +
  ggtitle("Household Total") +
  xlab("Year") +
  ylab("Watt Hours")

grid.arrange(a1, a2, a3, a4, a5, ncol=3, 
             top = "Annual Electric Consumption by Sub-meter")


#make a seasonal plot
s1 <- ggseasonplot(pMonthTS[,"S1_Kitchen"], year.labels=TRUE) +
  ylab("Total Watt Hours Consumed") +
  ggtitle("Kitchen")

s2 <- ggseasonplot(pMonthTS[,"S2_Laundry"], year.labels=TRUE) +
  ylab("Total Watt Hours Consumed") +
  ggtitle("Laundry Room")

s3 <- ggseasonplot(pMonthTS[,"S3_WH_AC"], year.labels=TRUE) +
  ylab("Total Watt Hours Consumed") +
  ggtitle("Water Heater & A/C")

s4 <- ggseasonplot(pMonthTS[,"S4_Rest"], year.labels=TRUE) +
  ylab("Total Watt Hours Consumed") +
  ggtitle("Rest of House")

s5 <- ggseasonplot(pMonthTS[,"total"], year.labels=TRUE) +
  ylab("Total Watt Hours Consumed") +
  ggtitle("Household Total")

grid.arrange(s1, s2, s3, s4, s5, ncol = 3, 
             top = "Monthly Electric Consumption by Sub-meter")

#make polar seasonal plot
ggseasonplot(pMonthTS[,"total"], polar=TRUE, year.labels=TRUE) +
  ylab("Total Watt Hours Consumed") +
  ggtitle("Household Total")

#create subseries seasonal plot
ss1 <- ggsubseriesplot(pMonthTS[,"S1_Kitchen"], year.labels=TRUE) +
  ylab("Total Watt Hours Consumed") +
  ggtitle("Kitchen")

ss2 <- ggsubseriesplot(pMonthTS[,"S2_Laundry"], year.labels=TRUE) +
  ylab("Total Watt Hours Consumed") +
  ggtitle("Laundry Room")

ss3 <- ggsubseriesplot(pMonthTS[,"S3_WH_AC"], year.labels=TRUE) +
  ylab("Total Watt Hours Consumed") +
  ggtitle("Water Heater & A/C")

ss4 <- ggsubseriesplot(pMonthTS[,"S4_Rest"], year.labels=TRUE) +
  ylab("Total Watt Hours Consumed") +
  ggtitle("Rest of House")

ss5 <- ggsubseriesplot(pMonthTS[,"total"], year.labels=TRUE) +
  ylab("Total Watt Hours Consumed") +
  ggtitle("Household Total")

grid.arrange(ss1, ss2, ss3, ss4, ss5, ncol = 3, 
             top = "Monthly Electric Consumption by Sub-meter")


#create faceted charts
autoplot(pMonthTS[,2:6], facets=TRUE, color="red") +
  geom_smooth()+
  ylab("Electric Consumption in Watt Hours")

#compare variables against each other
ggpairs(as.data.frame(pMonthTS[,2:6]))

#create lag plots
gglagplot(pMonthTS[,2])
gglagplot(pMonthTS[,3])
gglagplot(pMonthTS[,4])
gglagplot(pMonthTS[,5])
gglagplot(pMonthTS[,6])

#create acf plots
ggAcf(pMonthTS[,2])
ggAcf(pMonthTS[,3])
ggAcf(pMonthTS[,4])
ggAcf(pMonthTS[,5])
ggAcf(pMonthTS[,6]) 
#When data are seasonal, the autocorrelations will be larger for 
#the seasonal lags (at multiples of the seasonal frequency) than 
#for other lags.

#for white noise data, the acf plots will have values close to zero


#try some forecasting tools
# Set training data from Dec 2006 to Jan 2010
test1 <- window(pMonthTS,start=c(2006,12), end=c(2010,1))
rest1 <- window(pMonthTS, start=c(2010,1))

# Plot some forecasts
autoplot(pMonthTS[,6]) +
  autolayer(meanf(test1[,6], h=10),
            series="Mean", PI=FALSE) +
  autolayer(naive(test1[,6], h=10),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(test1[,6], h=10),
            series="Seasonal naïve", PI=FALSE) +
  autolayer(rwf(test1[,6], h=10, drift=TRUE), series="Drift", PI=FALSE) +
  ggtitle("Forecasts for Household Electric Consumption") +
  xlab("Year") + 
  ylab("Total Household Watt Hours") +
  guides(colour=guide_legend(title="Forecast"))


#focus on seasonal naive method
SeasN <- snaive(test1[,6], h=10)
checkresiduals(SeasN)
accuracy(SeasN, rest1[,6])

#try a linear regression forecast
#create training and test set
trainLRM <- window(pMonthTS,start=c(2006,12), end=c(2010,2))
testLRM <- window(pMonthTS, start=c(2010,2))
SeasN1 <- snaive(trainLRM,h=10)

#try cross validation
CV1 <- tsCV(pMonthTS[,6], snaive, h=10)
#i have no idea what this just did...

#produce blind forecast using basic forecast function
pMonthF <- forecast(trainLRM[,6], h=10)
autoplot(pMonthF) +
  autolayer(testLRM[,6], series = "Actual Consumption") +
  xlab("Year") +
  ylab("Consumption in Watt Hours") +
  ggtitle("'Blind' Forecast")

#produce a blind forecast beyond the data set
F2012 <- forecast(pMonthTS[,6], h=13)
autoplot(F2012) +
  xlab("Year") +
  ylab("Consumption in Watt Hours") +
  ggtitle("'Blind' Forecast for 2011")

summary(F2012)


#outliers?
tsoutliers(pMonthTS[,6])

#use tsclean function to replace outliers with suggestions
pMonthClean <- tsclean(pMonthTS[,6])
#rerun forecast
F2012clean <- forecast(pMonthClean, h=13)
autoplot(F2012clean) +
  xlab("Year") +
  ylab("Consumption in Watt Hours") +
  ggtitle("'Blind' Forecast with Cleaned Data for 2011")
summary(F2012clean)
summary(F2012)

#create a double plot with clean and unclean data
c1 <- autoplot(F2012) +
  xlab("Year") +
  ylab("Consumption in Watt Hours") +
  ggtitle("'Blind' Forecast for 2011")

c2 <- autoplot(F2012clean) +
  xlab("Year") +
  ylab("Consumption in Watt Hours") +
  ggtitle("'Blind' Forecast with Cleaned Data for 2011")

grid.arrange(c1, c2, ncol=1)


#create future forecast for next 30 days
#use tsclean function to replace outliers with suggestions
DayClean <- tsclean(pDayTS[,6])
#rerun forecast
FDayClean <- forecast(DayClean, h=100)
autoplot(FDayClean)  +
  xlab("Year") +
  ylab("Consumption in Watt Hours") +
  ggtitle("'Blind' Forecast for Next 400 Days") + 
  scale_x_continuous(limits = c(2010.75, 2011.25))
  
#try a smaller subset of days to predict
SubDays <- window(DayClean, start = c(2010, 1))
autoplot(SubDays)

FSubDays <- forecast(SubDays, h=31)
autoplot(FSubDays) +
  xlab("Year") +
  ylab("Consumption in Watt Hours") +
  ggtitle("'Blind' Forecast for Next 31 Days") 

#create hourly forecast
pHourSample <- filter(pHour, hour > "2010-11-01 00:00:00")
pHSampleTS <- ts(pHourSample, frequency = 24, start = c(1, 0))
autoplot(pHSampleTS[,6])

F24Hours <- forecast(pHSampleTS[,6], h=24*7)
autoplot(F24Hours)
#produced negative numbers?! whatttt....


#try a tslm model on monthly data using trend and season
LMMonth <- tslm(pMonthClean ~ trend + season)
summary(LMMonth)

autoplot(pMonthClean, series="Data") +
  autolayer(fitted(LMMonth), series="Fitted") +
  xlab("Year") + ylab("Watt Hours") +
  ggtitle("Montly Electric Consumption")

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
c3 <- autoplot(LMfcast) +
  ggtitle("Regression Forecast with Cleaned Data") +
  xlab("Year") + ylab("Watt Hours")

grid.arrange(c1, c2, c3, ncol=1)

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

#decompose the daily time series 
pDayD <- decompose(DayClean)

#plot the classical decomposition
DayClean %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Decomposed Daily Electric Consumption in Watt-Hours") 




