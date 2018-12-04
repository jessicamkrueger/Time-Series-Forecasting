#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggfortify)
library(vars)
library(forecast)
library(lubridate)

setwd("~/Ubiqum/Project 4/Task 1/Original Data Set")
power2 <- readRDS("power2.rds")

#sample by only Wednesdays data and tidy
power2$dayofWeek <- wday(power2$DateTime, label = TRUE)
Weds <- filter(power2, power2$dayofWeek == "Wed")
Weds <- gather(Weds, SubMeter, WattHour, S1_Kitchen:Total)
Weds$date <- date(Weds$DateTime)

Weds1 <- group_by(Weds, date, SubMeter) %>%
  summarize(DayTotalWH = sum(WattHour))

#plot the wednesdays
ggplot(Weds1, aes(x=date, y=DayTotalWH)) +
  geom_line() +
  facet_wrap(~SubMeter)

#sample by hour 
power2$hour <- hour(power2$DateTime)


#total one day's kWh
power2$day <- day(power2$DateTime)
power2$month <- month(power2$DateTime, label = TRUE)
power2$year <- year(power2$DateTime)

#create total usage by day 2006-2010
dayTotals <- power2 %>%
  group_by(year, month, day) %>%
  summarize(S1_DayTotal = sum(S1_Kitchen, na.rm = TRUE),
            S2_DayTotal = sum(S2_Laundry, na.rm = TRUE),
            S3_DayTotal = sum(S3_WH_AC, na.rm = TRUE),
            S4_DayTotal = sum(S4_Rest, na.rm = TRUE))

#create total usage by month 2006-2010
monthTotals <- power2 %>%
  group_by(year, month) %>%
  summarize(S1_Kitchen = sum(S1_Kitchen, na.rm = TRUE),
            S2_Laundry = sum(S2_Laundry, na.rm = TRUE),
            S3_WH_AC = sum(S3_WH_AC, na.rm = TRUE),
            S4_Rest = sum(S4_Rest, na.rm = TRUE)) %>%
  gather(SubMeter, Totalwh, S1_Kitchen:S4_Rest) %>%
  mutate(MonthYear = paste(month, year, sep = '-')) %>%
  mutate(kWh = Totalwh/1000)


#plot monthly totals in watt hours
monthTotals %>%
  filter(year != 2006) %>%
  ggplot(aes(x=month, y=kWh, group=SubMeter)) +
  geom_line(aes(color = SubMeter)) +
  facet_rep_wrap(~year, repeat.tick.labels = TRUE) +
  labs(x="Month", y= "Kilowatt Hours Used", 
       title="Monthly Kilowatt Totals")

#Monthly totals by submeter
S1monthTotals <- filter(monthTotals, SubMeter == "S1_Kitchen")

