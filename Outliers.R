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

#look at kitchen outliers
boxplot(power2$S1_Kitchen)
summary(power2$S1_Kitchen)

#Max for S1 is 88, let's look at that value
S1outs <- filter(power2, S1_Kitchen > 60)
S2outs <- filter(power2, S2_Laundry > 60)
#dont think there are any outliers for S3

#plot outliers for each Sub meter by month
ggplot(S1outs, aes(x=month)) +
  geom_bar(fill = "indianred") +
  labs(x="Month", y="Number of Outliers", 
       title = "Sub-meter 1 (Kitchen) Outliers by Month",
       subtitle = "Outliers are sub-meter readings over 60 watt-hours") +
  facet_rep_wrap(~year, repeat.tick.labels = TRUE)

ggplot(S2outs, aes(x=month)) +
  geom_bar(fill="palegreen3") +
  labs(x="Month", y="Number of Outliers", 
       title = "Sub-meter 2 (Laundry Room) Outliers by Month", 
       subtitle = "Outliers are sub-meter readings over 60 watt-hours") +
  facet_rep_wrap(~year, repeat.tick.labels = TRUE)


#plot outliers for each Sub meter by day of week
d1 <- ggplot(S1outs, aes(x=dayofWeek)) +
  geom_bar(fill = "indianred") +
  labs(x="Day of Week", y="Number of Outliers", 
       title = "Sub-meter 1 (Kitchen) Outliers by Day of Week",
       subtitle = "Outliers are sub-meter readings over 60 watt-hours")

d2 <- ggplot(S2outs, aes(x=dayofWeek)) +
  geom_bar(fill="palegreen3") +
  labs(x="Day of Week", y="Number of Outliers", 
       title = "Sub-meter 2 (Laundry Room) Outliers by Day of Week", 
       subtitle = "Outliers are sub-meter readings over 60 watt-hours")
grid.arrange(d1, d2, ncol=1)

#plot outliers for each Sub meter by hour of day
h1 <- ggplot(S1outs, aes(x=hour)) +
  geom_bar(fill = "indianred") +
  labs(x="Time of Day", y="Number of Outliers", 
       title = "Sub-meter 1 (Kitchen) Outliers by Time of Day",
       subtitle = "Outliers are sub-meter readings over 60 watt-hours") +
  scale_x_continuous(breaks = seq(0, 24, 3))

h2 <- ggplot(S2outs, aes(x=hour)) +
  geom_bar(fill="palegreen3") +
  labs(x="Time of Day", y="Number of Outliers", 
       title = "Sub-meter 2 (Laundry Room) Outliers by Time of Day", 
       subtitle = "Outliers are sub-meter readings over 60 watt-hours") +
  scale_x_continuous(breaks = seq(0, 24, 3))

grid.arrange(h1, h2, ncol = 1)


#plot outliers for each Sub meter by year
y1 <- ggplot(S1outs, aes(x=year)) +
  geom_bar(fill = "indianred") +
  labs(x="Year", y="Number of Outliers", 
       title = "Sub-meter 1 (Kitchen) Outliers by Year",
       subtitle = "Outliers are sub-meter readings over 60 watt-hours")

y2 <- ggplot(S2outs, aes(x=year)) +
  geom_bar(fill="palegreen3") +
  labs(x="Year", y="Number of Outliers", 
       title = "Sub-meter 2 (Laundry Room) Outliers by Year", 
       subtitle = "Outliers are sub-meter readings over 60 watt-hours")
grid.arrange(y1, y2, ncol=2)

#examine the 2010 past 11pm outliers for kitchen
K11pm <- filter(S1outs, year == 2010 & (hour == 23 | hour == 24))
#all values are on the same day, Saturday June 5, 2010. If all appliances
#were running at same time (oven, dishwasher, and microwave), these numbers
#are plausible. Since it's a Saturday...it makes sense that they would be
#up late cooking/cleaning. Maybe they had a party and were getting snacks/
#desserts out

#examine the S2 outliers between 12am and 1am
LR1am <- filter(S2outs, hour == 0 | hour == 1)
