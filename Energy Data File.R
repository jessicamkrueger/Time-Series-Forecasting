#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggfortify)
library(vars)
library(forecast)
library(lubridate)

#Load data set
setwd("~/Ubiqum/Project 4/Task 1/Original Data Set")
power <- read.csv("~/Ubiqum/Project 4/Task 1/Original Data Set/household_power_consumption.txt", 
                  sep=";", stringsAsFactors=FALSE)

for(i in c(3:ncol(power))) {
  power[,i] <- as.numeric(power[,i])
}

#combine date & time into one column
power1 <- mutate(power, DateTime = paste(Date, Time, sep = ' '))
power2 <- select(power1, DateTime, Global_active_power:Sub_metering_3)

#convert DateTime to time using lubridate
power2$DateTime <- dmy_hms(power2$DateTime)

#check structure of data to confirm changes to data types
str(power2)


#summary stats for data set
summary(power2)
#no date NA's!!! (after using lubridate)

#standard deviations of all variables
sd(power2$Global_active_power, na.rm = TRUE)
sd(power2$Global_reactive_power, na.rm = TRUE)
sd(power2$Voltage, na.rm = TRUE)
sd(power2$Global_intensity, na.rm = TRUE)
sd(power2$Sub_metering_1, na.rm = TRUE)
sd(power2$Sub_metering_2, na.rm = TRUE)
sd(power2$Sub_metering_3, na.rm = TRUE)

#boxplots of all variables
power3 <- select(power2, -DateTime)
boxplot(power3)

#This code quickly produces all histograms but they aren't labled so it's not super useful
for(i in c(2:ncol(power2))) {
  hist(power2[,i])
}

#quick histograms of each variable
hist(power2$Global_active_power)
hist(power2$Global_reactive_power)
hist(power2$Voltage)
hist(power2$Global_intensity)
hist(power2$Sub_metering_1)
hist(power2$Sub_metering_2)
hist(power2$Sub_metering_3)

#Explor sub metering 1 more
ggplot(power2, aes(Sub_metering_1)) +
  geom_histogram(binwidth = 1)

#change variable names to be shorter
power2 <- rename(power2, GAP = Global_active_power)
power2 <- rename(power2, GRP = Global_reactive_power)
power2 <- rename(power2, GI = Global_intensity)
power2 <- rename(power2, S1_Kitchen = Sub_metering_1)
power2 <- rename(power2, S2_Laundry = Sub_metering_2)
power2 <- rename(power2, S3_WH_AC = Sub_metering_3)


#transform the Global Active Power to match the submeters
power2 <- mutate(power2, S4_Rest = (GAP*1000/60 - Sub1 - Sub2 - Sub3))
saveRDS(power2, file = "Power2.rds")
summary(power2)

#subset one day of data to look at initially
oneday <- subset(power2, DateTime >= "2006-12-17 00:00:00" & DateTime < "2006-12-18 00:00:00")
str(oneday)
oneday$DateTime <- as.POSIXct(oneday$DateTime)
dec17TS <- ts(oneday, start = 0, frequency = 60)

#plot of 24 hours of each submeter 1-3
p1 <- ggplot(oneday, aes(x=DateTime, y= Sub1)) +
  geom_line() +
  labs(y="Sub 1: Kitchen")

p2 <- ggplot(oneday, aes(x=DateTime, y= Sub2)) +
  geom_line() +
  labs(y="Sub 2: Laundry Room")

p3 <- ggplot(oneday, aes(x=DateTime, y= Sub3)) +
  geom_line() +
  labs(y="Sub 3: Water Heater & A/C")

grid.arrange(p1, p2, p3, ncol=1)

#convert data to a ts object
powerTS <- ts(power2, frequency = 1440)

#try to plot ts data
autoplot(powerTS[,"GAP"]) #too much noise

#plot just the one day ts data
autoplot(dec17TS[,"GAP"]) +
  labs(x = "Hour", y="Global Active Power", 
       title = "December 17 - Global Active Power by Hour")

#create multivariate plot
autoplot(dec17TS, facets = TRUE) +
  scale_x_continuous(breaks = seq(0, 24, 1)) +
  labs(x = "Hour of Day", y = "Value", title = "December 17, 2006")

#create same mutlivariate plot on single axis by specifying facets = FALSE
autoplot(dec17TS, facets = FALSE) +
  scale_x_continuous(breaks = seq(1, 25, 1)) +
  labs(x = "Hour of Day", y = "Value", title = "December 17, 2006")
#the above doens't produce useful chart bc scales of data are off


#create season plot
ggseasonplot(powerTS)


