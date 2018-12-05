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
setwd("~/Ubiqum/Project 4/Task 1/Original Data Set")
power2 <- readRDS("~/Ubiqum/Project 4/Task 1/Original Data Set/power2.rds")
power2Tidy <- readRDS("~/Ubiqum/Project 4/Task 1/Original Data Set/power2Tidy.rds")

saveRDS(power2, file="power2.rds")

#what is total power used by each sub meter?
SubSums <- summarize_all(power2, sum, na.rm = TRUE)
SubSums1 <- select(SubSums, S1_Kitchen:S4_Rest)
SubSums2 <- gather(SubSums1, Sub_Meter, Total_WH_Active_Energy)

#Plot total watt-hours used by sub meter
ggplot(SubSums2, aes(x=Sub_Meter, y=Total_WH_Active_Energy)) +
  geom_col(aes(fill = Sub_Meter)) +
  labs(x="Sub Meter", y="Total Watt-Hour of Active Energy Used", 
       title = "Total Watt-Hour of Active Energy Used by Sub Meter - 2006-2010")

#what is average power used by each submeter
SubAvg <- summarize_all(power2, mean, na.rm = TRUE)
SubAvg1 <- dplyr::select(SubAvg, S1_Kitchen:S4_Rest) #use dplyr::select() because 
#select is masked by another loaded library right now
SubAvg2 <- gather(SubAvg1, Sub_Meter, Avg_WH_Active_Energy)

#Plot avg watt-hours used by sub meter
ggplot(SubAvg2, aes(x=Sub_Meter, y=Avg_WH_Active_Energy)) +
  geom_col(aes(fill = Sub_Meter)) +
  labs(x="Sub Meter", y="Average Watt-Hour of Active Energy Used", 
       title = "Average Watt-Hour of Active Energy Used by Sub Meter - 2006-2010")



#what is median power used by each submeter
SubMed <- summarize_all(power2, median, na.rm = TRUE)
SubMed1 <- dplyr::select(SubMed, S1_Kitchen:S4_Rest) #use dplyr::select() because 
#select is masked by another loaded library right now
SubMed2 <- gather(SubMed1, Sub_Meter, Med_WH_Active_Energy)

#Plot med watt-hours used by sub meter
ggplot(SubMed2, aes(x=Sub_Meter, y=Med_WH_Active_Energy)) +
  geom_col(aes(fill = Sub_Meter)) +
  labs(x="Sub Meter", y="Median Watt-Hour of Active Energy Used", 
       title = "Median Watt-Hour of Active Energy Used by Sub Meter - 2006-2010")

#create histograms of each sub to see distribtion and outliers
options(scipen=10000)
p1 <- ggplot(power2, aes(S1_Kitchen)) +
  geom_histogram(binwidth = 5, fill = "indianred1", color = "black") +
  scale_y_sqrt() +
  labs(x="Sub Meter 1: Kitchen", y="Square Root of Count of Observations", 
       title="Histogram of Kitchen Sub Meter")

p2 <- ggplot(power2, aes(S2_Laundry)) +
  geom_histogram(binwidth = 5, fill = "palegreen3", color = "black") +
  scale_y_sqrt() +
  labs(x="Sub Meter 2: Laundry", y="Square Root of Count of Observations", 
       title="Histogram of Laundry Room Sub Meter")

p3 <- ggplot(power2, aes(S3_WH_AC)) +
  geom_histogram(binwidth = 2, fill = "turquoise3", color = "black") +
  scale_y_sqrt() +
  labs(x="Sub Meter 3: Water Heater & A/C", y="Square Root of Count of Observations", 
       title="Histogram of Water Heater & A/C Sub Meter")

p4 <- ggplot(power2, aes(S4_Rest)) +
  geom_histogram(binwidth = 5, fill = "purple1", color = "black") +
  scale_y_sqrt() +
  labs(x="Sub Meter 4: Rest of House", y="Square Root of Count of Observations", 
       title="Histogram of Rest of House")

grid.arrange(p1, p2, p3, p4, ncol=2)

#We now have 1050 values less than zero for S4...how can that be??
negatives <- filter(power2, S4_Rest < 0)

#We also have 21 values equal to zero for S4...also doesn't seem feasible.
zeros <- filter(power2, S4_Rest == 0)

#create daily average of useage for each sub meter
DailyAvg <- power2 %>%
  dplyr::select(-(GAP:GI)) %>%
  mutate(Day = date(DateTime)) %>%
  group_by(Day) %>%
  summarize_all(mean, na.rm = TRUE)

#separate out data into 4 years of data and tidy
year1 <- filter(Day < "2007-12-16")

year2 <- filter(DailyAvg, DateTime > "2007-12-16" & 
                  DateTime < "2008-12-16")
year3 <- filter(DailyAvg, DateTime > "2008-12-16" & 
                  DateTime < "2009-12-16")
year4 <- filter(DailyAvg, DateTime > "2009-12-16")


y1 <- ggplot(year1) +
  geom_line(aes(x=DateTime, y=S1_Kitchen), color ="indianred") +
  geom_line(aes(x=DateTime, y=S2_Laundry), color ="palegreen3") +
  geom_line(aes(x=DateTime, y=S3_WH_AC), color ="turquoise3") +
  geom_line(aes(x=DateTime, y=S4_Rest), color ="purple1") +
  labs(x="Month", y="Average Daily Active Power", 
       title = "Year 1: 2006 - 2007")

y2 <- ggplot(year2) +
  geom_line(aes(x=DateTime, y=S1_Kitchen), color ="indianred") +
  geom_line(aes(x=DateTime, y=S2_Laundry), color ="palegreen3") +
  geom_line(aes(x=DateTime, y=S3_WH_AC), color ="turquoise3") +
  geom_line(aes(x=DateTime, y=S4_Rest), color ="purple1") +
  labs(x="Month", y="Average Daily Active Power", 
       title = "Year 2: 2007 - 2008")

y3 <- ggplot(year3) +
  geom_line(aes(x=DateTime, y=S1_Kitchen), color ="indianred") +
  geom_line(aes(x=DateTime, y=S2_Laundry), color ="palegreen3") +
  geom_line(aes(x=DateTime, y=S3_WH_AC), color ="turquoise3") +
  geom_line(aes(x=DateTime, y=S4_Rest), color ="purple1") +
  labs(x="Month", y="Average Daily Active Power", 
       title = "Year 3: 2008 - 2009")

y4 <- ggplot(year4) +
  geom_line(aes(x=DateTime, y=S1_Kitchen), color ="indianred") +
  geom_line(aes(x=DateTime, y=S2_Laundry), color ="palegreen3") +
  geom_line(aes(x=DateTime, y=S3_WH_AC), color ="turquoise3") +
  geom_line(aes(x=DateTime, y=S4_Rest), color ="purple1") +
  labs(x="Month", y="Average Daily Active Power", 
       title = "Year 4: 2009 - 2010")

grid.arrange(y1, y2, y3, y4, ncol = 2)

#try plotting with tidy data instead
year1tidy <- DailyAvg %>%
  filter(Day < "2008-1-1" & Day > "2006-12-31") %>%
  dplyr::select(-DateTime) %>%
  gather(SubMeter, Watt_Hour_AP, S1_Kitchen:S4_Rest) %>%
  year1tidy$Month <- month(year1tidy$Day, label = TRUE)

year2tidy <- DailyAvg %>%
  filter(Day > "2007-12-31" & Day < "2009-1-1") %>%
  dplyr::select(-DateTime) %>%
  gather(SubMeter, Watt_Hour_AP, S1_Kitchen:S4_Rest)
year2tidy$Month <- month(year2tidy$Day)

year3tidy <- DailyAvg %>%
  filter(Day > "2008-12-31" & Day < "2010-1-1") %>%
  dplyr::select(-DateTime) %>%
  gather(SubMeter, Watt_Hour_AP, S1_Kitchen:S4_Rest)
year3tidy$Month <- month(year3tidy$Day)

year4tidy <- DailyAvg %>%
  filter(Day > "2009-12-31") %>%
  dplyr::select(-DateTime) %>%
  gather(SubMeter, Watt_Hour_AP, S1_Kitchen:S4_Rest)
year4tidy$Month <- month(year4tidy$Day)

t1 <- ggplot(year1tidy) +
  geom_line(aes(x=Day, y=Watt_Hour_AP, color =SubMeter)) +
  labs(x="Date", y="Average Daily Active Power", 
       title = "Year 1: 2006 - 2007")
t2 <- ggplot(year2tidy) +
  geom_line(aes(x=Day, y=Watt_Hour_AP, color =SubMeter)) +
  labs(x="Date", y="Average Daily Active Power", 
       title = "Year 2: 2007 - 2008")
t3 <- ggplot(year3tidy) +
  geom_line(aes(x=Day, y=Watt_Hour_AP, color =SubMeter)) +
  labs(x="Date", y="Average Daily Active Power", 
       title = "Year 3: 2008 - 2009")
t4 <- ggplot(year2tidy) +
  geom_line(aes(x=Day, y=Watt_Hour_AP, color =SubMeter)) +
  labs(x="Date", y="Average Daily Active Power", 
       title = "Year 4: 2009 - 2010")
grid.arrange(t1, t2, t3, t4, ncol = 2)

#Facet the daily averages by month for each year
ggplot(year1tidy) +
  geom_line(aes(x=Day, y=Watt_Hour_AP, color =SubMeter)) +
  facet_wrap(~Month, scales = "free_x") +
  labs(y= "Average Daily Watt-Hour of Active Power", 
       title="2007 Monthly Sub Meter Data")

ggplot(year2tidy) +
  geom_line(aes(x=Day, y=Watt_Hour_AP, color =SubMeter)) +
  facet_wrap(~Month, scales = "free_x") +
  labs(title="2008 Monthly Sub Meter Data")

ggplot(year3tidy) +
  geom_line(aes(x=Day, y=Watt_Hour_AP, color =SubMeter)) +
  facet_wrap(~Month, scales = "free_x") +
  labs(title="2009 Monthly Sub Meter Data")

ggplot(year4tidy) +
  geom_line(aes(x=Day, y=Watt_Hour_AP, color =SubMeter)) +
  facet_wrap(~Month, scales = "free_x") +
  labs(title="2010 Monthly Sub Meter Data")

#create a monthly average for each submeter by year
year1Month <- year1tidy %>%
  group_by(Month, SubMeter) %>%
  summarize(Month_Avg = mean(Watt_Hour_AP, na.rm = TRUE))

year2Month <- year2tidy %>%
  group_by(Month, SubMeter) %>%
  summarize(Month_Avg = mean(Watt_Hour_AP, na.rm = TRUE))

year3Month <- year3tidy %>%
  group_by(Month, SubMeter) %>%
  summarize(Month_Avg = mean(Watt_Hour_AP, na.rm = TRUE))

year4Month <- year4tidy %>%
  group_by(Month, SubMeter) %>%
  summarize(Month_Avg = mean(Watt_Hour_AP, na.rm = TRUE))

#plot the monthly averages
m1 <- ggplot(year1Month) +
  geom_line(aes(x=Month, y=Month_Avg, color=SubMeter)) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  labs(y= "Avg. Watt-Hour", 
       title="2007")
m2 <- ggplot(year2Month) +
  geom_line(aes(x=Month, y=Month_Avg, color=SubMeter)) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  labs(y= "Avg. Watt-Hour", 
       title="2008")
m3 <- ggplot(year3Month) +
  geom_line(aes(x=Month, y=Month_Avg, color=SubMeter)) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  labs(y= "Avg. Watt-Hour", 
       title="2009")
m4 <- ggplot(year4Month) +
  geom_line(aes(x=Month, y=Month_Avg, color=SubMeter)) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  labs(y= "Avg. Watt-Hour", 
       title="2010")
grid.arrange(m1, m2, m3, m4, ncol = 1)

#bind all years back together for a full tidy dataset
power2Tidy <- bind_rows(year1tidy, year2tidy, year3tidy, year4tidy)
power2Tidy$Year <- year(power2Tidy$Day)
power2Tidy$DayofMonth <- day(power2Tidy$Day)

#compare a specific month (November) and submeter (S1) from each of the four years
Novembers <- filter(power2Tidy, power2Tidy$Month == 11)
NovemberS1 <-filter(Novembers, Novembers$SubMeter == "S1_Kitchen")
NovemberS1$Year <- as.factor(NovemberS1$Year) #need to make year a factor in order
#to use it as a different color on the graph

#plot November 2007 data by year
NovS12007 <- filter(NovemberS1, Year == "2007")
ggplot(NovS12007) +
  geom_line(aes(x=DayofMonth, y=Watt_Hour_AP, color=Year)) +
  scale_x_continuous(breaks = seq(1,30,1)) +
  labs(title= "November Data for Sub Meter 1(Kitchen)", 
       x="Day of the Month",
       y="Average Daily Watt-Hour Active Power")

#plot November S1 data and facet by year
ggplot(NovemberS1) +
  geom_line(aes(x=DayofMonth, y=Watt_Hour_AP), color = "indianred") +
  facet_rep_wrap(~Year, repeat.tick.labels = TRUE) +
  scale_x_continuous(breaks = seq(1,30,1)) +
  labs(title= "November Data for Sub Meter 1(Kitchen)", 
       x="Day of the Month",
       y="Average Daily Watt-Hour Active Power")

#plot Sub 3 data by year
ggplot(data = filter(power2Tidy, SubMeter == "S3_WH_AC")) +
  geom_line(aes(x=Day, y=Watt_Hour_AP, color = Year)) +
  labs(title= "Sub Meter 3(WH & AC)", x="Month",
       y="Average Daily Watt-Hour Active Power")

power2Tidy$Month <- as.factor(power2Tidy$Month)
power2Tidy$Year <- as.factor(power2Tidy$Year)
power2Tidy$SubMeter <- as.factor(power2Tidy$SubMeter)
saveRDS(power2Tidy, file = "power2Tidy.rds")


#create day of the week averages
power2$dayofWeek <- wday(power2$DateTime, label = TRUE)
dayAvgs <- power2 %>%
  group_by(dayofWeek) %>%
  summarise(S1avg = mean(S1_Kitchen, na.rm = TRUE), 
            S2avg = mean(S2_Laundry, na.rm = TRUE),
            S3avg = mean(S3_WH_AC, na.rm = TRUE),
            S4avg = mean(S4_Rest, na.rm = TRUE))

s1 <- ggplot(dayAvgs, aes(x=dayofWeek, y=S1avg)) +
  geom_col(fill ="indianred") +
  labs(x= "Day of the Week", y="Average Watt-Hour Used", 
       title="Sub-meter 1: Kitchen")

s2 <- ggplot(dayAvgs, aes(x=dayofWeek, y=S2avg)) +
  geom_col(fill ="palegreen3") +
  labs(x= "Day of the Week", y="Average Watt-Hour Used", 
       title="Sub-meter 2: Laundry Room")

s3 <- ggplot(dayAvgs, aes(x=dayofWeek, y=S3avg)) +
  geom_col(fill ="turquoise3") +
  labs(x= "Day of the Week", y="Average Watt-Hour Used", 
       title="Sub-meter 3: Water Heater & A/C")

s4 <- ggplot(dayAvgs, aes(x=dayofWeek, y=S4avg)) +
  geom_col(fill ="purple1") +
  labs(x= "Day of the Week", y="Average Watt-Hour Used", 
       title="Sub-meter 4: Rest of House")

grid.arrange(s1, s2, s3, s4, ncol=2)

#try the tidy way - plot each day with submeter averages
dayAvgsTidy <- gather(dayAvgs, SubMeter, AvgWH, S1avg:S4avg)
ggplot(dayAvgsTidy, aes(x=SubMeter, y=AvgWH)) +
  geom_col(aes(fill=SubMeter)) +
  facet_rep_wrap(~dayofWeek,repeat.tick.labels = TRUE)


#compare a specific month (December) and submeter (S1) from each of the four years
December <- filter(power2Tidy, power2Tidy$Month == 12)
DecS1 <-filter(December, December$SubMeter == "S1_Kitchen")
DecS1$Year <- as.factor(DecS1$Year) #need to make year a factor in order
#to use it as a different color on the graph

#plot November S1 data and facet by year
ggplot(DecS1) +
  geom_line(aes(x=DayofMonth, y=Watt_Hour_AP), color = "indianred") +
  facet_rep_wrap(~Year, repeat.tick.labels = TRUE) +
  scale_x_continuous(breaks = seq(1,31,1)) +
  labs(title= "December Data for Sub Meter 1(Kitchen)", 
       x="Day of the Month",
       y="Average Daily Watt-Hour Active Power")

#plot Sub 3 data by year
ggplot(data = filter(power2Tidy, SubMeter == "S3_WH_AC")) +
  geom_line(aes(x=Day, y=Watt_Hour_AP, color = Year)) +
  labs(title= "Sub Meter 3(WH & AC)", x="Month",
       y="Average Daily Watt-Hour Active Power")

power2Tidy$Month <- as.factor(power2Tidy$Month)
power2Tidy$Year <- as.factor(power2Tidy$Year)
power2Tidy$SubMeter <- as.factor(power2Tidy$SubMeter)
saveRDS(power2Tidy, file = "power2Tidy.rds")

#plot hour of day and day of week
power2$hour <- hour(power2$DateTime)
hourAvgs <- power2 %>%
  group_by(hour, day) %>%
  summarise(S1_Kitchen = mean(S1_Kitchen, na.rm = TRUE), 
            S2_Laundry = mean(S2_Laundry, na.rm = TRUE),
            S3_WH_AC = mean(S3_WH_AC, na.rm = TRUE),
            S4_Rest = mean(S4_Rest, na.rm = TRUE))
hourAvgsTidy <- gather(hourAvgs, SubMeter, AvgWH, S1_Kitchen:S4_Rest)
ggplot(hourAvgsTidy, aes(x=hour, y=AvgWH)) +
  geom_line(aes(color=SubMeter)) +
  facet_rep_wrap(~day, repeat.tick.labels = TRUE, scales = "free_y") +
  scale_x_continuous(breaks = seq(0,24, 2)) +
  labs(x="Hour of Day", y="Average Watt-Hour", 
       title="Daily Sub-meter Readings by Time of Day")

ggplot(hourAvgsTidy, aes(x=hour, y=AvgWH)) +
  geom_area(aes(fill=SubMeter)) +
  facet_rep_wrap(~day, repeat.tick.labels = TRUE, scales = "free_y") +
  scale_x_continuous(breaks = seq(0,24, 2)) +
  labs(x="Hour of Day", y="Average Watt-Hour", 
       title="Daily Sub-meter Readings by Time of Day")

#create tidy version of power2 data
powTidy <- gather(power2, SubMeter, WattHour, S1_Kitchen:S4_Rest)

saveRDS(powTidy, file="powTidy.rds")

#create separate files for each year for GapMinder software
year1Month$Month <- month(year1Month$Month, label = TRUE)
gap1 <- spread(year1Month, Month, Month_Avg)
write.csv(gap1, file = "gap1.csv")

#create average per hour against total usage
power2$hour <- hour(power2$DateTime)
power2$year <- year(power2$DateTime)
power2$month <- month(power2$DateTime, label = TRUE)
power2$day <- wday(power2$DateTime, label = TRUE)
power2$date <- date(power2$DateTime)

hourAvg <- group_by(power2, hour, month) %>%
  summarize(Kitchen_Avg = mean(S1_Kitchen, na.rm = TRUE),
            Laundry_Avg = mean(S2_Laundry, na.rm = TRUE),
            WH_AC_Avg = mean(S3_WH_AC, na.rm = TRUE),
            Rest_Avg = mean(S4_Rest, na.rm = TRUE)) %>%
  mutate(total = Kitchen_Avg + Laundry_Avg + 
                     WH_AC_Avg + Rest_Avg)

#tidy it
hourAvgT <- gather(hourAvg, SubMeter, AvgWH, Kitchen_Avg:Rest_Avg)
hourAvgT$SubMeter <- as.factor(hourAvgT$SubMeter)

#by submeter, time of day, and month
ggplot(hourAvgT, aes(x=hour, y=AvgWH)) +
  geom_area(aes(fill = factor(SubMeter, levels=c("Kitchen_Avg", "Laundry_Avg",
                                                 "WH_AC_Avg", "Rest_Avg")))) +
  scale_x_continuous(breaks = seq(0,24, 3)) +
  labs(x="Time of Day", y="Average Watt-Hour Consumed",
       title= "Electric Consumption by Time of Day",
       fill = "Sub-meter") +
  facet_rep_wrap(~month, repeat.tick.labels = TRUE)


#creating pie chart of usage
ggplot(SubAvg2, aes(x=factor(1), y=Avg_WH_Active_Energy, fill=Sub_Meter))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  labs(x=NULL, y=NULL, title = "Distribution of Total Energy Use",
       fill = "Sub-meter") +
  geom_text(aes(label = percent(percent))) +
  theme(axis.text.x=element_blank()) 

SubAvg2 <- mutate(SubAvg2, percent = (Avg_WH_Active_Energy/sum(Avg_WH_Active_Energy)))

#plotting GAP vs GRP (bivariate)
summary(power2$GAP)
summary(power2$GRP)

#subset one day
Dec18 <- filter(power2, date == "2006-12-18")
ggplot(Dec18, aes(x=GAP, y=GRP)) +
  geom_point() +
  geom_smooth()

#scaling GRP to match submeters
power2 <- mutate(power2, GRPwh = GRP*1000/60)
#does GRP (power lost) correlate to usage of any of the submeters?
Jan07 <- power2 %>%
  filter(year == "2007" & month == "Jan") %>%
  group_by(hour) %>%
  summarize(Kitchen = mean(S1_Kitchen, na.rm = TRUE),
            Laundry = mean(S2_Laundry, na.rm = TRUE),
            WH_AC = mean(S3_WH_AC, na.rm = TRUE),
            RestofHouse = mean(S4_Rest, na.rm = TRUE),
            PowerLost = mean(GRPwh, na.rm = TRUE))

#test for correlation in Jan data and overall data
cor.test(Jan07$Kitchen, Jan07$PowerLost) #.431
cor.test(Jan07$Laundry, Jan07$PowerLost) #.819
cor.test(Jan07$WH_AC, Jan07$PowerLost) #.646
cor.test(Jan07$RestofHouse, Jan07$PowerLost) #.505

cor.test(power2$S1_Kitchen, power2$GRPwh) #.123
cor.test(power2$S2_Laundry, power2$GRPwh) #.139
cor.test(power2$S3_WH_AC, power2$GRPwh) #.089
cor.test(power2$S4_Rest, power2$GRPwh) #.211
cor.test(power2$Total, power2$GRPwh) #.247

#plot laundry room and power lost
ggplot(Jan07) +
  geom_line(aes(x=hour, y=PowerLost, color = "Power Lost")) +
  geom_line(aes(x=hour, y=Kitchen, color = "Laundry"))

#filter out the outliers of power lost
maxloss <- power2 %>%
  filter(GRPwh > 10) %>%
  group_by(date) %>%
  summarize(Kitchen = mean(S1_Kitchen, na.rm = TRUE),
            Laundry = mean(S2_Laundry, na.rm = TRUE),
            WH_AC = mean(S3_WH_AC, na.rm = TRUE),
            Rest = mean(S4_Rest, na.rm = TRUE),
            PowerLoss = mean(GRPwh, na.rm = TRUE)) %>%
  gather(Measure, WattHour, Kitchen:Rest) %>%
  mutate(year = year(date))

#show only 2007 and laundry data
maxloss$Measure <- as.factor(maxloss$Measure)

maxloss %>%
  filter(year == 2007) %>%
  ggplot(aes(x=date, y=WattHour, fill= Measure)) +
  geom_area() +
  geom_line(aes(x=date, y=PowerLoss), color="black") +
  labs(x="Date", y="Average Watt Hour of Energy", 
       title="Sub-Meter Consumption vs. Power Lost",
       subtitle = "On Days When Average Power Lost is Above 10wh",
       color = "Energy Measure")


ggplot(Dec18) +
  geom_line(aes(x=DateTime, y=S1_Kitchen, color = "Kitchen")) +
  geom_line(aes(x=DateTime, y=S2_Laundry, color = "Laundry")) +
  geom_line(aes(x=DateTime, y=S3_WH_AC, color = "WH/AC")) +
  geom_line(aes(x=DateTime, y=S4_Rest, color = "Rest of House")) +
  geom_line(aes(x=DateTime, y=GRPwh, color = "Power Lost"))

#isolate the laundry room
ggplot(Dec18) +
  geom_line(aes(x=DateTime, y=S2_Laundry, color = "Laundry"))

#isolate kitchen
ggplot(Dec18) +
  geom_line(aes(x=DateTime, y=S1_Kitchen, color = "Kitchen"))

#isolate WH/AC
ggplot(Dec18) +
  geom_line(aes(x=DateTime, y=S3_WH_AC, color = "WH/AC")) +
  geom_line(aes(x=DateTime, y=GRPwh))
