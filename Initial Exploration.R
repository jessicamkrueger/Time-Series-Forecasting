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
power2 <- readRDS("~/Ubiqum/Project 4/Task 1/Original Data Set/Power2.rds")
power2Tidy <- readRDS("~/Ubiqum/Project 4/Task 1/Original Data Set/power2Tidy.rds")

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

#Plot avg watt-hours used by sub meter
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
  year1tidy$Month <- month(year1tidy$Day)

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
