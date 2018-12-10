#look at the water heater/AC closely to find patterns 
#in the usage by appliance. If we subset out the same week in November
#for all four years, we can see how the WH is performing based on its 
#past performance. Can we then forecast how much energy it will consume
#in the future? (is there a trend?)

WHnov07.1 <- power2 %>%
  filter(year == "2007" & month== "Nov" & week==45) 

WHnov08.1 <- power2 %>%
  filter(year == "2008" & month== "Nov" & week==45) 

WHnov09.1 <- power2 %>%
  filter(year == "2009" & month== "Nov" & week==45) 

WHnov10.1 <- power2 %>%
  filter(year == "2010" & month== "Nov" & week==45) 


WH1 <- ggplot(WHnov07.1, aes(x=DateTime, y=S3_WH_AC)) +
  geom_line()

WH2 <- ggplot(WHnov08.1, aes(x=DateTime, y=S3_WH_AC)) +
  geom_line()

WH3 <- ggplot(WHnov09.1, aes(x=DateTime, y=S3_WH_AC)) +
  geom_line()

WH4 <- ggplot(WHnov10.1, aes(x=DateTime, y=S3_WH_AC)) +
  geom_line()

grid.arrange(WH1, WH2, WH3, WH4, ncol = 1)

#create and plot time series
pDay <- power2 %>%
  group_by(day=floor_date(DateTime, "day")) %>%
  summarize_at(vars(S1_Kitchen:S4_Rest), sum, na.rm = TRUE) %>%
  mutate(total = S1_Kitchen + S2_Laundry + S3_WH_AC + S4_Rest)

pDayTS <- ts(pDay, frequency = 365, start = c(2006,350))
autoplot(pDayTS[,"S3_WH_AC"])
pDayWH <- tsclean(pDayTS[,"S3_WH_AC"])

WHDecomp <- decompose(pDayWH, type="multiplicative")
autoplot(WHDecomp) +
  ggtitle("Decomposed Time Series: Sub-meter 3 - Water Heater & A/C")

autoplot(pDayWH) +
  scale_x_continuous(limits = c(2007.75, 2008))

#TREND SHOWS THAT CONSUMPTION IS GOING UP FOR THE SUB3 -- COULD MEAN THAT 
#THE APPLIANCES IN THAT ROOM ARE BEING USED MORE OR THAT THOSE APPLIANCES
#ARE BECOMING LESS EFFICIENT


#look at kitchen
K1 <- ggplot(WHnov07.1, aes(x=DateTime, y=S1_Kitchen)) +
  geom_line()

K2 <- ggplot(WHnov08.1, aes(x=DateTime, y=S1_Kitchen)) +
  geom_line()

K3 <- ggplot(WHnov09.1, aes(x=DateTime, y=S1_Kitchen)) +
  geom_line()

K4 <- ggplot(WHnov10.1, aes(x=DateTime, y=S1_Kitchen)) +
  geom_line()

grid.arrange(K1, K2, K3, K4, ncol = 1)

pDayTS <- ts(pDay, frequency = 365, start = c(2006,350))
autoplot(pDayTS[,"S1_Kitchen"])
pDayKit <- tsclean(pDayTS[,"S1_Kitchen"])
autoplot(pDayKit)

KitDecomp <- decompose(pDayKit, type="multiplicative")
autoplot(KitDecomp)

#Kitchen usage is trending down...interesting

#let's look at the laundry room now

pDayTS <- ts(pDay, frequency = 365, start = c(2006,350))
autoplot(pDayTS[,"S2_Laundry"])
pDayLau <- tsclean(pDayTS[,"S2_Laundry"])
autoplot(pDayLau)

LaunDecomp <- decompose(pDayLau, type="multiplicative")
autoplot(LaunDecomp)

#laundry room trending down as well. Let's check rest of house
pDayTS <- ts(pDay, frequency = 365, start = c(2006,350))
autoplot(pDayTS[,"S4_Rest"])
pDayRest <- tsclean(pDayTS[,"S4_Rest"])
autoplot(pDayRest)

RestDecomp <- decompose(pDayRest, type="multiplicative")
autoplot(RestDecomp)

#rest of house also trending down. So something unsual is happening
# in the WH/AC room.