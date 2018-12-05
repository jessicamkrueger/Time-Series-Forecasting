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


power2 <- readRDS("~/Ubiqum/Project 4/Task 1/Original Data Set/power2.rds")
saveRDS(power2, file="power2.rds")

#Kitchen submeter trend over four 4 years (weekly average)
power2$week <- week(power2$DateTime)

S1week <- power2 %>%
  group_by(week, year) %>%
  summarize(Avg = mean(S1_Kitchen, na.rm = TRUE)) %>%
  mutate(week_year=paste(year, week, sep = "_"))

S2week <- power2 %>%
  group_by(week, year) %>%
  summarize(Avg = mean(S2_Laundry, na.rm = TRUE)) %>%
  mutate(week_year=paste(year, week, sep = "_"))

S3week <- power2 %>%
  group_by(week, year) %>%
  summarize(Avg = mean(S3_WH_AC, na.rm = TRUE)) %>%
  mutate(week_year=paste(year, week, sep = "_"))

ggplot(S1week, aes(x=week_year, y=Avg, group = 1)) +
  geom_line()+
  geom_smooth() +
  scale_x_discrete(breaks = (seq(1, 211, 20)))

ggplot(S2week, aes(x=week_year, y=Avg, group = 1)) +
  geom_line()+
  geom_smooth() +
  scale_x_discrete(breaks = (seq(1, 211, 20)))

ggplot(S3week, aes(x=week_year, y=Avg, group = 1)) +
  geom_line()+
  geom_smooth() +
  scale_x_discrete(breaks = (seq(1, 211, 20)))
                   