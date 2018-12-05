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

setwd("~/Ubiqum/Project 4/Task 1/Original Data Set")
power2 <- readRDS("~/Ubiqum/Project 4/Task 1/Original Data Set/power2.rds")
power2Tidy <- readRDS("~/Ubiqum/Project 4/Task 1/Original Data Set/power2Tidy.rds")

powerts <- ts(power2, frequency = 525600, start = 503604)
autoplot(powerts[,"S1_Kitchen"])
