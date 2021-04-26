## STA 4360 project part 2

## Two sample test for Delta Airlines(DL) and Southwest Airlines(WN) from DFW

#load packages
library(tidyverse)

#load data
dat <- read_csv('./Data/STA4360_data.csv')
dat <- dat %>% rename(year = Year)

#get data to compare
WN_high_DFW <- dat %>% filter(car == "WN", mkt_fare > 150, city1 == "Dallas/Fort Worth, TX")
DL_high_DFW <- dat %>% filter(car == "DL", mkt_fare > 150, city1 == "Dallas/Fort Worth, TX")

WN_prices <- WN_high_DFW

