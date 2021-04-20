## STA 4360 project part 2

## Two sample test for American Airlines(AA) and Southwest Airlines(WN) from DFW

#load packages
library(tidyverse)

#load data
dat <- read_csv('./Data/STA4360_data.csv')
dat <- dat %>% rename(year = Year)

#get data to compare
WN_high_DFW <- dat %>% filter(car == "WN", mkt_fare > 150, city1 == "Dallas/Fort Worth, TX")
AA_high_DFW <- dat %>% filter(car == "AA", mkt_fare > 150, city1 == "Dallas/Fort Worth, TX")

