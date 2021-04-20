## STA 4360 Project part 1

## One sample bayesian estimate for AA airfare prices out of DFW.


#Load Packages
library(tidyverse)
library(runjags)
theme_set(theme_bw())

#Load data

dat <- read_csv('./Data/STA4360_data.csv')
dat <- dat %>% rename(year = Year)

#visualize data

dat %>% ggplot(aes(x = Year, y = mkt_fare)) +
  geom_point()
dat %>% ggplot(aes(x = Year, y = mkt_fare)) +
  geom_point() +
  geom_hline(yintercept = 150, color = 'red')

#seperate data into high and low because of what we see on our graph.
#We will focus on the high data.
dat_high <- dat %>% filter(mkt_fare > 150)
dat_low <- dat %>% filter(mkt_fare <= 150)

AA <- dat %>% filter(car == "AA")

## WE chose to focus on just one airline, we will be looking at American Airlines(AA)
AA_high <- AA %>% filter(mkt_fare > 150)
AA_low <- AA %>% filter(mkt_fare <= 150)

AA_high_DFW <- AA_high %>% filter(city1 == "Dallas/Fort Worth, TX")

## One Sample Test

# assume weak prior information

mu_0 <- 250; n_0 <- 5; a <- 1; b <- 1

n <- length(AA_high_DFW$mkt_fare)

x_bar <- mean(AA_high_DFW$mkt_fare)

s <- sd(AA_high_DFW$mkt_fare)

# Lower 95th percentile
x_bar - qt(.975, n-1)*s/n^.5

# Upper 95th percentile
x_bar + qt(.975, n-1)*s/n^.5

mu_post <- n_0/(n_0 + n)*mu_0 + n/(n_0 + n)*x_bar

BSSE <- (n-1)*s^2 + n*n_0/(n_0 + n)*(mu_0 - x_bar)^2

(n + a)/2/((BSSE + b)/2)

#posterior variance

(n + a)/2/((BSSE + b)/2)^2

########## 95% Interval for mu ##########

mu_post - qt(.975, n + a)*(((BSSE + b)/(n + a))/(n + n_0))^.5

mu_post + qt(.975, n + a)*(((BSSE + b)/(n + a))/(n + n_0))^.5



