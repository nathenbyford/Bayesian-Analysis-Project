## STA 4360 Project Part 3

## Linear Regression model
# American Airlines(AA) high data flights market price out of DFW over time.

#Load Packages
library(tidyverse)
library(runjags)
theme_set(theme_bw())

# Load Data
dat <- read_csv('./Data/STA4360_data.csv')
dat <- dat %>% rename(year = Year)
AA_high_DFW <- dat %>% filter(car == "AA", mkt_fare > 150, city1 == "Dallas/Fort Worth, TX")

# setup data for model
AA_mdat <- AA_high_DFW %>% select(mkt_fare, year, city2)

# look at data 
ggplot(data = AA_mdat, aes(x = year, y = mkt_fare)) + 
  geom_point(aes(color = city2))

# Standard Linear Model

model <- lm(mkt_fare ~ year, data = AA_mdat)
summary(model)

#Seems to work pretty well

#visualize line on data
ggplot(data = AA_mdat, aes(x = year, y = mkt_fare)) + 
  geom_point(aes(color = city2)) +
  geom_abline(slope = 2.272, intercept = -4302.805)

# Jags model

modelString <- "
model {
  ## likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta0 + beta1*x_1[i]
  }
  beta0 ~ dnorm(0, 0.01)
  beta1 ~ dnorm(0, 0.01)
  sig ~ dunif(0, 100)
  tau <- 1 / sig^2
}
"

the_data <- with(AA_mdat, list('y' = mkt_fare, 'x_1' = year, 'N' = length(mkt_fare)))

posterior <- run.jags(modelString,
                      n.chains = 2,
                      data = the_data,
                      monitor = c('beta0', 'beta1'),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 10000)

summary(posterior)

# visualize using the mean of slope and intercept

ggplot(data = AA_mdat, aes(x = year, y = mkt_fare)) + 
  geom_point(aes(color = city2)) +
  geom_abline(slope = 0.128, intercept = -1.931)
