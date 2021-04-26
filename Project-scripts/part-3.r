## STA 4360 Project Part 3

## Linear Regression model
# American Airlines(AA) high data flights market price out of DFW over time.

#Load Packages
library(tidyverse)
library(runjags)
library(GGally)
theme_set(theme_bw())

# Load Data
dat <- read_csv('./Data/STA4360_data.csv')
dat <- dat %>% rename(year = Year)
AA_high_DFW <- dat %>% filter(car == "AA", mkt_fare > 150, city1 == "Dallas/Fort Worth, TX")

# setup data for model
AA_mdat <- AA_high_DFW %>% select(mkt_fare, year, city2)
AA_mdat$city2 <- as.factor(AA_mdat$city2)

# look at data 
ggplot(data = AA_mdat, aes(x = year, y = mkt_fare)) + 
  geom_point(aes(color = city2), alpha = .5) +
  labs(title = "Scatter Plot of Flights out of DFW on American Airlines", 
       color = "Destination", y = "Mean Market Fare (USD)")

# Standard Linear Model
model <- lm(mkt_fare ~ year, data = AA_mdat)
summary(model)

#Seems to work pretty well

#visualize line on data
ggplot(data = AA_mdat, aes(x = year, y = mkt_fare)) + 
  geom_point(aes(color = city2)) +
  geom_abline(slope = 2.272, intercept = -4302.805)

# Jags model
set.seed(1)

modelString <- "
model {
  ## likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta0 + beta1*x_1[i]
  }
  beta0 ~ dnorm(0, 0.00000001)
  beta1 ~ dnorm(0, 0.00001)
  sig ~ dunif(0, 100000)
  tau <- 1 / sig^2
  sig2 <- sig^2
}
"
## We use a large posterior
the_data <- with(AA_mdat, list('y' = mkt_fare, 'x_1' = year, 'N' = length(mkt_fare)))

posterior <- run.jags(modelString,
                      n.chains = 2,
                      data = the_data,
                      monitor = c('beta0', 'beta1', 'sig2'),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 50000)

summary(posterior)

# visualize using the mean of slope and intercept

ggplot(data = AA_mdat, aes(x = year, y = mkt_fare)) + 
  geom_point(alpha = .5) +
  geom_abline(slope = 2.272, intercept = -4302.805, 
              color = 'steelblue', size = 1.2, alpha = .8) +
  geom_abline(slope = 1.225, intercept = -2202.5665, 
              color = 'goldenrod2', size = 1.2, alpha = .8) +
  labs(title = "Simple Linear Regression fit by Approach", y = "Market Fare")

## Advanced linear model for Dummy Variables

#simple lm()

model_dat2 <- model.matrix(mkt_fare ~ year+city2, data = AA_mdat)

df <- AA_mdat %>% 
  mutate(
    city3 = city2 %>% 
      str_sub(1, -5) %>% 
      str_to_lower() %>% 
      str_remove_all("[.]") %>% 
      str_replace_all(" ", "_") %>%
      str_replace_all("/", "_")
  )

model_comp <- lm(mkt_fare ~ year + city3, data = df)
summary(model_comp)

X <- model.matrix(mkt_fare ~ year + city3 - 1, data = df)

predictor_list <- X %>% 
  as.data.frame() %>% as_tibble() %>% 
  rename_with(~ .x %>% str_remove("city3")) %>% 
  rename("x1" = "year", "x2" = "denver", "x3" = "des_moines", "x4" = "fayetteville",
         "x5" = "kansas_city", "x6" = "louisville", "x7" = "memphis", 
         "x8" = "mission_mcallen_edinburg", "x9" = "nashville", "x10" = "omaha", 
         "x11" = "pensacola", "x12" = "st_louis", "x13" = "valparaiso") %>%
  as.list()

data_list <- c(
  "n" = length(df$mkt_fare),
  "y" = list(df$mkt_fare),
  predictor_list
)

modelString2 <- "
model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- int + year * x1[i] + denver * x2[i] + des_moines * x3[i] +
        fayetteville * x4[i] + kansas_city * x5[i] + louisville * x6[i] + memphis * x7[i] +
        mission_mcallen_edinburg * x8[i] + nashville * x9[i] + omaha * x10[i] + pensacola * x11[i] +
        st_louis * x12[i] + valparaiso * x13[i]
  }
  int ~ dnorm(0, 0.000000001)
  year ~ dnorm(0, 0.00001)
  denver ~ dnorm(0, 0.00001)
  des_moines ~ dnorm(0, 0.00001)
  fayetteville ~ dnorm(0, 0.00001)
  kansas_city ~ dnorm(0, 0.00001)
  louisville ~ dnorm(0, 0.00001)
  memphis ~ dnorm(0, 0.00001)
  mission_mcallen_edinburg ~ dnorm(0, 0.00001)
  nashville ~ dnorm(0, 0.00001)
  omaha ~ dnorm(0, 0.00001)
  pensacola ~ dnorm(0, 0.00001)
  st_louis ~ dnorm(0, 0.00001)
  valparaiso ~ dnorm(0, 0.00001)
  sig ~ dunif(0, 10000)
  tau <- 1 / sig^2
  sig2 <- sig^2
}
"

posterior2 <- run.jags(modelString2,
                      n.chains = 2,
                      data = data_list,
                      monitor = c("int", "year", "denver", "des_moines", "fayetteville", "kansas_city", 
                                  "louisville", "memphis", "mission_mcallen_edinburg", 
                                  "nashville", "omaha", "pensacola", "st_louis", 
                                  "valparaiso", 'sig2'),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 50000)

summary(posterior2)
