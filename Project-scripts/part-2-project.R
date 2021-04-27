## STA 4360 project part 2

## Two sample test for Delta Airlines(DL) and Southwest Airlines(WN) from DFW

#load packages
library(tidyverse)

#load data
dat <- read.csv("./Data/STA4360_data.csv")

#get data to compare
WN_high_DFW <- dat %>% filter(car == "WN", mkt_fare > 150, city1 == "Dallas/Fort Worth, TX")
DL_high_DFW <- dat %>% filter(car == "DL", mkt_fare > 150, city1 == "Dallas/Fort Worth, TX")

WN_vec <- as.vector(WN_high_DFW$mkt_fare)
DL_vec <- as.vector(DL_high_DFW$mkt_fare)

both_dat <- rbind(WN_high_DFW, DL_high_DFW)

#summary
summary(WN_vec)
length(WN_vec)

summary(DL_vec)
length(DL_vec)

library(ggplot2)
theme_set(theme_bw())

g0 <- ggplot(both_dat,aes(x=car,y=mkt_fare))
g_box<- g0+ geom_boxplot()
g_boxDL_vec <- as.vector(WN_high_DFW$mkt_fare)



#Model for WN

#prior information
mu_0 <- 5; n_0 <- 0.1; a <- .1; b <- .1

n <- length(WN_vec)

x_bar <- mean(WN_vec)

s <- sd(WN_vec)

x_bar - qt(.975, n - 1)*s/n^.5

x_bar + qt(.975, n - 1)*s/n^.5

# posterior mean

mu_post <- n_0/(n_0 + n)*mu_0 + n/(n_0 + n)*x_bar

BSSE <- (n-1)*s^2 + n*n_0/(n_0 + n)*(mu_0 - x_bar)^2



(n + a)/2/((BSSE + b)/2)

# posterior variance

(n + a)/2/((BSSE + b)/2)^2

# 95% credible interval for precision

qgamma(.025, (n + a)/2, (BSSE + b)/2)

qgamma(.975, (n + a)/2, (BSSE + b)/2)


####### 95% interval for mu ##########


mu_post - qt(.975, n + a)*(((BSSE + b)/(n + a))/(n + n_0))^.5

mu_post + qt(.975, n + a)*(((BSSE + b)/(n + a))/(n + n_0))^.5





#Model for DL

#uninformative prior information
mu_1 <- 5; n_1 <- 0.1; a_1 <- .1; b_1 <- .1

m <- length(DL_vec)

y_bar <- mean(DL_vec)

s_1 <- sd(DL_vec)

y_bar - qt(.975, n - 1)*s/n^.5

y_bar + qt(.975, n - 1)*s/n^.5

# posterior mean

mu_1_post <- n_1/(n_1 + m)*mu_1 + m/(n_1 + m)*y_bar

BSSE <- (m-1)*s_1^2 + m*n_1/(n_1 + m)*(mu_1 - y_bar)^2



(m + a_1)/2/((BSSE + b_1)/2)

# posterior variance

(m + a_1)/2/((BSSE + b_1)/2)^2

# 95% credible interval for precision

qgamma(.025, (m + a_1)/2, (BSSE + b_1)/2)

qgamma(.975, (m + a_1)/2, (BSSE + b_1)/2)


####### 95% interval for mu ##########


mu_1_post - qt(.975, m + a_1)*(((BSSE + b_1)/(m + a_1))/(m + n_1))^.5

mu_1_post + qt(.975, m + a_1)*(((BSSE + b_1)/(m + a_1))/(m + n_1))^.5

#classic anova
library(multcomp)
aov_mod <- aov(mkt_fare~car, data = both_dat)
summary(aov_mod)
post_test <- glht(aov_mod, linfct = mcp(species = "Tukey"))

TukeyHSD(aov_mod)

plot(TukeyHSD(aov_mod))




# Bayesian ANOVA
library(rjags)
flight_jags_1 <- "
model {

  # likelihood
  for (i in 1:N) {
    mu[i] <- beta[ind[i]]
    y[i] ~ dnorm(mu[i], tau)
  }

  # uninformative priors
  for (j in 1:p) {
    beta[j] ~ dnorm(0, 0.001)
    
    # calculate difference from overall mean
    effect[j] <- beta[j] - overall_mean
    
    # calculate pair wise differences 
    for (n in 1:(j-1)){
      diffbeta[n,j] <- beta[n] - beta[j]
    }
  }

  tau ~ dgamma(scale, rate) # prior for normal precision
  scale ~ dunif(0, 1) # Hyper parameters for tau
  rate ~ dunif(0, 1)

}
"

# Organize data
flight_dat <- with(both_dat, list(
  "y" = mkt_fare,
  "ind" = as.integer(as.factor(car)),
  "N" = length(mkt_fare),
  "p" = 2, # fixed here, p was 0
  "overall_mean" = mean(mkt_fare)
))


model <- jags.model(
  textConnection(flight_jags_1),
  data = flight_dat,
  quiet = TRUE
)

output <- coda.samples(
  model = model,
  variable.names = c("beta", "diffbeta", "effect"),
  n.iter = 100000,
  thin = 10
)

summary(output)

#Plots
library(ggmcmc)
ms <- ggs(output) 
mt <- filter(ms, grepl("diffbeta", Parameter))
ggs_caterpillar(mt) + geom_vline(xintercept = 0, col = "red")

mt <- filter(ms, grepl("beta", Parameter))

ggs_density(mt)
ggs_histogram(mt)

ggplot(data = mt, aes(
  x = mt$value,
  col = Parameter,
  fill = Parameter
)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Posterior difference of Delta Airlines and Southwest Airlines out of Dallas ",
    x = "market Fare"
  )

