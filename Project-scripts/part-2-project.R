## STA 4360 project part 2





# load packages -----------------------------------------------------------

library("tidyverse"); theme_set(theme_bw())
library("parallel"); options(mc.cores = detectCores())





# two sample test ---------------------------------------------------------
# two sample test for Delta Airlines(DL) and Southwest Airlines(WN) from DFW


# load data
dat <- read.csv("./Data/STA4360_data.csv")

# get data to compare
WN_high_DFW <- dat %>% 
  filter(
    car == "WN", 
    mkt_fare > 150, 
    city1 == "Dallas/Fort Worth, TX"
  )

DL_high_DFW <- dat %>% 
  filter(
    car == "DL", 
    mkt_fare > 150, 
    city1 == "Dallas/Fort Worth, TX"
  )

WN_vec <- WN_high_DFW %>% pull(mkt_fare)
DL_vec <- DL_high_DFW %>% pull(mkt_fare)

both_dat <- bind_rows(WN_high_DFW, DL_high_DFW)

# summary
summary(WN_vec)
length(WN_vec)

summary(DL_vec)
length(DL_vec)


# make a boxplot
ggplot(both_dat, aes(mkt_fare, car)) +
  geom_boxplot(aes(fill = car)) +
  scale_y_discrete(
    breaks = c("DL", "WN"),
    labels = c("Delta", "Southwest")
  ) +
  labs(
    "x" = "Airlines",
    "title" = "Prices by Airline"
  ) +
  theme(
    axis.title.y = element_blank()
  )




# model for WN ------------------------------------------------------------

# set prior info
mu_0 <- 5; n_0 <- 0.1; a <- .1; b <- .1

# compute basic stats
n <- length(WN_vec)
x_bar <- mean(WN_vec)
s <- sd(WN_vec)

# form t interval
x_bar - qt(.975, n-1) * s/sqrt(n)
x_bar + qt(.975, n-1) * s/sqrt(n)

# posterior mean
mu_post <- n_0/(n_0 + n)*mu_0 + n/(n_0 + n)*x_bar
BSSE <- (n-1)*s^2 + n*n_0/(n_0 + n)*(mu_0 - x_bar)^2

(n + a)/2 / ((BSSE + b)/2)

# posterior variance

(n + a)/2/((BSSE + b)/2)^2


# 95% credible interval for precision
qgamma(.025, (n + a)/2, (BSSE + b)/2)
qgamma(.975, (n + a)/2, (BSSE + b)/2)


# 95% credible interval for mu
mu_post - qt(.975, n + a) * sqrt(((BSSE + b)/(n + a))/(n + n_0))
mu_post + qt(.975, n + a) * sqrt(((BSSE + b)/(n + a))/(n + n_0))





# model for DL ------------------------------------------------------------

# set prior info
mu_1 <- 5; n_1 <- 0.1; a_1 <- .1; b_1 <- .1

# compute basic stats
m <- length(DL_vec)
y_bar <- mean(DL_vec)
s_1 <- sd(DL_vec)

# form t interval
y_bar - qt(.975, n-1) * s/sqrt(n)
y_bar + qt(.975, n-1) * s/sqrt(n)

# posterior mean
mu_1_post <- n_1/(n_1 + m)*mu_1 + m/(n_1 + m)*y_bar
BSSE <- (m-1)*s_1^2 + m*n_1/(n_1 + m)*(mu_1 - y_bar)^2
(m + a_1)/2/((BSSE + b_1)/2)

# posterior variance
(m + a_1)/2/((BSSE + b_1)/2)^2

# 95% credible interval for precision
qgamma(.025, (m + a_1)/2, (BSSE + b_1)/2)
qgamma(.975, (m + a_1)/2, (BSSE + b_1)/2)

# 95% credible interval for mu
mu_1_post - qt(.975, m + a_1)*(((BSSE + b_1)/(m + a_1))/(m + n_1))^.5
mu_1_post + qt(.975, m + a_1)*(((BSSE + b_1)/(m + a_1))/(m + n_1))^.5




# classic anova -----------------------------------------------------------

library("multcomp")

aov_mod <- aov(mkt_fare~car, data = both_dat)
summary(aov_mod)

post_test <- glht(aov_mod, linfct = mcp(species = "Tukey"))

TukeyHSD(aov_mod)

plot(TukeyHSD(aov_mod))





# bayesian anova ----------------------------------------------------------

library("rjags")

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
  n.chains = getOption("mc.cores"),
  quiet = TRUE
)

# i removed thinning here, as chains looked fine
output <- coda.samples(
  model = model,
  variable.names = c("beta", "diffbeta", "effect"),
  n.iter = 1e4
)

summary(output, quantiles = c(.025, .975))



# bayesplot plots ---------------------------------------------------------

# examples of the kind of plots you can do
# (these may or may not be interesting, not commenting there)

library("bayesplot")

mcmc_dens(output)
mcmc_dens(output, regex_pars = "beta")
mcmc_dens(output, regex_pars = "^beta")
mcmc_dens(output, regex_pars = "^effect")

mcmc_hist(output, regex_pars = "^effect")

mcmc_acf_bar(output) # => thinning not needed

mcmc_intervals(output)

mcmc_combo(output, regex_pars = "^beta")
mcmc_combo(output, combo = c("hist", "trace"), regex_pars = "^beta")




# ggmcmc plots ------------------------------------------------------------
# i'd probably recommend against these,
# but i don't have a problem if you use them

library("ggmcmc")

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

