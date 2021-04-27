## STA 4360 project part 2 Multiple ANOVA




# load packages
library(tidyverse); theme_set(theme_bw())
library("parallel"); options(mc.cores = detectCores())

# Multiple Sample Test ----------------------------------------------------- 
# Multiple sample test for Delta airlines(DL), Southwest airlines(WN), 
# American Airlines(AA), Continental Airlines(CO), and T'way Air(TW) from DFW

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

AA_high_DFW <- dat %>% 
  filter(
    car == "AA", 
    mkt_fare > 150, 
    city1 == "Dallas/Fort Worth, TX"
    )

TW_high_DFW <- dat %>% 
  filter(
    car == "TW", 
    mkt_fare > 150, 
    city1 == "Dallas/Fort Worth, TX"
    )

CO_high_DFW <- dat %>% 
  filter(
    car == "CO", 
    mkt_fare > 150, 
    city1 == "Dallas/Fort Worth, TX"
    )


WN_vec <- WN_high_DFW %>% pull(mkt_fare)
DL_vec <- DL_high_DFW %>% pull(mkt_fare)
AA_vec <- AA_high_DFW %>% pull(mkt_fare)
TW_vec <- TW_high_DFW %>% pull(mkt_fare)
CO_vec <- CO_high_DFW %>% pull(mkt_fare)


all_dat <- bind_rows(WN_high_DFW, DL_high_DFW, AA_high_DFW, TW_high_DFW, CO_high_DFW)

#summary
summary(WN_vec)
length(WN_vec)

summary(DL_vec)
length(DL_vec)

summary(AA_vec)
length(AA_vec)

summary(TW_vec)
length(TW_vec)

summary(CO_vec)
length(CO_vec)


# make a boxplot
ggplot(all_dat, aes(mkt_fare, car)) +
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



# Model for WN ------------------------------------------------------

# set prior info
mu_WN <- 5; n_WN <- 0.1; a_WN <- .1; b_WN <- .1

# compute basic statistics
n <- length(WN_vec)
WN_bar <- mean(WN_vec)
s <- sd(WN_vec)

# form t interval
WN_bar - qt(.975, n - 1)*s/n^.5
WN_bar + qt(.975, n - 1)*s/n^.5

# posterior mean
mu_post <- n_WN/(n_WN + n)*mu_WN + n/(n_WN + n)*WN_bar
BSSE <- (n-1)*s^2 + n*n_WN/(n_WN + n)*(mu_WN - WN_bar)^2

(n + a_WN)/2/((BSSE + b_WN)/2)

# posterior variance

(n + a_WN)/2/((BSSE + b_WN)/2)^2

# 95% credible interval for precision
qgamma(.025, (n + a_WN)/2, (BSSE + b_WN)/2)
qgamma(.975, (n + a_WN)/2, (BSSE + b_WN)/2)


# 95% credible interval for mu
mu_post - qt(.975, n + a_WN)*(((BSSE + b_WN)/(n + a_WN))/(n + a_WN))^.5
mu_post + qt(.975, n + a_WN)*(((BSSE + b_WN)/(n + a_WN))/(n + a_WN))^.5



#Model for DL ------------------------------------------------------

# set prior info
mu_DL <- 5; n_DL <- 0.1; a_DL <- .1; b_DL <- .1

# compute basic stats
m <- length(DL_vec)
DL_bar <- mean(DL_vec)
s_1 <- sd(DL_vec)
DL_bar - qt(.975, m - 1)*s_1/m^.5
DL_bar + qt(.975, m - 1)*s_1/m^.5

# posterior mean
mu_DL_post <- n_DL/(n_DL + m)*mu_DL + m/(n_DL + m)*DL_bar
BSSE <- (m-1)*s_1^2 + m*n_DL/(n_DL + m)*(mu_DL - DL_bar)^2

(m + a_DL)/2/((BSSE + b_DL)/2)

# posterior variance

(m + a_DL)/2/((BSSE + b_DL)/2)^2

# 95% credible interval for precision
qgamma(.025, (m + a_DL)/2, (BSSE + b_DL)/2)
qgamma(.975, (m + a_DL)/2, (BSSE + b_DL)/2)


# 95% credible interval for mu
mu_DL_post - qt(.975, m + a_DL)*(((BSSE + b_DL)/(m + a_DL))/(m + n_DL))^.5
mu_DL_post + qt(.975, m + a_DL)*(((BSSE + b_DL)/(m + a_DL))/(m + n_DL))^.5



# Model for AA ------------------------------------------------------

# set prior info
mu_AA <- 5; n_AA <- 0.1; a_AA <- .1; b_AA <- .1

# comute basic stats
o <- length(AA_vec)
AA_bar <- mean(AA_vec)
s_AA <- sd(AA_vec)

# form t interval
AA_bar - qt(.975, o - 1)*s_AA/o^.5
AA_bar + qt(.975, o - 1)*s_AA/o^.5

# posterior mean
mu_AA_post <- n_AA/(n_AA + o)*mu_AA + o/(n_AA + o)*AA_bar
BSSE <- (o-1)*s_AA^2 + o*n_AA/(n_AA + o)*(mu_AA - AA_bar)^2
(o + a_AA)/2/((BSSE + b_AA)/2)

# posterior variance
(o + a_AA)/2/((BSSE + b_AA)/2)^2

# 95% credible interval for precision
qgamma(.025, (o + a_AA)/2, (BSSE + b_AA)/2)
qgamma(.975, (o + a_AA)/2, (BSSE + b_AA)/2)


# 95% credible interval for mu 
mu_AA_post - qt(.975, o + a_AA)*(((BSSE + b_AA)/(o + a_AA))/(o + n_AA))^.5
mu_AA_post + qt(.975, o + a_AA)*(((BSSE + b_AA)/(o + a_AA))/(o + n_AA))^.5



#Model for TW ------------------------------------------------------

# set prior information
mu_TW <- 5; n_TW <- 0.1; a_TW <- .1; b_TW <- .1

# compute basic stats
p <- length(TW_vec)
TW_bar <- mean(TW_vec)
s_TW <- sd(TW_vec)
TW_bar - qt(.975, p - 1)*s_TW/p^.5
TW_bar + qt(.975, p - 1)*s_TW/p^.5

# posterior mean
mu_TW_post <- n_TW/(n_TW + p)*mu_TW + p/(n_TW + p)*TW_bar
BSSE <- (p-1)*s_TW^2 + p*n_TW/(n_TW + p)*(mu_TW - TW_bar)^2
(p + a_TW)/2/((BSSE + b_TW)/2)

# posterior variance
(p + a_TW)/2/((BSSE + b_TW)/2)^2

# 95% credible interval for precision
qgamma(.025, (p + a_TW)/2, (BSSE + b_TW)/2)
qgamma(.975, (p + a_TW)/2, (BSSE + b_TW)/2)

# 95% credible interval for mu 
mu_TW_post - qt(.975, p + a_TW)*(((BSSE + b_TW)/(p + a_TW))/(p + n_TW))^.5
mu_TW_post + qt(.975, p + a_TW)*(((BSSE + b_TW)/(p + a_TW))/(p + n_TW))^.5



#Model for CO ------------------------------------------------------

# set prior information
mu_CO <- 5; n_CO <- 0.1; a_CO <- .1; b_CO <- .1

# compute basic stats
q <- length(CO_vec)
CO_bar <- mean(CO_vec)
s_CO <- sd(CO_vec)
CO_bar - qt(.975, q - 1)*s_CO/p^.5
CO_bar + qt(.975, q - 1)*s_CO/p^.5

# posterior mean
mu_CO_post <- n_CO/(n_CO + p)*mu_CO + p/(n_CO + p)*CO_bar
BSSE <- (p-1)*s_CO^2 + p*n_CO/(n_CO + p)*(mu_CO - CO_bar)^2

(q + a_CO)/2/((BSSE + b_CO)/2)

# posterior variance
(q + a_CO)/2/((BSSE + b_CO)/2)^2

# 95% credible interval for precision
qgamma(.025, (q + a_CO)/2, (BSSE + b_CO)/2)
qgamma(.975, (q + a_CO)/2, (BSSE + b_CO)/2)

# 95% credible interval for mu 
mu_CO_post - qt(.975, q + a_CO)*(((BSSE + b_CO)/(q + a_CO))/(q+ n_CO))^.5
mu_CO_post + qt(.975, q + a_CO)*(((BSSE + b_CO)/(q + a_CO))/(q + n_CO))^.5


#classic anova ------------------------------------------------------

library("multcomp")

aov_mod <- aov(mkt_fare~car, data = all_dat)
summary(aov_mod)

post_test <- glht(aov_mod, linfct = mcp(species = "Tukey"))

TukeyHSD(aov_mod)

plot(TukeyHSD(aov_mod))



# Bayesian anova ------------------------------------------------------

library(rjags)

flight_jags_1 <- "
model {

  # Likelihood
  for (i in 1:N) {                   
    mu[i]<- beta[ind[i]]              
    y[i] ~ dnorm(mu[i],tau)         
  }

  # Uninformative priors
  for (j in 1:p) {
    beta[j] ~ dnorm(0,0.0001)

    # Calculate difference from oveall mean
    Effect[j] <- beta[j] - overall_mean  

    # Calculate pair wise differences 
    for (n in 1:(j-1)){
      diffbet[n,j] <- beta[n] - beta[j]
    }
  }

  tau ~ dgamma(scale, rate)   # Prior for normal distribution precision.
  scale ~ dunif(0, 1)         # Hyper parameters for tau.
  rate ~ dunif(0, 1)

}
"

# Organize data
flight_dat <- with(all_dat, list(
  "y" = mkt_fare,
  "ind" = as.integer(as.factor(car)),
  "N" = length(mkt_fare),
  "p" = length(unique(car)),
  "overall_mean" = mean(mkt_fare)
))
 
model <- jags.model(
  textConnection(flight_jags_1), 
  data = flight_dat, 
  n.chains = getOption("mc.cores"),
  quiet = TRUE
)


output <- coda.samples(
  model = model, 
  variable.names = c("Beta","diffbeta","Effect"), 
  n.iter = 1e4
  )

summary(output, quantiles = c(.025, .975))

# bayesplot plot ---------------------------------------------------------

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


# ggmcmc plots
library(ggmcmc)
ms <- ggs(output) 
mt <- filter(ms,grepl("diffbeta", Parameter))
ggs_caterpillar(mt) + geom_vline(xintercept = 0, col = "red")

mt<-filter(ms,grepl("Beta", Parameter))
ggplot(mt, aes(x = value, col = Parameter, fill = Parameter)) + 
  geom_density( alpha = 0.2)
  
