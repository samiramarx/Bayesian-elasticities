#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# BAYES ESTIMATION - MKT 4
#--------------------------------------
# library
#--------------------------------------
library(tidyverse)
library(data.table)
library(broom)
library(rstan)
library(rstanarm)
#--------------------------------------
load("dta_spread_mkt4.RData")
#--------------------------------------

# General elements for estimation:
chains <- 3
iter <- 2000
cores<- 3

# Specify the data list that we will pass to Stan. This gives Stan everything declared in the data{} block. 
k <- 9 # intercept, f1F, f1R, f1A, f2F, f2R, f2A, g, gamma
n<- 1738
X <- cbind(c = rep(1, lenght.out = dta$rail_year),
           log(dta$avgf_1f),
           log(dta$avgf_1r),
           log(dta$avgf_1a),
           log(dta$avgf_2f),
           log(dta$avgf_2r),
           log(dta$avgf_2a),
           log(dta$avg_gva),
           log(dta$gjt_f))
#--------------------------------------
# V1F
y_v1f <- log(dta$jny_1f)
mkt4_v1f <- stan(file = "scripts/stan_models/stan_model_mkt4_v1f_T.stan", 
                 data = list(X = X, N = n, K = k, 
                             y = y_v1f), 
                 cores = cores, 
                 chains = chains, 
                 iter = 10000)
save(mkt4_v1f, file = "bayes_simulation_mkt4_v1f_T_SCOTYORK.RData")

# V1R
y_v1r <- log(dta$jny_1r)
init_valuesv1r = list(list(beta=c(0.54, 0.27, -1.32, 0.42, 0.54, 0.62, 1.26, 0.39, -1.25), sigma=1.77),
                      list(beta=c(0.54, 0.27, -1.32, 0.42, 0.54, 0.62, 1.26, 0.39, -1.25), sigma=1.77),
                      list(beta=c(0.54, 0.27, -1.32, 0.42, 0.54, 0.62, 1.26, 0.39, -1.25), sigma=1.77))
mkt4_v1r <- stan(file = "scripts/stan_models/stan_model_mkt4_v1r_T.stan", 
                 data = list(X = X, N = n, K = k, 
                             y = y_v1r), 
                 init= init_valuesv1r,
                 # thin=5,
                 cores = cores, 
                 chains = chains, 
                 iter = 10000)
save(mkt4_v1r, file = "bayes_simulation_mkt4_v1r_T_SCOTYORK.RData")

# V1A
y_v1a <- log(dta$jny_1a)
init_valuesv1a = list(list(beta=c(-0.81, 1.07, 0.02, -0.09, 0.75, 0.41, 0.83, 0.69, -2.18), sigma=1.81),
                      list(beta=c(-0.81, 1.07, 0.02, -0.09, 0.75, 0.41, 0.83, 0.69, -2.18), sigma=1.81),
                      list(beta=c(-0.81, 1.07, 0.02, -0.09, 0.75, 0.41, 0.83, 0.69, -2.18), sigma=1.81))
mkt4_v1a <- stan(file = "scripts/stan_models/stan_model_mkt4_v1a_T.stan", 
                 data = list(X = X, N = n, K = k, 
                             y = y_v1a), 
                 init = init_valuesv1a,
                 thin = 5, 
                 cores = cores, 
                 chains = chains, 
                 iter = 10000)
save(mkt4_v1a, file = "bayes_simulation_mkt4_v1a_T_SCOTYORK.RData")

# V2F
y_v2f <- log(dta$jny_2f)
init_valuesv2f = list(list(beta=c(-1.33, 0.02, 0.01, 0.03, -1.04, 0.05, 0.04, 2.28,-2.15), sigma=3.8),
                      list(beta=c(-1.33, 0.02, 0.01, 0.03, -1.04, 0.05, 0.04, 2.28,-2.15), sigma=3.8),
                      list(beta=c(-1.33, 0.02, 0.01, 0.03, -1.04, 0.05, 0.04, 2.28,-2.15), sigma=3.8))
mkt4_v2f <- stan(file = "scripts/stan_models/stan_model_mkt4_v2f_T.stan", 
                 data = list(X = X, N = n, K = k, 
                             y = y_v2f), 
                 init = init_valuesv2f,
                 thin = 5,
                 cores = cores, 
                 chains = chains, 
                 iter = 10000)
save(mkt4_v2f, file = "bayes_simulation_mkt4_v2f_T_SCOTYORK.RData")

# V2R
y_v2r <- log(dta$jny_2r)
mkt4_v2r <- stan(file = "scripts/stan_models/stan_model_mkt4_v2r_T.stan", 
                 data = list(X = X, N = n, K = k, 
                             y = y_v2r), 
                 cores = cores, 
                 chains = chains, 
                 iter = 2000)
save(mkt4_v2r, file = "bayes_simulation_mkt4_v2r_T_SCOTYORK.RData")

# V2A
y_v2a <- log(dta$jny_2a)
mkt4_v2a <- stan(file = "scripts/stan_models/stan_model_mkt4_v2a_T.stan", 
                 data = list(X = X, N = n, K = k, 
                             y = y_v2a), 
                 cores = cores, 
                 chains = chains, 
                 iter = 10000)
save(mkt4_v2a, file = "bayes_simulation_mkt4_v2a_T_SCOTYORK.RData")

# Unknow code -------------------------
# # In R
# 
# # Declare a data frame that contains the known parameter names in one column `variable` and their known values
# known_parameters <- data_frame(variable = c(paste0("beta[",1:k,"]"),"sigma"), real_value = c(200, -2, sigma))
# 
# # Extract params as a (draws * number of chains * number of params) array
# extract(fit, permuted = F, pars = c("beta", "sigma")) %>% 
#   # Stack the chains on top of one another and drop the chains label
#   plyr::adply(2) %>% 
#   dplyr::select(-chains) %>% 
#   # Convert from wide form to long form (stack the columns on one another)
#   melt() %>% 
#   # Perform a left join with the known parameters
#   left_join(known_parameters, by = "variable") %>%
#   # Generate the plot
#   ggplot(aes(x = value)) + 
#   geom_density(fill = "orange", alpha = 0.5) + # Make it pretty
#   facet_wrap(~ variable, scales = "free") +
#   geom_vline(aes(xintercept = real_value), colour = "red") +
#   ggtitle("Actual parameters and estimates\ncorrectly specified model\n")

#--------------------------------------
# End

