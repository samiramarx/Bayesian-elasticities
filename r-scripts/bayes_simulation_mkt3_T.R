#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# BAYES ESTIMATION - MKT 3 
#--------------------------------------
# library
#--------------------------------------
library(tidyverse)
library(data.table)
library(broom)
library(rstan)
library(rstanarm)
#--------------------------------------
load("dta_spread_mkt3.RData")
#--------------------------------------

# General elements for estimation:
chains <- 3
iter <- 2000
cores <- 3

# Specify the data list that we will pass to Stan. This gives Stan everything declared in the data{} block. 
k <- 6 # intercept, f1N, f2F, f2R, f2A, g, gamma
n<- 29955
X <- cbind(c = rep(1, lenght.out = dta$rail_year),
           # log(dta$avgf1n),
           log(dta$avgf_2f),
           log(dta$avgf_2r),
           log(dta$avgf_2a),
           log(dta$avg_gva),
           log(dta$gjt_f))
#--------------------------------------
# V1N
y_v1n <- log(dta$jny1n)
mkt3_v1n <- stan(file = "scripts/stan_models/stan_model_mkt3_v1n_T.stan",
                 data = list(X = X, N = n, K = k, 
                             y = y_v1n), 
                 cores = cores, 
                 chains = chains, 
                 iter = iter)
save(mkt3_v1n, file = "bayes_simulation_mkt3_v1n_T.RData")

# V2F
y_v2f <- log(dta$jny_2f)
mkt3_v2f <- stan(file = "scripts/stan_models/stan_model_mkt3_v2f_T.stan",
                 data = list(X = X, N = n, K = k, 
                             y = y_v2f), 
                 cores = cores, 
                 chains = chains, 
                 iter = iter)
save(mkt3_v2f, file = "bayes_simulation_mkt3_v2f_T.RData")

# V2R
y_v2r <- log(dta$jny_2r)
# init_values_2r <- list(list(beta=c(-0.77, 0, 0, -0.94, 0.09, 1.52, -0.82), sigma=1.23),
#                        list(beta=c(-0.77, 0, 0, -0.94, 0.09, 1.52, -0.82), sigma=1.23),
#                        list(beta=c(-0.77, 0, 0, -0.94, 0.09, 1.52, -0.82), sigma=1.23))
# init_values_2r <- list(list(beta=c(0.44, 0, 0.02, -0.81, 0.08, 1.30, -0.74), sigma=1.23),
#                        list(beta=c(0.44, 0, 0.02, -0.81, 0.08, 1.30, -0.74), sigma=1.23),
#                        list(beta=c(0.44, 0, 0.02, -0.81, 0.08, 1.30, -0.74), sigma=1.23))
mkt3_v2r <- stan(file = "scripts/stan_models/stan_model_mkt3_v2r_T.stan", 
                 #init= init_values_2r, #init_values from the autocorrelated estimation iter 1000, thin 5
                 #  thin=5, #control = list(adapt_delta=0.99),
                 data = list(X = X, N = n, K = k, 
                             y = y_v2r), 
                 cores = cores, 
                 chains = chains, 
                 iter = 10000)
save(mkt3_v2r, file = "bayes_simulation_mkt3_v2r_T.RData")

# V2A
y_v2a <- log(dta$jny_2a)
init_values = list(list(beta=c(1.21, 0, 0.30, 2.74, -0.95, 0.77, -1.95), sigma=1.84),
                   list(beta=c(1.21, 0, 0.30, 2.74, -0.95, 0.77, -1.95), sigma=1.84),
                   list(beta=c(1.21, 0, 0.30, 2.74, -0.95, 0.77, -1.95), sigma=1.84))
mkt3_v2a <- stan(file = "scripts/stan_models/stan_model_mkt3_v2a_T.stan",  #init=init_values,
                 data = list(X = X, N = n, K = k, 
                             y = y_v2a), 
                 thin=5,
                 cores = cores, 
                 chains = chains, 
                 iter = 10000)
save(mkt3_v2a, file = "bayes_simulation_mkt3_v2a_T.RData")

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

