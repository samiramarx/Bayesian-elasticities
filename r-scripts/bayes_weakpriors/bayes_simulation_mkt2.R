#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# BAYES ESTIMATION - MKT 2 - WEAKLY INFORMATIVE PRIOR
#--------------------------------------
# library
#--------------------------------------
library(tidyverse)
library(data.table)
library(broom)
library(rstan)
library(rstanarm)
#--------------------------------------
load("dta_spread_mkt2.RData")
#--------------------------------------

# General elements for estimation:
chains <- 3
iter <- 2000
cores<- 1

# Specify the data list that we will pass to Stan. This gives Stan everything declared in the data{} block. 
k <- 6 # intercept, f2F, f2R, f2A, g, gamma
n<- 5909
X <- cbind(c = rep(1, lenght.out = dta$rail_year),
           log(dta$avgf_2f),
           log(dta$avgf_2r),
           log(dta$avgf_2a),
           log(dta$avg_gva),
           log(dta$gjt_f))
#--------------------------------------
# V2F
y_v2f <- log(dta$jny_2f)
mkt2_v2f <- stan(file = "scripts/stan_models/stan_model_mkt2_v2f.stan", 
                 data = list(X = X, N = n, K = k, 
                             y = y_v2f), 
                 cores = cores, 
                 chains = chains, 
                 iter = iter)
save(mkt2_v2f, file = "bayes_simulation_mkt2_v2f.RData")

# V2R
y_v2r <- log(dta$jny_2r)
mkt2_v2r <- stan(file = "scripts/stan_models/stan_model_mkt2_v2r.stan", 
                 data = list(X = X, N = n, K = k, 
                             y = y_v2r), 
                 cores = cores, 
                 chains = chains, 
                 iter = iter)
save(mkt2_v2r, file = "bayes_simulation_mkt2_v2r.RData")

# V2A
y_v2a <- log(dta$jny_2a)
mkt2_v2a <- stan(file = "scripts/stan_models/stan_model_mkt2_v2a.stan", 
                 data = list(X = X, N = n, K = k, 
                             y = y_v2a), 
                 cores = cores, 
                 chains = chains, 
                 iter = iter)
save(mkt2_v2a, file = "bayes_simulation_mkt2_v2a.RData")

#--------------------------------------
# save(mkt2_v2f, mkt2_v2r, mkt2_v2a, file = "bayes_simulation_mkt2.RData")

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

