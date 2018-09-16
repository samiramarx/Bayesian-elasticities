#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

#--------------------------------------
# library
#--------------------------------------
library(xtable)
library(ggplot2)
library(plyr)
library(dplyr)
library(rstan)
#--------------------------------------
# PLOTTING POSTERIOR DISTRIBUTIONS

#V1N
load("bayes_simulation_mkt3_v1n_T_SCOTYORK.RData")

#rename model components
names(mkt3_v1n)[1] <- "Constant"
names(mkt3_v1n)[2] <- "f_1N"
names(mkt3_v1n)[3] <- "f_2F"
names(mkt3_v1n)[4] <- "f_2R"
names(mkt3_v1n)[5] <- "f_2A"
names(mkt3_v1n)[6] <- "g"
names(mkt3_v1n)[7] <- "gamma"
names(mkt3_v1n)[8] <- "sigma"
names(mkt3_v1n)

ref_values <- data_frame(variable= c("Constant", "f_1N","f_2F", "f_2R", "f_2A", "g", "gamma"), 
                         ols_value = c(0.62, -0.84, 1.03, 1.8, 0.43, 1.5, -3.30),
                         mean_value = c(0.20, -0.82, 1.04, 1.73, 0.44, 1.53, -3.25),
                         lower_bound = c(-1.31, -0.94, 0.86, 1.50, 0.32, 1.38, -3.41), 
                         upper_bound = c(1.66, -0.70, 1.20, 1.94, 0.56, 1.67, -3.11))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt3_v1n, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_1N', 'f_2F','f_2R', 'f_2A', 'g','gamma', 'Constant'))
temp %>% 
  # Generate the plot
  ggplot(aes(x = value)) + 
  geom_density(fill = "grey", alpha = 0.5) + # Make it pretty
  facet_wrap(~ variable, scales = "free", ncol=4) +
  geom_vline(aes(xintercept = mean_value)) +
  geom_vline(aes(xintercept = lower_bound), linetype="dotted")+
  geom_vline(aes(xintercept = upper_bound), linetype="dotted")+
  geom_vline(aes(xintercept = ols_value), colour ="red")+
  xlab("") + ylab("Density")
# ggtitle("Actual parameters and estimates\ncorrectly specified model\n")

#V2F
load("bayes_simulation_mkt3_v2f_T_SCOTYORK.RData")

#rename model components
names(mkt3_v2f)[1] <- "Constant"
names(mkt3_v2f)[2] <- "f_1N"
names(mkt3_v2f)[3] <- "f_2F"
names(mkt3_v2f)[4] <- "f_2R"
names(mkt3_v2f)[5] <- "f_2A"
names(mkt3_v2f)[6] <- "g"
names(mkt3_v2f)[7] <- "gamma"
names(mkt3_v2f)[8] <- "sigma"
names(mkt3_v2f)

ref_values <- data_frame(variable= c("Constant", "f_1N","f_2F", "f_2R", "f_2A", "g", "gamma"), 
                         ols_value = c(2.85, -0.43, -1.12, -0.04, 0.33, 1.66, -1.60),
                         mean_value = c(1.56, 0, -1.4, 0.02, 0.18, 1.72, -1.55),
                         lower_bound = c(0.20, 0.00, -1.50, 0.00, 0.08, 1.60, -1.66), 
                         upper_bound = c(2.88, 0.01, -1.30, 0.06, 0.28, 1.86, -1.44))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt3_v2f, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_1N', 'f_2F','f_2R', 'f_2A', 'g','gamma', 'Constant'))
temp %>% 
  # Generate the plot
  ggplot(aes(x = value)) + 
  geom_density(fill = "grey", alpha = 0.5) + # Make it pretty
  facet_wrap(~ variable, scales = "free", ncol=4) +
  geom_vline(aes(xintercept = mean_value)) +
  geom_vline(aes(xintercept = lower_bound), linetype="dotted")+
  geom_vline(aes(xintercept = upper_bound), linetype="dotted")+
  geom_vline(aes(xintercept = ols_value), colour ="red")+
  xlab("") + ylab("Density")
# ggtitle("Actual parameters and estimates\ncorrectly specified model\n")

#V2R
load("bayes_simulation_mkt3_v2r_T_SCOTYORK.RData")

#rename model components
names(mkt3_v2r)[1] <- "Constant"
names(mkt3_v2r)[2] <- "f_1N"
names(mkt3_v2r)[3] <- "f_2F"
names(mkt3_v2r)[4] <- "f_2R"
names(mkt3_v2r)[5] <- "f_2A"
names(mkt3_v2r)[6] <- "g"
names(mkt3_v2r)[7] <- "gamma"
names(mkt3_v2r)[8] <- "sigma"
names(mkt3_v2r)

ref_values <- data_frame(variable= c("Constant", "f_1N","f_2F", "f_2R", "f_2A", "g", "gamma"), 
                         ols_value = c(11.66, -0.44, 0.23, 0.34, 0.15, 0.80, -2.31),
                         mean_value = c(5.80, 0.01, 0.16, -0.06, 0.10, 1.24, -2.05),
                         lower_bound = c(4.54, 0.00, 0.06, -0.15, 0.01, 1.12, -2.17), 
                         upper_bound = c(7.18, 0.02, 0.27, -0.00, 0.18, 1.38, -1.94))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt3_v2r, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_1N', 'f_2F','f_2R', 'f_2A', 'g','gamma', 'Constant'))
temp %>% 
  # Generate the plot
  ggplot(aes(x = value)) + 
  geom_density(fill = "grey", alpha = 0.5) + # Make it pretty
  facet_wrap(~ variable, scales = "free", ncol=4) +
  geom_vline(aes(xintercept = mean_value)) +
  geom_vline(aes(xintercept = lower_bound), linetype="dotted")+
  geom_vline(aes(xintercept = upper_bound), linetype="dotted")+
  geom_vline(aes(xintercept = ols_value), colour ="red")+
  xlab("") + ylab("Density")
# ggtitle("Actual parameters and estimates\ncorrectly specified model\n")

#V2A
load("bayes_simulation_mkt3_v2a_T_SCOTYORK.RData")

#rename model components
names(mkt3_v2a)[1] <- "Constant"
names(mkt3_v2a)[2] <- "f_1N"
names(mkt3_v2a)[3] <- "f_2F"
names(mkt3_v2a)[4] <- "f_2R"
names(mkt3_v2a)[5] <- "f_2A"
names(mkt3_v2a)[6] <- "g"
names(mkt3_v2a)[7] <- "gamma"
names(mkt3_v2a)[8] <- "sigma"
names(mkt3_v2a)

ref_values <- data_frame(variable= c("Constant", "f_1N","f_2F", "f_2R", "f_2A", "g", "gamma"), 
                         ols_value = c(6.72, -0.5, 1.07, 2.28, -0.39, 1.54, -2.01),
                         mean_value = c(-2.37, 0.01, 1.04, 1.97, -0.54, 1.03, -1.94),
                         lower_bound = c(-3.99, 0.00, 0.84, 1.74, -0.69, 0.87, -2.13), 
                         upper_bound = c(-0.58, 0.03, 1.23, 2.18, -0.40, 1.19, -1.77))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt3_v2a, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_1N', 'f_2F','f_2R', 'f_2A', 'g','gamma', 'Constant'))
temp %>% 
  # Generate the plot
  ggplot(aes(x = value)) + 
  geom_density(fill = "grey", alpha = 0.5) + # Make it pretty
  facet_wrap(~ variable, scales = "free", ncol=4) +
  geom_vline(aes(xintercept = mean_value)) +
  geom_vline(aes(xintercept = lower_bound), linetype="dotted")+
  geom_vline(aes(xintercept = upper_bound), linetype="dotted")+
  geom_vline(aes(xintercept = ols_value), colour ="red")+
  xlab("") + ylab("Density")
# ggtitle("Actual parameters and estimates\ncorrectly specified model\n")

