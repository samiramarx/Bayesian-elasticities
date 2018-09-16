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
#--------------------------------------
# PLOTTING POSTERIOR DISTRIBUTIONS

#V2F
load("bayes_simulation_mkt2_v2f_T.RData")
#rename model components
names(mkt2_v2f)[1] <- "Constant"
names(mkt2_v2f)[2] <- "f_2F"
names(mkt2_v2f)[3] <- "f_2R"
names(mkt2_v2f)[4] <- "f_2A"
names(mkt2_v2f)[5] <- "g"
names(mkt2_v2f)[6] <- "gamma"
names(mkt2_v2f)[7] <- "sigma"
names(mkt2_v2f)

ref_values <- data_frame(variable= c("Constant", "f_2F", "f_2R", "f_2A", "g", "gamma"), 
                         ols_value = c(-1.09, -1.24,-0.06, 0.15, 1.75, -1.23),
                         mean_value = c(-0.28, -1.29, 0.05, 0.14, 1.68, -1.28),
                         lower_bound = c(-1.89, -1.37, 0.00, 0.06, 1.54, -1.39), 
                         upper_bound = c(1.14, -1.20, 0.14, 0.21, 1.83, -1.17))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt2_v2f, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_2F','f_2R', 'f_2A', 'g','gamma', 'Constant'))
temp %>% 
  # Generate the plot
  ggplot(aes(x = value)) + 
  geom_density(fill = "grey", alpha = 0.5) + # Make it pretty
  facet_wrap(~ variable, scales = "free", ncol=3) +
  geom_vline(aes(xintercept = mean_value)) +
  geom_vline(aes(xintercept = lower_bound), linetype="dotted")+
  geom_vline(aes(xintercept = upper_bound), linetype="dotted")+
  geom_vline(aes(xintercept = ols_value), colour ="red")+
  xlab("") + ylab("Density")
  # ggtitle("Actual parameters and estimates\ncorrectly specified model\n")

#V2R
load("bayes_simulation_mkt2_v2r_T.RData")
#rename model components
names(mkt2_v2r)[1] <- "Constant"
names(mkt2_v2r)[2] <- "f_2F"
names(mkt2_v2r)[3] <- "f_2R"
names(mkt2_v2r)[4] <- "f_2A"
names(mkt2_v2r)[5] <- "g"
names(mkt2_v2r)[6] <- "gamma"
names(mkt2_v2r)[7] <- "sigma"
names(mkt2_v2r)

ref_values <- data_frame(variable= c("Constant", "f_2F", "f_2R", "f_2A", "g", "gamma"),  
                         ols_value = c(11.72, 0.03, -0.60, -0.10, 0.39, -1.21),
                         mean_value = c(5.46, 0.04, -0.80, 0.01, 0.96, -1.07),
                         lower_bound = c(4.11, 0.00, -0.91, 0.00, 0.82, -1.18), 
                         upper_bound = c(6.86, 0.09, -0.70, 0.03, 1.09, -0.95))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt2_v2r, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_2F','f_2R', 'f_2A','g','gamma', 'Constant'))
temp %>% 
  # Generate the plot
  ggplot(aes(x = value)) + 
  geom_density(fill = "grey", alpha = 0.5) + # Make it pretty
  facet_wrap(~ variable, scales = "free", ncol=3) +
  geom_vline(aes(xintercept = mean_value)) +
  geom_vline(aes(xintercept = lower_bound), linetype="dotted")+
  geom_vline(aes(xintercept = upper_bound), linetype="dotted")+
  geom_vline(aes(xintercept = ols_value), colour ="red")+
  xlab("") + ylab("Density")
# ggtitle("Actual parameters and estimates\ncorrectly specified model\n")

#V2A
load("bayes_simulation_mkt2_v2a_T.RData")
#rename model components
names(mkt2_v2a)[1] <- "Constant"
names(mkt2_v2a)[2] <- "f_2F"
names(mkt2_v2a)[3] <- "f_2R"
names(mkt2_v2a)[4] <- "f_2A"
names(mkt2_v2a)[5] <- "g"
names(mkt2_v2a)[6] <- "gamma"
names(mkt2_v2a)[7] <- "sigma"
names(mkt2_v2a)

ref_values <- data_frame(variable= c("Constant", "f_2F", "f_2R", "f_2A", "g", "gamma"),  
                         ols_value = c(-5.53, -0.18, 1.04, -0.47, 0.66, 0.33),
                         mean_value = c(-0.61, 0.05, 1.01, -0.46, 0.29, -0.03),
                         lower_bound = c(-2.29, 0.00, 0.84, -0.57, 0.13, -0.08), 
                         upper_bound = c(0.92, 0.13, 1.15, -0.34, 0.45, -0.0))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt2_v2a, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_2F','f_2R', 'f_2A','g','gamma', 'Constant'))
temp %>% 
  # Generate the plot
  ggplot(aes(x = value)) + 
  geom_density(fill = "grey", alpha = 0.5) + # Make it pretty
  facet_wrap(~ variable, scales = "free", ncol=3) +
  geom_vline(aes(xintercept = mean_value)) +
  geom_vline(aes(xintercept = lower_bound), linetype="dotted")+
  geom_vline(aes(xintercept = upper_bound), linetype="dotted")+
  geom_vline(aes(xintercept = ols_value), colour ="red")+
  xlab("") + ylab("Density")
# ggtitle("Actual parameters and estimates\ncorrectly specified model\n")
