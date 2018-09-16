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
library(reshape2)
#--------------------------------------
# PLOTTING POSTERIOR DISTRIBUTIONS

#V1F
load("bayes_simulation_mkt4_v1f_T_SCOTYORK.RData")

#rename model components
names(mkt4_v1f)[1] <- "Constant"
names(mkt4_v1f)[2] <- "f_1F"
names(mkt4_v1f)[3] <- "f_1R"
names(mkt4_v1f)[4] <- "f_1A"
names(mkt4_v1f)[5] <- "f_2F"
names(mkt4_v1f)[6] <- "f_2R"
names(mkt4_v1f)[7] <- "f_2A"
names(mkt4_v1f)[8] <- "g"
names(mkt4_v1f)[9] <- "gamma"
names(mkt4_v1f)[10] <- "sigma"
names(mkt4_v1f)

ref_values <- data_frame(variable= c("Constant", "f_1F", "f_1R", "f_1A", "f_2F", "f_2R", "f_2A", "g", "gamma"), 
                         ols_value = c(-12.0, -0.94, 0.06, 0.28, 0.85, 0.67, 0.18, 2.74, -2.39),
                         mean_value = c(-2.4, -1.02, 0.05, 0.24, 0.91, 0.87, 0.16, 1.91, -2.66),
                         lower_bound = c(-4.15, -1.38, 0.00, 0.03, 0.63, 0.41, 0.00, 1.75, -2.92), 
                         upper_bound = c(-0.63, -0.66, 0.13, 0.44, 1.16, 1.28, 0.39, 2.08, -2.41))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt4_v1f, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_1F', 'f_1R', 'f_1A', 'f_2F','f_2R', 'f_2A', 'g','gamma', 'Constant'))
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

#V1R
load("bayes_simulation_mkt4_v1r_T_SCOTYORK.RData")

#rename model components
names(mkt4_v1r)[1] <- "Constant"
names(mkt4_v1r)[2] <- "f_1F"
names(mkt4_v1r)[3] <- "f_1R"
names(mkt4_v1r)[4] <- "f_1A"
names(mkt4_v1r)[5] <- "f_2F"
names(mkt4_v1r)[6] <- "f_2R"
names(mkt4_v1r)[7] <- "f_2A"
names(mkt4_v1r)[8] <- "g"
names(mkt4_v1r)[9] <- "gamma"
names(mkt4_v1r)[10] <- "sigma"
names(mkt4_v1r)

ref_values <- data_frame(variable= c("Constant", "f_1F", "f_1R", "f_1A", "f_2F", "f_2R", "f_2A", "g", "gamma"), 
                         ols_value = c(-3.34, -0.09, -0.43, -0.99, 0.72, 0.0, 2.37, 1.16, -1.79),
                         mean_value = c(-0.71, 0.04, -0.49, 0.02, 0.46, 0.13, 1.28, 1.21, -1.48),
                         lower_bound = c(-2.51, 0.00, -0.64, 0.00, 0.21, 0.00, 0.90, 0.59, -1.76), 
                         upper_bound = c(1.03, 0.12, -0.35, 0.05, 0.73, 0.34, 1.55, 0.95, -1.20))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt4_v1r, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_1F', 'f_1R', 'f_1A', 'f_2F','f_2R', 'f_2A', 'g','gamma', 'Constant'))
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

#V1A
load("bayes_simulation_mkt4_v1a_T_SCOTYORK.RData")

#rename model components
names(mkt4_v1a)[1] <- "Constant"
names(mkt4_v1a)[2] <- "f_1F"
names(mkt4_v1a)[3] <- "f_1R"
names(mkt4_v1a)[4] <- "f_1A"
names(mkt4_v1a)[5] <- "f_2F"
names(mkt4_v1a)[6] <- "f_2R"
names(mkt4_v1a)[7] <- "f_2A"
names(mkt4_v1a)[8] <- "g"
names(mkt4_v1a)[9] <- "gamma"
names(mkt4_v1a)[10] <- "sigma"
names(mkt4_v1a)

ref_values <- data_frame(variable= c("Constant", "f_1F", "f_1R", "f_1A", "f_2F", "f_2R", "f_2A", "g", "gamma"), 
                         ols_value = c(-5.88, 1.20, -0.25, 0.09, 0.74, 0.10, 1.04, 1.12, -2.03),
                         mean_value = c(-0.84, 1.10, 0.02, -0.09, 0.72, 0.43, 0.82, 0.69, -2.18),
                         lower_bound = c(-2.79, 0.69, 0.00, -0.24, 0.42, 0.00, 0.43, 0.49, -2.46), 
                         upper_bound = c(0.97, 1.45, 0.06, -0.00, 1.02, 0.82, 1.18, 0.88, -1.85))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt4_v1a, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_1F', 'f_1R', 'f_1A', 'f_2F','f_2R', 'f_2A', 'g','gamma', 'Constant'))
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

#V2F
load("bayes_simulation_mkt4_v2f_T_SCOTYORK.RData")

#rename model components
names(mkt4_v2f)[1] <- "Constant"
names(mkt4_v2f)[2] <- "f_1F"
names(mkt4_v2f)[3] <- "f_1R"
names(mkt4_v2f)[4] <- "f_1A"
names(mkt4_v2f)[5] <- "f_2F"
names(mkt4_v2f)[6] <- "f_2R"
names(mkt4_v2f)[7] <- "f_2A"
names(mkt4_v2f)[8] <- "g"
names(mkt4_v2f)[9] <- "gamma"
names(mkt4_v2f)[10] <- "sigma"
names(mkt4_v2f)

ref_values <- data_frame(variable= c("Constant", "f_1F", "f_1R", "f_1A", "f_2F", "f_2R", "f_2A", "g", "gamma"), 
                         ols_value = c(-8.38, -0.50, -0.14, 0.32, -0.59, -0.32, 0.03, 2.86, -1.71),
                         mean_value = c(-2.27, 0.02, 0.01, 0.04, -1.04, 0.05, 0.04, 2.37, -2.14),
                         lower_bound = c(-3.95, 0.00, 0.00, 0.00, -1.21, 0.00, 0.00, 2.21, -2.32), 
                         upper_bound = c(-0.86, 0.07, 0.04, 0.09, -0.86, 0.15, 0.11, 2.52, -1.93))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt4_v2f, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_1F', 'f_1R', 'f_1A', 'f_2F','f_2R', 'f_2A', 'g','gamma', 'Constant'))
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
load("bayes_simulation_mkt4_v2r_T_SCOTYORK.RData")

#rename model components
names(mkt4_v2r)[1] <- "Constant"
names(mkt4_v2r)[2] <- "f_1F"
names(mkt4_v2r)[3] <- "f_1R"
names(mkt4_v2r)[4] <- "f_1A"
names(mkt4_v2r)[5] <- "f_2F"
names(mkt4_v2r)[6] <- "f_2R"
names(mkt4_v2r)[7] <- "f_2A"
names(mkt4_v2r)[8] <- "g"
names(mkt4_v2r)[9] <- "gamma"
names(mkt4_v2r)[10] <- "sigma"
names(mkt4_v2r)

ref_values <- data_frame(variable= c("Constant", "f_1F", "f_1R", "f_1A", "f_2F", "f_2R", "f_2A", "g", "gamma"), 
                         ols_value = c(-0.38, 0.01, 0.22, 0.44, 0.51, -1.32, 0.35, 1.91, -2.07),
                         mean_value = c(-0.01, 0.16, 0.21, 0.39, 0.45, -1.32, 0.31, 1.87, -2.07),
                         lower_bound = c(-1.61, 0.00, 0.11, 0.22, 0.22, -1.72, 0.01, 1.73, -2.30), 
                         upper_bound = c(1.45, 0.38, 0.32, 0.57, 0.67, -0.98, 0.56, 2.03, -1.84))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt4_v2r, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_1F', 'f_1R', 'f_1A', 'f_2F','f_2R', 'f_2A', 'g','gamma', 'Constant'))
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
load("bayes_simulation_mkt4_v2a_T_SCOTYORK.RData")

#rename model components
names(mkt4_v2a)[1] <- "Constant"
names(mkt4_v2a)[2] <- "f_1F"
names(mkt4_v2a)[3] <- "f_1R"
names(mkt4_v2a)[4] <- "f_1A"
names(mkt4_v2a)[5] <- "f_2F"
names(mkt4_v2a)[6] <- "f_2R"
names(mkt4_v2a)[7] <- "f_2A"
names(mkt4_v2a)[8] <- "g"
names(mkt4_v2a)[9] <- "gamma"
names(mkt4_v2a)[10] <- "sigma"
names(mkt4_v2a)

ref_values <- data_frame(variable= c("Constant", "f_1F", "f_1R", "f_1A", "f_2F", "f_2R", "f_2A", "g", "gamma"), 
                         ols_value = c(-7.47, 0.54, -0.11, 0.88, 0.82, 0.46, -0.97, 1.85, -1.90),
                         mean_value = c(-1.17, 0.46, 0.03, 0.80, 0.83, 0.64, -1.08, 1.30, -2.05),
                         lower_bound = c(-2.92, 0.06, 0.00, 0.53, 0.52, 0.10, -1.45, 1.12, -2.34), 
                         upper_bound = c(0.69, 0.83, 0.08, 1.05, 1.13, 1.13, -0.69, 1.49, -1.73))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt4_v2a, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_1F', 'f_1R', 'f_1A', 'f_2F','f_2R', 'f_2A', 'g','gamma', 'Constant'))
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


