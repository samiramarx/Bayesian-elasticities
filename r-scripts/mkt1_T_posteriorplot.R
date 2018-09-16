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
library(reshape2)
#--------------------------------------
# PLOTTING POSTERIOR DISTRIBUTIONS

#V2F
load("bayes_simulation_mkt1_v2f_T.RData")
#rename model components
names(mkt1_v2f)[1] <- "Constant"
names(mkt1_v2f)[2] <- "f_2F"
names(mkt1_v2f)[3] <- "f_2R"
names(mkt1_v2f)[4] <- "g"
names(mkt1_v2f)[5] <- "gamma"
names(mkt1_v2f)[6] <- "sigma"
names(mkt1_v2f)

ref_values <- data_frame(variable= c("Constant", "f_2F", "f_2R", "g", "gamma"), 
                         ols_value = c(9.13, -1.28,0.17, 0.49, -0.98),
                         mean_value = c(6.10, -1.33, 0.17, 0.77, -0.89),
                         lower_bound = c(4.97, -1.42, 0.07, 0.67, -0.98), 
                         upper_bound = c(7.11, -1.23, 0.26, 0.87, -0.80))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt1_v2f, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_2F','f_2R', 'g','gamma', 'Constant'))
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
  # theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
  # ggtitle("Actual parameters and estimates\ncorrectly specified model\n")

#V2R
load("bayes_simulation_mkt1_v2r_T.RData")
#rename model components
names(mkt1_v2r)[1] <- "Constant"
names(mkt1_v2r)[2] <- "f_2F"
names(mkt1_v2r)[3] <- "f_2R"
names(mkt1_v2r)[4] <- "g"
names(mkt1_v2r)[5] <- "gamma"
names(mkt1_v2r)[6] <- "sigma"
names(mkt1_v2r)

ref_values <- data_frame(variable= c("Constant", "f_2F", "f_2R", "g", "gamma"), 
                         ols_value = c(17.58, 0.81, -0.89, -0.52, -1.06),
                         mean_value = c(11.27, 0.70, -0.89, 0.06, -0.88),
                         lower_bound = c(10.32, 0.61, -1, 0.00, -0.97), 
                         upper_bound = c(12.06, 0.80, -0.80, 0.14, -0.80))

# Extract params as a (draws * number of chains * number of params) array
temp <- rstan::extract(mkt1_v2r, permuted = F, pars = c("beta")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(ref_values, by = "variable")
temp$variable = factor(temp$variable, levels=c('f_2F','f_2R','g','gamma', 'Constant'))
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
