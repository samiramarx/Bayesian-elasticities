library(tidyverse); library(data.table); library(broom)
library(rstan)
library(rstanarm)

# parameters
n <- 1000
sigma <- 10
b0 <- rnorm(n, 200, 20)
b1 <- rnorm(n, -2, 0.5)
  
# simulation
x <- runif(n, min = 10, max = 50) # ticket price
e <- rnorm(n, mean = 0, sd = sigma)
y <- b0 + b1*x + e # train trips per month

# sample

DF <- data.frame(y, x)

DF %>% 
  ggplot(aes(x = x, y = y)) + geom_point()

# ols fit

ols <- lm(y ~ x, data = DF)
coef(ols)
glance(ols)$sigma

# bayes

# Specify the data list that we will pass to Stan. This gives Stan everything declared in the data{} block. 
k <- 2
X <- cbind(c = rep(1, lenght.out = x), x)

dta <- list(X = X, N = n, y = y, K = k)

fit <- stan(file = "scripts/model2.stan", data = dta, cores = 1, chains = 2, iter = 2000)

print(fit, pars = c("beta", "sigma"))
shinystan::launch_shinystan(fit)

# In R

# Declare a data frame that contains the known parameter names in one column `variable` and their known values
known_parameters <- data_frame(variable = c(paste0("beta[",1:k,"]"),"sigma"), real_value = c(200, -2, sigma))

# Extract params as a (draws * number of chains * number of params) array
extract(fit, permuted = F, pars = c("beta", "sigma")) %>% 
  # Stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Convert from wide form to long form (stack the columns on one another)
  melt() %>% 
  # Perform a left join with the known parameters
  left_join(known_parameters, by = "variable") %>%
  # Generate the plot
  ggplot(aes(x = value)) + 
  geom_density(fill = "orange", alpha = 0.5) + # Make it pretty
  facet_wrap(~ variable, scales = "free") +
  geom_vline(aes(xintercept = real_value), colour = "red") +
  ggtitle("Actual parameters and estimates\ncorrectly specified model\n")

