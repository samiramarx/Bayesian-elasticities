# http://nbviewer.jupyter.org/github/QuantEcon/QuantEcon.notebooks/blob/master/IntroToStan_basics_workflow.ipynb#

library(dplyr); library(ggplot2); library(rstan); library(reshape2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Generate a matrix of random numbers, and values for beta, nu and sigma

set.seed(42) # Set the random number generator seed so that we get the same parameters
N <- 1000 # Number of observations
P <- 10 # Number of covariates
X <- matrix(rnorm(N*P), N, P) # generate an N*P covariate matrix of random data
nu <- 5 # Set degrees of freedom
sigma <- 5 # And scale parameter
beta <- rnorm(10) # Generate some random coefficients that we'll try to recover
# Make sure the first element of beta is positive as in our chosen DGP
beta[1] <- abs(beta[1])


# Compile the script
compiled_function <- stan_model(file = "dgp.stan")
# And make the function available to the user in R
expose_stan_functions(compiled_function)


# simulation in R

# Draw a vector of random numbers for known Xs and parameters
y_sim <- dgp_rng(nu = nu, X = X, sigma = sigma, beta = beta)

# Plot the data
data_frame(y_sim = y_sim) %>% # Declare a data frame and pipe it into a ggplot
  ggplot(aes( x = y_sim)) + # Where we state the x-axis aesthetic (our simulated values)
  geom_histogram(binwidth = 3) # And tell ggplot what sort of chart to build

# In R

# Specify the data list that we will pass to Stan. This gives Stan everything declared in the data{} block. 
data <- list(X = X, N = N, y = y_sim, P = P)

# Call Stan. You'll need to give it either model_code (like the ones we defined above), a file (.stan file), 
# or a fitted Stan object (fit)
# You should also pass Stan a data list, number of cores to estimate on (jupyter only has access to one), 
# the number of Markov chains to run (4 by default)
# and number of iterations (2000 by default). 
# We use multiple chains to make sure that the posterior distribution that we converge on 
# is stable, and not affected by starting values. 

# The first time you run the models, they will take some time to compile before sampling. 
# On subsequent runs, it will only re-compile if you change the model code. 

incorrect_fit <- stan(file = "model_normal.stan", data = data, cores = 1, chains = 2, iter = 2000)
correct_fit <- stan(file = "model_student.stan", data = data, cores = 1, chains = 2, iter = 2000)

shinystan::launch_shinystan(correct_fit)

print(incorrect_fit, pars = c("beta", "sigma"))
print(correct_fit, pars = c("beta", "sigma", "nu"))


# In R

# Declare a data frame that contains the known parameter names in one column `variable` and their known values
known_parameters <- data_frame(variable = c(paste0("beta[",1:P,"]"),"sigma", "nu"), real_value = c(beta, sigma, nu))

# Extract params as a (draws * number of chains * number of params) array
extract(correct_fit, permuted = F, pars = c("beta", "sigma", "nu")) %>% 
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


extract(incorrect_fit, permuted = F, pars = c("beta", "sigma")) %>% 
  # Extract params as a (draws * number of chains * number of params) array
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Stack the chains on top of one another and drop the chains label
  melt() %>% 
  left_join(known_parameters, by = "variable") %>% # Join the known parameter table
  # Convert from wide form to long form (stack the columns on one another)
  # Write out the plot
  ggplot(aes(x = value)) + 
  geom_density(fill = "orange", alpha = 0.5) + # Make it pretty
  facet_wrap(~ variable, scales = "free") + # small sub-plots of each variable
  geom_vline(aes(xintercept = real_value), colour = "red") + # red vertical lines for the known parameters
  ggtitle("Actual parameters and estimates\nincorrectly specified model\n") # A title
