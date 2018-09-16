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
#--------------------------------------

# PRINT/EXPORT BAYES ESTIMATES
source("bayes_simulation_mkt3_v1n.RData")
source("bayes_simulation_mkt3_v2f.RData")
source("bayes_simulation_mkt3_v2r.RData")
source("bayes_simulation_mkt3_v2a.RData")

# PRINT
print(mkt3_v1n, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt3_v1n)

print(mkt3_v2f, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt3_v2f)

print(mkt3_v2r, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt3_v2r)

print(mkt3_v2a, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt3_v2a)

# EXPORT TABLES

mkt3_v1n_tbl <- summary(mkt3_v1n)
print(xtable(mkt3_v1n_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt3_v1n.tex")

mkt3_v2f_tbl <- summary(mkt3_v2f)
print(xtable(mkt3_v2f_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt3_v2f.tex")

mkt3_v2r_tbl <- summary(mkt3_v2r)
print(xtable(mkt3_v2r_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt3_v2r.tex")

mkt3_v2a_tbl <- summary(mkt3_v2a)
print(xtable(mkt3_v2a_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt3_v2a.tex")
