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
#-------------------------------------

load("bayes_simulation_mkt4_v1f_T_SCOTYORK.RData")
load("bayes_simulation_mkt4_v1r_T_SCOTYORK.RData")
load("bayes_simulation_mkt4_v1a_T_SCOTYORK.RData")
load("bayes_simulation_mkt4_v2f_T_SCOTYORK.RData")
load("bayes_simulation_mkt4_v2f_T_SCOTYORK.RData")
load("bayes_simulation_mkt4_v2a_T_SCOTYORK.RData")

# PRINT

print(mkt4_v1f, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt4_v1f)

print(mkt4_v1r, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt4_v1r)

print(mkt4_v1a, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt4_v1a)

print(mkt4_v2f, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt4_v2f)

print(mkt4_v2r, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt4_v2r)

print(mkt4_v2a, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt4_v2a)

# EXPORT TABLES

mkt4_v1f_tbl <- summary(mkt4_v1f)
print(xtable(mkt4_v1f_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt4_v1f_T_SCOTYORK.tex")

mkt4_v1r_tbl <- summary(mkt4_v1r)
print(xtable(mkt4_v1r_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt4_v1r_T_SCOTYORK.tex")

mkt4_v1a_tbl <- summary(mkt4_v1a)
print(xtable(mkt4_v1a_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt4_v1a_T_SCOTYORK.tex")

mkt4_v2f_tbl <- summary(mkt4_v2f)
print(xtable(mkt4_v2f_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt4_v2f_T_SCOTYORK.tex")

mkt4_v2r_tbl <- summary(mkt4_v2r)
print(xtable(mkt4_v2r_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt4_v2r_T_SCOTYORK.tex")

mkt4_v2a_tbl <- summary(mkt4_v2a)
print(xtable(mkt4_v2a_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt4_v2a_T_SCOTYORK.tex")
