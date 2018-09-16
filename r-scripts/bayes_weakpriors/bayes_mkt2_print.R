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
load("bayes_simulation_mkt2_v2f.RData")
load("bayes_simulation_mkt2_v2r.RData")
load("bayes_simulation_mkt2_v2a.RData")

# PRINT
print(mkt2_v2f, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt2_v2f)

print(mkt2_v2r, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt2_v2r)

print(mkt2_v2a, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt2_v2a)

# EXPORT TABLES

mkt2_v2f_tbl <- summary(mkt2_v2f)
print(xtable(mkt2_v2f_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt2_v2f.tex")

mkt2_v2r_tbl <- summary(mkt2_v2r)
print(xtable(mkt2_v2r_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt2_v2r.tex")

mkt2_v2a_tbl <- summary(mkt2_v2a)
print(xtable(mkt2_v2a_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt2_v2a.tex")
