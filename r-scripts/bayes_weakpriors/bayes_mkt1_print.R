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

source("bayes_simulation_mkt1_T.RData")

# PRINT
print(mkt1_v2f, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt1_v2f)

print(mkt1_v2r, pars = c("beta", "sigma"))
shinystan::launch_shinystan(mkt1_v2r)

# EXPORT TABLES
mkt1_v2f_tbl <- summary(mkt1_v2f)
print(xtable(mkt1_v2f_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt1_v2f.tex")

mkt1_v2r_tbl <- summary(mkt1_v2r)
print(xtable(mkt1_v2r_tbl$summary, type = "latex"), file = "exported_tables/bayes_mkt1_v2r.tex")

#--------------------------------------
# End