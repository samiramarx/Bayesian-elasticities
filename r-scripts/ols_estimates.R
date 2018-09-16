#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# OLS ESTIMATES

#--------------------------------------
# library
#--------------------------------------
library(data.table)
library(quantreg)
library(nnet)
library(mgcv)
library(systemfit)
library(stargazer)
library(texreg)

#--------------------------------------
load("dta_spread_mkt1.RData") #2F, 2R
mkt1 <- as.data.table(dta)

load("dta_spread_mkt2.RData") #2F, 2R, 2A
mkt2 <- as.data.table(dta)

load("dta_spread_mkt3.RData") #1N, 2F, 2R, 2A
mkt3 <- as.data.table(dta)

load("dta_spread_mkt3_SCOTYORK.RData") #1N, 2F, 2R, 2A
mkt3_cut <- as.data.table(dta)

load("dta_spread_mkt4.RData") #1F, 1R, 1A, 2F, 2R, 2A
mkt4 <- as.data.table(dta)

load("dta_spread_mkt4.RData") #1F, 1R, 1A, 2F, 2R, 2A
mkt4_cut <- as.data.table(dta)

rm(dta)
#------------------------------------------

# MKT share of tickets
# max(dta$mktshr_2f)
# hist(dta$mktshr_2f)
# dta[ mktshr_2f > 0.95 , ]

# MKT1
mkt1_v2f <-  log(jny_2f) ~ log(avgf_2f) + log(avgf_2r) + log(avg_gva) + log(gjt_f) 
mkt1_v2r <-  log(jny_2r) ~ log(avgf_2f) + log(avgf_2r) + log(avg_gva) + log(gjt_f) 

mkt1_system <- systemfit( list(mkt1_v2f, mkt1_v2r), data = mkt1)
summary(mkt1_system)

# MKT2
mkt2_v2f <-  log(jny_2f) ~ log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f) 
mkt2_v2r <-  log(jny_2r) ~ log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f) 
mkt2_v2a <-  log(jny_2a) ~ log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f)

mkt2_system <- systemfit( list(mkt2_v2f, mkt2_v2r, mkt2_v2a), data = mkt2)
summary(mkt2_system)

# MKT3
mkt3_v1n <-  log(jny1n) ~ log(avgf1n) + log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f)
mkt3_v2f <-  log(jny_2f) ~ log(avgf1n) + log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f)
mkt3_v2r <-  log(jny_2r) ~ log(avgf1n) + log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f)
mkt3_v2a <-  log(jny_2a) ~ log(avgf1n) + log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f)

mkt3_system <- systemfit( list(mkt3_v1n, mkt3_v2f, mkt3_v2r, mkt3_v2a), data = mkt3)
summary(mkt3_system)

# MKT3 - SCOTYORK
#first, run equations from mkt3
mkt3_cut_system <- systemfit( list(mkt3_v1n, mkt3_v2f, mkt3_v2r, mkt3_v2a), data = mkt3_cut)
summary(mkt3_cut_system)

# MKT4
mkt4_v1f <-  log(jny_1f) ~ log(avgf_1f) + log(avgf_1r) + log(avgf_1a) + log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f)
mkt4_v1r <-  log(jny_1r) ~ log(avgf_1f) + log(avgf_1r) + log(avgf_1a) + log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f)
mkt4_v1a <-  log(jny_1a) ~ log(avgf_1f) + log(avgf_1r) + log(avgf_1a) + log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f)
mkt4_v2f <-  log(jny_2f) ~ log(avgf_1f) + log(avgf_1r) + log(avgf_1a) + log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f)
mkt4_v2r <-  log(jny_2r) ~ log(avgf_1f) + log(avgf_1r) + log(avgf_1a) + log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f)
mkt4_v2a <-  log(jny_2a) ~ log(avgf_1f) + log(avgf_1r) + log(avgf_1a) + log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f)

# class <- c("f", "r", "a")
# type <- c("1", "2")
# tk <- paste0(rep(type, each=3), class)
# fit <- paste0("mkt4_v", tk)

mkt4_system <- systemfit( list(mkt4_v1f, mkt4_v1r, mkt4_v1a, mkt4_v2f, mkt4_v2r, mkt4_v2a), data = mkt4)
summary(mkt4_system)

# MKT4 - SCOTYORK
#first, run equations from mkt4
mkt4_cut_system <- systemfit( list(mkt4_v1f, mkt4_v1r, mkt4_v1a, mkt4_v2f, mkt4_v2r, mkt4_v2a), data = mkt4_cut)
summary(mkt4_cut_system)

#------------------------------------
# Exporting results

# texreg(list(mkt1_system, mkt2_system))

# texreg(mkt1_system)
# texreg(mkt2_system)
# texreg(mkt3_system)
# texreg(mkt4_system)

# End