# OLS - SUR (seemingly unrelated regression)

library(quantreg)
library(nnet)
library(mgcv)
library(systemfit)


load("dta_spread_2nd.RData")
names(dta)

vf <-  log(jny_2f) ~ log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f) + rail_year
vr <-  log(jny_2r) ~ log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_r) + rail_year
va <-  log(jny_2a) ~ log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_r) + rail_year

fitsur <- systemfit( list(vf, vr, va), data = dta)
summary(fitsur)

vf <-  lm(data = dta, log(jny_2f) ~ log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_f) + rail_year)
vr <-  lm(data = dta, log(jny_2r) ~ log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_r) + rail_year)
va <-  lm(data = dta, log(jny_2a) ~ log(avgf_2f) + log(avgf_2r) + log(avgf_2a) + log(avg_gva) + log(gjt_r) + rail_year)

summary(va)
