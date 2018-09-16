# OLS estimation with dummy - Time Trend

library(dummies)

load(file = "dta_spread_2nd.RData")

load("dta_spread.RData")
str(dta)

dta$rail_year <- factor(dta$rail_year, levels = c(1996:2014))

dummy_dta <- dummy.data.frame(dta, names = "rail_year")


v2f_2nd <- lm(data = dta, log(jny_2f) ~  log(avgf_2f) + 
                log(avgf_2r) +
                log(avgf_2a) +
                log(gjt_f)+
                log(gjt_r)+
                rail_year
)

summary(v2f_2nd)
