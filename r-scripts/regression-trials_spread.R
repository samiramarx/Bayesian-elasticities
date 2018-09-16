#=========================================
# MSc Transport Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# Regression trials - SPREAD 

load("dta_spread.RData")

v2f <- lm(data = dta, log(jny_2f) ~  log(avgf_2f) + 
            log(avgf_2r) +
            log(avgf_2a) +
            log(avgf_1f) +
            log(avgf_1r) +
            log(avgf_1a)+
            log(avg_gva)+
            log(gjt_f)+
            log(gjt_r)
)
summary(v2f)
xtable::xtable(v2f)

v2r <- lm(data = dta, log(jny_2r) ~  log(avgf_2f) + 
            log(avgf_2r) +
            log(avgf_2a) +
            log(avgf_1f) +
            log(avgf_1r) +
            log(avgf_1a)+
            log(avg_gva)+
            log(gjt_f)+
            log(gjt_r)
)

summary(v2r)
xtable::xtable(v2r)

v2a <- lm(data = dta, log(jny_2a) ~  log(avgf_2f) + 
            log(avgf_2r) +
            log(avgf_2a) +
            log(avgf_1f) +
            log(avgf_1r) +
            log(avgf_1a)+
            log(avg_gva)+
            log(gjt_f)+
            log(gjt_r)
)

summary(v2a)
xtable::xtable(v2a)

#------------------------------
#2nd-only class trials
#------------------------------

load("dta_spread_2nd.RData")

v2f_2nd <- lm(data = dta, log(jny_2f) ~  log(avgf_2f) + 
            log(avgf_2r) +
            log(avgf_2a) +
            log(avg_gva)+
            log(gjt_f)+
            log(gjt_r)
)

summary(v2f)
xtable::xtable(v2f)

v2r_2nd <- lm(data = dta, log(jny_2r) ~  log(avgf_2f) + 
            log(avgf_2r) +
            log(avgf_2a) +
            log(avg_gva)+
            log(gjt_f)+
            log(gjt_r)
)

summary(v2r)

v2a_2nd <- lm(data = dta, log(jny_2a) ~  log(avgf_2f) + 
            log(avgf_2r) +
            log(avgf_2a) +
            log(avg_gva)+
            log(gjt_f)+
            log(gjt_r)
)

summary(v2a)
log(dta$avgf_2f) 
dta[ log(avgf_2f)<0, ]

log(0.30)
