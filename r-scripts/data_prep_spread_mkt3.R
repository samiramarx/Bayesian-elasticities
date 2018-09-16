#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

#--------------------------------------
# library
#--------------------------------------
library(data.table); library(tidyr); library(dplyr)

load("dta_spread.RData")

#--------------------------------------
# creating 1N
#--------------------------------------
dta$rev1n <- dta$rev_1f + dta$rev_1r + dta$rev_1a
dta$jny1n <- dta$jny_1f + dta$jny_1r + dta$jny_1a
dta$rev1n_rpi <- dta$rev1n/dta$rpi
dta$avgf1n <- dta$rev1n_rpi/dta$jny1n
dta$mktshr1n <- dta$jny1n/(dta$jny1n+dta$jny_2f+dta$jny_2r+dta$jny_2a)

#--------------------------------------
# removing 1F, 1R, 1A
#--------------------------------------
rev1 <- names(select(dta, starts_with("rev_1")))
jny1 <- names(select(dta, starts_with("jny_1")))
avgf1 <- names(select(dta, starts_with("avgf_1")))
mktshr1 <- names(select(dta, starts_with("mktshr_1")))

dta <- dta[ , !c(rev1, 
                jny1, 
                avgf1,
                mktshr1
                ), with = FALSE]

#--------------------------------------
# deleting NaN, Inf
#--------------------------------------

# dta <- dta[complete.cases(dta), ] #delete all rows with a NA

dta <- dta[ avgf1n > 0 & avgf1n < 500, ] #delete all rows with avgfare <= 0
dta <- dta[ avgf_2f > 0 & avgf_2f < 500, ]
dta <- dta[ avgf_2r > 0 & avgf_2r < 500, ]
dta <- dta[ avgf_2a > 0 & avgf_2a < 500, ] 
dta <- dta[ jny1n > 0, ] #delete all rows with jny <= 0
dta <- dta[ jny_2f > 0, ] 
dta <- dta[ jny_2r > 0, ] 
dta <- dta[ jny_2a > 0, ] 

dta <- dta[ gjt_f > 0]
dta <- dta[ gjt_r > 0]

# summary(dta$avgf1n)
# summary(dta$avgf_2f)
# summary(dta$avgf_2r)
# summary(dta$avgf_2a)
# summary(dta$jny1n)
# summary(dta$jny_2f)
# summary(dta$jny_2r)
# summary(dta$jny_2a)
# 
# is.finite(dta$avgf1n) %>% all()
# is.finite(dta$avgf_2f) %>% all()
# is.finite(dta$avgf_2r) %>% all()
# is.finite(dta$avgf_2a) %>% all()
# is.finite(dta$jny1n) %>% all()
# is.finite(dta$jny_2f) %>% all()
# is.finite(dta$jny_2r)%>% all()
# is.finite(dta$jny_2a)%>% all()

#--------------------------------------
# filtering relevant mkt share
#--------------------------------------

# dta <- dta[ mktshr1n > 0.03, ]
# dta <- dta[ mktshr_2f > 0.03, ]
# dta <- dta[ mktshr_2r > 0.03, ]
# dta <- dta[ mktshr_2a > 0.03, ]

#---------------------------------------
# Save Data

save(dta, file = "dta_spread_mkt3.RData")
remove(list = ls())

