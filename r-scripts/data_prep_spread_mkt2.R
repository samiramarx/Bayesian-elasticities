#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

#-----------------------------------------
# library
#--------------------------------------
library(data.table); library(tidyr); library(dplyr)

load("dta_spread.RData")

#--------------------------------------
# filtering for mkt2: 2F, 2R, 2A
#--------------------------------------
dta <- dta[ rev_1f == "0" & rev_1r == "0" & rev_1a == "0", ]
dta <- dta[ !((rev_2a == "0")), ]

rev1 <- names(select(dta, starts_with("rev_1")))
jny1 <- names(select(dta, starts_with("jny_1")))
avgf1 <- names(select(dta, starts_with("avgf_1")))
mktshr1 <- names(select(dta, starts_with("mktshr_1")))

dta <- dta[ , !c(rev1, 
                jny1, 
                avgf1,
                mktshr1,
                ), with = FALSE]

#--------------------------------------
# deleting NaN, Inf
#--------------------------------------

# dta <- dta[complete.cases(dta), ] #delete all rows with a NA (some year that didn't have 2F or 2R jny and rev)

dta <- dta[ avgf_2f > 0 & avgf_2f < 500, ] #delete all rows with avgfare <= 0
dta <- dta[ avgf_2r > 0 & avgf_2r < 500, ]
dta <- dta[ avgf_2a > 0 & avgf_2a < 500, ] 
dta <- dta[ jny_2f > 0, ] #delete all rows with jny <= 0
dta <- dta[ jny_2r > 0, ] 
dta <- dta[ jny_2a > 0, ] 

dta <- dta[ gjt_f > 0]
dta <- dta[ gjt_r > 0]

# summary(dta$avgf_2f)
# summary(dta$avgf_2r)
# summary(dta$avgf_2a)
# summary(dta$jny_2f)
# summary(dta$jny_2r)
# summary(dta$jny_2a)
# 
# is.finite(dta$avgf_2f) %>% all()
# is.finite(dta$avgf_2r) %>% all()
# is.finite(dta$avgf_2a) %>% all()
# is.finite(dta$jny_2f) %>% all()
# is.finite(dta$jny_2r)%>% all()
# is.finite(dta$jny_2a)%>% all()

#---------------------------------------
# Save Data

save(dta, file = "dta_spread_mkt2.RData")
remove(list = ls())

