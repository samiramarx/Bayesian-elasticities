#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# MKT 4 - 1F, 1R, 1A, 2F, 2R, 2A

load("dta_spread.RData")

#--------------------------------------
# deleting NaN, Inf
#--------------------------------------

dta <- dta[complete.cases(dta), ] #delete all rows with a NA (some year that didn't have 2F or 2R jny and rev)

dta <- dta[ avgf_1f > 0 & avgf_1f < 500, ]
dta <- dta[ avgf_1r > 0 & avgf_1r < 500, ]
dta <- dta[ avgf_1a > 0 & avgf_1a < 500, ] #delete all rows with avgfare <= 0
dta <- dta[ avgf_2f > 0 & avgf_2f < 500, ] #delete all rows with avgfare <= 0
dta <- dta[ avgf_2r > 0 & avgf_2r < 500, ]
dta <- dta[ avgf_2a > 0 & avgf_2a < 500, ] 
dta <- dta[ jny_2f > 0, ] #delete all rows with jny <= 0
dta <- dta[ jny_2r > 0, ] 
dta <- dta[ jny_2a > 0, ] 

dta <- dta[ gjt_f > 0]
dta <- dta[ gjt_r > 0]

#--------------------------------------
# filtering relevant mkt share
#--------------------------------------
# 
# dta <- dta[ mktshr_1f > 0.03, ]
# dta <- dta[ mktshr_1r > 0.03, ]
# dta <- dta[ mktshr_1a > 0.03, ]
# dta <- dta[ mktshr_2f > 0.03, ]
# dta <- dta[ mktshr_2r > 0.03, ]
# dta <- dta[ mktshr_2a > 0.03, ]

#--------------------------------------
# Selecting SCOTLAND & YORK
#--------------------------------------
dta <- dta[ reg_orig=="SCOTLAND" | reg_orig== "YORK"]
dta <- dta[ reg_dest=="SCOTLAND" | reg_dest== "YORK"]

#---------------------------------------
# Save Data

save(dta, file = "dta_spread_mkt4.RData")
remove(list = ls())

