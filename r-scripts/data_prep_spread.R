#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

#-----------------------------------------
# library
#--------------------------------------
library(data.table); library(tidyr); library(dplyr)
#--------------------------------------
# raw data
#--------------------------------------
dta <- fread("raw_data/bi_nonlondon_long/bi_nonlondon_long.csv")
setnames(dta, old = c("rev1f", "rev1r", "rev1a", 
                      "jny1r", "jny1a", 
                      "o_lf_gvacap_nuts3_wp", "d_lf_gvacap_nuts3_wp", 
                      "o_lf_gdicap_nuts3_res", "d_lf_gdicap_nuts3_res", 
                      "o_population_district", "d_population_district"),
              new = c("rev_1f", "rev_1r", "rev_1a", 
                      "jny_1r", "jny_1a", 
                      "gva_o", "gva_d", 
                      "gdi_o", "gdi_d", 
                      "pop_o", "pop_d"))

backup <- copy(dta)

#--------------------------------------
# adding regions
#--------------------------------------
load("raw_data/raw_datawithregion/dta2.RData")
dta2 <- as.data.table(dta2)

# dta2[ , unique(v2)] %>% length()
reg_orig <- dta2[ , unique(v2), by=.(v8)]
names(reg_orig) <- c("reg_orig", "orig")

# dta2[ , unique(v3)] %>% length()
reg_dest <- dta2[ , unique(v3), by=.(v9)]
names(reg_dest) <- c("reg_dest", "dest")

dta <- left_join(dta, reg_orig, by = "orig")
dta <- left_join(dta, reg_dest, by = "dest")
dta <- as.data.table(dta)

#--------------------------------------
# variables of interest - crs_ref_bi, rail_year, rev, jny, gva, gjt
#--------------------------------------

rev <- select(dta, starts_with("rev")) %>% colnames()
jny <- select(dta, starts_with("jny")) %>% colnames()
gva <- select(dta, starts_with("gva")) %>% colnames()
gjt <- select(dta, starts_with("gjt")) %>% colnames()

dta <- dta[ , c("crs_ref_bi", 
                "rail_year", 
                rev, 
                jny, 
                gva, 
                gjt, 
                "rpi",
                "dist", 
                "pop_o",
                "pop_d",
                "gdpdeflator",
                "orig", "reg_orig",
                "dest", "reg_dest"), with = FALSE]

#--------------------------------------
# cleaning unused variables
#--------------------------------------
dta$rev_1n <- NULL
dta$rev_1s <- NULL
dta$rev_2s <- NULL

dta$jny_1n <- NULL
dta$jny_1s <- NULL
dta$jny_2s <- NULL

#--------------------------------------
# creating variables
#--------------------------------------
# average gva deflated
dta$avg_gva <- dta[ , ((dta$gva_o + dta$gva_d)/2)/dta$gdpdeflator] 

# average pop by OD
dta$avg_pop <- dta[ , (dta$pop_o + dta$pop_d)/2] 

# revenues corrected by inflation (transforming to 2014 values)
class <- c("1", "2")
type <- c("f", "r", "a") 

tk <- paste0(rep(class, each = 3), type)

for(i in tk) {
  revtk <- paste0("rev_", i)
  revtk_rpi <- paste0("rev_", i, "_rpi")
  dta[, (revtk_rpi) := dta[[revtk]] / rpi]
}

# average fares (2014 values)
for(i in tk) {
  revtk_rpi <- paste0("rev_", i, "_rpi")
  avgfare <- paste0("avgf_", i)
  jnytk <- paste0("jny_",i)
  dta[, (avgfare) := dta[[revtk_rpi]] / dta[[jnytk]] ]
}

# average fares per mile (2014 values)
for(i in tk) {
  avgfare_ml <- paste0("avgf_", i, "_mile")
  avgfare <- paste0("avgf_", i)
  dta[, (avgfare_ml) := dta[[avgfare]] / dist ]
}

# mkt share of each kind of ticket in each OD pair
for(i in tk) {
  mktshr <- paste0("mktshr_", i)
  jnytk <- paste0("jny_",i)
  dta[, (mktshr) := dta[[jnytk]]/(dta$jny_1f + dta$jny_1r + dta$jny_1a + dta$jny_2f + dta$jny_2r + dta$jny_2a)]
}

#--------------------------------------
# Negative values of rev and jny 
#--------------------------------------

dta <- dta[ rev_1f>= 0, ] #delete all rows with avgfare <= 0 & >500
dta <- dta[ rev_1r>= 0, ]
dta <- dta[ rev_1a>= 0, ]
dta <- dta[ rev_2f>= 0, ]
dta <- dta[ rev_2r>= 0, ]
dta <- dta[ rev_2a>= 0, ] 

dta <- dta[ jny_1f>= 0, ] #delete all rows with jny <= 0
dta <- dta[ jny_1r>= 0, ]
dta <- dta[ jny_1a>= 0, ]
dta <- dta[ jny_2f>= 0, ]
dta <- dta[ jny_2r>= 0, ]
dta <- dta[ jny_2a>= 0, ] 

#--------------------------------------
# Outliers avgf>500
#--------------------------------------

dta <- dta[ avgf_1f < 500 | !is.finite(avgf_1f), ] 
dta <- dta[ avgf_1r < 500 | !is.finite(avgf_1r), ]
dta <- dta[ avgf_1a < 500 | !is.finite(avgf_1a), ]
dta <- dta[ avgf_2f < 500 | !is.finite(avgf_2f), ]
dta <- dta[ avgf_2r < 500 | !is.finite(avgf_2r), ]
dta <- dta[ avgf_2a < 500 | !is.finite(avgf_2a), ] 

#--------------------------------------
# Outliers avgf_ml>5
#--------------------------------------

dta <- dta[ avgf_1f_mile < 3 | !is.finite(avgf_1f), ] 
dta <- dta[ avgf_1r_mile < 3 | !is.finite(avgf_1r), ]
dta <- dta[ avgf_1a_mile < 3 | !is.finite(avgf_1a), ]
dta <- dta[ avgf_2f_mile < 3 | !is.finite(avgf_2f), ]
dta <- dta[ avgf_2r_mile < 3 | !is.finite(avgf_2r), ]
dta <- dta[ avgf_2a_mile < 3 | !is.finite(avgf_2a), ]

#---------------------------------------
# Save Data

save(dta, file = "dta_spread.RData")
remove(list = ls())
