#=========================================
# MSc Transport Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# Regression trials

load(file = "dta.RData")

library(plyr)

# PDFH:
# volume ~ GDP pc , pop, car_own, fuelcost, cartime, buscost, bushead, bustime, aircost, airhead.

# Possible:
# volume ~ GVA, pop, car_own, fuelcost, cartime, bustime

#reducing data set - no distinction of ticket type

dta2 <- ddply(dta, .(crs_ref_bi, rail_year), summarize, 
              rev= as.numeric(sum(rev)), 
              jny= as.numeric(sum(jny)), 
              pop_o = as.numeric(unique(o_population_district)),
              pop_d = as.numeric(unique(d_population_district)),
              gva_o = as.numeric(unique(o_lf_gvacap_nuts3_wp)),
              gva_d = as.numeric(unique(o_lf_gvacap_nuts3_wp)),
              gdi_o = as.numeric(unique(o_lf_gdicap_nuts3_res)),
              gdi_d = as.numeric(unique(d_lf_gdicap_nuts3_res)),
              cpi = as.numeric(unique(cpi)),
              car_own = as.numeric(unique(o_lf_0carhhold)),
              carcost = as.numeric(unique(lf_carcost)),
              cartime = as.numeric(unique(lf_cartime)),
              fuelcost = as.numeric(unique(lf_fuelcost)),
              bustime = as.numeric(unique(lf_bustime))
)

str(dta2)
dta2 <- as.data.table(dta2)

dta2$av_pop <- (dta2$pop_o + dta2$pop_d)/2
dta2$av_gva <- (dta2$gva_o + dta2$gva_d)/2
dta2$av_gdi <- (dta2$gdi_o + dta2$gdi_d)/2

dta2[ crs_ref_bi == "ABACDF", unique(av_pop), by = .(rail_year)]

dta2[, av_pop_lagged := .SD[rail_year == 1996, av_pop], by = "crs_ref_bi"]
dta2[, pop_ratio := av_pop / av_pop_lagged]

dta2[, av_gva_lagged := .SD[rail_year == 1996, av_gva], by = "crs_ref_bi"]
dta2[, gva_ratio := av_gva / av_gva_lagged]

dta2[, av_gdi_lagged := .SD[rail_year == 1996, av_gdi], by = "crs_ref_bi"]
dta2[, gdi_ratio := av_gva / av_gdi_lagged]

dta2[, car_own_lagged := .SD[rail_year == 1996, car_own], by = "crs_ref_bi"]
dta2[, car_own_ratio := car_own - car_own_lagged]

dta2[, fuelcost_lagged := .SD[rail_year == 1996, fuelcost], by = "crs_ref_bi"]
dta2[, fuelcost_ratio := fuelcost / fuelcost_lagged]

dta2[, cartime_lagged := .SD[rail_year == 1996, cartime], by = "crs_ref_bi"]
dta2[, cartime_ratio := cartime / cartime_lagged]

dta2[, bustime_lagged := .SD[rail_year == 1996, bustime], by = "crs_ref_bi"]
dta2[, bustime_ratio := bustime / bustime_lagged]

# foo <- dta2[crs_ref_bi %in% c("ABACDF", "ABDBRI") & rail_year %in% 1996:1998]
# setorderv(foo, cols = c("crs_ref_bi", "rail_year"))
# 
# foo[, av_pop_lagged := .SD[rail_year == 1996, av_pop], by = "crs_ref_bi"]
# foo[, pop_ratio := av_pop / av_pop_lagged]


#----------------------------------------------------
load(file= "dta_2nd.RData")
dta_2nd <- as.data.table(dta_2nd)

library(plm)

reg <- lm(data = dta_2nd, log(jny) ~ log(avg_faremile) + log(avg_gva) + log(gjt))
summary(reg)


#----------------------------------------------------

