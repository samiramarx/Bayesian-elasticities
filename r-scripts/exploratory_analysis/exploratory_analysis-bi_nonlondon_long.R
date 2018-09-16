
# Exploratory Analysis

library(dplyr)
library(plyr)
library(ggplot2)
library(data.table)
library(gridExtra)

load(file = "dta.RData")

dta <- data.table(dta)

# Year
dta[ , unique(rail_year)]

dta[, .N, by = rail_year]

# OD pairs
dta[, .N, by = crs_ref_bi][,unique(N)]

# Stations
unique(c(dta[, unique(orig),], dta[, unique(dest),]))

# GVA & GDI
nrow(dta[, unique(o_lf_gvacap_nuts3_wp), by = .(orig, rail_year)]) /length(unique(dta$rail_year)) == length(unique(dta$orig))
nrow(dta[, unique(d_lf_gvacap_nuts3_wp), by = .(dest, rail_year)]) /length(unique(dta$rail_year)) == length(unique(dta$dest))
nrow(dta[, unique(o_lf_gdicap_nuts3_res), by = .(orig, rail_year)]) /length(unique(dta$rail_year)) == length(unique(dta$orig))
nrow(dta[, unique(d_lf_gdicap_nuts3_res), by = .(dest, rail_year)]) /length(unique(dta$rail_year)) == length(unique(dta$dest))

# Population
nrow(dta[, unique(o_population_district), by = .(orig, rail_year)]) /length(unique(dta$rail_year)) == length(unique(dta$orig))
nrow(dta[, unique(d_population_district), by = .(dest, rail_year)]) /length(unique(dta$rail_year)) == length(unique(dta$dest))

diff <- dta[ , (o_age_0_14_district+
       o_age_15_29_district+
       o_age_30_44_district+
       o_age_45_64_district+
       o_age_65plus_district) - o_population_district] 

summary(diff)

# dta[ , .(o_age_0_14_district,
#          o_age_15_29_district,
#          o_age_30_44_district,
#          o_age_45_64_district,
#          o_age_65plus_district, o_population_district)] 

dta[ , (d_age_0_14_district+
          d_age_15_29_district+
          d_age_30_44_district+
          d_age_45_64_district+
          d_age_65plus_district) - d_population_district] 

# Employment
dta[ , (o_lf_emp_res_occ_1_dist+
          o_lf_emp_res_occ_2_dist+
          o_lf_emp_res_occ_3_dist+
          o_lf_emp_res_occ_4_dist+
          o_lf_emp_res_occ_5_dist+
          o_lf_emp_res_occ_6_dist+
          o_lf_emp_res_occ_7_dist+
          o_lf_emp_res_occ_8_dist+
          o_lf_emp_res_occ_9_dist) - o_lf_emp_res_dist] 

# Routes and Distance

dist <- dta[ , .(var = unique(dist)), by = .(crs_ref_bi)][order(var)]

qplot(y=dist$var)
qplot(dist$var, geom="histogram")

#Example:
dta[orig=="LDS" & dest=="YRK", ]
dta[orig=="YRK", unique(dest)]

# Inflation

inf_cpi <- dta[ , .(var = unique(cpi)), by = .(rail_year)][order(var)]

inf_rpi <- dta[ , .(var1 = unique(rpi)), by = .(rail_year)][order(var1)]

inflation <- cbind(inf_cpi, inf_rpi$var1)

ggplot(inflation, aes(rail_year)) + 
  geom_line(aes(y = var, colour = "CPI")) + 
  geom_line(aes(y = V2, colour = "RPI"))

dta$avg_price_inf <- dta$avg_price*dta$CPI
dta$avg_price_km_inf <- dta$avg_price_km*dta$CPI

# Ticket Prices

tkt <- c("1f", "1r", "1a", "1s", "1n", "2f", "2r", "2a", "2s")


for ( i in tkt ) {
  col <- paste("avg_price_", i, sep = '')
  cols_rev <- paste("rev_", i, sep='')
  cols_jny <- paste("jny_", i, sep='')
  dta[[col]] <- dta[[cols_rev]]/dta[[cols_jny]]
}

for ( i in tkt ) {
  col <- paste("avg_price_", i, sep = '')
  print(all(dta[[col]]>0))
  
}

dta[ avg_price_1f<0, .(rev_1f, jny_1f, avg_price_1f)]
dta[ avg_price_1r<0, .(rev_1r, jny_1r, avg_price_1r)]
dta[ avg_price_1a<0, .(rev_1a, jny_1a, avg_price_1a)]
dta[ avg_price_1s<0, .(rev_1s, jny_1s, avg_price_1s)]
dta[ avg_price_1n<0, .(rev_1n, jny_1n, avg_price_1n)]


