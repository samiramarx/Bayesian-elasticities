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
setnames(dta, old = c("rev1f", "rev1r", "rev1a", "jny1r", "jny1a"),
              new = c("rev_1f", "rev_1r", "rev_1a", "jny_1r", "jny_1a"))

backup <- copy(dta)
#--------------------------------------
# variables of interest - 19 years of 6184 OD (19*6184 = 117496)
#--------------------------------------

# variables that uniquely identify an observation before melting
index <- c("flow_ref_bi", "crs_ref_bi", "rail_year", "orig", "dest")

# ticket types
tk <- c("1s", # first season (1s)
        "1n", "1f", "1r", "1a", # non season (1n) = full (1f) + reduced (1r) + advanced (1a)
        "2s", # standard season (2s)
        "2f", "2r", "2a")
rev <- paste0("rev_", tk)
jny <- paste0("jny_", tk)

# ticket control variables
ctrl <- c("gjt", "jtim", "ipen", "nint", "sgap", "aml")
tk_ctrl <- paste(rep(ctrl, each = 3), c("s", "f", "r"), sep = "_")

# socieconomic control variables
gva <- grep("gva", names(dta), value = TRUE)
gdi <- grep("gdi", names(dta), value = TRUE)
pop <- grep("population", names(dta), value = TRUE)

econ_ctrl <- c("rpi", "cpi", "gdpdeflator", gva, gdi, pop)

# keep only selected variables
dta <- dta[, c(index, rev, jny, tk_ctrl, econ_ctrl, "o_lf_0carhhold", "lf_carcost", "lf_cartime", "lf_fuelcost", "lf_bustime", "dist"), with = F]

#---------------------------------------
# melting revenue and journey volume - (117496 * 9 = 1057464)
#---------------------------------------
dta <-  melt(dta, 
             measure.vars = list(rev, jny), 
             variable.name = "tk_type", 
             value.name = c("rev", "jny"))

dta[, tk_type := tk[tk_type]]

dta <- separate(dta, 
                col = tk_type, 
                into = c("class", "type"), 
                sep = c(1), 
                remove = FALSE)

#---------------------------------------
# cleaning ticket control variables 
#---------------------------------------
# code from https://stackoverflow.com/questions/28297540/match-row-value-with-column-name-and-extract-value-in-r

dta[, type_lookup_control_variables := type]
dta[type == "a", type_lookup_control_variables := "r"] 
# this causes the control variables _r to be allocated to the ticket types a


for(i in ctrl) {
    df <- as.data.frame(dta[, tk_ctrl, with = F])
    lookup <- paste(i, dta$type_lookup_control_variables, sep = "_")
    values <- df[cbind(seq_len(length(lookup)), 
                       match(lookup, names(df))
                      )
                ]
    dta[[i]] <- values
}

dta[, (tk_ctrl) := NULL] # remove unused variables

#---------------------------------------
# quality control
#---------------------------------------

# total revenue and total volume
backup[, lapply(.SD, function(x) {sum(as.numeric(x))}), .SDcols = c(rev, jny)]
dta[, .(rev = sum(rev), jny = sum(jny)), by = tk_type]

# specific OD
backup[rail_year == 1996 & crs_ref_bi == "YRKLDS", c(index, rev, jny, tk_ctrl, econ_ctrl), with = F]
dta[rail_year == 1996 & crs_ref_bi == "YRKLDS"]

# dta %>% 
#   ggplot(aes(x = price)) +
#   geom_histogram()
# 
# dta %>% 
#   ggplot(aes(x = rail_year, y = price)) +
#   geom_point()
# 
# dta %>% 
#   ggplot(aes(x = as.factor(rail_year), y = price)) +
#   geom_boxplot()

#---------------------------------------
# cleaning
#---------------------------------------
# deleting negative revenue and journey
dta <- dta[ !(dta$rev<0 | dta$jny<0) , ]

# deleting s and n tickets
dta <- dta[ type !="s" & type != "n", ] #!(type %in% c("s", "n"))

# gjt == 0
dta <- dta[gjt!=0] #%>% nrow()

#---------------------------------------
# creating Variables
#---------------------------------------
# revenues corrected by inflation (transforming to 2014 values)
dta$rev_cpi <- dta[ , dta$rev/dta$cpi]

dta[ , .(rev_cpi, rev, cpi)] #checking

# average fares (2014 values)
dta$avg_fare <- dta[ , dta$rev_cpi/dta$jny]

dta[ , .(avg_fare, rev, jny)] #checking

#........ deleting NaN, Inf, and outliers (>501 & =0)
dta <- dta[is.finite(avg_fare) , ] 
dta <- dta[avg_fare < 501 & avg_fare != 0, ] # http://www.dailymail.co.uk/news/article-4109448/Britain-s-expensive-train-fare-revealed-501-ticket-Isle-Wight-Derbyshire-doesn-t-half-country.html

summary(dta$avg_fare)
hist(dta$avg_fare)

# average fares per mile (2014 values)
dta$avg_faremile <- dta[ , dta$avg_fare/dta$dist] 
dta[ is.finite(avg_faremile) , .(avg_fare, rev, jny)] #checking

# average gva by OD
dta$avg_gva <- dta[ , (dta$o_lf_gvacap_nuts3_wp + dta$o_lf_gvacap_nuts3_wp)/2] 


#---------------------------------------
# Save Data

save(dta, file = "dta.RData")
remove(list = ls())

