#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

library(readr); library(data.table); library(magrittr); library(ggplot2); library(formattable); library(gridExtra)

#----------------------------------------
# subsetting data frame - standard class
#----------------------------------------
load(file= "dta.RData")
dta <- as.data.table(dta)

dta_1st <- dta[ class == "1", unique(crs_ref_bi)] #%>% length()
#dta[ , unique(crs_ref_bi)] %>% length()

dta_2nd <- dta[ !(crs_ref_bi %in% dta_1st),]

save(dta_2nd, file = "dta_2nd.RData")

remove(list=ls())
