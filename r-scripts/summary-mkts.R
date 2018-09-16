#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# SUMMARY OF MKTS

#--------------------------------------
# library
#--------------------------------------
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)

#--------------------------------------
load("dta_spread_mkt1.RData") #2F, 2R
mkt1 <- as.data.table(dta)

load("dta_spread_mkt2.RData") #2F, 2R, 2A
mkt2 <- as.data.table(dta)

load("dta_spread_mkt3_SCOTYORK.RData") #1N, 2F, 2R, 2A
mkt3 <- as.data.table(dta)

load("dta_spread_mkt4.RData") #1F, 1R, 1A, 2F, 2R, 2A SCOTYORK
mkt4 <- as.data.table(dta)

rm(dta)
#------------------------------------------

mkt4[ , unique(crs_ref_bi)] %>% length()
od_mkt4 <- mkt4[ , .N, by = .(rail_year)]
od_mkt4$id <- "4"
od_mkt4[order(rail_year)]

mkt2[ , unique(crs_ref_bi)] %>% length()
od_mkt2 <- mkt2[ , .N, by = .(rail_year)] 
od_mkt2$id <- "2"
od_mkt2[order(rail_year)]

mkt3[ , unique(crs_ref_bi)] %>% length()
od_mkt3 <- mkt3[ , .N, by = .(rail_year)] 
od_mkt3$id <- "3"
od_mkt3[order(rail_year)]

mkt1[ , unique(crs_ref_bi)] %>% length()
od_mkt1 <- mkt1[ , .N, by = .(rail_year)] 
od_mkt1$id <- "1"
od_mkt1[order(rail_year)]

od_mkt <- rbind(od_mkt1, od_mkt2, od_mkt3, od_mkt4)

od_mkt_plot <-
  od_mkt %>%
  ggplot(aes(y=N, x = rail_year, colour = id)) + 
  geom_line() +
  geom_point() +
  scale_colour_grey() +
  ylab("Quantity of OD pairs") + xlab("Year") +
  scale_x_continuous(name="Year", breaks = c(1996:2014), limits=c(1996, 2014),
                       labels=c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))
  od_mkt_plot$labels$colour <- "Market"
  
ggsave("plots/od_mkt.pdf", plot=od_mkt_plot)

