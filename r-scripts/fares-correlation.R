#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# FARES CORRELATION

#--------------------------------------
# library
#--------------------------------------
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(expss)
library(GGally)


#--------------------------------------
load("dta_spread_mkt1.RData") #2F, 2R
mkt1 <- as.data.table(dta)

load("dta_spread_mkt2.RData") #2F, 2R, 2A
mkt2 <- as.data.table(dta)

load("dta_spread_mkt3_SCOTYORK.RData") #1N, 2F, 2R, 2A
mkt3 <- as.data.table(dta)

load("dta_spread_mkt4.RData") #1F, 1R, 1A, 2F, 2R, 2A
mkt4 <- as.data.table(dta)

rm(dta)
#------------------------------------------
#defining colors for plots
greys <- grep("gr[ea]y", colours(), value = TRUE)
greys <- greys[11:112]
greys_rev <- rev(greys)
#------------------------------------------

fares_mkt1 <- mkt1[ , .(avgf_2f, avgf_2r)]
setnames(fares_mkt1, old = c("avgf_2f", "avgf_2r"),
         new = c("2F", "2R"))
cor_mkt1 <- cor(fares_mkt1)


corrplot(cor_mkt1, method="shade",
          shade.col=NA,
          col = c(greys, greys_rev),
          tl.col="black",
          tl.srt=0, addCoef.col="white",
          tl.cex = 1,
          tl.offset = 1,
          cl.pos="n")

#for matrix plot
mkt1[ , .(avgf_2f, avgf_2r)] %>% ggpairs()

#-----------------------------------------

fares_mkt2 <- mkt2[ , .(avgf_2f, avgf_2r, avgf_2a)]
setnames(fares_mkt2, old = c("avgf_2f", "avgf_2r", "avgf_2a"),
         new = c("2F", "2R", "2A"))
cor <- cor(fares_mkt2)

corrplot(cor, method="shade", 
         shade.col=NA,
         col = c(greys, greys_rev),
         tl.col="black", 
         tl.srt=0, addCoef.col="white", 
         tl.cex = 1,
         tl.offset = 1,
         cl.pos="n")

mkt2[ , .(avgf_2f, avgf_2r, avgf_2a)] %>% ggpairs()

#-----------------------------------------

fares_mkt3 <- mkt3[ , .(avgf1n, avgf_2f, avgf_2r, avgf_2a)]
setnames(fares_mkt3, old = c("avgf1n", "avgf_2f", "avgf_2r", "avgf_2a"),
         new = c("1N", "2F", "2R", "2A"))
cor_mkt3 <- cor(fares_mkt3)

corrplot(cor_mkt3, method="shade", 
         shade.col=NA,
         col = c(greys, greys_rev),
         tl.col="black", 
         tl.srt=0, addCoef.col="white", 
         tl.cex = 1,
         tl.offset = 1,
         cl.pos="n")

mkt3[ , .(avgf1n, avgf_2f, avgf_2r, avgf_2a)] %>% ggpairs()

#-----------------------------------------

fares_mkt4 <- mkt4[ , .(avgf_1f, avgf_1r, avgf_1a, avgf_2f, avgf_2r, avgf_2a)]
setnames(fares_mkt4, old = c("avgf_1f","avgf_1r", "avgf_1a", "avgf_2f", "avgf_2r", "avgf_2a"),
         new = c("1F", "1R", "1A", "2F", "2R", "2A"))
cor_mkt4 <- cor(fares_mkt4)

corrplot(cor_mkt4, method="shade", 
         shade.col=NA,
         col = c(greys, greys_rev),
         tl.col="black", 
         tl.srt=0, addCoef.col="white", 
         tl.cex = 1,
         tl.offset = 1,
         cl.pos="n")

mkt4[ , .(avgf_1f, avgf_1r, avgf_1a, avgf_2f, avgf_2r, avgf_2a)] %>% ggpairs()
