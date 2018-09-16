#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# MARKET SHARE OF TICKETS

#--------------------------------------
# library
#--------------------------------------
library(data.table)
library(quantreg)
library(nnet)
library(mgcv)
library(systemfit)
library(stargazer)
library(texreg)
library(magrittr)

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

#MKT 1

# hist(mkt1$mktshr_2f)
# mkt1[ mktshr_2f < 0.01 , ] %>% nrow()

mktshr_2f <- mkt1[ , sum(jny_2f)/(sum(jny_2f)+sum(jny_2r)), by=(rail_year)]
mktshr_2f$V1 <- paste(round(mktshr_2f$V1*100, 0), "%",sep="")

mktshr_2r <- mkt1[ , sum(jny_2r)/(sum(jny_2f)+sum(jny_2r)), by=(rail_year)]
mktshr_2r$V1 <- paste(round(mktshr_2r$V1*100, 0), "%",sep="")

mkt1_mktshr <- cbind(mktshr_2f, mktshr_2r$V1)
names(mkt1_mktshr) <- c("year", "mktshr2f", "mktshr2r")

mkt1_mktshr <- mkt1_mktshr[order(rank(year), mktshr2f)]

# mkt1_mktshr = setNames(data.frame(t(mkt1_mktshr[,-1])), as.character(mkt1_mktshr$year/100))

#MKT 2

mktshr_2f <- mkt2[ , sum(jny_2f)/(sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_2f$V1 <- paste(round(mktshr_2f$V1*100, 0), "%",sep="")

mktshr_2r <- mkt2[ , sum(jny_2r)/(sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_2r$V1 <- paste(round(mktshr_2r$V1*100, 0), "%",sep="")

mktshr_2a <- mkt2[ , sum(jny_2a)/(sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_2a$V1 <- paste(round(mktshr_2a$V1*100, 0), "%",sep="")

mkt2_mktshr <- cbind(mktshr_2f, mktshr_2r$V1, mktshr_2a$V1)
names(mkt2_mktshr) <- c("year", "mktshr2f", "mktshr2r", "mktshr2a")

mkt2_mktshr <- mkt2_mktshr[order(rank(year), mktshr2f)]

#MKT 3 - SCOTYORK

mktshr_1n <- mkt3[ , sum(jny1n)/(sum(jny1n)+sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_1n$V1 <- paste(round(mktshr_1n$V1*100, 0), "%",sep="")

mktshr_2f <- mkt3[ , sum(jny_2f)/(sum(jny1n)+sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_2f$V1 <- paste(round(mktshr_2f$V1*100, 0), "%",sep="")

mktshr_2r <- mkt3[ , sum(jny_2r)/(sum(jny1n)+sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_2r$V1 <- paste(round(mktshr_2r$V1*100, 0), "%",sep="")

mktshr_2a <- mkt3[ , sum(jny_2a)/(sum(jny1n)+sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_2a$V1 <- paste(round(mktshr_2a$V1*100, 0), "%",sep="")

mkt3_mktshr <- cbind(mktshr_1n, mktshr_2f$V1, mktshr_2r$V1, mktshr_2a$V1)
names(mkt3_mktshr) <- c("year", "mktshr1n", "mktshr2f", "mktshr2r", "mktshr2a")

mkt3_mktshr <- mkt3_mktshr[order(rank(year), mktshr1n)]

#MKT 4

mktshr_1f <- mkt4[ , sum(jny_1f)/(sum(jny_1f)+sum(jny_1r)+sum(jny_1a)+sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_1f$V1 <- paste(round(mktshr_1f$V1*100, 0), "%",sep="")

mktshr_1r <- mkt4[ , sum(jny_1r)/(sum(jny_1f)+sum(jny_1r)+sum(jny_1a)+sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_1r$V1 <- paste(round(mktshr_1r$V1*100, 0), "%",sep="")

mktshr_1a <- mkt4[ , sum(jny_1a)/(sum(jny_1f)+sum(jny_1r)+sum(jny_1a)+sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_1a$V1 <- paste(round(mktshr_1a$V1*100, 0), "%",sep="")

mktshr_2f <- mkt4[ , sum(jny_2f)/(sum(jny_1f)+sum(jny_1r)+sum(jny_1a)+sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_2f$V1 <- paste(round(mktshr_2f$V1*100, 0), "%",sep="")

mktshr_2r <- mkt4[ , sum(jny_2r)/(sum(jny_1f)+sum(jny_1r)+sum(jny_1a)+sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_2r$V1 <- paste(round(mktshr_2r$V1*100, 0), "%",sep="")

mktshr_2a <- mkt4[ , sum(jny_2a)/(sum(jny_1f)+sum(jny_1r)+sum(jny_1a)+sum(jny_2f)+sum(jny_2r)+sum(jny_2a)), by=(rail_year)]
mktshr_2a$V1 <- paste(round(mktshr_2a$V1*100, 0), "%",sep="")

mkt4_mktshr <- cbind(mktshr_1f, mktshr_1r$V1, mktshr_1a$V1, mktshr_2f$V1, mktshr_2r$V1, mktshr_2a$V1)
names(mkt4_mktshr) <- c("year", "mktshr1f", "mktshr1r", "mktshr1a", "mktshr2f", "mktshr2r", "mktshr2a")

mkt4_mktshr <- mkt4_mktshr[order(rank(year), mktshr1f)]



# EXPORTING TO LATEX ==============================================

xtable:: xtable(mkt1_mktshr)
xtable:: xtable(mkt2_mktshr)
xtable:: xtable(mkt3_mktshr)
xtable:: xtable(mkt4_mktshr)
