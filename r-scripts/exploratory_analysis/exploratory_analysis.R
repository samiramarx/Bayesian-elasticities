
# Exploratory Analysis

library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)

load(file = "dta.RData")

# Additional Variables

dta$OD <- paste(dta$orig, "-", dta$dest, sep='')

# ----------------------------------------
# DF == DR

check_dta <- data.frame(id = 1:nrow(dta))

for(i in 49:68){
  check <- dta[, i] == dta[,i+20]
  check_dta[, i - 47] <- check
}

cbind(49:68, 49:68 + 20, 2:21)

# -----------------------------------------
# Routes and Distance

dta <- data.table(dta)

dist <- dta[ , .(var = unique(miles)), by = .(OD)][order(var)]

qplot(y=dist$var)
qplot(dist$var, geom="histogram")

#Example:
dta[orig=="LDS" & dest=="YRK",]
dta[orig=="LDS",]

# -----------------------------------------
# Population Test

pop2010 <- dta[year==2013,.(var = unique(pop_D)), by = .(orig)][order(var)]
qplot(y=pop2010$var)

pop_00 <- data.frame(dta[ pop_A==0 & pop_B==0, ])
write.csv( pop_00,"exploratory_analysis/pop_00.csv" )

lds <- dta[ orig=="LDS" , .(var = unique(pop_A)), by = .(year)]
write.csv( lds,"exploratory_analysis/lds_pop.csv" )

# ----------------------------------------
# Price Test

tkt <- c("1S", "1F", "1R", "1A", "1N", "2S", "2F", "2R", "2A")

cols_rev <- c(paste("rev", tkt, sep=''))
cols_vol <- c(paste("vol", tkt, sep=''))

dta$avg_price <- apply(dta[, cols_rev, with = FALSE], 1, sum) / apply(dta[, cols_vol, with = FALSE], 1, sum) 
summary(dta$avg_price)
sd(dta$avg_price)/mean(dta$avg_price)
qplot(dta$avg_price, geom="histogram")

dta$avg_price_mile <- dta$avg_price / dta$miles 
summary(dta$avg_price_mile)
sd(dta$avg_price_mile)/mean(dta$avg_price_mile)

qplot(dta$avg_price_mile, geom="histogram")
qplot(dta$avg_price_mile, geom="box-plot")
ggplot(dta, aes(y=avg_price_km, x=factor(year))) + geom_jitter()

dta[, median(avg_price_mile), by = year]

#Example:

dta[ orig=="LDS" & dest=="YRK", avg_price] %>% plot

dta[ orig=="LDS" & dest=="YRK", avg_price_km] %>% plot

# ----------------------------------------
# Inflation correction

inf_cpi <- dta[ , .(var = unique(CPI)), by = .(year)][order(var)]

inf_rpi <- dta[ , .(var1 = unique(rpi)), by = .(year)][order(var1)]

inflation <- cbind(inf_cpi, inf_rpi$var1)

ggplot(inflation, aes(year)) + 
  geom_line(aes(y = var, colour = "CPI")) + 
  geom_line(aes(y = V2, colour = "RPI"))

# CPI Correction
dta$avg_price_inf <- dta$avg_price*dta$CPI
dta$avg_price_km_inf <- dta$avg_price_km*dta$CPI

summary(dta$avg_price_km_inf)
plot1 <- ggplot(dta, aes(y=avg_price_km, x=factor(year))) + geom_boxplot()
plot2 <- ggplot(dta, aes(y=avg_price_km_inf, x=factor(year))) + geom_boxplot()

grid.arrange(plot1, plot2, ncol=2)


# ----------------------------------------
# Disposable Income

dta[orig=="ABD",.(var = unique(Dinc_O)), by = .(orig)][order(var)]

Dinc_00 <- dta[Dinc_A==0 & Dinc_B==0,]
write.csv( Dinc_00, "exploratory_analysis/Dinc_00.csv")

dta[orig=="LDS" & year==1998,.(var = unique(Dinc_O)), by = .(year, orig, dest)][order(var)]

# ----------------------------------------
# GDP

gdp_test <- dta[ ,.(var = unique(nomGDP)), by = .(year)][order(var)]

dta[ year==1995 ,.(var = unique(nomGDP)), by = .(orig)][order(var)]

# ----------------------------------------
# Stations

orig_stat <- dta[ ,unique(orig), ]
dest_stat <- dta[ ,unique(dest), ]
stations <- data.frame(stations = union(orig_stat,dest_stat))

write.csv(stations, "stations.csv")

dta[grepl("^X", orig) , ,  ]
