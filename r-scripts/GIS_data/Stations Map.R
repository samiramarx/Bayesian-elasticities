# Stations Map

library(OpenStreetMap)
library(ggplot2)
library(ggmap)
library(tmap)
library(leaflet)
library(readxl)

#--------------------------
#Getting the station names

load("dta_spread.RData")
# unique(dta$crs_ref_bi) %>% length()

dta$orig <- substr(dta$crs_ref_bi, start = 1, stop = 3)
dta$dest <- substr(dta$crs_ref_bi, start = 4, stop = 6)

stations <- unique(unique(dta$orig), unique(dta$dest))

geoinf <- read_excel("GIS_data/GB_Railway_Stations.xls", sheet = 1)
                    
geoinf <- data.table::as.data.table(geoinf)
stat <- subset(geoinf, code %in% stations)

#--------------------------
# Expanding the grid

e_bound <- c("-", "east_bound", "-", "1", "49.7", "-17.6")
w_bound <- c("-", "west_bound", "-", "1", "59.7", "8.7")
stat <- as.data.frame(stat)
stat <- data.frame(rbind(stat, e_bound,w_bound))
stat <- data.table::as.data.table(stat)

stat$lat <- as.numeric(stat$lat)
stat$long <- as.numeric(stat$long)

#--------------------------
# Plotting
stat_map <- qmplot(data=stat, long, lat, extent = "device", colour = I( "#CC0000"), zoom=7)

ggsave("plots/stat-map.pdf", plot = stat_map)

#==============================================================================
# BRAZIL
BH <- c(-19.812991, -43.929930)
Contagem <- c(-19.893928, -43.946590)
Contagem2 <- c(-19.883632, -44.110304)

brazil <- data.frame(rbind(BH, Contagem, Contagem2))
str(brazil)

brazil$X1 <- as.numeric(brazil$X1)
brazil$X2 <- as.numeric(brazil$X2)

qmplot(data = brazil, x=X2, y=X1)
