#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

#-----------------------------------------
# Library

library(readr); 
library(data.table); 
library(magrittr); 
library(ggplot2); 
library(formattable); 
library(gridExtra)
library(ggjoy); 
library(tidyverse)

#-----------------------------------------
# Data
load("dta_spread.RData")
dta <- as.data.table(dta)
dta$rail_year <- as.factor(dta$rail_year)

#-----------------------------------------
#FARES
#Adjusting data
class <- c("1", "2")
type <- c("f", "r", "a") 
tk <- paste0(rep(class, each = 3), type)

avgf <- paste0("avgf_", tk)
avgf_ml <- paste0("avgf_", tk, "_mile")

dta_sub <- dta[ , c("rail_year", avgf, avgf_ml), with = FALSE]

dta_sub <-  melt(dta_sub, 
             measure.vars = list(avgf, avgf_ml), 
             variable.name = "tk_type", 
             variable.factor = FALSE,
             value.name = c("avgf", "avgf_ml"))

dta_sub <- as.data.table(dta_sub)

dta_sub[ tk_type == 1, tk_type := "1f"]
dta_sub[ tk_type == 2, tk_type := "1r"]
dta_sub[ tk_type == 3, tk_type := "1a"]
dta_sub[ tk_type == 4, tk_type := "2f"]
dta_sub[ tk_type == 5, tk_type := "2r"]
dta_sub[ tk_type == 6, tk_type := "2a"]

summary(dta$avgf_1r)
dta_sub[ tk_type == "1r", avgf] %>% summary()
dta_sub$tk_type <- factor(dta_sub$tk_type, levels=c("1f", "1r", "1a", "2f", "2r", "2a"))

bp_fares <- dta_sub[ is.finite(avgf_ml) & avgf_ml < 1.5] %>% 
ggplot(aes(y=avgf_ml, x=rail_year)) + geom_boxplot() + 
  ylab("Average fare per mile (£/mi)") + xlab("Year") +
  scale_x_discrete(labels=c("96","97","98", "99", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14")) +
  facet_wrap(~ tk_type, ncol=3)

ggsave("plots/bp_fares.pdf", plot=bp_fares)

# ggplot(aes(y = avgf_ml, x = rail_year, fill = tk_type), data = dta_sub) + 
#   geom_boxplot()

# dta_sub[ is.finite(avgf_ml) & avgf_ml < 1.5] %>% 
#   ggplot(aes(x = avgf_ml, y = factor(rail_year, 2014:1996))) + 
#   geom_joy(scale = 2) + facet_wrap(~tk_type, ncol = 3)

#-----------------------------------------
#JOURNEYS
#Adjusting data

dta$jny <- as.numeric(dta$jny_1f + dta$jny_1r + dta$jny_1a + dta$jny_2f + dta$jny_2r + dta$rev_2a)

dta_sub2 <- dta[ , .(crs_ref_bi, rail_year, jny, dist, avg_gva)]
dta_sub2 <- as.data.table(dta_sub2)
dta_sub2$rail_year <- as.numeric(dta_sub2$rail_year)

jny_growth_agg <- 
dta_sub2[ , sum(jny)/1000000, by=.(rail_year)] %>% 
ggplot( aes(x=rail_year, y=V1)) +
  geom_line() +
  geom_point()+
  xlab("Year") + ylab("Volume of Journeys - Millions")+
  scale_x_continuous(name="Year", breaks = c(1:19), limits=c(1, 19),
  labels=c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))

ggsave("plots/jny_growth_agg.pdf", plot = jny_growth_agg)

dta_sub2[, dist_levels := cut(dist, c(15, 50, 100, 200, 700))]
vol_jny <- dta_sub2[, sum(jny)/1000000, by = .(dist_levels, rail_year)]
vol_jny$rail_year <- as.numeric(as.character(vol_jny$rail_year))

levels(vol_jny$dist_levels)[levels(vol_jny$dist_levels)=="(15,50]"] <- "20 - 50"
levels(vol_jny$dist_levels)[levels(vol_jny$dist_levels)=="(50,100]"] <- "50 - 100"
levels(vol_jny$dist_levels)[levels(vol_jny$dist_levels)=="(100,200]"] <- "100 - 200"
levels(vol_jny$dist_levels)[levels(vol_jny$dist_levels)=="(200,700]"] <- "> 200"
names(vol_jny)[names(vol_jny)=="dist_levels"]  <- "Distance"

jny_growth <- ggplot(vol_jny, aes(x=rail_year, y=V1, colour= Distance)) +
    geom_line() +
    geom_point()+
    scale_colour_grey()+
    xlab("Year") + ylab("Volume of Journeys - Millions")+
    theme(legend.position=c(0.15,0.80))
p$labels$colour <- "Distance (mi)"

ggsave("plots/jny_growth.pdf", plot = jny_growth)

dist_dt <- dta[ , unique(dist), by = .(crs_ref_bi)]

jny_hist <- ggplot(dist_dt, aes(x=V1)) + 
  geom_histogram(binwidth = 20) + 
  ylab("Quantity of OD Pairs") +
  xlab("OD Pairs Distances (mi)") +
  annotate("text", x=630, y=50, label="PLY-ABD", size = 2.5) +
  annotate("text", x=680, y=25, label="PLY-INV", size = 2.5)

ggsave("plots/jny_hist.pdf", plot = jny_hist)

grid.arrange(p, h, ncol=2)

#-----------------------------------------
#GVA

dta_sub2[, gva_levels := cut(avg_gva/1000, 4)]
dta_sub2[ , nof_od := .N, by = .(gva_levels, rail_year) ]

vol_jny_gva_r <- dta_sub2[, (sum(jny)/1000) / nof_od, by = .(gva_levels, rail_year)]
vol_jny_gva_r$rail_year <- as.numeric(as.character(vol_jny_gva_r$rail_year))

dta_sub2$rail_year<- as.numeric(dta_sub2$rail_year)

od <-
dta_sub2[ , unique(nof_od), by = .(rail_year, gva_levels)] %>%
  ggplot(aes(x=rail_year, y=V1, fill = gva_levels, colour= gva_levels)) +
  geom_area() +
  scale_colour_grey()+
  scale_fill_grey()+
  xlab("Year") + ylab("Quantity of OD pairs")+
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  theme(legend.position= "none") +
  scale_x_continuous(name="Year", breaks = c(5, 10, 15), labels = c(2000, 2005, 2010))
                    # labels=c("96", "97", "98", "99", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14"))  

ggsave("plots/od.pdf", plot = od)

  
gva_jny <- vol_jny_gva_r %>% 
ggplot(aes(x=rail_year, y=V1, colour= gva_levels)) +
  geom_line() +
  geom_point()+
  scale_colour_grey()+
  xlab("Year") + ylab("Average Volume of Journeys in ODs - Thousands")+
  theme(legend.position=c(0.15,0.80))
gva_jny$labels$colour <- "GVA (billions)"

ggsave("plots/gva_jny.pdf", plot = gva_jny)

dta_sub2$rail_year <- factor(dta_sub2$rail_year, levels = c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))

dta_sub2 %>% str()

gva_od <- dta_sub2%>%
  ggplot(aes(x = (avg_gva/1000), y = factor(rail_year, 2014:1996), fill = factor(gva_levels))) +
  geom_joy(scale = 2) +
  scale_fill_grey()+
  xlab("OD's GVA (billions)") +
  ylab("")+ 
  # theme(legend.position=c(0.85,0.80))
  labs(fill = "GVA (billions)")
  
ggsave("plots/gva_od.pdf", plot = gva_od)

bp_gva <- dta_sub2%>%
  ggplot(aes(x = rail_year, y = avg_gva)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("OD's GVA (millions)")+
  scale_x_discrete(labels=c("96","97","98", "99", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14"))

# dta_sub2[ , .N, by = .(rail_year)]
dta_sub2$rail_year<- as.numeric(dta_sub2$rail_year)
od_year <- 
  dta_sub2[ , .N, by = .(rail_year)] %>% 
  ggplot( aes(x=rail_year, y=N)) +
  geom_line() +
  geom_point()+
  xlab("Year") + ylab("Volume of Journeys - Millions")
  scale_x_continuous(name="Year", breaks = c(1:19), limits=c(1, 19),
                     labels=c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))


dta_sub2$rail_year<- as.numeric(dta_sub2$rail_year)
od_pergva <- 
    dta_sub2[ , .N, by = .(rail_year)] %>% 
    ggplot( aes(x=rail_year, y=N)) +
    geom_line() +
    geom_point()+
    xlab("Year") + ylab("Volume of Journeys - Millions")
  scale_x_continuous(name="Year", breaks = c(1:19), limits=c(1, 19),
                     labels=c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))

dta_sub2 %>% names
  
  grid.arrange(gva_jny, gva_od, ncol=2)

#-----------------------------------------
#GJT
#arranging data

dta_sub3 <- dta[ , .(rail_year, gjt_f, crs_ref_bi), ]
dta_sub3 <- dta_sub3[ gjt_f > 1,  ]

od <- dta_sub3[ rail_year == 1996, unique(crs_ref_bi)]
dta_sub3 <- subset(dta_sub3, crs_ref_bi %in% od)

dta_sub3[, red_gjt := gjt_f/.SD[rail_year==1996, gjt_f], by = crs_ref_bi]

str(dta_sub3)

avred_gjt <- dta_sub3[ , mean(red_gjt), by = rail_year]

avred_gjt$V2 <- 1-avred_gjt$V1

gjt <- avred_gjt %>% 
  ggplot(aes(x=rail_year, y=V2)) +
  geom_line() +
  geom_point()+
  scale_colour_grey()+
  xlab("Year") + ylab("Percentual reduction")+
  annotate("text", x=2014, y=0.07, label="7.3%", size = 3)

ggsave("plots/gjt_years.pdf", plot = gjt)


gjt_hist <- ggplot(dta_sub3, aes(x=gjt_f)) + 
  geom_histogram(binwidth = 60) + 
  ylab("Quantity of OD Pairs") +
  xlab("GJT (min)")
  # annotate("text", x=630, y=50, label="PLY-ABD", size = 2.5) +
  # annotate("text", x=680, y=25, label="PLY-INV", size = 2.5)

ggsave("plots/gjt_hist.pdf", plot = gjt_hist)




# #Growth in revenue and ticket volume (per year)
# 
# 
# dta[ , 
#      .(jny = sum(jny)/1000000, rev = accounting(sum(rev_cpi)/1000000)), 
#      by = rail_year] %>% 
#   melt(id.var = "rail_year") %>% 
#   ggplot(aes(x=rail_year, y=value, color=variable)) + geom_point()
# 
# 
# #Average fare and average fare per mile (per year)
# 
# dta[avg_faremile < quantile(avg_faremile, probs=(0.75)) & 
#       avg_faremile > quantile(avg_faremile, probs=(0.25)), ] %>% 
#   ggplot(aes(y=avg_faremile, x=rail_year)) + geom_boxplot() + ylab("Fare/Mile") + xlab("") + facet_wrap(~ tk_type, ncol=3)
# 
# summary(dta$avg_faremile)
# 
# #Histogram distance
# 
# dist <- dta[ , unique(dist), by=.(crs_ref_bi)]
# 
# ggplot(dist, aes(x=V1)) + geom_histogram(binwidth = 4) + 
#   ylab("") +
#   xlab("Journey Distance - miles") 
# 
# 
# #Histogram generalised journey time
# 
# gjt_f <- ggplot(dta, aes(x=gjt_f)) + geom_histogram()
# gjt_r <- ggplot(dta, aes(x=gjt_r)) + geom_histogram()
# gjt_s <- ggplot(dta, aes(x=gjt_s)) + geom_histogram()
# grid.arrange(gjt_f, gjt_r, gjt_s, ncol=1)
# 
# 
# dta[ , .(gjt_f, gjt_r, gjt_s)]
# 
# ggplot(faithful, aes(x=waiting)) +
#   geom_density(fill="blue", colour=NA, alpha=.2) +
#   geom_line(stat="density") +
#   xlim(35, 105)
# 
# names(dta)
# 
# # Outros
# df <- dta[jny < quantile(jny, probs=(0.75)) & 
#           jny > quantile(jny, probs=(0.25)), ] %>% 
#         .[avg_faremile < quantile(avg_faremile, probs=(0.75)) & 
#           avg_faremile > quantile(avg_faremile, probs=(0.25)), ] %>% 
#   .[crs_ref_bi=="YRKLDS", ]
#   
# df[ , .(rail_year,rev,avg_faremile, jny, tk_type)]
# 
# ggplot(df, aes(x=avg_faremile, y=jny)) + geom_point(alpha=0.3) + facet_wrap(~ tk_type, ncol=3)
# 
# ggplot(df[tk_type=="2a",], aes(x=avg_faremile, y=jny)) + geom_point(alpha=0.3) + facet_wrap(~ rail_year, ncol=3)
# 
