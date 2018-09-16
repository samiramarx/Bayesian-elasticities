library(plyr)
library(dplyr)

load("dta.Rdata")

#selecting relevant variables
dta<- dta[ , .(crs_ref_bi,rail_year, jny, type, avg_fare, avg_faremile, avg_gva, gjt, class)]

# deleting first class OD
dta1st <- dta[ class == "1", unique(crs_ref_bi)] #%>% length()
#dta[ , unique(crs_ref_bi)] %>% length()

dta2nd <- dta[ !(crs_ref_bi %in% dta1st),]

#deleting OD without advanced tickets
od_fra <- dta2nd[type == "a", unique(crs_ref_bi)]
dta2nd_fra <- dta2nd[ (crs_ref_bi %in% od_fra),]



# 
# dta2nd_fra[ crs_ref_bi=="AGVBPW", ] %>% nrow()
# 
# a <- dta2nd_fra[type == "a", unique(crs_ref_bi)]
# f <- dta2nd_fra[type == "f", unique(crs_ref_bi)] 
# r <- dta2nd_fra[type == "r", unique(crs_ref_bi)]
# 
# a == f
# f == r

foo <- dta2nd_fra[crs_ref_bi %in% c("RDCWVH", "AGVBPW") & rail_year %in% 2001:2003]

foo %>% 
  gather()

# spreading variables - https://stackoverflow.com/questions/42672455/spread-multiple-columns-with-values-by-one-columne-containing-key
value<- c("crs_ref_bi", "railyear", "jny", "avg_fare", "avg_faremile", "avg_gva", "gjt")
key <- "type"

dta2nd_fra_spread <- as.data.frame(dta2nd_fra) %>% 
  gather(key, value, -type) %>% 
  unite(tmp, type, key, sep = ".") %>% 
  group_by(tmp) %>% 
  dplyr::mutate(id = row_number()) %>%
  spread(tmp, value)

# # small example: spreading variables - https://stackoverflow.com/questions/42672455/spread-multiple-columns-with-values-by-one-columne-containing-key
dta <- dta[ , .(avg_fare, type, jny)]
dta <- dta[crs_ref_bi %in% c("RDCWVH", "AGVBPW") & rail_year %in% 2001:2003]

value<- c("avg_fare", "jny")
key <- "type"

df <- as.data.frame(dta) %>%
  gather(key, value, -type) %>%
  unite(tmp, type, key, sep = ".") %>%
  group_by(tmp) %>%
  dplyr::mutate(id = row_number()) %>%
  spread(tmp, value)

#------------------------------------------
# cleaning

dta_2nd_spread[ , unique("a.crs_ref_bi") ] %>% nrow()

$ == dta_2nd_spread$f.crs_ref_bi


stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)
