library(HDInterval)
library(magrittr)
library(purrr)
library(dplyr)
library(data.table)

load("bayes_simulation_mkt3_v1n_T_SCOTYORK.RData")
mkt3_v1n_hdi <- make_hdi(mkt3_v1n)
rm(mkt3_v1n)

load("bayes_simulation_mkt3_v2f_T_SCOTYORK.RData")
mkt3_v2f_hdi <- make_hdi(mkt3_v2f)
rm(mkt3_v2f)

load("bayes_simulation_mkt3_v2r_T_SCOTYORK.RData")
mkt3_v2r_hdi <- make_hdi(mkt3_v2r)
rm(mkt3_v2r)

load("bayes_simulation_mkt3_v2a_T_SCOTYORK.RData")
mkt3_v2a_hdi <- make_hdi(mkt3_v2a)
rm(mkt3_v2a)

cbind(mkt3_v1n_hdi, mkt3_v2f_hdi[2:4], mkt3_v2r_hdi[2:4], mkt3_v2a_hdi[2:4]) %>% 
  xtable(digits = 2)


make_hdi <- function(model) {
  drawns <- rstan::extract(model, permuted = F, pars = c("beta")) %>% 
    # Stack the chains on top of one another and drop the chains label
    plyr::adply(2) %>% 
    dplyr::select(-chains)
  
  ret <- drawns %>% 
    map(hdi, credMass = 0.95) %>% 
    map(~ c(.x, amp = unname(.x["upper"]) - unname(.x["lower"]))) %>% 
    bind_rows() %>% 
    mutate(stat = c("1.lower", "2.upper", "3.amp")) %>% 
    melt(id.vars = "stat", variable.name = "coef") %>% 
    dcast(coef ~ stat)

  ret
}

