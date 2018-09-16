library(bayesm)

load("dta_spread_2nd.RData")

# ols fit
v2f_2nd <- lm(data = dta, log(jny_2f) ~  log(avgf_2f))

summary(v2f_2nd)

# ols fit
v2f_2nd <- lm(data = dta, log(jny_2f) ~  log(avgf_2f) + 
                log(avgf_2r) +
                log(avgf_2a) +
                log(avg_gva)+
                log(gjt_f)+
                log(gjt_r)
)

summary(v2f_2nd)

#rmultireg
Y <- log(dta$jny_2f)
X <- cbind(c = rep(1, lenght.out = dta$rail_year), 
           log(dta$avgf_2f))
Bbar <- -1.5
A <- 0.5
nu <- 

rmultireg(Y, X, Bbar, A)


