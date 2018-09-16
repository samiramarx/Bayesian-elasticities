#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# OLS ESTIMATE - EXPORT TO LATEX

#--------------------------------------
# library
#--------------------------------------
library(systemfit)
library(stargazer)
#-------------------------------------

source("scripts/ols_estimates.R")

# MKT1

mkt1_v2f_lm <- lm(mkt1_v2f, data = mkt1)
mkt1_v2r_lm <- lm(mkt1_v2r, data = mkt1)

mkt1_v2f_coef <- mkt1_system$eq[[1]]$coefficients
mkt1_v2f_se <- sqrt(diag(mkt1_system$eq[[1]]$coefCov))
mkt1_v2f_p <- 2*pt(abs(mkt1_v2f_coef / mkt1_v2f_se), mkt1_system$df.residual, lower.tail = FALSE)

mkt1_v2r_coef <- mkt1_system$eq[[2]]$coefficients
mkt1_v2r_se <- sqrt(diag(mkt1_system$eq[[2]]$coefCov))
mkt1_v2r_p <- 2*pt(abs(mkt1_v2r_coef / mkt1_v2r_se), mkt1_system$df.residual, lower.tail = FALSE)

stargazer(mkt1_v2f_lm, mkt1_v2r_lm,
          coef = list(mkt1_v2f_coef, mkt1_v2r_coef),
          se = list(mkt1_v2f_se, mkt1_v2r_se),
          p = list(mkt1_v2f_p, mkt1_v2r_p),
          column.labels = c("Equation 1", "Equation 2"),
          keep.stat = c("n"))
#-------------------------------------
# MKT2

mkt2_v2f_lm <- lm(mkt2_v2f, data = mkt2)
mkt2_v2r_lm <- lm(mkt2_v2r, data = mkt2)
mkt2_v2a_lm <- lm(mkt2_v2a, data = mkt2)

mkt2_v2f_coef <- mkt2_system$eq[[1]]$coefficients
mkt2_v2f_se <- sqrt(diag(mkt2_system$eq[[1]]$coefCov))
mkt2_v2f_p <- 2*pt(abs(mkt2_v2f_coef / mkt2_v2f_se), mkt2_system$df.residual, lower.tail = FALSE)

mkt2_v2r_coef <- mkt2_system$eq[[2]]$coefficients
mkt2_v2r_se <- sqrt(diag(mkt2_system$eq[[2]]$coefCov))
mkt2_v2r_p <- 2*pt(abs(mkt2_v2r_coef / mkt2_v2r_se), mkt2_system$df.residual, lower.tail = FALSE)

mkt2_v2a_coef <- mkt2_system$eq[[3]]$coefficients
mkt2_v2a_se <- sqrt(diag(mkt2_system$eq[[3]]$coefCov))
mkt2_v2a_p <- 2*pt(abs(mkt2_v2a_coef / mkt2_v2a_se), mkt2_system$df.residual, lower.tail = FALSE)

stargazer(mkt2_v2f_lm, mkt2_v2r_lm, mkt2_v2a_lm,
          coef = list(mkt2_v2f_coef, mkt2_v2r_coef, mkt2_v2a_coef),
          se = list(mkt2_v2f_se, mkt2_v2r_se, mkt2_v2a_se),
          p = list(mkt2_v2f_p, mkt2_v2r_p, mkt2_v2a_p),
          column.labels = c("Equation 1", "Equation 2", "Equation 3"),
          keep.stat = c("n"))

#-------------------------------------
# MKT3

mkt3_v1n_lm <- lm(mkt3_v1n, data = mkt3)
mkt3_v2f_lm <- lm(mkt3_v2f, data = mkt3)
mkt3_v2r_lm <- lm(mkt3_v2r, data = mkt3)
mkt3_v2a_lm <- lm(mkt3_v2a, data = mkt3)

mkt3_v1n_coef <- mkt3_system$eq[[1]]$coefficients
mkt3_v1n_se <- sqrt(diag(mkt3_system$eq[[1]]$coefCov))
mkt3_v1n_p <- 2*pt(abs(mkt3_v1n_coef / mkt3_v1n_se), mkt3_system$df.residual, lower.tail = FALSE)

mkt3_v2f_coef <- mkt3_system$eq[[2]]$coefficients
mkt3_v2f_se <- sqrt(diag(mkt3_system$eq[[2]]$coefCov))
mkt3_v2f_p <- 2*pt(abs(mkt3_v2f_coef / mkt3_v2f_se), mkt3_system$df.residual, lower.tail = FALSE)

mkt3_v2r_coef <- mkt3_system$eq[[3]]$coefficients
mkt3_v2r_se <- sqrt(diag(mkt3_system$eq[[3]]$coefCov))
mkt3_v2r_p <- 2*pt(abs(mkt3_v2r_coef / mkt3_v2r_se), mkt3_system$df.residual, lower.tail = FALSE)

mkt3_v2a_coef <- mkt3_system$eq[[4]]$coefficients
mkt3_v2a_se <- sqrt(diag(mkt3_system$eq[[4]]$coefCov))
mkt3_v2a_p <- 2*pt(abs(mkt3_v2a_coef / mkt3_v2a_se), mkt3_system$df.residual, lower.tail = FALSE)

stargazer(mkt3_v1n_lm, mkt3_v2f_lm, mkt3_v2r_lm, mkt3_v2a_lm,
          coef = list(mkt3_v1n_coef, mkt3_v2f_coef, mkt3_v2r_coef, mkt3_v2a_coef),
          se = list(mkt3_v1n_se, mkt3_v2f_se, mkt3_v2r_se, mkt3_v2a_se),
          p = list(mkt3_v1n_p, mkt3_v2f_p, mkt3_v2r_p, mkt3_v2a_p),
          column.labels = c("Equation 1", "Equation 2", "Equation 3", "Equation 4"),
          keep.stat = c("n"))

#-------------------------------------
# MKT3 - CUTTED

mkt3_v1n_lm <- lm(mkt3_v1n, data = mkt3_cut)
mkt3_v2f_lm <- lm(mkt3_v2f, data = mkt3_cut)
mkt3_v2r_lm <- lm(mkt3_v2r, data = mkt3_cut)
mkt3_v2a_lm <- lm(mkt3_v2a, data = mkt3_cut)

mkt3_v1n_coef <- mkt3_cut_system$eq[[1]]$coefficients
mkt3_v1n_se <- sqrt(diag(mkt3_cut_system$eq[[1]]$coefCov))
mkt3_v1n_p <- 2*pt(abs(mkt3_v1n_coef / mkt3_v1n_se), mkt3_cut_system$df.residual, lower.tail = FALSE)

mkt3_v2f_coef <- mkt3_cut_system$eq[[2]]$coefficients
mkt3_v2f_se <- sqrt(diag(mkt3_cut_system$eq[[2]]$coefCov))
mkt3_v2f_p <- 2*pt(abs(mkt3_v2f_coef / mkt3_v2f_se), mkt3_cut_system$df.residual, lower.tail = FALSE)

mkt3_v2r_coef <- mkt3_cut_system$eq[[3]]$coefficients
mkt3_v2r_se <- sqrt(diag(mkt3_cut_system$eq[[3]]$coefCov))
mkt3_v2r_p <- 2*pt(abs(mkt3_v2r_coef / mkt3_v2r_se), mkt3_cut_system$df.residual, lower.tail = FALSE)

mkt3_v2a_coef <- mkt3_cut_system$eq[[4]]$coefficients
mkt3_v2a_se <- sqrt(diag(mkt3_cut_system$eq[[4]]$coefCov))
mkt3_v2a_p <- 2*pt(abs(mkt3_v2a_coef / mkt3_v2a_se), mkt3_cut_system$df.residual, lower.tail = FALSE)

stargazer(mkt3_v1n_lm, mkt3_v2f_lm, mkt3_v2r_lm, mkt3_v2a_lm,
          coef = list(mkt3_v1n_coef, mkt3_v2f_coef, mkt3_v2r_coef, mkt3_v2a_coef),
          se = list(mkt3_v1n_se, mkt3_v2f_se, mkt3_v2r_se, mkt3_v2a_se),
          p = list(mkt3_v1n_p, mkt3_v2f_p, mkt3_v2r_p, mkt3_v2a_p),
          column.labels = c("Equation 1", "Equation 2", "Equation 3", "Equation 4"),
          keep.stat = c("n"))

#-------------------------------------
# MKT4

mkt4_v1f_lm <- lm(mkt4_v1f, data = mkt4)
mkt4_v1r_lm <- lm(mkt4_v1r, data = mkt4)
mkt4_v1a_lm <- lm(mkt4_v1a, data = mkt4)
mkt4_v2f_lm <- lm(mkt4_v2f, data = mkt4)
mkt4_v2r_lm <- lm(mkt4_v2r, data = mkt4)
mkt4_v2a_lm <- lm(mkt4_v2a, data = mkt4)

mkt4_v1f_coef <- mkt4_system$eq[[1]]$coefficients
mkt4_v1f_se <- sqrt(diag(mkt4_system$eq[[1]]$coefCov))
mkt4_v1f_p <- 2*pt(abs(mkt4_v1f_coef / mkt4_v1f_se), mkt4_system$df.residual, lower.tail = FALSE)

mkt4_v1r_coef <- mkt4_system$eq[[2]]$coefficients
mkt4_v1r_se <- sqrt(diag(mkt4_system$eq[[2]]$coefCov))
mkt4_v1r_p <- 2*pt(abs(mkt4_v1r_coef / mkt4_v1r_se), mkt4_system$df.residual, lower.tail = FALSE)

mkt4_v1a_coef <- mkt4_system$eq[[3]]$coefficients
mkt4_v1a_se <- sqrt(diag(mkt4_system$eq[[3]]$coefCov))
mkt4_v1a_p <- 2*pt(abs(mkt4_v1a_coef / mkt4_v1a_se), mkt4_system$df.residual, lower.tail = FALSE)

mkt4_v2f_coef <- mkt4_system$eq[[4]]$coefficients
mkt4_v2f_se <- sqrt(diag(mkt4_system$eq[[4]]$coefCov))
mkt4_v2f_p <- 2*pt(abs(mkt4_v2f_coef / mkt4_v2f_se), mkt4_system$df.residual, lower.tail = FALSE)

mkt4_v2r_coef <- mkt4_system$eq[[5]]$coefficients
mkt4_v2r_se <- sqrt(diag(mkt4_system$eq[[5]]$coefCov))
mkt4_v2r_p <- 2*pt(abs(mkt4_v2r_coef / mkt4_v2r_se), mkt4_system$df.residual, lower.tail = FALSE)

mkt4_v2a_coef <- mkt4_system$eq[[6]]$coefficients
mkt4_v2a_se <- sqrt(diag(mkt4_system$eq[[6]]$coefCov))
mkt4_v2a_p <- 2*pt(abs(mkt4_v2a_coef / mkt4_v2a_se), mkt4_system$df.residual, lower.tail = FALSE)

stargazer(mkt4_v1f_lm, mkt4_v1r_lm, mkt4_v1a_lm, mkt4_v2f_lm, mkt4_v2r_lm, mkt4_v2a_lm,
          coef = list(mkt4_v1f_coef, mkt4_v1r_coef, mkt4_v1a_coef, mkt4_v2f_coef, mkt4_v2r_coef, mkt4_v2a_coef),
          se = list(mkt4_v1f_se, mkt4_v1r_se, mkt4_v1a_se, mkt4_v2f_se, mkt4_v2r_se, mkt4_v2a_se),
          p = list(mkt4_v1f_p, mkt4_v1r_p, mkt4_v1a_p, mkt4_v2f_p, mkt4_v2r_p, mkt4_v2a_p),
          column.labels = c("Equation 1", "Equation 2", "Equation 3", "Equation 4", "Equation 5", "Equation 6"),
          keep.stat = c("n"))

#-------------------------------------
# MKT4 - SCOTYORK

mkt4_v1f_lm <- lm(mkt4_v1f, data = mkt4_cut)
mkt4_v1r_lm <- lm(mkt4_v1r, data = mkt4_cut)
mkt4_v1a_lm <- lm(mkt4_v1a, data = mkt4_cut)
mkt4_v2f_lm <- lm(mkt4_v2f, data = mkt4_cut)
mkt4_v2r_lm <- lm(mkt4_v2r, data = mkt4_cut)
mkt4_v2a_lm <- lm(mkt4_v2a, data = mkt4_cut)

mkt4_v1f_coef <- mkt4_cut_system$eq[[1]]$coefficients
mkt4_v1f_se <- sqrt(diag(mkt4_cut_system$eq[[1]]$coefCov))
mkt4_v1f_p <- 2*pt(abs(mkt4_v1f_coef / mkt4_v1f_se), mkt4_cut_system$df.residual, lower.tail = FALSE)

mkt4_v1r_coef <- mkt4_cut_system$eq[[2]]$coefficients
mkt4_v1r_se <- sqrt(diag(mkt4_cut_system$eq[[2]]$coefCov))
mkt4_v1r_p <- 2*pt(abs(mkt4_v1r_coef / mkt4_v1r_se), mkt4_cut_system$df.residual, lower.tail = FALSE)

mkt4_v1a_coef <- mkt4_cut_system$eq[[3]]$coefficients
mkt4_v1a_se <- sqrt(diag(mkt4_cut_system$eq[[3]]$coefCov))
mkt4_v1a_p <- 2*pt(abs(mkt4_v1a_coef / mkt4_v1a_se), mkt4_cut_system$df.residual, lower.tail = FALSE)

mkt4_v2f_coef <- mkt4_cut_system$eq[[4]]$coefficients
mkt4_v2f_se <- sqrt(diag(mkt4_cut_system$eq[[4]]$coefCov))
mkt4_v2f_p <- 2*pt(abs(mkt4_v2f_coef / mkt4_v2f_se), mkt4_cut_system$df.residual, lower.tail = FALSE)

mkt4_v2r_coef <- mkt4_cut_system$eq[[5]]$coefficients
mkt4_v2r_se <- sqrt(diag(mkt4_cut_system$eq[[5]]$coefCov))
mkt4_v2r_p <- 2*pt(abs(mkt4_v2r_coef / mkt4_v2r_se), mkt4_cut_system$df.residual, lower.tail = FALSE)

mkt4_v2a_coef <- mkt4_cut_system$eq[[6]]$coefficients
mkt4_v2a_se <- sqrt(diag(mkt4_cut_system$eq[[6]]$coefCov))
mkt4_v2a_p <- 2*pt(abs(mkt4_v2a_coef / mkt4_v2a_se), mkt4_cut_system$df.residual, lower.tail = FALSE)

stargazer(mkt4_v1f_lm, mkt4_v1r_lm, mkt4_v1a_lm, mkt4_v2f_lm, mkt4_v2r_lm, mkt4_v2a_lm,
          coef = list(mkt4_v1f_coef, mkt4_v1r_coef, mkt4_v1a_coef, mkt4_v2f_coef, mkt4_v2r_coef, mkt4_v2a_coef),
          se = list(mkt4_v1f_se, mkt4_v1r_se, mkt4_v1a_se, mkt4_v2f_se, mkt4_v2r_se, mkt4_v2a_se),
          p = list(mkt4_v1f_p, mkt4_v1r_p, mkt4_v1a_p, mkt4_v2f_p, mkt4_v2r_p, mkt4_v2a_p),
          column.labels = c("Equation 1", "Equation 2", "Equation 3", "Equation 4", "Equation 5", "Equation 6"),
          keep.stat = c("n"))
