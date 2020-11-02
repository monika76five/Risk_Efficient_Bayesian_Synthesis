###Sims to demonstrate effects of r

##Load libraries, code, stan models###
#load rstan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("source_functions.R")
source("boot_quantile.R")
#load/compile stan models
mod_orig <- stan_model('wt_NB.stan')

#other libraries
library(sf) #load before LS2Wstat
library(LS2Wstat)
library(tidyverse)
library(Rfast)
library(superheat)
library(ggthemes)


## Negative Binomial data - intercept only

# N <- 1000 #sample size
# mu <- 100
# phi <- 10
# set.seed(123)
# y0 <- rnegbin(N, mu = mu, theta = phi)
# X <- matrix(1, nrow = N, ncol = 1)
# k <- dim(X)[2]
# plot(density(y0))
# 
# NBData <- as.data.frame(y0)
# names(NBData) <- c("outcome")

N <- 1000 #sample size
p <- c(0.3, 0.7)
phi <- c(5, 20)
mu <- 100
y0 <- rep(NA, N)

set.seed(1234)
for (i in 1:N){
 z_i <- which(rmultinom(1, 1, prob = p)==1)
 phi_i <- phi[z_i]
 y0[i] <- rnegbin(1, mu = mu, theta = phi_i)
}
X <- matrix(1, nrow = N, ncol = 1)
k <- dim(X)[2]
plot(density(y0))

NBData <- as.data.frame(y0)
names(NBData) <- c("outcome")

summary(m1 <- glm.nb(outcome ~ ., data = NBData))
exp(m1$coefficients)
m1$theta

thedata       = vector("list",1)
thedata[[1]]  = NBData

scale_factor_value = 1.0
shift_factor_value = 0.0

###########################################################################
######################## unweighted, fit, synthesize, and DP risks ########
###########################################################################

stan_data_notw <- list(y = as.vector(y0), X = X, K = k, n = N, 
                       alphas = as.vector(rep(1,N)))
sim_est_notw <- sampling(object = mod_orig, data = stan_data_notw,
                         chains = 1, ## init = myinits, init_r = 0.5,
                         iter = 12000, warmup = 7000, thin = 5 #,
                         #refresh = refresh,
                         #control = list(adapt_delta = 0.8, max_treedepth = 20)
)

sim_syn_notw <- gen_synthetic_data(results = sim_est_notw, m = 20, thin = 5, diagnostics = FALSE, S_excl = NULL)


notw_IR_r0p15_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_notw, 
                                         syn.vars = c("outcome"), 
                                         m = 20, n = N, r = 0.15)

notw_IR_r0p2_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_notw, 
                                         syn.vars = c("outcome"), 
                                         m = 20, n = N, r = 0.2)

notw_IR_r0p25_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_notw, 
                                          syn.vars = c("outcome"), 
                                          m = 20, n = N, r = 0.25)

notw_IR_r0p3_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_notw, 
                                         syn.vars = c("outcome"), 
                                         m = 20, n = N, r = 0.3)

###########################################################################
########## agency r = 0.15, fit, synthesize ################################
###########################################################################

IR_r0p15_tofit <- IdentificationRisk(origdata = NBData, syndata = thedata, 
                                    syn.vars = c("outcome"), 
                                    m = 1, n = N, r = 0.15)

alphas_r0p15 <- ComputeAlphas_constant(exp.risk_record = IR_r0p15_tofit,
                                      scale_factor = scale_factor_value,
                                      shift_factor = shift_factor_value)

stan_data_w_r0p15 <- stan_data_notw
stan_data_w_r0p15$alphas <- as.vector(alphas_r0p15)

sim_est_w_r0p15     <- sampling(object = mod_orig, data = stan_data_w_r0p15,
                               chains = 1, ## init = myinits, init_r = 0.5,
                               iter = 12000, warmup = 7000, thin = 5 #,
                               #refresh = refresh,
                               #control = list(adapt_delta = 0.8, max_treedepth = 20)
)

sim_syn_w_r0p15 <- gen_synthetic_data(results = sim_est_w_r0p15, m = 20, thin = 5, diagnostics = FALSE, S_excl = NULL)

w_r0p15_IR_r0p15_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p15,
                                           syn.vars = c("outcome"), 
                                           m = 20, n = N, r = 0.15)

w_r0p15_IR_r0p2_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p15,
                                           syn.vars = c("outcome"), 
                                           m = 20, n = N, r = 0.2)

w_r0p15_IR_r0p25_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p15,
                                            syn.vars = c("outcome"), 
                                            m = 20, n = N, r = 0.25)

w_r0p15_IR_r0p3_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p15,
                                           syn.vars = c("outcome"), 
                                           m = 20, n = N, r = 0.3)

###########################################################################
########## agency r = 0.2, fit, synthesize ################################
###########################################################################

IR_r0p2_tofit <- IdentificationRisk(origdata = NBData, syndata = thedata, 
                                    syn.vars = c("outcome"), 
                                    m = 1, n = N, r = 0.2)

alphas_r0p2 <- ComputeAlphas_constant(exp.risk_record = IR_r0p2_tofit,
                                      scale_factor = scale_factor_value,
                                      shift_factor = shift_factor_value)

stan_data_w_r0p2 <- stan_data_notw
stan_data_w_r0p2$alphas <- as.vector(alphas_r0p2)

sim_est_w_r0p2     <- sampling(object = mod_orig, data = stan_data_w_r0p2,
                               chains = 1, ## init = myinits, init_r = 0.5,
                               iter = 12000, warmup = 7000, thin = 5 #,
                               #refresh = refresh,
                               #control = list(adapt_delta = 0.8, max_treedepth = 20)
)

sim_syn_w_r0p2 <- gen_synthetic_data(results = sim_est_w_r0p2, m = 20, thin = 5, diagnostics = FALSE, S_excl = NULL)

w_r0p2_IR_r0p15_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p2,
                                           syn.vars = c("outcome"), 
                                           m = 20, n = N, r = 0.15)

w_r0p2_IR_r0p2_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p2,
                                           syn.vars = c("outcome"), 
                                           m = 20, n = N, r = 0.2)

w_r0p2_IR_r0p25_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p2,
                                            syn.vars = c("outcome"), 
                                            m = 20, n = N, r = 0.25)

w_r0p2_IR_r0p3_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p2,
                                            syn.vars = c("outcome"), 
                                            m = 20, n = N, r = 0.3)

###########################################################################
########## agency r = 0.25, fit, synthesize ################################
###########################################################################

IR_r0p25_tofit <- IdentificationRisk(origdata = NBData, syndata = thedata, 
                                    syn.vars = c("outcome"), 
                                    m = 1, n = N, r = 0.25)

alphas_r0p25 <- ComputeAlphas_constant(exp.risk_record = IR_r0p25_tofit,
                                      scale_factor = scale_factor_value,
                                      shift_factor = shift_factor_value)

stan_data_w_r0p25 <- stan_data_notw
stan_data_w_r0p25$alphas <- as.vector(alphas_r0p25)

sim_est_w_r0p25     <- sampling(object = mod_orig, data = stan_data_w_r0p25,
                               chains = 1, ## init = myinits, init_r = 0.5,
                               iter = 12000, warmup = 7000, thin = 5 #,
                               #refresh = refresh,
                               #control = list(adapt_delta = 0.8, max_treedepth = 20)
)

sim_syn_w_r0p25 <- gen_synthetic_data(results = sim_est_w_r0p25, m = 20, thin = 5, diagnostics = FALSE, S_excl = NULL)


w_r0p25_IR_r0p15_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p25,
                                            syn.vars = c("outcome"), 
                                            m = 20, n = N, r = 0.15)

w_r0p25_IR_r0p2_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p25,
                                           syn.vars = c("outcome"), 
                                           m = 20, n = N, r = 0.2)

w_r0p25_IR_r0p25_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p25,
                                             syn.vars = c("outcome"), 
                                             m = 20, n = N, r = 0.25)

w_r0p25_IR_r0p3_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p25,
                                             syn.vars = c("outcome"), 
                                             m = 20, n = N, r = 0.3)

###########################################################################
########## agency r = 0.3, fit, synthesize ###############################
###########################################################################

IR_r0p3_tofit <- IdentificationRisk(origdata = NBData, syndata = thedata, 
                                     syn.vars = c("outcome"), 
                                     m = 1, n = N, r = 0.3)

alphas_r0p3 <- ComputeAlphas_constant(exp.risk_record = IR_r0p3_tofit,
                                       scale_factor = scale_factor_value,
                                       shift_factor = shift_factor_value)

stan_data_w_r0p3 <- stan_data_notw
stan_data_w_r0p3$alphas <- as.vector(alphas_r0p3)

sim_est_w_r0p3     <- sampling(object = mod_orig, data = stan_data_w_r0p3,
                                chains = 1, ## init = myinits, init_r = 0.5,
                                iter = 12000, warmup = 7000, thin = 5 #,
                                #refresh = refresh,
                                #control = list(adapt_delta = 0.8, max_treedepth = 20)
)

sim_syn_w_r0p3 <- gen_synthetic_data(results = sim_est_w_r0p3, m = 20, thin = 5, diagnostics = FALSE, S_excl = NULL)


w_r0p3_IR_r0p15_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p3,
                                            syn.vars = c("outcome"), 
                                            m = 20, n = N, r = 0.15)

w_r0p3_IR_r0p2_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p3,
                                           syn.vars = c("outcome"), 
                                           m = 20, n = N, r = 0.2)

w_r0p3_IR_r0p25_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p3,
                                             syn.vars = c("outcome"), 
                                             m = 20, n = N, r = 0.25)

w_r0p3_IR_r0p3_final <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p3,
                                           syn.vars = c("outcome"), 
                                           m = 20, n = N, r = 0.3)