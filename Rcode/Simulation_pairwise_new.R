###Sims to demonstrate effects of r

##Load libraries, code, stan models###
#load rstan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("source_functions.R")
#load/compile stan models
mod_orig <- stan_model('wt_NB.stan')

#other libraries
library(sf) #load before LS2Wstat
library(LS2Wstat)
library(tidyverse)
library(Rfast)
library(superheat)
library(ggthemes)

load('NB_mu100_phi5and20_p0p3and0p7_n1000.RData')


###########################################################################
########## pairwise agency r = 0.15, fit, synthesize #######################
###########################################################################

alphas_r0p15_pw <- IdentificationRisk_pw(origdata = NBData, syndata = thedata, 
                                        syn.vars = c("outcome"), 
                                        m = 1, n = N, r = 0.15)

alphas_r0p15_pw_final <- ComputeAlphas_constant(exp.risk_record = (1 - alphas_r0p15_pw),
                                               scale_factor = scale_factor_value,
                                               shift_factor = shift_factor_value)

stan_data_w_r0p15_pw <- stan_data_notw
stan_data_w_r0p15_pw$alphas <- as.vector(alphas_r0p15_pw_final)

sim_est_w_r0p15_pw     <- sampling(object = mod_orig, data = stan_data_w_r0p15_pw,
                                  chains = 1, ## init = myinits, init_r = 0.5,
                                  iter = 12000, warmup = 7000, thin = 5 #,
                                  #refresh = refresh,
                                  #control = list(adapt_delta = 0.8, max_treedepth = 20)
)

sim_syn_w_r0p15_pw <- gen_synthetic_data(results = sim_est_w_r0p15_pw, m = 20, thin = 5, diagnostics = FALSE, S_excl = NULL)


w_r0p15_IR_r0p15_final_pw <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p15_pw,
                                              syn.vars = c("outcome"), 
                                              m = 20, n = N, r = 0.15)


###########################################################################
########## pairwise agency r = 0.2, fit, synthesize #######################
###########################################################################

alphas_r0p2_pw <- IdentificationRisk_pw(origdata = NBData, syndata = thedata, 
                                        syn.vars = c("outcome"), 
                                        m = 1, n = N, r = 0.2)

alphas_r0p2_pw_final <- ComputeAlphas_constant(exp.risk_record = (1 - alphas_r0p2_pw),
                                               scale_factor = scale_factor_value,
                                               shift_factor = shift_factor_value)

stan_data_w_r0p2_pw <- stan_data_notw
stan_data_w_r0p2_pw$alphas <- as.vector(alphas_r0p2_pw_final)

sim_est_w_r0p2_pw     <- sampling(object = mod_orig, data = stan_data_w_r0p2_pw,
                                  chains = 1, ## init = myinits, init_r = 0.5,
                                  iter = 12000, warmup = 7000, thin = 5 #,
                                  #refresh = refresh,
                                  #control = list(adapt_delta = 0.8, max_treedepth = 20)
)

sim_syn_w_r0p2_pw <- gen_synthetic_data(results = sim_est_w_r0p2_pw, m = 20, thin = 5, diagnostics = FALSE, S_excl = NULL)


w_r0p2_IR_r0p2_final_pw <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p2_pw,
                                              syn.vars = c("outcome"), 
                                              m = 20, n = N, r = 0.2)


###########################################################################
########## pairwise agency r = 0.25, fit, synthesize ######################
###########################################################################

alphas_r0p25_pw <- IdentificationRisk_pw(origdata = NBData, syndata = thedata, 
                                        syn.vars = c("outcome"), 
                                        m = 1, n = N, r = 0.25)

alphas_r0p25_pw_final <- ComputeAlphas_constant(exp.risk_record = (1 - alphas_r0p25_pw),
                                               scale_factor = scale_factor_value,
                                               shift_factor = shift_factor_value)

stan_data_w_r0p25_pw <- stan_data_notw
stan_data_w_r0p25_pw$alphas <- as.vector(alphas_r0p25_pw_final)

sim_est_w_r0p25_pw     <- sampling(object = mod_orig, data = stan_data_w_r0p25_pw,
                                  chains = 1, ## init = myinits, init_r = 0.5,
                                  iter = 12000, warmup = 7000, thin = 5 #,
                                  #refresh = refresh,
                                  #control = list(adapt_delta = 0.8, max_treedepth = 20)
)

sim_syn_w_r0p25_pw <- gen_synthetic_data(results = sim_est_w_r0p25_pw, m = 20, thin = 5, diagnostics = FALSE, S_excl = NULL)


w_r0p25_IR_r0p25_final_pw <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p25_pw,
                                              syn.vars = c("outcome"), 
                                              m = 20, n = N, r = 0.25)


###########################################################################
########## pairwise agency r = 0.3, fit, synthesize #######################
###########################################################################

alphas_r0p3_pw <- IdentificationRisk_pw(origdata = NBData, syndata = thedata, 
                                         syn.vars = c("outcome"), 
                                         m = 1, n = N, r = 0.3)

alphas_r0p3_pw_final <- ComputeAlphas_constant(exp.risk_record = (1 - alphas_r0p3_pw),
                                                scale_factor = scale_factor_value,
                                                shift_factor = shift_factor_value)

stan_data_w_r0p3_pw <- stan_data_notw
stan_data_w_r0p3_pw$alphas <- as.vector(alphas_r0p3_pw_final)

sim_est_w_r0p3_pw     <- sampling(object = mod_orig, data = stan_data_w_r0p3_pw,
                                   chains = 1, ## init = myinits, init_r = 0.5,
                                   iter = 12000, warmup = 7000, thin = 5 #,
                                   #refresh = refresh,
                                   #control = list(adapt_delta = 0.8, max_treedepth = 20)
)

sim_syn_w_r0p3_pw <- gen_synthetic_data(results = sim_est_w_r0p3_pw, m = 20, thin = 5, diagnostics = FALSE, S_excl = NULL)


w_r0p3_IR_r0p3_final_pw <- IdentificationRisk(origdata = NBData, syndata = sim_syn_w_r0p3_pw,
                                                syn.vars = c("outcome"), 
                                                m = 20, n = N, r = 0.3)