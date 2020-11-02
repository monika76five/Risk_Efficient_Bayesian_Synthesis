###########################################################################
############################# IR marginal #################################
###########################################################################

IdentificationRisk <- function(origdata, syndata, syn.vars, m, n, r){
  origdata = origdata
  syndata = syndata
  m = m ## number of synthetic populations
  n = n  ## number of records / individuals
  
  count_in_pattern = rep(n, n)
  count_within_r = matrix(rep(NA, n*m), ncol = m)
  T_vector = matrix(rep(NA, n*m), ncol = m)
  
  for (i in 1:n){
    radius = r*eval(parse(text=paste("origdata$",syn.vars,"[i]")))
    
    for (l in 1:m){
      syndata_l = syndata[[l]]
      match_l<-((eval(parse(text=paste("syndata_l$",syn.vars,"<=
                                      origdata$",syn.vars,"[i]+",radius,sep="",collapse="&")))&
                   eval(parse(text=paste("syndata_l$",syn.vars,">=
                                      origdata$",syn.vars,"[i]-",radius,sep="",collapse="&")))))
      
      match.prob_l<-ifelse(match_l,1/sum(match_l),0)
      if (max(match.prob_l) > 0){
        T_vector[i, l] = is.element(i,rownames(origdata)[match.prob_l==max(match.prob_l)])
      }
      else {
        T_vector[i, l] = 0
      }
      count_within_r[i, l] = sum(match_l)
    }
    
  }
  
  IdentificationRisksMatrix = 1 - count_within_r/count_in_pattern
  
  exp.risk_record = rep(NA, n)
  for (i in 1:n){
    exp.risk_record[i] = mean(IdentificationRisksMatrix[i,]*T_vector[i,])
  }
  return(exp.risk_record)
}

###########################################################################
######################## weights marginal #################################
###########################################################################

ComputeAlphas_constant <- function(exp.risk_record, scale_factor, shift_factor){
  
  alphas = scale_factor * (1 - exp.risk_record) + shift_factor
  alphas[alphas <= 0] = 0
  alphas[alphas >= 1] = 1
  
  return(alphas)
} ## end function ComputeWeights_marginal_constant() to construct marginal risk-based weights


###########################################################################
######################## synthetic data generation ########################
###########################################################################

gen_synthetic_data  <- function(results = res, m = 20, thin = 5, diagnostics = FALSE, S_excl = NULL)
{
  ## check convergence
  library(coda)
  library(bayesplot)
  library(ggplot2)
  
  estimated_model     <- results
  # posterior           = as.array(estimated_model)
  # np                  = nuts_params(estimated_model)
  # 
  # mcmc_pairs(posterior, np = np, pars = c("sigma_y[1]","mu[1]"))
  
  samps_mu            <- rstan::extract(estimated_model, pars = "mu", permuted = FALSE, inc_warmup = FALSE)
  samps_phi           <- rstan::extract(estimated_model, pars = "phi", permuted = FALSE, inc_warmup = FALSE)
  
  ## remove excluded MCMC draws whose log-likelihood-ratio / sensitivity is above Lipschitz constant
  if( !is.null(S_excl) && length(S_excl) > 0 )
  {
    samps_mu            <- samps_mu[-S_excl,]
    samps_phi           <- samps_phi[-S_excl,]
  } ## end condition on whether any MCMC iterations are to be excluded
  
  ## Adjust iter to remove MCMC iterations with L_s > L that were excluded
  iter                <- estimated_model@stan_args[[1]]$iter - length(S_excl)
  warmup              <- estimated_model@stan_args[[1]]$warmup
  nthin               <- floor((iter-warmup)/(m*thin))
  start               <- 0 ## because excluded warmup iterations using save_warmup = FALSE
  
  
  if( diagnostics )
  {
    color_scheme_set("mix-brightblue-gray")
    
    mcmc_trace(posterior, pars = "mu[1]", np = np) +
      xlab("Post-warmup iteration")
    
    mu.mcmc = as.mcmc(samps_mu)
    effectiveSize(mu.mcmc)
    acf(mu.mcmc[,1])
    coda::traceplot(mu.mcmc)
    
  } ## end condition on diagnostics
  
  
  ## generate synthetic data
  mu_draws <- rep(NA, m) # m-by-1
  phi_draws <- rep(NA, m) # m-by-1
  
  ### thining
  for (l in 1:m){
    mu_draws[l] <- exp(samps_mu[(start + nthin*l)])
    phi_draws[l] <- samps_phi[(start + nthin*l)]
  }
  
  syndata = vector("list",m)
  for (l in 1:m){
    syndata_l <- data.frame(rnegbin(N, mu_draws[l], phi_draws[l]))
    names(syndata_l) <- c("outcome")
    syndata[[l]] <- syndata_l
  }
  
  return(syndata_final = syndata)
  
} ## end gen_synthetic_data() to generate synthetic data from synthesizer


###########################################################################
############################# IR pairwise #################################
###########################################################################


## Identifcation risks calculation function
IdentificationRisk_pw <- function(origdata, syndata, known.vars, all.known.vars, syn.vars, m, n, r){
  origdata = origdata
  syndata = syndata
  m = m ## number of synthetic populations
  n = n  ## number of records / individuals

  count_in_pattern = rep(n, n)
  count_outside_r = vector("list", n)
  T_vector = vector("list", n)
  
  for (i in 1:n){
    
    all_index = 1:n
    record_noi_index = all_index[-i] 
    count_outside_r_i = matrix(rep(0, (n-1)*m), ncol = m) # dim: (n-1)-by-m
    T_vector_i = matrix(rep(0, (n-1)*m), ncol = m) # dim: (n-1)-by-m
    
    radius_i = r*eval(parse(text=paste("origdata$",syn.vars,"[i]")))
    
    for (j in 1:(n-1)){
      record_j_index = record_noi_index[j]
      radius_j = r*eval(parse(text=paste("origdata$",syn.vars,"[record_j_index]")))
      
      for (l in 1:m){
        syndata_l = syndata[[l]]
        match_l_i <- (eval(parse(text=paste("syndata_l$",syn.vars,"<=
                                           origdata$",syn.vars,"[i]+",radius_i,sep="",collapse="&")))&
                         eval(parse(text=paste("syndata_l$",syn.vars,">=
                                             origdata$",syn.vars,"[i]-",radius_i,sep="",collapse="&"))))
        
        match_l_j <- (eval(parse(text=paste("syndata_l$",syn.vars,"<=
                                           origdata$",syn.vars,"[record_j_index]+",radius_j,sep="",collapse="&")))&
                         eval(parse(text=paste("syndata_l$",syn.vars,">=
                                             origdata$",syn.vars,"[record_j_index]-",radius_j,sep="",collapse="&"))))
        
        match.prob_l_i <- ifelse(match_l_i, 1/sum(match_l_i), 0)
        match.prob_l_j <- ifelse(match_l_j, 1/sum(match_l_j), 0)
        
        if ((max(match.prob_l_i) > 0) & (max(match.prob_l_j) > 0)){
          T_vector_i[j, l] = (is.element(i,rownames(origdata)[match.prob_l_i==max(match.prob_l_i)]))&
            (is.element(record_j_index,rownames(origdata)[match.prob_l_j==max(match.prob_l_j)]))
        } else {
          T_vector_i[j, l] = 0
        }
        
        match_l_i_only <- match_l_i[all_index]
        match_l_j_only <- match_l_j[all_index]
        
        count_outside_r_i[j, l] = sum((1-match_l_i_only)&(1-match_l_j_only))
        
      }
      
    }
    
    count_outside_r[[i]] = count_outside_r_i
    T_vector[[i]] = T_vector_i
    
    print(i)
  }
  
  ### compute alpha_ij as a list for every i
  alpha_ij_list = vector("list", n)
  
  for (i in 1:n){
    
    ### obtain IR_ij
    count_outside_r_i = count_outside_r[[i]] # dim = (n-1)-by-m
    T_vector_i = T_vector[[i]] # dim = (n-1)-by-m
    
    IR_ij = (count_outside_r_i/count_in_pattern[i])*T_vector_i # dim = (n-1)-by-m
    
    alpha_ij_matrix = matrix(rep(NA, (count_in_pattern[[i]]-1)*m), ncol = m)
    
    for (l in 1:m){
      alpha_ij = 1 - IR_ij[,l]
      
      alpha_ij_matrix[,l] = alpha_ij
    }
    alpha_ij_list[[i]] = alpha_ij_matrix
  }
  
  ### normalizing alpha_ij to obtain alpha_i, by dividing by M_p - 1
  alpha_i_matrix = matrix(rep(NA, n*m), ncol = m)
  for (i in 1:n){
    alpha_ij_matrix = alpha_ij_list[[i]]
    
    alpha_i = colSums(alpha_ij_matrix)
    
    M_i = count_in_pattern[i]
    
    alpha_i_normalized = alpha_i/(M_i - 1)
    
    alpha_i_matrix[i,] = alpha_i_normalized
    
  }
  
  alpha_i_record = rowMeans(alpha_i_matrix) 
  
  return(alpha_i_record)
}