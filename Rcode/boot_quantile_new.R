boot_quantile <- function(syndata, m, seed, S, n, q){
  set.seed(seed)
  
  q_matrix = matrix(rep(NA, m*S), ncol = m)
  for (i in 1:m){
    value_vector = syndata[[i]]$outcome
    
    for (s in 1:S){
      q_matrix[s, i] = quantile(sample(value_vector, n, replace = TRUE), q)
    }
  }
  
  PE <- mean(q_matrix)
  lower <- quantile(q_matrix, 0.025)
  upper <- quantile(q_matrix, 0.975)
  
  res_r <- list(PE = PE, lower = lower, upper = upper)
  
}



