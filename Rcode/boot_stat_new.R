boot_stat <- function(syndata, m, FUN, seed, S, n){
  set.seed(seed)
  
  FUN_matrix = matrix(rep(NA, m*S), ncol = m)
  
  for (l in 1:m){
    value_vector = syndata[[l]]$outcome
    
    for (s in 1:S){
      FUN_matrix[s, l] = FUN(sample(value_vector, n, replace = TRUE))
    }
  }
  
  PE <- mean(FUN_matrix)
  lower <- quantile(FUN_matrix, 0.025)
  upper <- quantile(FUN_matrix, 0.975)
  
  res_r <- list(PE = PE, lower = lower, upper = upper)
  
}



