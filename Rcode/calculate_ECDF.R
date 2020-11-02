calculate_ECDF <- function(origdata, syndata, m){
  # require(stats)
  
  Um_outcome <- rep(NA, m)
  Ua_outcome <- rep(NA, m)
  for (l in 1:m){
    syndata_l <- syndata[[l]]
    merged_data <- rbind(origdata, syndata_l)
    
    ecdf_orig <- ecdf(origdata[,"outcome"]) 
    ecdf_syn <- ecdf(syndata_l[,"outcome"])
    
    percentile_orig <- ecdf_orig(merged_data[,"outcome"]) 
    percentile_syn <- ecdf_syn(merged_data[,"outcome"])
    
    ecdf_diff <- percentile_orig - percentile_syn
    
    Um_outcome[l] <- max(abs(ecdf_diff))
    
    Ua_outcome[l] <- mean(ecdf_diff^2)
  }
  
  res_r <- list(Um = mean(Um_outcome), Ua = mean(Ua_outcome))
  
}