ewma_sn_arl_calc =
  function(lambda = 0.1,
           L = 2.667,
           n = 1,
           sim = 10000) {
    
    ucl = L * sqrt(lambda * n * (1 - (1 - lambda) ^ 2) / (2 - lambda))
    lcl = - ucl
    
    runlength = numeric(sim)
    xi = matrix(rnorm(sim * n, mean, 1), nrow = n)
    ti = rowSums(xi >= 0)
    sni = 2 * ti - n
    
    zi = numeric(sim)
    indicator = numeric(sim)
    
    for (j in 1:sim) {
      if (j == 1) {
        zi[j] <- lambda * sni[j]
      } else {
        zi[j] <- lambda * sni[j] + (1 - lambda) * zi[j - 1]
      }
      
      if (zi[j] > UCL_steady | zi[j] < LCL_steady) {
        indicator[j] <- 1
      }
      
      if (any(indicator)) {
        runlength[j] <- which.max(indicator == 1)
      } else {
        runlength[j] <- sim
      }
    }
    
    return(runlength)
  }