cusum_sn_arl_calc <- function(n = 1,
                              k = 0.5,
                              h = 3,
                              t = 10000,
                              n_sim = 10000,
                              group_by_col = F,
                              side = 'two.sided') {
  arl <- rep(NA, n_sim)
  X <- array(rnorm(n * t * n_sim), dim = c(n, t, n_sim))
  for (i in 1:n_sim) {
    arl[i] <- cusum_sn(X = matrix(X[, , i], nrow = n, ncol = t),
                       h = h, k = k, plot = FALSE, side = side)
    if (is.na(arl[i])) {
      warning(paste0("There is NA in your simulation ", i,
                     ". Please try a lower h value or a higher k value. ",
                     "Returning the ARL0 without the NA."))
      break
    }
  }
  
  mean(arl, na.rm = TRUE)
}

cusum_sn_arl_calc()
