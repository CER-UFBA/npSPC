ewma_sn = function(X,
                   med = 0,
                   lambda = 0.05,
                   L = 2.667,
                   group_by_col = F,
                   side = 'two.sided') {
  if (group_by_col) {
    X = t(X)
  }
  
  n_plus = colSums(X > med)
  n_less = colSums(X < med)
  
  Sn = n_plus - n_less
  Z = lambda * Sn
  Z = c(0, Z)
  
  for (t in 2:length(Z)) {
    Z[t] = Z[t] + (1 - lambda) * Z[t - 1]
  }
  
  if (side == "two.sided") {
    ucl = L * sqrt(lambda * nrow(X) * (1 - (1 - lambda) ^ (2 * 
                                         length(Sn))) / (2 - lambda))
    lcl = -ucl
  } else if (side == "lower") {
    for (t in 2:length(Z)) {
      Z[t] = min(0, Z[t])
    }
    ucl = L * sqrt(lambda(1 - (1 - lambda) ^ 2 * length(Sn)) / (2 - lambda))
    lcl = -ucl
    ucl = NULL
  } else if (side == "upper") {
    for (t in 2:length(Z)) {
      Z[t] = max(0, Z[t])
    }
    ucl = L * sqrt(lambda(1 - (1 - lambda) ^ 2 * length(Sn)) / (2 - lambda))
    lcl = NULL
  } else {
    stop("Invalid argument for side. Must be one of 'two.sided',
         'lower', or 'upper'")
  }
  
  plot_chart(statistics = Z[-1], ic = 0,
             ucl = ucl, lcl = lcl,
             name = "EWMA for grouped data (Based on signs)")
  
  far = 1/mean(ewma_sn_simulation(lambda = lambda, L = L, n = nrow(X)))
}

ewma_sn_arl_calc(X)
