cusum_sn = function(X,
                   med = 0,
                   k = 0.5,
                   h = 5,
                   group_by_col = F,
                   plot = T,
                   side = 'two.sided') {
  if (group_by_col) {
    X = t(X)
  }
  
  n_plus = colSums(X > med)
  n_less = colSums(X < med)
  
  Sn = n_plus - n_less
  
  n = nrow(X)
  m = ncol(X)
  
  Sn_plus = rep(0, m + 1)
  Sn_minus = rep(0, m + 1)
  
  for (t in 2:m+1) {
    Sn_plus[t] = max(0, Sn_plus[t-1] + Sn[t] - k)
    Sn_minus[t] = min(0, Sn_minus[t-1] + Sn[t] + k)
  }
  
  Sn_plus = Sn_plus[-1]
  Sn_minus = Sn_minus[-1]
  
  if (side == "two.sided") {
    ucl = h
    lcl = -h
    statistics = Sn_plus
    statistics2 = Sn_minus
  } else if (side == "lower") {
    lcl = -h
    ucl = NULL
    statistics = Sn_minus
    statistics2 = NULL
  } else if (side == "upper") {
    ucl = h
    lcl = NULL
    statistics = Sn_plus
    statistics2 = NULL
  } else {
    stop("Invalid argument for side. Must be one of 'two.sided',
         'lower', or 'upper'")
  }
  
  print(statistics)
  print(statistics2)
  print(ucl)
  print(lcl)
  
  if (plot) {
    plot_chart(
      side = side,
      statistics = statistics,
      statistics2 = statistics2,
      ic = 0,
      ucl = ucl,
      lcl = lcl,
      name = "EWMA for grouped data (Based on signs)"
    )
  } else{
    if(side == 'two.sided'){
      return(which(Sn_plus > ucl | Sn_minus < lcl)[1])
    } else if (side == 'upper'){
      return(which(Sn_plus > ucl)[1])
    } else{
      return(which(Sn_minus < lcl)[1])
    }
    
  }
  
}

X = rgamma(10000, 1, 2) |> matrix(nrow = 50)
cusum_sn(X, plot = T)
