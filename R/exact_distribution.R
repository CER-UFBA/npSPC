#' @title Exact Distribution for WSNR
#' @description This function is hidden and not intended for direct use.
#'
#'
#' @keywords internal
#'
Exact_distribution = function(n, alpha, side = "two.sided"){

  Combinations = expand.grid(rep(list(c(-1, 1)), times = n))
  Ranks_matrix = matrix(rep(1:n, 2^n), nrow = 2^n, ncol = n, byrow = T)

  Matrix_aux = Ranks_matrix * Combinations

  ranks = rowSums(Matrix_aux)
  dist = table(ranks)/(2^n)

  maxim = min(as.numeric(names(cumsum(dist))[cumsum(dist) >=  1 - alpha]),
              (n * (n + 1))/2)
  LCL = -maxim
  UCL = maxim

  if(side == "lower"){
    UCL = NULL
  } else if(side == "upper"){
    LCL = NULL
  }
  return(list('Ranks' = ranks,
              'Distribution' = dist,
              'UCL' = UCL, 'LCL' = LCL))
}



a = (Exact_distribution(13, 1/500)$Distribution)
a
which(cumsum(a) <= 1/500/2)

barplot(Exact_distribution(13, 1/500)$Distribution)

Limits_asymptotical(21, 0.001)

Exact_distribution(21, 0.001)

