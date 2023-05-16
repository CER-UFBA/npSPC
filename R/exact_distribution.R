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

  maxim = max(which(cumsum(dist) <= alpha/2), 1)
  LCL = as.numeric(names(dist[maxim]))
  UCL = abs(LCL)

  return(list('Ranks' = ranks,
              'Distribution' = dist,
              'UCL' = UCL, 'LCL' = LCL))
}
