#' @title Limits Asymptotical
#' @description This function is hidden and not intended for direct use.
#'
#'
#' @keywords internal
#'
Limits_asymptotical = function(n, alpha, side = "two.sided"){
  mean = 0
  sd_asymptotic = sqrt( n * (n + 1) * (2 * n + 1) / 6 )
  LCL = qnorm(alpha/2, mean = mean, sd = sd_asymptotic)
  UCL = -LCL
  return(list('LCL'= LCL, 'UCL' = UCL))
}
