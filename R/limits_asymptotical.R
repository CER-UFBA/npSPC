#' @title Limits Asymptotical
#' @description This function is hidden and not intended for direct use.
#'
#'
#' @keywords internal
#'
Limits_asymptotical = function(n, alpha, side = "two.sided"){
  mean = 0
  sd_asymptotic = sqrt( n * (n + 1) * (2 * n + 1) / 6 )
  LCL = qnorm(alpha, mean = mean, sd = sd_asymptotic)
  UCL = -LCL

  if(side == "lower"){
    UCL = NULL
  } else if(side == "upper"){
    LCL = NULL
  }

  return(list('LCL'= LCL, 'UCL' = UCL))
}
