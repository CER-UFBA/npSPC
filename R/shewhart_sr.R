#' Shewhart Chart using Wilcoxon Signed-Rank Test
#'
#' Calculates the Shewhart Chart using the Wilcoxon Signed-Rank Test.
#'
#' @param X A matrix or data frame of numeric values.
#' @param mu0 the process median, if known.
#' If NULL, it will be estimated using the mean of the observations.
#' @param far The false alarm rate for control limits calculation.
#' Default is 0.0027.
#' @param group_by_col Logical indicating whether to treat
#' columns as different groups (default: FALSE).
#' @param exact Logical indicating whether to use the exact
#' distribution for control limits calculation (default: FALSE).
#' @param side A character string specifying the alternative
#' hypothesis for the test ("two.sided", "greater", or "less")
#' (default: "two.sided").
#' @examples
#' X <- matrix(rnorm(100), nrow = 10)
#' shewhart_sr(X, mu0 = 0, far = 0.01, group_by_col = TRUE)
#' @export
shewhart_sr = function(X, mu0 = NULL, far = .0027,
                               group_by_col = FALSE, exact = FALSE,
                               side = "two.sided"){
  if(group_by_col){
    X = t(X)
  }
  if(is.null(mu0)){
    mu0 = mean(X, na.rm = T)
  }
  alpha = far
  if(side == "two.sided"){
    alpha = far/2
  }

  n = nrow(X)
  m = ncol(X)

  Ranks = apply(abs(X - mu0), 2, rank)
  Tplus = colSums(apply((X - mu0) > 0, 2, ifelse, 1, 0) * Ranks)
  SR = 2*Tplus - n*(n+1)/2

  if(exact){

    Limits = Exact_distribution(n, alpha, side = side)

  } else {

    if(n < 20){
      message("You are using the asymptotically distribution for estimating the limits,
              it is recommended to use the exact distribution when the size of each subgroups
              are lower than 20.")
    }

    Limits = Limits_asymptotical(n, alpha, side = side)

  }
  arl <- round(1 / far)

  plot_chart(statistics = SR, ic = 0,
             ucl = rep(Limits$UCL, length(SR)),
             lcl = rep(Limits$LCL, length(SR)),
             name = "Shewhart Control Chart (based on Wilcoxon signed-rank statistics)",
             side = side)

  arl = paste0('ARL0 utilizado: ', arl)
  return(arl)

}

