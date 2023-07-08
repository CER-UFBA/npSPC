#' Shewhart Chart using Wilcoxon Signed-Rank Test
#'
#' Calculates the Shewhart Chart using the Wilcoxon Signed-Rank Test.
#'
#' @param X A matrix or data frame of numeric values.
#' @param mu0 the process median, if known.
#' If NULL, it will be estimated using the mean of the observations.
#' @param alpha The significance level for the control limits (default: 0.0027).
#' @param group_by_col Logical indicating whether to treat 
#' columns as different groups (default: FALSE).
#' @param exact Logical indicating whether to use the exact 
#' distribution for control limits calculation (default: FALSE).
#' @param side A character string specifying the alternative 
#' hypothesis for the test ("two.sided", "greater", or "less") 
#' (default: "two.sided").
#' @examples
#' X <- matrix(rnorm(100), nrow = 10)
#' shewhart_snr(X, mu0 = 0, alpha = 0.01, group_by_col = TRUE)
#' @export
shewhart_snr = function(X, mu0 = NULL, alpha = .0027,
                               group_by_col = FALSE, exact = FALSE,
                               side = "two.sided"){
  if(group_by_col){
    X = t(X)
  }
  if(is.null(mu0)){
    mu0 = mean(X, na.rm = T)
  }

  n = nrow(X)
  m = ncol(X)

  SRi = NULL
  for(i in 1:m){

    Ranki = rank(abs(X[,i] - mu0))
    Tplus = sum(ifelse((X[,i] - mu0) > 0, 1, 0) * Ranki)
    Tminus = sum(ifelse((X[,i] - mu0) < 0, 1, 0) * Ranki)
    SRi_aux = Tplus - Tminus
    SRi = c(SRi, SRi_aux)
  }


  if(exact){

    Limits = Exact_distribution(n, alpha)

  } else {

    if(n < 20){
      message("You are using the asymptotically distribution for estimating the limits, it is recommended to use the exact distribution when the size of each subgroups are lower than 20.")
    }

    Limits = Limits_asymptotical(n, alpha)

  }

  plot_chart(statistics = SRi, ic = 0,
             ucl = Limits$UCL, lcl = Limits$LCL,
             name = "SRi (Wilcoxon signed-rank)",
             side = side)

}
