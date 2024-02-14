#' CUSUM Control Chart based on the Wilcoxon Rank Sum Test
#'
#' This function generates a CUSUM Control Chart based on the Wilcoxon Rank Sum Test.
#' It compares a set of data points to a reference value and plots the control chart.
#'
#' @param X The reference value.
#' @param Y The data matrix or vector to be compared.
#' @param r The starting point for the reference value
#' (default is round(length(X)/2, 0)).
#' @param k The drift value for the CUSUM calculation
#' (default is 0.5).
#' @param h The control limit for the control chart.
#' @param group_by_col A logical value indicating whether to treat
#' columns as separate groups. Default is FALSE.
#' @param plot A logical value indicating whether to plot the control chart.
#' Default is TRUE.
#' @param side The side of the control chart,
#' can be "two.sided", "lower", or "upper".
#'
#' @return If `plot` is set to TRUE,
#' the function will generate a control chart plot.
#' If `plot` is set to FALSE,
#' the function will return the first out-of-control point (index)
#' based on the specified `side`.
#'
#' @examples
#' # Generate CUSUM Control Chart based on the Mann-Whitney Test
#' X <- 10
#' Y <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' cusum_wr(X, Y, r = 5, k = 0.5, h = 3, plot = TRUE, side = 'two.sided')
#'
#' @export
cusum_wr = function(X,
                     Y,
                     r = round(length(X)/2, 0),
                     k = 0.5,
                     h = 5,
                     group_by_col = F,
                     plot = T,
                     side = 'two.sided') {
  if(group_by_col){
    Y <- t(Y)
  }

  X = c(X)
  m = length(X)
  n = nrow(Y)
  j = ncol(Y)

  Cj_plus = Cj_minus = rep(0, j + 1)

  Wj = unlist(lapply(apply(Y, 2, wilcox.test, X), '[', 1))

  for (t in 2:(j+1)) {
    Cj_plus[t] = max(0, (Cj_plus[t - 1] + Wj[t-1] - (n * (m + n + 1)/2) - k))
    Cj_minus[t] = min(0, (Cj_minus[t - 1] + Wj[t-1] - (n * (m + n + 1)/2) + k))
  }

  Cj_plus = Cj_plus[-1]
  Cj_minus = Cj_minus[-1]

  #Nplus = ifelse(Sn_plus > 0, 1, 0)
  #Nminus = ifelse(Sn_minus < 0, 1, 0)

  #Nplus = ave(Nplus, cumsum(Nplus == 0), FUN = cumsum)
  #Nminus = ave(Nminus, cumsum(Nminus == 0), FUN = cumsum)

  if (side == "two.sided") {
    ucl = h
    lcl = -h
    statistics = Cj_plus
    statistics2 = Cj_minus
  } else if (side == "lower") {
    lcl = -h
    ucl = NULL
    statistics = Cj_minus
    statistics2 = NULL
  } else if (side == "upper") {
    ucl = h
    lcl = NULL
    statistics = Cj_plus
    statistics2 = NULL
  } else {
    stop("Invalid argument for side. Must be one of 'two.sided',
         'lower', or 'upper'")
  }

  if (plot) {
    plot_chart(
      side = side,
      statistics = statistics,
      statistics2 = statistics2,
      ic = 0,
      ucl = rep(ucl, length(statistics)),
      lcl = rep(lcl, length(statistics)),
      name = "CuSum Control Chart (based on Mann-Whitney statistics)"
    )
  } else{
    if(side == 'two.sided'){
      return(which(Cj_plus >= ucl | Cj_minus <= lcl)[1])
    } else if (side == 'upper'){
      return(which(Cj_plus >= ucl)[1])
    } else{
      return(which(Cj_minus <= lcl)[1])
    }

  }
}
