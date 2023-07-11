#' EWMA SN Control Chart for Grouped Data based on Exceedance Statistic
#'
#' This function calculates the Exponentially Weighted Moving Average
#' (EWMA) control chart based on the signs (SN) statistic for grouped data.
#'
#' @param X A matrix or data frame containing the grouped data.
#' @param med The median value to be used for the SN calculation.
#' Default is 0.
#' @param lambda The smoothing parameter for the EWMA calculation.
#' Default is 0.05.
#' @param L The control limit multiplier for the UCL and LCL calculation.
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
#' # Generate random grouped data
#' X <- matrix(rnorm(100), nrow = 10)
#' # Create the EWMA SN control chart
#' ewma_sn(X)
#'
#' @import purrr
#' @importFrom stats median
#' @importFrom graphics plot
#'
#' @export

ewma_ex = function(X,
                   Y,
                   lambda = 0.05,
                   L = 2.447,
                   r = round(length(X)/2, 0),
                   far = 0.0027,
                   group_by_col = FALSE,
                   plot = T,
                   side = "two.sided") {
  r = round(r, 0)

  if (r > length(X)) {
    stop("j needs to be integer and lower than Y sample's size.")
  }

  if(group_by_col){
    Y <- t(Y)
  }

  X = c(X)
  m = length(X)
  n = nrow(Y)
  j = ncol(Y)

  #define aux to get the right percentile
  #aux = ifelse(n%%2 == 0, r, r + 0.5)

  #estimating U
  Xr = quantile(X, r/m)
  U = (Y >= Xr) |> colSums()

  Z = lambda * U
  Z = c(lambda * U[1], rep(0, j - 1))

  a = r/(m + 1)
  cl = n * (1 - a) * (1 - (1 - lambda)^(1:j))

  for (t in 2:length(Z)) {
    Z[t] = lambda * U[t] + (1 - lambda) * Z[t - 1]
  }

  aux_limits = sqrt( (n * a * (1 - a) / (m + 2) ) *
                       (n * (1 - (1 - lambda)^(1:j) )^2 +
                          (lambda * (m + 1) / (2 - lambda) *
                             (1 - (1 - lambda)^(2 * (1:j) ) ) ) ) )

  if (side == "two.sided") {
    aux = L * aux_limits
    ucl = cl + aux
    lcl = cl - aux
  } else if (side == "lower") {
    for (t in 1:length(Z)) {
      Z[t] = min(cl[t], Z[t])
    }
    lcl = cl - L * aux_limits
    ucl = NULL
  } else if (side == "upper") {
    for (t in 1:length(Z)) {
      Z[t] = max(cl[t], Z[t])
    }
    ucl = cl + L * aux_limits
    lcl = NULL
  } else {
    stop("Invalid argument for side. Must be one of 'two.sided',
         'lower', or 'upper'")
  }

  if (plot) {
    plot_chart(
      side = side,
      statistics = Z,
      ic = cl,
      ucl = ucl,
      lcl = lcl,
      name = "EWMA control chart (Based on the Exceedance Statistic)"
    )
  } else{
    if(side == 'two.sided'){
      return(which(Z > ucl | Z < lcl)[1])
    } else if (side == 'upper'){
      return(which(Z > ucl)[1])
    } else{
      return(which(Z < lcl)[1])
    }
  }
}
