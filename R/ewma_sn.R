#' EWMA SN Control Chart for Grouped Data
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


ewma_sn = function(X,
                   med = 0,
                   lambda = 0.05,
                   L = 2.472,
                   group_by_col = F,
                   plot = T,
                   side = 'two.sided') {
  if (group_by_col) {
    X = t(X)
  }

  n_plus = colSums(X > med)
  n_less = colSums(X < med)

  Sn = n_plus - n_less
  Z = lambda * Sn

  for (t in 2:length(Z)) {
    Z[t] = Z[t] + (1 - lambda) * Z[t - 1]
  }

  if (side == "two.sided") {
    ucl = L * sqrt(lambda * nrow(X) *
                     (1 - (1 - lambda) ^ (2 * (1:length(Z)))) /
                     (2 - lambda))
    lcl = -ucl
  } else if (side == "lower") {
    for (t in 1:length(Z)) {
      Z[t] = min(0, Z[t])
    }
    lcl = -(L * sqrt(lambda * nrow(X) *
                       (1 - (1 - lambda) ^ (2 * (1:length(Z)))) /
                       (2 - lambda)))
    ucl = NULL
  } else if (side == "upper") {
    for (t in 1:length(Z)) {
      Z[t] = max(0, Z[t])
    }
    ucl = L * sqrt(lambda * nrow(X) *
                     (1 - (1 - lambda) ^ (2 * (1:length(Z)))) /
                     (2 - lambda))
    lcl = NULL
  } else {
    stop("Invalid argument for side. Must be one of 'two.sided',
         'lower', or 'upper'")
  }

  if (plot) {
    plot_chart(
      side = side,
      statistics = Z,
      ic = rep(0, length(Z)),
      ucl = ucl,
      lcl = lcl,
      name = "EWMA for grouped data (based on signs statistics)"
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

