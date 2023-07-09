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

  #change nrow(X) for apply length X

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
      ic = 0,
      ucl = ucl,
      lcl = lcl,
      name = "EWMA for grouped data (Based on signs)"
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

#' Exponentially Weighted Moving Average (EWMA) for Grouped Data
#'
#' Calculates the EWMA for grouped data based on signs.
#'
#' @param X A numeric matrix or data frame containing the data.
#' @param med The median value to use as the reference. Default is 0.
#' @param lambda The decay factor for the EWMA. Default is 0.05.
#' @param L The multiplier for the control limits. Default is 2.667.
#' @param group_by_col A logical value indicating whether to treat columns as groups.
#'   If TRUE, the matrix will be transposed. Default is FALSE.
#' @param side The side of the chart to plot. Must be one of 'two.sided', 'lower', or 'upper'.
#'   Default is 'two.sided'.
#'
#' @return The function generates a control chart plot and returns the average run length (ARL).
#'
#' @details The function calculates the EWMA based on signs for grouped data.
#'   It calculates the number of observations above and below the median for each group,
#'   and then applies the EWMA formula to obtain the control statistics.
#'   The control limits are calculated based on the side parameter.
#'   The function also plots the control chart using the plot_chart function
#'   and calculates the average run length (ARL) based on simulated data.
#'
#' @seealso \code{\link{ewma_sn_simulation}}, \code{\link{plot_chart}}
#'
#' @examples
#' # Generate example data
#' set.seed(42)
#' X <- matrix(rnorm(100), nrow = 10)
#'
#' # Calculate EWMA for grouped data
#' ewma_sn(X, med = 0, lambda = 0.05, L = 2.667, group_by_col = FALSE, side = 'two.sided')
#'
#' @importFrom graphics plot
#' @importFrom stats colSums
#' @importFrom base stop
#' @export
ewma_sn <- function(X,
                    med = 0,
                    lambda = 0.05,
                    L = 2.667,
                    group_by_col = FALSE,
                    side = 'two.sided') {
  if (group_by_col) {
    X = t(X)
  }

  n_plus = colSums(X > med)
  n_less = colSums(X < med)

  Sn = n_plus - n_less
  Z = lambda * Sn
  Z = c(0, Z)

  for (t in 2:length(Z)) {
    Z[t] = Z[t] + (1 - lambda) * Z[t - 1]
  }

  if (side == "two.sided") {
    ucl = L * sqrt(lambda * nrow(X) * (1 - (1 - lambda) ^ (2 *
                                                             length(Sn))) / (2 - lambda))
    lcl = -ucl
  } else if (side == "lower") {
    for (t in 2:length(Z)) {
      Z[t] = min(0, Z[t])
    }
    ucl = L * sqrt(lambda * (1 - (1 - lambda) ^ 2 * length(Sn)) / (2 - lambda))
    lcl = -ucl
    ucl = NULL
  } else if (side == "upper") {
    for (t in 2:length(Z)) {
      Z[t] = max(0, Z[t])
    }
    ucl = L * sqrt(lambda * (1 - (1 - lambda) ^ 2 * length(Sn)) / (2 - lambda))
    lcl = NULL
  } else {
    stop("Invalid argument for side. Must be one of 'two.sided',
         'lower', or 'upper'")
  }

  plot_chart(statistics = Z[-1], ic = 0,
             ucl = ucl, lcl = lcl,
             name = "EWMA for grouped data (Based on signs)")

  far = 1/mean(ewma_sn_simulation(lambda = lambda, L = L, n = nrow(X)))
}

