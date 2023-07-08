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
