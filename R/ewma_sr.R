#' EWMA Control Chart Based on Signed-Ranks Statistic
#'
#' This function constructs and plots an EWMA Control Chart
#' based on the signed-ranks statistic.
#'
#' @param X A matrix or data frame of observations.
#' @param med The center value for the control chart. Default is 0.
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
#' # Create and plot an EWMA Control Chart based on signed-ranks statistic
#' data <- matrix(rnorm(100), ncol = 5)
#' ewma_sr(data, med = 0, lambda = 0.05, L = 2.472,
#' group_by_col = FALSE, plot = TRUE, side = "two.sided")
#'
#' @import stats
#' @importFrom graphics plot
#' @importFrom stats wilcox.test
#'
#' @export
#'

ewma_sr <- function(X,
                    med = 0,
                    lambda = 0.05,
                    L = 2.472,
                    group_by_col = FALSE,
                    plot = TRUE,
                    side = 'two.sided') {

  if(group_by_col){
    X <- t(X)
  }

  n <- nrow(X)
  m <- ncol(X)

  SRi <- rep(NA, m)
  Z <- rep(0, m + 1)
  for(i in 1:m){
    t_plus <- wilcox.test(X[, i], mu = med)$statistic
    SRi[i] <- 2 * t_plus - (n * (n + 1)) / 2
    Z[i + 1] <- lambda * SRi[i] + (1 - lambda) * Z[i]
  }
  Z <- Z[-1]

  sigma <- sqrt(n * (n + 1) * (2 * n + 1) / 6)

  if (side == "two.sided") {
    ucl <- L * sigma * sqrt(lambda * (1 - (1 - lambda) ^
                                        (2 * 1:m)) / (2 - lambda))
    lcl <- -ucl
  } else if (side == "lower") {
    for (t in 1:m) {
      Z[t] <- min(0, Z[t])
    }
    lcl <- -(L * sigma * sqrt(lambda * (1 - (1 - lambda) ^
                                          (2 * 1:m)) / (2 - lambda)))
    ucl <- NULL
  } else if (side == "upper") {
    for (t in 1:m) {
      Z[t] <- max(0, Z[t])
    }
    ucl <- L * sigma * sqrt(lambda * (1 - (1 - lambda) ^
                                        (2 * 1:m)) / (2 - lambda))
    lcl <- NULL
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
      name = "EWMA Control Chart (based on signed-ranks statistics)"
    )
  } else {
    if (side == 'two.sided') {
      return(which(Z > ucl | Z < lcl)[1])
    } else if (side == 'upper') {
      return(which(Z > ucl)[1])
    } else {
      return(which(Z < lcl)[1])
    }
  }
}
