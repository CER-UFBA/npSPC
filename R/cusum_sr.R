#' CUSUM Control Chart for Grouped Data (Based on Rank Statistics)
#'
#' Calculates and plots the CUSUM Control Chart for grouped data based
#' on rank statistics.
#'
#' @param X A numeric matrix or data frame containing the data.
#' @param med The median value to use as the reference. Default is 0.
#' @param k The reference value for the change detection. Default is 0.5.
#' @param h The control limit value. Default is 5.
#' @param group_by_col A logical value indicating whether to treat
#' columns as groups. If TRUE, the matrix will be transposed. Default is FALSE.
#' @param plot A logical value indicating whether to plot the control chart.
#' Default is TRUE.
#' @param side The side of the chart to plot.
#' Must be one of 'two.sided', 'lower', or 'upper'. Default is 'two.sided'.
#'
#' @return If `plot` is set to FALSE, the function returns
#' the index of the first out-of-control observation.
#' If `plot` is set to TRUE, the function generates a control chart plot.
#'
#' @details The function calculates the CUSUM Control Chart for
#' grouped data based on rank statistics.
#' It calculates the rank statistic for each group using the Wilcoxon
#' rank sum test, and then applies the CUSUM algorithm to detect
#' changes in the process. The control limits are determined by
#' the value of `h` and the `side` parameter.
#' The function can plot the control chart using the `plot_chart`
#' function or return the index of the first out-of-control observation.
#'
#' @seealso \code{\link{plot_chart}}
#'
#' @examples
#' # Generate example data
#' set.seed(42)
#' X <- matrix(rnorm(100), nrow = 20)
#'
#' # Calculate CUSUM Control Chart for grouped data
#' cusum_sr(X, med = 0, k = 0.5, h = 5, group_by_col = FALSE,
#'          plot = TRUE, side = 'two.sided')
#'
#' @importFrom graphics plot
#' @importFrom stats wilcox.test
#' @importFrom base stop
#' @export

cusum_sr = function(X,
                    med = 0,
                    k = 0.5,
                    h = 5,
                    group_by_col = F,
                    plot = T,
                    side = 'two.sided') {
  if (group_by_col) {
    X = t(X)
  }

  n = nrow(X)
  m = ncol(X)

  Sr = rep(NA, m)

  for(i in 1:m){
    t_plus = wilcox.test(X[, i], mu = med)$statistic
    Sr[i] = 2 * t_plus - (n * (n + 1)) / 2
  }

  Sr_plus = rep(0, m - 1)
  Sr_minus = rep(0, m - 1)

  for (t in 2:m) {
    Sr_plus[t] = max(0, Sr_plus[t-1] + Sr[t] - k)
    Sr_minus[t] = min(0, Sr_minus[t-1] + Sr[t] + k)
  }

  #Nplus = ifelse(Sn_plus > 0, 1, 0)
  #Nminus = ifelse(Sn_minus < 0, 1, 0)

  #Nplus = ave(Nplus, cumsum(Nplus == 0), FUN = cumsum)
  #Nminus = ave(Nminus, cumsum(Nminus == 0), FUN = cumsum)

  if (side == "two.sided") {
    ucl = h
    lcl = -h
    statistics = Sr_plus
    statistics2 = Sr_minus
  } else if (side == "lower") {
    lcl = -h
    ucl = NULL
    statistics = Sr_minus
    statistics2 = NULL
  } else if (side == "upper") {
    ucl = h
    lcl = NULL
    statistics = Sr_plus
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
      name = "Cusum for grouped data (Based on signs)"
    )
  } else{
    if(side == 'two.sided'){
      return(which(Sr_plus >= ucl | Sr_minus <= lcl)[1])
    } else if (side == 'upper'){
      return(which(Sr_plus >= ucl)[1])
    } else{
      return(which(Sr_minus <= lcl)[1])
    }

  }

}
