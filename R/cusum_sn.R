#' CUSUM Control Chart for Grouped Data (Based on Signs)
#'
#' Calculates and plots the CUSUM Control Chart for grouped data based on signs.
#'
#' @param X A numeric matrix or data frame containing the data.
#' @param med The median value to use as the reference.
#' Default is 0.
#' @param k The reference value for the change detection.
#' Default is 0.5.
#' @param h The control limit value.
#' Default is 5.
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
#' @details The function calculates the CUSUM Control Chart
#' for grouped data based on signs. It calculates the number of observations
#' above and below the median for each group, and then applies the CUSUM
#' algorithm to detect changes in the process.
#' The control limits are determined by the value
#' of `h` and the `side` parameter.
#' The function can plot the control chart
#' using the `plot_chart` function or return the index
#' of the first out-of-control observation.
#'
#' @seealso \code{\link{plot_chart}}
#'
#' @examples
#' # Generate example data
#' set.seed(42)
#' X <- matrix(rnorm(100), nrow = 20)
#'
#' # Calculate CUSUM Control Chart for grouped data
#' cusum_sn(X, med = 0, k = 0.5, h = 5, group_by_col = FALSE,
#' plot = TRUE, side = 'two.sided')
#'
#' @importFrom graphics plot
#' @importFrom stats colSums
#' @importFrom base stop
#' @export

cusum_sn = function(X,
                   med = 0,
                   k = 0.5,
                   h = 5,
                   group_by_col = F,
                   plot = T,
                   side = 'two.sided') {
  if (group_by_col) {
    X = t(X)
  }

  n_plus = colSums(X > med)
  n_less = colSums(X < med)

  Sn = n_plus - n_less
  #SNplus = SNminus = c(0, rep(0, length(Sn) - 1))

  n = nrow(X)
  m = ncol(X)

  Sn_plus = rep(0, m - 1)
  Sn_minus = rep(0, m - 1)

  for (t in 2:m) {
    Sn_plus[t] = max(0, Sn_plus[t-1] + Sn[t] - k)
    Sn_minus[t] = min(0, Sn_minus[t-1] + Sn[t] + k)
  }

  #Nplus = ifelse(Sn_plus > 0, 1, 0)
  #Nminus = ifelse(Sn_minus < 0, 1, 0)

  #Nplus = ave(Nplus, cumsum(Nplus == 0), FUN = cumsum)
  #Nminus = ave(Nminus, cumsum(Nminus == 0), FUN = cumsum)

  if (side == "two.sided") {
    ucl = h
    lcl = -h
    statistics = Sn_plus
    statistics2 = Sn_minus
  } else if (side == "lower") {
    lcl = -h
    ucl = NULL
    statistics = Sn_minus
    statistics2 = NULL
  } else if (side == "upper") {
    ucl = h
    lcl = NULL
    statistics = Sn_plus
    statistics2 = NULL
  } else {
    stop("Invalid argument for side. Must be one of 'two.sided',
         'lower', or 'upper'")
  }

  if (plot) {
    plot_chart(side = side,
      statistics = statistics,
      statistics2 = statistics2,
      ic = 0,
      ucl = rep(ucl, length(statistics)),
      lcl = rep(lcl, length(statistics)),
      name = "Cusum for grouped data (Based on signs)"
    )
  } else{
    if(side == 'two.sided'){
      return(which(Sn_plus >= ucl | Sn_minus <= lcl)[1])
    } else if (side == 'upper'){
      return(which(Sn_plus >= ucl)[1])
    } else{
      return(which(Sn_minus <= lcl)[1])
    }

  }

}
