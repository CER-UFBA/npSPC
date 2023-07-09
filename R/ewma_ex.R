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
  aux = ifelse(n%%2 == 0, r, r + 0.5)
  
  #estimating U
  Xr = quantile(X, aux/m)
  U = (Y > Xr) |> colSums()
  
  Z = lambda * U
  Z[1] = Z[1] + 2.5 * (1 - lambda)
  
  a = r/(m + 1)
  
  for (t in 2:length(Z)) {
    Z[t] = Z[t] + (1 - lambda) * Z[t - 1]
  }
  
  if (side == "two.sided") {
    cl = n * (1 - a) * (1 - (1 - lambda)^1:j)
    ucl = cl + L * sqrt(n * a * (1 - a) * (n * (1 - (1 - lambda)^1:j)^2) +
                          (lambda * (m + 1))/(2 - lambda) * 
                          (1 - (1 - lambda)^(2*1:j))/(m + 2))
    lcl = -ucl
  } else if (side == "lower") {
    for (t in 1:length(Z)) {
      Z[t] = min(0, Z[t])
    }
    lcl = -(cl + L * sqrt(n * a * (1 - a) * (n * (1 - (1 - lambda)^1:j)^2) +
                            (lambda * (m + 1))/(2 - lambda) * 
                            (1 - (1 - lambda)^(2*1:j))/(m + 2)))
    ucl = NULL
  } else if (side == "upper") {
    for (t in 1:length(Z)) {
      Z[t] = max(0, Z[t])
    }
    ucl = cl + L * sqrt(n * a * (1 - a) * (n * (1 - (1 - lambda)^1:j)^2) +
                          (lambda * (m + 1))/(2 - lambda) * 
                          (1 - (1 - lambda)^(2*1:j))/(m + 2))
    lcl = NULL
  } else {
    stop("Invalid argument for side. Must be one of 'two.sided',
         'lower', or 'upper'")
  }
  
  if (plot) {
    plot_chart(
      side = side,
      statistics = Z,
      ic = n * (1 - a),
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
  
  print(Z)
  
}

X = c(74.030,74.002,74.019,73.992,74.008,
      73.995,73.992,74.001,74.011,74.004,
      73.998,74.024,74.021,74.005,74.002,
      74.002,73.996,73.993,74.015,74.009,
      73.992,74.007,74.015,73.989,74.014,
      74.009,73.994,73.997,73.985,73.993,
      73.995,74.006,73.994,74.000,74.005,
      73.985,74.003,73.993,74.015,73.988,
      74.008,73.995,74.009,74.005,74.004,
      73.998,74.000,73.990,74.007,73.995,
      73.994,73.998,73.994,73.995,73.990,
      74.004,74.000,74.007,74.000,73.996,
      73.983,74.002,73.998,73.997,74.012,
      74.006,73.967,73.994,74.000,73.984,
      74.012,74.014,73.998,73.999,74.007,
      74.000,73.984,74.005,73.998,73.996,
      73.994,74.012,73.986,74.005,74.007,
      74.006,74.010,74.018,74.003,74.000,
      73.984,74.002,74.003,74.005,73.997,
      74.000,74.010,74.013,74.020,74.003,
      73.982,74.001,74.015,74.005,73.996,
      74.004,73.999,73.990,74.006,74.009,
      74.010,73.989,73.990,74.009,74.014,
      74.015,74.008,73.993,74.000,74.010,
      73.982,73.984,73.995,74.017,74.013)


X = matrix(X, nrow = 5,byrow=F)

Y = c(74.012,74.015,74.030,73.986,74.000,
      73.995,74.010,73.990,74.015,74.001,
      73.987,73.999,73.985,74.000,73.990,
      74.008,74.010,74.003,73.991,74.006,
      74.003,74.000,74.001,73.986,73.997,
      73.994,74.003,74.015,74.020,74.004,
      74.008,74.002,74.018,73.995,74.005,
      74.001,74.004,73.990,73.996,73.998,
      74.015,74.000,74.016,74.025,74.000,
      74.030,74.005,74.000,74.016,74.012,
      74.001,73.990,73.995,74.010,74.024,
      74.015,74.020,74.024,74.005,74.019,
      74.035,74.010,74.012,74.015,74.026,
      74.017,74.013,74.036,74.025,74.026,
      74.010,74.005,74.029,74.000,74.020) |>
  matrix(nrow = 5, byrow = F)

Y |> dim()
X |> dim()

ewma_ex(X = X, Y = Y, r = 62, lambda = 0.1, L = 2.31)

