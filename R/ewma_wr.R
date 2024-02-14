#' EWMA Control Chart based on the Wilcoxon Rank Sum Test
#'
#' This function generates an EWMA (Exponentially Weighted Moving Average)
#' Control Chart on the Wilcoxon Rank Sum Test.
#' It compares a set of data points to a reference value and plots the control chart.
#'
#' @param X The reference value.
#' @param Y The data matrix or vector to be compared.
#' @param lambda The weight parameter for the EWMA calculation
#' (default is 0.05).
#' @param L The control limit multiplier
#' (default is 2.447).
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
#' # Generate EWMA Control Chart based on the Wilcoxon Rank Sum Test
#' X <- 10
#' Y <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' ewma_wr(X, Y, lambda = 0.1, L = 3, plot = TRUE, side = 'two.sided')
#'
ewma_wr = function(X,
                   Y,
                   lambda = 0.05,
                   L = 2.447,
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

  mu_W = n*(m + n + 1) / 2
  sigma_W = m * n * (m + n + 1)/12

  Z = c(lambda * Wj[1] + (1 - lambda) * mu_W, rep(0, j - 1))

  for (t in 2:length(Z)) {
    Z[t] = lambda * Wj[t] + (1 - lambda) * Z[t - 1]
  }

  #Nplus = ifelse(Sn_plus > 0, 1, 0)
  #Nminus = ifelse(Sn_minus < 0, 1, 0)

  #Nplus = ave(Nplus, cumsum(Nplus == 0), FUN = cumsum)
  #Nminus = ave(Nminus, cumsum(Nminus == 0), FUN = cumsum)
  cl = rep(mu_W, length(Z))
  if (side == "two.sided") {
    aux = L * sqrt(sigma_W * (lambda / (2 - lambda) * (1 - (1 - lambda)^(2*(1:j)))) )
    ucl = cl + aux
    lcl = cl - aux
  } else if (side == "lower") {
    for (t in 1:length(Z)) {
      Z[t] = min(cl[t], Z[t])
    }
    lcl = cl - L * sqrt(sigma_W * (lambda / (2 - lambda) * (1 - (1 - lambda)^(2*(1:j)))) )
    ucl = NULL
  } else if (side == "upper") {
    for (t in 1:length(Z)) {
      Z[t] = max(cl[t], Z[t])
    }
    ucl = cl + L * sqrt(sigma_W * (lambda / (2 - lambda) * (1 - (1 - lambda)^(2*(1:j)))) )
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
      name = "EWMA Control Chart (based on Wilcoxon rank statistics)"
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
