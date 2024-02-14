#' Shewhart Chart Based on Precedence Statistic
#'
#' This function generates a Shewhart control chart based on the precedence
#' statistic for a given set of data.
#'
#' @param X The reference value.
#' @param Y The data matrix or vector.
#' @param j The percentile value to calculate the
#' precedence statistic (default: round(nrow(Y)/2)).
#' @param far The false alarm rate (default: 0.0027).
#' @param group_by_col Logical value indicating whether to group the data by
#' columns (default: FALSE).
#' @param side The side of the control chart limits
#' ("two.sided", "lower", or "upper") (default: "two.sided").
#'
#' @return A character string indicating the used ARL0 (Average Run Length).
#'
#' @details The \code{shewhart_prec} function generates a Shewhart control
#' chart based on the precedence statistic.
#' The function calculates the precedence statistic for a given set of data
#' and constructs the control chart with
#' control limits based on the false alarm rate and the specified side.
#' It plots the control chart and returns
#' the used ARL0 (Average Run Length) as a character string.
#'
#' @examples
#' X <- rnorm(100)
#' Y <- matrix(rnorm(100), ncol = 5)
#' shewhart_prec(X, Y)
#'
#' @importFrom stats quantile
#'
#' @export

shewhart_prec <- function(X,
                          Y,
                          j = round(nrow(Y)/2, 0),
                          far = 0.0027,
                          group_by_col = FALSE,
                          side = "two.sided") {
  j = round(j, 0)
  if (j > nrow(Y)) {
    stop("j needs to be integer and lower than Y sample's size.")
  }

  if(group_by_col){
    Y <- t(Y)
  }

  X = c(X)
  m = length(X)
  n = nrow(Y)

  #define aux to get the right percentile
  aux = ifelse(n%%2 == 0, j, j + 0.5)

  #estimating w
  Yj = apply(Y, 2, quantile, aux/n)

  w = sapply(Yj, \(y) sum(X > y))

  if (side == "two.sided") {

    #estimating limits
    a = max(max(which(cumsum(d_precedence(m, n, j)) < far/2)), 0,
            na.rm = T)
    b = min(which(cumsum(d_precedence(m, n, j)) > 1 - far/2)[1], m,
            na.rm = T)

    lcl = rep(a, length(w))
    ucl = rep(b-1, length(w))

  } else if (side == "lower") {

    a = max(max(which(cumsum(d_precedence(m, n, j)) < far)), 0,
            na.rm = T)
    ucl <- NULL
    lcl <- rep(a, length(w))

  } else if (side == "upper") {

    b <- min(which(cumsum(d_precedence(m, n, j)) > 1 - far)[1], m,
             na.rm = T)
    ucl = rep(b-1, length(w))
    lcl <- NULL

  } else {
    stop("Invalid argument for side.
         Must be one of 'two.sided', 'lower', or 'upper'")
  }

  arl <- round(1 / far)

  plot_chart(statistics = w,
             ic = rep(which(cumsum(d_precedence(m, n, j)) >= 0.5)[1], length(w)),
             ucl = ucl, lcl = lcl,
             name = "Shewhart Control Chart (based on precedence statistics)",
             side = side)

  arl = paste0('Used ARL0: ', arl)
  return(arl)
}
