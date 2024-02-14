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
#' shewhart_mw(X, Y)
#'
#' @importFrom stats quantile
#'
#' @export

shewhart_mw <- function(X,
                        Y,
                        cl,
                        far = 0.0027,
                        group_by_col = FALSE,
                        plot = T,
                        side = "two.sided") {

  if(group_by_col){
    Y <- t(Y)
  }

  X = c(X)
  m = length(X)
  n = nrow(Y)

  if(cl < m*n/2 & side != 'lower'){
    warning(paste("Your cl is lower than the minimum UCL" ,
                  "for upper and two sided charts.",
                  "\nWe are using the minimum possible UCL."))
    cl = m*n/2 + 1
  }

  U = unlist(lapply(apply(Y, 2, wilcox.test, X), '[', 1))

  if (side == "two.sided") {

    ucl = rep(cl, length(U))
    lcl = rep(m*n - cl, length(U))

  } else if (side == "lower") {

    ucl <- NULL
    lcl <- rep(cl, length(U))

  } else if (side == "upper") {

    ucl = rep(cl, length(U))
    lcl <- NULL

  } else {
    stop("Invalid argument for side.
         Must be one of 'two.sided', 'lower', or 'upper'")
  }

  if (plot) {
    plot_chart(
      side = side,
      statistics = U,
      ic = m*n/2,
      ucl = ucl,
      lcl = lcl,
      name = "Shewhart Control Chart (based on Mann-Whitney statistics)"
    )
  } else {
    if (side == 'two.sided') {
      return(which(U > ucl | U < lcl)[1])
    } else if (side == 'upper') {
      return(which(U > ucl)[1])
    } else {
      return(which(U < lcl)[1])
    }
  }
}
