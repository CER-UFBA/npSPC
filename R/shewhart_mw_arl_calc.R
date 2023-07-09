#' ARL Calculation for EWMA Control Chart Based on Signed-Ranks Statistic
#'
#' This function calculates the Average Run Length (ARL)
#' for the EWMA Control Chart based on the signed-ranks statistic.
#'
#' @param n The sample size.
#' @param lambda The smoothing parameter for the EWMA calculation.
#' Default is 0.05.
#' @param L The control limit multiplier for the UCL and LCL calculation.
#' @param t The number of samples (lower values are problematic).
#' @param n_sim The number of simulations to perform.
#' @param side The side of the control chart, can be "two.sided",
#' "lower", or "upper".
#'
#' @return The average run length (ARL) for the EWMA Control Chart.
#'
#' @examples
#' # Calculate the ARL for EWMA Control Chart based on signed-ranks statistic
#' ewma_sr_arl_calc(n = 10, lambda = 0.1, L = 2.794,
#'                  n_sim = 100, side = "two.sided")
#'
#' @import stats
#'
#' @export
#'

shewhart_mw_arl_calc <- function(m,
                                 n,
                                 cl,
                                 t = 10000,
                                 n_sim = 1000,
                                 side = 'two.sided') {
  arl = NULL
  for (i in 1:n_sim) {
    X = rnorm(m)
    Y = matrix(rnorm(n * t), nrow = n, ncol = t)
    
    arl <- c(arl, shewhart_mw(X, Y, cl = cl, plot = FALSE, 
                              side = side))
    if (is.na(arl[i])) {
      warning(
        paste0(
          "There is NA in your simulation ",
          i,
          ". Please try a lower h value or a higher k value. ",
          "Returning the ARL0 without the NA."
        )
      )
      break
    }
  }
  return(mean(arl, na.rm = T))
  
}
