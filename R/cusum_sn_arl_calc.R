#' Average Run Length (ARL) Calculation for CUSUM Control Chart
#'
#' Calculates the average run length (ARL) for the CUSUM Control Chart.
#'
#' @param n The number of observations in each group. Default is 10.
#' @param k The reference value for the change detection. Default is 0.5.
#' @param h The control limit value. Default is 3.
#' @param t The number of samples (lower values are problematic).
#' @param n_sim The number of simulations to perform.
#' @param side The side of the control chart, can be "two.sided",
#' "lower", or "upper".
#'
#' @return The average run length (ARL) for the CUSUM Control Chart.
#'
#' @details The function calculates the average run length (ARL) for
#'   the CUSUM Control Chart.
#'   It performs multiple simulations based on the specified
#'   parameters and returns the average ARL.
#'   Each simulation consists of generating random data with
#'   the given number of observations (`n`)
#'   and calculating the CUSUM Control Chart using the `cusum_sn` function.
#'   The ARL is the average number of observations until the control
#'   chart signals an out-of-control condition.
#'   If NA values occur during the simulations, a warning message
#'   will be displayed, and those NA values will be excluded from the
#'   calculation.
#'
#' @examples
#' # Calculate ARL for CUSUM Control Chart
#' cusum_sn_arl_calc(n = 1, k = 0.5, h = 3, t = 10000, n_sim = 10000,
#'  group_by_col = FALSE, side = 'two.sided')
#'
#' @importFrom stats rnorm matrix
#' @export

cusum_sn_arl_calc <- function(n = 10,
                              k = 0.5,
                              h = 3,
                              t = 10000,
                              n_sim = 10000,
                              group_by_col = F,
                              side = 'two.sided') {
  arl = NULL
  for (i in 1:n_sim) {
    slice = matrix(rnorm(n * t), nrow = n, ncol = t)
    arl <- c(arl, cusum_sn(slice, k = k, h = h, plot = FALSE, 
                           side = "two.sided"))
    if (is.na(arl[i])) {
      warning(paste0(
          "There is NA in your simulation ", i,
          ". Please try a lower h value or a higher k value. ",
          "Returning the ARL0 without the NA."
        )
      )
      break
    }
  }
  return(mean(arl, na.rm = T))
}

cusum_sn_arl_calc(k = 6, h = 4, n = 10, t = 6000, n_sim = 1000)
