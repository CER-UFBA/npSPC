#' ARL0 Estimation for EWMA SN Control Chart
#'
#' This function estimates the Average Run Length (ARL0) for the
#' EWMA SN control chart based on simulations.
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
#' @return The estimated ARL0 value based on the simulations.
#'
#' @examples
#' # Estimate ARL0 for EWMA SN control chart
#' ewma_sn_arl_calc(n = 1, lambda = 0.1, L = 2.667, n_sim = 500,
#' side = 'upper')
#'
#' @import purrr
#' @importFrom stats mean
#'
#' @export
#'

ewma_sn_arl_calc <- function(n,
                             lambda = 0.05,
                             L = 2.472,
                             t = 10000,
                             n_sim = 1000,
                             side = 'two.sided') {
  arl <- rep(NA, n_sim)
  X <- array(rnorm(n * t * n_sim), dim = c(n, t, n_sim))
  for (i in 1:n_sim) {
    arl[i] <- ewma_sn(X = matrix(X[, , i], nrow = n, ncol = t),
                      lambda = lambda, L = L,
                      plot = FALSE, side = side)
    if (is.na(arl[i])) {
      warning(paste0("There is NA in your simulation ", i,
                     ". Please try a lower L value or a higher t value. ",
                     "Returning the ARL0 without the NA."))
      break
    }
  }

  mean(arl, na.rm = TRUE)
}
#' Average Run Length (ARL) Calculation for EWMA Control Chart
#'
#' Calculates the average run length (ARL) for the Exponentially Weighted Moving Average (EWMA) control chart.
#'
#' @param lambda The decay factor for the EWMA. Default is 0.1.
#' @param L The multiplier for the control limits. Default is 2.667.
#' @param n The number of observations in each simulation. Default is 1.
#' @param sim The number of simulations to perform. Default is 10000.
#'
#' @return A numeric vector representing the average run length (ARL) for each simulation.
#'
#' @details The function calculates the average run length (ARL) for the EWMA control chart.
#'   It performs multiple simulations based on the specified parameters and returns the ARL for each simulation.
#'   The control limits are calculated based on the lambda and L parameters.
#'   The ARL is the number of observations until the control chart signals an out-of-control condition.
#'
#' @examples
#' # Calculate ARL for EWMA control chart
#' ewma_sn_arl_calc(lambda = 0.1, L = 2.667, n = 1, sim = 10000)
#'
#' @importFrom stats rnorm rowSums
#' @export
ewma_sn_arl_calc <- function(lambda = 0.1,
                             L = 2.667,
                             n = 1,
                             sim = 10000) {

  ucl = L * sqrt(lambda * n * (1 - (1 - lambda) ^ 2) / (2 - lambda))
  lcl = -ucl

  runlength = numeric(sim)
  xi = matrix(rnorm(sim * n, mean, 1), nrow = n)
  ti = rowSums(xi >= 0)
  sni = 2 * ti - n

  zi = numeric(sim)
  indicator = numeric(sim)

  for (j in 1:sim) {
    if (j == 1) {
      zi[j] <- lambda * sni[j]
    } else {
      zi[j] <- lambda * sni[j] + (1 - lambda) * zi[j - 1]
    }

    if (zi[j] > UCL_steady | zi[j] < LCL_steady) {
      indicator[j] <- 1
    }

    if (any(indicator)) {
      runlength[j] <- which.max(indicator == 1)
    } else {
      runlength[j] <- sim
    }
  }

  return(runlength)
}
