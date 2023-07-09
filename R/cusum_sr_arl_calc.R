#' Calculate Average Run Length (ARL) for CUSUM Control Chart (Based on Signed-ranks)
#'
#' This function calculates the Average Run Length (ARL) for a 
#' CUSUM control chart
#' based on signed-ranks. It generates random samples and applies the 
#' CUSUM control
#' chart procedure to each sample to calculate the ARL.
#'
#' @param n The sample size. Default is 10.
#' @param k The reference value for the CUSUM chart. Default is 0.5.
#' @param h The decision interval for the CUSUM chart. Default is 3.
#' @param t The number of observations per sample. Default is 10000.
#' @param n_sim The number of simulated samples. Default is 10000.
#' @param group_by_col A logical value indicating whether the samples 
#' are grouped by columns.
#'   If TRUE, each column represents a sample. If FALSE (default), 
#'   each row represents a sample.
#' @param side The side of the control chart. Must be one of 'two.sided', 
#' 'lower', or 'upper'.
#'   Default is 'two.sided'.
#'
#' @return The average run length (ARL) of the CUSUM control chart.
#'
#' @details
#' The function generates \code{n_sim} random samples of size \code{n} 
#' and applies
#' the CUSUM control chart procedure to each sample. 
#' The CUSUM control chart is based on
#' signed-ranks and uses the reference value \code{k} and the 
#' decision interval \code{h}.
#' The ARL is calculated as the average number of observations until 
#' the CUSUM chart signals
#' out-of-control.
#'
#' @examples
#' arl <- cusum_sr_arl_calc(n = 10, k = 0.5, h = 3, t = 10000, 
#'                         n_sim = 10000, side = 'two.sided')
#' arl
#'
#' @seealso
#' \code{\link{cusum_sr}} for the CUSUM control chart procedure 
#' based on signed-ranks.
#'
#' @export
cusum_sr_arl_calc <- function(n = 10,
                              k = 0.5,
                              h = 3,
                              t = 10000,
                              n_sim = 10000,
                              group_by_col = FALSE,
                              side = 'two.sided') {
  arl = NULL
  for (i in 1:n_sim) {
    slice = matrix(rnorm(n * t), nrow = n, ncol = t)
    arl <- c(arl, cusum_sr(slice, k = k, h = h, plot = FALSE, 
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
  return(mean(arl, na.rm = TRUE))
}
