#' Shewhart Control Chart Based on SN Statistic
#'
#' This function constructs and plots a Shewhart Control Chart based on the SN statistic.
#'
#' @param X A matrix or data frame of observations.
#' @param med The center value for the control chart. Default is 0.
#' @param far The false alarm rate for control limits calculation. Default is 0.0027.
#' @param group_by_col Logical indicating whether the observations are grouped by column. 
#'                     If TRUE, the observations are transposed before further processing. Default is FALSE.
#' @param side The side of the control chart, can be "two.sided", "lower", or "upper".
#'
#' @return The Average Run Length (ARL) for the control chart.
#'
#' @examples
#' # Create and plot a Shewhart Control Chart based on SN statistic
#' data <- matrix(rnorm(100), ncol = 2)
#' shewhart_sn(data, med = 0, far = 0.0027, group_by_col = FALSE, side = "two.sided")
#'
#' @import purrr
#' @importFrom stats round
#'
#' @export
#' 

shewhart_sn <- function(X,
                        med = 0,
                        far = 0.0027,
                        group_by_col = FALSE,
                        side = "two.sided") {
  
  if (group_by_col) {
    X <- t(X)
  }
  
  n_plus <- colSums(X > med)
  n_less <- colSums(X < med)
  
  SN <- n_plus - n_less
  
  if (any(unlist(X) == med) || any(is.na(X))) {
    warning("There is a tie or NA in your data. Caution with your results.")
  }
  
  quantil <- max(which(pbinom(0:nrow(X), nrow(X), 1/2, 
                              lower.tail = FALSE) >= far))
  quantil <- min(nrow(X), quantil + 1)
  
  far <- 2 * pbinom(0:nrow(X), nrow(X), 1/2, 
                    lower.tail = FALSE)[quantil]
  
  if (side == "two.sided") {
    ucl <- 2 * quantil - nrow(X)
    ucl <- rep(ucl, length(SN))
    lcl <- -ucl
  } else if (side == "lower") {
    ucl <- NULL
    lcl <- rep(-quantil, length(SN))
  } else if (side == "upper") {
    ucl <- rep(quantil, length(SN))
    lcl <- NULL
  } else {
    stop("Invalid argument for side. 
         Must be one of 'two.sided', 'lower', or 'upper'")
  }
  
  arl <- round(1 / far)
  
  plot_chart(statistics = SN, ic = 0,
             ucl = ucl, lcl = lcl,
             name = "SNi (Based on signs)",
             side = side)
  
  arl = paste0('ARL0 utilizado: ', arl)
  return(arl)
}


#X = matrix(rnorm(10000), nrow = 50)
#shewhart_sn(X, group_by_col = F, side = 'two.sided')
