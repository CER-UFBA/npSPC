#' Shewhart SN chart
#'
#' This function produces a Shewhart SN chart to monitor the process median
#' in the presence of special cause variation.
#'
#' @param X a matrix or data frame of observations.
#' @param med the process median, if known.
#' If NULL, it will be estimated using the median of the observations.
#' @param far the false alarm rate for detecting special cause variation.
#' Default is 0.0027.
#' @param group_by_col a logical indicating whether each
#' column represents a group. Default is FALSE (each row is a group).
#' @param side a character string indicating the alternative
#' hypothesis for detecting special cause variation.
#' It can be "two.sided", "lower", or "upper". Default is "two.sided".
#'
#' @return This function returns a Shewhart SN chart.
#'
#' @details The Shewhart SN chart is a type of control chart that
#' monitors the process median.
#' It is based on the difference between the number of observations
#' above and below the process median, called the SN statistic.
#' The chart has two horizontal lines that represent the control
#' limits for the SN statistic, which are determined based on the
#' false alarm rate specified by the user.
#' If the SN statistic exceeds the control limits,
#' it indicates the presence of special cause variation.
#'
#' @examples
#' # Generate random data
#' set.seed(123)
#' X <- matrix(rnorm(100), nrow = 10)
#'
#' # Create a Shewhart SN chart
#' shewhart_sn(X)
#'
#' @import dplyr
#' @importFrom purrr map_dbl
#' @importFrom stats median pbinom
#'
#' @export
#'
shewhart_sn = function(X, med = NULL, far = .0027, group_by_col = FALSE,
                       side = c("two.sided", "lower", "upper")){

  if (is.null(med)) {
    med = X |> purrr::map_dbl(\(x) median(x, na.rm = TRUE)) |>
      median(na.rm = TRUE)
  }

  if (group_by_col) {
    X = t(X)
  }

  n_plus = colSums(X > med)
  n_less = colSums(X < med)

  SN = n_plus - n_less

  if (any(unlist(X) == med) | any(is.na(X))) {
    warning("There is a tie or NA in your data. Caution with your results.")
  }

  quantil = max(which(pbinom(0:nrow(X),
                             nrow(X), 1/2, lower.tail = FALSE) >= far))
  quantil = min(nrow(X), quantil + 1)

  far = 2*pbinom(0:nrow(X),
                 nrow(X), 1/2, lower.tail = FALSE)[quantil]

  if (side == "two.sided") {
    a = 2*quantil - nrow(X)
    la = -a
  } else if (side == "lower") {
    a = quantil
    la = NULL
  } else if (side == "upper") {
    a = NULL
    la = -quantil
  } else {
    stop("Invalid argument for side. Must be one of 'two.sided',
         'lower', or 'upper'")
  }

  arl = round(1/far)

  plot_chart(0, SN, a, la)

}

#X = matrix(rnorm(100), nrow = 20)
#shewhart_sn(X, group_by_col = F, side = 'lower')
