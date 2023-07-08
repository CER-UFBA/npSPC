#' Shewhart Control Chart based on Mann-Whitney Statistic
#'
#' Calculates and plots the Shewhart Control Chart based on the Mann-Whitney statistic.
#'
#' @param X A numeric matrix or data frame containing the data.
#' @param far The desired false alarm rate for the control chart. Default is 0.0027.
#' @param group_by_col A logical value indicating whether to treat columns as groups.
#'   If TRUE, the matrix will be transposed. Default is FALSE.
#' @param phase1_num_samples The number of samples to include in phase 1. Default is NULL.
#' @param side The side of the chart to plot. Must be one of 'two.sided', 'lower', or 'upper'.
#'   Default is 'two.sided'.
#'
#' @details The function calculates the Shewhart Control Chart based on the Mann-Whitney statistic.
#'   It divides the data into two phases: phase 1 and phase 2.
#'   Phase 1 consists of the specified number of samples, and phase 2 consists of the remaining samples.
#'   The Mann-Whitney statistic is computed between each sample in phase 2 and the phase 1 samples.
#'   The control limits are calculated based on the desired false alarm rate (far).
#'   The control chart is then plotted using the plot_chart function.
#'
#' @seealso \code{\link{plot_chart}}
#'
#' @examples
#' # Generate example data
#' set.seed(42)
#' X <- matrix(rnorm(100), nrow = 20)
#'
#' # Calculate Shewhart Control Chart based on Mann-Whitney statistic
#' shewhart_mw(X, far = 0.0027, group_by_col = FALSE, phase1_num_samples = 2, side = 'two.sided')
#'
#' @importFrom graphics plot
#' @importFrom stats wilcox.test
#' @importFrom base warning stop
#' @export
shewhart_mw <- function(X, far = 0.0027, group_by_col = FALSE,
                        phase1_num_samples = NULL,
                        side = "two.sided") {

  X = X |> data.frame()

  if (group_by_col) {
    X = t(X)
  }

  if (is.null(phase1_num_samples)) {
    phase1_num_samples = 1
  }

  phase1 = X[, 1:phase1_num_samples] |> unlist()
  phase2 = X[, -seq(1:phase1_num_samples)]

  if (any(unlist(phase1) == unlist(phase2)) | any(is.na(X))) {
    warning("There is a tie or NA in your data. Caution with your results.")
  }

  mw_statistics = sapply(phase2, function(x) wilcox.test(phase1, x)$statistic)

  ucl = length(phase1) * sapply(phase2, function(x) sum(!is.na(x))) - ucl

  if (side == "two.sided") {
    a = 2 * quantil - nrow(X)
    la = -a
  } else if (side == "lower") {
    a = quantil
    la = NULL
  } else if (side == "upper") {
    a = NULL
    la = -quantil
  } else {
    stop("Invalid argument for side. Must be one of 'two.sided', 'lower', or 'upper'")
  }

  arl = round(1 / far)

  plot_chart(statistics = mw_statistics, ic = median(mw_statistics),
             ucl = ucl, lcl = lcl,
             name = "Shewhart MW (Based on Mann-Whitney Statistic)")
}
