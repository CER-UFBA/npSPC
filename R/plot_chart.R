#' Plot Control Chart
#'
#' Plots a control chart with control limits and statistics.
#'
#' @param ic The value for the process center or target.
#' @param statistics A numeric vector of statistics to be plotted.
#' @param ucl The upper control limit.
#' @param lcl The lower control limit.
#' @param name A character string specifying the name of the statistics (default: NULL).
#' @examples
#' statistics <- c(1.2, 1.5, 1.4, 1.7, 2.0)
#' ucl <- 2.5
#' lcl <- 1.0
#' plot_chart(ic = 1.8, statistics = statistics, ucl = ucl, lcl = lcl, name = "Data")
#' @export
plot_chart = function(ic, statistics, ucl, lcl, name = NULL){
  plot(1:length(statistics), statistics, type = c('l'),
       ylim = c(min(lcl, min(statistics)), max(ucl, max(statistics))),
       xlab = "Groups", ylab = paste0(name, " Statistics"),
       main = "Control Chart")

  points(1:length(statistics), statistics, pch = 16, cex = 0.8,
         col = ifelse((statistics < lcl) | (statistics > ucl), "red", "black"))

  abline(h = c(lcl, ic, ucl), lty = c(2, 3, 2))
}
