#' Control Chart Plotting Function
#'
#' Plots the control chart based on the specified statistics and control limits.
#'
#' @param ic The in-control value for the control chart.
#' @param statistics A numeric vector of control statistics.
#' @param statistics2 A numeric vector of additional control statistics (optional).
#' @param ucl The upper control limit value.
#' @param lcl The lower control limit value.
#' @param name The name of the control chart (optional).
#' @param side The side of the chart to plot. Must be one of 'two.sided', 'lower', or 'upper'.
#'
#' @details The function plots the control chart based on the specified control statistics and control limits.
#'   It uses the `plot` function to create a line plot of the statistics.
#'   If additional statistics (`statistics2`) are provided, they are also plotted.
#'   The control limits are represented by dashed lines.
#'   The in-control value (`ic`) is represented by a dotted line.
#'   The points on the plot are colored red if they are outside the control limits, indicating an out-of-control condition.
#'
#' @importFrom graphics plot lines points abline
#' @export
plot_chart = function(ic, statistics, statistics2 = NULL,
                      ucl, lcl, name = NULL, side){
  if(length(ic) != length(statistics)){
    ic = rep(ic, length(statistics))
  }

  plot(1:length(statistics), statistics, type = c('l'),
       ylim = c(min(lcl, statistics, statistics2 ),
                max(ucl, statistics, statistics2)),
       xlab = "Groups", ylab = name,
       main = "Control Chart")

  if(!is.null(statistics2)){
    lines(1:length(statistics2), statistics2)
    points(1:length(statistics2), statistics2, pch = 16, cex = 0.8,
           col = ifelse((statistics2 <= lcl) |
                          (statistics2 >= ucl), "red", "black"))
    lines(1:length(statistics2), ucl, lty = 2)
    lines(1:length(statistics2), lcl, lty = 2)
  }

  lines(1:length(statistics), ic, lty = 3)
  if(side == 'two.sided'){
    points(1:length(statistics), statistics, pch = 16, cex = 0.8,
           col = ifelse((statistics <= lcl) |
                          (statistics >= ucl), "red", "black"))
    lines(1:length(statistics), ucl, lty = 2)
    lines(1:length(statistics), lcl, lty = 2)
  } else if(side == 'upper'){
    points(1:length(statistics), statistics, pch = 16, cex = 0.8,
           col = ifelse((statistics >= ucl), "red", "black"))
    lines(1:length(statistics), ucl, lty = 2)
  } else{
    points(1:length(statistics), statistics, pch = 16, cex = 0.8,
           col = ifelse((statistics <= lcl), "red", "black"))
    lines(1:length(statistics), lcl, lty = 2)
  }

}
