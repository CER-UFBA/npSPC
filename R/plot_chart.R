#' Plot Control Chart
#'
#' Plots a control chart with control limits and statistics.
#'
#' @param ic The value for the process center or target.
#' @param statistics A numeric vector of statistics to be plotted.
#' @param ucl The upper control limit.
#' @param lcl The lower control limit.
#' @param name A character string specifying the name of the statistics 
#' (default: NULL).
#' @examples
#' statistics <- c(1.2, 1.5, 1.4, 1.7, 2.0)
#' ucl <- 2.5
#' lcl <- 1.0
#' plot_chart(ic = 1.8, statistics = statistics, ucl = ucl, lcl = lcl, 
#' name = "Data")
#' @export
plot_chart = function(ic, statistics, statistics2 = NULL, 
                      ucl, lcl, name = NULL, side){
  plot(1:length(statistics), statistics, type = c('l'),
       ylim = c(min(lcl, min(statistics, na.rm = T), min(statistics2, na.rm = T )), 
                max(ucl, max(statistics, na.rm = T), max(statistics2, na.rm = T))),
       xlab = "Groups", ylab = paste0(name, " Statistics"),
       main = "Control Chart")

  if(!is.null(statistics2)){
    lines(1:length(statistics2), statistics2)
    points(1:length(statistics2), statistics2, pch = 16, cex = 0.8,
           col = ifelse((statistics2 <= lcl) | 
                          (statistics2 >= ucl), "red", "black"))
    lines(1:length(statistics2), ucl, lty = 2)
    lines(1:length(statistics2), lcl, lty = 2)
  }
  
  abline(h = ic, lty = 3)
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
