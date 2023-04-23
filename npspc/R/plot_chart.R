plot_chart = function(ic, statistics, ucl, lcl){
  plot(1:length(statistics), statistics, type = c('l'),
       ylim = c(min(lcl, min(statistics)), max(ucl, max(statistics))))

  points(1:length(statistics), statistics,
         ylim = c(min(lcl, min(statistics)), max(ucl, max(statistics))))
  abline(h = c(lcl, ucl), lty = 2)
  abline(h = ic, lty = 3)
}
