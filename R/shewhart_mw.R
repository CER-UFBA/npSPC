#shewhart_mw

X = matrix(rnorm(100), nrow = 20) |> data.frame()

phase1 = X[, 1:2]
phase2 = X[, -seq(1:2)]

phase1 = phase1 |> unlist()

sapply(phase2, function(x) wilcox.test(phase1, x)$statistic)



shewhart_mw = function(X, far = .0027, group_by_col = FALSE,
                       phase1_num_samples = NULL,
                       side = "two.sided"){
  
  X = X |> data.frame()
  
  if (group_by_col) {
    X = t(X)
  }
  
  if(is.null(phase1_num_samples)){
    #por enquanto vai receber 1
    #considerar uma função para calcular tamanho de amostra depois
    phase1_num_samples = 1
  }
  
  #phase1 and 2 samples
  phase1 = X[,1:phase1_num_samples] |> unlist()
  phase2 = X[, -seq(1:phase1_num_samples)]
  
  if (any(unlist(phase1) == unlist(phase2)) | any(is.na(X))) {
    warning("There is a tie or NA in your data. Caution with your results.")
  }
  
  #computing Mann Whitney statistitcs 
  mw_statistics = sapply(phase2, function(x) wilcox.test(phase1, x)$statistic)
  
  #computing ucl
  
  
  #computing LCL (mn_i - ucl)
  lcl = length(phase1)*sapply(phase2, function(x) sum(!is.na(x))) - ucl
  
  
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
  
  plot_chart(statistics = mw_statistics, ic = median(mw_statistics),
             ucl = ucl, lcl = lcl,
             name = "Shewhart MW (Based on Mann Whitney Statistic)")
  
}
