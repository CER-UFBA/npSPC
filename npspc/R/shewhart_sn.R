# Shewhart Control Charts Based on Signs!
#


X = tibble(
  V1 = c(rnorm(20, 0, 1)),
  V2 = c(rnorm(20, 2, 2)),
  V3 = c(rnorm(20, -1, 2)),
  V4 = c(rnorm(10, 1, 1), rep(1, 10))
)

mu0 = 1



shewhart_sn = function(X, mu0, far = .0027, col_by_group = F){
  X = ifelse(col_by_group, t(X), X)
  n_plus = X |> purrr::map_dbl(\(x) sum(x > mu0, na.rm = T))
  n_less = X |> purrr::map_dbl(\(x) sum(x < mu0, na.rm = T))

  SN = n_plus - n_less

  if(any((X - mu0) == 0) | any(is.na(X))){
    warning('There is a tie or NA in your data. Caution with your results.')
  }

  quantil = max(which(pbinom(0:nrow(X), nrow(X), 1/2, lower.tail = F) >= far))
  quantil = min(nrow(X), quantil + 1)
  far = 2*pbinom(0:n, n, 1/2, lower.tail = F)[quantil]

  a = 2*quantil - nrow(X)
  arl = round(1/far)

  plot_chart(0, SN, a, -a)
}



