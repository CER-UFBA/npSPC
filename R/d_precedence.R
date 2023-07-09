#' @keywords internal
#' @rdname hidden_functions
#' @noRd
d_precedence <- function(m, n, j) {
  # Function body
  w = 0:m
  choose(w + j - 1, w) * choose(m + n - j - w, m - w) /
    choose(m + n, m)
}
