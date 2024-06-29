#' Is pair p available in R at cell e?
#'
#' @param R A partial Room square.
#' @param p A pair.
#' @param e An empty cell of R.
#'
#' @return True if and only if the pair p can be placed in cell e in R.
avail <- function(R, p, e) {
  available <- R[R$row == e[1] & R$col == e[2], "avail"]$avail[[1]]
  p[1] %in% available && p[2] %in% available
}
