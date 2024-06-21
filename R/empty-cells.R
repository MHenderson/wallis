#' Empty cells of a partial Room square
#'
#' @param R A partial Room square.
#'
#' @return A list of empty cells of R.
#' @export
empty_cells <- function(R) {
  E <- R[is.na(R$first), ]
  E <- mapply(c, E$row, E$col, SIMPLIFY = FALSE)
  return(E)
}