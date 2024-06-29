#' Symbols visible from cell e
#'
#' @param R A Room square.
#' @param e A cell in R.
#'
#' @return A list of symbols visible in R from cell e.
see <- function(R, e) {
  first <- R[R$row == e[1] | R$col == e[2], "first"]$first
  first_non_na <- first[!is.na(first)]
  second <- R[R$row == e[1] | R$col == e[2], "second"]$second
  second_non_na <- second[!is.na(second)]
  sort(unique(c(first_non_na, second_non_na)))
}

#' Symbols visible from cell (row, col) in R
#'
#' @param R A Room square.
#' @param row A row index.
#' @param col A column index.
#'
#' @return A list of symbols visible in R from cell (col, rol).
see2 <- function(R, row, col) {
  first <- R[R$row == row | R$col == col, "first"]$first
  first_non_na <- first[!is.na(first)]
  second <- R[R$row == row | R$col == col, "second"]$second
  second_non_na <- second[!is.na(second)]
  sort(unique(c(first_non_na, second_non_na)))
}