#' Does a row satisfy the latin constraint?
#'
#' @param R A Room square
#' @param i A row index
#'
#' @return True if and only if row i of R satisfies the latin constraint.
is_row_latin_i <- function(R, i) {
  R <- R |> tidyr::pivot_longer(first:second)
  u <- R[R$row == i, "value"]$value
  u <- u[!is.na(u)]
  length(u) == length(unique(u))
}

#' Does a column satisfy the latin constraint?
#'
#' @param R A Room square
#' @param i A column index
#'
#' @return True if and only if column i of R satisfies the latin constraint.
is_col_latin_i <- function(R, i) {
  R <- R |> tidyr::pivot_longer(first:second)
  u <- R[R$col == i, "value"]$value
  u <- u[!is.na(u)]
  length(u) == length(unique(u))
}

#' Is a Room square row latin?
#'
#' @param R A Room square
#'
#' @return True if and only if R is row latin.
is_row_latin <- function(R) {
  all(purrr::map_lgl(1:max(R$row), is_row_latin_i, R = R))
}

#' Is A Room square column latin?
#'
#' @param R A Room square
#'
#' @return True if and only if R is column latin.
is_col_latin <- function(R) {
  all(purrr::map_lgl(1:max(R$col), is_col_latin_i, R = R))
}