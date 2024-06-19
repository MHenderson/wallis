#' Number of filled cells in a partial Room square
#'
#' @param R A partial Room square
#'
#' @return The number of filled cells in R.
#' @export
n_filled_cells <- function(R) {
  R |>
    dplyr::filter(!is.na(first)) |> 
    nrow()
}

#' Volume of a partial Room square
#'
#' @param R A partial Room square.
#'
#' @return The volume of R.
#' @export
volume <- function(R) {
  round(n_filled_cells(R)/choose(max(R$col) + 1, 2), 6)
}