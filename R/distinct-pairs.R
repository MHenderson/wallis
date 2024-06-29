#' Pairs used in R
#'
#' @param R A Room square.
#'
#' @return A list of the distinct pairs that appear in R.
distinct_pairs <- function(R) {
  R |>
    tidyr::pivot_wider() |>
    dplyr::filter(!is.na(first)) |>
    dplyr::filter(!is.na(second)) |>
    dplyr::distinct(first, second)
}