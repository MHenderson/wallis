#' Create a partial Room square with no filled cells
#'
#' @param n Size of partial Room square to create.
#'
#' @return A partial Room square of size n with no filled cells.
#' @export
empty_room <- function(n = 5) {
  tidyr::expand_grid(row = 1:(n - 1), col = 1:(n - 1)) |>
    dplyr::mutate(first = as.numeric(NA), second = as.numeric(NA))
}