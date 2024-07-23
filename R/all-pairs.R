#' All unordered pairs
#'
#' @param n Size of underlying set.
#'
#' @return A list of all unordered pairs.
all_pairs <- function(n) {
  
  y <- combn(0:(n - 1), 2)
  
  tibble::tibble(
    first = y[1,],
    second = y[2,]
  ) |>
    dplyr::mutate(ffs = purrr::map2(first, second, c)) |>
    dplyr::pull(ffs)
  
}

#' All ordered pairs
#'
#' @param n Size of underlying set
#'
#' @return A list of all ordered pairs.
all_ordered_pairs <- function(n) {
  
  y <- tidyr::expand_grid(i = 1:n, j = 1:n)
  
  y |>
    dplyr::mutate(ffs = purrr::map2(i, j, c)) |>
    dplyr::pull(ffs)
  
}
