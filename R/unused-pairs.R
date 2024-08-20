#' Pairs not used in a partial Room square
#'
#' @param R A partial Room square.
#' @param n Order of R.
#'
#' @return A list of pairs not used in R.
#' @export
#' 
#' @importFrom utils combn
unused_pairs <- function(R, n) {
  
  used_pairs <- R |> dplyr::select(first, second)
  
  x <- combn(0:(n - 1), 2)
  
  all_pairs <- tibble::tibble(
     first = x[1,],
    second = x[2,]
  )
  
  dplyr::anti_join(all_pairs, used_pairs, by = c("first", "second")) |>
    dplyr::mutate(ffs = purrr::map2(first, second, c)) |>
    dplyr::pull(ffs)

}