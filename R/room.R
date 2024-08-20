#' Is R a partial Room square?
#'
#' @param R A partial Room square.
#'
#' @return True if and only if R is a partial Room square, False otherwise.
#' @export
is_partial_room <- function(R) {
  expected_number_of_distinct_pairs <- choose(max(R$col) + 1, 2)
  nfc <- n_filled_cells(R)
  is_row_latin(R) && 
    is_col_latin(R) && 
    n_filled_cells(R) <= expected_number_of_distinct_pairs
}

#' Is R a Room square?
#'
#' @param R A Room square.
#'
#' @return True if and only if R is a Room square, False otherwise.
#' @export
is_room <- function(R) {
  nfc <- n_filled_cells(R)
  is_partial_room(R) && nrow(distinct_pairs(R)) == nfc
}

#' Is R a maximal partial Room square?
#'
#' @param R A partial Room square.
#' @param n Order of R.
#'
#' @return True if and only if R is a maximal partial Room square, False otherwise.
#' @export
is_maximal_proom <- function(R, n) {
  
  result <- is_partial_room(R)
  
  R <- R |>
    dplyr::mutate(
      see = purrr::map2(row, col, see2, R = R)
    ) |>
    dplyr::mutate(
      avail = purrr::map(see, setdiff, x = 0:(n - 1))
    )
  
  # iterate through the set of unusued pairs trying to place them
  # return true if and only if no pairs can be placed
  for(p in unused_pairs(R)) {
    
    E <- empty_cells(R) 
    # try to find a hole
    x <- NULL
    
    # iterate through empty cells in given order
    for(cell in E) {
      
      if(avail(R, p, cell)) {
        x <- cell
      }
      
    }
    
    # if we were successful then this is not a maximal proom
    if(!is.null(x))  {
      result <- FALSE
      break()
    }
    
  }
  
  return(result)
}