#' Remove both elements of a pair from a list
#'
#' @param X A list
#' @param p A pair
#'
#' @return The list X with both elements of p removed (if they exist).
remove_both <- function(X, p) {
  m1 <- match(p[1], X)
  if(!is.na(m1)) {
    X <- X[-m1]
  }
  m2 <- match(p[2], X)
  if(!is.na(m2)) {
    X <- X[-m2]
  }
  return(X)
}