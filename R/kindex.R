#' Index of support for LARS algorithm when in high-dimensions 
#' 
#' This function prevents the support of beta becoming greater than n_l/2.
#' This heuristic approach prevents erratic values of BIC when in high-dimensions. 
#' 
#' @param matrix matrix of beta estimates from LARS algorithm
#' @param n_l number of low-frequency observations

k.index <- function(matrix, n_l) {
  
  count <- apply(matrix, 1, function(x) {sum(x != 0)})
  if(max(count) > n_l/2) {
    kindex <- min(which(count > n_l/2))
  }
  else {
    kindex <- nrow(matrix)
  }
  
  return(kindex)
}