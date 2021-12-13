#' BIC score 
#' 
#' This function calculates the BIC score that has been shown to work better than ordinary BIC in 
#' high-dimensional scenarios. It uses the variance estimator given in \insertCite{yu2019estimating;textual}{DisaggregateTS}.
#' 
#' @param X aggregated indicator series matrix that has been GLS rotated 
#' @param Y low-frequency response vector that has been GLS rotated 
#' @param covariance aggregated AR covariance nmatrix 
#' @param beta estimate of beta from LARS algorithm for a certain lambda
#' @keywords BIC high-dimensions 
#' @references 
#' \insertAllCited{}
#' @importFrom Rdpack reprompt

hdBIC <- function(X,Y,covariance,beta) {
  
  n_l <- length(Y)
  support <- sum(beta != 0)
  u <- Y-X%*%beta
  log.lik <- -(n_l-support)/2 - n_l/2*log(2*pi/(n_l-support)*crossprod(u)) - log(det(covariance))/2
  BIC <- -2*(log.lik) + log(n_l)*support
  
  return(BIC)
  
}