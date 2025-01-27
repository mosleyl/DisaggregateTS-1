% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SparseTD.R
\name{SparseTD}
\alias{SparseTD}
\title{High-dimensional temporal disaggregation}
\usage{
SparseTD(
  Y,
  X = matrix(data = rep(1, times = nrow(Y)), nrow = nrow(Y)),
  penalty = "lasso",
  aggMat = "sum"
)
}
\arguments{
\item{Y}{the low-frequency response vector}

\item{X}{the high-frequency indicator matrix}

\item{penalty}{nominates the choice of regularisation (LASSO or Adaptive LASSO)}

\item{aggMat}{choice of the aggregation matrix}
}
\description{
This function provides the Sparse Temporal Disaggregation (spTD) methods proposed by \insertCite{mosley2021sparse;textual}{DisaggregateTS}
to perform temporal disaggregation of time series data in both standard and high-dimensional settings. Variable selection is also
performed by a LASSO penalty \insertCite{tibshirani1996regression;textual}{DisaggregateTS} or an Adaptive LASSO penalty
\insertCite{zou2006adaptive;textual}{DisaggregateTS}.
}
\examples{
SparseTD(Y = Y_Gen, X = X_Gen, penalty = 'lasso', aggMat = 'sum')
}
\references{
\insertAllCited{}
}
\keyword{Disaggregation}
\keyword{Lasso}
\keyword{Series}
\keyword{Sparse}
\keyword{Temporal}
\keyword{Time}
