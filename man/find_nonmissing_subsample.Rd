% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_of_integration.R
\name{find_nonmissing_subsample}
\alias{find_nonmissing_subsample}
\title{Find Non-Missing Subsamples}
\usage{
find_nonmissing_subsample(X)
}
\arguments{
\item{X}{A (\eqn{T}x\eqn{N})-matrix of \eqn{N} time series with \eqn{T} observations. Data may also be in a time series format (e.g. \code{ts}, \code{zoo} or \code{xts}) or data frame. Assumes a prior check on missing values in-sample has been done.}
}
\value{
A list with the following components
\item{\code{range}}{(2x\eqn{N})-dimensional matrix containing the first and last non-missing observation in each column of X.}
\item{\code{all_equal}}{Logical value indicating whether all series have the same non-missing indices.}
}
\description{
Find Non-Missing Subsamples
}
\references{
Smeekes, S. and Wilms, I. (2023). bootUR: An R Package for Bootstrap Unit Root Tests. \emph{Journal of Statistical Software}, 106(12), 1-39.
}
