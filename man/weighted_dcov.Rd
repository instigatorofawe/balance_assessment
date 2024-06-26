% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{weighted_dcov}
\alias{weighted_dcov}
\title{Compute weighted distance covariance}
\usage{
weighted_dcov(x, y, w)
}
\arguments{
\item{x}{Distance matrix for x}

\item{y}{Distance matrix for y}

\item{w}{Weights}
}
\value{
Distance covariance
}
\description{
This function computes the distance covariance between x and y, where data points are weighted by w
}
