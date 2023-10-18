#ifndef DISTANCE_CORRELATION_
#define DISTANCE_CORRELATION_

// #include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

NumericMatrix dcenter(NumericMatrix x);
arma::mat cross_weights(arma::vec w);
arma::mat weighted_dcenter(arma::mat x, arma::mat w);
double weighted_dcov(arma::mat x, arma::mat y, arma::mat w);
double weighted_dcor(arma::mat x, arma::mat y, arma::mat w);

#endif
