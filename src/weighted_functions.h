#ifndef WEIGHTED_FUNCTIONS_
#define WEIGHTED_FUNCTIONS_

#include <Rcpp.h>
using namespace Rcpp;

double weighted_mean(NumericVector x, NumericVector w);
double weighted_var(NumericVector x, NumericVector w);
double weighted_sd(NumericVector x, NumericVector w);

#endif
