#include "weighted_functions.h"

//' Weighted mean
//'
//' This function returns the weighted mean of a vector
//' @param x A numeric vector for which to compute the weighted mean
//' @param w A numeric vector of the same length as x
//' @return Returns the weighted mean
//' @export
// [[Rcpp::export]]
double weighted_mean(NumericVector x, NumericVector w) {
    return sum(x * w) / sum(w);
}

//' Weighted variance
//'
//' This function returns the weighted variance of a vector
//' @param x A numeric vector for which to compute the weighted variance
//' @param w A numeric vector of the same length as x, with frequency weights (i.e. sum of weights is equal to number of occurrences)
//' @return Returns the weighted variance
//' @export
// [[Rcpp::export]]
double weighted_var(NumericVector x, NumericVector w) {
    double xm = weighted_mean(x, w);
    return sum(w * pow(x - xm, 2)) / (sum(w) - 1);
}

//' Weighted sd
//'
//' This function returns the weighted sd of a vector
//' @param x A numeric vector for which to compute the weighted sd
//' @param w A numeric vector of the same length as x, with frequency weights (i.e. the sum of weights is equal to the number of occurrences)
//' @return Returns the weighted sd
//' @export
// [[Rcpp::export]]
double weighted_sd(NumericVector x, NumericVector w) {
    return sqrt(weighted_var(x, w));
}
