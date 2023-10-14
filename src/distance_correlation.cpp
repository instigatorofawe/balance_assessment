#include "distance_correlation.h"

//' Compute doubly centered matrix
//'
//' This function returns the doubly centered version of a matrix
//' @param x A matrix which to doubly center
//' @return Doubly centered version of a matrix
//' @export
// [[Rcpp::export]]
NumericMatrix dcenter(NumericMatrix x) {
    NumericVector row_means = rowMeans(x);
    NumericVector col_means = colMeans(x);
    double grand_mean = mean(x);

    NumericMatrix result = clone(x);

    for (int i = 0; i < x.nrow(); i++) {
        for (int j = 0; j < x.ncol(); j++) {
            result(i,j) = x(i,j) - row_means[i] - row_means[j] + grand_mean;
        }
    }

    return result;

}
