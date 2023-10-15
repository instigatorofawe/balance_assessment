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

//' Compute doubly centered matrix
//'
//' This function returns the doubly centered version of a matrix
//' @param x A matrix which to doubly center
//' @param w A matrix containing weights
//' @return Doubly centered version of a matrix
//' @export
// [[Rcpp::export]]
arma::mat weighted_dcenter(arma::mat x, arma::mat w) {
    arma::mat row_means = arma::sum(x % w, 1) % (1.0 / arma::sum(w, 1));
    arma::mat col_means = arma::sum(x % w, 0) % (1.0 / arma::sum(w, 0));
    double grand_mean = arma::sum(arma::sum(x % w)) / arma::sum(arma::sum(w));

    arma::mat result = arma::mat(size(x));

    for (int i = 0; i < x.n_rows; i++) {
        for (int j = 0; j < x.n_cols; j++) {
            result(i,j) = x(i,j) - row_means[i] - row_means[j] + grand_mean;
        }
    }

    return result;
}

//' Compute weighted distance covariance
//'
//' This function computes the distance covariance between x and y, where data points are weighted by w
double weighted_dcov(arma::mat x, arma::mat y, arma::vec w) {
    // TODO
    return 0;
}
