// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// dcenter
NumericMatrix dcenter(NumericMatrix x);
RcppExport SEXP _balanceAssessment_dcenter(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(dcenter(x));
    return rcpp_result_gen;
END_RCPP
}
// weighted_dcenter
arma::mat weighted_dcenter(arma::mat x, arma::mat w);
RcppExport SEXP _balanceAssessment_weighted_dcenter(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_dcenter(x, w));
    return rcpp_result_gen;
END_RCPP
}
// weighted_dcov
double weighted_dcov(arma::mat x, arma::mat y, arma::mat w);
RcppExport SEXP _balanceAssessment_weighted_dcov(SEXP xSEXP, SEXP ySEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_dcov(x, y, w));
    return rcpp_result_gen;
END_RCPP
}
// weighted_mean
double weighted_mean(NumericVector x, NumericVector w);
RcppExport SEXP _balanceAssessment_weighted_mean(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_mean(x, w));
    return rcpp_result_gen;
END_RCPP
}
// weighted_var
double weighted_var(NumericVector x, NumericVector w);
RcppExport SEXP _balanceAssessment_weighted_var(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_var(x, w));
    return rcpp_result_gen;
END_RCPP
}
// weighted_sd
double weighted_sd(NumericVector x, NumericVector w);
RcppExport SEXP _balanceAssessment_weighted_sd(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_sd(x, w));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_balanceAssessment_dcenter", (DL_FUNC) &_balanceAssessment_dcenter, 1},
    {"_balanceAssessment_weighted_dcenter", (DL_FUNC) &_balanceAssessment_weighted_dcenter, 2},
    {"_balanceAssessment_weighted_dcov", (DL_FUNC) &_balanceAssessment_weighted_dcov, 3},
    {"_balanceAssessment_weighted_mean", (DL_FUNC) &_balanceAssessment_weighted_mean, 2},
    {"_balanceAssessment_weighted_var", (DL_FUNC) &_balanceAssessment_weighted_var, 2},
    {"_balanceAssessment_weighted_sd", (DL_FUNC) &_balanceAssessment_weighted_sd, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_balanceAssessment(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
