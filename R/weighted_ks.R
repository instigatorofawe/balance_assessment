#' Table of unweighted and weighted KS statistics
#' @param treatment Treatment variable (binary)
#' @param data Data for which rebalancing should be evaluated
#' @param weights Propensity weights for each data point
#' @return unweighted and weighted KS statistics
#' @export
weighted_ks_table = function(treatment, data, weights) {
    treatment = treatment[!is.na(weights)]
    data = data[!is.na(weights),]
    weights = weights[!is.na(weights)]

    unweighted_stat = apply(data, 2, function(x) {
        # ks.test(x[treatment], x[!treatment])$stat
        weighted_two_sample_ks_stat(x[treatment], x[!treatment], rep(1, sum(treatment)), rep(1, sum(!treatment)))
    })

    weighted_stat = apply(data, 2, function(x) {
        weighted_two_sample_ks_stat(x[treatment], x[!treatment], weights[treatment], weights[!treatment])
    })

    result = data.frame(unweighted=abs(unweighted_stat), weighted=abs(weighted_stat))
    rownames(result) = colnames(data)
    return(result)
}

#' Weighted KS statistic
#' @param treatment Binary treatment variable
#' @param data Confounders for which to compute KS statistic
#' @param weights Weights for each data point
#' @return KS statistic between treated and untreated group for each confounder
#' @export
weighted_ks_stat = function(treatment, data, weights=rep(1, length(treatment))) {
    return(apply(data, 2, function(x) {
        weighted_two_sample_ks_stat(x[treatment], x[!treatment], weights[treatment], weights[!treatment])
    }))
}

#' Compute weighted 2-sample KS statistics
#' @param x First sample
#' @param y Second sample
#' @param weights_x Weights for first sample
#' @param weights_y Weights for second sample
#' @return Test statistic for weighted 2 samples
#' @export
weighted_two_sample_ks_stat = function(x, y, weights_x=rep(1, length(x)), weights_y=rep(1, length(y))) {
    cdf_x = weighted_ecdf(x, weights_x)
    cdf_y = weighted_ecdf(y, weights_y)
    unique_values = sort(unique(c(x, y)))
    differences = abs(cdf_y(unique_values) - cdf_x(unique_values))
    return(max(differences))
}

#' Compute weighted empirical CDF
#' @param x Empirical data points for which to compute weighted CDF
#' @param weights Weights of data points
#' @return Weighted empirical CDF function
#' @export
weighted_ecdf = function(x, weights=NULL) {
    if (is.null(weights)) {
        weights = rep(1, length(x))
    }

    x_order = order(x)
    stats::stepfun(x[x_order], c(0, cumsum(weights[x_order]))/sum(weights))
}
