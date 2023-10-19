#' Table of unweighted and weighted distance correlation
#' @param treatment Treatment variable (binary)
#' @param data Data for which rebalancing should be evaluated
#' @param weights Propensity weights for each data point
#' @return unweighted and weighted distance correlation
#' @export
weighted_dcorr_table = function(treatment, data, weights=rep(1, length(treatment))) {
    x_dist = stats::dist(data) %>% as.matrix
    u_dist = stats::dist(treatment) %>% as.matrix

    cross_weights = balanceAssessment::cross_weights(weights)

    dcorr = balanceAssessment::weighted_dcor(x_dist, u_dist, array(1, dim(x_dist)))
    weighted_dcorr = balanceAssessment::weighted_dcor(x_dist, u_dist, cross_weights)
    return(data.frame(unweighted=dcorr, weighted=weighted_dcorr))
}


#' @export
weighted_dcorr_stat = function(treatment, data, weights=rep(1, length(treatment))) {

    x_dist = stats::dist(data) %>% as.matrix
    u_dist = stats::dist(treatment) %>% as.matrix

    cross_weights = balanceAssessment::cross_weights(weights)

    return(balanceAssessment::weighted_dcor(x_dist, u_dist, cross_weights))
}
