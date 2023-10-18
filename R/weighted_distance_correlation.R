weighted_dcorr_table = function(treatment, data, weights) {
    x_dist = stats::dist(data) %>% as.matrix
    u_dist = stats::dist(treatment) %>% as.matrix

    cross_weights = balanceAssessment::cross_weights(weights)

    dcorr = balanceAssessment::weighted_dcor(x_dist, u_dist, array(1, dim(x_dist)))
    weighted_dcorr = balanceAssessment::weighted_dcor(x_dist, u_dist, cross_weights)
    return(data.frame(unweighted=dcorr, weighted=weighted_dcorr))
}
