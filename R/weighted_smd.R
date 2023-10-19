#' Weighted smd
#' @param treatment Binary treatment variable
#' @param data Confounders/data matrix over whose columns to compute standardized mean difference
#' @param weights Frequency weights for occurrences of data
#' @return unweighted and weighted smd
#' @export
weighted_smd_table = function(treatment, data, weights) {
    treatment = treatment[!is.na(weights)]
    data = data[!is.na(weights),]
    weights = weights[!is.na(weights)]

    # Unweighted mean difference
    unweighted_difference = apply(data, 2, function(x) {
        (mean(x[treatment]) - mean(x[!treatment]))/stats::sd(x)
    })

    weighted_difference = apply(data, 2, function(x) {
        (balanceAssessment::weighted_mean(x[treatment], weights[treatment]) - balanceAssessment::weighted_mean(x[!treatment], weights[!treatment])) /
            balanceAssessment::weighted_sd(x, weights)
    })


    result = data.frame(unweighted=abs(unweighted_difference), weighted=abs(weighted_difference))
    rownames(result) = colnames(data)
    return(result)
}

#' Weighted SMD stat
#' @param treament Binary treatment variable
#' @param data Confounders for which to compute standardized mean difference
#' @param weights Frequency weights for each data point
#' @return SMD statistic
#' @export
weighted_smd_stat = function(treatment, data, weights=rep(1, length(treatment))) {

    return(apply(data, 2, function(x) {
        (balanceAssessment::weighted_mean(x[treatment], weights[treatment]) - balanceAssessment::weighted_mean(x[!treatment], weights[!treatment])) /
            balanceAssessment::weighted_sd(x, weights)
    }))
}
