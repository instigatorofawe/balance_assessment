#' Weighted correlations
#' @param treatment Treatment variable
#' @param data Confounding data
#' @param weights Frequency weights
#' @return unweighted and weighted correlations
#' @export
weighted_correlation_table = function(treatment, data, weights) {
    treatment = treatment[!is.na(weights)]
    data = data[!is.na(weights),]
    weights = weights[!is.na(weights)]

    unweighted_cors = array(NA, dim(data)[2])
    weighted_cors = array(NA, dim(data)[2])

    for (i in seq_len(dim(data)[2])) {
        unweighted_cors[i] = cor(treatment, data[,i])
        weighted_cors[i] = cov.wt(data.frame(treatment, data[,i]), wt=weights, cor=T)$cor[1,2]
    }
    result = data.frame(unweighted=abs(unweighted_cors), weighted=abs(weighted_cors))
    rownames(result) = colnames(data)
    return(result)
}
