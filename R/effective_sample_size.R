#' Effective sample size
#' @param weights Weights for each data point
#' @return Effective sample size
#' @export
effective_sample_size = function(weights) {
    sum(weights)^2 / sum(weights^2)
}
