testthat::test_that("Double centering works", {
    set.seed(1234)
    n = 100
    d = 5
    x = sapply(seq_len(d), function(i) rnorm(n))
    x_dist = as.matrix(dist(x))

    atol = 1e-10

    x_dist_centered = balanceAssessment::dcenter(x_dist)

    weights = array(1, dim(x_dist))

    x_dist_centered_weighted = balanceAssessment::weighted_dcenter(x_dist, weights)

    testthat::expect_true(all(abs(colSums(x_dist_centered)) < atol))
    testthat::expect_true(all(abs(rowSums(x_dist_centered)) < atol))

    testthat::expect_true(all(abs(x_dist_centered - x_dist_centered_weighted) < atol))
})
