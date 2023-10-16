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

testthat::test_that("Distance covariance works", {
    set.seed(1234)
    n = 100
    d = 5

    x = sapply(seq_len(d), function(i) rnorm(n))
    x_dist = as.matrix(dist(x))
    y = sapply(seq_len(d), function(i) rnorm(n))
    y_dist = as.matrix(dist(y))

    atol = 1e-10

    x_centered = balanceAssessment::weighted_dcenter(x_dist, array(1, dim(x_dist)))
    y_centered = balanceAssessment::weighted_dcenter(y_dist, array(1, dim(y_dist)))

    dcov = balanceAssessment::weighted_dcov(x_dist, y_dist, array(1, dim(x_dist)))

    testthat::expect_true(abs(dcov- energy::dcov(x,y)) < atol)
})
