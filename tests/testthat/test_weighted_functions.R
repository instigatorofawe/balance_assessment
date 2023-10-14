testthat::test_that("Weighted mean works", {
    x_weighted = c(1,2,3)
    weights = c(1,2,2)

    x_unweighted = c(1,2,2,3,3)

    testthat::expect_equal(
        balanceAssessment::weighted_mean(x_weighted, rep(1, length(x_weighted))),
        mean(x_weighted)
    )

    testthat::expect_equal(
        balanceAssessment::weighted_mean(x_weighted, weights),
        mean(x_unweighted)
    )
})

testthat::test_that("Weighted var works", {
    x_weighted = c(1,2,3)
    weights = c(1,2,2)

    testthat::expect_equal(
        balanceAssessment::weighted_var(x_weighted, rep(1, length(x_weighted))),
        var(x_weighted)
    )

    testthat::expect_equal(
        balanceAssessment::weighted_var(x_weighted, weights),
        var(c(1,2,2,3,3))
    )
})

testthat::test_that("Weighted SD works", {
    x_weighted = c(1,2,3)
    weights = c(1,2,2)

    testthat::expect_equal(
        balanceAssessment::weighted_sd(x_weighted, rep(1, length(x_weighted))),
        sd(x_weighted)
    )

    testthat::expect_equal(
        balanceAssessment::weighted_sd(x_weighted, weights),
        sd(c(1,2,2,3,3))
    )
})
