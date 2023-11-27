testthat::test_that("Effective sample size works", {
    testthat::expect_equal(
        balanceAssessment::effective_sample_size(rep(1, 5)),
        5
    )
})
