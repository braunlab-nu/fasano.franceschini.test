test_that("test that serial and parallel versions return same statistics", {
    ## To avoid ASAN/UBSAN errors on CRAN, following advice given in
    ## https://github.com/RcppCore/RcppParallel/issues/169
    Sys.setenv(RCPP_PARALLEL_BACKEND = "tinythread")

    set.seed(0)

    S1 <- data.frame(rnorm(n = 100, mean = 1, sd = 1),
                     rnorm(n = 100, mean = 1, sd = 1))
    S2 <- data.frame(rnorm(n = 103, mean = 1, sd = 1),
                     rnorm(n = 103, mean = 1, sd = 1))

    test1 <- fasano.franceschini.test(S1, S2, nPermute = 34, threads = 1)
    test4 <- fasano.franceschini.test(S1, S2, nPermute = 20, threads = 4)
    expect_equal(test1$statistic, test4$statistic, tolerance = 1e-14)
    expect_equal(test1$estimate, test4$estimate, tolerance = 1e-14)

    S1 <- data.frame(rnorm(n = 100, mean = 1, sd = 2),
                     rnorm(n = 100, mean = 1, sd = 1))
    S2 <- data.frame(rnorm(n = 103, mean = 1, sd = 1),
                     rnorm(n = 103, mean = 1, sd = 2))

    test1 <- fasano.franceschini.test(S1, S2, nPermute = 31, threads = 1)
    test4 <- fasano.franceschini.test(S1, S2, nPermute = 37, threads = 4)
    expect_equal(test1$statistic, test4$statistic, tolerance = 1e-14)
    expect_equal(test1$estimate, test4$estimate, tolerance = 1e-14)
})
