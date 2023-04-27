test_that("test that serial and parallel versions return same statistics", {
    ## To avoid ASAN/UBSAN errors on CRAN, following advice given at
    ## https://github.com/RcppCore/RcppParallel/issues/169
    Sys.setenv(RCPP_PARALLEL_BACKEND = "tinythread")

    set.seed(0)

    S1 <- data.frame(rnorm(n = 100, mean = 1, sd = 1),
                     rnorm(n = 100, mean = 1, sd = 1))
    S2 <- data.frame(rnorm(n = 103, mean = 1, sd = 1),
                     rnorm(n = 103, mean = 1, sd = 1))
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 20, threads = 1, method = 'r')$statistic,
                 fasano.franceschini.test(S1, S2, nPermute = 20, threads = 4, method = 'r')$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 22, threads = 1, method = 'b')$statistic,
                 fasano.franceschini.test(S1, S2, nPermute = 22, threads = 4, method = 'b')$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S1, nPermute = 24, threads = 1, method = 'r')$statistic,
                 fasano.franceschini.test(S1, S1, nPermute = 24, threads = 4, method = 'r')$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S1, nPermute = 26, threads = 1, method = 'b')$statistic,
                 fasano.franceschini.test(S1, S1, nPermute = 26, threads = 4, method = 'r')$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S2, S2, nPermute = 26, threads = 1, method = 'r')$statistic,
                 fasano.franceschini.test(S2, S2, nPermute = 26, threads = 4, method = 'b')$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 34, threads = 1, method = 'r')$statistic,
                 fasano.franceschini.test(S1, S2, nPermute = 20, threads = 4, method = 'b')$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 34, threads = 1, method = 'b')$statistic,
                 fasano.franceschini.test(S1, S2, nPermute = 20, threads = 4, method = 'r')$statistic,
                 tolerance = 1e-15)


    S1 <- data.frame(rnorm(n = 100, mean = 1, sd = 2),
                     rnorm(n = 100, mean = 1, sd = 1))
    S2 <- data.frame(rnorm(n = 103, mean = 1, sd = 1),
                     rnorm(n = 103, mean = 1, sd = 2))
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 31, threads = 1)$statistic,
                 fasano.franceschini.test(S1, S2, nPermute = 37, threads = 4)$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 1, threads = 1)$statistic,
                 fasano.franceschini.test(S1, S2, nPermute = 1, threads = 4)$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S1, nPermute = 2, threads = 1)$statistic,
                 fasano.franceschini.test(S1, S1, nPermute = 2, threads = 4)$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S2, S2, nPermute = 0, threads = 1)$statistic,
                 fasano.franceschini.test(S2, S2, nPermute = 0, threads = 4)$statistic,
                 tolerance = 1e-15)

    test1 <- fasano.franceschini.test(S1, S2, nPermute = 10, threads = 1)
    test2 <- fasano.franceschini.test(S1, S2, nPermute = 20, threads = 2)
    test3 <- fasano.franceschini.test(S1, S2, nPermute = 30, threads = 3)
    test4 <- fasano.franceschini.test(S1, S2, nPermute = 40, threads = 4)
    expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
    expect_equal(test1$statistic, test3$statistic, tolerance = 1e-15)
    expect_equal(test1$statistic, test4$statistic, tolerance = 1e-15)
    expect_equal(test2$statistic, test3$statistic, tolerance = 1e-15)
    expect_equal(test2$statistic, test4$statistic, tolerance = 1e-15)
    expect_equal(test3$statistic, test4$statistic, tolerance = 1e-15)


    S1 <- data.frame(rep(1, 10), rep(1, 10))
    S2 <- data.frame(rep(2, 12), rep(2, 12))
    expect_equal(fasano.franceschini.test(S1, S1, nPermute = 10, threads = 1, method = 'r')$statistic,
                 fasano.franceschini.test(S1, S1, nPermute = 10, threads = 4, method = 'b')$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S2, S2, nPermute = 10, threads = 1, method = 'r')$statistic,
                 fasano.franceschini.test(S2, S2, nPermute = 10, threads = 4, method = 'b')$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 10, threads = 1, method = 'r')$statistic,
                 fasano.franceschini.test(S1, S2, nPermute = 10, threads = 4, method = 'b')$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 10, threads = 1, method = 'r')$statistic,
                 fasano.franceschini.test(S1, S2, nPermute = 10, threads = 4, method = 'b')$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 0, threads = 1)$statistic,
                 fasano.franceschini.test(S1, S2, nPermute = 0, threads = 4)$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 1, threads = 1)$statistic,
                 fasano.franceschini.test(S1, S2, nPermute = 1, threads = 4)$statistic,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 2, threads = 1)$statistic,
                 fasano.franceschini.test(S1, S2, nPermute = 2, threads = 4)$statistic,
                 tolerance = 1e-15)

    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 2, threads = 1, seed = 0)$p.value,
                 fasano.franceschini.test(S1, S2, nPermute = 2, threads = 4, seed = 0)$p.value,
                 tolerance = 1e-15)
    expect_equal(fasano.franceschini.test(S1, S2, nPermute = 21, threads = 2, seed = 0)$p.value,
                 fasano.franceschini.test(S1, S2, nPermute = 21, threads = 3, seed = 0)$p.value,
                 tolerance = 1e-15)
})
