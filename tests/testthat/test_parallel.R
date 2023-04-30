test_that("test that serial and parallel versions return same results", {
    ## To avoid ASAN/UBSAN errors on CRAN, following advice given at
    ## https://github.com/RcppCore/RcppParallel/issues/169
    Sys.setenv(RCPP_PARALLEL_BACKEND = "tinythread")

    set.seed(0)

    ff_comp <- function(S1, S2, p, t1, t2, m1, m2, seed = NULL) {
        test1 <- fasano.franceschini.test(S1, S2, nPermute = p, threads = t1,
                                          method = m1, seed = seed)
        test2 <- fasano.franceschini.test(S1, S2, nPermute = p, threads = t2,
                                          method = m2, seed = seed)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        if (!is.null(seed)) {
            expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)
        }


        test1 <- fasano.franceschini.test(S2, S1, nPermute = p, threads = t1,
                                          method = m1, seed = seed)
        test2 <- fasano.franceschini.test(S2, S1, nPermute = p, threads = t2,
                                          method = m2, seed = seed)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        if (!is.null(seed)) {
            expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)
        }

        test1 <- fasano.franceschini.test(S1, S1, nPermute = p, threads = t1,
                                          method = m1, seed = seed)
        test2 <- fasano.franceschini.test(S1, S1, nPermute = p, threads = t2,
                                          method = m2, seed = seed)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        if (!is.null(seed)) {
            expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)
        }

        test1 <- fasano.franceschini.test(S2, S2, nPermute = p, threads = t1,
                                          method = m1, seed = seed)
        test2 <- fasano.franceschini.test(S2, S2, nPermute = p, threads = t2,
                                          method = m2, seed = seed)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        if (!is.null(seed)) {
            expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)
        }
    }


    S1 <- data.frame(rnorm(n = 100, mean = 1, sd = 1),
                     rnorm(n = 100, mean = 1, sd = 1))
    S2 <- data.frame(rnorm(n = 103, mean = 1, sd = 1),
                     rnorm(n = 103, mean = 1, sd = 1))
    ff_comp(S1, S2, 20, 1, 4, 'r', 'r')
    ff_comp(S1, S2, 22, 1, 4, 'b', 'b', seed = 1)
    ff_comp(S1, S2, 8, 1, 4, 'r', 'b')
    ff_comp(S1, S2, 8, 1, 4, 'b', 'r', seed = 0)
    ff_comp(S1, S2, 8, 1, 3, 'r', 'b')
    ff_comp(S1, S2, 8, 1, 2, 'r', 'b', seed = 2)
    ff_comp(S1, S2, 8, 2, 3, 'r', 'b', seed = 4)
    ff_comp(S1, S2, 8, 4, 2, 'b', 'r')

    S1 <- data.frame(rnorm(n = 100, mean = 1, sd = 2),
                     rnorm(n = 100, mean = 1, sd = 1))
    S2 <- data.frame(rnorm(n = 103, mean = 1, sd = 1),
                     rnorm(n = 103, mean = 1, sd = 2))
    ff_comp(S1, S2, 11, 1, 1, 'r', 'b')
    ff_comp(S1, S2, 11, 1, 2, 'b', 'b', seed = 0)
    ff_comp(S1, S2, 11, 1, 3, 'b', 'r', seed = 0)
    ff_comp(S1, S2, 11, 1, 4, 'r', 'b')

    S1 <- data.frame(rep(1, 10), rep(1, 10))
    S2 <- data.frame(rep(2, 12), rep(2, 12))
    ff_comp(S1, S2, 60, 1, 1, 'r', 'r', seed = 0)
    ff_comp(S1, S2, 61, 2, 1, 'r', 'b')
    ff_comp(S1, S2, 62, 3, 1, 'r', 'r', seed = 1)
    ff_comp(S1, S2, 63, 4, 1, 'b', 'r')
    ff_comp(S1, S2, 64, 2, 3, 'r', 'r', seed = 2)
    ff_comp(S1, S2, 65, 4, 2, 'b', 'b')
    ff_comp(S1, S2, 66, 3, 4, 'r', 'b', seed = 3)
    ff_comp(S1, S2, 67, 4, 4, 'b', 'r', seed = 4)
})
