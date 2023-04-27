test_that("check that range tree and brute force methods return same statistics for 2d data", {
    set.seed(0)

    ffcomp <- function(S1, S2) {
        res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
        res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
        res3 <- fasano.franceschini.test(S1, S2, nPermute = 0)
        res4 <- fasano.franceschini.test(S1, S2, nPermute = 1)
        res5 <- fasano.franceschini.test(S1, S2, nPermute = 2)
        expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)
        expect_equal(res1$statistic, res3$statistic, tolerance = 1e-15)
        expect_equal(res1$statistic, res4$statistic, tolerance = 1e-15)
        expect_equal(res1$statistic, res5$statistic, tolerance = 1e-15)
        expect_equal(res2$statistic, res3$statistic, tolerance = 1e-15)
        expect_equal(res2$statistic, res4$statistic, tolerance = 1e-15)
        expect_equal(res2$statistic, res5$statistic, tolerance = 1e-15)
        expect_equal(res3$statistic, res4$statistic, tolerance = 1e-15)
        expect_equal(res3$statistic, res5$statistic, tolerance = 1e-15)
        expect_equal(res4$statistic, res5$statistic, tolerance = 1e-15)
    }

    S1 <- data.frame(rnorm(n = 50, mean = 0, sd = 1),
                     rnorm(n = 50, mean = 1, sd = 1))
    S2 <- data.frame(rnorm(n = 83, mean = 0.2, sd = 1),
                     rnorm(n = 83, mean = 0.1, sd = 2))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rnorm(n = 134, mean = 11, sd = 2),
                     rnorm(n = 134, mean = 11, sd = 2))
    S2 <- data.frame(rnorm(n = 45, mean = 11, sd = 2),
                     rnorm(n = 45, mean = 11, sd = 2))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rbeta(n = 200, shape1 = 1.4, shape2 = 1.73),
                     rbeta(n = 200, shape1 = 1.4, shape2 = 1.73))
    S2 <- data.frame(rbeta(n = 23, shape1 = 1.4, shape2 = 1.73),
                     rbeta(n = 23, shape1 = 1.4, shape2 = 1.73))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rbeta(n = 324, shape1 = 10.2, shape2 = 0.3),
                     rbeta(n = 324, shape1 = 10.2, shape2 = 0.3))
    S2 <- data.frame(rbeta(n = 1559, shape1 = 10.2, shape2 = 0.3),
                     rbeta(n = 1559, shape1 = 10.2, shape2 = 0.3))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- cbind(rpois(n = 100, lambda = 4),
                rpois(n = 100, lambda = 3))
    S2 <- cbind(rpois(n = 100, lambda = 4),
                rpois(n = 100, lambda = 3))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- cbind(rep(1, 100), rep(2, 100))
    S2 <- cbind(rep(1, 100), rep(2, 100))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- cbind(rep(1, 100), rep(2, 100))
    S2 <- cbind(rep(2, 100), rep(1, 100))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rnorm(n = 50), rnorm(n = 50))
    S2 <- data.frame(rnorm(n = 33), rnorm(n = 33))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- cbind(c(1, 2, 3), c(2, 3, 4))
    S2 <- cbind(c(1, 2, 3), c(1, 2, 3))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- cbind(c(1, 1), c(1, 1))
    S2 <- cbind(c(1, 1), c(1, 1))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)
})
