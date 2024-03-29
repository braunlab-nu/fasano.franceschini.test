test_that("check that range tree and brute force methods return same statistics for 4d data", {
    set.seed(4)

    ffcomp <- function(S1, S2) {
        res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
        res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
        res3 <- fasano.franceschini.test(S1, S2, nPermute = 0)
        res4 <- fasano.franceschini.test(S1, S2, nPermute = 1)
        expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)
        expect_equal(res1$statistic, res3$statistic, tolerance = 1e-15)
        expect_equal(res1$statistic, res4$statistic, tolerance = 1e-15)
        expect_equal(res2$statistic, res3$statistic, tolerance = 1e-15)
        expect_equal(res2$statistic, res4$statistic, tolerance = 1e-15)
        expect_equal(res3$statistic, res4$statistic, tolerance = 1e-15)
    }

    S1 <- data.frame(rnorm(n = 70, mean = 1.4, sd = 0.3),
                     rnorm(n = 70, mean = 1.5, sd = 1),
                     rnorm(n = 70, mean = 1.6, sd = 1.8),
                     rnorm(n = 70, mean = 1.6, sd = 1.8))
    S2 <- data.frame(rnorm(n = 206, mean = 1.4, sd = 0.3),
                     rnorm(n = 206, mean = 1.5, sd = 1),
                     rnorm(n = 206, mean = 1.6, sd = 1.8),
                     rnorm(n = 206, mean = 2.2, sd = 0.01))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rbeta(n = 104, shape1 = 1.3, shape2 = 2.53),
                     rgamma(n = 104, shape = 4.4, rate = 0.4),
                     rbeta(n = 104, shape1 = 1.2, shape2 = 0.31),
                     rbeta(n = 104, shape1 = 2.1, shape2 = 1.39))
    S2 <- data.frame(rbeta(n = 67, shape1 = 1.3, shape2 = 2.63),
                     rbeta(n = 67, shape1 = 1.2, shape2 = 0.12),
                     rbeta(n = 67, shape1 = 2.1, shape2 = 1.39),
                     rexp(n = 67, rate = 32.3))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rbeta(n = 401, shape1 = 2.3, shape2 = 2.73),
                     rbeta(n = 401, shape1 = 2.2, shape2 = 0.12),
                     rbeta(n = 401, shape1 = 1.1, shape2 = 1.39),
                     rgamma(n = 401, shape = 0.9, rate = 3.01))
    S2 <- data.frame(rbeta(n = 736, shape1 = 2.3, shape2 = 2.23),
                     rbeta(n = 736, shape1 = 2.2, shape2 = 0.14),
                     rbeta(n = 736, shape1 = 0.2, shape2 = 1.39),
                     rbeta(n = 736, shape1 = 1.2, shape2 = 1.36))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rnorm(n = 50), rnorm(n = 50), rnorm(n = 50), rnorm(n = 50))
    S2 <- data.frame(rnorm(n = 33), rnorm(n = 33), rnorm(n = 33), rnorm(n = 33))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- cbind(c(1, 1), c(1, 1), c(1, 1), c(1, 1))
    S2 <- cbind(c(1, 1), c(1, 1), c(1, 1), c(1, 1))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)
})
