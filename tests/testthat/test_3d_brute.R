test_that("check that range tree and brute force methods return same statistics for 3d data", {
    set.seed(3)

    ffcomp <- function(S1, S2) {
        delta <-
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

    S1 <- cbind(rnorm(n = 35, mean = 1.4, sd = 0.2),
                rnorm(n = 35, mean = 1.5, sd = 2),
                rnorm(n = 35, mean = 1.6, sd = 1.8))
    S2 <- cbind(rnorm(n = 207, mean = 1.4, sd = 0.2),
                rnorm(n = 207, mean = 1.5, sd = 2),
                rnorm(n = 207, mean = 1.6, sd = 1.8))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rbeta(n = 101, shape1 = 1.2, shape2 = 2.73),
                     rbeta(n = 101, shape1 = 1.2, shape2 = 0.11),
                     rbeta(n = 101, shape1 = 2.1, shape2 = 1.39))
    S2 <- cbind(rbeta(n = 66, shape1 = 1.3, shape2 = 2.73),
                rbeta(n = 66, shape1 = 1.2, shape2 = 0.12),
                rbeta(n = 66, shape1 = 2.1, shape2 = 1.39))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rbeta(n = 403, shape1 = 1.1, shape2 = 2.71),
                     rbeta(n = 403, shape1 = 1.3, shape2 = 0.14),
                     rbeta(n = 403, shape1 = 2.4, shape2 = 1.35))
    S2 <- data.frame(rbeta(n = 780, shape1 = 1.5, shape2 = 2.76),
                     rbeta(n = 780, shape1 = 1.7, shape2 = 0.18),
                     rbeta(n = 780, shape1 = 0.6, shape2 = 1.38))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rbeta(n = 402, shape1 = 1.3, shape2 = 2.43),
                     rbeta(n = 402, shape1 = 1.2, shape2 = 0.12),
                     rbeta(n = 402, shape1 = 2.1, shape2 = 1.39))
    S2 <- data.frame(rbeta(n = 759, shape1 = 1.3, shape2 = 2.33),
                     rexp(n = 759, rate = 1.1),
                     rbeta(n = 759, shape1 = 0.3, shape2 = 1.39))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rnorm(n = 50), rnorm(n = 50), rnorm(n = 50))
    S2 <- data.frame(rnorm(n = 33), rnorm(n = 33), rnorm(n = 33))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- cbind(c(1, 1), c(1, 1), c(1, 1))
    S2 <- cbind(c(1, 1), c(1, 1), c(1, 1))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)
})
