test_that("compare with brute force 2d implementation", {
    set.seed(0)

    ffcomp <- function(S1, S2) {
        res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
        res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
        expect_equal(res1$estimate, res2$estimate, tolerance = 1e-14)
        expect_equal(res1$statistic, res2$statistic, tolerance = 1e-14)
    }

    S1 <- data.frame(rnorm(n = 50, mean = 0, sd = 1),
                     rnorm(n = 50, mean = 1, sd = 1))
    S2 <- data.frame(rnorm(n = 83, mean = 0.2, sd = 1),
                     rnorm(n = 83, mean = 0.1, sd = 2))
    ffcomp(S1, S2)

    S1 <- data.frame(rnorm(n = 134, mean = 11, sd = 2),
                     rnorm(n = 134, mean = 11, sd = 2))
    S2 <- data.frame(rnorm(n = 45, mean = 11, sd = 2),
                     rnorm(n = 45, mean = 11, sd = 2))
    ffcomp(S1, S2)

    S1 <- data.frame(rbeta(n = 200, shape1 = 1.4, shape2 = 1.73),
                     rbeta(n = 200, shape1 = 1.4, shape2 = 1.73))
    S2 <- data.frame(rbeta(n = 23, shape1 = 1.4, shape2 = 1.73),
                     rbeta(n = 23, shape1 = 1.4, shape2 = 1.73))
    ffcomp(S1, S2)

    S1 <- data.frame(rbeta(n = 324, shape1 = 10.2, shape2 = 0.3),
                     rbeta(n = 324, shape1 = 10.2, shape2 = 0.3))
    S2 <- data.frame(rbeta(n = 1559, shape1 = 10.2, shape2 = 0.3),
                     rbeta(n = 1559, shape1 = 10.2, shape2 = 0.3))
    ffcomp(S1, S2)

    S1 <- cbind(rpois(n = 100, lambda = 4),
                rpois(n = 100, lambda = 3))
    S2 <- cbind(rpois(n = 100, lambda = 4),
                rpois(n = 100, lambda = 3))
    ffcomp(S1, S2)

    S1 <- cbind(rep(1, 100), rep(2, 100))
    S2 <- cbind(rep(1, 100), rep(2, 100))
    ffcomp(S1, S2)

    S1 <- cbind(rep(1, 100), rep(2, 100))
    S2 <- cbind(rep(2, 100), rep(1, 100))
    ffcomp(S1, S2)
})
