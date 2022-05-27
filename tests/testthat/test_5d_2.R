test_that("compare with brute force 5d implementation", {
    set.seed(5)

    ffcomp <- function(S1, S2) {
        res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
        res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
        expect_equal(res1$estimate, res2$estimate, tolerance = 1e-14)
        expect_equal(res1$statistic, res2$statistic, tolerance = 1e-14)
    }

    S1 <- data.frame(rnorm(n = 70, mean = 1.4, sd = 0.3),
                     rnorm(n = 70, mean = 1.5, sd = 1),
                     rnorm(n = 70, mean = 2.2, sd = 1.2),
                     rnorm(n = 70, mean = 2.2, sd = 1.8),
                     rnorm(n = 70, mean = -1.1, sd = 1.8))
    S2 <- data.frame(rnorm(n = 106, mean = 1.4, sd = 0.3),
                     rnorm(n = 106, mean = 1.5, sd = 1),
                     rnorm(n = 106, mean = 1.7, sd = 1.8),
                     rnorm(n = 106, mean = 2.2, sd = 0.01),
                     rnorm(n = 106, mean = 2.2, sd = 0.05))
    ffcomp(S1, S2)

    S1 <- data.frame(rbeta(n = 102, shape1 = 1.3, shape2 = 2.63),
                     rgamma(n = 102, shape = 4.4, rate = 0.4),
                     rbeta(n = 102, shape1 = 1.2, shape2 = 0.11),
                     rbeta(n = 102, shape1 = 2.1, shape2 = 1.39),
                     rbeta(n = 102, shape1 = 2.1, shape2 = 1.39))
    S2 <- data.frame(rbeta(n = 65, shape1 = 1.3, shape2 = 2.73),
                     rbeta(n = 65, shape1 = 1.2, shape2 = 0.11),
                     rbeta(n = 65, shape1 = 2.1, shape2 = 1.39),
                     rexp(n = 65, rate = 33.3),
                     rexp(n = 65, rate = 33.3))
    ffcomp(S1, S2)

    S1 <- data.frame(rbeta(n = 300, shape1 = 1.3, shape2 = 2.73),
                     rbeta(n = 300, shape1 = 1.2, shape2 = 0.11),
                     rbeta(n = 300, shape1 = 2.1, shape2 = 1.39),
                     rgamma(n = 300, shape = 0.9, rate = 3.42),
                     rgamma(n = 300, shape = 0.9, rate = 3.42))
    S2 <- data.frame(rbeta(n = 631, shape1 = 1.3, shape2 = 2.73),
                     rbeta(n = 631, shape1 = 1.2, shape2 = 0.11),
                     rbeta(n = 631, shape1 = 7.2, shape2 = 1.39),
                     rbeta(n = 631, shape1 = 7.2, shape2 = 1.39),
                     rbeta(n = 631, shape1 = 7.2, shape2 = 1.39))
    ffcomp(S1, S2)
})
