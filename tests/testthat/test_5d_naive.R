test_that("compare with naive R 5d implementation", {
    set.seed(5)

    ffcomp <- function(S1, S2) {
        res <- fasano.franceschini.test(S1, S2, nPermute = 0)
        d12 <- c(getDstat_naive(S1, S1, S2), getDstat_naive(S2, S1, S2))
        names(d12) <- names(res$estimate)

        n1 <- as.numeric(dim(S1)[1])
        n2 <- as.numeric(dim(S2)[1])
        z <- sqrt(n1*n2 / (n1+n2)) * ((d12[1] + d12[2])/2.0)
        names(z) <- names(res$statistic)

        expect_equal(d12, res$estimate, tolerance = 1e-14)
        expect_equal(z, res$statistic, tolerance = 1e-14)
    }

    S1 <- data.frame(rnorm(n = 70, mean = 1.4, sd = 0.3),
                     rnorm(n = 70, mean = 1.5, sd = 1),
                     rnorm(n = 70, mean = 1.6, sd = 1.8),
                     rnorm(n = 70, mean = 1.6, sd = 1.8),
                     rnorm(n = 70, mean = 1.6, sd = 1.8))
    S2 <- data.frame(rnorm(n = 206, mean = 1.4, sd = 0.3),
                     rnorm(n = 206, mean = 1.5, sd = 1),
                     rnorm(n = 206, mean = 1.6, sd = 1.8),
                     rnorm(n = 206, mean = 2.2, sd = 0.01),
                     rnorm(n = 206, mean = 2.2, sd = 0.01))
    ffcomp(S1, S2)

    S1 <- data.frame(rbeta(n = 102, shape1 = 1.3, shape2 = 2.73),
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

    S1 <- data.frame(rbeta(n = 400, shape1 = 1.3, shape2 = 2.73),
                     rbeta(n = 400, shape1 = 1.2, shape2 = 0.11),
                     rbeta(n = 400, shape1 = 2.1, shape2 = 1.39),
                     rgamma(n = 400, shape = 0.9, rate = 3.01),
                     rgamma(n = 400, shape = 0.9, rate = 3.01))
    S2 <- data.frame(rbeta(n = 761, shape1 = 1.3, shape2 = 2.73),
                     rbeta(n = 761, shape1 = 1.2, shape2 = 0.11),
                     rbeta(n = 761, shape1 = 0.2, shape2 = 1.39),
                     rbeta(n = 761, shape1 = 0.2, shape2 = 1.39),
                     rbeta(n = 761, shape1 = 0.2, shape2 = 1.39))
    ffcomp(S1, S2)
})
