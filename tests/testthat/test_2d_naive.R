test_that("compare with naive R 2d implementation", {
    set.seed(2)

    ffcomp <- function(S1, S2) {
        res <- fasano.franceschini.test(S1, S2, nPermute = 0)
        d12 <- c(getDstat_naive(S1, S1, S2), getDstat_naive(S2, S1, S2))
        names(d12) <- names(res$estimate)

        n1 <- as.numeric(dim(S1)[1])
        n2 <- as.numeric(dim(S2)[1])
        d <- n1 * n2 * (d12[1] + d12[2])
        names(d) <- names(res$statistic)

        expect_equal(d, res$statistic, tolerance = 1e-15)
    }

    S1 <- data.frame(rnorm(n = 50, mean = 0, sd = 1),
                     rnorm(n = 50, mean = 1, sd = 1))
    S2 <- data.frame(rnorm(n = 83, mean = 0.2, sd = 1),
                     rnorm(n = 83, mean = 0.1, sd = 2))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rnorm(n = 132, mean = 10, sd = 1),
                     rnorm(n = 132, mean = 10, sd = 1))
    S2 <- data.frame(rnorm(n = 47, mean = 10, sd = 1),
                     rnorm(n = 47, mean = 10, sd = 1))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rbeta(n = 100, shape1 = 1.3, shape2 = 2.73),
                     rbeta(n = 100, shape1 = 1.3, shape2 = 2.73))
    S2 <- data.frame(rbeta(n = 23, shape1 = 1.3, shape2 = 2.73),
                     rbeta(n = 23, shape1 = 1.3, shape2 = 2.73))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- data.frame(rbeta(n = 224, shape1 = 10.1, shape2 = 0.1),
                     rbeta(n = 224, shape1 = 10.1, shape2 = 0.1))
    S2 <- data.frame(rbeta(n = 1557, shape1 = 10.1, shape2 = 0.1),
                     rbeta(n = 1557, shape1 = 10.1, shape2 = 0.1))
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
    S2 <- cbind(c(2, 3, 4), c(1, 2, 3))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)

    S1 <- cbind(c(1, 1), c(1, 1))
    S2 <- cbind(c(1, 1), c(1, 1))
    ffcomp(S1, S2)
    ffcomp(S1, S1)
    ffcomp(S2, S2)
})
