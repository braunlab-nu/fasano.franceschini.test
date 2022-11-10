test_that("check that range tree and brute force methods return same statistics for high dimensional data", {
    # This test takes a while, so skip on CRAN
    skip_on_cran()

    set.seed(0)
    d <- 16
    n <- 25

    # Same distributions
    S1 <- data.frame(rnorm(n))
    S2 <- data.frame(rnorm(n))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rnorm(n))
        S2 <- cbind(S2, rnorm(n))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$estimate, res2$estimate, tolerance = 1e-14)
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-14)

    # Different distributions
    S1 <- data.frame(rnorm(n, mean = 1))
    S2 <- data.frame(rnorm(n))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rnorm(n, mean = 1))
        S2 <- cbind(S2, rnorm(n))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$estimate, res2$estimate, tolerance = 1e-14)
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-14)
})

test_that("check that range tree and brute force methods return same statistics for high dimensional data", {
    # This test takes a while, so skip on CRAN
    skip_on_cran()

    set.seed(0)
    d <- 18
    n1 <- 20
    n2 <- 21

    S1 <- data.frame(rnorm(n1))
    S2 <- data.frame(rnorm(n2))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rnorm(n1))
        S2 <- cbind(S2, rnorm(n2))
    }

    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$estimate, res2$estimate, tolerance = 1e-14)
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-14)
})
