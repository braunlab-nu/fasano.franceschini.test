test_that("check that range tree and brute force methods return same statistics for high dimensional data", {
    skip_on_cran()
    set.seed(0)
    d <- 16
    S1 <- data.frame(rnorm(25))
    S2 <- data.frame(rnorm(25))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rnorm(25))
        S2 <- cbind(S2, rnorm(25))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$estimate, res2$estimate, tolerance = 1e-14)
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-14)
})

test_that("check that range tree and brute force methods return same statistics for high dimensional data", {
    skip_on_cran()
    set.seed(0)
    d <- 18
    S1 <- data.frame(rnorm(20, mean = 1))
    S2 <- data.frame(rnorm(22))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rnorm(20, mean = 1))
        S2 <- cbind(S2, rnorm(22))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$estimate, res2$estimate, tolerance = 1e-14)
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-14)
})
