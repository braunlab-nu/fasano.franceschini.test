test_that("check that range tree and brute force methods return same statistics for high dimensional data", {
    # This test takes a while, so skip on CRAN
    skip_on_cran()

    set.seed(0)
    d <- 12
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
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S1, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S1, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S2, nPermute = 1, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 1, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    S1 <- data.frame(rep(1, n))
    S2 <- data.frame(rep(1, n))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rep(1, n))
        S2 <- cbind(S2, rep(1, n))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S2, nPermute = 2, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 2, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    # Different distributions
    S1 <- data.frame(rnorm(n, mean = 1))
    S2 <- data.frame(rnorm(n))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rnorm(n, mean = 1))
        S2 <- cbind(S2, rnorm(n))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S2, nPermute = 3, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 3, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    S1 <- data.frame(rep(1, n))
    S2 <- data.frame(rep(2, n))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rep(1, n))
        S2 <- cbind(S2, rep(2, n))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S2, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S2, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S2, nPermute = 4, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 4, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)
})

test_that("check that range tree and brute force methods return same statistics for high dimensional data", {
    # This test takes a while, so skip on CRAN
    skip_on_cran()

    set.seed(0)
    d <- 16
    n <- 23

    # Same distributions
    S1 <- data.frame(rnorm(n))
    S2 <- data.frame(rnorm(n))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rnorm(n))
        S2 <- cbind(S2, rnorm(n))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    S1 <- data.frame(rep(1, n))
    S2 <- data.frame(rep(1, n))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rep(1, n))
        S2 <- cbind(S2, rep(1, n))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    # Different distributions
    S1 <- data.frame(rnorm(n, mean = 1))
    S2 <- data.frame(rnorm(n))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rnorm(n, mean = 1))
        S2 <- cbind(S2, rnorm(n))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    S1 <- data.frame(rep(1, n))
    S2 <- data.frame(rep(2, n))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rep(1, n))
        S2 <- cbind(S2, rep(2, n))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)
})

test_that("check that range tree and brute force methods return same statistics for high dimensional data", {
    # This test takes a while, so skip on CRAN
    skip_on_cran()

    set.seed(0)
    d <- 20
    n1 <- 7
    n2 <- 16

    S1 <- data.frame(rnorm(n1))
    S2 <- data.frame(rnorm(n2))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rnorm(n1))
        S2 <- cbind(S2, rnorm(n2))
    }

    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)
})
