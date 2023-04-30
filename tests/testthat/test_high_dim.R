test1 <- function(d, n) {
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

    res1 <- fasano.franceschini.test(S2, S1, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S2, S1, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S1, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S1, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S2, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S2, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S2, nPermute = 1, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 1, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)
}
test2 <- function(d, n) {
    S1 <- data.frame(rep(1, n))
    S2 <- data.frame(rep(1, n))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rep(1, n))
        S2 <- cbind(S2, rep(1, n))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S2, S1, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S2, S1, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S1, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S1, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S2, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S2, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S2, nPermute = 2, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 2, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)
}
test3 <- function(d, n) {
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

    res1 <- fasano.franceschini.test(S2, S1, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S2, S1, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S1, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S1, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S2, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S2, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S2, nPermute = 3, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 3, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)
}
test4 <- function(d, n) {
    S1 <- data.frame(rep(1, n))
    S2 <- data.frame(rep(2, n))
    for (i in 1:(d - 1)) {
        S1 <- cbind(S1, rep(1, n))
        S2 <- cbind(S2, rep(2, n))
    }
    res1 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S2, S1, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S2, S1, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S1, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S1, S1, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S2, S2, nPermute = 0, method = 'r')
    res2 <- fasano.franceschini.test(S2, S2, nPermute = 0, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)

    res1 <- fasano.franceschini.test(S1, S2, nPermute = 1, method = 'r')
    res2 <- fasano.franceschini.test(S1, S2, nPermute = 1, method = 'b')
    expect_equal(res1$statistic, res2$statistic, tolerance = 1e-15)
}

test_that("check that range tree and brute force methods return same statistic for d=8", {
    # This test takes a while, so skip on CRAN
    skip_on_cran()
    set.seed(0)
    d <- 8
    n <- 25

    test1(d, n)
    test2(d, n)
    test3(d, n)
    test4(d, n)
})

test_that("check that range tree and brute force methods return same statistic for d=10", {
    # This test takes a while, so skip on CRAN
    skip_on_cran()
    set.seed(1)
    d <- 10
    n <- 20

    test1(d, n)
    test2(d, n)
    test3(d, n)
    test4(d, n)
})

test_that("check that range tree and brute force methods return same statistic for d=12", {
    # This test takes a while, so skip on CRAN
    skip_on_cran()
    set.seed(2)
    d <- 12
    n <- 18

    test1(d, n)
    test2(d, n)
    test3(d, n)
})

test_that("check that range tree and brute force methods return same statistic for d=13", {
    # This test takes a while, so skip on CRAN
    skip_on_cran()
    set.seed(3)
    d <- 13
    n <- 15

    test1(d, n)
    test2(d, n)
    test4(d, n)
})

test_that("check that range tree and brute force methods return same statistic for d=15", {
    # This test takes a while, so skip on CRAN
    skip_on_cran()
    set.seed(4)
    d <- 15
    n <- 10

    test1(d, n)
    test3(d, n)
})

test_that("check that range tree and brute force methods return same statistic for d=20", {
    # This test takes a while, so skip on CRAN
    skip_on_cran()
    set.seed(5)
    d <- 20
    n <- 6

    test2(d, n)
    test4(d, n)
})
