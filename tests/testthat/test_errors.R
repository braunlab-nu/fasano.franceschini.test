test_that("test for bad inputs", {
    set.seed(1)

    S1 <- "a"
    S2 <- data.frame(rnorm(n=50))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 0))
    expect_error(fasano.franceschini.test(S2, S1, nPermute = 0))

    S1 <- data.frame(rnorm(n=61))
    S2 <- data.frame(rnorm(n=72), rnorm(n=72))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 0))
    expect_error(fasano.franceschini.test(S2, S1, nPermute = 0))

    S1 <- data.frame(rnorm(n=61), rnorm(n=61))
    S2 <- data.frame(rnorm(n=72), rnorm(n=72))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = -1))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 0.1))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 1.2))

    expect_error(fasano.franceschini.test(S1, S2, nPermute = 0, cores = -1))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 0, cores = 0))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 0, cores = 0.1))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 0, cores = 1.1))

    S1 <- rnorm(n=50)
    S2 <- data.frame(rnorm(n=50))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 0))
    expect_error(fasano.franceschini.test(S2, S1, nPermute = 0))

    S1 <- rnorm(n=60)
    S2 <- as.matrix(rnorm(60))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 0))
    expect_error(fasano.franceschini.test(S2, S1, nPermute = 0))
})
