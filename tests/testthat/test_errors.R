test_that("test for bad inputs", {
    set.seed(1)

    # Test for issues with either sample
    S1 <- "a"
    S2 <- data.frame(rnorm(n = 50))
    expect_error(fasano.franceschini.test(S1, S2))
    expect_error(fasano.franceschini.test(S2, S1))

    S1 <- data.frame(rnorm(n = 61))
    S2 <- data.frame(rnorm(n = 72), rnorm(n = 72))
    expect_error(fasano.franceschini.test(S1, S2))
    expect_error(fasano.franceschini.test(S2, S1))

    S1 <- data.frame(c(1, 2), c(2, 3))
    S2 <- data.frame(c(1, 2), c(2, 3), c(3, 4))
    expect_error(fasano.franceschini.test(S1, S2))
    expect_error(fasano.franceschini.test(S2, S1))

    S1 <- rnorm(n = 50)
    S2 <- data.frame(rnorm(n = 50))
    expect_error(fasano.franceschini.test(S1, S2))
    expect_error(fasano.franceschini.test(S2, S1))

    S1 <- rnorm(n = 60)
    S2 <- as.matrix(rnorm(n = 60))
    expect_error(fasano.franceschini.test(S1, S2))
    expect_error(fasano.franceschini.test(S2, S1))


    # Test for bad inputs to other arguments
    S1 <- data.frame(rnorm(n = 61), rnorm(n = 61))
    S2 <- data.frame(rnorm(n = 72), rnorm(n = 72))

    expect_error(fasano.franceschini.test(S1, S2, nPermute = -1))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 0.1))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 1.2))

    expect_error(fasano.franceschini.test(S1, S2, threads = -1))
    expect_error(fasano.franceschini.test(S1, S2, threads = 0))
    expect_error(fasano.franceschini.test(S1, S2, threads = 0.1))
    expect_error(fasano.franceschini.test(S1, S2, threads = 1.1))

    expect_error(fasano.franceschini.test(S1, S2, cores = 1))
    expect_error(fasano.franceschini.test(S1, S2, cores = 1, threads = 1))
    expect_error(fasano.franceschini.test(S1, S2, threads = 1, cores = 1))

    expect_error(fasano.franceschini.test(S1, S2, method = 1))
    expect_error(fasano.franceschini.test(S1, S2, method = "a"))
    expect_error(fasano.franceschini.test(S1, S2, method = "c"))
    expect_error(fasano.franceschini.test(S1, S2, method = "q"))
    expect_error(fasano.franceschini.test(S1, S2, method = "s"))

    expect_error(fasano.franceschini.test(S1, S2, p.conf.level = 0))
    expect_error(fasano.franceschini.test(S1, S2, p.conf.level = -0.05))
    expect_error(fasano.franceschini.test(S1, S2, p.conf.level = 1.0))
    expect_error(fasano.franceschini.test(S1, S2, p.conf.level = 1.05))
    expect_error(fasano.franceschini.test(S1, S2, p.conf.level = 'a'))

    expect_error(fasano.franceschini.test(S1, S2, verbose = 0))
    expect_error(fasano.franceschini.test(S1, S2, verbose = 'a'))

    expect_error(fasano.franceschini.test(S1, S2, seed = 1.1))
    expect_error(fasano.franceschini.test(S1, S2, seed = 'a'))
    expect_error(fasano.franceschini.test(S1, S2, seed = FALSE))
    expect_error(fasano.franceschini.test(S1, S2, seed = TRUE))
})
