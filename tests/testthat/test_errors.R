test_that("test for bad inputs", {
    set.seed(1)

    # Test for issues with either sample
    S1 <- "a"
    S2 <- data.frame(rnorm(n = 50))
    expect_error(fasano.franceschini.test(S1, S1))
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

    ## Test for bad inputs to other arguments
    S1 <- data.frame(rnorm(n = 3), rnorm(n = 3))
    S2 <- data.frame(rep(1, 4), rep(2, 4))

    # Check nPermute
    expect_error(fasano.franceschini.test(S1, S2, nPermute = -1.1))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = -1))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = -0.1))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 0.1))
    expect_error(fasano.franceschini.test(S1, S2, nPermute = 1.1))
    expect_error(fasano.franceschini.test(S1, S2, nperm = 1))
    expect_error(fasano.franceschini.test(S1, S2, npermute = 1))
    expect_no_error(fasano.franceschini.test(S1, S2, nPermute = 1))
    expect_no_error(fasano.franceschini.test(S1, S2, nPermute = 2))
    expect_no_error(fasano.franceschini.test(S1, S2, nPermute = 3))

    # Check threads
    expect_error(fasano.franceschini.test(S1, S2, threads = -1.1))
    expect_error(fasano.franceschini.test(S1, S2, threads = -1))
    expect_error(fasano.franceschini.test(S1, S2, threads = -1.0))
    expect_error(fasano.franceschini.test(S1, S2, threads = -0.1))
    expect_error(fasano.franceschini.test(S1, S2, threads = 0))
    expect_error(fasano.franceschini.test(S1, S2, threads = 0.1))
    expect_error(fasano.franceschini.test(S1, S2, threads = 0.9))
    expect_error(fasano.franceschini.test(S1, S2, threads = 1.1))
    expect_error(fasano.franceschini.test(S1, S2, threads = 2.1))
    expect_error(fasano.franceschini.test(S1, S2, cores = 1))
    expect_error(fasano.franceschini.test(S1, S2, cores = 1, threads = 1))
    expect_error(fasano.franceschini.test(S1, S2, threads = 1, cores = 1))

    # Check method
    expect_error(fasano.franceschini.test(S1, S2, method = 0))
    expect_error(fasano.franceschini.test(S1, S2, method = 1))
    expect_error(fasano.franceschini.test(S1, S2, method = "a"))
    expect_error(fasano.franceschini.test(S1, S2, method = "c"))
    expect_error(fasano.franceschini.test(S1, S2, method = "q"))
    expect_error(fasano.franceschini.test(S1, S2, method = "s"))
    expect_no_error(fasano.franceschini.test(S1, S2, method = 'r', nPermute = 0))
    expect_no_error(fasano.franceschini.test(S1, S2, method = 'b', nPermute = 0))

    # Check verbose
    expect_error(fasano.franceschini.test(S1, S2, verbose = 0))
    expect_error(fasano.franceschini.test(S1, S2, verbose = 1))
    expect_error(fasano.franceschini.test(S1, S2, verbose = 'a'))
    expect_error(fasano.franceschini.test(S1, S2, verbose = "TRUE"))
    expect_error(fasano.franceschini.test(S1, S2, verbose = "FALSE"))
    expect_no_error(fasano.franceschini.test(S1, S2, verbose = TRUE, nPermute = 2))
    expect_no_error(fasano.franceschini.test(S1, S2, verbose = FALSE, nPermute = 2))

    # Check seed
    expect_error(fasano.franceschini.test(S1, S2, seed = 1.1))
    expect_error(fasano.franceschini.test(S1, S2, seed = -1.1))
    expect_error(fasano.franceschini.test(S1, S2, seed = 'a'))
    expect_error(fasano.franceschini.test(S1, S2, seed = FALSE))
    expect_error(fasano.franceschini.test(S1, S2, seed = TRUE))
    expect_no_error(fasano.franceschini.test(S1, S2, seed = -1, nPermute = 2))
    expect_no_error(fasano.franceschini.test(S1, S2, seed = 1, nPermute = 2))
})
