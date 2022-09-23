test_that("test that seed works for serial version", {
    set.seed(0)

    comp <- function(test1, test2) {
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-14)
        expect_equal(test1$estimate, test2$estimate, tolerance = 1e-14)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-14)
    }

    S1 <- data.frame(rnorm(n = 50, mean = 0.2, sd = 2),
                     rnorm(n = 50, mean = 1.3, sd = 1.1))
    S2 <- data.frame(rnorm(n = 83, mean = 0.2, sd = 2),
                     rnorm(n = 83, mean = 1.3, sd = 1.1))
    test1 <- fasano.franceschini.test(S1, S2, nPermute = 100, seed = 0, method = 'r')
    test2 <- fasano.franceschini.test(S1, S2, nPermute = 100, seed = 0, method = 'r')
    comp(test1, test2)

    S1 <- data.frame(rnorm(n = 33, mean = 1.4, sd = 0.3),
                     rnorm(n = 33, mean = 1.5, sd = 1),
                     rnorm(n = 33, mean = 1.6, sd = 1.8))
    S2 <- data.frame(rnorm(n = 206, mean = 1.4, sd = 0.3),
                     rnorm(n = 206, mean = 1.5, sd = 1),
                     rnorm(n = 206, mean = 1.6, sd = 1.8))
    test1 <- fasano.franceschini.test(S1, S2, nPermute = 20, seed = 1, method = 'b')
    test2 <- fasano.franceschini.test(S1, S2, nPermute = 20, seed = 1, method = 'b')
    comp(test1, test2)

    S1 <- data.frame(rnorm(n = 20), rnorm(n = 20), rnorm(n = 20))
    S2 <- data.frame(rnorm(n = 35), rnorm(n = 35), rnorm(n = 35))
    test1 <- fasano.franceschini.test(S1, S2, seed = 2)
    test2 <- fasano.franceschini.test(S1, S2, seed = 2)
    comp(test1, test2)
})

test_that("test that seed works for parallel version", {
    set.seed(0)

    comp <- function(test1, test2) {
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-14)
        expect_equal(test1$estimate, test2$estimate, tolerance = 1e-14)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-14)
    }

    S1 <- data.frame(rnorm(n = 50, mean = 0.2, sd = 2),
                     rnorm(n = 50, mean = 1.3, sd = 1.1))
    S2 <- data.frame(rnorm(n = 83, mean = 0.2, sd = 2),
                     rnorm(n = 83, mean = 1.3, sd = 1.1))
    test1 <- fasano.franceschini.test(S1, S2, nPermute = 100, seed = 0, method = 'r', threads = 2)
    test2 <- fasano.franceschini.test(S1, S2, nPermute = 100, seed = 0, method = 'r', threads = 2)
    comp(test1, test2)

    S1 <- data.frame(rnorm(n = 43, mean = 1.6, sd = 1),
                     rnorm(n = 43, mean = 1.5, sd = 1),
                     rnorm(n = 43, mean = 1.4, sd = 1))
    S2 <- data.frame(rnorm(n = 106, mean = 1.4, sd = 2),
                     rnorm(n = 106, mean = 1.5, sd = 2),
                     rnorm(n = 106, mean = 1.6, sd = 2))
    test1 <- fasano.franceschini.test(S1, S2, nPermute = 20, seed = 1, method = 'b', threads = 2)
    test2 <- fasano.franceschini.test(S1, S2, nPermute = 20, seed = 1, method = 'b', threads = 2)
    comp(test1, test2)

    S1 <- data.frame(rnorm(n = 25), rnorm(n = 25), rnorm(n = 25))
    S2 <- data.frame(rnorm(n = 32), rnorm(n = 32), rnorm(n = 32))
    test1 <- fasano.franceschini.test(S1, S2, seed = 2, threads = 4)
    test2 <- fasano.franceschini.test(S1, S2, seed = 2, threads = 4)
    comp(test1, test2)
})
