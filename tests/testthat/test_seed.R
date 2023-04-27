test_that("test that seed works for serial version", {
    set.seed(0)

    comp <- function(test1, test2) {
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)
    }

    S1 <- data.frame(rnorm(n = 50, mean = 0.2, sd = 2),
                     rnorm(n = 50, mean = 1.3, sd = 1.1))
    S2 <- data.frame(rnorm(n = 83, mean = 0.2, sd = 2),
                     rnorm(n = 83, mean = 1.3, sd = 1.1))
    comp(fasano.franceschini.test(S1, S2, nPermute = 100, seed = 0, method = 'r'),
         fasano.franceschini.test(S1, S2, nPermute = 100, seed = 0, method = 'r'))
    comp(fasano.franceschini.test(S1, S1, nPermute = 100, seed = 1, method = 'r'),
         fasano.franceschini.test(S1, S1, nPermute = 100, seed = 1, method = 'r'))
    comp(fasano.franceschini.test(S2, S2, nPermute = 100, seed = 2, method = 'r'),
         fasano.franceschini.test(S2, S2, nPermute = 100, seed = 2, method = 'r'))

    S1 <- data.frame(rnorm(n = 33, mean = 1.4, sd = 0.3),
                     rnorm(n = 33, mean = 1.5, sd = 1),
                     rnorm(n = 33, mean = 1.6, sd = 1.8))
    S2 <- data.frame(rnorm(n = 206, mean = 1.4, sd = 0.3),
                     rnorm(n = 206, mean = 1.5, sd = 1),
                     rnorm(n = 206, mean = 1.6, sd = 1.8))
    comp(fasano.franceschini.test(S1, S2, nPermute = 1, seed = 1, method = 'r'),
         fasano.franceschini.test(S1, S2, nPermute = 1, seed = 1, method = 'b'))
    comp(fasano.franceschini.test(S1, S2, nPermute = 2, seed = 2, method = 'b'),
         fasano.franceschini.test(S1, S2, nPermute = 2, seed = 2, method = 'r'))
    comp(fasano.franceschini.test(S1, S2, nPermute = 3, seed = 3, method = 'r'),
         fasano.franceschini.test(S1, S2, nPermute = 3, seed = 3, method = 'r'))
    comp(fasano.franceschini.test(S1, S2, nPermute = 4, seed = 4, method = 'b'),
         fasano.franceschini.test(S1, S2, nPermute = 4, seed = 4, method = 'b'))

    S1 <- data.frame(rnorm(n = 20), rnorm(n = 20), rnorm(n = 20))
    S2 <- data.frame(rnorm(n = 35), rnorm(n = 35), rnorm(n = 35))
    comp(fasano.franceschini.test(S1, S2, seed = 2),
         fasano.franceschini.test(S1, S2, seed = 2))

    S1 <- data.frame(rep(1, 10), rep(2, 10))
    S2 <- data.frame(rep(2, 10), rep(1, 10))
    comp(fasano.franceschini.test(S1, S2, seed = 2),
         fasano.franceschini.test(S1, S2, seed = 2))
    comp(fasano.franceschini.test(S1, S1, seed = 3),
         fasano.franceschini.test(S1, S1, seed = 3))
    comp(fasano.franceschini.test(S2, S2, seed = 0),
         fasano.franceschini.test(S2, S2, seed = 0))

    S1 <- data.frame(rep(1, 10), rep(1, 10), rep(1, 10), rep(1, 10), rep(1, 10))
    S2 <- data.frame(rep(2, 10), rep(2, 10), rep(2, 10), rep(2, 10), rep(1, 10))
    comp(fasano.franceschini.test(S1, S2, seed = 2, nPermute = 6),
         fasano.franceschini.test(S1, S2, seed = 2, nPermute = 6))
    comp(fasano.franceschini.test(S1, S1, seed = 3, nPermute = 12),
         fasano.franceschini.test(S1, S1, seed = 3, nPermute = 12))
    comp(fasano.franceschini.test(S2, S2, seed = 0, nPermute = 18),
         fasano.franceschini.test(S2, S2, seed = 0, nPermute = 18))
})

test_that("test that seed works for parallel version", {
    set.seed(0)

    comp <- function(test1, test2) {
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)
    }

    S1 <- data.frame(rnorm(n = 50, mean = 0.2, sd = 2),
                     rnorm(n = 50, mean = 1.3, sd = 1.1))
    S2 <- data.frame(rnorm(n = 83, mean = 0.2, sd = 2),
                     rnorm(n = 83, mean = 1.3, sd = 1.1))
    test1 <- fasano.franceschini.test(S1, S2, nPermute = 100, seed = 0, method = 'r', threads = 2)
    test2 <- fasano.franceschini.test(S1, S2, nPermute = 100, seed = 0, method = 'r', threads = 2)
    test3 <- fasano.franceschini.test(S1, S2, nPermute = 100, seed = 0, method = 'r', threads = 2)
    comp(test1, test2)
    comp(test1, test3)
    comp(test2, test3)

    S1 <- data.frame(rnorm(n = 43, mean = 1.6, sd = 1),
                     rnorm(n = 43, mean = 1.5, sd = 1),
                     rnorm(n = 43, mean = 1.4, sd = 1))
    S2 <- data.frame(rnorm(n = 106, mean = 1.4, sd = 2),
                     rnorm(n = 106, mean = 1.5, sd = 2),
                     rnorm(n = 106, mean = 1.6, sd = 2))
    test1 <- fasano.franceschini.test(S1, S2, nPermute = 20, seed = 1, method = 'b', threads = 2)
    test2 <- fasano.franceschini.test(S1, S2, nPermute = 20, seed = 1, method = 'b', threads = 2)
    test3 <- fasano.franceschini.test(S1, S2, nPermute = 20, seed = 1, method = 'b', threads = 2)
    comp(test1, test2)
    comp(test1, test3)
    comp(test2, test3)

    S1 <- data.frame(rnorm(n = 25), rnorm(n = 25), rnorm(n = 25))
    S2 <- data.frame(rnorm(n = 32), rnorm(n = 32), rnorm(n = 32))
    test1 <- fasano.franceschini.test(S1, S2, seed = 2, threads = 4)
    test2 <- fasano.franceschini.test(S1, S2, seed = 2, threads = 4)
    test3 <- fasano.franceschini.test(S1, S2, seed = 2, threads = 4)
    test4 <- fasano.franceschini.test(S1, S2, seed = 2, threads = 4)
    comp(test1, test2)
    comp(test1, test3)
    comp(test1, test4)
    comp(test2, test3)
    comp(test2, test4)
    comp(test3, test4)

    S1 <- data.frame(rep(1, 10), rep(1, 10))
    S2 <- data.frame(rep(1, 20), rep(1, 20))
    test1 <- fasano.franceschini.test(S1, S2, seed = 2, threads = 4)
    test2 <- fasano.franceschini.test(S1, S2, seed = 2, threads = 4)
    test3 <- fasano.franceschini.test(S1, S2, seed = 2, threads = 4)
    test4 <- fasano.franceschini.test(S1, S2, seed = 2, threads = 4)
    comp(test1, test2)
    comp(test1, test3)
    comp(test1, test4)
    comp(test2, test3)
    comp(test2, test4)
    comp(test3, test4)

    test1 <- fasano.franceschini.test(S1, S1, seed = 0, threads = 3)
    test2 <- fasano.franceschini.test(S1, S1, seed = 0, threads = 3)
    test3 <- fasano.franceschini.test(S1, S1, seed = 0, threads = 3)
    test4 <- fasano.franceschini.test(S1, S1, seed = 0, threads = 3)
    comp(test1, test2)
    comp(test1, test3)
    comp(test1, test4)
    comp(test2, test3)
    comp(test2, test4)
    comp(test3, test4)

    test1 <- fasano.franceschini.test(S2, S2, seed = 6, threads = 2)
    test2 <- fasano.franceschini.test(S2, S2, seed = 6, threads = 2)
    test3 <- fasano.franceschini.test(S2, S2, seed = 6, threads = 2)
    test4 <- fasano.franceschini.test(S2, S2, seed = 6, threads = 2)
    comp(test1, test2)
    comp(test1, test3)
    comp(test1, test4)
    comp(test2, test3)
    comp(test2, test4)
    comp(test3, test4)

    test1 <- fasano.franceschini.test(S1, S2, seed = 0, threads = 1)
    test2 <- fasano.franceschini.test(S1, S2, seed = 0, threads = 2)
    test3 <- fasano.franceschini.test(S1, S2, seed = 0, threads = 3)
    test4 <- fasano.franceschini.test(S1, S2, seed = 0, threads = 4)
    comp(test1, test2)
    comp(test1, test3)
    comp(test1, test4)
    comp(test2, test3)
    comp(test2, test4)
    comp(test3, test4)

    test1 <- fasano.franceschini.test(S1, S2, seed = 0, threads = 1, nPermute = 7)
    test2 <- fasano.franceschini.test(S1, S2, seed = 0, threads = 2, nPermute = 7)
    test3 <- fasano.franceschini.test(S1, S2, seed = 0, threads = 3, nPermute = 7)
    comp(test1, test2)
    comp(test1, test3)
    comp(test2, test3)

    test1 <- fasano.franceschini.test(S2, S1, seed = 5, threads = 4, nPermute = 8)
    test2 <- fasano.franceschini.test(S2, S1, seed = 5, threads = 2, nPermute = 8)
    test3 <- fasano.franceschini.test(S2, S1, seed = 5, threads = 1, nPermute = 8)
    comp(test1, test2)
    comp(test1, test3)
    comp(test2, test3)

    S1 <- data.frame(rep(1, 20), rep(2, 20), rep(3, 20))
    S2 <- data.frame(rep(1, 3), rep(2, 3), rep(3, 3))
    test1 <- fasano.franceschini.test(S1, S2, seed = 1, threads = 1, nPermute = 5)
    test2 <- fasano.franceschini.test(S1, S2, seed = 1, threads = 2, nPermute = 5)
    test3 <- fasano.franceschini.test(S1, S2, seed = 1, threads = 3, nPermute = 5)
    test4 <- fasano.franceschini.test(S1, S2, seed = 1, threads = 4, nPermute = 5)
    comp(test1, test2)
    comp(test1, test3)
    comp(test1, test4)
    comp(test2, test3)
    comp(test2, test4)
    comp(test3, test4)

    test1 <- fasano.franceschini.test(S2, S1, seed = 0, threads = 1, nPermute = 3)
    test2 <- fasano.franceschini.test(S2, S1, seed = 0, threads = 2, nPermute = 3)
    test3 <- fasano.franceschini.test(S2, S1, seed = 0, threads = 3, nPermute = 3)
    test4 <- fasano.franceschini.test(S2, S1, seed = 0, threads = 4, nPermute = 3)
    comp(test1, test2)
    comp(test1, test3)
    comp(test1, test4)
    comp(test2, test3)
    comp(test2, test4)
    comp(test3, test4)

    test1 <- fasano.franceschini.test(S2, S1, seed = 2, threads = 1, nPermute = 13)
    test2 <- fasano.franceschini.test(S2, S1, seed = 2, threads = 2, nPermute = 13)
    test3 <- fasano.franceschini.test(S2, S1, seed = 2, threads = 3, nPermute = 13)
    test4 <- fasano.franceschini.test(S2, S1, seed = 2, threads = 4, nPermute = 13)
    comp(test1, test2)
    comp(test1, test3)
    comp(test1, test4)
    comp(test2, test3)
    comp(test2, test4)
    comp(test3, test4)
})
