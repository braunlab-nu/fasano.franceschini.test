test_that("test that seed works for serial version", {
    set.seed(0)

    comp <- function(S1, S2, p, seed, m1, m2) {
        test1 <- fasano.franceschini.test(S1, S2, nPermute = p, seed = seed, method = m1)
        test2 <- fasano.franceschini.test(S1, S2, nPermute = p, seed = seed, method = m2)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)

        test1 <- fasano.franceschini.test(S2, S1, nPermute = p, seed = seed, method = m1)
        test2 <- fasano.franceschini.test(S2, S1, nPermute = p, seed = seed, method = m2)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)

        test1 <- fasano.franceschini.test(S1, S1, nPermute = p, seed = seed, method = m1)
        test2 <- fasano.franceschini.test(S1, S1, nPermute = p, seed = seed, method = m2)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)

        test1 <- fasano.franceschini.test(S2, S2, nPermute = p, seed = seed, method = m1)
        test2 <- fasano.franceschini.test(S2, S2, nPermute = p, seed = seed, method = m2)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)
    }

    S1 <- data.frame(rnorm(n = 50, mean = 0.2, sd = 2),
                     rnorm(n = 50, mean = 1.3, sd = 1.1))
    S2 <- data.frame(rnorm(n = 83, mean = 0.2, sd = 2),
                     rnorm(n = 83, mean = 1.3, sd = 1.1))
    comp(S1, S2, 50, 0, 'r', 'r')
    comp(S1, S2, 60, 1, 'r', 'b')
    comp(S1, S2, 70, 2, 'b', 'r')
    comp(S1, S2, 82, 3, 'b', 'b')

    S1 <- data.frame(rnorm(n = 33, mean = 1.4, sd = 0.3),
                     rnorm(n = 33, mean = 1.5, sd = 1),
                     rnorm(n = 33, mean = 1.6, sd = 1.8))
    S2 <- data.frame(rnorm(n = 206, mean = 1.4, sd = 0.3),
                     rnorm(n = 206, mean = 1.5, sd = 1),
                     rnorm(n = 206, mean = 1.6, sd = 1.8))
    comp(S1, S2, 100, 1, 'r', 'b')
    comp(S1, S2, 100, 0, 'r', 'r')
    comp(S1, S2, 100, -1, 'b', 'r')

    S1 <- data.frame(rnorm(n = 20), rnorm(n = 20), rnorm(n = 20))
    S2 <- data.frame(rnorm(n = 35), rnorm(n = 35), rnorm(n = 35))
    comp(S1, S2, 1, 0, 'r', 'r')
    comp(S1, S2, 2, 0, 'b', 'r')
    comp(S1, S2, 3, 0, 'r', 'b')
    comp(S1, S2, 4, 0, 'r', 'b')
    comp(S1, S2, 5, 0, 'b', 'b')

    S1 <- data.frame(rep(1, 10), rep(2, 10))
    S2 <- data.frame(rep(2, 10), rep(1, 10))
    comp(S1, S2, 100, -10, 'r', 'r')
    comp(S1, S2, 100, -5, 'r', 'r')
    comp(S1, S2, 100, 0, 'r', 'r')

    S1 <- data.frame(rep(1, 10), rep(1, 10), rep(1, 10), rep(1, 10), rep(1, 10))
    S2 <- data.frame(rep(2, 10), rep(2, 10), rep(2, 10), rep(2, 10), rep(1, 10))
    comp(S1, S2, 93, 1, 'r', 'r')
    comp(S1, S2, 95, 2, 'r', 'b')
    comp(S1, S2, 97, -3, 'b', 'r')
    comp(S1, S2, 101, 123, 'b', 'b')
})

test_that("test that seed works for parallel version", {
    set.seed(0)

    comp <- function(S1, S2, p, seed, m1, m2, t1, t2) {
        test1 <- fasano.franceschini.test(S1, S2, nPermute = p, seed = seed,
                                          method = m1, threads = t1)
        test2 <- fasano.franceschini.test(S1, S2, nPermute = p, seed = seed,
                                          method = m2, threads = t2)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)

        test1 <- fasano.franceschini.test(S2, S1, nPermute = p, seed = seed,
                                          method = m1, threads = t1)
        test2 <- fasano.franceschini.test(S2, S1, nPermute = p, seed = seed,
                                          method = m2, threads = t2)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)

        test1 <- fasano.franceschini.test(S1, S1, nPermute = p, seed = seed,
                                          method = m1, threads = t1)
        test2 <- fasano.franceschini.test(S1, S1, nPermute = p, seed = seed,
                                          method = m2, threads = t2)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)

        test1 <- fasano.franceschini.test(S2, S2, nPermute = p, seed = seed,
                                          method = m1, threads = t1)
        test2 <- fasano.franceschini.test(S2, S2, nPermute = p, seed = seed,
                                          method = m2, threads = t2)
        expect_equal(test1$statistic, test2$statistic, tolerance = 1e-15)
        expect_equal(test1$p.value, test2$p.value, tolerance = 1e-15)
    }

    S1 <- data.frame(rnorm(n = 50, mean = 0.2, sd = 2),
                     rnorm(n = 50, mean = 1.3, sd = 1.1))
    S2 <- data.frame(rnorm(n = 83, mean = 0.2, sd = 2),
                     rnorm(n = 83, mean = 1.3, sd = 1.1))
    comp(S1, S2, 10, -3, 'r', 'r', 1, 1)
    comp(S1, S2, 10, -3, 'r', 'r', 1, 2)
    comp(S1, S2, 10, -3, 'r', 'r', 1, 3)
    comp(S1, S2, 10, -3, 'r', 'r', 1, 4)

    S1 <- data.frame(rnorm(n = 43, mean = 1.6, sd = 1),
                     rnorm(n = 43, mean = 1.5, sd = 1),
                     rnorm(n = 43, mean = 1.4, sd = 1))
    S2 <- data.frame(rnorm(n = 106, mean = 1.4, sd = 2),
                     rnorm(n = 106, mean = 1.5, sd = 2),
                     rnorm(n = 106, mean = 1.6, sd = 2))
    comp(S1, S2, 43, 0, 'b', 'b', 1, 1)
    comp(S1, S2, 43, 0, 'b', 'b', 2, 2)
    comp(S1, S2, 43, 0, 'b', 'b', 3, 3)
    comp(S1, S2, 43, 0, 'b', 'b', 4, 4)

    S1 <- data.frame(rnorm(n = 25), rnorm(n = 25), rnorm(n = 25))
    S2 <- data.frame(rnorm(n = 32), rnorm(n = 32), rnorm(n = 32))
    comp(S1, S2, 101, 1, 'r', 'b', 2, 3)
    comp(S1, S2, 101, 1, 'r', 'b', 3, 2)
    comp(S1, S2, 101, 1, 'r', 'b', 2, 5)
    comp(S1, S2, 101, 1, 'r', 'b', 5, 2)

    S1 <- data.frame(rep(1, 10), rep(1, 10))
    S2 <- data.frame(rep(1, 20), rep(1, 20))
    comp(S1, S2, 202, -3, 'r', 'r', 4, 4)
    comp(S1, S2, 202, -3, 'b', 'r', 4, 4)
    comp(S1, S2, 202, -3, 'r', 'b', 4, 4)
    comp(S1, S2, 202, -3, 'b', 'b', 4, 4)

    S1 <- data.frame(rep(1, 10), rep(1, 10))
    S2 <- data.frame(rep(1, 20), rep(1, 20))
    comp(S1, S2, 221, 0, 'b', 'b', 3, 3)
    comp(S1, S2, 221, 1, 'r', 'b', 3, 3)
    comp(S1, S2, 221, 2, 'b', 'r', 3, 3)
    comp(S1, S2, 221, 3, 'r', 'r', 3, 3)

    S1 <- data.frame(rep(1, 20), rep(2, 20), rep(3, 20))
    S2 <- data.frame(rep(1, 3), rep(2, 3), rep(3, 3))
    comp(S1, S2, 0, 0, 'r', 'r', 1, 1)
    comp(S1, S2, 0, 0, 'r', 'r', 1, 2)
    comp(S1, S2, 0, 0, 'r', 'r', 2, 2)
    comp(S1, S2, 0, 0, 'r', 'r', 1, 3)
    comp(S1, S2, 0, 0, 'r', 'r', 3, 2)
    comp(S1, S2, 0, 0, 'r', 'r', 1, 4)
    comp(S1, S2, 0, 0, 'r', 'r', 4, 3)
    comp(S1, S2, 1, 1, 'b', 'r', 2, 3)
    comp(S1, S2, 2, 4, 'r', 'r', 3, 2)
    comp(S1, S2, 3, 7, 'r', 'b', 3, 3)
    comp(S1, S2, 4, 2, 'b', 'r', 2, 2)
    comp(S1, S2, 5, -4, 'r', 'r', 2, 2)
    comp(S1, S2, 6, -134, 'b', 'b', 2, 2)
    comp(S1, S2, 7, 7000, 'r', 'r', 2, 2)
    comp(S1, S2, 8, -10000, 'b', 'r', 2, 2)
    comp(S1, S2, 9, 3223, 'r', 'b', 2, 2)
})
