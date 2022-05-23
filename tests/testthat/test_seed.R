test_that("test that seed works for serial version", {
    set.seed(0)

    S1 <- data.frame(rnorm(n = 50, mean = 0.2, sd = 2),
                     rnorm(n = 50, mean = 1.3, sd = 1.1))
    S2 <- data.frame(rnorm(n = 83, mean = 0.2, sd = 2),
                     rnorm(n = 83, mean = 1.3, sd = 1.1))
    test1 <- fasano.franceschini.test(S1, S2, nPermute = 100, seed = 0)
    test2 <- fasano.franceschini.test(S1, S2, nPermute = 100, seed = 0)
    expect_equal(test1$statistic, test2$statistic, tolerance = 1e-14)
    expect_equal(test1$estimate, test2$estimate, tolerance = 1e-14)
    expect_equal(test1$p.value, test2$p.value, tolerance = 1e-14)

    S1 <- data.frame(rnorm(n = 33, mean = 1.4, sd = 0.3),
                     rnorm(n = 33, mean = 1.5, sd = 1),
                     rnorm(n = 33, mean = 1.6, sd = 1.8))
    S2 <- data.frame(rnorm(n = 206, mean = 1.4, sd = 0.3),
                     rnorm(n = 206, mean = 1.5, sd = 1),
                     rnorm(n = 206, mean = 1.6, sd = 1.8))

    test1 <- fasano.franceschini.test(S1, S2, nPermute = 20, seed = 1)
    test2 <- fasano.franceschini.test(S1, S2, nPermute = 20, seed = 1)
    expect_equal(test1$statistic, test2$statistic, tolerance = 1e-14)
    expect_equal(test1$estimate, test2$estimate, tolerance = 1e-14)
    expect_equal(test1$p.value, test2$p.value, tolerance = 1e-14)

    test1 <- fasano.franceschini.test(S1, S2, nPermute = 31, seed = 2)
    test2 <- fasano.franceschini.test(S1, S2, nPermute = 31, seed = 2)
    expect_equal(test1$statistic, test2$statistic, tolerance = 1e-14)
    expect_equal(test1$estimate, test2$estimate, tolerance = 1e-14)
    expect_equal(test1$p.value, test2$p.value, tolerance = 1e-14)
})
