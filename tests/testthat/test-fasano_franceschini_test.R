test_that("FF test uses validated inputs", {
  S1 <- data.frame(
    x = c(1, 1),
    y = c(1, -1)
  )
  S2 <- data.frame(
    x = c(0, 1),
    y = c(0, -1)
  )

  expect_error(fasano.franceschini.test(S1 = NULL, S2 = S2, cores = 1), "S1 and S2 must be a `data.frame`")
  expect_error(fasano.franceschini.test(S1 = NULL, S2 = NULL, cores = 1), "S1 and S2 must be a `data.frame`")
  expect_error(fasano.franceschini.test(S1 = S1, S2 = NULL, cores = 1), "S1 and S2 must be a `data.frame`")
  expect_error(fasano.franceschini.test(S1 = as.matrix(S1), S2 = S2, cores = 1), "S1 and S2 must be a `data.frame`")
  expect_error(fasano.franceschini.test(S1 = S1, S2 = as.matrix(S2), cores = 1), "S1 and S2 must be a `data.frame`")
  expect_error(fasano.franceschini.test(S1 = as.matrix(S1), S2 = as.matrix(S2), cores = 1), "S1 and S2 must be a `data.frame`")
  expect_error(fasano.franceschini.test(S1 = S1, S2 = S2, nPermute = -1, cores = 1), "nPermute must be a positive value")
})



test_that("FF test computes with bootstrap, bootstrap is depreciated", {

  # sample 1 - 30 points in quad 1
  S1 <- data.frame(
    x = c(rep(1, 30), rep(-1, 4), rep(-1, 3), rep(1, 2), 0),
    y = c(rep(1, 30), rep(1, 4), rep(-1, 3), rep(-1, 2), 0)
  )
  # sample 2 - 1 points in quad 1
  S2 <- data.frame(
    x = c(1, rep(-1, 4), rep(-1, 3), rep(1, 2)),
    y = c(1, rep(1, 4), rep(-1, 3), rep(-1, 2))
  )

  set.seed(123)

  output <- fasano.franceschini.test(S1 = S1, S2 = S2, nBootstrap = 1000, cores = 1)
  expect_equal(unname(output$p.value), 0.001998002)
  expect_named(output$p.value, "p-value")
  expect_equal(unname(output$statistic), 0.525)
  expect_named(output$statistic, "D-stat")
  expect_warning(fasano.franceschini.test(S1 = S1, S2 = S2, nBootstrap = 1000, cores = 1),"The 'nPermute' argument has been set equal to 'nBootstrap'.")
})

test_that("FF test computes with Permute", {

  # sample 1 - 30 points in quad 1
  S1 <- data.frame(
    x = c(rep(1, 30), rep(-1, 4), rep(-1, 3), rep(1, 2), 0),
    y = c(rep(1, 30), rep(1, 4), rep(-1, 3), rep(-1, 2), 0)
  )
  # sample 2 - 1 points in quad 1
  S2 <- data.frame(
    x = c(1, rep(-1, 4), rep(-1, 3), rep(1, 2)),
    y = c(1, rep(1, 4), rep(-1, 3), rep(-1, 2))
  )

  set.seed(123)

  output <- fasano.franceschini.test(S1 = S1, S2 = S2, nPermute = 1000, cores = 1)
  expect_equal(unname(output$p.value), 0.001998002)
  expect_named(output$p.value, "p-value")
  expect_equal(unname(output$statistic), 0.525)
  expect_named(output$statistic, "D-stat")
})



test_that("FF test computes with Press and Teukolsky model fit", {

  # sample 1 - 30 points in quad 1
  S1 <- data.frame(
    x = c(rep(1, 30), rep(-1, 4), rep(-1, 3), rep(1, 2), 0),
    y = c(rep(1, 30), rep(1, 4), rep(-1, 3), rep(-1, 2), 0)
  )

  # sample 2 - 1 points in quad 1
  S2 <- data.frame(
    x = c(1, rep(-1, 4), rep(-1, 3), rep(1, 2)),
    y = c(1, rep(1, 4), rep(-1, 3), rep(-1, 2))
  )

  set.seed(123)

  output <- fasano.franceschini.test(S1 = S1, S2 = S2, cores = 1)
  expect_equal(unname(output$p.value), 0.0213812679306983)
  expect_named(output$p.value, "p-value")
  expect_equal(unname(output$statistic), 0.525)
  expect_named(output$statistic, "D-stat")

})
