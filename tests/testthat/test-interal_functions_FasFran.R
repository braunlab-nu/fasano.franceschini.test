test_that("ksCDF output has limits", {
  # input between 0 and Inf
  # output between 0 and 1
  expect_equal(ksCDF(Inf), 0)
  expect_equal(ksCDF(0), 1)
  expect_error(ksCDF(-0.1), "lambda must be between 0 and Inf, negative number supplied")
})



test_that("quadCount computes accurate normalized fractions", {
  base <- quadCount(
    x_origin = 0,
    y_origin = 0,
    x = c(rep(1, 4), rep(-1, 3), rep(-1, 2), 1),
    y = c(rep(1, 4), rep(1, 3), rep(-1, 2), -1)
  )

  expect_length(base, 4L)
  expect_equal(base, c(0.4, 0.3, 0.2, 0.1))
})



test_that("quadCount handles ties", {

  # Tied with quad 1 and quad 2
  expect_equal(
    quadCount(
      x_origin = 0,
      y_origin = 0,
      x = c(1, 0, -1, 1),
      y = c(1, 1, -1, -1)
    ),
    c(0.375, 0.125, 0.25, 0.25)
  )

  # Tied with quad 3 and quad 4
  expect_equal(
    quadCount(
      x_origin = 0,
      y_origin = 0,
      x = c(1, -1, 0, 1),
      y = c(1, 1, -1, -1)
    ),
    c(0.25, 0.25, 0.125, 0.375)
  )


  # Tied with quad 1 and quad 4
  expect_equal(
    quadCount(
      x_origin = 0,
      y_origin = 0,
      x = c(1, -1, -1, 1),
      y = c(0, 1, -1, -1)
    ),
    c(0.125, 0.25, 0.25, 0.375)
  )

  # Tied with quad 2 and quad 3
  expect_equal(
    quadCount(
      x_origin = 0,
      y_origin = 0,
      x = c(1, -1, -1, 1),
      y = c(1, 0, -1, -1)
    ),
    c(0.25, 0.125, 0.375, 0.25)
  )

  # Tied with origin
  expect_equal(
    quadCount(
      x_origin = 0,
      y_origin = 0,
      x = c(0, -1, -1, 1),
      y = c(0, 1, -1, -1)
    ),
    c(0.0625, 0.3125, 0.3125, 0.3125)
  )
})



test_that("getDstat computes max difference", {

  # sample 1 - 10 points in quad 1
  S1 <- data.frame(
    x = c(rep(1, 10), rep(-1, 4), rep(-1, 3), rep(1, 2), 0),
    y = c(rep(1, 10), rep(1, 4), rep(-1, 3), rep(-1, 2), 0)
  )
  # sample 2 - 1 points in quad 1
  S2 <- data.frame(
    x = c(1, rep(-1, 4), rep(-1, 3), rep(1, 2)),
    y = c(1, rep(1, 4), rep(-1, 3), rep(-1, 2))
  )

  base <- getDstat(originSamples = S1, S1 = S1, S2 = S2, cores = 1)

  # expectation: (10.25/20) - (1/10) = 0.4125
  expect_equal(base, 0.4125)
  expect_length(base, 1L)
})
