context("Test if brier score works")

test_that("brier score works", {
  x <<- runif(100, 0, 1)
  p <<- rbinom(100, 1, x)

  expect_error(brierScore("fda", "fdsa"))
  expect_error(brierScore(c("p", "p", "x")))
  expect_error(brierScore("p", c("x", "x")))

  p1 = c(p, 3)
  expect_error(brierScore("p1", "x"))

  x1 = runif(100, -1, 2)
  expect_error(brierScore("p", "x1"))

  x2 = c(x, 0.4)
  expect_error(brierScore("p", "x2"))

  x3 = c(x, 1.4)
  expect_error(brierScore("p", "x3"))

  expect_equal(brierScore("p", "x")$bs, mean((x - p)^2))
  expect_equal(brierScore("p", "x")$n, length(x))
})
