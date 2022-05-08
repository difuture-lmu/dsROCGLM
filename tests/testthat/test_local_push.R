context("Check if push returns a useable call")

test_that("Check if push returns a useable call", {
  obj = lm(Sepal.Width ~ ., data = iris)

  expect_silent({ cl = pushObject("Dummy", iris, just_return_call = TRUE) })
  expect_equal(iris, eval(parse(text = cl)))

  expect_silent({ cl = pushObject("Dummy", iris, just_return_call = TRUE, package = "abcdefg123") })
  expect_error({ eval(parse(text = cl))})
})
