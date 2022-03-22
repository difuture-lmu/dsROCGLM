context("Check if seeding based on object is working")

test_that("seeding works", {
  # '<<-' is required here because of the testing environment.
  x <<- rnorm(10)
  y <<- x

  expect_error(seedBoundedToObject(x))
  expect_error(seedBoundedToObject("x", rm_attributes = "TRUE"))
  expect_equal(seedBoundedToObject("x"), seedBoundedToObject("y"))

  z <<- x
  attr(z, "a") = 1
  expect_equal(seedBoundedToObject("x"), seedBoundedToObject("z", rm_attributes = TRUE))
})
