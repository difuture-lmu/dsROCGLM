context("Check if remove missings works correctly")

test_that("remove missings really removes missings", {

  x <<- data.frame(x = c(1, 2, 3), y = c(3, NA, 1))
  expect_error(removeMissings())
  expect_error(removeMissings("bla"))
  expect_equal(na.omit(x), removeMissings("x"))
})
