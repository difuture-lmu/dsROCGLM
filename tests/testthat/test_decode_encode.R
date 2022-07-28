context("Ceck if decoding and encoding produces the same model")

test_that("encode produces outpout", {
  mod = lm(Sepal.Width ~ ., data = iris)

  expect_silent({ bin = encodeObject(mod) })
  expect_equal(names(bin), "mod")
  expect_true(is.character(bin))
})

test_that("Decode - encode creates the same object", {
  assign("mod", lm(Sepal.Width ~ ., data = iris), .GlobalEnv)

  expect_silent({ bin = encodeObject(mod) })
  expect_silent({ mod_b = decodeBinary(bin)})
  expect_equal(mod, mod_b)
})
