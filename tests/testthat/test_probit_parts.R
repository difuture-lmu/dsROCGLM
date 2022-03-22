context("Test if probit parts are calculated correctly")

test_that("score vector is correct", {
  dat = airquality[complete.cases(airquality), ]
  dat$oz = ifelse(dat$Ozone >= 30, 1, 0)
  dat <<- dat
  form = "oz ~ Solar.R + Wind + Temp"
  mod = glm(as.formula(form), family = binomial(link = "probit"), data = dat)
  glm_obs_info = solve(stats::vcov(mod))
  glm_dev = mod$deviance

  params = coef(mod)
  params_char = "xxx"
  params_char = paste(paste0(seq_along(params), "xex", params), collapse = "xnx")

  parts = calculateDistrGLMParts(form, data = "dat", params_char = params_char)

  expect_equal(-2 * log(parts$likelihood), glm_dev)
  #glm_obs_info
  #parts$XtX
  #(dev = -2 * log(parts$likelihood))
})

test_that("individual parts can be calculated", {
  score <<- runif(10)
  truth <<- ifelse(score > 0.5, 1, 0)

  expect_silent({tmp1 = rocGLMFrame("truth", "score", "score")})
  expect_silent({tmp2 = dsROCGLM:::computePlacementValues("score", "truth", "score", tset = seq(0, 1, 0.1))})
  expect_silent({tmp3 = dsROCGLM:::calcU(seq(0, 1, 0.1), tmp2)})

  expect_true(inherits(tmp1, "data.frame"))
  expect_true(inherits(tmp2, "numeric"))
  expect_true(inherits(tmp3, "matrix"))
})
