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
