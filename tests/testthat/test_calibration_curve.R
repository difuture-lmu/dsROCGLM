context("Test if calibration curve works")

test_that("calibrationCurve works", {
  x <<- runif(100, 0, 1)
  p1 <<- rbinom(100, 1, x)
  p2 <<- rbinom(100, 1, x)

  expect_error(calibrationCurve("fda", "fdsa"))
  expect_error(calibrationCurve(c("p", "p", "x")))
  expect_error(calibrationCurve("p", c("x", "x")))

  expect_error({ calibrationCurve("p1", "x", nbins = 51L) })

  expect_silent({ cc1 = calibrationCurve("p1", "x", nbins = 50L) })
  expect_true(any(is.na(cc1$prob)))
  expect_silent({ cc1 = calibrationCurve("p1", "x", nbins = 30L) })
  expect_true(any(is.na(cc1$prob)))

  expect_silent({ cc1 = calibrationCurve("p1", "x", nbins = 5L) })
  expect_silent({ cc2 = calibrationCurve("p2", "x", nbins = 5L) })
})

test_that("plotCalibrationCurve works", {
  x <<- runif(100, 0, 1)
  set.seed(31415)
  p1 <<- rbinom(100, 1, x)
  set.seed(31415)
  p2 <<- rbinom(100, 1, x)

  expect_silent({ cc1 = calibrationCurve("p1", "x") })
  expect_silent({ cc2 = calibrationCurve("p2", "x") })

  # Prepare individual curves (usually done in `dsCalibrationCurve`:
  individuals = list(cc1, cc2)
  names(individuals) = c("server1", "server2")
  snames = names(individuals)
  bins = individuals[[1]]$bin

  truth   = numeric()
  prob    = numeric()
  weights = integer()
  for (b in bins) {
    tmp_truth   = numeric()
    tmp_prob    = numeric()
    tmp_weights = integer()

    for (s in snames) {
      tmp = individuals[[s]]
      tmp_truth   = c(tmp_truth, tmp$truth[tmp$bin == b] * tmp$n[tmp$bin == b])
      tmp_prob    = c(tmp_prob, tmp$prob[tmp$bin == b] * tmp$n[tmp$bin == b])
      tmp_weights = c(tmp_weights, tmp$n[tmp$bin == b])
    }
    w = sum(tmp_weights, na.rm = TRUE)
    truth = c(truth, sum(tmp_truth, na.rm = TRUE) / w)
    prob  = c(prob, sum(tmp_prob, na.rm = TRUE) / w)
  }
  aggregated = data.frame(bin = bins, lower = individuals[[1]]$lower,
    upper = individuals[[1]]$upper, truth = truth, prob = prob)

  llcc = list(individuals = individuals, aggregated = aggregated)

  expect_error(plotCalibrationCurve("x"))
  expect_error(plotCalibrationCurve(llcc, individuals = "no"))

  expect_silent({ gg = plotCalibrationCurve(llcc, individuals = TRUE) })
  expect_true("ggplot" %in% class(gg))
  expect_silent({ gg = plotCalibrationCurve(llcc, individuals = FALSE) })
  expect_true("ggplot" %in% class(gg))
})
