context("Test if score getter are working properly")

test_that("test truth prob checker", {

  truth <<- rep(c(0, 1), 5)
  score <<- rnorm(10)
  truth2 <<- LETTERS[truth + 1]
  truth3 <<- truth2
  truth3[1] = "C"
  truth3 <<- truth3

  expect_silent({df_ts = dsROCGLM:::checkTruthProb("truth", "score")})
  expect_equal(truth, df_ts$truth)
  expect_equal(score, df_ts$prob)

  expect_warning(dsROCGLM:::checkTruthProb("truth2", "score"))
  expect_error(dsROCGLM:::checkTruthProb("truth3", "score"))
  expect_warning(dsROCGLM:::checkTruthProb("truth", "score", pos = "bla"))
})

test_that("score variances can be calculated", {
  truth <<- rep(c(0, 1), 5)
  score <<- rnorm(10)

  pidx = truth == 1
  nidx = truth == 0

  expect_equal(sum((score[pidx] - mean(score[pidx]))^2), getPositiveScoresVar("truth", "score"))
  expect_equal(sum((score[nidx] - mean(score[nidx]))^2), getNegativeScoresVar("truth", "score"))

  m = 1
  expect_equal(sum((score[pidx] - m)^2), getPositiveScoresVar("truth", "score", m))
  expect_equal(sum((score[nidx] - m)^2), getNegativeScoresVar("truth", "score", m))

  expect_equal(sum(score[pidx]), getPositiveScoresVar("truth", "score", return_sum = TRUE))
  expect_equal(sum(score[nidx]), getNegativeScoresVar("truth", "score", return_sum = TRUE))
})

test_that("score getter works", {
  truth <<- rep(c(0, 1), 5)
  score <<- rnorm(10)

  expect_error(getPositiveScores("truth", "score"))

  l2s <<- 0.01

  expect_silent({s1 = getPositiveScores("truth", "score", seed_object = "l2s", sort = TRUE)})
  expect_silent({s2 = getPositiveScores("truth", "score", seed_object = "l2s", sort = TRUE)})
  expect_equal(s1, s2)

  expect_silent({s1 = getNegativeScores("truth", "score", seed_object = "l2s", sort = TRUE)})
  expect_silent({s2 = getNegativeScores("truth", "score", seed_object = "l2s", sort = TRUE)})
  expect_equal(s1, s2)
})
