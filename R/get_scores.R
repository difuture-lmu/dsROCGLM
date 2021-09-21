#'
#' @title Truth and Prediction Checker
#' @description This function checks if the vector of true values and predictions
#'   has the correct format to be used for the ROC-GLM. If something does not suit,
#'   then the function tries to convert it into the correct format.
#' @param truth_name (`character(1L)`) Character containing the name of the vector of 0-1-values
#'   encoded as integer or numeric.
#' @param prob_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @param pos (`character(1L)`) Character containing the name of the positive class.
#' @return Data frame containing the truth and prediction vector.
#' @author Daniel S.
checkTruthProb = function(truth_name, prob_name, pos = NULL) {
  #if (! truth_name %in% ls()) stop(quote(truth_name), " is no element of the environment!")
  #if (! prob_name %in% ls()) stop(quote(prob_name), " is no element of the environment!")

  truth = eval(parse(text = truth_name))
  prob  = eval(parse(text = prob_name))

  if (length(unique(truth)) > 2)
    stop("\"", truth_name, "\" contains ", length(unique(truth)), " > 2 elements! Two are required!")

  ntruth = length(truth)
  checkmate::assertNumeric(prob, any.missing = FALSE, len = ntruth, null.ok = FALSE, finite = TRUE)

  if (is.null(pos)) {
    if (is.character(truth) | is.factor(truth))
      warning("\"", truth_name, "\" is not enocded as 0-1 integer, conversion is done automatically.",
        "This may lead to a label flip! Set argument \"pos\" to ensure correct encoding.")

    if (is.character(truth)) truth = as.integer(as.factor(truth))
    if (is.factor(truth))    truth = as.integer(truth)

    if (max(truth) == 2) truth = truth - 1
  } else {
    if (is.character(truth) | is.factor(truth)) truth = ifelse(truth == pos, 1, 0)
    if (is.numeric(truth))
      warning(quote("pos"), " is set but \"", truth_name, "\" is numeric. Are you sure",
        " you know what the response really is?")
  }
  return(invisible(data.frame(truth = truth, prob = prob)))
}

#'
#' @title Return variance of score differneces
#' @description
#' @param truth_name (`character(1L)`) Character containing the name of the vector of 0-1-values
#'   encoded as integer or numeric.
#' @param prob_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @param lag (`integer(1L)`) Lag to the next neighbours considered for calculating the standard deviation of the noise.
#' @return Variance of differences of positive scores
#' @author Daniel S.
#' @export
getPositiveScoresVar = function(truth_name, prob_name, lag = 4L) {
  df_pred = checkTruthProb(truth_name, prob_name)
  checkmate::assertCount(lag, na.ok = FALSE, positive = TRUE)

  truth = df_pred$truth
  prob  = df_pred$prob

  pv  = prob[truth == 1]
  dv  = diff(pv, lag = lag)

  return(sum((dv - mean(dv))^2))
}

#'
#' @title Return positive scores
#' @description This function just returns positive scores and is used
#'   as aggregator to send these positive scores.
#' @param truth_name (`character(1L)`) Character containing the name of the vector of 0-1-values
#'   encoded as integer or numeric.
#' @param prob_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @param sd_noise (`numerical(1L)`) Standard deviation used to add noise to the return value.
#' @param ntimes (`integer(1L)`) Times the standard deviation used for simulating noise added to the data.
#' @return Positive scores
#' @author Daniel S.
#' @export
getPositiveScores = function(truth_name, prob_name, sd_noise, ntimes = 2L) {
  df_pred = checkTruthProb(truth_name, prob_name)
  checkmate::assertNumeric(sd_noise, lower = 0, len = 1L, any.missing = FALSE, null.ok = FALSE)
  checkmate::assertCount(ntimes, na.ok = FALSE, positive = TRUE)

  truth = df_pred$truth
  prob  = df_pred$prob

  pv  = prob[truth == 1]
  sde = ntimes * sd_noise

  return(rnorm(n = length(pv), mean = pv, sd = sde))
}

#'
#' @title Return variance of differences of negative scores
#' @description This function just returns negative scores and is used
#'   as aggregator to send these positive scores.
#' @param truth_name (`character(1L)`) Character containing the name of the vector of 0-1-values
#'   encoded as integer or numeric.
#' @param prob_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @param lag (`integer(1L)`) Lag to the next neighbours considered for calculating the standard deviation of the noise.
#' @return Variance of differences of Negative scores
#' @author Daniel S.
#' @export
getNegativeScoresVar = function(truth_name, prob_name, lag = 4L) {
  df_pred = checkTruthProb(truth_name, prob_name)
  checkmate::assertCount(lag, na.ok = FALSE, positive = TRUE)

  truth = df_pred$truth
  prob  = df_pred$prob

  nv  = prob[truth == 0]
  dv  = diff(nv, lag = lag)

  return(sum((dv - mean(dv))^2))
}

#'
#' @title Return negative scores
#' @description This function just returns negative scores and is used
#'   as aggregator to send these positive scores.
#' @param truth_name (`character(1L)`) Character containing the name of the vector of 0-1-values
#'   encoded as integer or numeric.
#' @param prob_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @param sd_noise (`numerical(1L)`) Standard deviation used to add noise to the return value.
#' @param ntimes (`integer(1L)`) Times the standard deviation used for simulating noise added to the data.
#' @return Negative scores
#' @author Daniel S.
#' @export
getNegativeScores = function(truth_name, prob_name, sd_noise, ntimes = 2L) {
  df_pred = checkTruthProb(truth_name, prob_name)
  checkmate::assertNumeric(sd_noise, lower = 0, len = 1L, any.missing = FALSE, null.ok = FALSE)
  checkmate::assertCount(ntimes, na.ok = FALSE, positive = TRUE)

  truth = df_pred$truth
  prob  = df_pred$prob

  nv  = prob[truth == 0]
  sde = ntimes * sd_noise

  return(rnorm(n = length(nv), mean = nv, sd = sde))
}


