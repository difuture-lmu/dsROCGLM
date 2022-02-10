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
#' @return Variance of differences of positive scores
#' @author Daniel S.
#' @export
getPositiveScoresVar = function(truth_name, prob_name) {
  df_pred = checkTruthProb(truth_name, prob_name)

  truth = df_pred$truth
  prob  = df_pred$prob

  ## Calculate brier score just if there are at least five or more values to ensure privacy:
  nfilter_privacy = .getPrivacyLevel()
  if (length(truth) < nfilter_privacy)
    stop("More than ", nfilter_privacy, " observations are required to ensure privacy!")

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
#' @param epsilon (`numeric(1L)`) Privacy parameter for differential privacy (DP).
#' @param delta (`numeric(1L)`) Probability of violating epsilon DP.
#' @return Positive scores
#' @author Daniel S.
#' @export
getPositiveScores = function(truth_name, prob_name, epsilon = 0.2, delta = 0.2) {
  df_pred = checkTruthProb(truth_name, prob_name)
  checkmate::assertNumeric(epsilon, len = 1L, lower = 0, upper = 1)
  checkmate::assertNumeric(delta, len = 1L, lower = 0, upper = 1)

  if (epsilon == 0) stop("Epsilon must be > 0")
  if (delta == 0) stop("Delta must be > 0")

  if (! "l2s" %in% c(ls(envir = .GlobalEnv), ls()))
    stop("Cannot find l2 sensitivity. Please push an l2 sensitivity with name 'l2s' to the servers.")

  l2s = eval(parse(text = "l2s"))
  assertNumeric(l2s, len = 1L, lower = 0)
  if (l2s == 0) stop("L2 sensitivity must be > 0")

  truth = df_pred$truth
  prob  = df_pred$prob

  pv  = prob[truth == 1]
  sde = sqrt(2 * log(1.25 / delta)) * l2s$l2sens / epsilon

  return(rnorm(n = length(pv), mean = pv, sd = sde))
}

#'
#' @title Return variance of differences of negative scores
#' @description This function just returns negative scores and is used
#'   as aggregator to send these positive scores.
#' @param truth_name (`character(1L)`) Character containing the name of the vector of 0-1-values
#'   encoded as integer or numeric.
#' @param prob_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @return Variance of differences of Negative scores
#' @author Daniel S.
#' @export
getNegativeScoresVar = function(truth_name, prob_name) {
  df_pred = checkTruthProb(truth_name, prob_name)

  truth = df_pred$truth
  prob  = df_pred$prob

  ## Calculate brier score just if there are at least five or more values to ensure privacy:
  nfilter_privacy = .getPrivacyLevel()
  if (length(truth) < nfilter_privacy)
    stop("More than ", nfilter_privacy, " observations are required to ensure privacy!")

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
#' @param epsilon (`numeric(1L)`) Privacy parameter for differential privacy (DP).
#' @param delta (`numeric(1L)`) Probability of violating epsilon DP.
#' @return Negative scores
#' @author Daniel S.
#' @export
getNegativeScores = function(truth_name, prob_name, epsilon = 0.2, delta = 0.2) {
  df_pred = checkTruthProb(truth_name, prob_name)

  checkmate::assertNumeric(epsilon, len = 1L, lower = 0, upper = 1)
  checkmate::assertNumeric(delta, len = 1L, lower = 0, upper = 1)

  if (epsilon == 0) stop("Epsilon must be > 0")
  if (delta == 0) stop("Delta must be > 0")

  if (! "l2s" %in% c(ls(envir = .GlobalEnv), ls()))
    stop("Cannot find l2 sensitivity. Please push an l2 sensitivity with name 'l2s' to the servers.")

  l2s = eval(parse(text = "l2s"))
  assertNumeric(l2s, len = 1L, lower = 0)
  if (l2s == 0) stop("L2 sensitivity must be > 0")


  truth = df_pred$truth
  prob  = df_pred$prob

  nv  = prob[truth == 0]
  sde = sqrt(2 * log(1.25 / delta)) * l2s$l2sens / epsilon

  return(rnorm(n = length(nv), mean = nv, sd = sde))
}
