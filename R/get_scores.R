#'
#' @title Get a seed depending on an object
#' @description This function creates a seed based on the hash of an object.
#' @param object (`character(1L)`) Character containing the name of the object
#'   to which the seed is bounded.
#' @param rm_attributes (`logical(1L)`) Flag whether attributes should be deleted or not.
#' @return Integer containing a seed.
#' @author Daniel S.
#' @export
seedBoundedToObject = function(object, rm_attributes = TRUE) {
  checkmate::assertCharacter(object, len = 1L)
  checkmate::assertLogical(rm_attributes, len = 1L)
  so = eval(parse(text = object))

  if (! (is.numeric(so) || is.data.frame(so)))
    stop("Object must be a \"numeric vector\" or \"data.frame\" and not ", dQuote(class(so)))

  if (rm_attributes) attributes(so) = NULL

  a = digest::sha1(mean(unlist(so), na.rm = TRUE))
  seed_add = as.integer(gsub("[^\\d]+", "", substr(a, 1, 9), perl = TRUE))
  if (is.na(seed_add)) seed_add = 0
  return(seed_add)
}

#'
#' @title Get a seed depending on an object
#' @description This function creates a seed based on the hash of an object.
#' @param object (`character(1L)`) Character containing the name of the object
#'   to which the seed is bounded.
#' @return Object
#' @author Daniel S.
#' @export
getObject = function(object) {
  checkmate::assertCharacter(object, len = 1L)
  so = eval(parse(text = object))
  return(so)
}


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
#' @title Return variance of positive scores
#' @description This function just returns the variance of positive scores.
#' @param truth_name (`character(1L)`) Character containing the name of the vector of 0-1-values
#'   encoded as integer or numeric.
#' @param prob_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @param m (`numeric(1L)`) Sample mean used for variance calculation. If `NULL` (default), the
#'   sample mean of the positive scores is used.
#' @return Variance of differences of positive scores
#' @author Daniel S.
#' @export
getPositiveScoresVar = function(truth_name, prob_name, m = NULL) {
  df_pred = checkTruthProb(truth_name, prob_name)

  checkmate::assertNumeric(m, len = 1L, null.ok = TRUE)

  truth = df_pred$truth
  prob  = df_pred$prob

  ## Calculate brier score just if there are at least five or more values to ensure privacy:
  nfilter_privacy = .getPrivacyLevel()
  if (length(truth) < nfilter_privacy)
    stop("More than ", nfilter_privacy, " observations are required to ensure privacy!")

  pv = prob[truth == 1]

  if (is.null(m))
    return(stats::var(pv) * (length(pv) - 1))
  else
    return(sum((pv - m)^2))
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
#' @param seed_object (`character(1L)`) Name of an object which is used
#'   to add a seed based on an object.
#' @param seed_object (`character(1L)`) Name of an object which is used
#'   to add a seed based on an object.
#' @param sort (`logical(1L)`) Indicator whether the return values should be
#'   sorted or not.
#' @return Positive scores
#' @author Daniel S.
#' @export
getPositiveScores = function(truth_name, prob_name, epsilon = 0.2, delta = 0.2,
  seed_object = NULL, sort = FALSE) {

  df_pred = checkTruthProb(truth_name, prob_name)
  checkmate::assertNumeric(epsilon, len = 1L, lower = 0, upper = 1)
  checkmate::assertNumeric(delta, len = 1L, lower = 0, upper = 1)

  checkmate::assertCharacter(seed_object, null.ok = TRUE, len = 1L)
  checkmate::assertLogical(sort, len = 1L)

  if (epsilon == 0) stop("Epsilon must be > 0")
  if (delta == 0) stop("Delta must be > 0")

  if (! "l2s" %in% c(ls(envir = .GlobalEnv), ls()))
    stop("Cannot find l2 sensitivity. Please push an l2 sensitivity with name 'l2s' to the servers.")

  l2s = eval(parse(text = "l2s"))
  checkmate::assertNumeric(l2s, len = 1L, lower = 0)
  if (l2s == 0) stop("L2 sensitivity must be > 0")

  truth = df_pred$truth
  prob  = df_pred$prob

  if (sort) {
    pv  = sort(prob[truth == 1])
  } else {
    pv  = prob[truth == 1]
  }
  sde = GMVar(l2s, epsilon, delta)

  if (! is.null(seed_object)) {
    seed_old = .Random.seed
    seed = seedBoundedToObject(seed_object)
    set.seed(seed)
  }
  out = stats::rnorm(n = length(pv), mean = pv, sd = sde)

  if (! is.null(seed_object)) set.seed(seed_old)

  return(out)
}

#'
#' @title Return variance of negative scores
#' @description This function just returns the variance of negative scores.
#' @param truth_name (`character(1L)`) Character containing the name of the vector of 0-1-values
#'   encoded as integer or numeric.
#' @param prob_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @param m (`numeric(1L)`) Sample mean used for variance calculation. If `NULL` (default), the
#'   sample mean of the negative scores is used.
#' @return Variance of differences of Negative scores
#' @author Daniel S.
#' @export
getNegativeScoresVar = function(truth_name, prob_name, m = NULL) {
  df_pred = checkTruthProb(truth_name, prob_name)

  truth = df_pred$truth
  prob  = df_pred$prob

  ## Calculate brier score just if there are at least five or more values to ensure privacy:
  nfilter_privacy = .getPrivacyLevel()
  if (length(truth) < nfilter_privacy)
    stop("More than ", nfilter_privacy, " observations are required to ensure privacy!")


  nv = prob[truth == 0]

  if (is.null(m))
    return(stats::var(nv) * (length(nv) - 1))
  else
    return(sum((nv - m)^2))
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
#' @param seed_object (`character(1L)`) Name of an object which is used
#'   to add a seed based on an object.
#' @param sort (`logical(1L)`) Indicator whether the return values should be
#'   sorted or not.
#' @return Negative scores
#' @author Daniel S.
#' @export
getNegativeScores = function(truth_name, prob_name, epsilon = 0.2, delta = 0.2,
  seed_object = NULL, sort = FALSE) {

  df_pred = checkTruthProb(truth_name, prob_name)
  checkmate::assertNumeric(epsilon, len = 1L, lower = 0, upper = 1)
  checkmate::assertNumeric(delta, len = 1L, lower = 0, upper = 1)

  checkmate::assertCharacter(seed_object, null.ok = TRUE, len = 1L)
  checkmate::assertLogical(sort, len = 1L)

  if (epsilon == 0) stop("Epsilon must be > 0")
  if (delta == 0) stop("Delta must be > 0")

  if (! "l2s" %in% c(ls(envir = .GlobalEnv), ls()))
    stop("Cannot find l2 sensitivity. Please push an l2 sensitivity with name 'l2s' to the servers.")

  l2s = eval(parse(text = "l2s"))
  checkmate::assertNumeric(l2s, len = 1L, lower = 0)
  if (l2s == 0) stop("L2 sensitivity must be > 0")


  truth = df_pred$truth
  prob  = df_pred$prob

  if (sort) {
    nv  = sort(prob[truth == 0])
  } else {
    nv  = prob[truth == 0]
  }
  sde = GMVar(l2s, epsilon, delta)

  if (! is.null(seed_object)) {
    seed_old = .Random.seed
    seed = seedBoundedToObject(seed_object)
    set.seed(seed)
  }

  out = stats::rnorm(n = length(nv), mean = nv, sd = sde)

  if (! is.null(seed_object)) set.seed(seed_old)

  return(out)
}

#'
#' @title Calculate standard deviation for Gaussian Mechanism
#' @param l2s (`numeric(1L)`) l2-sensitivity.
#' @param epsilon (`numeric(1L)`) First privacy parameter for (e,d)-differential privacy.
#' @param delta (`numeric(1L)`) second privacy parameter for (e,d)-differential privacy.
#' @return Numerical value for the standard deviation for the normal distribution.
#' @author Daniel S.
GMVar = function(l2s, epsilon, delta) {
  checmate::assertNumeric(l2s, len = 1L)
  checmate::assertNumeric(epsilon, len = 1L)
  checmate::assertNumeric(delta, len = 1L)

  return(sqrt(2 * log(1.25 / delta)) * l2s / epsilon)
}
