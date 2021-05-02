#'
#' @title Truth and Prediction Checker
#' @description This function checks if the vector of true values and predictions
#'   has the correct format to be used for the ROC-GLM. If something does not suit,
#'   then the function tries to convert it into the correct format.
#' @param truth_name [character(1L)] Character containing the name of the vector of 0-1-values encoded as integer or numeric.
#' @param prob_name [character(1L)] Character containing the name of the vector of probabilities.
#' @param pos [character(1L)] Character containing the name of the positive class.
#' @return Data frame containing the truth and prediction vector.
#' @author Daniel S.
checkTruthProb = function (truth_name, prob_name, pos = NULL)
{
  #if (! truth_name %in% ls()) stop(quote(truth_name), " is no element of the environment!")
  #if (! prob_name %in% ls()) stop(quote(prob_name), " is no element of the environment!")

  truth = eval(parse(text = truth_name))
  prob  = eval(parse(text = prob_name))

  if (length(unique(truth)) > 2) stop("\"", truth_name, "\" contains ", length(unique(truth)), " > 2 elements! Two are required!")

  ntruth = length(truth)
  checkmate::assertNumeric(prob, any.missing = FALSE, len = ntruth, null.ok = FALSE, finite = TRUE)

  if (is.null(pos)) {
    if (is.character(truth) | is.factor(truth)) warning("\"", truth_name, "\" is not enocded as 0-1 integer, conversion is done automaticall. This may lead to a label flip! Set argument \"pos\" to ensure correct encoding.")

    if (is.character(truth)) truth = as.integer(as.factor(truth))
    if (is.factor(truth))    truth = as.integer(truth)

    if (max(truth) == 2) truth = truth - 1
  } else {
    if (is.character(truth) | is.factor(truth)) truth = ifelse(truth == pos, 1, 0)
    if (is.numeric(truth)) warning(quote("pos"), " is set but \"", truth_name, "\" is numeric. Are you sure you know what the response really is?")
  }
  return (invisible(data.frame(truth = truth, prob = prob)))
}

#'
#' @title Return positive scores
#' @description This function just returns positive scores and is used
#'   as aggregator to send these positive scores.
#' @param truth_name [character(1L)] Character containing the name of the vector of 0-1-values encoded as integer or numeric.
#' @param prob_name [character(1L)] Character containing the name of the vector of probabilities.
#' @return Positive scores
#' @author Daniel S.
#' @export
getPositiveScores = function (truth_name, prob_name)
{
  df_pred = checkTruthProb(truth_name, prob_name)

  truth = df_pred$truth
  prob  = df_pred$prob

  return (prob[truth == 1])
}

#'
#' @title Return negative scores
#' @description This function just returns negative scores and is used
#'   as aggregator to send these positive scores.
#' @param truth_name [character(1L)] Character containing the name of the vector of 0-1-values encoded as integer or numeric.
#' @param prob_name [character(1L)] Character containing the name of the vector of probabilities.
#' @return Negative scores
#' @author Daniel S.
#' @export
getNegativeScores = function (truth_name, prob_name)
{
  df_pred = checkTruthProb(truth_name, prob_name)

  truth = df_pred$truth
  prob  = df_pred$prob

  return (prob[truth == 0])
}


#'
#' @title Compute Placement Values on Server
#' @description This function calculates the placement values required to calculate the ROC-GLM.
#' @param pooled_scores_name [character(1L)] Name of the object holding the pooled negative scores.
#' @param truth_name [character(1L)] Character containing the name of the vector of 0-1-values encoded as integer or numeric.
#' @param prob_name [character(1L)] Character containing the name of the vector of probabilities.
#' @param tset Set of thresholds
#' @return Placement values
#' @author Stefan B., Daniel S.
computePlacementValues = function (pooled_scores_name, truth_name, prob_name, tset = NULL)
{
  df_pred = checkTruthProb(truth_name, prob_name)

  checkmate::assertCharacter(pooled_scores_name, len = 1L)

  truth = df_pred$truth
  prob  = df_pred$prob

  p_scores = eval(parse(text = pooled_scores_name))

  F_emp = ecdf(p_scores)
  S_emp = function (x) 1 - F_emp(x)

  if (is.null(tset)) {
    tset = prob[truth == 1]
  }
  tset = sort(tset)

  return (S_emp(tset))
}

#'
#' @title Calculate U Matrix for ROC-GLM
#' @description This function calculates U matrix which is used as target variable for the ROC-GLM.
#' @param tset [numeric()] Set of thresholds
#' @param pv [numeric()] Placement values
#' @return Matrix of zeros and ones that are used as target variable for Probit regression
#' @author Stefan B., Daniel S.
calcU = function (tset, pv)
{

  ## CHECKS CHECKS

  tset_sorted = sort(tset)
  out = vapply(X = tset, FUN.VALUE = integer(length(pv)), FUN = function (th) {
    ifelse(pv <= th, 1L, 0L)
  })
  return (out)
}

#'
#' @title Get Data for ROC-GLM
#' @description This function calculates the data used for the ROC-GLM
#' @param U [matrix()] Response matrix for ROC-GLM
#' @param tset [numeric()] Set of thresholds
#' @return Data used for the ROC-GLM
#' @author Stefan B., Daniel S.
rocGLMData = function (U, tset)
{

  ## CHECKS CHECKS

  roc_glm_data = data.frame(
    y = rep(c(0, 1), times = length(tset)),
    x = rep(qnorm(tset), each = 2L),
    w = as.vector(apply(U, 2, function (x) c(sum(x == 0), sum(x == 1))))
  )
  return (roc_glm_data)
}

#'
#' @title Calculate data for ROC-GLM
#' @description This function stores the data on the server used for the ROC-GLM
#' @param truth_name [character(1L)] Character containing the name of the vector of 0-1-values encoded as integer or numeric.
#' @param prob_name [character(1L)] Character containing the name of the vector of probabilities.
#' @param pooled_scores_name [character(1L)] Character containing the name of the object holding the pooled negative scores.
#' @return Data.frame used to calculate ROC-GLM
#' @author Stefan B., Daniel S.
#' @export
rocGLMFrame = function (truth_name, prob_name, pooled_scores_name)
{
  checkmate::assertCharacter(pooled_scores_name, len = 1L)
  df_pred = checkTruthProb(truth_name, prob_name)

  truth = df_pred$truth
  prob  = df_pred$prob

  if (length(truth) < 5) stop("More than 5 ovservations are required to ensure privacy!")

  tset = prob[truth == 1]
  pv = computePlacementValues(pooled_scores_name, prob, truth, tset)
  U  = calcU(tset, pv)
  roc_glm_data = rocGLMData(U, tset)

  return (roc_glm_data)
}


#'
#' @title Calculate Parts for Fisher Scoring
#' @description This function calculates the parts required to conduct an update for the Fisher scoring for probit regression
#' @param formula [character(1L)] Formula used for the probit regression as character.
#' @param data [character(1L)] Data as character string. Note that all servers must have data of that name.
#' @param w [character(1L)] Weights as character. The weight vector must be a column in `data`.
#' @param params_char [character(1L)] Parameter vector encoded as one string.
#' @return List with XtX, Xy, and the likelihood of the respective server.
#' @author Daniel S.
#' @export
calculateDistrGLMParts = function (formula, data,  w = NULL, params_char)
{

  ## CHECKS CHECKS

  fm = format(formula)
  target = strsplit(fm, " ~ ")[[1]][1]

  eval(parse(text = paste0("X = model.matrix(", fm, ", data = ", data, ")")))
  eval(parse(text = paste0("y = as.integer(", data, "[['", target, "']])")))
  if (max(y) == 2) y = y - 1

  # Flip labels:
  #y = abs(y - 1)

  if (!is.null(w)) {
    eval(parse(text = paste0("w = ", data, "[['", w, "']]")))
  } else {
    w = rep(1, length(y))
  }
  if (params_char == "xxx") {
    beta = rep(0, ncol(X))
  } else {
    beta = unlist(lapply(strsplit(params_char, "xnx")[[1]], FUN = function (p) {
      sp = strsplit(p, "xex")
      params = vapply(sp, FUN.VALUE = numeric(1L), function (s) as.numeric(s[2]))
      names(params) = vapply(sp, FUN.VALUE = character(1L), function (s) s[1])

      return (params)
    }))
  }

  w_mat = diag(sqrt(w))
  lambda = calculateLambda(y, X, beta)
  W = diag(as.vector(lambda * (X %*% beta + lambda)))

  if (! is.null(w)) {
    X = w_mat %*% X
    lambda = w_mat %*% lambda
  }
  XtX = t(X) %*% W %*% X
  Xy = t(X) %*% lambda

  out = list(XtX = XtX, Xy = Xy, likelihood = probitLikelihood(y, X, beta))
  return (out)
}

#'
#' @title Transform Response for Probit Fisher-Scoring
#' @description This function transform the original 0-1-response y  to a new reponse
#'   "lambda" that is used as respone for the fisher scoring.
#' @param y Original 0-1-response
#' @param X Design matrix
#' @param beta Estimated parameter vector
#' @return Numeric vector of new response
#' @author Stefan B., Daniel S.
calculateLambda = function (y, X, beta)
{

  ## CHECKS CHECKS

  eta = X %*% beta
  q = 2 * y - 1
  qeta = q * eta

  return ((dnorm(qeta) * q) / (pnorm(qeta)))
}

#'
#' @title Calculate Likelihood of Probit Model
#' @description This function calculates the likelihood used in a probit regression (Bernoulli distribution + probit link).
#' @param y Original 0-1-response
#' @param X Design matrix
#' @param beta Estimated parameter vector
#' @param x Weight vector
#' @return Numeric value containing the likelihood
#' @author Stefan B., Daniel S.
probitLikelihood = function (y, X, beta, w = NULL)
{

  ## CHECKS CHECKS

  eta = X %*% beta

  if (is.null(w)) {
    w = rep(1, times = nrow(X))
  }
  lh = pnorm(eta)^y * (1 - pnorm(eta))^(1 - y)
  prod(lh^w)
}

#'
#' @title Calculate AUC from ROC-GLM
#' @description This function calculates the AUC from the ROC-GLM by integrating the binormal form `pnorm(a + b*qnorm(x))`
#'   for x in 0 to 1. The parameter a and b are obtained by `dsROCGLM`.
#' @param roc_glm [lit()] List containing the ROC-GLM parameter returned from `dsROCGLM`.
#' @return Numeric value of the approximated AUC.
#' @author Daniel S.
#' @export
calculateAUC = function (roc_glm)
{
  params = roc_glm$parameter
  temp = function (x) pnorm(params[1] + params[2] * qnorm(x))
  int = integrate(f = temp, lower = 0, upper = 1)
  return (int$value)
}

