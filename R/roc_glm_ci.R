#'
#' @title Calculate CI for AUC
#' @description This function calculates a CI for the AUC based on the approach proposed by DeLong.
#' @param connections (`DSI::connection`) Connection to an OPAL server.
#' @param truth_name (`character(1L)`) Character containing the name of the vector of 0-1-values
#'   encoded as integer or numeric.
#' @param pred_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @param roc_glm (`list()`) List containing the ROC-GLM parameter returned from `dsROCGLM`.
#' @param alpha (`numeric(1L)`) Significance level alpha (default is `0.05`).
#' @param epsilon (`numeric(1L)`) Privacy parameter for differential privacy (DP).
#' @param delta (`numeric(1L)`) Probability of violating epsilon DP.
#' @param seed_object (`character(1L)`) Name of an object which is used
#'   to add a seed based on an object.
#' @return Numeric vector with two values containing the boundaries of the confidence interval.
#' @author Daniel S.
#' @export
aucCI = function(connections, truth_name, pred_name, roc_glm, alpha = 0.05, epsilon = 0.2, delta = 0.2,
  seed_object = NULL) {

  mns = ds.mean(truth_name, datasources = connections)

  n_neg = sum(mns$Mean.by.Study[,"Ntotal"] * (1 - mns$Mean.by.Study[, "EstimatedMean"]))
  n_pos = sum(mns$Mean.by.Study[,"Ntotal"] * mns$Mean.by.Study[, "EstimatedMean"])

  checkmate::assertCharacter(seed_object, null.ok = TRUE, len = 1L)

  if (is.null(seed_object)) seed_object = "NULL"

  ## Calculate mean of negative and positive scores:
  m_neg = DSI::datashield.aggregate(connections, paste0("getNegativeScoresVar(\"", truth_name,
    "\", \"", pred_name, "\", return_sum = TRUE)"))
  m_neg = sum(unlist(n_neg)) / n_neg

  m_pos = DSI::datashield.aggregate(connections, paste0("getPositiveScoresVar(\"", truth_name,
    "\", \"", pred_name, "\", return_sum = TRUE)"))
  m_pos = sum(unlist(n_pos)) / n_pos

  ## Get sd of differences:
  ssd_neg = DSI::datashield.aggregate(connections, paste0("getNegativeScoresVar(\"", truth_name,
    "\", \"", pred_name, "\", m = ", m_neg, ")"))
  sdd_neg = 1 / (n_neg - 1) * sum(unlist(ssd_neg))

  ssd_pos = DSI::datashield.aggregate(connections, paste0("getPositiveScoresVar(\"", truth_name,
    "\", \"", pred_name, "\", m = ", m_pos, ")"))
  sdd_pos = 1 / (n_pos - 1) * sum(unlist(ssd_pos))


  n_scores = DSI::datashield.aggregate(connections, paste0("getNegativeScores(\"", truth_name, "\", \"",
    pred_name, "\", ", epsilon, ", ", delta, ", \"", seed_object, "\", TRUE)"))
  p_scores = DSI::datashield.aggregate(connections, paste0("getPositiveScores(\"", truth_name, "\", \"",
    pred_name, "\", ", epsilon, ", ", delta, ", \"", seed_object, "\", TRUE)"))

  auc = calculateAUC(roc_glm)

  pooled_n_scores = Reduce("c", n_scores)
  pooled_p_scores = Reduce("c", p_scores)

  s_d = function(x) 1 - stats::ecdf(pooled_p_scores)(x)
  s_nond = function(x) 1 - stats::ecdf(pooled_n_scores)(x)

  # Variance of empirical auc after DeLong:
  var_auc = stats::var(s_d(pooled_n_scores)) / length(pooled_n_scores) +
    stats::var(s_nond(pooled_p_scores)) / length(pooled_p_scores)

  logit_pm = log(auc / (1 - auc)) + c(-1, 1) * stats::qnorm(1 - alpha / 2) * sqrt(var_auc) / (auc * (1 - auc))

  return(1 / (1 + exp(-logit_pm)))
}
