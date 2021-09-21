#'
#' @title Calculate CI for AUC
#' @description This function calculates a CI for the AUC based on the approach proposed by DeLong.
#' @param connections (`DSI::connection`) Connection to an OPAL server.
#' @param truth_name (`character(1L)`) Character containing the name of the vector of 0-1-values
#'   encoded as integer or numeric.
#' @param pred_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @param roc_glm (`list()`) List containing the ROC-GLM parameter returned from `dsROCGLM`.
#' @param alpha (`numeric(1L)`) Significance level alpha (default is `0.05`).
#' @param lag (`integer(1L)`) Lag to the next neighbours considered for calculating the standard deviation of the noise.
#' @param ntimes (`integer(1L)`) Times the standard deviation used for simulating noise added to the data.
#' @return Numeric vector with two values containing the boundaries of the confidence interval.
#' @author Daniel S.
#' @export
aucCI = function(connections, truth_name, pred_name, roc_glm, alpha = 0.05, lag = 4L, ntimes = 2L) {

  mns = ds.mean(truth_name)

  ## Get sd of differences:
  ssd_neg = DSI::datashield.aggregate(connections, paste0("getNegativeScoresVar(\"", truth_name,
    "\", \"", pred_name, "\", ", lag, ")"))
  n_neg   = sum(mns$Mean.by.Study[,"Ntotal"] * (1 - mns$Mean.by.Study[, "EstimatedMean"]))
  sdd_neg = 1 / (n_neg - 1) * sum(unlist(ssd))

  ssd_pos = DSI::datashield.aggregate(connections, paste0("getPositiveScoresVar(\"", truth_name,
    "\", \"", pred_name, "\", ", lag, ")"))
  n_pos   = sum(mns$Mean.by.Study[,"Ntotal"] * mns$Mean.by.Study[, "EstimatedMean"])
  sdd_pos = 1 / (n_pos - 1) * sum(unlist(ssd))


  n_scores = DSI::datashield.aggregate(connections, paste0("getNegativeScores(\"", truth_name, "\", \"",
    pred_name, "\", ", sdd_neg, ", ", ntimes, ")"))
  p_scores = DSI::datashield.aggregate(connections, paste0("getPositiveScores(\"", truth_name, "\", \"",
    pred_name, "\", ", sdd_pos, ", ", ntimes, ")"))

  auc = calculateAUC(roc_glm)

  ### ???? Where is mod comming from? GlobalEnv?
  ### ACTION: Remove -> check
  #pred = stats::predict(mod, type = "response")
  #data.frame(sort(n_scores[[1]]), sort(pred[dat$gender == 0]))

  pooled_n_scores = Reduce("c", n_scores)
  pooled_p_scores = Reduce("c", p_scores)

  s_d = function(x) 1 - stats::ecdf(pooled_p_scores)(x)
  s_nond = function(x) 1 - stats::ecdf(pooled_n_scores)(x)

  # Variance of empirical auc after DeLong:
  var_auc = stats::var(s_d(pooled_n_scores)) / length(pooled_n_scores) +
    stats::var(s_nond(pooled_p_scores)) / sum(pooled_p_scores)

  logit_pm = log(auc / (1 - auc)) + c(-1, 1) * stats::qnorm(1 - alpha / 2) * sqrt(var_auc) / (auc * (1 - auc))

  return(1 / (1 + exp(-logit_pm)))
}
