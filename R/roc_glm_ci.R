#'
#' @title Calculate CI for AUC
#' @description This function calculates a CI for the AUC based on the approach proposed by DeLong.
#' @param connections [DSI::connection] Connection to an OPAL server.
#' @param roc_glm [list()] List containing the ROC-GLM parameter returned from `dsROCGLM`.
#' @param alpha [numeric(1L)] Significance level alpha (default is `0.05`).
#' @return Numeric vector with two values containing the boundaries of the confidence interval.
#' @author Daniel S.
#' @export
aucCI = function (connections, roc_glm, alpha = 0.05)
{
  n_scores = DSI::datashield.aggregate(connections, "getNegativeScores(\"D$gender\", \"pred\")")
  p_scores = DSI::datashield.aggregate(connections, "getPositiveScores(\"D$gender\", \"pred\")")

  auc = calculateAUC(roc_glm)

  pred = predict(mod, type = "response")
  data.frame(sort(n_scores[[1]]), sort(pred[dat$gender == 0]))

  pooled_n_scores = Reduce("c", n_scores)
  pooled_p_scores = Reduce("c", p_scores)

  s_d = function (x) 1 - ecdf(pooled_p_scores)(x)
  s_nond = function (x) 1 - ecdf(pooled_n_scores)(x)

  # Variance of empirical auc after DeLong:
  var_auc = var(s_d(pooled_n_scores)) / length(pooled_n_scores) + var(s_nond(pooled_p_scores)) / sum(pooled_p_scores)

  logit_pm = log(auc / (1 - auc)) + c(-1, 1) * qnorm(1 - alpha / 2) * sqrt(var_auc) / (auc * (1 - auc))

  return (1 / (1 + exp(-logit_pm)))
}


