#'
#' @title Calculate Probit Regression on Server
#' @description Probit
#' @param connections [DSI::connection] Connection to an OPAL server.
#' @param formula [character(1L)] Formula used for the probit regression.
#' @param data [character(1L)] Data as string
#' @param w [character(1L)] Weights for the probit regression. Must be a column in `data`.
#' @param stop_tol [numeric(1L)] Tolerance for the stop criteria (`abs(dev - dev_old) / (abs(dev) + 0.1)`) for
#'   which the Fischer scoring stops (default is `1e-8`).
#' @param iter_max [integer(1L)] Maximal number of Fischer scoring iterations (default is `25L`).
#' @param trace [logical(1L)] If `TRUE` (default), information about the progress is shown.
#' @return List with estimated parameter, number of iterations, and the deviance when the algorithm is stopped.
#' @author Daniel S.
#' @export
dsProbitRegr = function (connections, formula, data, w = NULL, stop_tol = 1e-8, iter_max = 25L, trace = FALSE)
{
  checkmate::assertCharacter(formula, len = 1L)
  checkmate::assertCharacter(data, len = 1L)
  checkmate::assertCharacter(w, len = 1L)
  checkmate::assertNumeric(stop_tol, len = 1L)
  checkmate::assertCount(iter_max, positive = TRUE)
  checkmate::assertLogical(trace, len = 1L)

  iter = 0L
  dev_old = Inf

  if (trace) cat("\n")

  while (iter <= iter_max) {

    ## Use a "custom encoding for parameter":
    ## - xnx is the separator for the next parameter
    ## - xex separator for names and values
    if (iter == 0L) {
      params_char = "xxx"
    } else {
      params_char = paste(paste0(seq_along(params), "xex", params), collapse = "xnx")
    }

    ## Calculate distributed parts. Note:
    ## - Data has to be present at each server
    ## - Weight vector w also needs to be an extra object on the server
    if (is.null(w)) {
      call = paste0("calculateDistrGLMParts(formula = ", formula, ", data = '", data, "', params_char = '", params_char, "')")
    } else {
      call = paste0("calculateDistrGLMParts(formula = ", formula, ", data = '", data, "', w = '", w, "', params_char = '", params_char, "')")
    }
    eval(parse(text = paste0("cq = quote(", call, ")")))

    update = DSI::datashield.aggregate(conns = connections, cq)

    lh = Reduce("+", lapply(update, function (x) x$likelihood))
    dev = -2 * log(lh)
    XtX = Reduce("+", lapply(update, function (x) x$XtX))
    Xty = Reduce("+", lapply(update, function (x) x$Xy))

    if (iter == 0L) params = rep(0, length(update[[1]]$Xy))
    params = params + solve(XtX) %*% Xty

    iter = iter + 1L

    if (trace) cat("Deviance of iter", iter, "=", round(dev, digits = 4L), "\n")

    stop_crit = abs(dev - dev_old) / (abs(dev) + 0.1)
    if (stop_crit < stop_tol) { if (trace) { cat("\n")}; break; }
    dev_old = dev
  }
  out = list(iter = iter, parameter = params, deviance = dev)
  return (out)
}

#'
#' @title Calculate Probit Regression on Server
#' @description ROC-GLM function for DataSHIELD. The function prepares the data on the server and fits the model.
#'   The model returned by the function contains ROC-GLM as parametrized model.
#' @param connections [DSI::connection] Connection to an OPAL server.
#' @param truth_name [character(1L)] Character containing the name of the vector of 0-1-values encoded as integer or numeric.
#' @param prob_name [character(1L)] Character containing the name of the vector of probabilities.
#' @param trace [logical(1L)] If `TRUE` (default), information about the progress is shown.
#' @param clean_server [logical(1L)] Set to `TRUE` (default) if all temprary data stored on the server should
#'   be removed when the fitting is finished.
#' @param alpha [numeric(1L)] Significance level alpha for confidence interval (default is `0.05`).
#' @return List with estimated parameter, number of iterations, and the deviance when the algorithm is stopped.
#' @author Daniel S.
#' @export
dsROCGLM = function (connections, truth_name, pred_name, trace = TRUE, clean_server = TRUE, alpha = 0.05)
{
  checkmate::assertLogical(trace, len = 1L, any.missing = FALSE, null.ok = FALSE)
  checkmate::assertLogical(clean_server, len = 1L, any.missing = FALSE, null.ok = FALSE)

  if (trace) cat ("\nInitializing ROC-GLM\n\n> Host: Received scores of negative response\n\n\n")

  ## Checks are included in "getNegativeScores":
  n_scores = DSI::datashield.aggregate(conns = connections, paste0("getNegativeScores(\"", truth_name, "\", \"", pred_name, "\")"))
  pooled_scores = Reduce("c", n_scores)

  if (trace) cat("> Host: Pushing pooled scores\n")

  ds.predict.base::pushModel(connections, pooled_scores)

  if (trace) cat("> Server: Calculating placement values and parts for ROC-GLM\n")

  cq = NULL # Dummy for checks
  eval(parse(text = paste0("cq = quote(", paste0("rocGLMFrame(\"", truth_name, "\",\"", pred_name, "\", \"pooled_scores\")"), ")")))
  DSI::datashield.assign(connections, "roc_data", cq)

  if (trace) cat("> Server: Calculating probit regression to obtain ROC-GLM\n")
  roc_glm = dsProbitRegr(connections, "y ~ x", "roc_data", w = "w", trace = TRUE)

  if (trace) cat("> Host: Finished calculating ROC-GLM\n")

  ## Clean server objects:
  if (clean_server) {
    if (trace) cat("> Host: Cleaning data on server\n")
    DSI::datashield.rm(connections, "pooled_scores")
    DSI::datashield.rm(connections, "roc_data")
  }

  if (trace) cat("> Host: Calculating AUC and CI\n")

  roc_glm$auc = calculateAUC(roc_glm)
  roc_glm$ci = aucCI(connections, roc_glm)
  roc_glm$alpha = alpha

  class(roc_glm) = "ROC.GLM"

  if (trace) cat("Finished!\n\n")

  return (roc_glm)
}

