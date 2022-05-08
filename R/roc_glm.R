#'
#' @title Calculate Probit Regression on Server
#'
#' @description Distributed computing of the probit regression using DataSHIELD. The method gets the
#'   parts required for the fisher scoring in each iteration and aggreagtes it to a global update.
#'   Hence, the updating step is done at the analyst's machine while calculation of the required parts
#'   are done at the servers.
#' @param connections (`DSI::connection`) Connection to an OPAL server.
#' @param formula (`character(1L)`) Formula used for the probit regression.
#' @param data (`character(1L)`) Data as string.
#' @param w (`character(1L)`) Weights for the probit regression. Must be a column in `data`.
#' @param stop_tol (`numeric(1L)`) Tolerance for the stop criteria (`abs(dev - dev_old) / (abs(dev) + 0.1)`) for
#'   which the Fischer scoring stops (default is `1e-8`).
#' @param iter_max (`integer(1L)`) Maximal number of Fischer scoring iterations (default is `25L`).
#' @param trace (`logical(1L)`) If `TRUE` (default), information about the progress is shown.
#' @return List with estimated parameter, number of iterations, and the deviance at the stopping iteration.
#' @author Daniel S.
#' @export
dsProbitRegr = function(connections, formula, data, w = NULL, stop_tol = 1e-8, iter_max = 25L, trace = FALSE) {
  checkmate::assertCharacter(formula, len = 1L)
  checkmate::assertCharacter(data, len = 1L)
  checkmate::assertCharacter(w, len = 1L)
  checkmate::assertNumeric(stop_tol, len = 1L)
  checkmate::assertCount(iter_max, positive = TRUE)
  checkmate::assertLogical(trace, len = 1L)

  iter = 0L
  dev_old = Inf

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
      call = paste0("calculateDistrGLMParts(formula = ", formula, ", data = '", data,
        "', params_char = '", params_char, "')")
    } else {
      call = paste0("calculateDistrGLMParts(formula = ", formula, ", data = '", data, "', w = '",
        w, "', params_char = '", params_char, "')")
    }
    cq = NULL
    eval(parse(text = paste0("cq = quote(", call, ")")))

    update = DSI::datashield.aggregate(conns = connections, cq)

    lh = Reduce("+", lapply(update, function(x) ifelse(is.nan(x$likelihood), 0, x$likelihood)))
    dev = -2 * log(lh)
    xtx = Reduce("+", lapply(update, function(x) ifelse(is.nan(x$XtX), 0, x$XtX)))
    xty = Reduce("+", lapply(update, function(x) ifelse(is.nan(x$Xy), 0, x$Xy)))

    if (iter == 0L) params = rep(0, length(update[[1]]$Xy))
    params = params + solve(xtx) %*% xty

    iter = iter + 1L

    if (trace) message("[", Sys.time(), "] Deviance of iter", iter, "=", round(dev, digits = 4L))

    stop_crit = abs(dev - dev_old) / (abs(dev) + 0.1)
    if (stop_crit < stop_tol) {
      break;
    }
    dev_old = dev
  }
  out = list(iter = iter, parameter = params, deviance = dev)
  return(out)
}

#'
#' @title Calculate Probit Regression on Server
#' @description ROC-GLM function for DataSHIELD. The function prepares the data on the server and fits the model.
#'   The model returned by the function contains ROC-GLM as parametrized model.
#' @param connections (`DSI::connection`) Connection to an OPAL server.
#' @param truth_name (`character(1L)`) Character containing the name of the vector of 0-1-values
#'   encoded as integer or numeric.
#' @param pred_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @param trace (`logical(1L)`) If `TRUE` (default), information about the progress is shown.
#' @param clean_server (`logical(1L)`) Set to `TRUE` (default) if all temprary data stored on the server should
#'   be removed when the fitting is finished.
#' @param alpha (`numeric(1L)`) Significance level alpha for confidence interval (default is `0.05`).
#' @param dat_name (`character(1L)`) Name of the data set on the servers..
#' @param seed_object (`character(1L)`) Name of an object which is used
#'   to add a seed based on an object.
#' @param ... Additional arguments passed to `dsL2Sens` (connections and pred_name is already set).
#' @return List with estimated parameter, number of iterations, and the deviance when the algorithm is stopped.
#' @author Daniel S.
#' @export
dsROCGLM = function(connections, truth_name, pred_name, trace = TRUE, clean_server = TRUE,
  alpha = 0.05, dat_name = "D", seed_object = NULL, ...) {

  checkmate::assertCharacter(dat_name, len = 1L)
  l2s = dsL2Sens(connections = connections, dat_name = dat_name, pred_name = pred_name, ...)
  if (trace)
    message("\n[", Sys.time(), "] L2 sensitivity is: ", round(l2s, 4), "\n")
  pushObject(connections, l2s)

  # Select privacy parameters based on the l2 sensitivity:
  l2breaks = c(0.01, 0.03, 0.05, 0.07, Inf)
  priv_pars_choice = list(c(0.2, 0.1), c(0.3, 0.4), c(0.5, 0.3), c(0.5, 0.5), c(0.5, 0.5))
  pp_select = which(l2s <= l2breaks)[1]

  if (pp_select == 5)
    warning("l2-sensitivity may be too high for good results! ",
      "Epsilon = 0.5 and delta = 0.5 is used which may lead to bad results.")

  epsilon = priv_pars_choice[[pp_select]][1]
  delta = priv_pars_choice[[pp_select]][2]

  if (trace)
    message("\n[", Sys.time(), "] Setting: epsilon = ", epsilon, " and delta = ", delta, "\n")


  checkmate::assertLogical(trace, len = 1L, any.missing = FALSE, null.ok = FALSE)
  checkmate::assertLogical(clean_server, len = 1L, any.missing = FALSE, null.ok = FALSE)
  checkmate::assertCharacter(seed_object, null.ok = TRUE, len = 1L)

  if (is.null(seed_object)) seed_object = "NULL"

  if (trace)
    message("\n[", Sys.time(), "] Initializing ROC-GLM\n\n[", Sys.time(),
      "] Host: Received scores of negative response\n")

  if (trace) message("[", Sys.time(), "] Receiving negative scores")

  ## Checks are included in "getNegativeScores":
  n_scores = DSI::datashield.aggregate(conns = connections, paste0("getNegativeScores(\"", truth_name,
    "\", \"", pred_name, "\", ", epsilon, ", ", delta, ", \"", seed_object, "\", TRUE)"))
  pooled_scores = Reduce("c", n_scores)

  if (trace) message("[", Sys.time(), "] Host: Pushing pooled scores")

  pushObject(connections, pooled_scores)

  if (trace) message("[", Sys.time(), "] Server: Calculating placement values and parts for ROC-GLM")

  cq = NULL # Dummy for checks
  eval(parse(text = paste0("cq = quote(", paste0("rocGLMFrame(\"", truth_name, "\",\"",
    pred_name, "\", \"pooled_scores\")"), ")")))
  DSI::datashield.assign(connections, "roc_data", cq)

  if (trace) message("[", Sys.time(), "] Server: Calculating probit regression to obtain ROC-GLM")
  roc_glm = dsProbitRegr(connections, "y ~ x", "roc_data", w = "w", trace = TRUE)

  if (trace) message("[", Sys.time(), "] Host: Finished calculating ROC-GLM")

  ## Clean server objects:
  if (clean_server) {
    if (trace) message("[", Sys.time(), "] Host: Cleaning data on server")
    DSI::datashield.rm(connections, "pooled_scores")
    DSI::datashield.rm(connections, "roc_data")
  }

  if (trace) message("[", Sys.time(), "] Host: Calculating AUC and CI")

  roc_glm$auc = calculateAUC(roc_glm)
  roc_glm$ci = aucCI(connections, truth_name, pred_name, roc_glm, alpha = alpha,
    epsilon = epsilon, delta = delta, seed_object = seed_object)
  roc_glm$alpha = alpha
  roc_glm$privacy_pars = c(epsilon = epsilon, delta = delta, l2s = l2s)

  class(roc_glm) = "ROC.GLM"

  if (trace) message("[", Sys.time(), "] Finished!")

  return(roc_glm)
}
