#'
#' @title Calculate data for calibration curve
#' @description This function calculates a vector of values for the calibration curve.
#' @param truth_name (`character(1L)`) Character containing the name of the vector of
#'   0-1-values encoded as integer or numeric.
#' @param prob_name (`character(1L)`) Character containing the name of the vector of probabilities.
#' @param nbins (`integer(1L)`) Number of bins between 0 and 1 (default is 10).
#' @param remove_critical_bins (`logical(1L)`) Just remove critical bins and return just bins with `length(bin) > n_critical`.
#' @return Brier score and number of values used as weights for later aggregation.
#' @author Daniel S.
#' @export
calibrationCurve = function(truth_name, prob_name, nbins = 10L, remove_critical_bins = TRUE) {
  checkmate::assertCharacter(truth_name, len = 1L, null.ok = FALSE, any.missing = FALSE)
  checkmate::assertCharacter(prob_name, len = 1L, null.ok = FALSE, any.missing = FALSE)
  checkmate::assertCount(nbins, na.ok = FALSE, positive = TRUE, null.ok = FALSE)

  truth = eval(parse(text = truth_name))
  prob  = eval(parse(text = prob_name))

  ntruth = length(truth)
  checkmate::assertNumeric(prob, len = ntruth, null.ok = FALSE, any.missing = FALSE)

  if (is.character(truth)) truth = as.integer(as.factor(truth))
  if (is.factor(truth))    truth = as.integer(truth)
  if (nbins > ntruth / 2)
    stop("Number of bins `nbins` should be smaller then half the size of `truth`",
      round(ntruth / 2), ".")
  truth = truth - min(truth)

  if (any(truth > 1)) stop("Truth values has to be 0 and 1!")
  if ((min(prob) < 0) && (max(prob) > 1)) stop("Score (probabilities are not between 0 and 1!)")

  breaks   = seq(0, 1, length.out = nbins + 1L)
  bins     = cut(prob, breaks)
  tb       = table(bins)
  df_count = data.frame(bin = names(tb), n = as.numeric(tb), lower = breaks[-length(breaks)], upper = breaks[-1])

  ## Calculate curve only if each bin has at least nfilter_privacy or more values to ensure privacy:
  nfilter_privacy = .getPrivacyLevel()
  if (remove_critical_bins) {
    idx_critical = which((tb < nfilter_privacy) & (tb > 0))
  } else {
    if (any(tb[tb > 0] < nfilter_privacy))
      stop("More than ", nfilter_privacy, " observations per bin are required to ensure privacy! Critical number of observations in bin are ",
        paste(tb[(tb > 0) & (tb < nfilter_privacy)], collapse = ", "), ".")
  }

  df_tmp = data.frame(truth, prob)
  out    = stats::aggregate(df_tmp, by = list(bin = bins), FUN = "mean")
  out    = merge(x = df_count, y = out, by = "bin", all.x = TRUE)

  if (remove_critical_bins) {
    idx_anonymize = out$bin %in% names(idx_critical)
    out$truth[idx_anonymize] = NA
    out$prob[idx_anonymize] = NA
    #out$n[idx_anonymize] = NA
  }
  return(out)
}


#'
#' @title Calculate calibration curve on DataSHIELD servers
#' @description This function enables to calculate a calibration curve on DataSHIELD servers.
#' @param connections (`DSI::connection`) Connection to an OPAL server.
#' @param truth_name (`character(1L)`) `R` object containing the models name as character.
#' @param pred_name (`character(1L)`) Name of the object predictions should be assigned to.
#' @param nbins (`integer(1L)`) Number of bins between 0 and 1 (default is 10).
#' @param remove_critical_bins (`logical(1L)`) Just remove critical bins and return just bins with `length(bin) > n_critical`.
#' @return Calibration curve for multiple server
#' @author Daniel S.
#' @export
dsCalibrationCurve = function(connections, truth_name, pred_name, nbins = 10L, remove_critical_bins = TRUE) {
  checkmate::assertCharacter(truth_name, len = 1L, null.ok = FALSE, any.missing = FALSE)
  checkmate::assertCharacter(pred_name, len = 1L, null.ok = FALSE, any.missing = FALSE)
  checkmate::assertCount(nbins, na.ok = FALSE, positive = TRUE, null.ok = FALSE)

  sym = DSI::datashield.symbols(connections)

  snames = names(sym)
  for (s in snames) {
    if (! pred_name %in% sym[[s]])
      stop("There is no data object '", pred_name, "' on server '", s, "'.")
  }

  call = paste0("calibrationCurve(\"", truth_name, "\", \"", pred_name, "\", ", nbins, ", ", remove_critical_bins, ")")
  cq = NULL
  eval(parse(text = paste0("cq = quote(", call, ")")))
  individuals = DSI::datashield.aggregate(conns = connections, cq)
  bins = individuals[[1]]$bin

  truth   = numeric()
  prob    = numeric()
  weights = integer()
  missing_ratio = numeric()
  for (b in bins) {
    tmp_truth   = numeric()
    tmp_prob    = numeric()
    tmp_weights = integer()

    for (s in snames) {
      tmp = individuals[[s]]
      tmp_truth   = c(tmp_truth, tmp$truth[tmp$bin == b] * tmp$n[tmp$bin == b])
      tmp_prob    = c(tmp_prob, tmp$prob[tmp$bin == b] * tmp$n[tmp$bin == b])
      tmp_weights = c(tmp_weights, tmp$n[tmp$bin == b])
    }
    w = sum(tmp_weights, na.rm = TRUE)
    truth = c(truth, sum(tmp_truth, na.rm = TRUE) / w)
    prob  = c(prob, sum(tmp_prob, na.rm = TRUE) / w)
    missing_ratio = c(missing_ratio, weighted.mean(is.na(tmp_truth), w = tmp_weights))
  }

  aggregated = data.frame(bin = bins, lower = individuals[[1]]$lower,
    upper = individuals[[1]]$upper, truth = truth, prob = prob,
    missing_ratio = missing_ratio)

  return(list(individuals = individuals, aggregated = aggregated))
}

#'
#' @title Plot calibration curve
#' @description This function plots the calibration curve returned by `dsCalibrationCurve()`.
#' @param cc (`list()`) Object returned by `dsCalibrationCurve()`
#' @param individuals (`logical(1L)`) Logical value indicating whether the individual calibration
#'   curves should be plotted or not (default is `TRUE`).
#' @param ... Additional arguments passed to `geom_point()` and `geom_line()` for the calibration line and points.
#' @return ggplot of calibration curve(s)
#' @author Daniel S.
#' @export
plotCalibrationCurve = function(cc, individuals = TRUE, ...) {
  checkmate::assertList(cc, len = 2L)
  temp = lapply(names(cc), function(ccname) checkmate::assertChoice(ccname, choices = c("individuals", "aggregated")))
  checkmate::assertLogical(individuals, len = 1L)

  for (s in names(cc$individuals)) {
    cc$individuals[[s]]$server = s
  }
  tmp = do.call(rbind, cc$individuals)

  gg = ggplot2::ggplot()
  if (individuals) {
    gg = gg +
      ggplot2::geom_point(data = tmp,
        ggplot2::aes_string(x = "prob", y = "truth", color = "server"),
        alpha = 0.5) +
      ggplot2::geom_line(data = tmp,
        ggplot2::aes_string(x = "prob", y = "truth", color = "server"),
        alpha = 0.5) +
      ggplot2::labs(color = "Server")
  }
  gg = gg +
    ggplot2::geom_point(data = cc$aggregated, ggplot2::aes_string(x = "prob", y = "truth"), ...) +
    ggplot2::geom_line(data = cc$aggregated, ggplot2::aes_string(x = "prob", y = "truth"), ...)

  gg = gg +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "dark red") +
    ggplot2::xlab("Predicted") +
    ggplot2::ylab("True frequency")

  return(gg)
}
