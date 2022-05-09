#'
#' @title Remove missing values from data
#' @description Remove missing values from data and assign it to new data or old data.
#' @param symbol (`character(1L)`) R object from which missings should be removed.
#' @return Object defined in symbol wrapped by `na.omit`.
#' @author Daniel S.
#' @export
removeMissings = function(symbol) {
  if (missing(symbol)) stop("Symbol must be given.")

  obj = eval(parse(text = symbol))
  return(stats::na.omit(obj))
}
