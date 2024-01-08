# Get `datashield.privacyLevel` from DESCRIPTION file. Note that we do not set the option
# as DataSHIELD does because of the risk of highjacking the R environment. Instead, when
# a function is called that uses the privacy level, the function gets it directly from the
# DESCRIPTION file.
.getPrivacyLevel = function() {
  pl = utils::packageDescription("dsROCGLM")$Options
  pl = as.integer(gsub("\\D", "", pl))
  if (is.na(pl)) stop("No privacy level specified in DESCRIPTION.")
  return(pl)
}


#'
#' @title Generate all combinations of DP parameters fromt he paper
#'
#' @description This function creates a data frame with the cosidered values from the paper. One may want to choose or set a better grid or method in the future.
#' @return data.frame with combinations and info whether parameters are valid accorsing to the paper.
#' @author Raphael R. 
#' @export
generateParameterTableDP = function(){
  sens = seq(0.01, 0.09, 0.02)
  eps = seq(0.1, 1, 0.2)
  del = c(0.00001, 0.0001, 0.001, 0.01, 0.1)
  possible_priv_vals = expand.grid(
    sens = sens,
    eps = eps,
    del = del
  )
  # The choice of a valid setting is from the paper
  possible_priv_vals$valid = FALSE
  possible_priv_vals[possible_priv_vals$sens == sens[1] & possible_priv_vals$eps > eps[1], ]$valid <-TRUE
  possible_priv_vals[possible_priv_vals$sens == sens[2] & possible_priv_vals$eps == eps[5], ]$valid <-TRUE
  possible_priv_vals[possible_priv_vals$sens == sens[2] & possible_priv_vals$eps == eps[4] & possible_priv_vals$del > del[2], ]$valid <-TRUE
  possible_priv_vals[possible_priv_vals$sens == sens[2] & possible_priv_vals$eps == eps[3] & possible_priv_vals$del == del[5], ]$valid <-TRUE
  possible_priv_vals[possible_priv_vals$sens == sens[3] & possible_priv_vals$eps == eps[5] & possible_priv_vals$del > del[3], ]$valid <-TRUE
  possible_priv_vals[possible_priv_vals$sens == sens[3] & possible_priv_vals$eps == eps[4] & possible_priv_vals$del == del[5], ]$valid <-TRUE
  possible_priv_vals[possible_priv_vals$sens == sens[4] & possible_priv_vals$eps == eps[5] & possible_priv_vals$del == del[5], ]$valid <-TRUE
  return(possible_priv_vals)
}
