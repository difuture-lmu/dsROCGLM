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
  sens = c(0.01, 0.1, 0.2, 0.3, 0.4) 
  eps = c(0.1, 0.5, 1, 5, 10)
  del = c(0.00001, 0.0001, 0.001, 0.01, 0.1)
  possible_priv_vals = expand.grid(
    sens = sens,
    eps = eps,
    del = del
  )
  # The choice of a valid setting is from the paper
  possible_priv_vals$valid = FALSE
  possible_priv_vals[possible_priv_vals$sens == sens[1] & possible_priv_vals$eps > eps[1], ]$valid <-TRUE#
  possible_priv_vals[possible_priv_vals$sens == sens[1] & possible_priv_vals$eps == eps[1] & possible_priv_vals$del > del[3], ]$valid <-TRUE#
  
  possible_priv_vals[possible_priv_vals$sens == sens[2] & possible_priv_vals$eps > eps[3], ]$valid <-TRUE#
  possible_priv_vals[possible_priv_vals$sens == sens[2] & possible_priv_vals$eps == eps[3] & possible_priv_vals$del == del[5], ]$valid <-TRUE#
  possible_priv_vals[possible_priv_vals$sens == sens[2] & possible_priv_vals$eps == eps[2] & possible_priv_vals$del == del[5], ]$valid <-TRUE# 
  
  possible_priv_vals[possible_priv_vals$sens == sens[3] & possible_priv_vals$eps == eps[5], ]$valid <-TRUE#
  possible_priv_vals[possible_priv_vals$sens == sens[3] & possible_priv_vals$eps == eps[4] & possible_priv_vals$del > del[1], ]$valid <-TRUE#
  
  possible_priv_vals[possible_priv_vals$sens == sens[4] & possible_priv_vals$eps == eps[5], ]$valid <-TRUE#
  possible_priv_vals[possible_priv_vals$sens == sens[4] & possible_priv_vals$eps == eps[4] & possible_priv_vals$del > del[3], ]$valid <-TRUE#
  
  possible_priv_vals[possible_priv_vals$sens == sens[5] & possible_priv_vals$eps == eps[5] & possible_priv_vals$del > del[2], ]$valid <-TRUE
  
  
  # only helper to check visually whether the definitions assignments from above are correct.
  # library(ggplot2)
  # library(data.table)
  # dt = data.table(possible_priv_vals)
  # dt %>% .[, lapply(.SD, as.factor)] %>% 
  #   .[, del := factor(del, rev(c(0.00001, 0.0001, 0.001, 0.01, 0.1)))] %>% 
  # ggplot+
  #   geom_point(aes(eps, del, color = valid), size = 5)+
  #   facet_wrap(~sens, ncol = 5)
  
  # make numbers to characters to get rid of the problem with inprecise strings (better solution?)
  possible_priv_vals[, names(possible_priv_vals)[-4]] <- lapply(possible_priv_vals[, -4], as.character)

  return(possible_priv_vals)
}



#'
#' Calculates the error function used for the analytic Gaussian mechanism
#'
#' @param x (`numeric()`) A (vector of) real number(s)
#' @return (`numeric()`) The evaluated error function
#' @author Raphael Rehms
erf = function(x){
  return(2 * pnorm(x * sqrt(2)) - 1)
}


#'
#' Calculated sigma for analytic Gaussian mechanism
#'
#' @description Caluclates the minimal valid sigma for given privacy parameters for Gaussian mechanism
#'   (see https://arxiv.org/abs/1805.06530).
#' @param epsilon (`numeric()`) Epsilon > 0
#' @param delta (`numeric()`) Delta that is between (0,1)
#' @param sens (`numeric()`) Sensitivity of the algorithm
#' @param tol (`numeric()`) Tolerance for binary search
#' @return (`numeric()`) Sigma that can be used to generate calibrated Gaussian noise
#' @author Raphael Rehms
#' @export
analyticGaussianMechanism = function(epsilon, delta, sens, tol = 1e-12) {
  phi = function(t) {
    0.5 * (1.0 + erf(t / sqrt(2.0)))
  }
  
  caseA = function(epsilon, s) {
    phi(sqrt(epsilon * s)) - exp(epsilon) * phi(-sqrt(epsilon * (s + 2.0)))
  }
  
  caseB = function(epsilon, s) {
    phi(-sqrt(epsilon * s)) - exp(epsilon) * phi(-sqrt(epsilon * (s + 2.0)))
  }
  
  doubling_trick = function(predicate_stop, s_inf, s_sup) {
    while (!predicate_stop(s_sup)) {
      s_inf = s_sup
      s_sup = 2.0 * s_inf
    }
    return(c(s_inf, s_sup))
  }
  
  binary_search = function(predicate_stop, predicate_left, s_inf, s_sup) {
    s_mid = s_inf + (s_sup - s_inf) / 2.0
    while (!predicate_stop(s_mid)) {
      if (predicate_left(s_mid)) {
        s_sup = s_mid
      } else {
        s_inf = s_mid
      }
      s_mid = s_inf + (s_sup - s_inf) / 2.0
    }
    return(s_mid)
  }
  
  delta_thr = caseA(epsilon, 0.0)
  
  if (delta == delta_thr) {
    alpha = 1.0
  } else {
    if (delta > delta_thr) {
      predicate_stop_DT = function(s) caseA(epsilon, s) >= delta
      function_s_to_delta = function(s) caseA(epsilon, s)
      predicate_left_BS = function(s) function_s_to_delta(s) > delta
      function_s_to_alpha = function(s) sqrt(1.0 + s / 2.0) - sqrt(s / 2.0)
    } else {
      predicate_stop_DT = function(s) caseB(epsilon, s) <= delta
      function_s_to_delta = function(s) caseB(epsilon, s)
      predicate_left_BS = function(s) function_s_to_delta(s) < delta
      function_s_to_alpha = function(s) sqrt(1.0 + s / 2.0) + sqrt(s / 2.0)
    }
    
    predicate_stop_BS = function(s) abs(function_s_to_delta(s) - delta) <= tol
    
    s_ = doubling_trick(predicate_stop_DT, 0.0, 1.0)
    s_inf = s_[1]; s_sup = s_[2]
    
    s_final = binary_search(predicate_stop_BS, predicate_left_BS, s_inf, s_sup)
    alpha = function_s_to_alpha(s_final)
  }
  
  sigma = alpha * sens / sqrt(2.0 * epsilon)
  
  return(sigma)
}

