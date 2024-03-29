% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roc_glm_parts.R
\name{calculateAUC}
\alias{calculateAUC}
\title{Calculate AUC from ROC-GLM}
\usage{
calculateAUC(roc_glm)
}
\arguments{
\item{roc_glm}{(`list()`) List containing the ROC-GLM parameter returned from `dsROCGLM`.}
}
\value{
Numeric value of the approximated AUC.
}
\description{
This function calculates the AUC from the ROC-GLM by integrating the binormal
  form `pnorm(a + b*qnorm(x))`  for x in 0 to 1. The parameter a and b are obtained by `dsROCGLM`.
}
\author{
Daniel S.
}
