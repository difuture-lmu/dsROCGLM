% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_scores.R
\name{getPositiveScoresVar}
\alias{getPositiveScoresVar}
\title{Return variance of positive scores}
\usage{
getPositiveScoresVar(truth_name, prob_name, m = NULL, return_sum = FALSE)
}
\arguments{
\item{truth_name}{(`character(1L)`) Character containing the name of the vector of 0-1-values
encoded as integer or numeric.}

\item{prob_name}{(`character(1L)`) Character containing the name of the vector of probabilities.}

\item{m}{(`numeric(1L)`) Sample mean used for variance calculation. If `NULL` (default), the
sample mean of the positive scores is used.}

\item{return_sum}{(`logical(1L)`) Logical value indicating whether the function should
just return the sum of positive scores.}
}
\value{
Variance of differences of positive scores
}
\description{
This function just returns the variance of positive scores.
}
\author{
Daniel S.
}
