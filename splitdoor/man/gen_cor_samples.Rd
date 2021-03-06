% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/independence_test.R
\name{gen_cor_samples}
\alias{gen_cor_samples}
\title{Simulate two vectors and compute correlation between them.}
\usage{
gen_cor_samples(x_vec, y_vec, cor_metric, x_vals, names_xvals)
}
\arguments{
\item{p_value}{}
}
\value{
A data.frame containing test results for each (treatment, outcome) pair.
Probability of independence of the two time-series is provided along with Pass/Fail result of the test based on the user-provided independence_threshold.
}
\description{
A part of the randomization test procedure.
}
