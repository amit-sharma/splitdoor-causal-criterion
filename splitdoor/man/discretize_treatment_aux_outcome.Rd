% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/independence_test.R
\name{discretize_treatment_aux_outcome}
\alias{discretize_treatment_aux_outcome}
\title{Discretize treatment and outcome timeseries data into user-provided number of levels. Used by the fisher independence function.}
\usage{
discretize_treatment_aux_outcome(tseries_tbl, num_discrete_levels)
}
\arguments{
\item{num_discrete_levels}{}
}
\value{
A data.frame containing discretized values of the user-provided time-series.
}
\description{
Discretize treatment and outcome timeseries data into user-provided number of levels. Used by the fisher independence function.
}
