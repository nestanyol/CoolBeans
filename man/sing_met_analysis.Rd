% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sing_met.R
\name{sing_met_analysis}
\alias{sing_met_analysis}
\title{Analysis of single metabolites based on a target exposure feature (and covariate/s).}
\usage{
sing_met_analysis(
  data,
  train_data,
  exposure_feature,
  start_metabolites,
  covariates = NULL,
  threshold = 0.01,
  correction = NULL
)
}
\arguments{
\item{data}{data frame to use for analysis. It contains subject id, an exposure feature variable and metabolite variables;
can also contain covariates.}

\item{train_data}{data or split data.}

\item{exposure_feature}{variable of exposure name (e.g. diet score, group of exposure).}

\item{start_metabolites}{column index where metabolite features start.}

\item{covariates}{list of optional covariates.}

\item{threshold}{Threshold for significance in the results filtering.}

\item{correction}{Multiple testinc optional correction.}
}
\value{
A [tibble::tibble()].
}
\description{
The function concurrently fits a linear model with each metabolite feature in the data set, with the exposure feature
as the primary predictor and other features that are defined as covariates.
}
