% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baseline.R
\name{baseline}
\alias{baseline}
\alias{basesurv_hentzien2018}
\alias{basesurv_riskgroup_hentzien2018}
\title{Baseline survival function}
\usage{
basesurv_hentzien2018(time)

basesurv_riskgroup_hentzien2018(time)
}
\arguments{
\item{time}{Numeric. Time in year.}
}
\description{
Baseline survival function
}
\details{
\code{basesurv_hentzien2018} is given by Hentzien (2018):
\deqn{S_0(t) = 1-0.00562t - 0.00057679t^2}

\code{basesurv_riskgroup_hentzien2018} is given by Hentzien (2018):
\deqn{S_0(t) = 1-0.00610t - 0.00050468t^2}
}
\references{
Hentzien M, Delpierre C, Pugliese P, Allavena C, Jacomet C, Valantin M-A, et al. (2018) Derivation and internal validation of a mortality risk index for aged people living with HIV: The Dat'AIDS score. PLoS ONE 13(4): e0195725. https://doi.org/10.1371/journal.pone.0195725
}
\seealso{
Other dataids: 
\code{\link{dataids_score}}

Other dataids: 
\code{\link{dataids_score}}
}
\concept{dataids}
