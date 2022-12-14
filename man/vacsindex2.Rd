% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vacsindex2.R
\name{vacsindex2}
\alias{vacsindex2}
\alias{score_vacsindex2}
\alias{points_vacindex2}
\title{VACS Index 2}
\usage{
score_vacsindex2(age, cd4_count, vl_log, hgb, fib4, egfr, hepc, alb, bmi, wbc)

points_vacindex2()
}
\arguments{
\item{age}{Numeric. Year.}

\item{cd4_count}{Numeric. CD4 cell count. cell/mm\eqn{^3}.}

\item{vl_log}{Numeric. Log 10 of viral load.}

\item{hgb}{Numeric. Hemoglobin. g/dL.}

\item{fib4}{Numeric. Fibrosis-4 Index for Liver Fibrosis.}

\item{egfr}{Numeric. Estimated glomerular filtration rate. (ml/min/1.73m\eqn{^2}).}

\item{hepc}{Logical. TRUE for co-infection with hepatitis C virus.}

\item{alb}{Numeric. Albumin. g/dL.}

\item{bmi}{Numeric. Body mass index. m/kg\eqn{^2}.}

\item{wbc}{Numeric. White blood cell. \eqn{10^3} count/µL.}
}
\value{
\code{score_vacsindex2()} returns a numerical vector and \code{points_vacsindex2()} returns a list of the parameters used to compute the VACS Index 2.
}
\description{
Compute the VACS Index 2.
}
\examples{
data("vacs2")
vacs2$score_vacs2 <- score_vacsindex2(
  age = vacs2$AGE,cd4_count = vacs2$CD4,vl_log = vacs2$VL_LOG,
  hgb = vacs2$HGB,fib4 = vacs2$FIB4,egfr = vacs2$EGFR,
  hepc = vacs2$HEPC==1,alb = vacs2$ALB,bmi = vacs2$BMI,wbc = vacs2$WBC)
}
\references{
Tate, J. P., Sterne, J. A., Justice, A. C., Study, V. A. C., & Antiretroviral Therapy Cohort Collaboration (ART-CC. (2019). Albumin, white blood cell count, and body mass index improve discrimination of mortality in HIV-positive individuals. AIDS (London, England), 33(5), 903.

McGinnis, K. A., Justice, A. C., Moore, R. D., Silverberg, M. J., Althoff, K. N., Karris, M., ... & Study, V. A. C. (2021). Discrimination And Calibration Of The Vacs Index 2.0 For Predicting Mortality Among People With Hiv In North America. Clinical Infectious Diseases: an Official Publication of the Infectious Diseases Society of America.
}
\concept{vacsindex2}
