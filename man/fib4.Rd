% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{fib4}
\alias{fib4}
\title{Fibrosis-4 Index for Liver Fibrosis formula}
\usage{
fib4(age, alt, ast, plt)
}
\arguments{
\item{age}{Numeric. Years.}

\item{alt}{Numeric. Alanine aminotransferase. U/L.}

\item{ast}{Numeric. Aspartate aminotransferase. U/L.}

\item{plt}{Numeric. Platelet count. \eqn{\times} 10^9/L.}
}
\value{
The FIB-4 index.
}
\description{
Fibrosis-4 Index for Liver Fibrosis formula
}
\references{
Sterling, R. K., Lissen, E., Clumeck, N., Sola, R., Correa, M. C., Montaner, J., ... & Nelson, M. (2006). Development of a simple noninvasive index to predict significant fibrosis in patients with HIV/HCV coinfection. Hepatology, 43(6), 1317-1325.
}
\seealso{
Other utils: 
\code{\link{anemia}()},
\code{\link{bmi}()},
\code{\link{egfr}()},
\code{\link{low_bmi}()}
}
\concept{utils}
