% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{anemia}
\alias{anemia}
\title{Anemia}
\usage{
anemia(hgb, sex)
}
\arguments{
\item{hgb}{Numeric. Hemoglobine. g/dL.}

\item{sex}{Character. Either \code{"male"} or \code{"female"}.}
}
\value{
Logical. TRUE if \code{hgb} < 13 g/dL. For female, TRUE if \code{hgb} < 12 g/dL.
}
\description{
Anemia
}
\seealso{
Other utils: 
\code{\link{bmi}()},
\code{\link{egfr}()},
\code{\link{fib4}()},
\code{\link{low_bmi}()}
}
\concept{utils}
