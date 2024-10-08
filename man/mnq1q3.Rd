% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mnq1q3.R
\name{mnq1q3}
\alias{mnq1q3}
\title{Mean, 25th percentile, and 75th percentile.}
\usage{
mnq1q3(x)
}
\arguments{
\item{x}{Numeric values.}
}
\value{
a named vector with three elements:
\enumerate{
\item mn Mean.
\item q1 First quartile (= 25th percentile).
\item q3 Third quartile (= 75th percentile).
}
}
\description{
Compute mean with a 25th percentile to 75th percentile interval.
}
\examples{
mnq1q3(x=50:60)
}
\author{
Marcel Miché
}
