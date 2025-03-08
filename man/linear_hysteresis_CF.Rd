% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_hysteresis_CF.R
\name{linear_hysteresis_CF}
\alias{linear_hysteresis_CF}
\title{Calculates connected/contributing fraction}
\usage{
linear_hysteresis_CF(
  current_storage = 0,
  delta_storage = 0,
  max_storage = 0,
  current_contrib_frac = 0,
  threshold = -0.01
)
}
\arguments{
\item{current_storage}{Current storage volume}

\item{delta_storage}{Change in volume}

\item{max_storage}{Maximum possible storage volume}

\item{current_contrib_frac}{Current contributing fraction (0-1)}

\item{threshold}{Threshold for change in storage to set connected fraction to zero.}
}
\value{
Returns the updated connected/contributing fraction
}
\description{
Calculates connected/contributing fraction
}
\examples{
cf <- linear_hysteresis_CF(current_storage = 50,
delta_storage = 10, max_storage = 100, current_contrib_frac = 0)
}
\author{
Kevin Shook
}
\keyword{internal}
