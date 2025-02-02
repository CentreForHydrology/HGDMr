% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HGDM_filling_curve.R
\name{HGDM_filling_curve}
\alias{HGDM_filling_curve}
\title{Generates filling curve from HGDM}
\usage{
HGDM_filling_curve(
  upland_area = NULL,
  small_depression_area = NULL,
  large_depression_area = NULL,
  area_units = "km2",
  max_small_depression_storage = 0,
  max_large_depression_storage = 0,
  storage_units = "mm",
  small_depressions_initial_connected_fraction = 0,
  upland_fraction_to_small = 0,
  upland_fraction_to_large = 0,
  upland_fraction_to_outlet = 0,
  small_fraction_to_large = 0,
  forcings = NULL,
  small_p = NULL,
  large_rating = 0,
  sub_intervals = 1
)
}
\arguments{
\item{upland_area}{Required. Area of uplands, which drain to the outlet, 
small depressions or the large depression.}

\item{small_depression_area}{Required. Area of small depressions.}

\item{large_depression_area}{Optional. If \code{0} or \code{NULL} large depression is not \
modelled.}

\item{area_units}{Units of all areas. Must be one of \option{km2} (default), \option{ha} or \option{m2}.}

\item{max_small_depression_storage}{Maximum depth of storage in small depressions.}

\item{max_large_depression_storage}{Maximum depth of storage in large depressions.}

\item{storage_units}{Units of all storage depths. Must be one of \option{mm} (default) \option{m},
or \option{m3}. If a depth is specified then it will be converted to a volume by multiplying
by the appropriate area.}

\item{small_depressions_initial_connected_fraction}{Initial connected fraction (0-1).}

\item{upland_fraction_to_small}{Fraction of uplands draining to small depressions. If \code{0} then
the small depressions are unlikely to fill.}

\item{upland_fraction_to_large}{Fraction of uplands draining to large depression. This is
the basin of the large depression.}

\item{upland_fraction_to_outlet}{Fraction of uplands draining directly to outlet. Analogous
to the effective fraction.}

\item{small_fraction_to_large}{Fraction of small depression area draining into large depression.
Governed by location of large depression in the basin.}

\item{forcings}{A data frame of time series of \code{rainfall}, \code{snowmelt},
\code{evap}, and \code{runoff}. The first variable must be either \code{date} (an
\pkg{R} date) or \code{datetime} (a POSIXct date-time).}

\item{small_p}{Parameter for small depression water volume-area relationship.}

\item{large_rating}{Rating curve parameters for large depression.}

\item{sub_intervals}{Number of sub-intervals for solution of each time step.}
}
\value{
Returns a data frame with the following values: date, 
basin contributing fraction, basin volume fraction, large depression volume fraction,
small depression volume fraction, small depression contributing fraction
}
\description{
Runs HGDM
}
\examples{
\dontrun{
  filling <- HGDM(upland_area, small_depression_area, large_depression_area = large_depression_area, 
  area_units = "km2", max_small_depression_storage, 
  max_large_depression_storage,
  initial_small_depression_storage, 
  initial_large_depression_storage,
  storage_units = storage_units,
  small_depressions_initial_connected_fraction,
  upland_fraction_to_small,
  upland_fraction_to_large,
  upland_fraction_to_outlet,
  small_fraction_to_large,
  forcings = forcings,
  small_p,
  large_rating = small_p)
}
}
