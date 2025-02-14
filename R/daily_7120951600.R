#' daily basin 7120951600 PHyDAP fluxes
#' @description
#' A dataframe of daily CRHM fluxes modelled for basin 7120951600. The fluxes were taken from
#' the PHyDAP project \url{https://www.frdr-dfdr.ca/repo/dataset/7ce4bd7a-4bcc-4f8c-8129-32a691f46c8e} hourly outputs
#' of CRHM models forced with ERA5 data over the period 1950-2020. The fluxes were then aggregated to daily values. 
#' 
#' @format A dateframe with 25932 rows and 5 columns spanning the period 1950-2020.
#' @source PHyDAP
#' @references Shook, Kevin R., Zhihua He, John W. Pomeroy, Chris Spence, and Colin J. Whitfield. “A Practitioner-Oriented Regional Hydrology Data Product for Use in Site-Specific Hydraulic Applications.” Scientific Data 11, no. 1 (October 14, 2024): 1125. https://doi.org/10.1038/s41597-024-03962-1.
#' @details 
#' Variables: 
#' \describe{
#' \item{date}{R date}
#' \item{rainfall}{Daily rainfall on water (mm)}
#' \item{snowmelt}{Daily snow melt on water (mm)}
#' \item{runoff}{Daily upland runoff (mm)}
#' \item{evap}{Daily water evaporation (mm) }
#' }
#' 
"daily_7120951600"
NULL