#' Generates filling curve from HGDM
#'
#' @description
#' Runs HGDM to generate filling curves for a given basin
#' 
#'
#' @param upland_area Required. Area of uplands, which drain to the outlet, 
#' small depressions or the large depression.
#' @param small_depression_area Required. Area of small depressions.
#' @param large_depression_area Optional. If \code{0} or \code{NULL} large depression is not \
#' modelled.
#' @param area_units Units of all areas. Must be one of \option{km2} (default), \option{ha} or \option{m2}.
#' @param max_small_depression_storage Maximum depth of storage in small depressions.
#' @param max_large_depression_storage Maximum depth of storage in large depressions.
#' @param storage_units Units of all storage depths. Must be one of \option{mm} (default) \option{m},
#' or \option{m3}. If a depth is specified then it will be converted to a volume by multiplying
#' by the appropriate area. 
#' @param small_depressions_initial_connected_fraction Initial connected fraction (0-1).
#' @param upland_fraction_to_small Fraction of uplands draining to small depressions. If \code{0} then
#' the small depressions are unlikely to fill.
#' @param upland_fraction_to_large Fraction of uplands draining to large depression. This is
#' the basin of the large depression.
#' @param upland_fraction_to_outlet Fraction of uplands draining directly to outlet. Analogous
#' to the effective fraction.
#' @param small_fraction_to_large Fraction of small depression area draining into large depression.
#' Governed by location of large depression in the basin.
#' @param forcings A data frame of time series of \code{rainfall}, \code{snowmelt},
#' \code{evap}, and \code{runoff}. The first variable must be either \code{date} (an
#' \pkg{R} date) or \code{datetime} (a POSIXct date-time).
#' @param small_p Parameter for small depression water volume-area relationship.
#' @param large_rating Rating curve parameters for large depression.
#' @param sub_intervals Number of sub-intervals for solution of each time step.
#'
#' @return Returns a data frame with the following values: date, 
#' basin contributing fraction, basin volume fraction, large depression volume fraction,
#' small depression volume fraction, small depression contributing fraction
#' 
#' @export
#'
#' @examples \dontrun{
#'   filling <- HGDM(upland_area, small_depression_area, large_depression_area = large_depression_area, 
#'   area_units = "km2", max_small_depression_storage, 
#'   max_large_depression_storage,
#'   initial_small_depression_storage, 
#'   initial_large_depression_storage,
#'   storage_units = storage_units,
#'   small_depressions_initial_connected_fraction,
#'   upland_fraction_to_small,
#'   upland_fraction_to_large,
#'   upland_fraction_to_outlet,
#'   small_fraction_to_large,
#'   forcings = forcings,
#'   small_p,
#'   large_rating = small_p)
#' }
HGDM_filling_curve <- function(upland_area = NULL, 
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
                                       sub_intervals = 1) {
  
  # set parameter values
  initial_small_depression_storage <- 0
  initial_large_depression_storage <- 0
  
  # create forcing data set
  # first estimate max applied rainfall and runoff
  
  # check values and set up parameters
  # do areas
  if (is.null(upland_area) | (upland_area <= 0))
    stop("Area of uplands is required")
  
  if (is.null(large_depression_area) | (large_depression_area <= 0)) 
    do_large_depression <- FALSE
  else
    do_large_depression <- TRUE  
  
  # convert areas
  if (is.null(area_units) | area_units == "")
    area_units <- "km2"
  
  if (tolower(area_units) == "km2")
    area_mult <- 1e6
  else if (tolower(area_units) == "ha")
    area_mult <- 1e4
  else
    area_mult <- 1.0
  
  fill_upland_area <- upland_area * area_mult
  fill_small_depression_area <- small_depression_area * area_mult
  
  if (!do_large_depression)
    fill_basin_area <- upland_area + small_depression_area 
  else {
    fill_large_depression_area <- large_depression_area * area_mult
    fill_basin_area <- upland_area + small_depression_area + large_depression_area
  }
  
  # do volumes
  storage_units <- tolower(storage_units)
  if (storage_units == "mm") {
    depth_mult <- 0.001
    do_depth_to_volume <- TRUE
  }
  else {
    if (storage_units == "m") {
      depth_mult <- 1
      do_depth_to_volume <- TRUE
    }
    else{
      # units in m3
      depth_mult <- 1.0  
      do_depth_to_volume <- FALSE
    }
  }
  
  if (do_depth_to_volume) {
    fill_max_small_depression_water_volume <- max_small_depression_storage * depth_mult * small_depression_area
    if (do_large_depression) {
      fill_max_large_depression_water_volume <- max_large_depression_storage * depth_mult * large_depression_area
    } else {
      fill_max_large_depression_water_volume <- NULL
    }
  } else {
    fill_max_small_depression_water_volume <- max_small_depression_storage
    if (do_large_depression) {
      fill_max_large_depression_water_volume <- max_large_depression_storage 
    } else {
      fill_max_large_depression_water_volume <- NULL
    }
  }
  

  # convert max storage to applied depth and multiply by 4
  
  if (do_large_depression) 
    fill_total_max_vol <- fill_max_large_depression_water_volume + fill_max_small_depression_water_volume
  else
    fill_total_max_vol <- fill_max_small_depression_water_volume
  
  fill_max_depth <- fill_total_max_vol / fill_basin_area  # m
  
  fill_max_applied_depth <- floor(fill_max_depth * 1000 * 4)   # mm
  
  numvals <- fill_max_applied_depth    # apply 1 mm / interval
  date <- seq.Date(Sys.Date(), by = "day", length.out = numvals)
  
  rainfall <- rep.int(1,  times = fill_max_applied_depth)
  runoff <- rainfall
  evap <- rep.int(0,  times = fill_max_applied_depth)
  snowmelt <- evap
  
  forcings <- data.frame(date, rainfall, runoff, evap, snowmelt)
  
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
  return(filling)
}