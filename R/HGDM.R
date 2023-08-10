#' Applies HGDM to forcings
#'
#' @param upland_area Required. Area of uplands, which drain to the outlet, 
#' small depressions or the large depression.
#' @param small_depression_area Required. Area of small depressions.
#' @param large_depression_area Optional. If \code{0} or \code{NULL} large depression is not \
#' modelled.
#' @param area_units Units of all areas. Must be one of \option{km2} (default), \option{ha} or \option{m2}.
#' @param max_small_depression_storage Maximum depth of storage in small depressions.
#' @param max_large_depression_storage Maximum depth of storage in large depressions.
#' @param initial_small_depression_storage Initial depth of storage in small depressions.
#' @param initial_large_depression_storage Initial depth of storage in large depressions.
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
#' @param forcings Reauired. A data frame of time series of \code{rainfall}, \code{snowmelt},
#' \code{evap}, and \code{runoff}. The first variable must be either \code{date} (an
#' \pkg{R} date) or \code{datetime} (a POSIXct date-time).
#' @param small_p Parameter for small depression water volume-area relationship.
#' @param large_rating Rating curve parameters for large depression.
#' @param sub_intervals Number of sub-intervals for solution of each time step.
#'
#' @return Returns a data frame with the following values: date, 
#' basin contributing fraction, basin volume fraction, large depression volume fraction,
#' small depression volume fraction, small depression contributing fraction,
#' net flux, runoff, discharge
#' 
#' @import stringr
#' @export
#'
#' @examples \dontrun{
#' 
#' basin_area <- 1018  - 13 # km2 - basin area - lake area
#' small_depression_frac <- 0.24
#' small_depression_area <- small_depression_frac * basin_area
#' large_depression_area <- 0
#' upland_area <- basin_area - (small_depression_area + large_depression_area)
#' area_units <- "km2"
#' max_small_depression_storage <- 300
#' max_large_depression_storage <- 0
#' initial_small_depression_storage <- max_small_depression_storage / 2
#' initial_large_depression_storage <- max_large_depression_storage / 2
#' storage_units <- "mm"
#' small_depressions_initial_connected_fraction <- 0
#' upland_fraction_to_small <- 0.98
#' upland_fraction_to_large <- 0
#' upland_fraction_to_outlet <- 0.02
#' small_fraction_to_large <- 0
#' small_p <- 1.2
#' sub_intervals <- 1
#' 
#' results <- HGDM(upland_area, 
#' small_depression_area, 
#' large_depression_area = 0, 
#' area_units = "km2", max_small_depression_storage, 
#' max_large_depression_storage,
#' initial_small_depression_storage, 
#' initial_large_depression_storage,
#' storage_units,
#' small_depressions_initial_connected_fraction,
#' upland_fraction_to_small,
#' upland_fraction_to_large,
#' upland_fraction_to_outlet,
#' small_fraction_to_large,
#' forcings = daily_fluxes,
#' small_p)
#' }
HGDM <- function(upland_area = NULL, 
                 small_depression_area = NULL, 
                 large_depression_area = NULL, 
                 area_units = "km2", 
                 max_small_depression_storage = 0,
                 max_large_depression_storage = 0,
                 initial_small_depression_storage = 0,
                 initial_large_depression_storage = 0,
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
  
  # define vectors used for output
  total_contrib_frac <- c(0)
  total_outflow_volume <- c(0)
  small_depression_contrib_frac <- c(0)
  small_depression_water_volume <- c(0)
  small_depression_water_depth <- c(0)
  small_depression_water_area <- c(0)
  large_depression_water_volume <- c(0)
  large_depression_water_area <- c(0) 
  large_depression_contrib_frac <- c(0)
  large_depression_contrib_area <- c(0)
  
  # check values and set up parameters
  # do areas
  if (is.null(upland_area) | (upland_area <= 0))
    stop("Area of uplands is required")
  
  if (is.null(forcings))
    stop("Forcing variables are required")
  
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
   
   upland_area <- upland_area * area_mult
   small_depression_area <- small_depression_area * area_mult
   
  if (!do_large_depression)
      basin_area <- upland_area + small_depression_area 
    else {
      large_depression_area <- large_depression_area * area_mult
      basin_area <- upland_area + small_depression_area + large_depression_area
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
     max_small_depression_water_volume <- max_small_depression_storage * depth_mult * small_depression_area
     initial_small_depression_water_volume <- initial_small_depression_storage * depth_mult * small_depression_area
     if (do_large_depression) {
       max_large_depression_water_volume <- max_large_depression_storage * depth_mult * large_depression_area
       initial_large_depression_water_volume <- initial_large_depression_storage * depth_mult * large_depression_area
     } else {
       max_large_depression_water_volume <- NULL
       initial_large_depression_water_volume <- NULL
     }
   } else {
     max_small_depression_water_volume <- max_small_depression_storage
     initial_small_depression_water_volume <- initial_small_depression_storage
     if (do_large_depression) {
       max_large_depression_water_volume <- max_large_depression_storage 
       initial_large_depression_water_volume <- initial_large_depression_storage  
     } else {
       max_large_depression_water_volume <- NULL
       initial_large_depression_water_volume <- NULL
     }
   }
   
  # do redirection fractions

  if (upland_fraction_to_small > 1)
    stop("upland_fraction_to_small is a fraction <= 1")
  
  if (upland_fraction_to_large > 1)
    stop("upland_fraction_to_large is a fraction <= 1")

  if (upland_fraction_to_outlet > 1)
    stop("upland_to_outlet is a fraction <= 1") 
  
  upland_total <- upland_fraction_to_small + upland_fraction_to_large + upland_fraction_to_outlet
  
  if (upland_total > 1) {
    message("Total upland fraction > 1, will adjust")
    upland_fraction_to_small <- upland_fraction_to_small / upland_total
    upland_fraction_to_large <- upland_fraction_to_large / upland_total
    upland_fraction_to_outlet <- upland_fraction_to_outlet / upland_total
  }
  
  sub_intervals <- max(sub_intervals, 1)
  
 # iterate over all time steps
 # make sure that forcings are a data frame, not a tibble
  forcings <- data.frame(forcings)
  num_steps <- nrow(forcings)
  timename <- names(forcings)[1]

  # set initial state vars
  small_depression_contrib_frac[1] <- small_depressions_initial_connected_fraction
  small_depression_water_volume[1] <- initial_small_depression_water_volume
  small_depression_water_depth[1] <- small_depression_water_volume[1] / small_depression_area
  small_depression_water_area[1] <- volfrac2areafrac_Clark(small_depression_water_volume[1] /
                                                             max_small_depression_water_volume,
                                                           small_p) * small_depression_area 
  
  if (do_large_depression) {
    large_depression_water_volume[1] <- initial_large_depression_water_volume
    large_depression_water_area[1] <- area_frac(initial_large_depression_water_volume,
                                                max_large_depression_water_volume,
                                                large_depression_area,
                                                large_rating)
  }
  else {
    large_depression_water_volume[1] <- 0
  }
  
  for (i in 2:num_steps) {
   rain <- forcings[i, "rainfall"]
   evap <- forcings[i, "evap"]
   snowmelt <- forcings[i, "snowmelt"]
   runoff <- forcings[i, "runoff"]
   net_depth <- (rain - evap + snowmelt) / 1000 # m
   runoff <- runoff / 1000                      # m
   
   if (net_depth == 0 & runoff <= 0) {
     # no change in storage - keep state vars const and set fluxes to zero
     
     total_contrib_frac[i] <-  total_contrib_frac[i - 1]
     total_outflow_volume[i] <- 0
     small_depression_contrib_frac[i] <- small_depression_contrib_frac[i - 1]
     small_depression_water_volume[i] <- small_depression_water_volume[i - 1]
     small_depression_water_area[i] <- small_depression_water_area[i - 1]
     small_depression_water_depth[i] <- small_depression_water_depth[i - 1]
     
     if (do_large_depression) {
       large_depression_water_volume[i] <- large_depression_water_volume[i - 1] 
       large_depression_water_area[i] <- large_depression_water_area[i - 1]
       large_depression_contrib_frac[i] <- large_depression_contrib_frac[i - 1] 
     }

   } else {
     # there are fluxes to apply
     
     interval_runoff <- runoff     
     interval_net_depth <- net_depth 
     interval_small_depression_water_volume <- small_depression_water_volume[i - 1]
     interval_small_contrib_frac <- small_depression_contrib_frac[i - 1] 
     

     # figure out volume to be added to small depressions
     interval_delta_small_runoff_vol <- interval_runoff * upland_area * upland_fraction_to_small
     
     interval_small_water_area <- volfrac2areafrac_Clark(interval_small_depression_water_volume /
                                    max_small_depression_water_volume,
                                    small_p) * small_depression_area  #m2
     
     delta_small_volume <- interval_net_depth * interval_small_water_area  + interval_delta_small_runoff_vol #m3
     
     # get new contributing fraction of small depressions before changing volume of water in small
     # depressions
     small_contrib_frac_new <- linear_hysteresis_CF(interval_small_depression_water_volume, 
                                                    delta_small_volume, 
                                                    max_small_depression_water_volume, 
                                                    interval_small_contrib_frac)
    
       
       # add water to small depressions, taking into account contributing fraction
       
       if (delta_small_volume > 0 ) {
        interval_small_depression_water_volume <- interval_small_depression_water_volume + delta_small_volume * ( 1 - interval_small_contrib_frac) #m3
        interval_small_outflow_filled <- max(interval_small_depression_water_volume - max_small_depression_water_volume, 0)  # m3 - small volume filled
        interval_small_depression_water_volume <- min(interval_small_depression_water_volume, max_small_depression_water_volume)
    
        # get outflows of small depressions to large depression and outlet
        interval_small_outflow_vol <- (delta_small_volume * interval_small_contrib_frac) + interval_small_outflow_filled
        interval_small_volume_to_large <- interval_small_outflow_vol * small_fraction_to_large
        interval_small_volume_to_outlet <- interval_small_outflow_vol * (1 - small_fraction_to_large)
        
        } else {
          
         if (delta_small_volume < 0) { 
           interval_small_depression_water_volume <- interval_small_depression_water_volume + delta_small_volume #m3
           interval_small_outflow_filled <- 0  # m3 - small volume filled
           interval_small_depression_water_volume <- max(interval_small_depression_water_volume, 0)
         } else {
           # zero change 
           interval_small_outflow_filled <- 0
         }
         # get outflows of small depressions to large depression and outlet
         interval_small_outflow_vol <- 0
         interval_small_volume_to_large <- 0
         interval_small_volume_to_outlet <- 0
       }
       
       # get upland to outlet
       interval_upland_volume_to_outlet <- interval_runoff * upland_area * upland_fraction_to_outlet
  
      
       # do large depression
       if (do_large_depression) {
         interval_large_depression_water_volume <- large_depression_water_volume[i - 1]
         interval_upland_volume_to_large <- interval_runoff * upland_area * upland_fraction_to_large
         interval_large_depression_water_area <- area_frac(interval_large_depression_water_volume,
                                                           max_large_depression_water_volume,
                                                           large_depression_area,
                                                           large_rating) * large_depression_area
         
         interval_large_depression_water_volume <- interval_large_depression_water_volume + 
           (interval_net_depth * interval_large_depression_water_area) +
           interval_small_volume_to_large +
           interval_upland_volume_to_large

         if (interval_large_depression_water_volume >= max_large_depression_water_volume) {
           interval_large_depression_volume_to_outlet <- max(0, (interval_large_depression_water_volume - max_large_depression_water_volume))
           interval_large_depression_water_volume <- max_large_depression_water_volume
           interval_large_depression_contrib_area <- large_depression_area + 
             (upland_fraction_to_large * upland_area) + 
             interval_small_contrib_frac * 
             small_depression_area  * upland_fraction_to_large
                                                           
           interval_large_depression_contrib_frac <- 1
         } else{
           interval_large_depression_contrib_area <- 0
           interval_large_depression_volume_to_outlet <- 0
           interval_large_depression_contrib_frac <- 0
         }
         
       } else {
         interval_large_depression_water_volume <- NULL
         interval_large_depression_water_area <- NULL
         interval_large_depression_contrib_frac <- NULL
         interval_large_depression_volume_to_outlet <- NULL
         interval_large_depression_contrib_area <- NULL
       }
       
       # update fluxes and states
       # update contributing fractions

       interval_small_contrib_area_to_outlet <- interval_small_contrib_frac * 
         (small_depression_area + (upland_area * upland_fraction_to_small))
       interval_upland_contrib_area_to_outlet <- upland_area * upland_fraction_to_outlet
       
       if (do_large_depression) {
         total_contrib_area <- interval_large_depression_contrib_area + 
         interval_small_contrib_area_to_outlet + interval_upland_contrib_area_to_outlet
       }
       else {
         total_contrib_area <- interval_small_contrib_area_to_outlet + interval_upland_contrib_area_to_outlet
       }
       
       total_interval_outflow_volume <- 
         interval_upland_volume_to_outlet +
         interval_small_volume_to_outlet
       
       if (do_large_depression) 
         total_interval_outflow_volume <- total_interval_outflow_volume + 
         interval_large_depression_volume_to_outlet
       
     
     # assemble total fluxes and state variables for the interval
     interval_small_contrib_frac <- small_contrib_frac_new 
     total_contrib_frac[i] <- total_contrib_area / basin_area
     total_outflow_volume[i] <- total_interval_outflow_volume
     small_depression_contrib_frac[i] <- interval_small_contrib_frac
     small_depression_water_volume[i] <- interval_small_depression_water_volume
     small_depression_water_area[i] <- interval_small_water_area
     small_depression_water_depth[i] <- interval_small_depression_water_volume / small_depression_area
     
     if (do_large_depression) {
      large_depression_water_volume[i] <- interval_large_depression_water_volume
      large_depression_water_area[i] <- interval_large_depression_water_area 
      large_depression_contrib_frac[i] <- interval_large_depression_contrib_frac
      large_depression_contrib_area[i] <- interval_large_depression_contrib_area

     }
    }
  }
  
  # assemble outputs
  
  all <- data.frame(forcings$date, 
                    total_contrib_frac, 
                    total_outflow_volume,
                    small_depression_contrib_frac, 
                    small_depression_water_volume, 
                    small_depression_water_depth,
                    small_depression_water_area)
  
  if (do_large_depression) {
    all <- cbind(all, large_depression_contrib_frac,
                 large_depression_water_volume,
                 large_depression_water_area)
  }
  
  return(all)
}
