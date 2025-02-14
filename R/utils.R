#' Convert depressional storage volume to area
#'
#' @param rating_curve Required. Data frame containing the variables \code{area} and \code{volume}.
#' @param volumes A scalar or vector of depression storage volumes.
#' @param method Method for interpolation. Default is \option{linear}. Alternatively, 
#' a spline can be fitted by specifying \code{method = "spline"} and any other options used by the function \code{spline}.
#'
#' @return Returns a scalar or vector of lake stages
#' @keywords internal
#' @author Kevin Shook
#' @export
#'
#' @examples \dontrun{
#' a <- vol2area_lookup(rating_curve, volume)
#' }

vol2area_rating <- function(rating_curve, volumes, method = "linear"){
  if (method == "linear")
    area <- sapply(volumes, function(l) approx(rating_curve$volume, rating_curve$area, xout = l))
  else
    area <- sapply(volumes, function(l) spline(rating_curve$volume, rating_curve$area, xout = l, method = "fmm"))
  
  a <- as.data.frame(area)
  area <- unlist(a[2,])
  names(area) <- NULL
  return(area)
}

#' Estimates total depressional water area fraction
#'
#' @param volume_frac Fraction of depressional storage
#' @param p Exponent
#'
#' @return Returns the fractional water area
#' @keywords internal
#' @author Kevin Shook
#' @export
#'
#' @examples
#' areafrac <- volfrac2areafrac_Clark(0.5, 1.2)
volfrac2areafrac_Clark <- function(volume_frac, p) {
 area_frac <- min(max(volume_frac ^ (2/(p + 2)), 0), 1)
 return(area_frac)
}

#' Determines if area-volume estimation is by equation or rating
#'
#' @param volume Volume of water in depressional storage 
#' @param max_volume Volume of depression
#' @param max_area Area of depression
#' @param rating_parameters Parameters for estimating water area fraction
#'
#' @return Returns water area fraction
#' @export
#' @keywords internal
#' @author Kevin Shook
#' @examples
#' water_area_frac <- area_frac(1000, 2000, NULL, 1.2)
area_frac <- function(volume, max_volume, max_area= NULL, rating_parameters) {
  if (volume <= 0)
    return(0)
  
  if (volume >= max_volume)
    return(1)
  
  # check to see if values are data frame or equation parameters
  
  if (!is.data.frame(rating_parameters)) {
     area_fraction <- volfrac2areafrac_Clark((volume / max_volume), rating_parameters)
  }
  else {
    area <- vol2area_rating(vol2area_rating, volume)
    area_fraction <- area / max_area
  }
  return(area_fraction)
}
