#' Calculates connected/contributing fraction
#'
#' @param current_storage Current storage volume
#' @param delta_storage Change in volume
#' @param max_storage Maximum possible storage volume
#' @param current_contrib_frac Current contributing fraction (0-1)
#' @param threshold Threshold for change in storage to set connected fraction to zero.
#'
#' @return Returns the updated connected/contributing fraction
#' @keywords internal
#' @author Kevin Shook
#' @export
#'
#' @examples
#' cf <- linear_hysteresis_CF(current_storage = 50,
#' delta_storage = 10, max_storage = 100, current_contrib_frac = 0)
linear_hysteresis_CF <- function(current_storage = 0, 
                                 delta_storage = 0, 
                                 max_storage = 0, 
                                 current_contrib_frac = 0, 
                                 threshold = -0.01) {
  
  vf1 <- current_storage / max_storage
  cf1 <- current_contrib_frac
  
  if (delta_storage == 0) {
    return(current_contrib_frac)
  } else {
    if (delta_storage < threshold) {
      vf2 <- (current_storage + delta_storage) / max_storage
      return(0)
    } else {
      vf2 <- vf1 + (1 - cf1 ) *  (delta_storage / max_storage)
      if (vf1 < 0.999) {
        cf1 <- current_contrib_frac
        cf2 <- (((1.0 - cf1) * (vf2 - vf1)) / (1.0 - vf1)) + cf1
        cf2 <- min(max(min(cf2, vf2), 0), 1)
        return(cf2)
      } else {
        cf2 <- 1.0
        return(cf2)
      }
    }
  }
}

