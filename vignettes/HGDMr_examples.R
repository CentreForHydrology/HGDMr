## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = TRUE,
                      echo = TRUE,
                      fig.width = 7, 
                      warning = FALSE,
                      message = FALSE)
library(HGDMr)
library(ggplot2)

## -----------------------------------------------------------------------------
data(HGDMr)
summary(daily_7120951600)

## -----------------------------------------------------------------------------
area_units <- "km2"
basin_area <- 100 
small_depression_frac <- 0.24
small_depression_area <- small_depression_frac * basin_area
large_depression_area <- 0
upland_area <- basin_area - (small_depression_area + large_depression_area)

## -----------------------------------------------------------------------------
storage_units <- "mm"
max_small_depression_storage <- 500
max_large_depression_storage <- 0

## -----------------------------------------------------------------------------
initial_small_depression_storage <- max_small_depression_storage / 2
initial_large_depression_storage <- max_large_depression_storage / 2
small_depressions_initial_connected_fraction <- 0

## -----------------------------------------------------------------------------
upland_fraction_to_small <- 0.98
upland_fraction_to_large <- 0
upland_fraction_to_outlet <- 0.02
small_fraction_to_large <- 0

## -----------------------------------------------------------------------------
small_p <- 1.2
sub_intervals <- 1
large_rating <- 1.4

## ----eval = FALSE-------------------------------------------------------------
# simulation <- HGDM(
#   upland_area,
#   small_depression_area,
#   large_depression_area,
#   area_units,
#   max_small_depression_storage,
#   max_large_depression_storage,
#   initial_small_depression_storage,
#   initial_large_depression_storage,
#   storage_units,
#   small_depressions_initial_connected_fraction,
#   upland_fraction_to_small,
#   upland_fraction_to_large,
#   upland_fraction_to_outlet,
#   small_fraction_to_large,
#   forcings = daily_7120951600,
#   small_p,
#   large_rating,
#   sub_intervals)

## ----eval = FALSE-------------------------------------------------------------
# p <- ggplot(simulation, aes(date, total_outflow_volume)) +
#   geom_point()
# p

## ----eval = FALSE-------------------------------------------------------------
# 
# p <- ggplot(simulation, aes(date, total_contrib_frac)) +
#   geom_point() +
#   ylim(0, 1)
# p

## ----eval = FALSE-------------------------------------------------------------
# 
# simulation$small_depression_water_volume_fraction <-
#   simulation$small_depression_water_depth / (max_small_depression_storage / 1000)
# 
# p2 <- ggplot(simulation,
#             aes(small_depression_water_volume_fraction,
#                 small_depression_contrib_frac)) +
#   geom_point() +
#   xlab("Meta depression volumetric fraction") +
#   ylab("Meta depression connected/contributing fraction") +
#   coord_fixed(ratio = 1) +
#   xlim(0, 1) +
#   ylim(0, 1) +
#   geom_abline(slope = 1, intercept = 0, colour = "red")
# p2

## -----------------------------------------------------------------------------

max_large_depression_storage <- 2000
total_depression_area <- small_depression_area
large_depression_frac <- 0.3
large_depression_area <- large_depression_frac * total_depression_area
small_depression_area <- (1 - large_depression_frac) * total_depression_area
upland_area <- basin_area - (small_depression_area + large_depression_area)
initial_large_depression_storage <- max_large_depression_storage / 2

upland_fraction_to_small <- 0.96
upland_fraction_to_large <- 0.02
upland_fraction_to_outlet <- 0.02
small_fraction_to_large <- 0.25

## ----eval = FALSE-------------------------------------------------------------
# 
# simulation_large_pond <- HGDM(
#   upland_area,
#   small_depression_area,
#   large_depression_area,
#   area_units,
#   max_small_depression_storage,
#   max_large_depression_storage,
#   initial_small_depression_storage,
#   initial_large_depression_storage,
#   storage_units,
#   small_depressions_initial_connected_fraction,
#   upland_fraction_to_small,
#   upland_fraction_to_large,
#   upland_fraction_to_outlet,
#   small_fraction_to_large,
#   forcings = daily_7120951600,
#   small_p,
#   large_rating,
#   sub_intervals
#   )

## ----eval = FALSE-------------------------------------------------------------
# p3 <- ggplot(simulation_large_pond, aes(date, total_contrib_frac)) +
#   geom_point()
# p3

## ----eval = FALSE-------------------------------------------------------------
# max_water_volume <- ((max_large_depression_storage / 1000) *
#                        (large_depression_area * 1e6)) +
#   ((max_small_depression_storage / 1000) * (small_depression_area * 1e6))
# 
# simulation_large_pond$total_water_volume_fraction <-
#   (simulation_large_pond$small_depression_water_volume +
#      simulation_large_pond$large_depression_water_volume) / max_water_volume
# 
# p4 <- ggplot(simulation_large_pond,
#              aes(total_water_volume_fraction, total_contrib_frac)) +
#   geom_point() +
#   xlab("Total volumetric fraction") +
#   ylab("Total connected/contributing fraction") +
#   coord_fixed(ratio = 1) +
#   xlim(0, 1) +
#   ylim(0, 1)
# p4

