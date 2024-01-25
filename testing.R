# Load required packages

library(dplyr)
library(sf)
library(terra)
library(raster)
library(mapview)

###
setwd("/media/geodata/shared/Turf/Greenville")
library(here)

## load geopackage with all the VIs
Green.plots<-read_sf(here("ToZonal/Greenville_plots_2024zonal11.gpkg"))
