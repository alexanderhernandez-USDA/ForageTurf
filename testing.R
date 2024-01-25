# Load required packages

library(dplyr)
library(sf)
library(terra)
library(raster)
library(mapview)
library(excelR)
###
setwd("/media/geodata/shared/Turf/Greenville")
library(here)

## load geopackage with all the VIs
Green.plots<-read_sf(here("ToZonal/Greenville_plots_2024zonal11.gpkg"))

# Load Shaun's field data for Y2023
VisualRat2023<-readxl::read_excel(here("Greenvilleratings22_23.xlsx"), sheet = "KBG_2023")
