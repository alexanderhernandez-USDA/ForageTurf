# Load required packages

library(dplyr)
library(sf)
library(terra)
library(raster)
library(mapview)
library(excelR)
library(stringr)
###
setwd("/media/geodata/shared/Turf/Greenville")
library(here)

## load geopackage with all the VIs
Green.plots<-read_sf(here("ToZonal/Greenville_plots_2024zonal11.gpkg"))

# Load Shaun's field data for Y2023
VisualRat2023<-readxl::read_excel(here("Greenvilleratings_22_23.xlsx"), sheet = "KBG_2023",
                                  skip = 2)
VisualRat2022<-visual.ratings%>%
  dplyr::select(RepPlot,VR_07_28,VR_08_10,VR_09_23)%>%
  rename_with(~ paste(., "2022", sep = "_"),starts_with("VR")) %>% # adds the year "2022" to the end of the column name
  rename_with(~str_remove(.,"\\_")) # removes the "_" after the first two characters in the column name
  
