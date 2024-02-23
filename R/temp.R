#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Site Map
# Coder: Nate Jones
# Date: 11/9/2023
# Purpose: Create publication quality map for wetland sites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup workspace --------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list=ls())

#load relevant packages
library(tidyverse) #data wrangling
library(readxl)    #load xlsx files
library(raster)    #raster data
library(sf)        #vector data
library(mapview)   #interactive plotting
library(whitebox)  #GIS
library(elevatr)   #DEM download
library(tigris)    #State shape download
library(nhdplusTools) #nhdplus download
library(terrainr)

#load site locations
pnts <- read_xlsx('data//site_locations.xlsx')

#Create sf point object
pnts <- pnts %>% 
  st_as_sf(
    coords = c("Longitude", "Latitude"), 
    crs = 4326) 

# #Create area of interest for download
# aoi <- st_bbox(pnts) %>% st_as_sfc() %>% st_as_sf()
# 
# #Convert to planar coordinates
# aoi <- st_transform(aoi, crs = st_crs("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +type=crs"))

#download arial imagery 
with_progress(
  output_files <- get_tiles(pnts ,
                          output_prefix = tempfile(),
                          services = c("ortho"), 
                          resolution = 10)
  )

raster::plotRGB(raster::brick(output_files[["ortho"]][[1]]), scale = 1)

