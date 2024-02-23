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
library(terrainr)  #Download NAIP imagery

#load site locations
pnts <- read_xlsx('data//site_locations.xlsx')

#Turn off spherical geometry with sf
sf_use_s2(FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Create spatial data ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create sf point object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pnts <- pnts %>% 
  st_as_sf(
    coords = c("Longitude", "Latitude"), 
    crs = 4326)

#Create area of interest for downloads ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create aoi for entire study areas
aoi <- st_bbox(pnts) %>% st_as_sfc() %>% st_as_sf()

#Convert to planar coordinates
aoi <- st_transform(aoi, crs = st_crs("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +type=crs"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Gather publicly available data --------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Download state shapes -----------------------------------------------------
states <- states(cb=T, resolution='500k') %>% 
  dplyr::filter(STUSPS %in% c("VA", "MD", "DE", "WV", "PA", "DC", "NJ")) %>% 
  st_transform(., crs = 4269)

#3.2 Download DEM --------------------------------------------------------------
dem <- get_elev_raster(aoi, z=14)

#3.3 Download aerial imagery ---------------------------------------------------
output_files <- get_tiles(pnts ,
    output_prefix = tempfile(),
    services = c("ortho"), 
    resolution = 10)
naip_all_sites <- raster::brick(output_files[["ortho"]][[1]])

#3.4 Delineate watersheds using NHD data ---------------------------------------
#Delineate Greensboro Watershed ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Gage 01491000 (https://waterdata.usgs.gov/monitoring-location/01491000)
green_gage_shp <- 
  tibble(lat  = 38.99719444, lon = -75.7858056) %>%
  st_as_sf(
    coords = c("lon", "lat"), 
    crs = 4326)

#Identify NHD reach 
start_comid <- discover_nhdplus_id(green_gage_shp, raindrop = T)
start_comid

#Snag flowline
g_flow_net <- navigate_nldi(list(featureSource = "comid", 
                                 featureID = start_comid$comid[1]), 
                            mode = "upstreamTributaries", 
                            distance_km = 1000)

#get nhdplus files
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(g_flow_net$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = 'download', 
                         flowline_only = FALSE,
                         return_data = TRUE, 
                         overwrite = TRUE)
#Identify catchment
g_shed <- sf::read_sf(subset_file, "CatchmentSP")
g_shed <- st_union(g_shed)

#Delineate Tuckahoe Watershed ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Gage 01491500 (https://waterdata.usgs.gov/monitoring-location/01491500)
tuck_gage_shp <- 
  tibble(lat  = 38.96680556, lon = -75.9430556) %>%
  st_as_sf(
    coords = c("lon", "lat"), 
    crs = 4326)

#Identify NHD reach 
start_comid <- discover_nhdplus_id(tuck_gage_shp, raindrop = T)
start_comid

#Snag flowline
t_flow_net <- navigate_nldi(list(featureSource = "comid", 
                                 featureID = start_comid$comid[1]), 
                            mode = "upstreamTributaries", 
                            distance_km = 1000)

#get nhdplus files
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(t_flow_net$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = 'download', 
                         flowline_only = FALSE,
                         return_data = TRUE, 
                         overwrite = TRUE)
#Identify catchment
t_shed <- sf::read_sf(subset_file, "CatchmentSP")
t_shed <- st_union(t_shed)

#3.5 Create wetland polygons using wbt workflow --------------------------------
#Create temp dir
temp_dir <- tempdir()

#write dem to temp file
writeRaster(dem, paste0(temp_dir,"\\dem.tif"), overwrite=T)

#Smooth DEM
wbt_fast_almost_gaussian_filter(
  input = "dem.tif",
  output = "dem_filter.tif",
  wd = temp_dir)

#Identify depressions
wbt_stochastic_depression_analysis(
  dem = "dem_filter.tif", 
  output = "depressions.tif", 
  rmse = 0.15, 
  range = 100, 
  wd = temp_dir)

#Reclass raster
wbt_reclass(
  input = "depressions.tif", 
  output = "reclass.tif", 
  reclass_vals = '0;0;0.80;1;0.80;1', 
  wd = temp_dir
)

#Identify "connected" inundated areas
wbt_clump(
  input = 'reclass.tif',
  output = 'groups.tif',
  diag = T, 
  zero_back = T,
  wd = temp_dir
)

#Create wetland polygons
wbt_raster_to_vector_polygons(
  input = "groups.tif", 
  output = "wetlands.shp",
  wd = temp_dir)

#read groups into R
wetlands <- st_read(paste0(temp_dir,"//wetlands.shp"))

#Subset wetlands to sites
wetlands <- wetlands[pnts,]

#plot for funzies
mapview(wetlands) + mapview(pnts)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Plot Figure ---------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Create regional location panel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
state_map <- states %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf(data = t_shed, bg="grey60") +
  geom_sf(data = g_shed, bg="grey60") +
  coord_sf(xlim=c(-77,-75), ylim = c(36.75, 40), clip="on")+
  theme_bw()

#4.2 Create panel of Greensboro watershed ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#4.3 Create panel of study landscape ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#4.4 Create panels of wetland complexes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Panel B: Wetlands--------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

