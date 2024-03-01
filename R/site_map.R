#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Site Map
# Coder: Nate Jones
# Date: 11/9/2023
# Purpose: Create publication quality map for wetland sites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 etup workspace -----------------------------------------------------------
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
library(patchwork)

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
aoi <- st_bbox(st_buffer(pnts, 0.005)) %>% st_as_sfc() %>% st_as_sf()

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
output_files <- get_tiles(aoi ,
    output_prefix = tempfile(),
    services = c("ortho"), 
    resolution = 10)
naip_all_sites <- raster::brick(output_files[["ortho"]][[1]])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Download NWI data ---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MD Wetlands
download.file(
  url = "https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/MD_shapefile_wetlands.zip", 
  destfile = paste0(tempdir(), "\\MD_shapefile_wetlands.zip"))
unzip(
  zipfile = paste0(tempdir(), "\\MD_shapefile_wetlands.zip"), 
  exdir = tempdir())
md_nwi<-st_read(paste0(tempdir(), "\\MD_shapefile_wetlands\\MD_Wetlands.shp"))

#DE Wetlands
download.file(
  url = "https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/DE_shapefile_wetlands.zip", 
  destfile = paste0(tempdir(), "\\DE_shapefile_wetlands.zip"))
unzip(
  zipfile = paste0(tempdir(), "\\DE_shapefile_wetlands.zip"), 
  exdir = tempdir())
de_nwi<-st_read(paste0(tempdir(), "\\DE_shapefile_wetlands\\DE_Wetlands.shp"))

#combine wetland files
nwi <- bind_rows(md_nwi, de_nwi)
nwi <- st_transform(nwi, crs = 4269)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Hydrologic Analyses -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.1 Delineate Upper Choptank Watershed ----------------------------------------
#Define upper Choptank watershed outlet
outlet <- 
  tibble(lat  =  38.806482, lon = -75.908928) %>%
  st_as_sf(
    coords = c("lon", "lat"), 
    crs = 4326)

#Identify NHD reach 
start_comid <- discover_nhdplus_id(outlet, raindrop = T)
start_comid

#Snag flowline
flow_net <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid$comid[1]), 
                            mode = "upstreamTributaries", 
                            distance_km = 1000)

#get nhdplus files
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flow_net$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = 'download', 
                         flowline_only = FALSE,
                         return_data = TRUE, 
                         overwrite = TRUE)
#Identify catchment
shed <- sf::read_sf(subset_file, "CatchmentSP")
shed <- st_union(shed)

#Isolate flownet
flow_net <- flow_net$UT_flowlines

#5.2 NWI edit ------------------------------------------------------------------
#Clip to shed area
nwi <- nwi[shed, ]

#limit to freshwater forested wetlands
nwi <- nwi %>% filter(WETLAND_TY == 'Freshwater Forested/Shrub Wetland')

#5.3 Wetland Delineation -------------------------------------------------------
#write dem to temp file
writeRaster(dem, paste0(tempdir(),"\\dem.tif"), overwrite=T)

#Smooth DEM
wbt_fast_almost_gaussian_filter(
  input = "dem.tif",
  output = "dem_filter.tif",
  wd = tempdir())

#Identify depressions
wbt_stochastic_depression_analysis(
  dem = "dem_filter.tif", 
  output = "depressions.tif", 
  rmse = 0.15, 
  range = 100, 
  wd = tempdir())

#Reclass raster
wbt_reclass(
  input = "depressions.tif", 
  output = "reclass.tif", 
  reclass_vals = '0;0;0.80;1;0.80;1', 
  wd = tempdir()
)

#Identify "connected" inundated areas
wbt_clump(
  input = 'reclass.tif',
  output = 'groups.tif',
  diag = T, 
  zero_back = T,
  wd = tempdir()
)

#Create wetland polygons
wbt_raster_to_vector_polygons(
  input = "groups.tif", 
  output = "wetlands.shp",
  wd = tempdir())

#read groups into R
wetlands <- st_read(paste0(tempdir(),"//wetlands.shp"))

#Subset wetlands to sites
wetland_sites <- wetlands[pnts,]

#plot for funzies
mapview(wetlands) + mapview(pnts)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.0 Plot Figure ---------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.1 Create regional location panel --------------------------------------------
state_map <- states %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf(data = shed, bg="grey60") +
  coord_sf(xlim=c(-77,-75), ylim = c(36.75, 40), clip="on")+
  theme_bw()
state_map

#6.2 Create panel of Greensboro watershed --------------------------------------
shed_map <- ggplot() + 
  geom_sf(data = shed,col="grey20", lwd=1.2) +
  geom_sf(data = nwi, col="darkgreen", alpha=0.3) +
  geom_sf(data = flow_net, col='darkblue', lwd=1.05, alpha = 0.7) + 
  geom_sf(data = shed,col="grey20", bg=NA, lwd=1.2) +
  geom_sf(data = aoi, lwd=1.3, col='darkred', bg = NA) +
  theme_void()
shed_map

#6.3 Create panel of study landscape -------------------------------------------
#Use this approach to print NAIP imagery (https://medium.com/@tobias.stalder.geo/plot-rgb-satellite-imagery-in-true-color-with-ggplot2-in-r-10bdb0e4dd1f)
#Convert NAIP image to dataframe
df <- as.data.frame(naip_all_sites, xy=TRUE) %>% 
  as_tibble() %>% 
  filter(lyr.3!=0)

#Plot 
aoi_map <- ggplot() +                    
    geom_sf(data = aoi)+
    geom_raster(
      data = df,
      aes(x = x, y =y),
      fill = rgb(r = df$lyr.1, g = df$lyr.2, b = df$lyr.3, maxColorValue = 1),
      show.legend = FALSE) +
    geom_sf(data = wetlands, col = NA, bg="darkblue", alpha = .5) +
    scale_fill_identity()+ 
  theme_void()
aoi_map

# 6.4 Plot wetland complexes ---------------------------------------------------
#Define wetland complexs
complex_1 <- pnts %>% filter(Property == 'Baltimore Corner North' )
mapview(complex_1)
complex_2 <- pnts %>% filter(Property == 'Baltimore Corner South'|Property == "Baltimore Corner")
mapview(complex_2)
complex_3 <- pnts %>% filter(`Site ID` %in% c('ND', 'BD', 'TS', 'DK'))
mapview(complex_3)
complex_4 <- pnts %>% filter(`Site ID` %in% c('DB', 'TA', 'TB', 'FN'))
mapview(complex_4)

#Create boxes around each complex
box_fun<-function(xy){
  #Define bounding box
  box <- xy %>% st_bbox() 
  x <- (box$xmin + box$xmax)/2
  y <- (box$ymin + box$ymax)/2
  
  #Define new box
  data.frame(
      x = c(x+0.00045, x+0.00045, x-0.00045, x-0.00045),
      y = c(y+0.00045, y-0.00045, y+0.00045, y-0.00045)) %>% 
    as.matrix() %>% 
    st_polygonize()
}

complex_2 %>% st_bbox() %>% st_as_sfc() %>% st_as_sf() %>% st_centroid() %>% mapview()

#6.5 Create panels of wetland complexes ----------------------------------------
state_map + shed_map + aoi_map





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Code Graveyard ----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Watershed Delineation -----------------------------------------------------
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


