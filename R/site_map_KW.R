#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Site Map
# Coder: Nate Jones
# Date: 11/9/2023
# Purpose: Create publication quality map for wetland sites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup workspace -----------------------------------------------------------
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
wetland_data<- read_csv('data//wetland_info_table.csv')

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

#2/4/25 need to find another shape file to download

states <- states(cb=T, resolution='500k',class="sf",year=2022) %>% 
  dplyr::filter(STUSPS %in% c("VA", "MD", "DE", "WV", "PA", "DC", "NJ")) %>% 
  st_transform(., crs = 4269)

#3.2 Download DEM --------------------------------------------------------------
dem <- get_elev_raster(aoi, z=14)

#3.3 Download aerial imagery ---------------------------------------------------
dem_aoi <- st_bbox(dem) %>% st_as_sfc() %>% st_as_sf()
output_files <- get_tiles(dem_aoi ,
    output_prefix = tempfile(),
    services = c("ortho"), 
    resolution = 10)
naip_all_sites <- raster::brick(output_files[["ortho"]][[1]])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Download NWI data ---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MD Wetlands
#download.file(
#  url = "https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/MD_geodatabase_wetlands.zip", 
#  destfile = paste0(tempdir(), "\\MD_Wetlands"))
#unzip(
#  zipfile = paste0(tempdir(), "\\MD_Wetlands"), 
#  exdir = tempdir())
#md_nwi<-st_read(paste0(tempdir(), "\\MD_shapefile_wetlands\\MD_Wetlands.shp")) #change command to read geodatabase files

#updated 2/4/25 to get geodatabase instead of shape file
url <- "https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/MD_geodatabase_wetlands.zip"
temp_file <- tempfile(fileext=".zip")
download.file(url,temp_file,mode="wb")
unzip(temp_file,exdir=tempdir())
gbd_path <- file.path(tempdir(),"MD_geodatabase_wetlands.gdb")
layers <- st_layers(gbd_path)
layer_name <- layers$name[3]
md_nwi <- st_read(dsn=gbd_path,layer=layer_name)

#DE Wetlands
#download.file(
#  url = "https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/DE_shapefile_wetlands.zip", 
#  destfile = paste0(tempdir(), "\\DE_Wetlands"))
#unzip(
#  zipfile = paste0(tempdir(), "\\DE_Wetlands"), 
#  exdir = tempdir())
#de_nwi<-st_read(paste0(tempdir(), "\\DE_shapefile_wetlands\\DE_Wetlands.shp"))

#updated 2/4/25 to get geodatabase instead of shape file
url2 <- "https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/DE_geodatabase_wetlands.zip"
temp_file2 <- tempfile(fileext=".zip")
download.file(url2,temp_file2,mode="wb")
unzip(temp_file2,exdir=tempdir())
gbd_path2 <- file.path(tempdir(),"DE_geodatabase_wetlands.gdb")
layers <- st_layers(gbd_path2)
layer_name2 <- layers$name[4]
de_nwi <- st_read(dsn=gbd_path2,layer=layer_name2)

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
#6.1 Create AOI for plots ------------------------------------------------------
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
  box <- xy %>% 
    #Define box around points
    st_bbox() %>% st_as_sfc() %>% st_as_sf() %>% 
    #Define Centroid of bbox
    st_centroid() %>% 
    #Add buffer to centroid
    st_buffer(0.003) %>% 
    #Define bbox of buffer
    st_bbox() %>% st_as_sfc() %>% st_as_sf()
  
  #export box
  box
}

#Define bbox for each wetland complex
box_1 <- box_fun(complex_1)
mapview(box_1) + mapview(complex_1)  

box_2 <- box_fun(complex_2)
mapview(box_2) + mapview(complex_2)  

box_3 <- box_fun(complex_3)
mapview(box_3) + mapview(complex_3)  

box_4 <- box_fun(complex_4)
mapview(box_4) + mapview(complex_4)  

#6.2 Create regional location panel --------------------------------------------
state_map <- states %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf(data = shed, bg="grey60") +
  coord_sf(xlim=c(-77,-75), ylim = c(36.75, 40), clip="on")+
  theme_bw() + 
  theme(axis.text = element_text(size = 8)) +
  scale_x_continuous(breaks=c(-77,-75)) +
  scale_y_continuous(breaks = c(37, 38, 39, 40))
state_map

#6.3 Create Choptank Watershed Map ---------------------------------------------
shed_map <- ggplot() + 
  geom_sf(data = shed,col="grey20", lwd=0.1) +
  geom_sf(data = nwi, bg="darkgreen", lwd=NA) +
  geom_sf(data = flow_net, col='darkblue', lwd=0.3, alpha = 0.7) + 
  geom_sf(data = shed,col="grey20", bg=NA, lwd=0.5) +
  geom_sf(data = aoi, lwd=1.3, col='darkred', bg = NA) +
  theme_void()
shed_map

#6.4 Create panel of study landscape -------------------------------------------
#Use this approach to print NAIP imagery (https://medium.com/@tobias.stalder.geo/plot-rgb-satellite-imagery-in-true-color-with-ggplot2-in-r-10bdb0e4dd1f)
#Convert NAIP image to dataframe
df <- as.data.frame(naip_all_sites, xy=TRUE) %>% 
  as_tibble() %>% 
  filter(lyr.3!=0) %>% 
  filter(x >= st_bbox(wetlands)$xmin) %>%
  filter(x <= st_bbox(wetlands)$xmax) %>% 
  filter(y >= st_bbox(wetlands)$ymin) %>%
  filter(y <= st_bbox(wetlands)$ymax)

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
    geom_sf(data = box_1, col="darkred", bg=NA, lwd=1.2) +
    geom_sf(data = box_2, col="darkred", bg=NA, lwd=1.2) +
    geom_sf(data = box_3, col="darkred", bg=NA, lwd=1.2) +
    geom_sf(data = box_4, col="darkred", bg=NA, lwd=1.2) +
    theme_void()
aoi_map

# 6.5 Plot wetland complexes ---------------------------------------------------
#Crop DEM for each complex
dem_1 <- crop(dem, box_1) %>% rasterToPoints() %>% as_tibble()
  colnames(dem_1) <- c("x", "y", "z")
dem_2 <- crop(dem, box_2) %>% rasterToPoints() %>% as_tibble()
  colnames(dem_2) <- c("x", "y", "z")
dem_3 <- crop(dem, box_3) %>% rasterToPoints() %>% as_tibble()
  colnames(dem_3) <- c("x", "y", "z")
dem_4 <- crop(dem, box_4) %>% rasterToPoints() %>% as_tibble()
  colnames(dem_4) <- c("x", "y", "z")
  
#Crop wetlands for each complex
wetlands_1 <- wetlands[box_1,]
wetlands_2 <- wetlands[box_2,]
wetlands_3 <- wetlands[box_3,]
wetlands_4 <- wetlands[box_4,]

#Crop study sites for each complex
wetland_sites_1 <- wetland_sites[box_1,]
wetland_sites_2 <- wetland_sites[box_2,]
wetland_sites_3 <- wetland_sites[box_3,]
wetland_sites_4 <- wetland_sites[box_4,]


#create wetland complex maps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Complex map 1
complex_map_1 <-ggplot() +
  geom_sf(data=box_1, bg=NA, col=NA) +
  geom_raster(data = dem_1, aes(x,y,fill=z)) +
  scale_fill_gradientn(colours = hcl.colors(30, "grays"), guide='none') +
  geom_sf(data = wetlands_1, bg="darkblue", alpha=0.25) +
  geom_sf(data = wetland_sites_1, bg="darkblue", alpha=0.95) +
  coord_sf(
     xlim = c(st_coordinates(box_1)[,1] %>% min(., na.rm = T )+0.00025, st_coordinates(box_1)[,1] %>% max(., na.rm = T)-0.00025), 
     ylim = c(st_coordinates(box_1)[,2] %>% min(., na.rm = T )+0.0003, st_coordinates(box_1)[,2] %>% max(., na.rm = T)- 0.0003)) +
  theme_void()
complex_map_1
  
#Complex map 2
complex_map_2 <-ggplot() +
  geom_sf(data=box_2, bg=NA, col=NA) +
  geom_raster(data = dem_2, aes(x,y,fill=z)) +
  scale_fill_gradientn(colours = hcl.colors(30, "grays"), guide='none') +
  geom_sf(data = wetlands_2, bg="darkblue", alpha=0.25) +
  geom_sf(data = wetland_sites_2, bg="darkblue", alpha=0.95) +
  coord_sf(
    xlim = c(st_coordinates(box_2)[,1] %>% min(., na.rm = T )+0.00025, st_coordinates(box_2)[,1] %>% max(., na.rm = T)-0.00025), 
    ylim = c(st_coordinates(box_2)[,2] %>% min(., na.rm = T )+0.0003, st_coordinates(box_2)[,2] %>% max(., na.rm = T)- 0.0003)) +
  theme_void()
complex_map_2

#Complex map 3
complex_map_3 <-ggplot() +
  geom_sf(data=box_3, bg=NA, col=NA) +
  geom_raster(data = dem_3, aes(x,y,fill=z)) +
  scale_fill_gradientn(colours = hcl.colors(30, "grays"), guide='none') +
  geom_sf(data = wetlands_3, bg="darkblue", alpha=0.25) +
  geom_sf(data = wetland_sites_3, bg="darkblue", alpha=0.95) +
  coord_sf(
    xlim = c(st_coordinates(box_3)[,1] %>% min(., na.rm = T )+0.00025, st_coordinates(box_3)[,1] %>% max(., na.rm = T)-0.00025), 
    ylim = c(st_coordinates(box_3)[,2] %>% min(., na.rm = T )+0.0003, st_coordinates(box_3)[,2] %>% max(., na.rm = T)- 0.0003)) +
  theme_void()
complex_map_3

#Complex map 4
complex_map_4 <-ggplot() +
  geom_sf(data=box_4, bg=NA, col=NA) +
  geom_raster(data = dem_4, aes(x,y,fill=z)) +
  scale_fill_gradientn(colours = hcl.colors(30, "grays"), guide='none') +
  geom_sf(data = wetlands_4, bg="darkblue", alpha=0.25) +
  geom_sf(data = wetland_sites_4, bg="darkblue", alpha=0.95) +
  coord_sf(
    xlim = c(st_coordinates(box_4)[,1] %>% min(., na.rm = T )+0.00025, st_coordinates(box_4)[,1] %>% max(., na.rm = T)-0.00025), 
    ylim = c(st_coordinates(box_4)[,2] %>% min(., na.rm = T )+0.0003, st_coordinates(box_4)[,2] %>% max(., na.rm = T)- 0.0003)) +
  theme_void()
complex_map_4

# 6.6 Wetland info plot --------------------------------------------------------
#tidy data
wetland_data <- wetland_data %>% 
  dplyr::select(area_m2, perimeter_m, hand_m) %>% 
  drop_na()

#hydrograph
wetland_area_plot <- wetland_data %>% 
  #Start ggplot object
  ggplot(aes(x=area_m2 , y=perimeter_m)) + 
  #Add line data
  geom_point(pch=19, cex=3, col="grey20") +
  #Add best fit line
  geom_smooth(method='lm', formula= y~x, lty=2, lwd=1.1, col="grey30", se=F) +
  #Plot y-axis in log scale
  scale_x_log10() +
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 10), 
    axis.text  = element_text(size = 8)
  ) + 
  #Add labels
  xlab(expression("Wetland Area (m"^"2"*")")) + 
  ylab("Wetland Perimeter (m)") 
wetland_area_plot

wetland_hand_plot <- wetland_data %>% 
  #Start ggplot object
  ggplot(aes(x=hand_m , y=perimeter_m)) + 
  #Add line data
  geom_point(pch=19, cex=3, col="grey20") +
  #Add best fit line
  #geom_smooth(method='lm', formula= y~x, lty=2, lwd=1.1, col="grey30", se=F) +
  #Plot y-axis in log scale
  scale_x_log10() +
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 10), 
    axis.text  = element_text(size = 8)
  ) + 
  #Add labels
  xlab(expression("Wetland Area (m"^"2"*")")) + 
  ylab("Height Above Nearest Drainage (m)") 
wetland_hand_plot

#6.7 Create panels of wetland complexes ----------------------------------------
design <- "ABBCCCC
           #BBCCCC
           #BBCCCC
           #BBCCCC
           #DEHHH#
           #DEHHH#
           #FGHHH#
           #FGHHH#"
           
(state_map + shed_map + (aoi_map) + free(complex_map_1) + free(complex_map_2) + free(complex_map_3) + free(complex_map_4) + free(wetland_area_plot)) +
  plot_layout(design = design) +
  plot_annotation(tag_levels = c("a"), tag_suffix = ")") &
  theme(plot.tag = element_text(size = 10))

ggsave("docs/site_map.png", width = 7, height = 5.5, units = "in", dpi = 300)

