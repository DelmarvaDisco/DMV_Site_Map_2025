#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Site Map
# Coder: K Wardinski 
# Date: 10/1/2023
# Purpose: Create AOI plots of individual wetlands  
# Notes: Based on DelmarvaDisco Github Site_Info script created by Nate Jones
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup workspace -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list=ls())

#load relevant packages
library(mapview)
library(readxl)
library(sf)
library(tidyverse)
library(ggspatial)
library(prettymapr)
library(devEMF)

#load core data directory
syn<-read_xlsx('data/DISCO_core_data.xlsx', sheet = 'Synoptic Sampling Sites')
jl<-read_xlsx('data/DISCO_core_data.xlsx', sheet = 'Jackson Lane Catchment')

#load wetland shape from DMV_spatial analysis
wetlands<-st_read('data/wetlands.shp')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Prep data ------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

syn<-syn %>% 
  dplyr::select(
    Site_Id = `Site ID`, 
    Wetland_Name = 'Wetland Name', 
    Latitude = Latitude, 
    Longitude = Longitude, 
    Property = Property) %>% 
    mutate(project_component = 'synoptic')

#Jackson Lane
jl<-jl %>% 
  dplyr::select(
    Site_Id  = SiteID, 
    Wetland_Name = 'Wetland Name', 
    Latitude = Latitude, 
    Longitude = Longitude) %>% 
  mutate(
    Property = 'Jackson Lane',
    project_component = 'experimental catchment')

#combine
df<-bind_rows(syn, jl)

#create simple feature
df<-st_as_sf(df, coords = c("Longitude", "Latitude"), crs = '+proj=longlat +datum=WGS84 +no_defs')

#Add coordinates to simple feature's tibble
df<-df %>% 
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2],
  )

#reproject to UTM Zone 17N (https://spatialreference.org/)
df<-st_transform(df, crs = '+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ' )

#plot
mapview(wetlands) + mapview(df)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Alternate figure of individual wetland sites ------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#3.1 Create AOI for soil core sites ------------------------------------------------------


#Soil cores
complex_QB <- df %>% 
  filter(str_detect(Site_Id, "QB")) %>% 
  filter(!str_detect(Site_Id, "UW2"))
mapview(complex_QB)

complex_ND <- df %>% 
  filter(str_detect(Site_Id, "ND")) %>% 
  filter(!str_detect(Site_Id, "UW2")) %>% 
  filter(!str_detect(Site_Id, "UW3"))
mapview(complex_ND)

complex_TB <- df %>% 
  filter(str_detect(Site_Id, "TB")) %>% 
  filter(!str_detect(Site_Id, "UW2")) %>% 
  filter(!str_detect(Site_Id, "UW3"))
mapview(complex_TB)

complex_DB <- df %>% 
  filter(str_detect(Site_Id, "DB")) %>% 
  filter(!str_detect(Site_Id, "UW2")) %>% 
  filter(!str_detect(Site_Id, "UW3"))
mapview(complex_DB)

## 3.2 Create boxes around each complex ------------------
box_fun<-function(xy){
  #Define bounding box
  box <- xy %>% 
    #Define box around points
    st_bbox() %>% st_as_sfc() %>% st_as_sf(crs = '+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ') %>% 
    #Define Centroid of bbox
    st_centroid() %>% 
    #Add buffer to centroid
    st_buffer(50) %>% 
    #Define bbox of buffer
    st_bbox() %>% st_as_sfc() %>% st_as_sf(crs = '+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ')
  
  #export box
  box
}


## 3.3 Define bbox and plot for each wetland complex -------------------

# ESRI World Imagery tile URL for background in plots
esri_tile_url <- "https://services.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/tile/${z}/${y}/${x}.jpg"


## QB ##
box_QB <- box_fun(complex_QB)

mapview(box_QB) + mapview(complex_QB) + mapview(wetlands)

complex_map_QB <-ggplot() +
  geom_sf(data=box_QB, bg=NA, col=NA) +
  annotation_map_tile(type = esri_tile_url) +
  geom_sf(data = wetlands, bg="darkblue", alpha=0.5) +
  geom_sf(data = complex_QB, bg="darkblue", alpha=0.7) +
  coord_sf(
    xlim = c(st_coordinates(box_QB)[,1] %>% min(., na.rm = T )+0.00025, st_coordinates(box_QB)[,1] %>% max(., na.rm = T)-0.00025), 
    ylim = c(st_coordinates(box_QB)[,2] %>% min(., na.rm = T )+0.0003, st_coordinates(box_QB)[,2] %>% max(., na.rm = T)- 0.0003)) +
  annotation_scale(line_width = 1, height = unit(0.35, "cm"), 
                   text_cex = 2,text_col = "transparent",
                   pad_x = unit(6, "cm"),pad_y = unit(0.7, "cm")) +
  theme_void()


emf(file = "docs/QB_Map",width = 6, height = 4, bg = "transparent")
complex_map_QB
dev.off()

## ND ##
box_ND <- box_fun(complex_ND)
mapview(box_ND) + mapview(complex_ND) + mapview(wetlands) 

complex_map_ND <-ggplot() +
  geom_sf(data=box_ND, bg=NA, col=NA) +
  annotation_map_tile(type = esri_tile_url) +
  geom_sf(data = wetlands, bg="darkblue", alpha=0.5) +
  geom_sf(data = complex_ND, bg="darkblue", alpha=0.7) +
  coord_sf(
    xlim = c(st_coordinates(box_ND)[,1] %>% min(., na.rm = T )+0.00025, st_coordinates(box_ND)[,1] %>% max(., na.rm = T)-0.00025), 
    ylim = c(st_coordinates(box_ND)[,2] %>% min(., na.rm = T )+0.0003, st_coordinates(box_ND)[,2] %>% max(., na.rm = T)- 0.0003)) +
  annotation_scale(line_width = 1, height = unit(0.35, "cm"), 
                   text_cex = 2,text_col = "transparent",
                   pad_x = unit(6, "cm"),pad_y = unit(0.7, "cm")) +
  theme_void()


emf(file = "docs/ND_Map",width = 6, height = 4, bg = "transparent")
complex_map_ND
dev.off()

## TB ##
box_TB <- box_fun(complex_TB)
mapview(box_TB) + mapview(complex_TB) + mapview(wetlands)

complex_map_TB <-ggplot() +
  geom_sf(data=box_TB, bg=NA, col=NA) +
  annotation_map_tile(type = esri_tile_url) +
  geom_sf(data = wetlands, bg="darkblue", alpha=0.5) +
  geom_sf(data = complex_TB, bg="darkblue", alpha=0.7) +
  coord_sf(
    xlim = c(st_coordinates(box_TB)[,1] %>% min(., na.rm = T )+0.00025, st_coordinates(box_TB)[,1] %>% max(., na.rm = T)-0.00025), 
    ylim = c(st_coordinates(box_TB)[,2] %>% min(., na.rm = T )+0.0003, st_coordinates(box_TB)[,2] %>% max(., na.rm = T)- 0.0003)) +
  annotation_scale(line_width = 1, height = unit(0.35, "cm"), 
                   text_cex = 2,text_col = "transparent",
                   pad_x = unit(6, "cm"),pad_y = unit(0.7, "cm")) +
  theme_void()


emf(file = "docs/TB_Map",width = 6, height = 4, bg = "transparent")
complex_map_TB
dev.off()


## DB ##
box_DB <- box_fun(complex_DB)
mapview(box_DB) + mapview(complex_DB) + mapview(wetlands)

complex_map_DB <-ggplot() +
  geom_sf(data=box_DB, bg=NA, col=NA) +
  annotation_map_tile(type = esri_tile_url) +
  geom_sf(data = wetlands, bg="darkblue", alpha=0.5) +
  geom_sf(data = complex_DB, bg="darkblue", alpha=0.7) +
  coord_sf(
    xlim = c(st_coordinates(box_DB)[,1] %>% min(., na.rm = T )+0.00025, st_coordinates(box_DB)[,1] %>% max(., na.rm = T)-0.00025), 
    ylim = c(st_coordinates(box_DB)[,2] %>% min(., na.rm = T )+0.0003, st_coordinates(box_DB)[,2] %>% max(., na.rm = T)- 0.0003)) +
  annotation_scale(line_width = 1, height = unit(0.35, "cm"), 
                   text_cex = 2,text_col = "transparent",
                   pad_x = unit(6, "cm"),pad_y = unit(0.7, "cm")) +
  theme_void()

emf(file = "docs/DB_Map",width = 6, height = 4, bg = "transparent")
complex_map_DB
dev.off()

