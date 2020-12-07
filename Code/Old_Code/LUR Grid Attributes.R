#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 Land Use Regression
#' 
#' Task: Create a grid for the study domain and summarize land use and traffic 
#' variables for each grid cell
#' 
#' Author: Sheena Martenies
#' Date Created: May 1, 2018
#' Contact: sheena.martenies@colostate.edu
#' -----------------------------------------------------------------------------

#' Load required libraries
library(sf)
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(ggmap)
library(tidyverse)

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm_13 <- "+init=epsg:26913"

#' read in grid data 
grid_250 <- st_read(here::here("Data", "Grid_250_m_AEA.csv"),
                    stringsAsFactors = F, wkt = "WKT", crs = albers) 

#' 1 km buffer around the grid
grid_bound <- st_buffer(st_union(grid_250), dist = 1000) %>% 
  st_transform(ll_wgs84)

#' -----------------------------------------------------------------------------
#' 1) Summarize land characteristics and traffic exposures for the 250 m grid
#' 
#' 1A) Extract raster values using sp polygons
#' -----------------------------------------------------------------------------

grid_spdf <- as(grid_250, "Spatial")

#' Average % tree cover and % impervious surface
tree_cover <- raster(here::here("Data", "Tree_Cover_AEA.tif"))

tree_cover_sp <- raster::extract(tree_cover, grid_spdf, fun=mean, sp=T)
colnames(tree_cover_sp@data)[ncol(tree_cover_sp@data)] <- "tree_cover"
grid_spdf <- merge(grid_spdf, tree_cover_sp, by="grid_id")

impervious <- raster(here::here("Data", "Impervious_AEA.tif"))

impervious_sp <- raster::extract(impervious, grid_spdf, fun=mean, sp=T)
colnames(impervious_sp@data)[ncol(impervious_sp@data)] <- "impervious"
grid_spdf <- merge(grid_spdf, impervious_sp, by="grid_id")

#' clean up environment
rm(tree_cover, tree_cover_sp, impervious, impervious_sp)

#' Most frequent land use category
land_use <- raster(here::here("Data", "Land_Use_AEA.tif"))

grid_spdf$land_use <- unlist(raster::extract(land_use, grid_spdf, fun=modal, sp=F))
grid_spdf$land_use <- as.vector(grid_spdf$land_use[,1])

rm(land_use, land_use_sp)

#' -----------------------------------------------------------------------------
#' 1) Summarize land characteristics and traffic exposures for the 250 m grid
#' 
#' 1B) Road segment lengths using sf 
#' -----------------------------------------------------------------------------

grid_sf <- st_as_sf(grid_spdf)

#' Highway lengths
highways <- st_read(here::here("Data", "Highways_AEA.csv"),
                    stringsAsFactors = F, wkt = "WKT", crs = albers) 
plot(st_geometry(highways))

#' sf and dplyr 
highways_df <- st_intersection(grid_sf, highways) %>%
  group_by(grid_id) %>%
  summarise %>%
  mutate(highway_m = unclass(st_length(.))) %>%
  st_set_geometry(NULL)

grid_sf <- left_join(grid_sf, highways_df, by="grid_id")
grid_sf$highway_m <- ifelse(is.na(grid_sf$highway_m), 0, grid_sf$highway_m)

#' Check
sum(unclass(st_length(highways)))
sum(grid_sf$highway_m)

rm(highways, highways_df)

#' Major road lengths
major <- st_read(here::here("Data", "Major_Roads_AEA.csv"),
                    stringsAsFactors = F, wkt = "WKT", crs = albers) 
plot(st_geometry(major))

#' sf and dplyr 
major_df <- st_intersection(grid_sf, major) %>%
  group_by(grid_id) %>%
  summarise %>%
  mutate(major_m = unclass(st_length(.))) %>%
  st_set_geometry(NULL)

grid_sf <- left_join(grid_sf, major_df, by="grid_id")
grid_sf$major_m <- ifelse(is.na(grid_sf$major_m), 0, grid_sf$major_m)

#' Check
sum(unclass(st_length(major)))
sum(grid_sf$major_m)

rm(major, major_df)

#' traffic density variable: weighted sum of road lengths
#' Current weights are 1, 0.5, and 0.25 for highway, major, and local, respectively

grid_sf$road_km_wt <- (grid_sf$highway_m * 1) + (grid_sf$major_m * 0.5)

#' Sum of AADT across road links within the grid
nhpms_aadt <- st_read(here::here("Data", "NHPMS_AADT_AEA.csv"),
                      stringsAsFactors = F, wkt = "WKT", crs = albers) 
plot(st_geometry(nhpms_aadt))

#' sf and dplyr 
aadt_df <- st_intersection(grid_sf, nhpms_aadt) %>%
  group_by(grid_id) %>%
  summarise(aadt = sum(aadt)) %>%
  st_set_geometry(NULL)

grid_sf <- left_join(grid_sf, aadt_df, by="grid_id")
grid_sf$aadt <- ifelse(is.na(grid_sf$aadt), 0, grid_sf$aadt)

rm(nhpms_aadt, aadt_df)

#' -----------------------------------------------------------------------------
#' 4) Summarize census data using grid cell centroids
#' -----------------------------------------------------------------------------

#' Get block groups in study area
#' Population and housing data:
pop_housing <- st_read(here::here("Data", "Population_and_Housing_AEA.csv"),
                       stringsAsFactors = F, wkt = "WKT", crs = albers) %>% 
  select(GEOID, pop_density, housing_density)

#' Join based on grid centroids
grid_cent <- st_centroid(grid_250) %>%
  select("grid_id")
grid_cent <- st_join(grid_cent, pop_housing) %>%
  st_set_geometry(NULL)

grid_sf <- left_join(grid_sf, grid_cent, by="grid_id")

st_write(grid_sf, here::here("Data", "Grid_250_m_Attributes_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
