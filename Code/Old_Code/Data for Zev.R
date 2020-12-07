#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 Land Use Regression
#' 
#' Task: Datasets for Zev
#' 
#' Author: Sheena Martenies
#' Date Created: May 1, 2018
#' Contact: sheena.martenies@colostate.edu
#' -----------------------------------------------------------------------------


library(sp)
library(sf)
library(tidyverse)

load("./Processed_Data/Grid 250 m with attributes.RData")
grid_poly <- as(grid_250, "SpatialPolygonsDataFrame")
grid_sf <- st_as_sf(grid_poly)

load("./Processed_Data/Grid 250 m with monitors.RData")
grid_mon_poly <- as(grid_mon, "SpatialPolygonsDataFrame")
grid_mon_sf <- st_as_sf(grid_mon_poly)

grid_attributes <- left_join(grid_sf, st_set_geometry(grid_mon_sf, NULL),
                             by="grid_id")


# load("./Processed_Data/Grid 250 m with participants.RData")
# grid_hs_poly <- as(grid_hs, "SpatialPolygonsDataFrame")
# grid_hs_sf <- st_as_sf(grid_hs_poly)
# 
# grid_attributes <- left_join(grid_attributes, st_set_geometry(grid_hs_sf, NULL),
#                              by="grid_id")


sum(grid_attributes$mon)
# sum(grid_attributes$hs)

grid_attributes <- st_set_geometry(grid_attributes, NULL)

save(grid_attributes, file="./Processed_Data/grid_data_for_Zev.RData")
write.csv(grid_attributes, "./Processed_Data/grid_data_for_Zev.csv")
