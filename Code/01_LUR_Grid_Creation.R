#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Create a 250 m grid to cover the study domain
#' Author: Sheena Martenies
#' Date Created: October 16, 2018
#' 
#' Contact: smarte4@illinois.edu
#' -----------------------------------------------------------------------------

#' Load required libraries
library(sf)
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(ggmap)
library(tidyverse)

#' Coordinate reference systems
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm_13 <- "+init=epsg:26913"

#' -----------------------------------------------------------------------------
#  Create a "boundary" for the study area ----
#'    Nominally, the study boundary is the 470 belt that surrounds the Denver
#'    metro area. Here, I'm creating a boundary by first identifying some points
#'    along the highway (using Google Maps), drawing a bounding box and adding
#'    a 1 km buffer to account for traffic gradient emissions. This boundary
#'    will be used to process all of the other spatial inputs (e.g., lang use
#'    and traffic metrics)
#' -----------------------------------------------------------------------------

#' Coordinates based on Google Maps
pts_df <- data.frame(lon=c(-105.194150, -105.086157, -104.785862,
                           -104.715897, -105.007800),
                     lat=c(39.712174, 39.553840, 39.548127,
                           39.740367, 39.984862))
pts <-  st_as_sf(pts_df, coords=c("lon", "lat"), crs=ll_wgs84)
plot(st_geometry(pts), main="Boundary Points: latlong")

#' Project to Albers equal area (used by the Land Use dataset)
#' Want to preserve area so we can calculate area-based metrics such as 
#' population density
pts_aea <- st_transform(pts, crs=albers)
plot(st_geometry(pts_aea), main="Boundary Points: Albers Equal Area")

#' Get boundary around these points
pts_bound <- st_make_grid(pts_aea, n = 1)
plot(st_geometry(pts_bound), main="bounding box", border="red", col=NA)
plot(st_geometry(pts_aea), col="blue", add=T)

#' Add a 1 km buffer around the bounding box
#' Albers equal area uses units of meters
bound_1km <- st_buffer(pts_bound, dist = 1000)
plot(st_geometry(bound_1km), main="extended bounding box")
plot(st_geometry(pts_bound), col=NA, border="red", add=T)
plot(st_geometry(pts_aea), col="blue", add=T)

#' Create the 250 m grid using the boundary box
grid_250 <- st_make_grid(bound_1km, cellsize = c(250, 250))
grid_250 <- st_sf(grid_250)
grid_250$grid_id <- seq(1:nrow(grid_250))

glimpse(grid_250)
plot(st_geometry(grid_250))
nrow(grid_250)

#' Save grid
st_write(grid_250, here::here("Data", "Grid_250_m_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

grid_250_sub <- grid_250[15000:20000,]
plot(st_geometry(grid_250_sub), border = "red", add = T)

#' Save subgrid for testing spatial code later
st_write(grid_250_sub, here::here("Data", "Grid_250_sub_m_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' Save grid as a shapefile
grid_250_ll <- st_transform(grid_250, crs = ll_wgs84)
plot(st_geometry(grid_250_ll))

st_write(grid_250_ll, here::here("Data", "Grid_250_m.shp"),
         delete_dsn = T)

