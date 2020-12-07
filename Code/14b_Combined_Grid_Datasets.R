#' =============================================================================
#' Project: ECHO LUR
#' Date created: February 19, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script combined the grid with the spatial covariates and spatiotemporal
#' covariates
#' 
#' Also saves an "archive" dataset with a date stamp in case we need to go back
#' to anything
#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggthemes)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

today <- Sys.Date()

#' -----------------------------------------------------------------------------
#' Read in each of the datasets and create one big data frame
#' -----------------------------------------------------------------------------

#' 250 m grid
locations_file_name <- "Grid_250_m_AEA.csv"

locations_sf <- read_csv(here::here("Data", locations_file_name)) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
  
#' Spatial covariates
sp_covariates_file_name <- paste0("Spatial_Covariates_", locations_file_name)
sp_covariates <- read_csv(here::here("Data", sp_covariates_file_name)) %>% 
  select(-WKT) 
  
#' Spatiotemporal covariates
st_covariates_file_name <- paste0("ST_Covariates_", locations_file_name)
st_covariates <- read_csv(here::here("Data", st_covariates_file_name)) %>% 
  select(-WKT)
  
#' Put it all together
all_data <- full_join(locations_sf, st_covariates, by = "grid_id") %>% 
  full_join(sp_covariates, by = c("grid_id"))
names(all_data)

unique(all_data$month_yr)

#' Write out data
#' Two .csv files: with and without date  

data_name <- "Combined_Grid_Data_AEA.csv"
archive_data_name <- paste0("Combined_Grid_Data_AEA_", today, ".csv")

st_write(all_data, here::here("Data", data_name),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
st_write(all_data, here::here("Data/Archived_Data", archive_data_name),
         layer_options = "GEOMETRY=AS_WKT")
