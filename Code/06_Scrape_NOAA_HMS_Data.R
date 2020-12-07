#' -----------------------------------------------------------------------------
#' Date created: September 11, 2020
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description: Download NOAA Hazard Mapping System shapefiles for smoke plumes 
#' 
#' The Hazard Mapping System (from NOAA) makes publicly available all of the
#' shapefiles they generate each day at this website: 
#' https://www.ospo.noaa.gov/Products/land/hms.html
#' Information on how these data are collected is found here:
#' https://www.ospo.noaa.gov/Products/land/hms.html
#' 
#' These files are made available as .zip files, so we'll need to unzip them
#' This script downloads them, unzips them, and copies them to the right folder.
#' Shapefiles are a file format developed by Esri (ArcGIS) and  actually consist 
#' of 4 files: .dbf, .prj, .shp, and .shx
#' 
#' The shapefiles from NOAA include polygons outlining the smoke plumes. They 
#' include all of the coordinate reference information as well. Note that these
#' files cover the entire United States
#' 
#' Update 11.24.20: transferring data to my UIUC computer and updating the 
#' code to reflect the new file storage system on the NOAA site
#' ----------------------------------------------------------------------------

#' Load required libraries
library(tidyverse)
library(readxl)
library(writexl)
library(httr)
library(sf)
library(rvest)

#' Some coordinate reference systems
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm_13N <- "+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' Create a folder to keep these data
if(!dir.exists(here::here("Secondary_Data/HMS_Smoke"))) dir.create(here::here("Secondary_Data/HMS_Smoke"))

#' -----------------------------------------------------------------------------
#' 1) Download archived data from the NOAA website
#' Note that "today's" data might not be "finalized", so the date sequence is 
#' all dates between the start date and "yesterday"

start_date <- as.Date("2010-01-01")
end_date <- Sys.Date() - 1
dates <- seq(start_date, end_date, by="1 day")
dates <- format(dates, "%m%d%Y")

#' Temp folder to hold .zip files
if(!dir.exists(here::here("Data/Temp"))) dir.create(here::here("Data/Temp"))

#' loop through dates
for (i in 1:length(dates)) {
  
  m <- substr(dates[i],1,2)
  d <- substr(dates[i],3,4)
  y <- substr(dates[i],5,8)
  
  shp_name <- paste0("hms_smoke", y, m, d)
  
  hms_url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/",
                    y, "/", m, "/hms_smoke", y, m, d, ".zip")
  
  #' Download from the NOAA website
  download.file(hms_url, destfile = here::here("Data/Temp", "hms_temp.zip"))
  
  #' Unzip the .zip and move all of the files to the hms_smoke folder 
  unzip(here::here("Data/Temp", "hms_temp.zip"), 
        exdir = here::here("Secondary_Data", "HMS_Smoke"))
  file.remove(here::here("Data/Temp", "hms_temp.zip"))
}



