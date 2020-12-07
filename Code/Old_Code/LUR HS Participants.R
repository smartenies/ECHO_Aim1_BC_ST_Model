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
library(raster)
library(Hmisc)
library(ggplot2)
library(ggmap)
library(tidyverse)

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm_13 <- "+init=epsg:26913"

#' -----------------------------------------------------------------------------
#' 1) Identify which grid cells contain an interested HS participant
#'    NOTE: Must have access to the UCDenver server to run this section of the
#'    code
#' -----------------------------------------------------------------------------

load("./Processed_Data/Grid 250 m.RData")

#' Read in geocoded participant addresses
p_path <- "P:/LEAD/Outside-Users/Martenies/Shared Files"
hs_add <- readOGR(dsn = p_path, layer = "HealthyStartII_Geocoded_Final")

#' Idenitfy interested participants
hs_list <- read_excel(path=paste(p_path, "/ECHO Aim 1 Participant Contact Tracker 20180403.xlsx", sep=""),
                      skip=1, sheet=1)

PID_list <- hs_list[which(hs_list$Interested=="Yes"), "PID"]

hs_int_add <- hs_add[which(hs_add$PID %in% PID_list$PID),]
nrow(hs_int_add)

#' Project points to the same CRS as the grid
hs_int_aea <- spTransform(hs_int_add, CRS(albers))
plot(hs_int_aea, main="Interested HS participants")

#' Remove participant information
hs_int_aea@data <- hs_int_aea@data[,c(13, 7, 8)]

save(hs_int_aea, file="./Processed_Data/Interested participants all.RData")
