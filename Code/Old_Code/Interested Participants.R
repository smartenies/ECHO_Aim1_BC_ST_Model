#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 Land Use Regression
#' 
#' Task: Get participant information for Grace
#' 
#' Author: Sheena Martenies
#' Date Created: April 8, 2018
#' Contact: sheena.martenies@colostate.edu
#' -----------------------------------------------------------------------------

#' Load required libraries
library(sp)
library(gstat)
library(rgeos)
library(rgdal)
library(raster)
library(maptools)
library(Hmisc)
library(ggplot2)
library(ggmap)
library(readxl)
library(writexl)

#' -----------------------------------------------------------------------------
#' Read in geocoded participant addresses
p_path <- "P:/LEAD/Outside-Users/Martenies/Shared Files"
hs_add <- readOGR(dsn = p_path, layer = "HealthyStartII_Geocoded_Final")

#' Idenitfy interested participants
hs_list <- read_excel(path=paste(p_path, "/ECHO Aim 1 Participant Contact Tracker 20180403.xlsx", sep=""),
                      skip=1, sheet=1)

PID_list <- hs_list[which(hs_list$Interested=="Yes"),]

hs_int_add <- hs_add[which(hs_add$PID %in% PID_list$PID),]
nrow(hs_int_add)

#' Identify which interested participants live within the grid
load("./Processed_Data/LUR Boundary.RData")

#' Project points to the same CRS as the grid
hs_int_aea <- spTransform(hs_int_add, crs(bound_1km))
plot(hs_int_aea, main="Interested HS participants")

hs_int_aea <- hs_int_aea[bound_1km,]

plot(bound_1km)
points(hs_int_aea)

#' Merge participant info with addresses
hs_data <- merge(PID_list, 
                 as.data.frame(hs_int_aea[,c("PID", "Address1", "Apt", 
                                             "City", "State", "Zip")]),
                 by="PID")
hs_data <- hs_data[,-c((ncol(hs_data)-1), ncol(hs_data))]

#' Get participant names
load("P:/LEAD/Outside-Users/Martenies/Research/2018_ECHO_CEI/Data/HSI.RData")
hs_names <- unique(hsI[,c("pid", "full_name")])
rm(hsI)

hs_names$PID <- hs_names$pid

hs_data <- merge(hs_names[,c("PID", "full_name")], hs_data, by="PID", all.y=T)

write_xlsx(hs_data, path="./HS_Data/participant_info.xlsx")
