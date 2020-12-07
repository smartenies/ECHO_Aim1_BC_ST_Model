#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 Land Use Regression
#' 
#' Task: Create a spatial object that contains potenital community monitoring
#' locations and combine this with the monitoring locations
#' 
#' Author: Sheena Martenies
#' Date Created: April 23, 2018
#' Contact: sheena.martenies@colostate.edu
#' 
#' NOTE: in the interest of time, going to use "sp", but will work on converting
#' to sf for the full analysis
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
library(tidyverse)

simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm_13 <- "+init=epsg:26913"

#' -----------------------------------------------------------------------------
#' 1) Identify points for the study area
#' -----------------------------------------------------------------------------

#' Community Monitors
mon <- read_xlsx(path = "./Raw_Data/Community Monitor Sites.xlsx")
mon_df <- mon
coordinates(mon) <- c("lon", "lat")
crs(mon) <- ll_wgs84
mon <- SpatialPointsDataFrame(mon, data=mon_df)
summary(mon)
plot(mon, main="Community monitors")

#' ID the city each of these locations is in
cities <- readOGR(dsn="./Raw_Data", layer="Munibounds")
summary(cities)

cities <- spTransform(cities, CRS(ll_wgs84))

mon$city <- as.character(sp::over(mon, cities[,"first_city"])[,1])

#" ID the county of these locations
counties <- readOGR(dsn="./Raw_Data", layer="tl_2017_us_county")
summary(counties)

counties <- spTransform(counties, CRS(ll_wgs84))

mon$county <- as.character(sp::over(mon, counties[,"NAME"])[,1])

head(as.data.frame(mon@data))

#' Project to albers equal area
mon_aea <- spTransform(mon, CRS(albers))

#' Interested Participants
load("./Processed_Data/Interested participants all.RData")
load("./Processed_Data/LUR boundary.RData")
hs_int_aea$seq_id <- 100 + seq(1:nrow(hs_int_aea@data))

hs_int_aea <- hs_int_aea[bound_1km,]

#' Drop un-needed participants
drop <- c(114, 107, 104, 119)
hs_int_aea@data$keep <- ifelse(hs_int_aea@data$seq_id %in% drop, 0, 1)
write.table(as.data.frame(hs_int_aea@data), row.names = F,
            file="./Processed_Data/include key.txt", sep=",")

hs_int_aea <- hs_int_aea[which(hs_int_aea$keep == 1),]
hs_int_aea@data$keep <- NULL

hs_int <- spTransform(hs_int_aea, CRS(ll_wgs84))

#' map these locations along with interested participants

bound <- spTransform(bound_1km, CRS(ll_wgs84))
bound_map <- fortify(bound, region="id")

#' base map for the area
base_map <- get_map(location=bbox(bound), maptype="roadmap",
                    zoom = 10, source="google")

ggmap(base_map) +
  #ggtitle("Monitor locations (participants jittered)") +
  geom_polygon(data=bound_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=as.data.frame(mon), aes(x=lon, y=lat, color="mon"),
             pch=20, size=3) + 
  geom_jitter(data=as.data.frame(hs_int), aes(x=X, y=Y, color="pt"),
             pch=20, size=3, width=0.013, height=0.013) +
  # geom_point(data=as.data.frame(hs_int), aes(x=X, y=Y, color="pt"),
  #            pch=20, size=3, width=0.013, height=0.013) +
  scale_color_manual(name="Monitor Type",
                     values = c("mon" = "blue", "pt" = "red"),
                     labels = c("mon" = "Community", "pt" = "Participant")) +
  theme(legend.position = c(0.9, 0.9)) +
  coord_map(ylim=c(39.5, 40.05)) +
  simple_theme
ggsave(filename=("./Maps/Participant + Monitor Locations.jpeg"), device="jpeg")

#' -----------------------------------------------------------------------------
#' 3) Identify which grid cells contain a community monitor or participant
#' -----------------------------------------------------------------------------

load("./Processed_Data/Grid 250 m.RData")

#Monitors
plot(mon_aea, main="Community monitors: Albers Equal Area")

#' Participants
plot(hs_int_aea, main="Participants: Albers Equal Area")

#' Add grid cell ID to points
grid_raster <- raster(grid_250)
mon_aea$grid_id <- raster::extract(grid_raster, mon_aea, method="simple")
mon_aea$grid_id
save(mon, mon_aea, file="./Processed_Data/Community Monitors.RData")

hs_int_aea$grid_id <- raster::extract(grid_raster, hs_int_aea, method="simple")
hs_int_aea$grid_id

key <- hs_int_aea@data[,c("seq_id", "PID", "grid_id")]
write.table(key, file="./Processed_Data/grid key.txt", row.names = F)
rm(key)

hs_int_aea@data <- hs_int_aea@data[,c("seq_id", "grid_id", "X", "Y")]
hs_int@data <- hs_int_aea@data[,c("seq_id", "X", "Y")]

save(hs_int, hs_int_aea, file="./Processed_Data/Interested participants.RData")

#' Indicate which grid cells the monitors are in
all_mon <- rbind(as(hs_int_aea, "SpatialPoints"), 
                 as(mon_aea[which(mon_aea@data$seq_id != 224),], "SpatialPoints"))
plot(all_mon)

save(all_mon, file="./Processed_Data/All Monitors.RData")

contains_list <- sp::over(all_mon, grid_250)
contains_list

grid_mon <- grid_250
grid_mon$mon <- ifelse(grid_250$grid_id %in% contains_list$grid_id, 1, 0)
summary(grid_mon)

spplot(grid_mon, zcol="mon", main="Grid cells containing all monitors")
save(grid_mon, file="./Processed_Data/Grid 250 m with monitors.RData")
