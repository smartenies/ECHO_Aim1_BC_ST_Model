#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 Land Use Regression
#' 
#' Task: compare grid cells that contain HS particpants with the rest of the 
#' study domain
#' 
#' Author: Sheena Martenies
#' Date Created: April 12, 2018
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
library(leaflet)
library(ggmap)
library(readxl)

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm_13 <- "+init=epsg:26913"

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))

#' -----------------------------------------------------------------------------
#' 1) Create a grid for the study area
#' -----------------------------------------------------------------------------
map_crs <- "+proj=longlat +datum=WGS84"

#' read in grid data 
grid_250 <- st_read(here::here("Data", "Grid_250_m_AEA.csv"),
                    stringsAsFactors = F, wkt = "WKT", crs = albers) 

#' 1 km buffer around the grid
grid_bound <- st_buffer(st_union(grid_250), dist = 1000) %>% 
  st_transform(ll_wgs84)
#' base map for the area

base_map <- get_map(location=unname(st_bbox(grid_bound)), maptype="roadmap",
                    zoom = 10, source="google")

ggmap(base_map) +
  ggtitle("Study Area") +
  geom_sf(data=st_sf(grid_bound), 
          inherit.aes = F, color="black", fill="NA", size=1) 

#' -----------------------------------------------------------------------------
#' 2) Interested HS participants
#' -----------------------------------------------------------------------------

hs_int_aea <- st_read(here::here("Data/HS_Data", "Interested Participants.csv"),
                      stringsAsFactors = F, wkt = "WKT", crs = albers) %>% 
  mutate()

#' Assigning grid attributes to participants
load("./Processed_Data/Grid 250 m with attributes.RData")
grid_att <- as.data.frame(grid_250[which(grid_250@data$grid_id %in% grid_ids),])

hs_int_aea <- merge(hs_int_aea, grid_att, by="grid_id")
hs_int_aea <- hs_int_aea[bound_1km,]

#' Jitter the locations of the participants
pt_map <- hs_int_aea@data

pt_map$X_jit <- jitter(pt_map$X, factor=0.013)
pt_map$Y_jit <- jitter(pt_map$Y, factor=0.013)

coordinates(pt_map) <- ~ X_jit + Y_jit
proj4string(pt_map) <- ll_wgs84

pt_map@data <- cbind(pt_map@data, coordinates(pt_map))

ggmap(base_map) +
  ggtitle("Approximate Locations of Interested Participants") +
  geom_polygon(data=bound_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
#   geom_point(data=as.data.frame(hs_int_ll), aes(x=X, y=Y),
#               color="blue", pch=18) +
  geom_jitter(data=as.data.frame(pt_map), aes(x=X, y=Y), size=3,
             color="red", pch=20, width=0.013, height=0.013)

# load("./Processed_Data/Grid 250 m with participants.RData")
# spplot(grid_250, zcol="hs", main="Participants within the 250 m grid")

#' Interactive map with grid attributes
leaflet(data=pt_map) %>%
  addTiles %>%
  addPolygons(data = bound, color="black", weight=1, opacity=0.2) %>%
  addMarkers(~X_jit, ~Y_jit, 
             popup=~paste("Participant No.: ", seq_id, "<br>",
                          "Tree cover (%): ", tree_cover, "<br>",
                          "Impervious (%): ", impervious, "<br>",
                          "Highways (km): ", round(highway_km, 2), "<br>",
                          "Major roads (km): ", round(major_km, 2), "<br>",
                          "Wtd sum of roads (km): ", round(road_km_wt, 2), "<br>",
                          sep=""))

#' Table of participant grid cell attributes
as.data.frame(pt_map@data[,c(5:10)])


#' -----------------------------------------------------------------------------
#' 3) Spatial attribtues
#' -----------------------------------------------------------------------------

load("./Processed_Data/tree_cover.RData")
spplot(tree, main="Percent tree cover (30 m raster)")
spplot(grid_250, zcol="tree_cover",
       main="Percent tree cover (250 m grid)")

load("./Processed_Data/impervious.RData")
spplot(impervious, main="Percent impervious surface (30 m raster)")
spplot(grid_250, zcol="impervious",
       main="Percent impervious surface (250 m grid)")

load("./Processed_Data/highways.RData")
load("./Processed_Data/major roads.RData")
load("./Processed_Data/local roads.RData")

# plot(local, col="grey50", main="Road network in the study area")
# plot(major, col="blue", add=T)
plot(major, col="blue", main="Road network in the study area")
plot(highways, col="red", add=T)
spplot(grid_250, zcol="road_km_wt",
       main="Weighted sum (km) of highways and major roads (250 m grid)")

load("./Processed_Data/nhpms_aadt.RData")
plot(nhpms_aadt, col="blue", main="NHPMS 2015 with AADT > 0")
spplot(grid_250, zcol="aadt",
       main="NHPMS 2015 AADT (250 m grid)")

#' -----------------------------------------------------------------------------
#' 4) Compare participant grid cells with all grid cells
#' -----------------------------------------------------------------------------

load("./Processed_Data/Grid 250 m with attributes.RData")
grid_all <- grid_250

load("./Processed_Data/Grid 250 m with participants.RData")
grid_all <- merge(grid_all, grid_250, by="grid_id")

grid_all_df <- as.data.frame(grid_all)
grid_hs_df <- grid_all_df[which(grid_all_df$hs == 1),]

#' All grid cells
summary(grid_all_df[,2:7])

#' Healthy Start participant cells
summary(grid_hs_df[,2:7])

#' Smoothed density plots each attribute

#' Percent tree cover
tree_cov <- ggplot() +
  geom_density(data=grid_all_df, aes(grid_all_df$tree_cover, fill="all")) + 
  geom_density(data=grid_hs_df, aes(grid_hs_df$tree_cover, fill="hs"),
               alpha=0.5) + 
  scale_fill_manual(name="Distribution",
                    values = c("all" = "red", "hs" = "blue"),
                    labels = c("All grid cells", "HS grid cells")) +
  xlab("Percent Tree Cover") +
  simple_theme
plot(tree_cov)

ks.test(grid_all_df$tree_cover, grid_hs_df$tree_cover)

#' Percent impervious surfaces
imp_surf <- ggplot() +
  geom_density(data=grid_all_df, aes(grid_all_df$impervious, fill="all")) + 
  geom_density(data=grid_hs_df, aes(grid_hs_df$impervious, fill="hs"), 
               alpha=0.5) + 
  scale_fill_manual(name="Distribution",
                    values = c("all" = "red", "hs" = "blue"),
                    labels = c("All grid cells", "HS grid cells")) +
  xlab("Percent Impervious Surfaces") +
  simple_theme
plot(imp_surf)

ks.test(grid_all_df$impervious, grid_hs_df$impervious)

#' Highways
high_den <- ggplot() +
  geom_density(data=grid_all_df, aes(grid_all_df$highway_km, fill="all")) + 
  geom_density(data=grid_hs_df, aes(grid_hs_df$highway_km, fill="hs"), 
               alpha=0.5) + 
  scale_fill_manual(name="Distribution",
                    values = c("all" = "red", "hs" = "blue"),
                    labels = c("All grid cells", "HS grid cells")) +
  xlab("Highway density (km)") +
  simple_theme
plot(high_den)

ks.test(grid_all_df$highway_km, grid_hs_df$highway_km)

#' Major roads
maj_den <- ggplot() +
  geom_density(data=grid_all_df, aes(grid_all_df$major_km, fill="all")) + 
  geom_density(data=grid_hs_df, aes(grid_hs_df$major_km, fill="hs"), 
               alpha=0.5) + 
  scale_fill_manual(name="Distribution",
                    values = c("all" = "red", "hs" = "blue"),
                    labels = c("All grid cells", "HS grid cells")) +
  xlab("Major road density (km)") +
  simple_theme
plot(maj_den)

ks.test(grid_all_df$major_km, grid_hs_df$major_km)

#' Weighted road density
road_den <- ggplot() +
  geom_density(data=grid_all_df, aes(grid_all_df$road_km_wt, fill="all")) + 
  geom_density(data=grid_hs_df, aes(grid_hs_df$road_km_wt, fill="hs"),
               alpha=0.5) + 
  scale_fill_manual(name="Distribution",
                    values = c("all" = "red", "hs" = "blue"),
                    labels = c("All grid cells", "HS grid cells")) +
  xlab("Weighted road density (km)") +
  simple_theme
plot(road_den)

ks.test(grid_all_df$road_km_wt, grid_hs_df$road_km_wt)

#' AADT
aadt <- ggplot() +
  geom_density(data=grid_all_df, aes(grid_all_df$aadt, fill="all")) + 
  geom_density(data=grid_hs_df, aes(grid_hs_df$aadt, fill="hs"),
               alpha=0.5) + 
  scale_fill_manual(name="Distribution",
                    values = c("all" = "red", "hs" = "blue"),
                    labels = c("All grid cells", "HS grid cells")) +
  xlab("AADT") +
  simple_theme
plot(aadt)

#' AADT dot plot
aadt_dp <- ggplot(data=grid_all@data, aes(x=as.factor(hs), y=aadt)) +
  geom_boxplot() +
  # scale_fill_manual(name="Distribution",
  #                   values = c("all" = "red", "hs" = "blue"),
  #                   labels = c("All grid cells", "HS grid cells")) +
  # xlab("AADT") +
  simple_theme
plot(aadt_dp)

histogram(grid_all_df$aadt, col="red")
histogram(grid_hs_df$aadt, col="blue", add=T)

ks.test(grid_all_df$aadt, grid_hs_df$aadt)

#' -----------------------------------------------------------------------------
#' 5) Summary of land use types by area
#' -----------------------------------------------------------------------------

#' Read in the land use raster
#' 30 m grid cells
load("./Processed_Data/land_use.RData")
land_use
plot(land_use)

#' Frequency table
f_tab <- as.data.frame(freq(land_use))
f_tab$category <- c("Open Water", 
                    "Developed: Open space", "Developed: Low intensity",
                    "Developed: Medium intensity", "Developed: High intensity",
                    "Barren land", "Decidious forest", "Evergreen forest",
                    "Mixed forest", "Shrub/scrub", "Grasslands", "Pasture",
                    "Cultivated crops", "Woody wetlands", "Emergent wetlands") 
f_tab$pct_area <- round(f_tab$count / sum(f_tab$count) * 100, 2)
f_tab

#' Collapse categories based on those used in the DEEDS LUR
