#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 Land Use Regression
#' 
#' Task: Create a grid for the study domain and summarize land use and traffic 
#' variables for each grid cell
#' 
#' Author: Sheena Martenies
#' Date Created: April 6, 2018
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

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm_13 <- "+init=epsg:26913"

#' -----------------------------------------------------------------------------
#' 1) Create a "boundary" for the study area
#'    Nominally, the study boundary is the 470 belt that surrounds the Denver
#'    metro area. Here, I'm creating a boundary by first identifying some points
#'    along the highway (using Google Maps), drawing a bounding box and adding
#'    a 1 km buffer to account for traffic gradient emissions. This boundary
#'    will be used to process all of the other spatial inputs (e.g., lang use
#'    and traffic metrics)
#' -----------------------------------------------------------------------------

pts <- data.frame(lon=c(-105.194150, -105.086157, -104.785862,
                        -104.715897, -105.007800),
                  lat=c(39.712174, 39.553840, 39.548127,
                        39.740367, 39.984862))
pts_df <- pts
coordinates(pts) <- c("lon", "lat")
crs(pts) <- ll_wgs84
pts <- SpatialPointsDataFrame(pts, data=pts_df)
summary(pts)
plot(pts, main="boundary points: latlong")

#' Project to Albers equal area (used by the Land Use dataset)
pts_aea <- spTransform(pts, CRS(albers))
plot(pts_aea, main="boundary points: Albers Equal Area")

#' Get boundary around these points
pts_bound <- gEnvelope(pts_aea, byid = F)
plot(pts_bound, main="bounding box", border="red", col=NA)
points(pts_aea, col="blue")

#' Add a 1 km buffer around the bounding box
bound_1km <- gBuffer(pts_bound, width=1000, capStyle="SQUARE")
plot(bound_1km, main="extended bounding box")
plot(pts_bound, col=NA, border="red", add=T)
points(pts_aea, col="blue")

save(bound_1km, file="./Processed_Data/LUR boundary.RData")

#' Create grid using the boundary box
#' Two resolutions: 250 m and 500 m
ext <- extent(bound_1km)

grid_250_r <- raster(ext, crs=CRS(albers), resolution = c(250,250))
grid_250_r

grid_250 <- SpatialGridDataFrame(as(grid_250_r, "SpatialGrid"),
                                 data.frame(grid_id = seq(1:ncell(grid_250_r))))
summary(grid_250)
plot(grid_250)
rm(grid_250_r)

grid_500_r <- raster(ext, crs=CRS(albers), resolution = c(500,500))
grid_500_r

grid_500 <- SpatialGridDataFrame(as(grid_500_r, "SpatialGrid"),
                                 data.frame(grid_id = seq(1:ncell(grid_500_r))))
summary(grid_500)
plot(grid_500)
rm(grid_500_r)

#' Save as .RData and shapefiles
save(grid_250, file="./Processed_Data/Grid 250 m.RData")
save(grid_500, file="./Processed_Data/Grid 500 m.RData")
writeOGR(obj = grid_250, layer="grid_250_m", driver = "ESRI Shapefile",
         dsn="./Processed_Data", overwrite_layer = T)
writeOGR(obj = grid_500, layer="grid_500_m", driver = "ESRI Shapefile",
         dsn="./Processed_Data", overwrite_layer = T)

#' -----------------------------------------------------------------------------
#' 2) Process the spatial data used to evaluate whether HS participants differ
#'    from the rest of the study area in terms of land charateristics and traffic
#'    exposure
#' -----------------------------------------------------------------------------

#' Read in the NLDC data, clip, and summarize for each grid cell
impervious_path <- "./Raw_Data/nlcd_2011_impervious_2011_edition_2014_10_10/nlcd_2011_impervious_2011_edition_2014_10_10.img"
land_use_path <- "./Raw_Data/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img"
tree_cover_path <- "./Raw_Data/CONUSAnalytical_2_8_16/Analytical/nlcd2011_usfs_conus_canopy_analytical.img"

#' Impervious surfaces
impervious_f <- raster(impervious_path)
impervious_f
spplot(impervious_f, main="Percent Impervious Surfaces")

#' Percent Tree cover
tree_f <- raster(tree_cover_path)
tree_f
spplot(tree_f, main="Percent Tree Cover")

#' Land use categorization
land_use_f <- raster(land_use_path)
land_use_f
plot(land_use_f, "NLCD.2011.Land.Cover.Class",
     main="Land Use Characterization")

#' clip rasters
load("./Processed_Data/Grid 250 m.RData")
ext <- extent(grid_250)

#' Impervious surfaces
#' Check range; shouldn't have anything over 100%
impervious <- crop(impervious_f, ext)
impervious
plot(impervious, main="Percent impervious surface in the study area")
rm(impervious_f)

save(impervious, file="./Processed_Data/impervious.RData")
writeRaster(impervious, file="./Processed_Data/impervious.tif",
            format="GTiff", overwrite=T)

tree <- crop(tree_f, ext)
tree
plot(tree, main="Percent tree canopy in the study area")
rm(tree_f)

save(tree, file="./Processed_Data/tree_cover.RData")
writeRaster(tree, file="./Processed_Data/tree_cover.tif",
            format="GTiff", overwrite=T)

land_use <- crop(land_use_f, ext)
land_use
plot(land_use, main="Land use categories")
rm(land_use_f)

save(land_use, file="./Processed_Data/land_use.RData")
writeRaster(land_use, file="./Processed_Data/land_use.tif",
            format="GTiff", overwrite=T)

#' Read in road network shapefiles, project to Albers equal area, clip to grid
highways_f <- readOGR(dsn="./Raw_Data", layer="HIGHWAYS_CROP")
major_f <- readOGR(dsn="./Raw_Data", layer="MAJOR_ROADS_CROP")
local_f <- readOGR(dsn="./Raw_Data", layer="LOCAL_ROADS_CROP")

summary(highways_f)
summary(major_f)
summary(local_f)

# plot(local_f, col="grey50")
# plot(major_f, col="blue", add=T)
# plot(highways_f, col="red", add=T)

#' Clip by grid extent and project to Albers Equal Area
load("./Processed_Data/Grid 250 m.RData")
c_df <- coordinates(grid_250)

mat <- matrix(c(min(c_df[,1]), min(c_df[,2]),
                max(c_df[,1]), min(c_df[,2]),
                max(c_df[,1]), max(c_df[,2]),
                min(c_df[,1]), max(c_df[,2]),
                min(c_df[,1]), min(c_df[,2])),
              ncol=2, byrow=T)

bound <- Polygon(mat)
bound <- SpatialPolygons(list(Polygons(list(bound), ID = "boundary")),
                         proj4string=CRS(albers))
plot(bound)
save(bound, file="./Processed_Data/Grid boundary.RData")

bound_utm <- spTransform(bound, crs(highways_f))

highways <- spTransform(highways_f[bound_utm,], crs(albers))
save(highways, file="./Processed_Data/highways.RData")
writeOGR(obj = highways, layer="highways", driver = "ESRI Shapefile",
         dsn="./Processed_Data", overwrite_layer = T)
rm(highways_f)

major <- spTransform(major_f[bound_utm,], crs(albers))
save(major, file="./Processed_Data/major roads.RData")
writeOGR(obj = major, layer="major roads", driver = "ESRI Shapefile",
         dsn="./Processed_Data", overwrite_layer = T)
rm(major_f)

local <- spTransform(local_f[bound_utm,], crs(albers))
save(local, file="./Processed_Data/local roads.RData")
writeOGR(obj = local, layer="local roads", driver = "ESRI Shapefile",
         dsn="./Processed_Data", overwrite_layer = T)
rm(local_f)

plot(local, col="grey50", main="Road network in the study area")
plot(major, col="blue", add=T)
plot(highways, col="red", add=T)

#' Read in NHPS 2015 shapefile, project to Albers equal area, clip to grid
nhpms_15f <- readOGR(dsn="./Raw_Data", layer="Colorado_Sections_2015")
summary(nhpms_15f)

bound_wgs <- spTransform(bound, crs(nhpms_15f))

nhpms_2015 <- spTransform(nhpms_15f[bound_wgs,], crs(albers))
save(nhpms_2015, file="./Processed_Data/nhpms_2015.RData")
writeOGR(obj = nhpms_2015, layer="nhpms_2015", driver = "ESRI Shapefile",
         dsn="./Processed_Data", overwrite_layer = T)
rm(nhpms_15f)

plot(nhpms_2015, main="NHSP Links: 2015", col=as.factor(nhpms_2015$f_system))
legend("right", legend = c("1" = "Interstates",
                           "2" = "PA- Freeways",
                           "3" = "PA- Other",
                           "4" = "Minor Arterial",
                           "5" = "Major Collector",
                           "6" = "Minor Collector",
                           "7" = "Local Road"), 
       col = 1:7, cex = 0.8, pch = 1, title = "Functional Class")
histogram(nhpms_2015$f_system)
summary(nhpms_2015$aadt)
histogram(nhpms_2015$aadt)

nhpms_aadt <- nhpms_2015[which(nhpms_2015$aadt > 0),]
plot(nhpms_aadt, main="nhpms Links with AADT > 0", col=as.factor(nhsp_aadt$f_system))
legend("right", legend = c("1" = "Interstates",
                           "2" = "PA- Freeways",
                           "3" = "PA- Other",
                           "4" = "Minor Arterial",
                           "5" = "Major Collector",
                           "6" = "Minor Collector",
                           "7" = "Local Road"), 
       col = 1:7, cex = 0.8, pch = 1, title = "Functional Class")

save(nhpms_aadt, file="./Processed_Data/nhpms_aadt.RData")
writeOGR(obj = nhpms_aadt, layer="nhpms_aadt", driver = "ESRI Shapefile",
         dsn="./Processed_Data", overwrite_layer = T)
summary(nhpms_aadt$aadt)
histogram(nhpms_aadt$aadt)

#' -----------------------------------------------------------------------------
#' 3) Summarize land characteristics and traffic exposures for the 250 m grid
#' -----------------------------------------------------------------------------

load("./Processed_Data/Grid 250 m.RData")

#' Average % tree cover and % impervious surface
load("./Processed_Data/tree_cover.RData")
load("./Processed_Data/impervious.RData")

tree_g <- as(tree, "SpatialGridDataFrame")
impervious_g <- as(impervious, "SpatialGridDataFrame")

grid_250$tree_cover <- sp::over(grid_250, tree_g, fn=mean)[,1]
summary(grid_250$tree_cover)
spplot(tree, main="percent tree canopy cover: 30 m raster")
spplot(grid_250, zcol="tree_cover",
       main="average percent tree canopy cover: 250 m grid")

grid_250$impervious <- sp::over(grid_250, impervious_g, fn=mean)[,1]
summary(grid_250$impervious)
spplot(impervious, main="percent impervious surface: 30 m raster")
spplot(grid_250, zcol="impervious",
       main="average percent impervious surface: 250 m grid")

save(grid_250, file="./Processed_Data/Grid 250 m with attributes.RData")

#' clean up environment
rm(tree, tree_g, impervious, impervious_g)

#' Most frequent land use category
load("./Processed_Data/Grid 250 m with attributes.RData")
load("./Processed_Data/land_use.RData")

land_use_g <- as(land_use, "SpatialGridDataFrame")

#' function for mode (mfv) in modeest package
library(modeest)
grid_250$land_use <- sp::over(grid_250, land_use_g, fn=mfv)[,5]

summary(grid_250$land_use)
plot(land_use, main="Most frequent land_use: 30 m raster")
spplot(grid_250, zcol="land_use",
       main="Most frequent land_use: 250 m grid")

save(grid_250, file="./Processed_Data/Grid 250 m with attributes.RData")

#' Grid to Polygons (for road lengths)
load("./Processed_Data/Grid 250 m with attributes.RData")
#' grid_ids <- grid_250$grid_id
#' poly_250 <- as(grid_250, "SpatialPolygonsDataFrame")
#' 
#' #' Function to get lengths within cell polygons
#' get_length <- function(poly, lines) {
#'   if (gIntersects(lines, poly)) {
#'     roads <- gIntersection(lines, poly)
#'     length_km <- gLength(roads) / 1000
#'     length_df <- data.frame(grid_id = as.character(poly@data$grid_id),
#'                             length_km = length_km)
#'   } else {
#'     length_df <- data.frame(grid_id = as.character(poly@data$grid_id),
#'                             length_km = 0)
#'   }
#'   return(length_df)
#' }
#' 
#' #' Set up cluster to run the next bit of code in parallel
#' library(doParallel)
#' 
#' #' Highway lengths
load("./Processed_Data/highways.RData")
#' a1 <- Sys.time()
#' cl <- makeCluster(4)
#' registerDoParallel(cl)
#' highway_df <- foreach(i=1:length(grid_ids), .packages=c("sp", "rgeos"), .combine=rbind) %dopar% {
#'   get_length(poly=poly_250[which(poly_250@data$grid_id == grid_ids[i]),],
#'              lines=highways)
#' }
#' Sys.time() - a1
#' 
#' highway_df$highway_km <- highway_df$length_km
#' save(highway_df, file="./Processed_Data/cell_highway_lengths.RData")
load("./Processed_Data/cell_highway_lengths.RData")
grid_250 <- merge(grid_250, highway_df[,c("grid_id", "highway_km")], by="grid_id")
save(grid_250, file="./Processed_Data/Grid 250 m with attributes.RData")

#' Check
gLength(highways, byid=F) / 1000
sum(grid_250$highway_km)

plot(highways, main="Highways")
spplot(grid_250, zcol="highway_km",
       main="Highway length (km): 250 m grid")

#' Compare with grid overlay method
# load("./Processed_Data/Grid 250 m.RData")
# test_grid <- grid_250
# 
# load("./Processed_Data/highways.RData")
# test_grid$highways <- sp::over(as(test_grid, "SpatialPolygonsDataFrame"),
#                                highways, fn=gLength)

#' Check
gLength(highways, byid=F) / 1000
sum(grid_250$highway_km)

rm(highways, highways_df)

#' Major road lengths
#' Break into five data frames
load("./Processed_Data/major roads.RData")
# a1 <- Sys.time()
# cl <- makeCluster(4)
# registerDoParallel(cl)
# major_df1 <- foreach(i=1:8000, .packages=c("sp", "rgeos"), .combine=rbind) %dopar% {
#   get_length(poly=poly_250[which(poly_250@data$grid_id == grid_ids[i]),],
#              lines=major)
# }
# save(major_df1, file="./Processed_Data/major_df1.RData")
# 
# cl <- makeCluster(4)
# registerDoParallel(cl)
# major_df2 <- foreach(i=8001:16000, .packages=c("sp", "rgeos"), .combine=rbind) %dopar% {
#   get_length(poly=poly_250[which(poly_250@data$grid_id == grid_ids[i]),],
#              lines=major)
# }
# save(major_df2, file="./Processed_Data/major_df2.RData")
# 
# cl <- makeCluster(4)
# registerDoParallel(cl)
# major_df3 <- foreach(i=16001:24000, .packages=c("sp", "rgeos"), .combine=rbind) %dopar% {
#   get_length(poly=poly_250[which(poly_250@data$grid_id == grid_ids[i]),],
#              lines=major)
# }
# save(major_df3, file="./Processed_Data/major_df3.RData")
# 
# cl <- makeCluster(4)
# registerDoParallel(cl)
# major_df4 <- foreach(i=24001:30000, .packages=c("sp", "rgeos"), .combine=rbind) %dopar% {
#   get_length(poly=poly_250[which(poly_250@data$grid_id == grid_ids[i]),],
#              lines=major)
# }
# save(major_df4, file="./Processed_Data/major_df4.RData")
# 
# cl <- makeCluster(4)
# registerDoParallel(cl)
# major_df5 <- foreach(i=30001:length(grid_ids), .packages=c("sp", "rgeos"), .combine=rbind) %dopar% {
#   get_length(poly=poly_250[which(poly_250@data$grid_id == grid_ids[i]),],
#              lines=major)
# }
# Sys.time() - a1
# save(major_df5, file="./Processed_Data/major_df5.RData")
# 
# major_df <- rbind(major_df1, major_df2, major_df3, major_df4, major_df5)
# 
# major_df$major_km <- major_df$length_km
# save(major_df, file="./Processed_Data/cell_major_road_lengths.RData")
load("./Processed_Data/cell_major_road_lengths.RData")
grid_250 <- merge(grid_250, major_df[,c("grid_id", "major_km")], by="grid_id")
save(grid_250, file="./Processed_Data/Grid 250 m with attributes.RData")

plot(major, main="Major Roads")
spplot(grid_250, zcol="major_km",
       main="Major road length (km): 250 m grid")

#' Check
gLength(major, byid=F) / 1000
sum(grid_250$major_km)
rm(major, major_df)

#' traffic density variable: weighted sum of road lengths
#' Current weights are 1, 0.5, and 0.25 for highway, major, and local, respectively
load("./Processed_Data/Grid 250 m with attributes.RData")
names(grid_250)
grid_250$road_km_wt <- ((grid_250$highway_km * 1) + 
                          (grid_250$major_km * 0.5))
summary(grid_250)
spplot(grid_250, zcol="road_km_wt",
       main="Weighted sum of road lengths (km): 250 m grid")

#' Save grid 
save(grid_250, file="./Processed_Data/Grid 250 m with attributes.RData")

#' Sum of AADT across road links within the grid
#' Function to get sum of AADT within cell polygons
# get_aadt <- function(poly, lines) {
#   if (gIntersects(lines, poly)) {
#     roads <- lines[poly,]
#     aadt <- sum(roads@data$aadt)
#     aadt_df <- data.frame(grid_id = as.character(poly@data$grid_id),
#                           aadt = aadt)
#   } else {
#     aadt_df <- data.frame(grid_id = as.character(poly@data$grid_id),
#                           aadt = 0)
#   }
#   return(aadt_df)
# }
# 
load("./Processed_Data/nhpms_aadt.RData")
# a1 <- Sys.time()
# cl <- makeCluster(4)
# registerDoParallel(cl)
# aadt_df <- foreach(i=1:length(grid_ids), .packages=c("sp", "rgeos"), .combine=rbind) %dopar% {
#   get_aadt(poly=poly_250[which(poly_250@data$grid_id == grid_ids[i]),],
#            lines=nhpms_aadt)
# }
# Sys.time() - a1
# 
# save(aadt_df, file="./Processed_Data/cell_aadt.RData")
load("./Processed_Data/cell_aadt.RData")
grid_250 <- merge(grid_250, aadt_df[,c("grid_id", "aadt")], by="grid_id")
save(grid_250, file="./Processed_Data/Grid 250 m with attributes.RData")

rm(nhpms_aadt, aadt_df)

#' -----------------------------------------------------------------------------
#' 4) Summarize census data using grid cell centroids
#' -----------------------------------------------------------------------------

load("./Processed_Data/Grid 250 m with attributes.RData")
load("./Processed_Data/Grid boundary.RData")

#' Get block groups in study area
bg_f <- readOGR(dsn="./Raw_Data", layer="ACS_2016_5YR_BG_08")
bg_f <- spTransform(bg_f, CRS(albers))

plot(bg_f)
plot(bound, add=T, border="red", col=NA)

bg <- bg_f[bound,c("GEOID", "Shape_Area")]
bg$GEOID <- as.character(bg$GEOID)
rm(bg_f)

plot(bg)
plot(bound, add=T, border="red", col=NA)

#' Grid centroids
grid_poly <- as(grid_250, "SpatialPolygonsDataFrame")
grid_cent <- gCentroid(grid_poly, byid = T)
grid_cent_df <- as.data.frame(cbind(grid_cent@coords, sp::over(grid_cent, grid_poly)))

grid_cent <- SpatialPointsDataFrame(grid_cent, data=grid_cent_df)

plot(bg)
plot(bound, add=T, border="red", col=NA)
points(grid_cent, col="blue")

plot(grid_poly[which(grid_poly$grid_id==1),])
plot(grid_cent[which(grid_cent$grid_id==1),], add=T)

#' Population density, SFH denisty, and MDU density
bg_ids <- bg@data$GEOID

#' Population: 
#'     B010001e1 = estimate of total population
pop <- read.table("./Raw_Data/ACS_2016_5YR_BG_08_Population.txt",
                  header=T, sep=",", stringsAsFactors = F) %>%
  select(GEOID, B01001e1) %>%
  mutate(GEOID = gsub("15000US", "", GEOID)) %>%
  filter(GEOID %in% bg_ids) %>%
  rename(total_pop = B01001e1)

#' Housing:
#'     B25024e1,UNITS IN STRUCTURE: Total: Housing units
#'     B25024e2,"UNITS IN STRUCTURE: 1, detached: Housing units
#'     B25024e3,"UNITS IN STRUCTURE: 1, attached: Housing units
#'     B25024e4,UNITS IN STRUCTURE: 2: Housing units
#'     B25024e5,UNITS IN STRUCTURE: 3 or 4: Housing units
#'     B25024e6,UNITS IN STRUCTURE: 5 to 9: Housing units
#'     B25024e7,UNITS IN STRUCTURE: 10 to 19: Housing units
#'     B25024e8,UNITS IN STRUCTURE: 20 to 49: Housing units
#'     B25024e9,UNITS IN STRUCTURE: 50 or more: Housing units
#'     B25024e10,UNITS IN STRUCTURE: Mobile home: Housing units
#'     B25024e11,"UNITS IN STRUCTURE: Boat, RV, van, etc.: Housing units
h_vars <- paste("B25024e", 1:11, sep="")

housing <- read.table("./Raw_Data/ACS_2016_5YR_BG_08_Housing.txt",
                      header=T, sep=",", stringsAsFactors = F) %>%
  select(GEOID, h_vars) %>%
  mutate(GEOID = gsub("15000US", "", GEOID),
         single = B25024e2 + B25024e3,
         multi = B25024e4 + B25024e5 + B25024e6 + B25024e7 + B25024e8 + B25024e9,
         mobile = B25024e10 + B25024e11,
         housing = single + multi + mobile) %>%
  filter(GEOID %in% bg_ids) %>%
  select(GEOID, single, multi, mobile, housing)

#' Polygon area in sq km
#' Map units are m, so need to convert to km
bg$area <- gArea(spgeom = bg, byid=T) / (1000^2)
head(bg[,c("Shape_Area", "area")])

#' Merge census data and polygons
bg <- merge(bg, pop, by="GEOID")
bg <- merge(bg, housing, by="GEOID")

#' Caluclate population and housing denisty
#' Persons per sq km and units per sq km
bg@data$pop_den <- bg@data$total_pop / bg@data$area
spplot(bg, zcol="total_pop", main="Block group population (n)")
spplot(bg, zcol="pop_den", main="Block group population density (n / km^2)")

bg@data$single_den <- bg@data$single / bg@data$area
spplot(bg, zcol="single", main="Block group single family homes (n)")
spplot(bg, zcol="single_den", main="Block group single family home density (n / km^2)")

bg@data$multi_den <- bg@data$multi / bg@data$area
spplot(bg, zcol="multi", main="Block group multi-family homes (n)")
spplot(bg, zcol="multi_den", main="Block group multi-family home density (n / km^2)")

bg@data$mobile_den <- bg@data$mobile / bg@data$area
spplot(bg, zcol="mobile", main="Block group mobile homes (n)")
spplot(bg, zcol="mobile_den", main="Block group mobile home density (n / km^2)")

bg@data$housing_den <- bg@data$housing / bg@data$area
spplot(bg, zcol="housing", main="Block group housing units (n)")
spplot(bg, zcol="housing_den", main="Block group housing unit density (n / km^2)")

grid_census <- as.data.frame(cbind(sp::over(grid_cent, grid_poly),
                                   sp::over(grid_cent, bg)))

save(bg, grid_census, file="./Processed_Data/ACS_2016_BG.RData")
grid_250 <- merge(grid_250, grid_census[,c("grid_id", "total_pop", "pop_den",
                                           "single", "multi", "mobile", "housing",
                                           "single_den", "multi_den", 
                                           "mobile_den", "housing_den")], 
                  by="grid_id")
save(grid_250, file="./Processed_Data/Grid 250 m with attributes.RData")

spplot(grid_250, zcol="housing_den", main="housing unit density (n / km^2): 250 m grid")
spplot(grid_250, zcol="single_den", main="single family housing unit density (n / km^2): 250 m grid")
spplot(grid_250, zcol="multi_den", main="multi-family housing density (n / km^2): 250 m grid")
spplot(grid_250, zcol="mobile_den", main="mobile housing density (n / km^2): 250 m grid")
