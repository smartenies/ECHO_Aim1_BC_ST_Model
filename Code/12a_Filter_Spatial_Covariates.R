#' =============================================================================
#' Project: ECHO LUR
#' Date created: December 14, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script summarizes the spatial covariates at each of the sampled 
#' locations
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

#' -----------------------------------------------------------------------------
#' Read in the covariate data
#' -----------------------------------------------------------------------------

#' raster inputs
elevation <- raster(here::here("Data", "Elevation_AEA.tif"))
tree_cover <- raster(here::here("Data", "Tree_Cover_AEA.tif"))
impervious <- raster(here::here("Data", "Impervious_AEA.tif"))
land_use <- raster(here::here("Data", "Land_Use_AEA.tif"))
open_land <- raster(here::here("Data", "Open_Land_AEA.tif"))
low_int_land <- raster(here::here("Data", "Low_Int_Land_AEA.tif"))
med_int_land <- raster(here::here("Data", "Med_Int_Land_AEA.tif"))
high_int_land <- raster(here::here("Data", "High_Int_Land_AEA.tif"))
ag_land <- raster(here::here("Data", "Ag_Land_AEA.tif"))
pop_density <- raster(here::here("Data", "Population_Density_AEA.tif"))
pop_count <- raster(here::here("Data", "Population_Count_AEA.tif"))

#' vector inputs
airports <- read_csv(here::here("Data", "Airports_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
CAFOs <- read_csv(here::here("Data", "CAFOs_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
compost <- read_csv(here::here("Data", "Compost_Facilities_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
highways <- read_csv(here::here("Data", "Highways_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
landfills <- read_csv(here::here("Data", "Landfills_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
major_roads <- read_csv(here::here("Data", "Major_Roads_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
military <- read_csv(here::here("Data", "Military_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
mines <- read_csv(here::here("Data", "mines_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
npl_sites <- read_csv(here::here("Data", "NPL_sites_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
og_wells <- read_csv(here::here("Data", "OG_Wells_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
parks <- read_csv(here::here("Data", "Parks_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
rail <- read_csv(here::here("Data", "Rail_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
wwtp <- read_csv(here::here("Data", "Waste_Water_Plants_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
aadt <- read_csv(here::here("Data", "NHPMS_AADT_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

#' Functions to summarize raster and vector data
ras_cov <- function(sf_obj, ras_obj, sum_funct) {
  sp_obj <- as(sf_obj, "Spatial")
  temp_ext <- raster::extract(ras_obj, sp_obj, fun = sum_funct, na.rm=T,
                              small = T, weights = T, normalizeWeights = T)
  return(as.numeric(temp_ext))
}

ras_wt_sum <- function(sf_obj, ras_obj) {
  sp_obj <- as(sf_obj, "Spatial")
  temp_ext <- as.data.frame(raster::extract(ras_obj, sp_obj, na.rm=T, 
                                            small = T, weights = T))
  wt_sum <- sum(temp_ext$value * temp_ext$weight, na.rm=T) 
  return(as.numeric(wt_sum))
}

ras_area <- function(sf_obj, ras_obj) {
  resol <- res(ras_obj)[1]
  sp_obj <- as(sf_obj, "Spatial")
  ras_clip <- crop(ras_obj, sp_obj)
  ras_mask <- mask(ras_clip, sp_obj)
  ras_mask[0] <- NA
  ras_area <- cellStats(ras_mask, 'sum') * resol**2
  ras_area_km <- ras_area / (10^6)
  return(ras_area_km)
}

vec_length <- function(sf_obj, covariate_obj) {
  sf_sp_obj <- as(sf_obj, "Spatial")
  cov_cropped <- tmaptools::crop_shape(covariate_obj, sf_sp_obj, polygon = T)
  length <- ifelse(nrow(cov_cropped) == 0, 0, 
                   sum(unclass(st_length(cov_cropped))))
  return(length)
}

aadt_summary <- function(sf_obj, covariate_obj, sum_funct) {
  sf_intersect <- st_intersection(covariate_obj, sf_obj)
  value <- ifelse(nrow(sf_intersect) == 0, 0, sum_funct(sf_intersect$aadt))
  return(value)
}

#' Function to get the mode of a vector
get_mode <- function(x, na.rm = T) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#' vectorized if_else for cleaning data
my_if_else <- function(x) {
  if (is.infinite(x)) 0 else x
}

#' -----------------------------------------------------------------------------
#' Spatial covariates for each sampling location
#' -----------------------------------------------------------------------------

locations_sf <- read_csv(here::here("Data/Filter_Data", "Filter_Locations_AEA.csv")) %>% 
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

#' Use the 9th and Yuma location for the central site as well (co-located)
central_sf <- filter(locations_sf, Location %in% "9th and Yuma") %>% 
  slice(1) 
central_sf$filter_id[1] <- "080310027"

locations_sf <- rbind(locations_sf, central_sf)

plot(st_geometry(locations_sf))

print(sum(duplicated(locations_sf$filter_id)))

#' ---------------------------------------------------------------------------
#' For each point, summarize the land use characteristics within specific 
#' buffers: 50m, 100 m, 250 m, 500 m, 1000 m, and 2500 m
#' 
#' Spatial covariates (within each buffer):
#'     - Average elevation
#'     - Average percent tree cover
#'     - Average percent impervious surface
#'     - Most common land-use category (i.e., mode)
#'     - Distance (in meters) to: airport, CAFOs, compost, highways, landfills, 
#'       major roads, military installations, mines, npl sites, O&G wells, 
#'       parks, rail, and waste water treatment plants
#'     - Average population density and weighted population count
#'     - Length of highways and major roads
#'     - Average AADT of intersecting road segments
#' ---------------------------------------------------------------------------

# Empty data frame for covariates
covariates <- data.frame()

for (i in 1:nrow(locations_sf)) {
# for (i in 1:5) {
  print(paste("Location", i, "of", nrow(locations_sf)))
  point <- slice(locations_sf, i)
  
  pt_50 <- st_buffer(point, dist = 50)
  pt_100 <- st_buffer(point, dist = 100)
  pt_250 <- st_buffer(point, dist = 250)
  pt_500 <- st_buffer(point, dist = 500)
  pt_1000 <- st_buffer(point, dist = 1000)
  pt_2500 <- st_buffer(point, dist = 2500)
  
  #' average elevation 
  cov_temp <- data.frame(filter_id = point$filter_id,
                         elevation_50 = ras_cov(pt_50, elevation, mean),
                         elevation_100 = ras_cov(pt_100, elevation, mean),
                         elevation_250 = ras_cov(pt_250, elevation, mean),
                         elevation_500 = ras_cov(pt_500, elevation, mean),
                         elevation_1000 = ras_cov(pt_1000, elevation, mean),
                         elevation_2500 = ras_cov(pt_2500, elevation, mean))
  
  #' average tree cover
  cov_temp <- cov_temp %>% 
    mutate(tree_cover_50 = ras_cov(pt_50, tree_cover, mean),
           tree_cover_100 = ras_cov(pt_100, tree_cover, mean),
           tree_cover_250 = ras_cov(pt_250, tree_cover, mean),
           tree_cover_500 = ras_cov(pt_500, tree_cover, mean),
           tree_cover_1000 = ras_cov(pt_1000, tree_cover, mean),
           tree_cover_2500 = ras_cov(pt_2500, tree_cover, mean))
  
  #' average impervious surface
  cov_temp <- cov_temp %>% 
    mutate(impervious_50 = ras_cov(pt_50, impervious, mean),
           impervious_100 = ras_cov(pt_100, impervious, mean),
           impervious_250 = ras_cov(pt_250, impervious, mean),
           impervious_500 = ras_cov(pt_500, impervious, mean),
           impervious_1000 = ras_cov(pt_1000, impervious, mean),
           impervious_2500 = ras_cov(pt_2500, impervious, mean))
  
  #' most frequent land use category
  cov_temp <- cov_temp %>% 
    mutate(land_use_50 = raster::extract(land_use, as(pt_50, "Spatial"), 
                                         get_mode)[1,1],
           land_use_100 = raster::extract(land_use, as(pt_100, "Spatial"), 
                                          get_mode)[1,1],
           land_use_250 = raster::extract(land_use, as(pt_250, "Spatial"), 
                                          get_mode)[1,1],
           land_use_500 = raster::extract(land_use, as(pt_500, "Spatial"), 
                                          get_mode)[1,1],
           land_use_1000 = raster::extract(land_use, as(pt_1000, "Spatial"), 
                                           get_mode)[1,1],
           land_use_2500 = raster::extract(land_use, as(pt_2500, "Spatial"), 
                                           get_mode)[1,1])
  
  #' average area of each land use type
  cov_temp <- cov_temp %>% 
    mutate(open_50 = ras_area(pt_50, open_land),
           open_100 = ras_area(pt_100, open_land),
           open_250 = ras_area(pt_250, open_land),
           open_500 = ras_area(pt_500, open_land),
           open_1000 = ras_area(pt_1000, open_land),
           open_2500 = ras_area(pt_2500, open_land)) %>% 
    mutate(low_int_50 = ras_area(pt_50, low_int_land),
           low_int_100 = ras_area(pt_100, low_int_land),
           low_int_250 = ras_area(pt_250, low_int_land),
           low_int_500 = ras_area(pt_500, low_int_land),
           low_int_1000 = ras_area(pt_1000, low_int_land),
           low_int_2500 = ras_area(pt_2500, low_int_land)) %>% 
    mutate(med_int_50 = ras_area(pt_50, med_int_land),
           med_int_100 = ras_area(pt_100, med_int_land),
           med_int_250 = ras_area(pt_250, med_int_land),
           med_int_500 = ras_area(pt_500, med_int_land),
           med_int_1000 = ras_area(pt_1000, med_int_land),
           med_int_2500 = ras_area(pt_2500, med_int_land)) %>% 
    mutate(high_int_50 = ras_area(pt_50, high_int_land),
           high_int_100 = ras_area(pt_100, high_int_land),
           high_int_250 = ras_area(pt_250, high_int_land),
           high_int_500 = ras_area(pt_500, high_int_land),
           high_int_1000 = ras_area(pt_1000, high_int_land),
           high_int_2500 = ras_area(pt_2500, high_int_land)) %>% 
    mutate(ag_50 = ras_area(pt_50, ag_land),
           ag_100 = ras_area(pt_100, ag_land),
           ag_250 = ras_area(pt_250, ag_land),
           ag_500 = ras_area(pt_500, ag_land),
           ag_1000 = ras_area(pt_1000, ag_land),
           ag_2500 = ras_area(pt_2500, ag_land))
  
  #' average population density (n/km2) and populaton count (n)
  cov_temp <- cov_temp %>% 
    mutate(pop_den_50 = ras_cov(pt_50, pop_density, mean),
           pop_den_100 = ras_cov(pt_100, pop_density, mean),
           pop_den_250 = ras_cov(pt_250, pop_density, mean),
           pop_den_500 = ras_cov(pt_500, pop_density, mean),
           pop_den_1000 = ras_cov(pt_1000, pop_density, mean),
           pop_den_2500 = ras_cov(pt_2500, pop_density, mean))
  
  cov_temp <- cov_temp %>% 
    mutate(pop_ct_50 = ras_wt_sum(pt_50, pop_count),
           pop_ct_100 = ras_wt_sum(pt_100, pop_count),
           pop_ct_250 = ras_wt_sum(pt_250, pop_count),
           pop_ct_500 = ras_wt_sum(pt_500, pop_count),
           pop_ct_1000 = ras_wt_sum(pt_1000, pop_count),
           pop_ct_2500 = ras_wt_sum(pt_2500, pop_count))
  
  #' Distance (in meters) to the closest: airport, CAFOs, compost, highways, 
  #'       landfills, major roads, military installations, mines, npl sites, 
  #'       oil and gas wells, parks, rail, and waste water treatment plants
  cov_temp <- cov_temp %>% 
    mutate(dist_m_airport = min(unclass(st_distance(point, airports))),
           dist_m_cafos = min(unclass(st_distance(point, CAFOs))),
           dist_m_compost = min(unclass(st_distance(point, compost))),
           dist_m_highways = min(unclass(st_distance(point, highways))),
           dist_m_landfill = min(unclass(st_distance(point, landfills))),
           dist_m_major_rd = min(unclass(st_distance(point, major_roads))),
           dist_m_military = min(unclass(st_distance(point, military))),
           dist_m_mines = min(unclass(st_distance(point, mines))),
           dist_m_npl_sites = min(unclass(st_distance(point, npl_sites))),
           dist_m_og_wells = min(unclass(st_distance(point, og_wells))),
           dist_m_parks = min(unclass(st_distance(point, parks))),
           dist_m_rail = min(unclass(st_distance(point, rail))),
           dist_m_wwtp = min(unclass(st_distance(point, wwtp))))
  
  #' Length of highways and major roads (in meters) in each buffer
  cov_temp <- cov_temp %>% 
    mutate(len_m_highways_50 = vec_length(pt_50, highways),
           len_m_highways_100 = vec_length(pt_100, highways),
           len_m_highways_250 = vec_length(pt_250, highways),
           len_m_highways_500 = vec_length(pt_500, highways),
           len_m_highways_1000 = vec_length(pt_1000, highways),
           len_m_highways_2500 = vec_length(pt_2500, highways)) %>% 
    mutate(len_m_major_roads_50 = vec_length(pt_50, major_roads),
           len_m_major_roads_100 = vec_length(pt_100, major_roads),
           len_m_major_roads_250 = vec_length(pt_250, major_roads),
           len_m_major_roads_500 = vec_length(pt_500, major_roads),
           len_m_major_roads_1000 = vec_length(pt_1000, major_roads),
           len_m_major_roads_2500 = vec_length(pt_2500, major_roads))
  
  #' average AADT of road segments that intersect with the buffer
  cov_temp <- cov_temp %>% 
    mutate(aadt_50 = aadt_summary(pt_50, aadt, mean),
           aadt_100 = aadt_summary(pt_100, aadt, mean),
           aadt_250 = aadt_summary(pt_250, aadt, mean),
           aadt_500 = aadt_summary(pt_500, aadt, mean),
           aadt_1000 = aadt_summary(pt_1000, aadt, mean),
           aadt_2500 = aadt_summary(pt_2500, aadt, mean))
  
  #' Add this location's covariates to the data frame
  covariates <- bind_rows(covariates, cov_temp)
}

loc_covariates <- left_join(locations_sf, covariates, by = "filter_id") %>%
  dplyr::select(filter_id, campaign, elevation_50:aadt_2500)
print(sum(duplicated(loc_covariates$filter_id)))

# loc_covariates2 <- left_join(central_sf, covariates, by = "filter_id") %>%
#   dplyr::select(filter_id, campaign, elevation_50:aadt_2500)
# loc_covariates <- rbind(loc_covariates, loc_covariates2)

covariates_file_name <- "Spatial_Covariates_Filters_AEA.csv"
st_write(loc_covariates, dsn = here::here("Data", covariates_file_name),
         delete_dsn = T, layer_options = "GEOMETRY=AS_WKT")
