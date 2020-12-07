#' =============================================================================
#' Project: ECHO LUR
#' Date created: January 2, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script summarizes the spatiotemporal covariates at each of the prediction
#' locations
#' 
#' WARNING: THIS CODE CAN TAKE A LONG TIME TO RUN, EVEN WITH THE DOPARALLEL LOOP
#' --NEED TO COME UP WITH A PLAN: CLUSTER, VECTORIZED, ETC.
#' 
#' #' UPDATE 9/8/19: This script can now be run on the CVMBS server using RStudio
#' Server
#' =============================================================================

list_of_packages <- c("sf", "raster", "ggplot2", "ggmap", "ggsn", "ggthemes",
                      "stringr", "tidyverse", "lubridate", "writexl", "readxl",
                      "gstat")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)>0) install.packages(new_packages, dependencies = T)

rm(list_of_packages, new_packages)

start_time <- Sys.time()

library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggthemes)
library(stringr)
library(tidyverse)
library(lubridate)
library(writexl)
library(gstat)

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Read in the ST covariate data
#' Filter to only use data from the local monitoring network
#' Use months and years from the campaign data
#' -----------------------------------------------------------------------------

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

#' PM2.5 concentrations
# pm_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) %>% 
pm_data <- read_csv(here::here("echo_aim_1/Data", "Monitor_PM_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  filter(County_Code %in% counties) %>% 
  filter(Sample_Duration != "1 HOUR") %>% 
  arrange(Date_Local, monitor_id)

#' Black carbon
# bc_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv")) %>% 
bc_data <- read_csv(here::here("echo_aim_1/Data", "Monitor_BC_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' Temperature
# temp_data <- read_csv(here::here("Data", "Monitor_TEMP_Data_AEA.csv")) %>% 
temp_data <- read_csv(here::here("echo_aim_1/Data", "Monitor_TEMP_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers)%>% 
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' NO2
# no2_data <- read_csv(here::here("Data", "Monitor_NO2_Data_AEA.csv")) %>% 
no2_data <- read_csv(here::here("echo_aim_1/Data", "Monitor_NO2_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers)%>% 
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' Smoke days
# smoke_data <- read_csv(here::here("Data", "Monitor_Smoke_Days_AEA.csv")) %>%
smoke_data <- read_csv(here::here("echo_aim_1/Data", "Monitor_Smoke_Days_AEA.csv")) %>%
  filter(!is.na(smoke_day_1sd)) %>% 
  filter(!is.na(monitor_id)) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' -----------------------------------------------------------------------------
#' Spatiotemporal covariates for each prediction location
#' going to do each week from 2009 to mid-2019
#' -----------------------------------------------------------------------------

campaign_names <- c("Campaign1", "Campaign2", "Campaign3", "Campaign4")
covariate_names <- c("pm", "bc", "no2", "temp")
covariate_list <- list(pm_data, bc_data, no2_data, temp_data)
names(covariate_list) <- covariate_names

start_date <- as.Date("01/01/2009", format = "%m/%d/%Y")
end_date <- as.Date("2019-07-31", format = "%Y-%m-%d")
date_list1 <- seq.Date(start_date, end_date, by = "day")
date_list2 <- as.Date(cut(sort(date_list1), "week"))

week_date_list <- unique(date_list2) 

#' Spatial data for each sampling location
locations_file_name <- "Grid_250_m_AEA.csv"

# locations_sf <- read_csv(here::here("Data", locations_file_name)) %>%
#   st_as_sf(wkt = "WKT", crs = albers)
locations_sf <- read_csv(here::here("echo_aim_1/Data", locations_file_name)) %>%
  st_as_sf(wkt = "WKT", crs = albers)

plot(st_geometry(locations_sf))

#' ---------------------------------------------------------------------------
#' For each grid location and month, summarize the following:
#'     - PM, BC, NO2 and Temp of the closest monitor (nn)
#'     - PM, BC, NO2 and Temp estimated from IDW of all monitors in the area
#'     - Smoke day at the closest monitor (nn_smoke_1sd) based on a 1 sd increase
#'     - Smoke day at the closest monitor (nn_smoke_2sd) based on a 2 sd increase
#'     - Smoke day at any area monitor (area_smoke_1sd) based on a 1 sd increase
#'     - Smoke day at any area monitor (area_smoke_2sd) based on a 2 sd increase
#' ---------------------------------------------------------------------------

spatiotemp_cov <- function(i, df = locations_sf) {
  grid_ids <- unique(locations_sf$grid_id)
  point <- filter(locations_sf, grid_id == grid_ids[i])

  cov_temp <- data.frame()
  
  #' loop through weeks
  for(j in 1:length(week_date_list)) {
      
    temp <- data.frame(grid_id = grid_ids[i],
                       week_id = week_date_list[j])
    
    temp_start_date <- week_date_list[j]
    temp_end_date <- week_date_list[j] + 6
    temp_date_seq <- seq.Date(temp_start_date, temp_end_date, by = "day")
    
    #' loop through PM, BC, and temperature covariates
    for (k in 1:length(covariate_names)) {
      cov_df <- covariate_list[[covariate_names[k]]]
      
      cov_week <- filter(cov_df, Date_Local %in% temp_date_seq)
      
      if(nrow(cov_week) > 0) {
        
        #' Get the mean value from the closest monitor
        #' Get the area mean value from all monitors
        cov_week <- cov_week %>% 
          #' st_distance calculates distance between points
          #' because we only have one point in point, the returned matrix
          #' only has one column
          #' Modified from here:
          #' https://stackoverflow.com/questions/49853696/distances-of-points-between-rows-with-sf
          mutate(distance = unclass(st_distance(st_geometry(.), st_geometry(point), by_element = T))) %>% 
          arrange(distance)
        
        cov_rankings <- select(as.data.frame(cov_week), monitor_id, distance) %>% 
          arrange(distance) %>% 
          distinct() %>% 
          slice(1)
        cov_ranked <- filter(cov_week, monitor_id %in% cov_rankings$monitor_id) %>% 
          arrange(distance)
        
        cov_means <- cov_ranked %>% 
          group_by(monitor_id, distance) %>% 
          summarize(weekly_mean = mean(Arithmetic_Mean, na.rm=T)) %>% 
          arrange(distance)
        
        temp$nn <- cov_means$weekly_mean[1]
        temp$area <- mean(cov_week$Arithmetic_Mean, na.rm=T)
        
        #' Get the value at the distributed site based on IDW of all central sites
        idw_val <- idw(Arithmetic_Mean ~ 1, cov_week, point)
        
        temp$idw <- idw_val$var1.pred
        temp$units <- unique(cov_ranked$Units_of_Measure)
        
        rm(cov_rankings, cov_ranked, cov_means)
        
      } else {
        temp$nn <- NA
        temp$area <- NA
        temp$idw <- NA
        temp$units <- NA
      }
      
      colnames(temp)[(ncol(temp)-3):ncol(temp)] <- 
        paste(colnames(temp)[(ncol(temp)-3):ncol(temp)], covariate_names[k], 
              sep="_")
      
      rm(cov_df, cov_week)
    }
      
    #' Now add smoke days
    smoke_week <- filter(smoke_data,  Date_Local %in% temp_date_seq)
    
    if(nrow(smoke_week) > 0) {
      smoke_week <- smoke_week %>% 
        #' st_distance calculates distance between points
        #' because we only have one point in point, the returned matrix
        #' only has one column
        #' Modified from here:
        #' https://stackoverflow.com/questions/49853696/distances-of-points-between-rows-with-sf
        mutate(distance = unclass(st_distance(st_geometry(.), st_geometry(point), by_element = T))) %>% 
        arrange(distance)
      
      smoke_days_area <-  smoke_week %>% 
        summarize(smoke_day_1sd = ifelse(any(smoke_day_1sd == 1), 1, 0),
                  smoke_day_2sd = ifelse(any(smoke_day_2sd == 1), 1, 0))
      
      smoke_rankings <- select(as.data.frame(smoke_week), monitor_id, distance) %>% 
        arrange(distance) %>% 
        distinct() %>% 
        slice(1)
      smoke_ranked <- filter(smoke_week, monitor_id %in% smoke_rankings$monitor_id) %>% 
        arrange(distance)
      
      smoke_days_nn <- smoke_ranked %>% 
        group_by(monitor_id, distance) %>% 
        summarize(smoke_day_1sd = ifelse(any(smoke_day_1sd == 1), 1, 0),
                  smoke_day_2sd = ifelse(any(smoke_day_2sd == 1), 1, 0)) %>% 
        arrange(distance)
      
      temp$nn_smoke_1sd <- smoke_days_nn$smoke_day_1sd[1]
      temp$nn_smoke_2sd <- smoke_days_nn$smoke_day_2sd[1]
      temp$area_smoke_1sd <- smoke_days_area$smoke_day_1sd[1]
      temp$area_smoke_2sd <- smoke_days_area$smoke_day_2sd[1]
      temp$units_smoke <- "yes=1/no=0"
      
      rm(smoke_rankings, smoke_ranked, smoke_days_area, smoke_days_nn)
      
    } else {
      temp$nn_smoke_1sd <- NA
      temp$nn_smoke_2sd <- NA
      temp$area_smoke_1sd <- NA
      temp$area_smoke_2sd <- NA
      temp$units_smoke <- "yes=1/no=0"
    }  
    
    cov_temp <- bind_rows(cov_temp, temp)
    rm(smoke_week, temp)
  }
  return(cov_temp)
}

test <- spatiotemp_cov(i = 1)

#' Run this in parallel to speed it up
library(foreach)
library(doParallel)

cores <- detectCores()
cl <- makeCluster(cores[1]-50) 
registerDoParallel(cl)

package_list <- c("sf", "raster", "ggplot2", "ggmap", "ggsn", "ggthemes",
                  "stringr", "tidyverse", "lubridate", "writexl", "readxl",
                  "gstat")

covariates <- foreach(i=1:nrow(locations_sf), .combine = rbind,
                      .packages = package_list) %dopar% {
                        
  cov_temp <- spatiotemp_cov(i)
}

stopCluster(cl)

locations_cov <- left_join(covariates, locations_sf, by = "grid_id")

covariates_file_name <- "ST_Covariates_Grid_250_m_AEA.csv"

st_write(locations_cov, dsn = here::here("echo_aim_1/Data", covariates_file_name),
         delete_dsn = T, layer_options = "GEOMETRY=AS_WKT")

Sys.time() - start_time
















#' for (i in 1:length(grid_ids)) {
#'   print(paste("Grid cell", i, "of", length(grid_ids)))
#'   point <- filter(locations_sf, grid_id == grid_ids[i])
#'   
#'   #' loop through month_yrs
#'   for(j in 1:length(month_yr_list)) {
#'     
#'     temp <- data.frame(grid_id = grid_ids[i],
#'                        month = str_split(month_yr_list[j], "_")[[1]][[1]],
#'                        year = str_split(month_yr_list[j], "_")[[1]][[2]],
#'                        month_yr = month_yr_list[j])
#'     ncol_minus <- 2
#'     
#'     #' loop through PM, BC, and temperature covariates
#'     for (k in 1:length(covariate_names)) {
#'       cov_df <- covariate_list[[covariate_names[k]]]
#'       
#'       cov_month <- filter(cov_df, month_yr == month_yr_list[j]) %>% 
#'         
#'         #' st_distance calculates distance between points
#'         #' because we only have one point in point, the returned matrix
#'         #' only has one column
#'         #' Modified from here:
#'         #' https://stackoverflow.com/questions/49853696/distances-of-points-between-rows-with-sf
#'         mutate(distance = unclass(st_distance(st_geometry(.), st_geometry(point), by_element = T))) %>% 
#'         arrange(distance)
#'       
#'       if(nrow(cov_month >0)) {
#'         cov_rankings <- select(as.data.frame(cov_month), monitor_id, distance) %>% 
#'           arrange(distance) %>% 
#'           distinct() %>% 
#'           slice(1:3)
#'         cov_ranked <- filter(cov_month, monitor_id %in% cov_rankings$monitor_id) %>% 
#'           arrange(distance)
#'         
#'         cov_means <- cov_ranked %>% 
#'           group_by(monitor_id, distance) %>% 
#'           summarize(monthly_mean = mean(Arithmetic_Mean)) %>% 
#'           arrange(distance)
#'         
#'         temp$nearest_neighbor <- cov_means$monthly_mean[1]
#'         temp$nearest_3_neighbors <- mean(cov_means$monthly_mean)
#'         temp$Units_of_Measure <- unique(cov_ranked$Units_of_Measure)
#'       } else {
#'         temp$nearest_neighbor <- NA
#'         temp$nearest_3_neighbors <- NA
#'         temp$Units_of_Measure <- NA
#'       }
#'       
#'       colnames(temp)[(ncol(temp)-ncol_minus):ncol(temp)] <- 
#'         paste(colnames(temp)[(ncol(temp)-ncol_minus):ncol(temp)], covariate_names[k], 
#'               sep="_")
#'       
#'       rm(cov_df, cov_month, cov_rankings, cov_ranked, cov_means)
#'     }
#'     
#'     #' Now add smoke days
#'     smoke_month <- filter(smoke_data, month_yr == month_yr_list[j]) %>% 
#'       
#'       #' st_distance calculates distance between points
#'       #' because we only have one point in point, the returned matrix
#'       #' only has one column
#'       #' Modified from here:
#'       #' https://stackoverflow.com/questions/49853696/distances-of-points-between-rows-with-sf
#'       mutate(distance = unclass(st_distance(st_geometry(.), st_geometry(point), by_element = T))) %>% 
#'       arrange(distance)
#'     
#'     smoke_rankings <- select(as.data.frame(smoke_month), monitor_id, distance) %>% 
#'       arrange(distance) %>% 
#'       distinct() %>% 
#'       slice(1:3)
#'     smoke_ranked <- filter(smoke_month, monitor_id %in% smoke_rankings$monitor_id) %>% 
#'       arrange(distance)
#'     
#'     smoke_days <- smoke_ranked %>% 
#'       group_by(monitor_id, distance) %>% 
#'       summarize(smoke_day = ifelse(any(smoke_day == 1), 1, 0)) %>% 
#'       arrange(distance)
#'     
#'     temp$nearest_neighbor_smoke <- smoke_days$smoke_day[1]
#'     temp$nearest_3_neighbors_smoke <- ifelse(any(smoke_days$smoke_day == 1), 1, 0)
#'     temp$Units_of_Measure_smoke <- "yes=1/no=0"
#'     
#'     cov_temp <- bind_rows(cov_temp, temp)
#'     
#'     rm(smoke_month, smoke_rankings, smoke_ranked, smoke_days)
#'   }
#' }
#' 
#' locations_cov <- left_join(locations_sf, cov_temp, by = c("grid_id", "month_yr"))
#' 
#' covariates_file_name <- paste0("Spatiotemporal_Covariates_", campaign_name, "_AEA.csv")
#' st_write(locations_cov, dsn = here::here("Data", covariates_file_name),
#'          delete_dsn = T, layer_options = "GEOMETRY=AS_WKT")