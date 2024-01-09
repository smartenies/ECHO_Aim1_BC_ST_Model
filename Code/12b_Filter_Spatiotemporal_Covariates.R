#' =============================================================================
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Spatial predictors at sampling sites
#' Date created: January 2, 2019
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description:
#' This script summarizes the spatiotemporal covariates at each of the sampled 
#' locations
#' 
#' Updated 05.11.20:
#' In order to fit the long-term ST model, I need ST covariates that go back 
#' to at least 2009. This updated version of the script attempts to achieve that.
#' This script averages the ST covariates for the week in which samples were 
#' collected using the first day of the week as the "indicator" date
#' 
#' Update 05.10.21: I am updating the datasets to reflect the corrected smoke
#' data set that includes observations for a period when the CDPHE data
#' were not available
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
library(writexl)
library(gstat)

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Read in the ST covariate data
#' -----------------------------------------------------------------------------

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

#' PM2.5 concentrations
pm_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv"),
                    guess_max = 100000) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  filter(Sample_Duration != "1 HOUR") %>% 
  arrange(Date_Local, monitor_id)

#' Black carbon
bc_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv"),
                    guess_max = 100000) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' Temperature
temp_data <- read_csv(here::here("Data", "Monitor_TEMP_Data_AEA.csv"),
                      guess_max = 200000) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers)%>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' NO2
no2_data <- read_csv(here::here("Data", "Monitor_NO2_Data_AEA.csv"),
                     guess_max = 100000) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' Smoke days
#smoke_data <- read_csv(here::here("Data", "Monitor_Smoke_Days_AEA.csv")) %>%
smoke_data <- read_csv(here::here("Data", "Monitor_Smoke_Days_Updated_AEA.csv"),
                      guess_max = 100000) %>%
  filter(!is.na(smoke_day_1sd)) %>% 
  filter(!is.na(monitor_id)) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>%
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)
table(smoke_data$smoke_day_2sd)

#' -----------------------------------------------------------------------------
#' Spatiotemporal covariates for each unique sampling location
#' Need weekly estimates from 2009-2020
#' Use each monday date as the "indicator" date
#' -----------------------------------------------------------------------------

campaign_names <- paste0("Campaign", c(1,2,3,4,5,"X"))
covariate_names <- c("pm", "bc", "no2", "temp")
covariate_list <- list(pm_data, bc_data, no2_data, temp_data)
names(covariate_list) <- covariate_names

#' Spatial data for each sampling location 
locations_sf <- read_csv(here::here("Data/Filter_Data", "Filter_Locations_AEA.csv")) %>% 
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) 

#' Use the 9th and Yuma location for the central site monitor as well (co-located)
central_sf <- filter(locations_sf, Location == "9th and Yuma") %>% 
  slice(1) 
central_sf$filter_id[1] <- "080310027"
central_sf$campaign[1] <- "CampaignX"

locations_sf <- rbind(locations_sf, central_sf)

plot(st_geometry(locations_sf))

print(sum(duplicated(locations_sf$filter_id)))

#' Unique locations
unique_locations <- select(locations_sf, site_id) %>%
  distinct()

#' -----------------------------------------------------------------------------
#' For each filter, summarize the following:
#'     - PM, BC, NO2 and Temp of the closest monitor (nn)
#'     - PM, BC, NO2 and Temp estimated from IDW of all monitors in the area
#'     - Smoke day at the closest monitor (nn_smoke_1sd) based on a 1 sd increase
#'     - Smoke day at the closest monitor (nn_smoke_2sd) based on a 2 sd increase
#'     - Smoke day at any area monitor (area_smoke_1sd) based on a 1 sd increase
#'     - Smoke day at any area monitor (area_smoke_2sd) based on a 2 sd increase
#' -----------------------------------------------------------------------------

# filter_ids <- unique(locations_sf$filter_id)
site_ids <- unique_locations$site_id

#' List of dates 
#' Use start of the week as the "indicator" date
date_start <- as.Date("01/01/2009", format = "%m/%d/%Y")
date_end <- as.Date("12/31/2020", format = "%m/%d/%Y")
date_list_all <- seq.Date(date_start, date_end, by = "day")
date_list <- unique(as.Date(cut(as.Date(date_list_all), "week")))

cov_temp <- data.frame()
options(dplyr.summarise.inform = F)

for (i in 1:length(site_ids)) {
  print(paste("Location", i, "of", length(site_ids)))
  #point <- filter(locations_sf, filter_id == filter_ids[i])
  point <- filter(unique_locations, site_id == site_ids[i])
  
  temp <- data.frame()
  
  for(j in 1:length(date_list)) {
    if(j %% 10 == 0) print(paste("Date", j, "of", length(date_list)))
    
    start_date <- date_list[j]
    end_date <- date_list[j] + 6
    date_seq <- seq.Date(start_date, end_date, by = "day")
    
    # temp <- select(point, filter_id, campaign) %>% 
    #   st_set_geometry(NULL)
    temp2 <- data.frame(site_id = point$site_id,
                        week = date_list[j])
    
    #' loop through PM, BC, NO2, and temperature covariates
    for (k in 1:length(covariate_names)) {
      
      cov_df <- covariate_list[[covariate_names[k]]]
      
      cov_week <- filter(cov_df, Date_Local %in% date_seq)
      
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
          slice(1:3)
        cov_ranked <- filter(cov_week, monitor_id %in% cov_rankings$monitor_id) %>%
          arrange(distance)
        
        cov_nn_means <- filter(cov_ranked, monitor_id == cov_rankings$monitor_id[1]) %>%
          group_by(monitor_id, distance) %>%
          summarize(weekly_mean = mean(Arithmetic_Mean, na.rm=T)) %>%
          arrange(distance)
        
        cov_nn3_means <- cov_ranked %>%
          group_by(monitor_id, distance) %>%
          summarize(weekly_mean = mean(Arithmetic_Mean, na.rm=T)) %>%
          arrange(distance)
        
        cov_area_means <- cov_week %>%
          group_by(monitor_id) %>%
          summarize(weekly_mean = mean(Arithmetic_Mean, na.rm=T))
        
        temp2$nn <- cov_nn_means$weekly_mean[1]
        temp2$nn3 <- mean(cov_nn3_means$weekly_mean, na.rm = T)
        temp2$area <- mean(cov_area_means$weekly_mean, na.rm=T)
        
        #' Get the value at the distributed site based on IDW of all central sites
        idw_val <- idw(weekly_mean ~ 1, cov_area_means, point, debug.level = 0)
        
        temp2$idw <- idw_val$var1.pred
        temp2$units <- unique(cov_ranked$Units_of_Measure)
        
        rm(cov_rankings, cov_ranked, cov_nn_means, cov_nn3_means, cov_area_means)
        
      } else {
        temp2$nn <- NA
        temp2$nn3 <- NA
        temp2$area <- NA
        temp2$idw <- NA
        temp2$units <- NA
      }
      
      colnames(temp2)[(ncol(temp2)-4):ncol(temp2)] <- 
        paste(colnames(temp2)[(ncol(temp2)-4):ncol(temp2)], covariate_names[k], 
              sep="_")
      
      rm(cov_df, cov_week)
    }
    
    #' Now add smoke days
    smoke_week <- filter(smoke_data,  Date_Local %in% date_seq)
    smoke_days_area <-  filter(smoke_week, County_Code %in% counties)
    
    if(nrow(smoke_week) > 0 & nrow(smoke_days_area) > 0) {
      smoke_week <- smoke_week %>% 
        #' st_distance calculates distance between points
        #' because we only have one point in point, the returned matrix
        #' only has one column
        #' Modified from here:
        #' https://stackoverflow.com/questions/49853696/distances-of-points-between-rows-with-sf
        mutate(distance = unclass(st_distance(st_geometry(.), st_geometry(point), by_element = T))) %>% 
        arrange(distance)
      
      smoke_days_area <-  smoke_week %>% 
        filter(County_Code %in% counties) %>%
        summarize(smoke_day_1sd = ifelse(any(smoke_day_1sd == 1), 1, 0),
                  smoke_day_2sd = ifelse(any(smoke_day_2sd == 1), 1, 0))
      
      smoke_rankings <- select(as.data.frame(smoke_week), monitor_id, distance) %>%
        arrange(distance) %>%
        distinct() %>%
        slice(1:3)
      smoke_ranked <- filter(smoke_week, monitor_id %in% smoke_rankings$monitor_id) %>%
        arrange(distance)
      
      smoke_days_nn <- filter(smoke_ranked, monitor_id == smoke_rankings$monitor_id[1]) %>%
        group_by(monitor_id, distance) %>%
        summarize(smoke_day_1sd = ifelse(any(smoke_day_1sd == 1), 1, 0),
                  smoke_day_2sd = ifelse(any(smoke_day_2sd == 1), 1, 0)) %>%
        arrange(distance)
      
      smoke_days_nn3 <- smoke_ranked %>%
        group_by(monitor_id, distance) %>%
        summarize(smoke_day_1sd = ifelse(any(smoke_day_1sd == 1), 1, 0),
                  smoke_day_2sd = ifelse(any(smoke_day_2sd == 1), 1, 0)) %>%
        arrange(distance)
      
      temp2$nn_smoke_1sd <- smoke_days_nn$smoke_day_1sd[1]
      temp2$nn_smoke_2sd <- smoke_days_nn$smoke_day_2sd[1]
      temp2$nn3_smoke_1sd <- ifelse(any(smoke_days_nn3$smoke_day_2sd == 1), 1, 0)
      temp2$nn3_smoke_2sd <- ifelse(any(smoke_days_nn3$smoke_day_2sd == 1), 1, 0)
      temp2$area_smoke_1sd <- smoke_days_area$smoke_day_1sd[1]
      temp2$area_smoke_2sd <- smoke_days_area$smoke_day_2sd[1]
      temp2$units_smoke <- "yes=1/no=0"
      
      rm(smoke_rankings, smoke_ranked, smoke_days_nn, smoke_days_nn3)
      
    } else {
      temp2$nn_smoke_1sd <- NA
      temp2$nn_smoke_2sd <- NA
      temp2$nn3_smoke_1sd <- NA
      temp2$nn3_smoke_2sd <- NA
      temp2$area_smoke_1sd <- NA
      temp2$area_smoke_2sd <- NA
      temp2$units_smoke <- "yes=1/no=0"
    } 
    
    temp <- bind_rows(temp, temp2)
    rm(smoke_week, smoke_week_area, temp2)
  }
  
  cov_temp <- bind_rows(cov_temp, temp)
  rm(temp)
}
options(warn=0)
options(dplyr.summarise.inform = T)

glimpse(cov_temp)
summary(cov_temp)

covariates_file_name1 <- paste0("ST_Covariates_Sites_Updated_AEA.csv")
st_write(cov_temp, dsn = here::here("Data", covariates_file_name1),
         delete_dsn = T, layer_options = "GEOMETRY=AS_WKT")

locations_cov <- left_join(cov_temp, locations_sf, by = "site_id") %>%
  dplyr::select(filter_id, campaign, site_id, st_week = week, nn_pm:units_smoke) %>%
  select(-c(area_bc:idw_bc)) %>%
  mutate(filter_id = ifelse(str_detect(filter_id, "080310027"), "080310027",
                            filter_id))

covariates_file_name2 <- paste0("ST_Covariates_Filters_Updated_AEA.csv")
st_write(locations_cov, dsn = here::here("Data", covariates_file_name2),
         delete_dsn = T, layer_options = "GEOMETRY=AS_WKT")

#' Check updated smoke and NO2 variables
locations_cov_orig <- read_csv(here::here(here::here("Data", "ST_Covariates_Filters_AEA.csv")),
                               guess_max = 100000)
locations_cov_update <- read_csv(here::here(here::here("Data", "ST_Covariates_Filters_Updated_AEA.csv")),
                                 guess_max = 100000)

summary(locations_cov_orig$idw_no2)
summary(locations_cov_update$idw_no2)

table(locations_cov_orig$area_smoke_2sd)
table(locations_cov_update$area_smoke_2sd)
