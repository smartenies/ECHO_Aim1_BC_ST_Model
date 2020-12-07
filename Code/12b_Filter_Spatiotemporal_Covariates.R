#' =============================================================================
#' Project: ECHO LUR
#' Date created: January 2, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script summarizes the spatiotemporal covariates at each of the sampled 
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
pm_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  filter(County_Code %in% counties) %>% 
  filter(Sample_Duration != "1 HOUR") %>% 
  arrange(Date_Local, monitor_id)

#' Black carbon
bc_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' Temperature
temp_data <- read_csv(here::here("Data", "Monitor_TEMP_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers)%>% 
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' NO2
no2_data <- read_csv(here::here("Data", "Monitor_NO2_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers)%>% 
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' Smoke days
smoke_data <- read_csv(here::here("Data", "Monitor_Smoke_Days_AEA.csv")) %>%
  filter(!is.na(smoke_day_1sd)) %>% 
  filter(!is.na(monitor_id)) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' -----------------------------------------------------------------------------
#' Spatiotemporal covariates for each sampling location
#' -----------------------------------------------------------------------------

campaign_names <- c("Campaign1", "Campaign2", "Campaign3", "Campaign4")
covariate_names <- c("pm", "bc", "no2", "temp")
covariate_list <- list(pm_data, bc_data, no2_data, temp_data)
names(covariate_list) <- covariate_names

#' Spatial data for each sampling location 
#' Use Filter_BC file, which has locations and sampling dates
locations_file_name <- "Filter_BC.csv"

locations_all <- read_csv(here::here("Data", locations_file_name)) %>% 
  filter(!is.na(lon) & !is.na(lat)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84)  %>% 
  st_transform(crs = albers) %>% 
  filter(!is.na(month)) %>% 
  arrange(StartDateLocal)

locations_sf1 <- dplyr::select(locations_all, filter_id, campaign, 
                              StartDateLocal, EndDateLocal)

#' Create an object to hold the BC central site data as well
s_date <- as.Date(cut(as.Date(sort(bc_data$Date_Local)[1]), "week"))
e_date <- sort(bc_data$Date_Local)[nrow(bc_data)]

bc_date_seq <- seq.Date(s_date, e_date, by = "week")

#' Filter 200202 was co-located with the monitoring site
central_sf1 <- filter(locations_all, filter_id == "200202") %>% 
  dplyr::select(filter_id) 
  
central_sf1$filter_id[1] <- "080310027"

central_df <- data.frame(filter_id = as.character("080310027"),
                         campaign = as.character("CampaignX"),
                         StartDateLocal = bc_date_seq)
central_df$filter_id <- as.character(central_df$filter_id)
central_df$campaign <- as.character(central_df$campaign)
central_df$EndDateLocal <- central_df$StartDateLocal + 6

central_sf <- full_join(central_sf1, central_df, by = "filter_id")
central_sf$filter_id <- paste(as.character(central_sf$filter_id),
                              seq_along(central_sf$filter_id),
                              sep = "_")

#' Join distributed site and central site
locations_sf <- rbind(locations_sf1, central_sf)
plot(st_geometry(locations_sf))

#' -----------------------------------------------------------------------------
#' For each filter, summarize the following:
#'     - PM, BC, NO2 and Temp of the closest monitor (nn)
#'     - PM, BC, NO2 and Temp of the 3 closest monitors (nn3)
#'     - PM, BC, NO2 and Temp estimated from IDW of all monitors in the area
#'     - Smoke day at the closest monitor (nn_smoke_1sd) based on a 1 sd increase
#'     - Smoke day at the closest monitor (nn_smoke_2sd) based on a 2 sd increase
#'     - Smoke day at the closest 3 monitors (nn3_smoke_1sd) based on a 1 sd increase
#'     - Smoke day at the closest 3 monitor (nn3_smoke_2sd) based on a 2 sd increase
#'     - Smoke day at any area monitor (area_smoke_1sd) based on a 1 sd increase
#'     - Smoke day at any area monitor (area_smoke_2sd) based on a 2 sd increase
#' -----------------------------------------------------------------------------

filter_ids <- unique(locations_sf$filter_id)

# Empty data frame for covariates
cov_temp <- data.frame()

for (i in 1:length(filter_ids)) {
  print(paste("Filter", i, "of", length(filter_ids)))
  point <- filter(locations_sf, filter_id == filter_ids[i])
  # point <- filter(locations_sf, site_id == filter_ids[i])

  start_date <- point$StartDateLocal
  end_date <- point$EndDateLocal
  date_seq <- seq.Date(start_date, end_date, by = "day")

  temp <- select(point, filter_id, campaign) %>%
    st_set_geometry(NULL)

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

      temp$nn <- cov_nn_means$weekly_mean[1]
      temp$nn3 <- mean(cov_nn3_means$weekly_mean, na.rm = T)
      temp$area <- mean(cov_area_means$weekly_mean, na.rm=T)

      #' Get the value at the distributed site based on IDW of all central sites
      idw_val <- idw(weekly_mean ~ 1, cov_area_means, point)

      temp$idw <- idw_val$var1.pred
      temp$units <- unique(cov_ranked$Units_of_Measure)

      rm(cov_rankings, cov_ranked, cov_nn_means, cov_nn3_means, cov_area_means)

    } else {
      temp$nn <- NA
      temp$nn3 <- NA
      temp$area <- NA
      temp$idw <- NA
      temp$units <- NA
    }

    colnames(temp)[(ncol(temp)-4):ncol(temp)] <-
      paste(colnames(temp)[(ncol(temp)-4):ncol(temp)], covariate_names[k],
            sep="_")

    rm(cov_df, cov_week)
  }

  #' Now add smoke days
  smoke_week <- filter(smoke_data,  Date_Local %in% date_seq)

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

    temp$nn_smoke_1sd <- smoke_days_nn$smoke_day_1sd[1]
    temp$nn_smoke_2sd <- smoke_days_nn$smoke_day_2sd[1]
    temp$nn3_smoke_1sd <- ifelse(any(smoke_days_nn3$smoke_day_2sd == 1), 1, 0)
    temp$nn3_smoke_2sd <- ifelse(any(smoke_days_nn3$smoke_day_2sd == 1), 1, 0)
    temp$area_smoke_1sd <- smoke_days_area$smoke_day_1sd[1]
    temp$area_smoke_2sd <- smoke_days_area$smoke_day_2sd[1]
    temp$units_smoke <- "yes=1/no=0"

    rm(smoke_rankings, smoke_ranked, smoke_days_area, smoke_days_nn, smoke_days_nn3)

  } else {
    temp$nn_smoke_1sd <- NA
    temp$nn_smoke_2sd <- NA
    temp$nn3_smoke_1sd <- NA
    temp$nn3_smoke_2sd <- NA
    temp$area_smoke_1sd <- NA
    temp$area_smoke_2sd <- NA
    temp$units_smoke <- "yes=1/no=0"
  }

  cov_temp <- bind_rows(cov_temp, temp)
  rm(smoke_week)
}

locations_cov <- left_join(locations_sf, cov_temp, by = c("filter_id", "campaign")) %>% 
  dplyr::select(filter_id, campaign, StartDateLocal, EndDateLocal, nn_pm:units_smoke) %>% 
  select(-c(area_bc:idw_bc)) %>% 
  mutate(filter_id = ifelse(str_detect(filter_id, "080310027"), "080310027",
                            filter_id))

covariates_file_name <- paste0("ST_Covariates_Filters_AEA.csv")
st_write(locations_cov, dsn = here::here("Data", covariates_file_name),
         delete_dsn = T, layer_options = "GEOMETRY=AS_WKT")

