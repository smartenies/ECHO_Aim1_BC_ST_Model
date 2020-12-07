#' -----------------------------------------------------------------------------
#' Date created: December 12, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: Combine EPA pre-formatted and AQS data mart data to have a full dataset
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)

#' Coordinate reference systems 
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

aqs_path <- here::here("Secondary_Data", "EPA_Air_Quality_System_Data")

#' -----------------------------------------------------------------------------
#' Read in the MET datasets and simplify 
#' -----------------------------------------------------------------------------

#met_vars <- c("TEMP")
met_vars <- c("PRESS", "RH_DP", "TEMP", "WIND")

for (met in 1:length(met_vars)) {
  print(paste(met, "of", length(met_vars), "variables"))
  
  file_list <- list.files(aqs_path, pattern = met_vars[met])
  
  met_data <- data.frame()
  
  for (i in 1:length(file_list)) {
    temp <- read_csv(paste0(aqs_path, file_list[i]))
    colnames(temp) <- gsub(" ", "_", colnames(temp))
    temp <- filter(temp, State_Code == "08") %>% 
      mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
             Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
      mutate(monitor_id = paste0(State_Code, County_Code, Site_Num))
    
    met_data <- bind_rows(met_data, temp)
    rm(temp)
  }
  
  met_name <- paste0("Monitor_", met_vars[met], "_Data.csv")
  write_csv(met_data, paste0(aqs_path, met_name))
}

#' #' -----------------------------------------------------------------------------
#' #' Just read in the temperature dataset
#' #' -----------------------------------------------------------------------------
#' 
#' temp_data <- read_csv(paste0(aqs_path, "Monitor_TEMP_Data.csv"))
#' 
#' #' -----------------------------------------------------------------------------
#' #' Next, get the CDPHE data in similar shape
#' #' -----------------------------------------------------------------------------
#' 
#' cdphe_data <- read_csv(here::here("Data/CDPHE_AQS_Data", "TEMP_CDPHE.csv")) %>% 
#'   gather(key = "monitor_loc", value = "one_hr_conc", 
#'          -c(Parameter_Code, Date_Local, hour_MST)) %>% 
#'   mutate(Date_Local = str_pad(Date_Local, width = 8, pad = "0", side = "left")) %>% 
#'   mutate(Date_Local = as.Date(Date_Local, format = "%m%d%Y")) %>% 
#'   mutate(one_hr_conc = ifelse(is.nan(one_hr_conc), NA, one_hr_conc))
#' 
#' #' summarize to daily means
#' #' Assign a POC code of 1
#' cdphe_means <- cdphe_data %>% 
#'   select(-c(hour_MST)) %>% 
#'   group_by(monitor_loc, Parameter_Code, Date_Local) %>%
#'   dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
#'                    Max_Value = max(one_hr_conc, na.rm = T)) %>% 
#'   mutate(Units_of_Measure = "Degrees Fahrenheit",
#'          Arithmetic_Mean = ifelse(is.nan(Arithmetic_Mean), NA, Arithmetic_Mean),
#'          Max_Value = ifelse(is.infinite(Max_Value), NA, Max_Value),
#'          Sample_Duration = "24 HOUR",
#'          POC = 1) %>% 
#'   ungroup()
#' 
#' #' add monitor IDs and coordinates
#' monitors <- read_csv(here::here("Data/CDPHE_AQS_Data", "CDPHE_Monitors.csv")) %>% 
#'   rename(Longitude = lon, Latitude = lat)
#' 
#' cdphe_means <- left_join(cdphe_means, monitors, by = "monitor_loc")
#' cdphe_means <- select(cdphe_means, -c(monitor_info, monitor_loc)) %>% 
#'   filter(!is.na(Arithmetic_Mean)) %>% 
#'   filter(!is.na(monitor_id))
#' 
#' glimpse(cdphe_means)
#' write_csv(cdphe_means, here::here("Data/CDPHE_AQS_Data", "TEMP_Daily_CDPHE.csv"))
#' 
#' #' -----------------------------------------------------------------------------
#' #' Fill in missing AQS data with CDPHE data
#' #' -----------------------------------------------------------------------------
#' 
#' #' Get dates from AQS data
#' temp_dates <- unique(temp_data$Date_Local)
#' 
#' #' Filter CDPHE data to exclude AQS dates
#' cdphe_means <- filter(cdphe_means, !(Date_Local %in% temp_dates))
#' unique(cdphe_means$Date_Local)
#' 
#' #' combined the datasets
#' monitor_data <- bind_rows(temp_data, cdphe_means)
#' 
#' #' monitor check
#' length(unique(temp_data$monitor_id))
#' length(unique(cdphe_means$monitor_id))
#' length(unique(monitor_data$monitor_id))
#' 
#' setdiff(monitor_data$monitor_id, temp_data$monitor_id)
#' 
#' #' make the monitor data spatial
#' monitor_data <- monitor_data %>%  
#'   st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>% 
#'   st_transform(crs = albers)
#' 
#' st_write(monitor_data, here::here("Data", "Monitor_TEMP_Data_AEA.csv"),
#'          layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
#' 
#' #' -----------------------------------------------------------------------------
#' #' Now do the relative humidity dataset
#' #' -----------------------------------------------------------------------------
#' 
#' rh_data <- read_csv(paste0(aqs_path, "Monitor_RH_DP_Data.csv"))
#' 
#' #' -----------------------------------------------------------------------------
#' #' Next, get the CDPHE data in similar shape
#' #' -----------------------------------------------------------------------------
#' 
#' cdphe_data <- read_csv(here::here("Data/CDPHE_AQS_Data", "RH_DP_CDPHE.csv")) %>% 
#'   gather(key = "monitor_loc", value = "one_hr_conc", 
#'          -c(Parameter_Code, Date_Local, hour_MST)) %>% 
#'   mutate(Date_Local = str_pad(Date_Local, width = 8, pad = "0", side = "left")) %>% 
#'   mutate(Date_Local = as.Date(Date_Local, format = "%m%d%Y")) %>% 
#'   mutate(one_hr_conc = ifelse(is.nan(one_hr_conc), NA, one_hr_conc))
#' 
#' #' summarize to daily means
#' #' Assign a POC code of 1
#' cdphe_means <- cdphe_data %>% 
#'   select(-c(hour_MST)) %>% 
#'   group_by(monitor_loc, Parameter_Code, Date_Local) %>%
#'   dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
#'                    Max_Value = max(one_hr_conc, na.rm = T)) %>% 
#'   mutate(Units_of_Measure = "Percent relative humidity",
#'          Arithmetic_Mean = ifelse(is.nan(Arithmetic_Mean), NA, Arithmetic_Mean),
#'          Max_Value = ifelse(is.infinite(Max_Value), NA, Max_Value),
#'          Sample_Duration = "24 HOUR",
#'          POC = 1) %>% 
#'   ungroup()
#' 
#' #' add monitor IDs and coordinates
#' monitors <- read_csv(here::here("Data/CDPHE_AQS_Data", "CDPHE_Monitors.csv")) %>% 
#'   rename(Longitude = lon, Latitude = lat)
#' 
#' cdphe_means <- left_join(cdphe_means, monitors, by = "monitor_loc")
#' cdphe_means <- select(cdphe_means, -c(monitor_info, monitor_loc)) %>% 
#'   filter(!is.na(Arithmetic_Mean)) %>% 
#'   filter(!is.na(monitor_id))
#' 
#' glimpse(cdphe_means)
#' write_csv(cdphe_means, here::here("Data/CDPHE_AQS_Data", "RH_Daily_CDPHE.csv"))
#' 
#' #' -----------------------------------------------------------------------------
#' #' Fill in missing AQS data with CDPHE data
#' #' -----------------------------------------------------------------------------
#' 
#' #' Get dates from AQS data
#' rh_dates <- unique(rh_data$Date_Local)
#' 
#' #' Filter CDPHE data to exclude AQS dates
#' cdphe_means <- filter(cdphe_means, !(Date_Local %in% rh_dates))
#' unique(cdphe_means$Date_Local)
#' 
#' #' combined the datasets
#' monitor_data <- bind_rows(rh_data, cdphe_means)
#' 
#' #' monitor check
#' length(unique(rh_data$monitor_id))
#' length(unique(cdphe_means$monitor_id))
#' length(unique(monitor_data$monitor_id))
#' 
#' setdiff(monitor_data$monitor_id, rh_data$monitor_id)
#' 
#' #' make the monitor data spatial
#' monitor_data <- monitor_data %>%  
#'   st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>% 
#'   st_transform(crs = albers)
#' 
#' st_write(monitor_data, here::here("Data", "Monitor_RH_DP_Data_AEA.csv"),
#'          layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
#' 
#' #' -----------------------------------------------------------------------------
#' #' Next up, wind speed
#' #' -----------------------------------------------------------------------------
#' 
#' wind_data <- read_csv(paste0(aqs_path, "Monitor_WIND_Data.csv"))
#' 
#' #' -----------------------------------------------------------------------------
#' #' Next, get the CDPHE data in similar shape
#' #' -----------------------------------------------------------------------------
#' 
#' cdphe_data <- read_csv(here::here("Data/CDPHE_AQS_Data", "WIND_CDPHE.csv")) %>% 
#'   gather(key = "monitor_loc", value = "one_hr_conc", 
#'          -c(Parameter_Code, Date_Local, hour_MST)) %>% 
#'   mutate(Date_Local = str_pad(Date_Local, width = 8, pad = "0", side = "left")) %>% 
#'   mutate(Date_Local = as.Date(Date_Local, format = "%m%d%Y")) %>% 
#'   mutate(one_hr_conc = ifelse(is.nan(one_hr_conc), NA, one_hr_conc)) %>% 
#'   
#'   #' CDPHE data are in mph, EPA data are in knots
#'   #' 1 knot = 1.2 mph
#'   mutate(one_hr_conc = one_hr_conc * (1/1.2))
#' 
#' #' summarize to daily means
#' #' Assign a POC code of 1
#' cdphe_means <- cdphe_data %>% 
#'   select(-c(hour_MST)) %>% 
#'   group_by(monitor_loc, Parameter_Code, Date_Local) %>%
#'   dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
#'                    Max_Value = max(one_hr_conc, na.rm = T)) %>% 
#'   mutate(Units_of_Measure = "Wind speed (knots)",
#'          Arithmetic_Mean = ifelse(is.nan(Arithmetic_Mean), NA, Arithmetic_Mean),
#'          Max_Value = ifelse(is.infinite(Max_Value), NA, Max_Value),
#'          Sample_Duration = "24 HOUR",
#'          POC = 1) %>% 
#'   ungroup()
#' 
#' #' add monitor IDs and coordinates
#' monitors <- read_csv(here::here("Data/CDPHE_AQS_Data", "CDPHE_Monitors.csv")) %>% 
#'   rename(Longitude = lon, Latitude = lat)
#' 
#' cdphe_means <- left_join(cdphe_means, monitors, by = "monitor_loc")
#' cdphe_means <- select(cdphe_means, -c(monitor_info, monitor_loc)) %>% 
#'   filter(!is.na(Arithmetic_Mean)) %>% 
#'   filter(!is.na(monitor_id))
#' 
#' glimpse(cdphe_means)
#' write_csv(cdphe_means, here::here("Data/CDPHE_AQS_Data", "WIND_Daily_CDPHE.csv"))
#' 
#' #' -----------------------------------------------------------------------------
#' #' Fill in missing AQS data with CDPHE data
#' #' -----------------------------------------------------------------------------
#' 
#' #' Get dates from AQS data
#' wind_dates <- unique(wind_data$Date_Local)
#' 
#' #' Filter CDPHE data to exclude AQS dates
#' cdphe_means <- filter(cdphe_means, !(Date_Local %in% wind_dates))
#' unique(cdphe_means$Date_Local)
#' 
#' #' combined the datasets
#' monitor_data <- bind_rows(wind_data, cdphe_means)
#' 
#' #' monitor check
#' length(unique(wind_data$monitor_id))
#' length(unique(cdphe_means$monitor_id))
#' length(unique(monitor_data$monitor_id))
#' 
#' setdiff(monitor_data$monitor_id, wind_data$monitor_id)
#' 
#' #' make the monitor data spatial
#' monitor_data <- monitor_data %>%  
#'   st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>% 
#'   st_transform(crs = albers)
#' 
#' st_write(monitor_data, here::here("Data", "Monitor_WIND_Data_AEA.csv"),
#'          layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
#' 
