#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Combine EPA and CDPHE met data
#' Date created: December 12, 2018
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)

#' Coordinate reference systems 
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

aqs_path <- here::here("Secondary_Data", "EPA_Air_Quality_System_Data")

#' -----------------------------------------------------------------------------
#' Read in the MET data sets and simplify 
#' -----------------------------------------------------------------------------

met_vars <- c("PRESS", "RH_DP", "TEMP", "WIND")

for (met in 1:length(met_vars)) {
  print(paste(met, "of", length(met_vars), "variables"))
  
  file_list <- list.files(aqs_path, pattern = met_vars[met])
  file_list <- file_list[which(!(str_detect(file_list, "Monitor")))]
  
  met_data <- data.frame()
  
  for (i in 1:length(file_list)) {
    temp <- read_csv(paste0(aqs_path, "/", file_list[i]))
    colnames(temp) <- gsub(" ", "_", colnames(temp))
    temp <- filter(temp, State_Code == "08") %>% 
      mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
             Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
      mutate(monitor_id = paste0(State_Code, County_Code, Site_Num))
    
    met_data <- bind_rows(met_data, temp)
    rm(temp)
  }
  
  met_name <- paste0("Monitor_", met_vars[met], "_Data.csv")
  write_csv(met_data, paste0(aqs_path, "/", met_name))
}

#' -----------------------------------------------------------------------------
#' Just read in the temperature data set
#' -----------------------------------------------------------------------------

temp_data <- read_csv(paste0(aqs_path, "/", "Monitor_TEMP_Data.csv"))

#' -----------------------------------------------------------------------------
#' Next, get the CDPHE data in similar shape
#' -----------------------------------------------------------------------------

cdphe_mon <- read_csv(here::here("Secondary_Data/CDPHE_Air_Quality_Data", 
                                 "CDPHE_Monitors.csv")) %>%
  rename(Longitude = "lon", Latitude = "lat")

cdphe_data <- read_csv(here::here("Secondary_Data/CDPHE_Air_Quality_Data", 
                                  "TEMP_CDPHE.csv")) %>%
  pivot_longer(-c(hour_MST, Date_Local, Parameter_Code),
               names_to = "monitor_loc", values_to = "one_hr_conc") %>%
  
  #' summarize to daily means
  #' #' Assign a POC code of 1
  group_by(monitor_loc, Parameter_Code, Date_Local) %>%
  dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
                   Max_Value = max(one_hr_conc, na.rm = T)) %>%
  mutate(Units_of_Measure = "Degrees Fahrenheit",
         Arithmetic_Mean = ifelse(is.nan(Arithmetic_Mean), NA, Arithmetic_Mean),
         Max_Value = ifelse(is.infinite(Max_Value), NA, Max_Value),
         Sample_Duration = "1 HOUR",
         POC = 1) %>%
  
  #' make Date_Local a vector of dates
  mutate(Date_Local = as.Date(Date_Local, format = "%m%d%Y")) %>%
  ungroup()

#' add monitor IDs and coordinates
cdphe_means <- left_join(cdphe_data, cdphe_mon, by = "monitor_loc")
cdphe_means <- select(cdphe_means, -c(monitor_info, monitor_loc))

glimpse(cdphe_means)

#' -----------------------------------------------------------------------------
#' Fill in missing AQS data with CDPHE data
#' -----------------------------------------------------------------------------

#' Get dates from AQS data
temp_dates <- unique(temp_data$Date_Local)

#' Filter CDPHE data to exclude AQS dates
cdphe_means <- filter(cdphe_means, !(Date_Local %in% temp_dates))
unique(cdphe_means$Date_Local)

#' combined the datasets
monitor_data <- bind_rows(temp_data, cdphe_means)

#' monitor check
length(unique(temp_data$monitor_id))
length(unique(cdphe_means$monitor_id))
length(unique(monitor_data$monitor_id))

setdiff(monitor_data$monitor_id, temp_data$monitor_id)

#' Drop "080590014" and "081230013" because they aren't in the monitoring plan
drop <- c("080590014", "081230013")

#' make the monitor data spatial
monitor_data <- filter(monitor_data, !(monitor_id %in% drop)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>%
  st_transform(crs = albers)

st_write(monitor_data, here::here("Data", "Monitor_TEMP_Data_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' -----------------------------------------------------------------------------
#' Now do the relative humidity dataset
#' -----------------------------------------------------------------------------

rh_data <- read_csv(paste0(aqs_path, "/", "Monitor_RH_DP_Data.csv"))

#' -----------------------------------------------------------------------------
#' Next, get the CDPHE data in similar shape
#' -----------------------------------------------------------------------------

cdphe_mon <- read_csv(here::here("Secondary_Data/CDPHE_Air_Quality_Data", 
                                 "CDPHE_Monitors.csv")) %>%
  rename(Longitude = "lon", Latitude = "lat")

cdphe_data <- read_csv(here::here("Secondary_Data/CDPHE_Air_Quality_Data", 
                                  "RH_DP_CDPHE.csv")) %>%
  pivot_longer(-c(hour_MST, Date_Local, Parameter_Code),
               names_to = "monitor_loc", values_to = "one_hr_conc") %>%
  
  #' summarize to daily means
  #' #' Assign a POC code of 1
  group_by(monitor_loc, Parameter_Code, Date_Local) %>%
  dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
                   Max_Value = max(one_hr_conc, na.rm = T)) %>%
  mutate(Units_of_Measure = "Percent relative humidity",
         Arithmetic_Mean = ifelse(is.nan(Arithmetic_Mean), NA, Arithmetic_Mean),
         Max_Value = ifelse(is.infinite(Max_Value), NA, Max_Value),
         Sample_Duration = "1 HOUR",
         POC = 1) %>%
  
  #' make Date_Local a vector of dates
  mutate(Date_Local = as.Date(Date_Local, format = "%m%d%Y")) %>%
  ungroup()

#' add monitor IDs and coordinates
cdphe_means <- left_join(cdphe_data, cdphe_mon, by = "monitor_loc")
cdphe_means <- select(cdphe_means, -c(monitor_info, monitor_loc))

glimpse(cdphe_means)

#' -----------------------------------------------------------------------------
#' Fill in missing AQS data with CDPHE data
#' -----------------------------------------------------------------------------

#' Get dates from AQS data
rh_dates <- unique(rh_data$Date_Local)

#' Filter CDPHE data to exclude AQS dates
cdphe_means <- filter(cdphe_means, !(Date_Local %in% rhdates))
unique(cdphe_means$Date_Local)

#' combined the data sets
monitor_data <- bind_rows(temp_data, cdphe_means)

#' monitor check
length(unique(temp_data$monitor_id))
length(unique(cdphe_means$monitor_id))
length(unique(monitor_data$monitor_id))

setdiff(monitor_data$monitor_id, temp_data$monitor_id)

#' Drop "080310016", "080590014", "081230013" because they aren't in the monitoring plan
drop <- c("080310016", "080590014", "081230013")

#' make the monitor data spatial
monitor_data <- filter(monitor_data, !(monitor_id %in% drop)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>%
  st_transform(crs = albers)

st_write(monitor_data, here::here("Data", "Monitor_RH_Data_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

