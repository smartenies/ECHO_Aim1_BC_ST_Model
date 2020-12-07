#' -----------------------------------------------------------------------------
#' Date created: October 25, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: Combine EPA and CDPHE monitoring data to have a full dataset
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)

#' Coordinate reference systems 
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

aqs_path <- here::here("Secondary_Data", "EPA_Air_Quality_System_Data")

#' -----------------------------------------------------------------------------
#' First, read in the daily PM2.5 and simplify the datasets
#' -----------------------------------------------------------------------------

pol <- "88101"
file_list <- list.files(aqs_path, pattern = pol)

aqs_data <- data.frame()

for (i in 1:length(file_list)) {
  temp <- read_csv(paste0(aqs_path, file_list[i]))
  colnames(temp) <- gsub(" ", "_", colnames(temp))
  
  temp <- filter(temp, State_Code == "08") %>% 
    mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
           Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
    mutate(monitor_id = paste0(State_Code, County_Code, Site_Num)) %>% 
    rename(Max_Value = "1st_Max_Value")
  
  aqs_data <- bind_rows(aqs_data, temp)
  rm(temp)
}

#' -----------------------------------------------------------------------------
#' Next, get the CDPHE data in similar shape
#' -----------------------------------------------------------------------------

#' pol <- "88101"
#' 
#' cdphe_data <- read_csv(here::here("Data/CDPHE_AQS_Data", "88101_CDPHE.csv")) %>% 
#'   gather(key = "monitor_loc", value = "metrics", 
#'          -c(Parameter_Code, Date_Local, hour_MST, metric_key)) %>% 
#'   mutate(Date_Local = str_pad(Date_Local, width = 8, pad = "0", side = "left")) %>% 
#'   mutate(Date_Local = as.Date(Date_Local, format = "%m%d%Y")) %>% 
#'   rowwise() %>% 
#'   mutate(metrics = as.list(str_split(metrics, "_"))) %>% 
#'   mutate(one_hr_conc = as.numeric(unlist(metrics)[1])) %>% 
#'   mutate(one_hr_conc = ifelse(is.nan(one_hr_conc), NA, one_hr_conc))
#' 
#' #' summarize to daily means
#' #' Assign a POC code of 1
#' cdphe_means <- cdphe_data %>% 
#' select(-c(hour_MST, metrics, metric_key)) %>% 
#'   group_by(monitor_loc, Parameter_Code, Date_Local) %>%
#'   dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
#'             Max_Value = max(one_hr_conc, na.rm = T)) %>% 
#'   mutate(Units_of_Measure = "Micrograms/cubic meter (LC)",
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
#' cdphe_means <- select(cdphe_means, -c(monitor_info, monitor_loc))
#' 
#' glimpse(cdphe_means)
#' 
#' write_csv(cdphe_means, here::here("Data/CDPHE_AQS_Data", "88101_Daily_CDPHE.csv"))

#' -----------------------------------------------------------------------------
#' Fill in missing AQS data with CDPHE data
#' -----------------------------------------------------------------------------

#' #' Get dates from AQS data
#' aqs_dates <- unique(aqs_data$Date_Local)
#' 
#' #' Filter CDPHE data to exclude AQS dates
#' cdphe_means <- filter(cdphe_means, !(Date_Local %in% aqs_dates))
#' unique(cdphe_means$Date_Local)
#' 
#' #' combined the datasets
#' monitor_data <- bind_rows(aqs_data, cdphe_means)
#' 
#' #' monitor check
#' length(unique(aqs_data$monitor_id))
#' length(unique(cdphe_means$monitor_id))
#' length(unique(monitor_data$monitor_id))
#' 
#' setdiff(monitor_data$monitor_id, aqs_data$monitor_id)
#' 
#' #' if a monitor isn't in the AQS dataset, don't include it in the full dataset
#' #' For example, 08 031 3001-3 is a TEOM and not suitable for regulatory purposes
#' #' (based on the 2018 Network Plan:
#' #' https://www.colorado.gov/airquality/tech_doc_repository.aspx?action=open&file=2018AnnualNetworkPlan.pdf)
#' #' drop <- dplyr::setdiff(monitor_data$monitor_id, aqs_data$monitor_id)
#' 
#' #' Drop a monitor in Delta county because the Network Monitoring Plan says it 
#' #' should be a PM10 monitor and the monthly concentration is pretty high
drop <- "080290004"

#' make the monitor data spatial
monitor_data <- aqs_data
monitor_data <- filter(monitor_data, !(monitor_id %in% drop)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>% 
  st_transform(crs = albers)

st_write(monitor_data, here::here("Data", "Monitor_PM_Data_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' -----------------------------------------------------------------------------
#' Next, NO2 data
#' -----------------------------------------------------------------------------

pol <- "42602"

file_list <- list.files(aqs_path, pattern = pol)

aqs_data <- data.frame()

for (i in 1:length(file_list)) {
  temp <- read_csv(paste0(aqs_path, file_list[i]))
  colnames(temp) <- gsub(" ", "_", colnames(temp))
  
  temp <- filter(temp, State_Code == "08") %>% 
    mutate(Method_Code = as.character(Method_Code)) %>% 
    mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
           Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
    mutate(monitor_id = paste0(State_Code, County_Code, Site_Num)) %>% 
    rename(Max_Value = "1st_Max_Value")
  
  aqs_data <- bind_rows(aqs_data, temp)
  rm(temp)
}

#' make the monitor data spatial
monitor_data <- aqs_data
monitor_data <- monitor_data %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>% 
  st_transform(crs = albers)

st_write(monitor_data, here::here("Data", "Monitor_NO2_Data_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)


#' -----------------------------------------------------------------------------
#' Now, read in the daily PM2.5 speciated data (for BC) and simplify the datasets
#' -----------------------------------------------------------------------------

#' https://aqs.epa.gov/aqsweb/documents/codetables/parameters.html
#' Black Carbon PM2.5 at 880 nm-- Parameter code = 88313
#' Using 880 nm because that's what the Sootscan uses (Ahmed et al 2009)

pol <- 88313
file_list <- list.files(aqs_path, pattern = "SPEC")

aqs_data <- data.frame()

for (i in 1:length(file_list)) {
  temp <- read_csv(paste0(aqs_path, file_list[i]))
  colnames(temp) <- gsub(" ", "_", colnames(temp))
  
  temp <- filter(temp, State_Code == "08") 
  
  #' if a year doesn't have data, skip it
  if(nrow(temp) == 0) next
  
  temp <- temp %>% 
    #' Just want to save the BC (880 nm) measurements
    filter(Parameter_Code == pol) %>% 
    mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
           Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
    mutate(monitor_id = paste0(State_Code, County_Code, Site_Num)) %>% 
    rename(Max_Value = "1st_Max_Value")
  
  aqs_data <- bind_rows(aqs_data, temp)
  rm(temp)
}

#' #' -----------------------------------------------------------------------------
#' #' Next, get the CDPHE data in similar shape
#' #' Only using the monitor at Yuma due to it's long term record
#' #' Site information from the 2018 Montior Network Plan
#' #' https://www.colorado.gov/airquality/tech_doc_repository.aspx?action=open&file=2018AnnualNetworkPlan.pdf
#' #' 
#' #' These data have to be obtained directly from CDPHE
#' #' Brad Rink (CDPHE, bradley.rink@state.co.us) has been amazingly helpful in
#' #' getting these data for us
#' #' -----------------------------------------------------------------------------
#' 
#' yuma_id <- "080310027"
#' yuma_lon <- -105.015317
#' yuma_lat <- 39.732146
#' 
#' pol <- 88313
#' cdphe_data <- read_csv(here::here("Data/CDPHE_Aeth_Data", 
#'                                   "Yuma_st_Aeth_Combined.csv")) %>% 
#'   mutate(Date_Local = as.Date(Samp_date, format = "%m/%d/%Y"),
#'          hour_MST = Samp_time,
#'          monitor_id = yuma_id,
#'          Parameter_Code = pol)
#' 
#' bc_dates <- seq.Date(from = aqs_data$Date_Local[nrow(aqs_data)] + 1, 
#'                      to = cdphe_data$Date_Local[nrow(cdphe_data)],
#'                      by = "day")
#' 
#' #' Assign some additional parameters, including a POC code of 1
#' cdphe_means <- cdphe_data %>% 
#'   group_by(monitor_id, Parameter_Code, Date_Local) %>%
#'   dplyr::summarize(Arithmetic_Mean = mean(BC_880_nm, na.rm = T),
#'                    Max_Value = max(BC_880_nm, na.rm = T)) %>% 
#'   mutate(Units_of_Measure = "Micrograms/cubic meter (LC)",
#'          Arithmetic_Mean = ifelse(is.nan(Arithmetic_Mean), NA, Arithmetic_Mean),
#'          Max_Value = ifelse(is.infinite(Max_Value), NA, Max_Value),
#'          POC = 1) %>% 
#'   ungroup() %>% 
#'   
#'   #' add coordinates
#'   mutate(Longitude = yuma_lon, Latitude = yuma_lat)
#' 
#' #' -----------------------------------------------------------------------------
#' #' Fill in missing AQS data with CDPHE data
#' #' -----------------------------------------------------------------------------
#' 
#' #' Get dates from AQS data
#' aqs_dates <- unique(aqs_data$Date_Local)
#' 
#' #' Filter CDPHE data to exclude AQS dates
#' cdphe_means <- filter(cdphe_means, !(Date_Local %in% aqs_dates))
#' unique(cdphe_means$Date_Local)
#' 
#' #' combined the datasets
#' monitor_data <- bind_rows(aqs_data, cdphe_means)
#' 
#' #' monitor check
#' length(unique(aqs_data$monitor_id))
#' length(unique(cdphe_means$monitor_id))
#' length(unique(monitor_data$monitor_id))
#' 
#' setdiff(monitor_data$monitor_id, aqs_data$monitor_id)

#' make the monitor data spatial
monitor_data <- aqs_data
monitor_data <- monitor_data %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>% 
  st_transform(crs = albers)

st_write(monitor_data, here::here("Data", "Monitor_BC_Data_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' #' -----------------------------------------------------------------------------
#' #' Clean CDPHE ozone data just in case
#' #' -----------------------------------------------------------------------------
#' 
#' pol <- "44201"
#' 
#' cdphe_data <- read_csv(here::here("Data/CDPHE_AQS_Data", "44201_CDPHE.csv")) %>% 
#'   gather(key = "monitor_loc", value = "metrics", 
#'          -c(Parameter_Code, Date_Local, hour_MST, metric_key)) %>% 
#'   mutate(Date_Local = str_pad(Date_Local, width = 8, pad = "0", side = "left")) %>% 
#'   mutate(Date_Local = as.Date(Date_Local, format = "%m%d%Y")) %>% 
#'   rowwise() %>% 
#'   mutate(metrics = str_split(metrics, "_")) %>% 
#'   mutate(one_hr_conc = as.numeric(unlist(metrics)[1])) %>% 
#'   mutate(one_hr_conc = ifelse(is.nan(one_hr_conc), NA, one_hr_conc)) %>% 
#'   mutate(eight_hr_conc = as.numeric(unlist(metrics)[2])) %>% 
#'   mutate(eight_hr_conc = ifelse(is.nan(eight_hr_conc), NA, eight_hr_conc))
#' 
#' #' summarize to daily means
#' #' Assign a POC code of 1
#' cdphe_means <- cdphe_data %>% 
#'   select(-c(hour_MST, metrics, metric_key)) %>% 
#'   group_by(monitor_loc, Parameter_Code, Date_Local) %>%
#'   dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
#'                    Max_Hourly = max(one_hr_conc, na.rm = T),
#'                    Max_8Hour_Mean = ifelse(sum(!is.na(eight_hr_conc)) > 18,
#'                                            max(eight_hr_conc, na.rm = T),
#'                                            NA)) %>% 
#'   mutate(Units_of_Measure = "Parts per billion",
#'          Arithmetic_Mean = ifelse(is.nan(Arithmetic_Mean), NA, Arithmetic_Mean),
#'          Max_Hourly = ifelse(is.infinite(Max_Hourly), NA, Max_Hourly),
#'          Max_8Hour_Mean = ifelse(is.infinite(Max_8Hour_Mean), NA, Max_8Hour_Mean),
#'          Sample_Duration = "1 HOUR",
#'          POC = 1) %>% 
#'   ungroup()
#' 
#' #' add monitor IDs and coordinates
#' monitors <- read_csv(here::here("Data/CDPHE_AQS_Data", "CDPHE_Monitors.csv")) %>% 
#'   rename(Longitude = lon, Latitude = lat)
#' 
#' cdphe_means <- left_join(cdphe_means, monitors, by = "monitor_loc")
#' cdphe_means <- select(cdphe_means, -c(monitor_info, monitor_loc))
#' 
#' glimpse(cdphe_means)
#' 
#' write_csv(cdphe_means, here::here("Data/CDPHE_AQS_Data", "44201_Daily_CDPHE.csv"))
