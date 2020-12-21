#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Combine EPA and CDPHE pollutant data
#' Date created: October 25, 2018
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Note: only including CDPHE data for monitors listed in the network plan
#' https://www.colorado.gov/airquality/tech_doc_repository.aspx?action=open&file=2020AnnualNetworkPlan.pdf
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)

#' Coordinate reference systems 
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

aqs_path <- here::here("Secondary_Data", "EPA_Air_Quality_System_Data")

epa_monitors <- read_csv(here::here("Secondary_Data", "EPA_Air_Quality_System_Data",
                                    "aqs_monitors.csv")) 
colnames(epa_monitors) <- gsub(" ", "_", colnames(epa_monitors))

co_monitors <- filter(epa_monitors, State_Code == "08") %>%
  mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
         Site_Num = str_pad(Site_Number, width = 4, side = "left", pad = "0")) %>% 
  mutate(monitor_id = paste0(State_Code, County_Code, Site_Number))

glimpse(co_monitors)
write_csv(co_monitors, here::here("Secondary_Data", "EPA_Air_Quality_System_Data",
                                  "colorado_monitors.csv"))

co_coords <- select(co_monitors, monitor_id, Longitude, Latitude) %>%
  distinct()

cdphe_mon <- read_csv(here::here("Secondary_Data/CDPHE_Air_Quality_Data", 
                                 "CDPHE_Monitors.csv")) %>%
  select(-lon, -lat)

cdphe_mon <- left_join(cdphe_mon, co_coords, by = "monitor_id")

#' -----------------------------------------------------------------------------
#' First, read in the daily PM2.5 and simplify the data sets
#' -----------------------------------------------------------------------------

pol <- "88101"
file_list <- list.files(aqs_path, pattern = pol)

aqs_data <- data.frame()

for (i in 1:length(file_list)) {
  temp <- read_csv(paste0(aqs_path, "/", file_list[i]))
  colnames(temp) <- gsub(" ", "_", colnames(temp))
  
  temp <- filter(temp, State_Code == "08") %>% 
    mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
           Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
    mutate(monitor_id = paste0(State_Code, County_Code, Site_Num)) %>% 
    rename(Max_Value = "1st_Max_Value")
  
  aqs_data <- bind_rows(aqs_data, temp)
  rm(temp)
}

glimpse(aqs_data)

#' -----------------------------------------------------------------------------
#' Next, get the CDPHE data in similar shape
#' Need to summarize hourly data to daily mean
#' -----------------------------------------------------------------------------

pol <- "88101"
cdphe_data <- read_csv(here::here("Secondary_Data/CDPHE_Air_Quality_Data", 
                                  "88101_CDPHE.csv")) %>%
  pivot_longer(-c(hour_MST, Date_Local, Parameter_Code),
               names_to = "monitor_loc", values_to = "one_hr_conc") %>%
  
  #' summarize to daily means
  #' Assign a POC code of 1
  group_by(monitor_loc, Parameter_Code, Date_Local) %>%
  dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
                   Max_Value = max(one_hr_conc, na.rm = T)) %>%
  mutate(Units_of_Measure = "Micrograms/cubic meter (LC)",
         Arithmetic_Mean = ifelse(is.nan(Arithmetic_Mean), NA, Arithmetic_Mean),
         Max_Value = ifelse(is.infinite(Max_Value), NA, Max_Value),
         Sample_Duration = "24 HOUR",
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
aqs_dates <- unique(aqs_data$Date_Local)

#' Filter CDPHE data to exclude AQS dates
cdphe_means <- filter(cdphe_means, !(Date_Local %in% aqs_dates))
unique(cdphe_means$Date_Local)

#' combined the datasets
monitor_data <- bind_rows(aqs_data, cdphe_means)

#' monitor check
length(unique(aqs_data$monitor_id))
length(unique(cdphe_means$monitor_id))
length(unique(cdphe_mon$monitor_id))

setdiff(monitor_data$monitor_id, aqs_data$monitor_id)

#' Drop 080131001, which uses a TEOM and is not included in the EPA data set
#' Drop 080970008, which the CDPHE report says measures PM10
#' Drop 080450007, which is not listed in the CDPHE monitoring plan

drop <- c("080970008", "080131001", "080450007")

#' make the monitor data spatial
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
  temp <- read_csv(paste0(aqs_path, "/", file_list[i]))
  colnames(temp) <- gsub(" ", "_", colnames(temp))
  
  temp <- filter(temp, State_Code == "08") %>% 
    mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
           Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
    mutate(monitor_id = paste0(State_Code, County_Code, Site_Num)) %>% 
    mutate(Method_Code = as.character(Method_Code)) %>%
    rename(Max_Value = "1st_Max_Value")
  
  aqs_data <- bind_rows(aqs_data, temp)
  rm(temp)
}

glimpse(aqs_data)

#' -----------------------------------------------------------------------------
#' Next, get the CDPHE data in similar shape
#' Need to summarize hourly data to daily mean
#' -----------------------------------------------------------------------------

pol <- "42602"
cdphe_data <- read_csv(here::here("Secondary_Data/CDPHE_Air_Quality_Data", 
                                  "42602_CDPHE.csv")) %>%
  pivot_longer(-c(hour_MST, Date_Local, Parameter_Code),
               names_to = "monitor_loc", values_to = "one_hr_conc") %>%
  
  #' summarize to daily means
  #' #' Assign a POC code of 1
  group_by(monitor_loc, Parameter_Code, Date_Local) %>%
  dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
                   Max_Value = max(one_hr_conc, na.rm = T)) %>%
  mutate(Units_of_Measure = "Parts per billion",
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
aqs_dates <- unique(aqs_data$Date_Local)

#' Filter CDPHE data to exclude AQS dates
cdphe_means <- filter(cdphe_means, !(Date_Local %in% aqs_dates))
unique(cdphe_means$Date_Local)

#' combined the data sets
monitor_data <- bind_rows(aqs_data, cdphe_means)

#' monitor check
length(unique(aqs_data$monitor_id))
length(unique(cdphe_means$monitor_id))
length(unique(cdphe_mon$monitor_id))

setdiff(monitor_data$monitor_id, aqs_data$monitor_id)

#' Drop "081230013" and "080310023", which are not listed in the network plan 
drop <- c("081230013", "080310023")

#' make the monitor data spatial
monitor_data <- filter(monitor_data, !(monitor_id %in% drop)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>%
  st_transform(crs = albers)

st_write(monitor_data, here::here("Data", "Monitor_NO2_Data_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)


#' -----------------------------------------------------------------------------
#' Now, read in the daily PM2.5 speciated data (for BC) and simplify the data sets
#' -----------------------------------------------------------------------------

#' https://aqs.epa.gov/aqsweb/documents/codetables/parameters.html
#' Black Carbon PM2.5 at 880 nm-- Parameter code = 88313
#' Using 880 nm because that's what the Sootscan uses (Ahmed et al 2009)

pol <- 88313
file_list <- list.files(aqs_path, pattern = "SPEC")

aqs_data <- data.frame()

for (i in 1:length(file_list)) {
  temp <- read_csv(paste0(aqs_path, "/", file_list[i]))
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

#' -----------------------------------------------------------------------------
#' Next, get the CDPHE data in similar shape
#' Only using the monitor at Yuma due to it's long term record
#' Site information from the 2018 Montior Network Plan
#' https://www.colorado.gov/airquality/tech_doc_repository.aspx?action=open&file=2018AnnualNetworkPlan.pdf
#'
#' These data have to be obtained directly from CDPHE
#' Brad Rink (bradley.rink@state.co.us) and John Olasin (mailto:john.olasin@state.co.us)
#' have been amazingly helpful in getting these data for us
#' -----------------------------------------------------------------------------

yuma_location <- "I25DEN"

pol <- 88313
cdphe_data <- read_excel(here::here("Secondary_Data/CDPHE_Air_Quality_Data",
                                    "Aeth_2020.xlsx"))
colnames(cdphe_data) <- gsub(" ", "_", tolower(colnames(cdphe_data)))
  
cdphe_data <- cdphe_data %>%
  mutate(Date_Local = as.Date(date, format = "%Y%m%d"),
         hour_MST = start_time,
         monitor_loc = yuma_location,
         Parameter_Code = pol) %>%
  rename("one_hr_conc" = "880nm_value") %>%
  mutate(one_hr_conc = as.numeric(one_hr_conc)) %>%
  select(Date_Local, hour_MST, monitor_loc, Parameter_Code, one_hr_conc) %>%
  
  #' summarize to daily means
  #' #' Assign a POC code of 1
  group_by(monitor_loc, Parameter_Code, Date_Local) %>%
  dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
                   Max_Value = max(one_hr_conc, na.rm = T)) %>%
  mutate(Units_of_Measure = "Micrograms/cubic meter (LC)",
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
aqs_dates <- unique(aqs_data$Date_Local)

#' Filter CDPHE data to exclude AQS dates
cdphe_means <- filter(cdphe_means, !(Date_Local %in% aqs_dates))
unique(cdphe_means$Date_Local)

#' combined the data sets
monitor_data <- bind_rows(aqs_data, cdphe_means)

#' monitor check
length(unique(aqs_data$monitor_id))
length(unique(cdphe_means$monitor_id))
length(unique(cdphe_mon$monitor_id))

setdiff(monitor_data$monitor_id, aqs_data$monitor_id)

#' make the monitor data spatial
monitor_data <- monitor_data %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>%
  st_transform(crs = albers)

st_write(monitor_data, here::here("Data", "Monitor_BC_Data_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

