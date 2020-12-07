#' =============================================================================
#' Project: ECHO LUR
#' Date created: January 18, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script combined the filter data, spatial covariates, and spatiotemporal
#' covariates into a single dataset
#' 
#' Also saves an "archive" dataset with a date stamp in case we need to go back
#' to anything
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

today <- Sys.Date()

#' -----------------------------------------------------------------------------
#' Read in each of the datasets and create one big data frame
#' -----------------------------------------------------------------------------

#' Filter TWA data
#' PM (uncalibrated)
filter_pm_file_name <- paste0("Filter_PM.csv")
filter_pm <- read_csv(here::here("Data", filter_pm_file_name)) %>% 
  mutate(MonitorNumber = as.character(MonitorNumber),
         indoor_monitor_id = as.character(indoor_monitor_id)) %>% 
  select(-c(StartDateLocal, EndDateLocal))
sum(duplicated(filter_pm$filter_id))
table(filter_pm[which(duplicated(filter_pm$filter_id) ==T), "is_blank"])
# View(filter(filter_pm, duplicated(filter_pm$filter_id)))

#' Add the calibrated PM data
filter_pm_file_name2 <- paste0("Filter_PM_Calibrated.csv")
filter_pm2 <- read_csv(here::here("Data", filter_pm_file_name2)) %>%
  select(filter_id, campaign, pm_ug_m3_adjlm, pm_ug_m3_lm, pm_ug_m3_dem)
sum(duplicated(filter_pm2$filter_id))

filter_pm <- left_join(filter_pm, filter_pm2, by =c("campaign", "filter_id"))

#' BC (uncalibrated)
filter_bc_file_name <- paste0("Filter_BC.csv")
filter_bc <- read_csv(here::here("Data", filter_bc_file_name)) %>% 
  select(filter_id, campaign, is_blank, blank_mean_bc = blank_mean, 
         blank_cv_bc = blank_cv,
         bc_mass_ug, bc_mass_ug_corrected, bc_ug_m3_uncorrected:negative_bc_mass)
sum(duplicated(filter_bc$filter_id))
table(filter_bc[which(duplicated(filter_bc$filter_id) ==T), "is_blank"])
# View(filter(filter_bc, duplicated(filter_bc$filter_id)))

filter_bc <- select(filter_bc, -is_blank)

#' Add the calibrated BC data
filter_bc_file_name2 <- paste0("Filter_BC_Calibrated.csv")
filter_bc2 <- read_csv(here::here("Data", filter_bc_file_name2)) %>% 
  select(filter_id, campaign, bc_ug_m3_adjlm, bc_ug_m3_lm, bc_ug_m3_dem)
sum(duplicated(filter_bc2$filter_id))

filter_bc <- left_join(filter_bc, filter_bc2, by =c("campaign", "filter_id"))

#' Metals (currently uncalibrated)
filter_met_file_name <- paste0("Filter_Metals.csv")
filter_met <- read_csv(here::here("Data", filter_met_file_name))
sum(duplicated(filter_met$filter_id))
# View(filter(filter_met, duplicated(filter_met$filter_id)))

#' Combined
filter_data <- full_join(filter_pm, filter_bc, by = c("filter_id", "campaign")) %>% 
  full_join(filter_met, by = c("filter_id", "campaign"))
sum(duplicated(filter_data$filter_id))
table(filter_data[which(duplicated(filter_data$filter_id) ==T), "is_blank"])
# View(filter(filter_data, duplicated(filter_data$filter_id)))

#' Spatial covariates
sp_covariates_file_name <- "Spatial_Covariates_Filters_AEA.csv"
sp_covariates <- read_csv(here::here("Data", sp_covariates_file_name)) %>% 
  select(-WKT)
sp_covariates[which(sp_covariates$filter_id == "080310027"), "campaign"] <- "CampaignX"

#' Spatiotemporal covariates
st_covariates_file_name <- "ST_Covariates_Filters_AEA.csv"
st_covariates <- read_csv(here::here("Data", st_covariates_file_name)) %>% 
  mutate(filter_id = as.character(filter_id))

#' Put it all together
all_data <- full_join(filter_data, sp_covariates, by = c("filter_id", "campaign")) %>% 
  full_join(st_covariates, by = c("filter_id", "campaign"))
class(all_data)
names(all_data)

site_match <- filter(all_data, filter_id == "200202")

#' Set central monitor bc_ug_m3_dem == nn_bc
#' Set indoor == 0
#' set lon/lat
all_data <- all_data %>% 
  mutate(bc_ug_m3_dem = ifelse(filter_id == "080310027", nn_bc, bc_ug_m3_dem),
         indoor = ifelse(filter_id == "080310027", 0, indoor),
         lon = ifelse(filter_id == "080310027", site_match$lon, lon),
         lat = ifelse(filter_id == "080310027", site_match$lat, lat))
  
#' Duplicated IDs should all be blanks or the monitor ID
sum(duplicated(all_data$filter_id))
table(all_data[which(duplicated(all_data$filter_id) ==T), "is_blank"])

# View(filter(all_data, duplicated(all_data$filter_id)))

#' Fix paired IDs if needed
table(all_data$paired_id)
table(all_data$paired_id, all_data$campaign)

#' Write out data
#' Two .csv files: with and without date  
data_name <- "Combined_Filter_Data_AEA.csv"
archive_data_name <- paste0("Combined_Filter_Data_AEA_", today, ".csv")

write_csv(all_data, here::here("Data", data_name))
write_csv(all_data, here::here("Data/Archived_Data", archive_data_name))
