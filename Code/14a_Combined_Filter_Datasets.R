#' =============================================================================
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Combining filter measurements, spatial covariates, and ST covariates
#' Date created: January 18, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Note: this script also saves an "archive" data set with a date stamp in case 
#' we need to go back to anything
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
#' Read in each of the data sets and create one big data frame
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
#View(filter(filter_pm, duplicated(filter_pm$filter_id)))

#' Add the calibrated PM data
filter_pm_file_name2 <- paste0("Filter_PM_Calibrated.csv")
filter_pm2 <- read_csv(here::here("Data", filter_pm_file_name2)) %>%
  select(filter_id, campaign, pm_ug_m3_raw,
         pm_ug_m3_lm, pm_ug_m3_lm_all, pm_ug_m3_dem, pm_ug_m3_dem_all)
sum(duplicated(filter_pm2$filter_id))

filter_pm <- left_join(filter_pm, filter_pm2, by =c("campaign", "filter_id"))

#' Calibrated BC data
filter_bc_file_name <- paste0("Filter_BC_Calibrated.csv")
filter_bc <- read_csv(here::here("Data", filter_bc_file_name)) %>%
  select(filter_id, campaign, bc_blank_mean:negative_bc_orig_mass,
         bc_ug_m3_raw, bc_ug_m3_lm, bc_ug_m3_dem)
sum(duplicated(filter_bc$filter_id))

#' Metals (currently uncalibrated)
filter_met_file_name <- paste0("Filter_Metals.csv")
filter_met <- read_csv(here::here("Data", filter_met_file_name))
sum(duplicated(filter_met$filter_id))
#View(filter(filter_met, duplicated(filter_met$filter_id)))

#' Combined
filter_data <- full_join(filter_pm, filter_bc, by = c("filter_id", "campaign")) %>%
  full_join(filter_met, by = c("filter_id", "campaign")) %>%
  mutate(sample_week = as.Date(cut(as.Date(StartDateTimeLocal), "week"))) %>%
  mutate(st_week = sample_week) %>%
  select(site_id, filter_id, campaign, is_blank, indoor, participant, 
         StartDateTimeLocal, EndDateTimeLocal, sample_week, st_week,
         logged_runtime, logged_rt_volume_L, low_volume_flag, ultralow_volume_flag,
         pm_below_lod, pm_below_loq, bc_below_lod, 
         negative_pm_mass, potential_contamination, 
         pm_mass_ug, bc_mass_ug_corrected, blank_corrected_bc, bc_blank_mean,
         pm_ug_m3_raw:pm_ug_m3_dem, bc_ug_m3_raw:bc_ug_m3_dem,
         Al_ug_m3:Zn_ug_m3) 
names(filter_data)
sum(duplicated(filter_data$filter_id))
table(filter_data[which(duplicated(filter_data$filter_id) ==T), "is_blank"])
# View(filter(filter_data, duplicated(filter_data$filter_id)))

#' Spatial covariates
sp_covariates_file_name <- "Spatial_Covariates_Filters_AEA.csv"
sp_covariates <- read_csv(here::here("Data", sp_covariates_file_name)) %>% 
  select(-c(WKT, campaign, filter_id)) %>%
  distinct()

#' Spatiotemporal covariates
st_covariates_file_name <- "ST_Covariates_Sites_AEA.csv"
st_covariates <- read_csv(here::here("Data", st_covariates_file_name)) %>%
  rename(st_week = week)

#' Put it all together
all_data1 <- left_join(st_covariates, filter_data, by = c("site_id", "st_week")) %>%
  left_join(sp_covariates, by = "site_id") %>%
  select(-c(nn3_bc:idw_bc))
class(all_data1)
names(all_data1)

all_data2 <- all_data1 %>% 
  select(site_id, filter_id, campaign, lon, lat, is_blank:Zn_ug_m3, 
         elevation_50:aadt_2500, st_week:units_smoke)
ncol(all_data2) == ncol(all_data1)
names(all_data2)

#' Data frame for the central site
cent_data <- filter(st_covariates, site_id == "16") %>%
  left_join(filter(sp_covariates, site_id == "16"), by = "site_id") 
cent_data$sample_week <- cent_data$st_week
cent_data$filter_id <- "080310027"
cent_data$campaign <- "CampaignX"
cent_data$is_blank <- 0
cent_data$indoor <- 0
cent_data$participant <- 0
cent_data$bc_ug_m3_raw <- cent_data$nn_bc
cent_data$bc_ug_m3_lm <- cent_data$nn_bc
cent_data$bc_ug_m3_dem <- cent_data$nn_bc

#' Combine all the data
all_data <- bind_rows(all_data2, cent_data)

test1 <- filter(all_data, filter_id == "080310027")
test2 <- filter(all_data, campaign == "Campaign5")
test3 <- filter(all_data, site_id == "20")
test4 <- filter(all_data, filter_id != "080310027") %>%
  filter(!is.na(bc_ug_m3_dem)) %>%
  filter(st_week == sample_week)

#' Write out data
#' Two .csv files: with and without date  
data_name <- "Combined_Filter_Data_AEA.csv"
archive_data_name <- paste0("Combined_Filter_Data_AEA_", today, ".csv")

write_csv(all_data, here::here("Data", data_name))
write_csv(all_data, here::here("Data/Archived_Data", archive_data_name))
