#' =============================================================================
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Clean UPAS PM2.5 gravimetric data
#' Date created: December 11, 2018
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description:
#' This script cleans the black carbon transmissometry data for each UPAS 
#' sampling campaign and calculates the TWA for each filter
#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggthemes)
library(ggspatial)
library(viridis)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
options(scipen = 9999) #avoid scientific notation

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Read in and organize black carbon data
#' -----------------------------------------------------------------------------

campaign_names <- paste0("Campaign", c(1, 2, 3, 4, 5))

filter_path <- here::here("Raw_Data/Filter_Data")
filter_data_list <- list()

for (camp in 1:length(campaign_names)) {
  campaign_name <- campaign_names[camp]
  
  filter_data_name <- paste0("ECHO_Filter_Data_", campaign_name, "_Formatted.xlsx")
  
  #' Read in the transmissometry data from the Powerhouse
  filter_data <- read_xlsx(paste0(filter_path, "/", filter_data_name), 
                           sheet = "Sootscan",
                           range = cell_cols("A:H")) %>% 
    select(-starts_with("X")) %>%
    select(-(contains("TEFLON"))) %>% 
    mutate(campaign = campaign_name)
  colnames(filter_data) <- gsub(" ", "_", colnames(filter_data))
  colnames(filter_data) <- gsub("__", "_", colnames(filter_data))
  
  filter_data <- mutate(filter_data, filter_id = str_pad(Filter_ID...1, width = 6, 
                                                         pad= "0")) %>% 
    mutate(filter_id = str_remove(filter_id, "\r")) %>% 
    mutate(bc_mass_ug = as.numeric(BC)) %>% 
    filter(!is.na(filter_id))
  glimpse(filter_data)
  print(sum(duplicated(filter_data$filter_id)))
  
  #' Get rid of some extra columns
  filter_data <- filter_data %>% 
    select(campaign, filter_id, bc_mass_ug)
  glimpse(filter_data)
  
  #' Add the sampling volumes and other metadata
  volume_name <- "Sample_Volumes.csv"
  volumes <- read_csv(here::here("Data/UPAS_Data", volume_name)) %>% 
    mutate(filter_id = str_replace(filter_id, "Fieldblank #", "B")) %>% 
    mutate(filter_id = str_pad(filter_id, width = 6, pad= "0"))
  
  filter_vol_data <- left_join(filter_data, volumes, by=c("filter_id", "campaign"))
  glimpse(filter_vol_data)
  print(sum(duplicated(filter_vol_data$filter_id)))
  
  #' Add the location data
  location_name <- "Filter_Locations_AEA.csv"
  locations <- read_csv(here::here("Data/Filter_Data", location_name)) %>% 
    st_as_sf(wkt = "WKT", crs = albers)
  
  filter_data_sf <- left_join(filter_vol_data, locations, by=c("filter_id", "campaign")) %>% 
    select(-c(Location)) %>%
    distinct()
  glimpse(filter_data_sf)
  
  print(sum(duplicated(filter_data_sf$filter_id)))
  
  #' ID blank filters
  filter_data_sf <- filter_data_sf %>% 
    mutate(is_blank = ifelse(str_detect(filter_id, "B") & is.na(SampledVolume), 1, 0))
  
  #' ID which filters go together
  filter_data_sf <- filter_data_sf %>% 
    mutate(is_paired = ifelse(str_detect(Details_GPS, "participant") | 
                                str_detect(Details_GPS, "collocated"), 1, 0)) %>% 
    #' set "residence of" filters to not-paired
    mutate(is_paired = ifelse(is_paired == 1 & str_detect(Details_GPS, "residence of "), 0, is_paired)) %>% 
    
    #' set "location #" filters to not-paired
    mutate(is_paired = ifelse(is_paired == 1 & str_detect(Details_GPS, "location #"), 0, is_paired)) %>% 
    
    mutate(MonitorNumber = str_pad(MonitorNumber, width = 2, pad = "0")) %>% 
    
    mutate(indoor_monitor_id = ifelse(is_paired == 1 & indoor == 1, MonitorNumber,
                                      ifelse(is_paired == 1 & indoor == 0, 
                                             str_remove(Details_GPS, "collocated with monitor #"), NA))) %>% 
    
             
    mutate(paired_id = ifelse(is_paired == 1 & indoor == 1, paste(week, MonitorNumber, sep = "_"),
                              ifelse(is_paired == 1 & indoor == 0, 
                                     paste(week, str_remove(Details_GPS, "collocated with monitor #"), sep = "_"),
                                     NA))) %>% 
    mutate(participant_id = ifelse(participant == 1 & indoor == 1, 
                                   gsub(".*?([0-9]+).*", "\\1", Details_GPS), NA))
  
  
  if(campaign_names[camp] != "Campaign3") {
    participant_ids <- select(filter_data_sf, participant_id, indoor_monitor_id) %>% 
      filter(!is.na(participant_id))
    
    filter_data_sf <- select(filter_data_sf, -participant_id) %>% 
      left_join(participant_ids, by = "indoor_monitor_id")
  }

  print(sum(duplicated(filter_data_sf$filter_id)))
  print(table(filter_data_sf$paired_id))
  
  #' ---------------------------------------------------------------------------
  #' Check to make sure the blanks are OK
  #' ---------------------------------------------------------------------------

  #' distribution of blanks
  ggplot() +
    geom_histogram(data = filter(filter_data_sf, is_blank == 1), 
                   aes(x = bc_mass_ug), 
                   fill = "blue", alpha = 0.25) +
    simple_theme
  blank_hist_name <- paste0("Blank_BC_Histogram_", campaign_name, ".jpeg")
  ggsave(filename = here::here("Figs/Data_Cleaning", blank_hist_name), 
         device = "jpeg", dpi=500, units = "in", height = 4, width = 4)
  
  #' distribution of blanks and sampled filters
  ggplot() +
    geom_histogram(data = filter(filter_data_sf, is_blank == 0), 
                   aes(x = bc_mass_ug), fill = "red") +
    geom_histogram(data = filter(filter_data_sf, is_blank == 1), 
                   aes(x = bc_mass_ug), fill = "blue", alpha = 0.25) +
    simple_theme
  
  #' During our consultation with Christian L'Orange (Powerhouse) on Nov 6, 2018, 
  #' he suggested be "blank correct" the BC measurements if the blank values were 
  #' consistent and small. I still need to find a good SOP to cite, but for now, I'm
  #' going to subtract the mean of the blanks (which will add mass when the mean is 
  #' negative) if the coefficient of variation for the blank measurements is < 25%

  #' Blank corrected measurements will be flagged
  filter_blanks <- filter(filter_data_sf, is_blank == 1)
  
  blank_name <- paste0("Field_Blanks_BC_", campaign_name, ".csv")
  write_csv(filter_blanks, here::here("Data/Filter_Data", blank_name))
  
  ggplot() +
    geom_histogram(data = filter_blanks, aes(x = bc_mass_ug), 
                   fill = "blue", alpha = 0.25) +
    simple_theme
  
  summary(filter_blanks$bc_mass_ug)
  summary(filter_data_sf[filter_data_sf$is_blank == 0, "bc_mass_ug"])
  mean_blanks <- mean(filter_blanks$bc_mass_ug, na.rm = T)
  mean_blanks
  
  sd_blanks <- sd(filter_blanks$bc_mass_ug, na.rm = T)
  sd_blanks
  
  cv_blanks <- (sd_blanks / mean_blanks) * 100
  cv_blanks
  
  filter_data_sf <- filter_data_sf %>% 
    mutate(bc_blank_mean = mean_blanks,
           bc_blank_cv = cv_blanks) %>% 
    rowwise() %>%
    mutate(bc_mass_ug_corrected = ifelse(abs(cv_blanks) < 25, 
                                         bc_mass_ug - mean_blanks, bc_mass_ug))
  
  ggplot() +
    geom_histogram(data = filter(filter_data_sf, is_blank == 0), 
                   aes(x = bc_mass_ug_corrected), fill = "red") +
    geom_histogram(data = filter(filter_data_sf, is_blank == 1),
                   aes(x = bc_mass_ug_corrected), fill = "blue", alpha = 0.25) +
    simple_theme
  
  ggplot() +
    geom_histogram(data = filter(filter_data_sf, is_blank == 0), 
                   aes(x = bc_mass_ug), fill = "red") +
    geom_histogram(data = filter(filter_data_sf, is_blank == 0),
                   aes(x = bc_mass_ug_corrected), fill = "blue", alpha = 0.25) +
    simple_theme
  
  correct_hist_name <- paste0("BC_Correction_Histogram_", campaign_name, ".jpeg")
  ggsave(filename = here::here("Figs/Data_Cleaning", correct_hist_name), 
         device = "jpeg", dpi=500, units = "in", height = 4, width = 4)
  
  #' Get LOD and LOQ
  #' Normally, we'd calculate the LOD the same way we did for PM
  #' Because of issues with the negative BC measurements in the blanks,
  #' we're going to use the value of 1.41 ug from Casey's BC paper
  #' (Quinn et al., 2018)-- this is based on the lower limit of detection of
  #' the transmissometer and the area of the filter
  #' 
  #' For this measurement, we're using the corrected BC mass value
  
  lod <- 1.41

  #' Flag mass differences below the LOD and LOQ
  filter_data_sf <- filter_data_sf %>% 
    
    #' Are any measurements below the limit of detection (LOD)?
    mutate(bc_lod = lod,
           bc_below_lod = ifelse(bc_mass_ug_corrected < lod, 1, 0))
  
  #' ---------------------------------------------------------------------------
  #' Calculating time weighted averages
  #' ---------------------------------------------------------------------------
  
  filter_data_sf <- filter_data_sf %>% 
    
    #' calculate TWA using Sampled Volume  (convert L to m3)
    #' Also include three different concentrations calculated using sampled 
    #' volumes based on the run time logged by the UPAS  and volumes
    #' based on the run times calculated from the UTC and Local time stamps.
    #' There should not be too much of a difference in these concentrations.
    mutate(bc_ug_m3_uncorrected = bc_mass_ug / logged_rt_volume_m3,
           bc_ug_m3_logged_vol = bc_mass_ug_corrected / logged_rt_volume_m3,
           bc_ug_m3_sampled_vol = bc_mass_ug_corrected / (SampledVolume/10^3),
           bc_ug_m3_local_rt_vol = bc_mass_ug_corrected / local_rt_volume_m3,
           bc_ug_m3_utc_rt_vol = bc_mass_ug_corrected / utc_rt_volume_m3) %>% 
             
    #' flag if blank corrected
    #' flag if the original BC mass was negative
    mutate(blank_corrected_bc = ifelse(bc_mass_ug == bc_mass_ug_corrected, 0, 1)) %>% 
    mutate(negative_bc_orig_mass = ifelse(bc_mass_ug < 0, 1, 0)) 
  
  table(filter_data_sf$blank_corrected_bc)
  
  #' Save filter data
  filter_data_list[[camp]] <- as.data.frame(filter_data_sf)
}

filter_data_all <- bind_rows(filter_data_list) %>% 
  select(-WKT)

#' Any duplicate IDs should be blanks
print(sum(duplicated(filter_data_all$filter_id)))
filter_data_all$filter_id[duplicated(filter_data_all$filter_id)]

#' Counts for each ID should be even
print(table(filter_data_all$paired_id))

write_csv(filter_data_all, here::here("Data", "Filter_BC.csv"))

