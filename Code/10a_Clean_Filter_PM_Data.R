#' =============================================================================
#' Project: ECHO LUR
#' Date created: December 9, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script cleans the PM2.5 gravimetric data for each UPAS sampling campaign
#' and calculates the TWA for each filter
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
  text  = element_text(family="Calibri",size = 12, color = 'black'),
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
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Read in and organize gravimetric data
#' -----------------------------------------------------------------------------

campaign_names <- c("Campaign1", "Campaign2", "Campaign3", "Campaign4")

filter_path <- "R:/RSTOR-Magzamen/Research/Projects/ECHO_Aim1/Raw_Data/Filter_Data/"
filter_data_list <- list()

for(camp in 1:length(campaign_names)) {
  campaign_name <- campaign_names[camp]
  
  filter_data_name <- paste0("ECHO_Filter_Data_", campaign_name, "_Formatted.xlsx")
  
  #' Read in the gravimetric data from the Powerhouse
  filter_data <- read_xlsx(paste0(filter_path, filter_data_name), 
                           sheet = "Gravimetric") %>% 
    select(-starts_with("...")) %>% 
    mutate(campaign = campaign_name)
  colnames(filter_data) <- gsub(" ", "_", colnames(filter_data))
  colnames(filter_data) <- gsub("__", "_", colnames(filter_data))
  
  glimpse(filter_data)
  
  filter_data <- mutate(filter_data, filter_id = str_pad(Filter_ID...1, width = 6, 
                                                         pad= "0")) %>% 
    mutate(filter_id = str_remove(filter_id, "\r")) %>% 
    mutate(pm_mass_ug = as.numeric(Mass_Difference)) %>% 
    filter(!is.na(filter_id))
  glimpse(filter_data)
  print(sum(duplicated(filter_data$filter_id)))

  #' Get rid of some extra columns
  filter_data <- filter_data %>% 
    select(campaign, filter_id, pm_mass_ug)
  glimpse(filter_data)
  
  #' Add the sampling volumes and other metadata
  volume_name <- "Sample_Volumes.csv"
  volumes <- read_csv(here::here("Data/UPAS_Data", volume_name)) %>% 
    mutate(filter_id = str_replace(filter_id, "Fieldblank #", "B")) %>% 
    mutate(filter_id = str_pad(filter_id, width = 6, pad= "0")) %>% 
    filter(!is.na(filter_id))
  glimpse(volumes)

  filter_vol_data <- left_join(filter_data, volumes, by=c("filter_id", "campaign"))
  glimpse(filter_vol_data)
  print(sum(duplicated(filter_vol_data$filter_id)))
  # filter_vol_data$filter_id[which(duplicated(filter_vol_data$filter_id))]
  
  filter_vol_data <- filter(filter_vol_data, !is.na(campaign_name))
  
  #' Add the location data
  location_name <- "Filter_Locations_AEA.csv"
  locations <- read_csv(here::here("Data/Filter_Data", location_name)) %>% 
    st_as_sf(wkt = "WKT", crs = albers)

  filter_data_sf <- left_join(filter_vol_data, locations, by = c("filter_id", "campaign")) %>% 
    select(-c(Location, geocode_add))
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
    
    mutate(indoor_monitor_id = ifelse(is_paired == 1 & indoor == 1, MonitorNumber,
                               ifelse(is_paired == 1 & indoor == 0, 
                                      str_remove(Details_GPS, "collocated with monitor #"), NA))) %>% 
    
    mutate(paired_id = ifelse(is_paired == 1 & indoor == 1, paste(week, MonitorNumber, sep = "_"),
                              ifelse(is_paired == 1 & indoor == 0, 
                                     paste(week, str_remove(Details_GPS, "collocated with monitor #"), sep = "_"),
                                     NA))) %>% 
    mutate(participant_id = ifelse(participant == 1 & indoor == 1, 
                                   gsub(".*?([0-9]+).*", "\\1", Details_GPS), NA))
  
  
  #' Campaign 3 was our "friends and family" campaign-- no paired data
  if(camp != 3) {
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
                   aes(x = pm_mass_ug), 
                   fill = "blue", alpha = 0.25) +
    simple_theme
  blank_hist_name <- paste0("Blank_PM_Histogram_", campaign_name, ".jpeg")
  ggsave(filename = here::here("Figs/Cleaning", blank_hist_name), 
         device = "jpeg", dpi=500, units = "in", height = 4, width = 4)
  
  #' Based on EPA Method 2.12, we can consider field blanks "valid" when the mass
  #' difference between pre- and post-weighing is < 30 ug (See Chapter 10 Page 9)
  #' Per the SOP, the sampled filters are not blank corrected (Chapter 10 page 21)
  #' #' https://www3.epa.gov/ttnamti1/files/ambient/pm25/qa/m212.pdf
  
  filter_data_sf <- filter_data_sf %>% 
    mutate(blank_passes_qc = ifelse(is_blank == 1,
                                    ifelse(abs(pm_mass_ug) < 30, 1, 0), NA))
  
  filter_data_blanks <- filter(filter_data_sf, is_blank == 1) %>% 
    filter(!is.na(blank_passes_qc))
  
  #' Do more than 90% of field blanks pass?
  #' This 90% threshold is something I specified! It's not based on a method
  pass_percent <- (sum(filter_data_blanks$blank_passes_qc) / 
                     nrow(filter_data_blanks)) * 100
  pass_percent
  
  filter_data_sf <- mutate(filter_data_sf, pct_blanks_passing = pass_percent)
  
  summary(filter_data_blanks$pm_mass_ug)
  
  #' Get LOD and LOQ
  #' LOD is 3 times the standard deviation of the mean of blank mass differences
  #' LOQ is 10 times the standard deviation of the mean of blank mass differences
  blanks_sd <- filter(filter_data_blanks, blank_passes_qc == 1) %>% 
    summarize(blank_sd = sd(pm_mass_ug))
  
  lod <- 3*blanks_sd$blank_sd
  loq <- 10*blanks_sd$blank_sd
  
  blank_name <- paste0("Field_Blanks_PM_", campaign_name, ".csv")
  write_csv(filter_data_blanks, here::here("Data/Filter_Data", blank_name))
  
  #' Flag mass differences below the LOD and LOQ
  filter_data_sf <- filter_data_sf %>% 
    
    #' Are any measurements below the limit of detection (LOD) or limit of 
    #' quanitfication (LOQ)?
    mutate(lod = lod,
           loq = loq, 
           below_lod = ifelse(pm_mass_ug < lod, 1, 0), 
           below_loq = ifelse(pm_mass_ug < loq, 1, 0))
  
  table(filter_data_sf$below_lod)
  table(filter_data_sf$below_loq)
  
  print(sum(duplicated(filter_data_sf$filter_id)))
  
  #' ---------------------------------------------------------------------------
  #' Combining data and calculating time weighted averages
  #' Flagging suspect data
  #' ---------------------------------------------------------------------------
  
  filter_data_sf <- filter_data_sf %>% 
    
    #' flag negative concentrations
    #' flag masses exceeding 1000 ug as potenitally contaminated -- arbitrary!
    mutate(negative_pm_mass = ifelse(pm_mass_ug < 0, 1, 0),
           potential_contamination = ifelse(pm_mass_ug > 1000, 1, 0)) %>% 
    
    #' calculate TWA
    #' three different concentrations calculated using sampled volumes based on 
    #' the run time logged by the UPAS (the favored concentration) and volumes
    #' based on the run times calculated from the UTC and Local timestamps.
    #' There should not be too much of a difference in these concentrations.
    mutate(pm_ug_m3 = pm_mass_ug / logged_rt_volume_m3,
           pm_ug_m3_local_rt_vol = pm_mass_ug / local_rt_volume_m3,
           pm_ug_m3_utc_rt_vol = pm_mass_ug / utc_rt_volume_m3) %>% 
    mutate(pm_pct_diff_conc_logged_local = ((pm_ug_m3 - pm_ug_m3_local_rt_vol) / pm_ug_m3) * 100,
           pm_pct_diff_conc_logged_utc = ((pm_ug_m3 - pm_ug_m3_utc_rt_vol) / pm_ug_m3) * 100) %>% 
    
    #' flag if percent difference exceeds 5%
    mutate(pm_pct_diff_conc_local_flag = ifelse(abs(pm_pct_diff_conc_logged_local) > 5, 1, 0),
           pm_pct_diff_conc_utc_flag = ifelse(abs(pm_pct_diff_conc_logged_local) > 5, 1, 0))

  print(table(filter_data_sf$negative_pm_mass))
  print(table(filter_data_sf$potential_contamination))
  print(table(filter_data_sf$pm_pct_diff_conc_local_flag))
  print(table(filter_data_sf$pm_pct_diff_conc_utc_flag))
  
  #' Save filter data
  filter_data_list[[camp]] <- as.data.frame(filter_data_sf)
}

filter_data_all <- bind_rows(filter_data_list) %>% 
  select(-WKT)

#' Any duplicate IDs should be blanks
print(sum(duplicated(filter_data_all$filter_id)))
filter_data_all$filter_id[duplicated(filter_data_all$filter_id)]

#' Counts for each paired filter ID should be even
print(table(filter_data_all$paired_id))

write_csv(filter_data_all, here::here("Data", "Filter_PM.csv"))



