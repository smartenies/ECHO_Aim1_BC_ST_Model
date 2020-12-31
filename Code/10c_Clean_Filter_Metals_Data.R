#' =============================================================================
#' Project: ECHO LUR
#' Date created: December 9, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script cleans the XRF (metals) for each UPAS sampling campaign
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
#' Read in and organize metals XRF
#' -----------------------------------------------------------------------------


campaign_names <- paste0("Campaign", c(1, 2, 3, 4, 5))

filter_path <- here::here("Raw_Data/Filter_Data")
filter_data_list <- list()

standard_area <- 7.065 #centimeters

for (camp in 1:length(campaign_names)) {
  campaign_name <- campaign_names[camp]
  
  filter_data_name <- paste0("ECHO_Filter_Data_", campaign_name, "_Formatted.xlsx")
  
  #' Read in the black from the Powerhouse
  filter_data <- read_xlsx(paste0(filter_path, "/", filter_data_name),
                           sheet = "Metals")
  print(sum(duplicated(filter_data$filter_id)))
  
  filter_data <- filter_data %>% 
    gather(key = metal, value = ug_cm2, -filter_id) %>% 
    
    #' ID below the LOD prior to converting to numeric
    mutate(campaign = campaign_name,
           xrf_below_lod = ifelse(str_detect(ug_cm2, "LOD") | str_detect(ug_cm2, "LOQ"), 1, 0)) %>% 
    
    #' Convert character to numeric
    mutate(ug_cm2 = as.numeric(ug_cm2)) %>% 
    mutate(filter_id = str_pad(filter_id, width = 6, pad= "0")) 
  
  glimpse(filter_data)

  #' Add the sampling volumes and other metadata
  volume_name <- "Sample_Volumes.csv"
  volumes <- read_csv(here::here("Data/UPAS_Data", volume_name)) %>% 
    mutate(filter_id = str_replace(filter_id, "Fieldblank #", "B")) %>% 
    mutate(filter_id = str_pad(filter_id, width = 6, pad= "0"))
  
  filter_vol_data <- left_join(filter_data, volumes, by= c("filter_id", "campaign"))
  glimpse(filter_vol_data)
  #print(sum(duplicated(filter_vol_data$filter_id)))
  
  #' Add the location data
  location_name <- "Filter_Locations_AEA.csv"
  locations <- read_csv(here::here("Data/Filter_Data", location_name)) %>% 
    st_as_sf(wkt = "WKT", crs = albers)
  
  filter_data_sf <- left_join(filter_vol_data, locations, by = c("filter_id", "campaign")) %>% 
    select(-c(Location))
  glimpse(filter_data_sf)
  
  #' ---------------------------------------------------------------------------
  #' Combining data and calculating time weighted averages
  #' ---------------------------------------------------------------------------
  
  filter_data_sf <- filter_data_sf %>% 
    
    #' Convert ug_cm2 to ug
    #' Use standard area of filter
    mutate(mass_ug = ug_cm2 * standard_area) %>% 
    
    #' calculate TWA
    mutate(ug_m3 = mass_ug / logged_rt_volume_m3) %>% 
    
    #' Just fiter ID, metal, and concentration
    select(filter_id, metal, ug_m3) %>% 
    filter(!is.na(metal))
  
  #' Convert back to a wide data frame
  filter_data_sf2 <- spread(filter_data_sf, metal, ug_m3)
  colnames(filter_data_sf2)[-1] <- 
    paste0(colnames(filter_data_sf2)[-1], "_ug_m3")
  
  filter_data_sf2$campaign <- campaign_name
  
  #' Save filter data
  filter_data_list[[camp]] <- as.data.frame(filter_data_sf2)
}

filter_data_all <- bind_rows(filter_data_list) 

#' Any duplicate IDs should be blanks
print(sum(duplicated(filter_data_all$filter_id)))
filter_data_all$filter_id[duplicated(filter_data_all$filter_id)]

write_csv(filter_data_all, here::here("Data", "Filter_Metals.csv"))


