#' =============================================================================
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Scrape UPAS meta data
#' Date created: December 4, 2018
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description:
#' This script gathers the metadata for each UPAS (used to calculate 
#' time-weighted averages of PM2.5 and BC) and extracts the sampling location
#' data from the catalogs Grace and the field team compiled
#' 
#' NOTE: Because of differences in how data for each campaign was stored in the 
#' Dropbox, the code to identify sampling locations for Campaign 1 and Campaign 
#' 2 is different. The code to extract metadata from the UPAS output files still
#' works the same way as the other scripts.
#' 
#' NOTE: The outputs of this script include PHI and cannot be shared!!
#' =============================================================================

library(tidyverse)
library(lubridate)
library(writexl)
library(readxl)

#' -----------------------------------------------------------------------------
#' Metadata: All Campaigns
#' -----------------------------------------------------------------------------

campaign_names <- c("Campaign1", "Campaign2", "Campaign3", "Campaign4", "Campaign5")
meta_data_list <- list()

for (camp in 1:length(campaign_names)) {
  campaign_name <- campaign_names[camp]
  print(campaign_names[camp])
  
  #' Identify the directory with the UPAS data files
  campaign_dir <- here::here("Raw_Data/UPAS_Metadata", campaign_name)
  
  weeks <- list.dirs(campaign_dir, recursive = F)
  weeks_names <- list.dirs(campaign_dir, recursive = F, full.names = F)
  
  #' Empty data frame for metadata
  meta_data_df <- data.frame()
  upas_timestamps_df <- data.frame()
  
  for (i in 1:length(weeks)) {
    
    #' Get a list of the individual UPAS metadata files in the upas_home folder
    upas <- list.files(path=weeks[i], full.names = T, recursive = F, 
                       pattern = "LOG_")
    length(upas)
    
    for (j in 1:length(upas)) {
      
      #' Get metadata
      meta_data <- read_delim(upas[j], delim = ",", n_max = 36) %>% 
        mutate_if(is.factor, as.character)
      upas_id <- as.character(meta_data[1,2])
      
      if (j == 1 & i == 1 & camp == 1) {
        meta_data_dic <- slice(meta_data, 1:36)
        meta_data_dic_name <- paste0("Metadata_Dictionary.csv")
        write_csv(meta_data_dic, here::here("Raw_Data/UPAS_Metadata/", 
                                            meta_data_dic_name))
      }
      
      meta_data <- t(meta_data[,1:2])
      colnames(meta_data) <- meta_data[1,]
      
      meta_data <- as.data.frame(meta_data) %>%
        slice(2) %>% 
        mutate(week = weeks_names[i])
      
      meta_data_df <- bind_rows(meta_data_df, meta_data) %>% 
        mutate_if(is.factor, as.character)
      
      #' Get timestamp data
      time_data <- read_delim(upas[j], delim = ",", skip = 57)

      if (time_data[1,1] == "SampleTime") {
        colnames(time_data) <- time_data[1,]
        time_data <- slice(time_data, -1) %>% 
          mutate(week = weeks_names[i])
        
        if(nrow(time_data) == 0) {
          time_data[1,] <- NA
          time_data$week <- weeks_names[i]
        }
        
      } else {
        if (j != 1) {
          #' For some reason, some of the meta data files are missing the "headers"
          #' for the columns; they'll need to be read in again and renamed
          #' These files have more columns that the other meta data, so I'm 
          #' dropping these extra columns
          
          time_data <- read.table(upas[j], sep = ",", skip = 57,
                                   stringsAsFactors = F, colClasses = "character")
          time_data <- select(time_data, -c((ncol(upas_timestamps_df) + 1):ncol(time_data)))
          colnames(time_data) <- colnames(upas_timestamps_df)
          
        } else {
          break
        }
      }
      time_data$upas_id <- as.character(upas_id)
      upas_timestamps_df <- bind_rows(upas_timestamps_df, time_data)  %>% 
        mutate_if(is.factor, as.character)
      rm(meta_data, time_data)
    }
    
    rm(upas)
  }
  
  meta_data_df$campaign <- campaign_names[camp]
  upas_timestamps_df$campaign <- campaign_name
  
  meta_data_list[[camp]] <- meta_data_df 
  
  timestamp_name <- paste0("UPAS_Timestamps_Campaign", camp, ".csv")
  write_csv(upas_timestamps_df, here::here("Data/UPAS_Data", timestamp_name))
}

upas_metadata <- bind_rows(meta_data_list)
write_csv(upas_metadata, here::here("Data/UPAS_Data", "UPAS_Metadata.csv"))


#' -----------------------------------------------------------------------------
#' Sampling locations
#' -----------------------------------------------------------------------------

catalog_path <- here::here("Raw_Data/Campaign_Logistics")

#' -----------------------------------------------------------------------------
#' Sampling locations: Campaign 2
#' Starting with campaign 2 because this is where we have the location data 
#' for all of the community sites
#' -----------------------------------------------------------------------------

campaign_name2 <- c("Campaign2")
catalogs2 <- list.files(path = catalog_path, pattern = campaign_name2, 
                       full.names = T)

coordinate_name <- "Campaign 2 Coordinates.xlsx"
coordinate_df <- read_excel(paste0(catalog_path, "/", coordinate_name)) %>% 
  rename("Location" = "Address") %>% 
  mutate(Details_GPS_2 = paste(Latitude, Longitude, sep = ", ")) %>% 
  select(Location, Details_GPS_2) %>% 
  
  #' Fix creekside park naming issue
  #' The address is different between the catalog and the coordinates spreadsheet
  #' Dropping the address alltogether since the coordinates are correct
  mutate(Location = ifelse(str_detect(Location, "Creekside"), 
                           "Creekside Park",
                           Location))

#' Note that the location data for community sites in Campaign 2 are slightly
#' different from the location data for community sites listed in the Campaign 1
#' catalog. For consistency, I'm using the Campaign 2 location information for 
#' all community site filters. These data are updated at the bottom of this 
#' script
comm_sites_2 <- filter(coordinate_df, !str_detect(Location, "CO")) %>%
  select(Location, Details_GPS_2) %>%
  distinct()

catalog_data2 <- data.frame()

for(i in 1:length(catalogs2)) {
  temp2 <- read_xlsx(catalogs2[i], 
                     sheet = 2, range = cell_cols(1:10)) %>% 
     rename("Details_GPS" = "...10",
            "filter_id" = "Filter code") %>% 
     select(filter_id, Location, Details_GPS) %>% 
    
    #' Fix creekside park naming issue
    mutate(Location = ifelse(str_detect(Location, "Creekside"), 
                             "Creekside Park",
                             Location)) %>% 
    
    #' filter out "cancelled" filters
    filter(!str_detect(Details_GPS, "cancelled")) %>% 
     
    #' The name of Cherry Creek State Park East is incorrect in the catalog
    #' Updating based on the naming convention used in the Campaign 2 Coordinates
    #' spreadsheet
    mutate(Location = ifelse(Location == "Cherry Creek State Park West (217a)",
                             "Cherry Creek State Park East (217a)",
                             Location)) %>% 
    slice(-1) 
  
  #' make temp2 data.frame look like the temp df for campaign 1
  #' Join based on location
  #' Keep street addresses for residential locations (anything with a ",CO" in 
  #' the name) and geocode in the next script
  #' Add coordinates for parks and other community sites
  temp2 <- left_join(temp2, coordinate_df, by = "Location") %>% 
    mutate(Details_GPS = ifelse(str_detect(Location, ", CO"), 
                                Details_GPS, Details_GPS_2)) %>% 
    select(-Details_GPS_2)
  
  catalog_data2 <- bind_rows(catalog_data2, temp2)
  rm(temp2)
}

catalog_data2 <- mutate(catalog_data2, campaign = campaign_name2) %>%
  mutate(filter_id = as.character(filter_id))
names(catalog_data2)
glimpse(catalog_data2)
sum(duplicated(catalog_data2$filter_id))

#' -----------------------------------------------------------------------------
#' Sampling locations-- Campaign 1 
#' -----------------------------------------------------------------------------

campaign_name <- "Campaign1"
catalogs <- list.files(path = catalog_path, pattern = campaign_name, 
                       full.names = T)

catalog_data1 <- data.frame()

for(i in 1:length(catalogs)) {
  temp <- read_xlsx(catalogs[i], sheet = 2, range = cell_cols(1:10)) %>% 
    rename("Details_GPS" = "...10",
           "filter_id" = "Filter code") %>% 
    select(filter_id, Location, Details_GPS) %>% 
    slice(-1) 
  
  #' Correct Heller Pond name
  temp <- temp %>%
    mutate(Location = ifelse(str_detect(Location, "Heron Pond/ Heller Open Space"),
                             "Heron Pond/Heller Open Space",
                             Location))
  
  catalog_data1 <- bind_rows(catalog_data1, temp) %>% 
    mutate(campaign = campaign_name)
  rm(temp)
}

catalog_data1 <- left_join(catalog_data1, coordinate_df, by = "Location") %>% 
  mutate(Details_GPS = ifelse(str_detect(Location, ", CO"), 
                              Details_GPS, Details_GPS_2)) %>% 
  select(-Details_GPS_2)

catalog_data1 <- mutate(catalog_data1, campaign = campaign_name) %>%
  mutate(filter_id = as.character(filter_id))
names(catalog_data1)
glimpse(catalog_data1)
sum(duplicated(catalog_data1$filter_id))

#' Community sites:
comm_sites_1 <- filter(catalog_data1, !str_detect(Location, "CO")) %>%
  select(Location, Details_GPS) %>%
  distinct()

comm_sites_check <- full_join(comm_sites_1, comm_sites_2, by = "Location") %>%
  mutate(check1v2 = ifelse(Details_GPS == Details_GPS_2, 1, 0))

#' -----------------------------------------------------------------------------
#' Sampling locations: Campaign 3
#' -----------------------------------------------------------------------------

campaign_name3 <- c("Campaign3")
catalogs3 <- list.files(path = catalog_path, pattern = campaign_name3, 
                        full.names = T)

catalog_data3 <- data.frame()

for(i in 1:length(catalogs3)) {
  temp3 <- read_xlsx(catalogs3[i], 
                     sheet = 2, range = cell_cols(1:10)) %>% 
    rename("Details_GPS" = "...10",
           "filter_id" = "Filter code") %>% 
    select(filter_id, Location, Details_GPS) %>% 
    
    #' Fix creekside park naming issue
    mutate(Location = ifelse(str_detect(Location, "Creekside"), 
                             "Creekside Park",
                             Location)) %>% 
    
    #' Fix Hudson st address and Emporia st address
    mutate(Location = ifelse(str_detect(Location, "370 Hudson street, Denver, 80220"),
                             "370 Hudson street, Denver, CO 80220",
                             Location),
           Location = ifelse(str_detect(Location, "2836 Emporia Street, Denver 80238"),
                             "2836 Emporia Street, Denver, CO 80238",
                             Location)) %>%
    
    #' filter out "cancelled" filters
    filter(!str_detect(Details_GPS, "cancelled")) %>% 
    
    slice(-1) 
  
  #' Get anschutz lab coordinates
  anschutz_coords <- filter(temp3, str_detect(Details_GPS, "#33")) %>% 
    select(-filter_id) %>% 
    distinct() %>% 
    mutate(coords = str_split(Details_GPS, ";")[[1]][2]) %>% 
    mutate(lon = str_split(coords, ",")[[1]][1],
           lat = str_split(coords, ",")[[1]][2])
  
  #' make temp3 data.frame look like the temp df for campaign 1
  #' Join based on location
  #' Keep street addresses for residential locations (anything with a ", CO" in 
  #' the name) and geocode in the next script
  #' Add coordinates for parks and other community sites
  #' Add Anschutz lab coordinates back in
  temp3 <- left_join(temp3, coordinate_df, by = "Location") %>% 
    mutate(Details_GPS = ifelse(str_detect(Location, ", CO"), 
                                Details_GPS, Details_GPS_2)) %>% 
    mutate(Details_GPS = ifelse(str_detect(Location, "Anschutz"), 
                                paste0(anschutz_coords$lat, ", ",
                                       anschutz_coords$lon), 
                                Details_GPS)) %>% 
    select(-Details_GPS_2)
  
  catalog_data3 <- bind_rows(catalog_data3, temp3)
  rm(temp3)
}

catalog_data3 <- mutate(catalog_data3, campaign = campaign_name3) %>%
  mutate(filter_id = as.character(filter_id))
names(catalog_data3)
sum(duplicated(catalog_data3$filter_id))

comm_sites_3 <- filter(catalog_data3, !str_detect(Location, "CO")) %>%
  select(Location, Details_GPS) %>%
  rename(Details_GPS_3 = "Details_GPS") %>%
  distinct()

comm_sites_check <- full_join(comm_sites_check, comm_sites_3, by = "Location") %>%
  mutate(check2v3 = ifelse(Details_GPS_2 == Details_GPS_3, 1, 0))

#' -----------------------------------------------------------------------------
#' Sampling locations: Campaign 4
#' -----------------------------------------------------------------------------

campaign_name4 <- c("Campaign4")
catalogs4 <- list.files(path = catalog_path, pattern = campaign_name4, 
                        full.names = T)

catalog_data4 <- data.frame()

for(i in 1:length(catalogs4)) {
  temp4 <- read_xlsx(catalogs4[i], 
                     sheet = 2, range = cell_cols(1:11)) %>% 
    rename("Details_GPS" = "...11",
           "filter_id" = "Filter code") %>% 
    select(filter_id, Location, Details_GPS) %>% 
    
    #' Fix creekside park naming issue
    mutate(Location = ifelse(str_detect(Location, "Creekside"), 
                             "Creekside Park",
                             Location)) %>% 
    
    #' filter out "cancelled" filters
    filter(!str_detect(Details_GPS, "cancelled")) %>% 
    
    slice(-c(1:2)) 
  
  #' make temp4 data.frame look like the temp df for campaign 1
  #' Join based on location
  #' Keep street addresses for residential locations (anything with a ",CO" in 
  #' the name) and geocode in the next script
  #' Add coordinates for parks and other community sites
  temp4 <- left_join(temp4, coordinate_df, by = "Location") %>% 
    mutate(Details_GPS = ifelse(str_detect(Location, ", CO"), 
                                Details_GPS, Details_GPS_2)) %>% 
    select(-Details_GPS_2)
  
  catalog_data4 <- bind_rows(catalog_data4, temp4)
  rm(temp4)
}

catalog_data4 <- mutate(catalog_data4, campaign = campaign_name4) %>%
  mutate(filter_id = as.character(filter_id))
names(catalog_data4)
sum(duplicated(catalog_data4$filter_id))

comm_sites_4 <- filter(catalog_data4, !str_detect(Location, "CO")) %>%
  select(Location, Details_GPS) %>%
  rename(Details_GPS_4 = "Details_GPS") %>%
  distinct()

comm_sites_check <- full_join(comm_sites_check, comm_sites_4, by = "Location") %>%
  mutate(check2v4 = ifelse(Details_GPS_2 == Details_GPS_4, 1, 0))

#' -----------------------------------------------------------------------------
#' Sampling locations: Campaign 5 (Only two locations; Navajo St. and Yuma St)
#' -----------------------------------------------------------------------------

campaign_name5 <- c("Campaign5")
catalogs5 <- list.files(path = catalog_path, pattern = campaign_name5, 
                        full.names = T)

catalog_data5 <- data.frame()

for(i in 1:length(catalogs5)) {
  temp5 <- read_xlsx(catalogs5[i], 
                     sheet = 2, range = cell_cols(1:11)) %>% 
    rename("Details_GPS" = "...11",
           "filter_id" = "Filter code") %>% 
    select(filter_id, Location, Details_GPS) %>% 
    
    #' Fix creekside park naming issue
    #' Fix Navajo St name issue
    mutate(Location = ifelse(str_detect(Location, "Creekside"), 
                             "Creekside Park",
                             Location)) %>% 
    mutate(Location = ifelse(str_detect(Location, "Navajo"), 
                             "Navajo St.",
                             Location)) %>%
    
    #' filter out "cancelled" filters
    filter(!str_detect(Details_GPS, "cancelled")) %>% 
    
    slice(-c(1:2)) 
  
  #' make temp5 data.frame look like the temp df for campaign 1
  #' Join based on location
  #' Keep street addresses for residential locations (anything with a ",CO" in 
  #' the name) and geocode in the next script
  #' Add coordinates for parks and other community sites
  temp5 <- left_join(temp5, coordinate_df, by = "Location") %>% 
    mutate(Details_GPS = ifelse(str_detect(Location, ", CO"), 
                                Details_GPS, Details_GPS_2)) %>% 
    select(-Details_GPS_2)
  
  catalog_data5 <- bind_rows(catalog_data5, temp5)
  rm(temp5)
}

catalog_data5 <- mutate(catalog_data5, campaign = campaign_name5) %>%
  mutate(filter_id = as.character(filter_id))
names(catalog_data5)
sum(duplicated(catalog_data5$filter_id))

comm_sites_5 <- filter(catalog_data5, !str_detect(Location, "CO")) %>%
  select(Location, Details_GPS) %>%
  rename(Details_GPS_5 = "Details_GPS") %>%
  distinct()

comm_sites_check <- full_join(comm_sites_check, comm_sites_5, by = "Location") %>%
  mutate(check2v5 = ifelse(Details_GPS_2 == Details_GPS_5, 1, 0))

#' -----------------------------------------------------------------------------
#' Combine all catalog data
#' Make sure location data are consistent for all campaigns
#' For co-located monitoring locations, listed below, use the coordinates of the 
#' monitors as listed by USEPA to make sure these align 
#' Rename 1400 Jackson as NJH (National Jewish Hospital)
#' 
#' 9th and Yuma: 39.73217, -105.0153
#' 49th and Acoma: 39.7861	-104.9886
#' Navajo St: 39.77949, -105.00518
#' 1400 Jackson St, Denver, CO 80206 (NJH): 39.738578	-104.939925
#' -----------------------------------------------------------------------------

catalog_data <- bind_rows(catalog_data1, catalog_data2, 
                          catalog_data3, catalog_data4, catalog_data5)
glimpse(catalog_data)

monitor_loc <- data.frame(Location = c("9th and Yuma", "49th and Acoma",
                                       "Navajo St.", "1400 Jackson St, Denver, CO 80206"),
                          Details_GPS = c("39.73217, -105.0153", "39.7861, -104.9886",
                                          "39.77949, -105.00518", "39.738578, -104.939925"))

other_data <- filter(catalog_data, !(Location %in% monitor_loc$Location))
coloc_data <- filter(catalog_data, Location %in% monitor_loc$Location)

#' Check to make sure data were filtered correctly
nrow(other_data) + nrow(coloc_data) == nrow(catalog_data)

#' Add coordinates for collocated monitors and correct the name of NJH site
coloc_data <- coloc_data %>%
  select(-Details_GPS) %>%
  left_join(monitor_loc, by = "Location") %>%
  mutate(Location = ifelse(str_detect(Location, "1400 Jackson"),
                           "NJH", Location))

#' Recombine data sets
catalog_data_corrected <- bind_rows(other_data, coloc_data) %>%
  arrange(filter_id)
nrow(catalog_data_corrected) == nrow(catalog_data)

write_csv(catalog_data_corrected, here::here("Data/Filter_Data", "Filter_Locations.csv"))
