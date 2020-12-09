#' =============================================================================
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Clean UPAS meta data
#' Date created: November 26, 2018
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description: Summarizing location data for each filter and geocoding the 
#' participant addresses
#' 
#' NOTE: The outputs of this script are PHI and cannot be shared!!
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

register_google(key = google_api_key)

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

map_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
options(scipen = 9999) #avoid scientific notation

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Cleaning up the campaign location data and subsetting the collocated monitors
#' Geocoding participant addresses
#' Creating the sf object
#' Mapping sampling locations
#' -----------------------------------------------------------------------------

campaign_locations <- read_csv(here::here("Data/Filter_Data", 
                                          "Filter_Locations.csv")) %>% 
  
  #' Fix participant addresses 
  mutate(Location = ifelse(Location == "4699 Kittredge St. #111, Denver, CO 80210", 
                           "4699 Kittredge St. #111, Denver, CO 80239",
                           Location)) %>% 
  mutate(Location = ifelse(Location == "4872 Dearborn St., Dearborn, CO 80239",
                           "4872 Dearborn St., Denver, CO 80239",
                           Location))
  
  #' #' Fix Community Locations
  #' mutate(Details_GPS = ifelse(Location == "Cherry Creek State Park West (218a)" & 
  #'                               campaign == "Campaign1", 
  #'                             "39.6303019, -104.85968", Details_GPS)) %>% 
  #' mutate(Details_GPS = ifelse(Location == "Cherry Creek State Park East (217a)" & 
  #'                               campaign == "Campaign1", 
  #'                             "39.6317924, -104.8356315", Details_GPS)) %>% 
  #' mutate(Details_GPS = ifelse(Location == "Discovery Park (203a)" &
  #'                               campaign == "Campaign1",
  #'                             "39.7671419, -105.108995", Details_GPS)) %>% 
  #' mutate(Details_GPS = ifelse(Location == "Heron Pond/ Heller Open Space" &
  #'                               campaign == "Campaign1",
  #'                             "39.792604, -104.97219", Details_GPS)) %>% 
  #' mutate(Location = ifelse(Location == "Heron Pond/ Heller Open Space",
  #'                          "Heron Pond/Heller Open Space",
  #'                          Location)) %>% 
  #' mutate(Details_GPS = ifelse(Location == "Bear Valley Park" &
  #'                             campaign == "Campaign1",
  #'                             "39.660873, -105.07577", Details_GPS)) %>% 
  #' mutate(Details_GPS = ifelse(Location == "Bowles Grove Park" &
  #'                               campaign == "Campaign1",
  #'                             "39.6143391, -105.0287366", Details_GPS)) %>% 
  #' mutate(Details_GPS = ifelse(Location == "deKoevend Park" &
  #'                               campaign == "Campaign1",
  #'                             "39.59865, -104.9583136", Details_GPS)) %>% 
  #' mutate(Details_GPS = ifelse(Location == "Johnson Park (207a)" &
  #'                               campaign == "Campaign1",
  #'                             "39.7835858, -105.0830168", Details_GPS)) %>% 
  #' mutate(Details_GPS = ifelse(Location == "Speer Blvd Park (214a)" &
  #'                               campaign == "Campaign1",
  #'                             "39.7511608, -105.0053618", Details_GPS)) %>%
  #' mutate(Details_GPS = ifelse(Location == "Standley Lake North Open Space Park (204a)" &
  #'                               campaign == "Campaign1",
  #'                             "39.8737971, -105.1294784", Details_GPS))
  
#' Comfirm there's only one "Details_GPS" value per Location value
gps_check <- select(campaign_locations, Details_GPS, Location) %>% 
  distinct()
  
#' Additional data cleaning
campaign_locations <- campaign_locations %>% 

  #' Drop anything where "Location" is blank
  filter(!is.na(Location)) %>% 
  
  #' Rename field blank filter IDs to "B#" to match Christian's data
  mutate(filter_id = gsub("Field blank #", "B", filter_id)) %>% 
  mutate(filter_id = str_pad(filter_id, width = 6, pad= "0")) %>% 
  
  #' Make everything in the details_gps column lowercase
  mutate(Details_GPS = tolower(Details_GPS)) %>% 
  
  #' Is this an address? 1 = yes, 0 = no
  #' Location contains "CO", meaning it is an address to geocode
  mutate(needs_geocoding = ifelse(str_detect(Location, "CO"), 1, 0)) %>%
  
  #' Is this a participant?
  mutate(participant = ifelse(str_detect(Details_GPS, 
                                         paste(c("collocated", "participant"), 
                                               collapse = "|")),
                              1, 0)) %>% 
  
  #' Is this an indoor monitor? 1 = yes, 0 = no
  #' Details GPS says "collocated" or "residence of" for ourdoor monitors
  #' Details GPS says "Participant #" for indoor monitors
  mutate(indoor = ifelse(participant == 1, 
                         ifelse(str_detect(Details_GPS, 
                                           paste(c("collocated", "residence"), 
                                                 collapse = "|")), 0, 1), 0)) %>% 
  
  #' Get addresses for geocoding
  mutate(geocode_add = ifelse(needs_geocoding == 1, Location, NA)) %>% 
  
  #' extract lon and lat data from community sites
  rowwise() %>% 
  mutate(lon = ifelse(needs_geocoding == 0, str_split(Details_GPS, ",")[[1]][2], NA),
         lat = ifelse(needs_geocoding == 0, str_split(Details_GPS, ",")[[1]][1], NA)) %>% 
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat)) 

#' Get location information for the 49th and Acoma (Globeville), I25 Denver (Yuma St)
#' and National Jewish (1400 Jackson) monitors
#' We've collocated samplers-- need this for calibrating later
collocation_sites <- c("49th and Acoma", 
                       "9th and Yuma", 
                       "1400 Jackson St, Denver, CO 80206")

collocation_filters <- select(campaign_locations, filter_id, Location) %>% 
  filter(Location %in% collocation_sites)
write_csv(collocation_filters, here::here("Data", "Collocation_Filter_IDs.csv"))

#' List of unique addresses to geocode
geocode_add <- campaign_locations %>% 
  filter(is.na(lon)) %>% 
  filter(!is.na(geocode_add)) %>% 
  dplyr::select(geocode_add) %>% 
  distinct()

#' geocoding using ggmap-- need to cite!
#' Make sure you have a Google API key registered in the header of this script
geocode_add <- as.data.frame(geocode_add)

geocode_geo <- data.frame()
for(i in 1:nrow(geocode_add)) {
  df <- slice(geocode_add, i)
  df2 <- mutate_geocode(df, geocode_add, override_limit=T)
  geocode_geo <- bind_rows(geocode_geo, df2)
}

head(geocode_geo)
View(geocode_geo)

#' Manually geocode one address
geocode_geo$lon[7] <- -104.804943
geocode_geo$lat[7] <- 39.776964

#' Merge geocoded addresses with original data
locations_df <- left_join(campaign_locations, geocode_geo, 
                          by=c("geocode_add")) %>% 
  mutate(lon = ifelse(is.na(lon.x), lon.y, lon.x),
         lat = ifelse(is.na(lat.x), lat.y, lat.x)) %>% 
  mutate(lon2 = lon, lat2 = lat) %>% 
  dplyr::select(-c(lon.x, lon.y, lat.x, lat.y))

glimpse(locations_df)

#' Create and sf object using the geocoded locations
#' EPSG 4236 is WGS84
locations_sf <- filter(locations_df, !is.na(lon2)) %>% 
  st_as_sf(coords = c("lon2", "lat2"), crs = ll_wgs84)

plot(st_geometry(locations_sf))

locations_aea <- st_transform(locations_sf, crs=albers)

#' Write out the geocoded locations
locations_file_name <- "Filter_Locations_AEA.csv"
st_write(locations_aea, here::here("Data/Filter_Data", locations_file_name),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

names(locations_aea)
sum(duplicated(locations_aea$filter_id))

#' Plot unique locations by type (residence vs. community)
locations1 <- dplyr::distinct(locations_aea, participant) %>% 
  st_transform(crs = ll_wgs84)
plot(st_geometry(filter(locations1, participant == 1)), col="blue")
plot(st_geometry(filter(locations1, participant == 0)), col="red", add=T)

#' Map of campaign sampling locations (residences jittered to protect privacy)
#' Map in ggmap
base_map <- get_map(location = "Commerce City, CO", zoom = 10)
ggmap(base_map)
attr(base_map, "bb")

x_min <- as.numeric(attr(base_map, "bb")["ll.lon"][1,1])
x_max <- as.numeric(attr(base_map, "bb")["ur.lon"][1,1])
y_min <- as.numeric(attr(base_map, "bb")["ll.lat"][1,1])
y_max <- as.numeric(attr(base_map, "bb")["ur.lat"][1,1])

ggmap(base_map) +
  #ggplot() +
  geom_sf(data = st_jitter(filter(locations1, participant == 1), 0.02), 
          aes(fill = "res", color = "res"),
          inherit.aes = F) +
  geom_sf(data = filter(locations1, participant == 0),
          aes(fill = "com", color = "com"),
          inherit.aes = F) +
  scale_color_manual(name = "Sampling\nLocation Type",
                     values = c("com" = "red", "res" = "blue"),
                     labels = c("com" = "Community", "res" = "Residence")) +
  scale_fill_manual(name = "Sampling\nLocation Type",
                    values = c("com" = "red", "res" = "blue"),
                    labels = c("com" = "Community", "res" = "Residence")) +
  north(x.min = x_min, x.max = x_max,
        y.min =  y_min, y.max = y_max,
        symbol = 10, location = "topright", scale = 0.05) +
  scalebar(x.min = x_min, x.max = x_max,
           y.min =  y_min, y.max = y_max,
           dist_unit = "km", dist = 10, transform = T,
           st.bottom = F, st.size = 3, 
           height = 0.01,
           location = "bottomright",
           anchor = c(x = x_min*0.992,
                      y = y_min + y_min*0.001)) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.15, 0.85)) +
  map_theme

fig_name <- "LUR_Sampling_Sites.jpeg"
ggsave(filename = here::here("Figs", fig_name), 
       device = "jpeg", dpi=500, units = "in", height = 6, width = 6)

#' -----------------------------------------------------------------------------
#' Time stamp issue:
#' Rev 101 of the firmware had a issue where the UTC time stamps differed from 
#' the "elapsed time"-- the incremental time stamps for each logged metadata 
#' measurement. This section of code identifies where this mismatch occurs
#' -----------------------------------------------------------------------------

timestamp_name <- "UPAS_Timestamps.csv"
timestamps <- read_csv(here::here("Data/UPAS_Data", timestamp_name))

upas_ids <- unique(timestamps$upas_id)

#' Empty data frame for results
time_diff_df <- data.frame()

for (i in 1:length(upas_ids)) {
  time_data <- filter(timestamps, upas_id == upas_ids[i])
  
  for (camp in 1:length(unique(time_data$campaign))) {
    time_data2 <- filter(time_data, campaign == unique(time_data$campaign)[camp])
    weeks <- unique(time_data2$week) 
    
    for (j in 1:length(weeks)) {
      time <- filter(time_data2, week == weeks[j]) 
      
      if(nrow(time) < 2) next
      
      #' what is the interval between measurements?
      interval <- as.numeric(time$SampleTime[2] - time$SampleTime[1])
      
      time2 <- time %>% 
        select(SampleTime, UnixTime, DateTimeUTC, DateTimeLocal) %>%
        mutate(DateTimeUTC = gsub("T", " ", DateTimeUTC),
               DateTimeLocal = gsub("T", " ", DateTimeLocal)) %>%
        mutate(DateTimeUTC = as.POSIXct(DateTimeUTC, tz="GMT", format="%Y-%m-%d %H:%M:%S"),
               DateTimeLocal = as.POSIXct(DateTimeLocal, 
                                          tz="America/Denver", format="%Y-%m-%d %H:%M:%S"),
               
               #' POSIXct uses seconds, so add 21600 seconds to get UTC from local time
               Local_to_UTC = DateTimeLocal + (6*60*60),
               elapsed_time_s = ((as.numeric(rownames(.)) - 1) * interval)) %>%
        mutate(elapsed_time_h = elapsed_time_s / (60*60))
      
      #' Regress elapsed time by UTC time stamp
      #' Plot the time stamps to see where they are different
      #' MDT is 6 hours behind UTC
      utc_lm <- lm(elapsed_time_s ~ DateTimeUTC, time2)
      utc_slope <- summary(utc_lm)$coefficients[2]
      
      local_lm <- lm(elapsed_time_s ~ DateTimeLocal, time2)
      summary(local_lm)
      local_slope <- summary(local_lm)$coefficients[2]
      
      #' How well are the UTC and local times correlated?
      utc_local_correlation <- cor(as.numeric(time$DateTimeUTC), 
                                   as.numeric(time$DateTimeLocal))
      
      temp <- data.frame(campaign = unique(time_data$campaign)[camp],
                         week = weeks[j],
                         UPASserial = paste0("PS", str_pad(upas_ids[i], 
                                                           width = 4, 
                                                           pad= "0")),
                         utc_slope = utc_slope,
                         local_slope = local_slope,
                         utc_local_correlation = utc_local_correlation)
      time_diff_df <- bind_rows(time_diff_df, temp)
      rm(temp)
    }
  }
}

#' write out time difference data
time_diff_file_name <- "Sample_TimeDiff.csv"
write_csv(time_diff_df, here::here("Data/UPAS_Data", time_diff_file_name))

rm(time, time_data, time_data2, time_diff_df, time2, timestamps, utc_lm, local_lm)

#' -----------------------------------------------------------------------------
#' Cleaning up the UPAS metadata
#' Calculating the sampling volumes
#' 
#' Note: LoggedRunTime is in h and VolumetricFlowRate is in L/min
#' Note: SampledVolume is in L
#' 
#' Note: For Campaigns 1 and 2, some of the logged runtimes (UPAS metadata) don't 
#' match the runtimes we would expect based on the field team reports of run times.
#' This was due to an issue with the Rev 101 version of the UPAs firmware. When
#' logged and calculated run times differed, we flagged the data. Time-weighted
#' averages for PM and BC will be calculated using volumes based on both versions
#' of the run time (which will be identical (within 0.5%) for many if not most of 
#' the samples.)
#' -----------------------------------------------------------------------------

#' function to assign a month
month_function <- function(s_date, e_date, na.rm = T) {
  if(is.na(s_date) | is.na(e_date)) {
    return(NA)
  } else {
    date_seq <- seq.Date(from = s_date, to = e_date, by = "day")
    month_seq <- month(date_seq)
    
    month_seq = month_seq[!is.na(month_seq)]
    
    ux <- unique(month_seq)
    
    if(length(ux) == 1) {
      return(ux)
    } else {
      return(ux[which.max(tabulate(match(month_seq, ux)))])
    }
  }
}

link_path <- "R:/RSTOR-Magzamen/Research/Projects/ECHO_Aim1/Raw_Data/Filter_Monitor_Links/"

#' Link filters to monitors
link_data_name <- paste0(link_path, "Campaign_Links.xlsx")
links <- read_excel(link_data_name) %>% 
  select(campaign = Campaign, UPASserial, week = CampaignWeek, MonitorNumber, 
         filter_id = Filter_Code) %>% 
  mutate(week = paste0("Week", as.numeric(week)),
         campaign = paste0("Campaign", campaign))

#' time differences to flag
time_diff <- read_csv(here::here("Data/UPAS_Data", "Sample_TimeDiff.csv"))

#' metadata
metadata_name <- "UPAS_Metadata.csv"
volumes <- read_csv(here::here("Data/UPAS_Data", metadata_name)) %>% 
  mutate(UPASserial = paste0("PS", str_pad(UPASserial, width = 4, pad = "0"))) %>%
  mutate(week = gsub(" ", "", week)) %>% 
  dplyr::select(campaign, UPASserial, UPASfirmware, VolumetricFlowRate, 
                DutyCycle, SampledVolume, SampledRuntime, LoggedRuntime, 
                AverageVolumetricFlowRate, 
                StartDateTimeLocal, EndDateTimeLocal, 
                StartDateTimeUTC, EndDateTimeUTC, week) %>% 
  
  #' add in the filter IDs from Grace's spreadsheet-- see 08_Scrape_UPAS_Metadata.R
  left_join(links, by = c("UPASserial", "week", "campaign")) %>%
  
  #' add in the data on time differences
  full_join(time_diff, by = c("UPASserial", "week", "campaign")) %>% 
  
  #' We'll calculate sample run times using both sets of time stamps as well 
  #' as what the UPAS logged as it's sample run time. Because the UTC 
  #' timestamps have an issue with "time drift" for some monitors (issues with 
  #' the firmware-- see above code); we're going favor logged sample run times 
  #' extracted from the metadata, but will have all three here just in case
  mutate(SampledVolume = as.numeric(SampledVolume)) %>% 
  
  mutate(StartDateTimeLocal = as.Date(StartDateTimeLocal, format = "%Y-%m-%d %H:%M:%S"),
         EndDateTimeLocal = as.Date(EndDateTimeLocal, format = "%Y-%m-%dT%H:%M:%S"),
         StartDateTimeUTC = as.Date(StartDateTimeUTC, format = "%Y-%m-%d %H:%M:%S"),
         EndDateTimeUTC = as.Date(EndDateTimeUTC, format = "%Y-%m-%dT%H:%M:%S")) %>% 
  
  mutate(calc_runtime_Local = as.numeric(difftime(EndDateTimeLocal, 
                                                  StartDateTimeLocal, units = "hours")),
         calc_runtime_UTC = as.numeric(difftime(EndDateTimeUTC, 
                                                StartDateTimeUTC, units = "hours")),
         logged_runtime = as.numeric(LoggedRuntime)) %>% 
  
  #' Flag is these two runtimes differ from each other or the logged sample runtime
  mutate(runtime_timezone_flag = ifelse(calc_runtime_Local != calc_runtime_UTC, 1, 0),
         runtime_local_logged_flag = ifelse(calc_runtime_Local != logged_runtime, 1, 0),
         runtime_UTC_logged_flag = ifelse(calc_runtime_UTC != logged_runtime, 1, 0)) %>%  
  
  mutate(StartDateLocal = as.Date(StartDateTimeLocal, format = "%Y-%m-%d %H:%M:%S"),
         EndDateLocal = as.Date(EndDateTimeLocal, format = "%Y-%m-%dT%H:%M:%S")) %>% 
  
  rename("UPAS" = "UPASserial") %>% 
  mutate(filter_id = str_remove(filter_id, '[[:punct:] ]+')) %>% 
  mutate(filter_id = str_pad(filter_id, width = 6, pad= "0")) %>% 
  
  #' sampling week and month based on start and end dates
  #' The weeks are identified using isoweek() in lubridate
  #' Months are based on the start date
  mutate(start_iso_week = isoweek(as.Date(StartDateLocal)),
         start_month = month(as.Date(StartDateLocal)),
         start_year = year(as.Date(StartDateLocal)),
         end_iso_week = isoweek(as.Date(EndDateLocal)),
         end_month = month(as.Date(EndDateLocal)),
         end_year = year(as.Date(EndDateLocal))) %>% 
  
  rowwise() %>% 
  mutate(month = month_function(s_date = as.Date(StartDateLocal), 
                                e_date = as.Date(EndDateLocal)),
         year = year(as.Date(EndDateTimeLocal))) %>% 
  
  #' month_yr for ST covariates
  mutate(month_yr = paste(month, year, sep="_")) %>% 
  
  #' season is meteorological season (w.g., winter == Dec, Jan, Feb)
  mutate(season = ifelse(month %in% c(12, 1, 2), 1, 
                         ifelse(month %in% c(3, 4, 5), 2, 
                                ifelse(month %in% c(6, 7, 8), 3, 4)))) %>% 
  
  #' Calculate three sample volumes based on 1) logged run time time, 
  #' 2) UTC run time, and 3) local run time  
  mutate(logged_rt_volume_L = (logged_runtime*60) * 
           (as.numeric(VolumetricFlowRate)*(as.numeric(DutyCycle)/100)),
         local_rt_volume_L = (calc_runtime_Local*60) * 
           (as.numeric(VolumetricFlowRate)*(as.numeric(DutyCycle)/100)),
         utc_rt_volume_L = (calc_runtime_UTC*60) * 
           (as.numeric(VolumetricFlowRate)*(as.numeric(DutyCycle)/100))) %>% 
  
  #' Difference between volumes
  mutate(vol_pctdiff_local_utc = ((local_rt_volume_L - utc_rt_volume_L) / local_rt_volume_L) * 100,
         vol_pctdiff_logged_local = ((logged_rt_volume_L - local_rt_volume_L) / logged_rt_volume_L) * 100,
         vol_pctdiff_logged_utc = ((logged_rt_volume_L - utc_rt_volume_L) / logged_rt_volume_L) * 100) %>% 
  
  #' Flag sampled volumes less than 1000 L
  #' Flag flowrates that are not within 5% of 1 L/min
  #' Flag if volumes are not within 0.5%
  mutate(low_volume_flag = ifelse(logged_rt_volume_L < 1000, 1, 0),
         flow_rate_flag = ifelse(VolumetricFlowRate < 1*0.95 | 
                                   VolumetricFlowRate > 1*1.05, 1, 0),
         volume_mismatch_local_utc = ifelse(abs(vol_pctdiff_local_utc) > 0.5, 1, 0),
         volume_mismatch_logged_local = ifelse(abs(vol_pctdiff_logged_local) > 0.5, 1, 0),
         volume_mismatch_logged_utc = ifelse(abs(vol_pctdiff_logged_utc) > 0.5, 1, 0)) %>% 
  
  #' Convert L to m^3 based on calculated volume
  mutate(logged_rt_volume_m3 = logged_rt_volume_L / (10^3),
         local_rt_volume_m3 = local_rt_volume_L / (10^3),
         utc_rt_volume_m3 = utc_rt_volume_L / (10^3)) 

#' What do the differences in volume look like?
print(summary(volumes$vol_pctdiff_logged_local))
hist(volumes$vol_pctdiff_logged_local)

print(summary(volumes$vol_pctdiff_logged_utc))
hist(volumes$vol_pctdiff_logged_utc)

#' write out volumes
volumes_file_name <- "Sample_Volumes.csv"
write_csv(volumes, here::here("Data/UPAS_Data", volumes_file_name))


