#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 Land Use Regression
#' 
#' Task: Calculate long-term monthly averages at each monitor and ID smoke days
#' 
#' Author: Sheena Martenies
#' 
#' Date Created: November 19, 2018
#' 
#' Contact: sheena.martenies@colostate.edu
#' -----------------------------------------------------------------------------

#' Load required libraries
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(sf)
library(ggplot2)
library(ggthemes)

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

#' Coordinate reference systems 
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' -----------------------------------------------------------------------------
#' 1) Calculate long-term (2009-2019) averages by month and monitor

years <- c(2008:2019)
time_zone <- "America/Denver"

#' read in all data, summarize to long-term monthly averages
monitor_data_all <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(month = month(Date_Local),
         year = year(Date_Local)) %>% 
  filter(year %in% years)

#' Just use the 24H data for the primary instrument at each location
# monitor_data_temp <- filter(monitor_data_all, Sample_Duration != "1 HOUR") %>%
#   filter(POC == 1)

monitor_data <- filter(monitor_data_all, POC == 1)

monitor_means <- monitor_data %>% 
  group_by(monitor_id, month) %>% 
  summarize(monthly_mean = mean(Arithmetic_Mean, na.rm=T),
            monthly_sd = sd(Arithmetic_Mean, na.rm=T))

ggplot(monitor_means) +
  geom_line(aes(x = month, y = monthly_mean, col=factor(monitor_id))) +
  simple_theme

#' -----------------------------------------------------------------------------
#' 2) ID dates where we have monitoring data and smoke data

pm_dates <- gsub("-", "", as.character(unique(monitor_data$Date_Local)))

#' list of smoke files
smoke_files <- list.files(here::here("Data/Smoke_Data"))
smoke_files <- smoke_files[which(str_detect(smoke_files, paste(c(years),collapse = '|')))]

num_exp <- "[[:digit:]]+"

smoke_dates <- str_extract(smoke_files, num_exp)

shared_dates <- intersect(pm_dates, smoke_dates)

#' -----------------------------------------------------------------------------
#' 2) Determine if each monitor overlaps with plume (within 50 km) and has a
#' concentration 1 SD or 2 SD higher than the long term monthly average

#' loop through dates
smoke_days <- data.frame()

buff_distance <- 50000

for(i in 1:length(shared_dates)) {
  print(paste(i, "of", length(shared_dates), "dates"))
  
  smoke_name <- paste0("hms_smoke", shared_dates[i], "_AEA.csv")
  
  if(!file.exists(here::here("Data/Smoke_Data", smoke_name))) {
    temp <- data.frame(date = shared_dates[i],
                       monitor = NA,
                       plume_within_50km = NA,
                       pm_exceeds_mean_1sd = NA,
                       pm_exceeds_mean_2sd = NA)
    smoke_days <- bind_rows(smoke_days, temp)
    rm(temp)
    
  } else {
    plumes <- read_csv(here::here("Data/Smoke_Data", smoke_name)) 
    
    if(plumes$WKT[1] != "GEOMETRYCOLLECTION EMPTY") {
      plumes_sf <- st_as_sf(plumes, wkt = "WKT", crs = albers)
      plumes_sf <- filter(plumes_sf, !is.na(st_is_valid(plumes_sf)))
      
      monitors_temp <- filter(monitor_data, 
                              Date_Local == as.Date(shared_dates[i], format = "%Y%m%d")) 
      monitor_ids <- unique(monitors_temp$monitor_id)
      
      monitor_means <- monitor_data %>% 
        group_by(monitor_id, month) %>% 
        summarize(monthly_mean = mean(Arithmetic_Mean, na.rm=T),
                  monthly_sd = sd(Arithmetic_Mean, na.rm=T))
      
      for (j in 1:length(monitor_ids)) {
        monitor_sf <- filter(monitors_temp, monitor_id == monitor_ids[j])
        if(nrow(monitor_sf) == 0) next
        
        # monitor_sf <- st_as_sf(monitor_sf, wkt = "WKT", crs = albers)
        
        monitor_buffer <- st_buffer(monitor_sf, dist = buff_distance)
        
        #' Do any of the plumes intersect with the 50 km buffer?
        plume_intersect <- any(st_intersects(monitor_buffer, plumes_sf, sparse = F))
        
        #' is the daily mean higher than 1 sd above the long-term average?
        #' is the daily mean higher than 2 sd above the long-term average?
        long_term_mean <- filter(monitor_means, monitor_id == monitor_ids[j])
        monthly_lt_mean <- filter(long_term_mean, 
                                  month == month(as.Date(shared_dates[i], format = "%Y%m%d")))
        monthly_metric_1sd <- monthly_lt_mean$monthly_mean + (1*monthly_lt_mean$monthly_sd)
        monthly_metric_2sd <- monthly_lt_mean$monthly_mean + (2*monthly_lt_mean$monthly_sd)
        
        pm_exceeds_1sd <- monitor_sf$Arithmetic_Mean >= monthly_metric_1sd
        pm_exceeds_2sd <- monitor_sf$Arithmetic_Mean >= monthly_metric_2sd
        
        temp <- data.frame(date = shared_dates[i],
                           monitor = monitor_ids[j],
                           plume_within_50km = plume_intersect,
                           pm_exceeds_mean_1sd = pm_exceeds_1sd,
                           pm_exceeds_mean_2sd = pm_exceeds_2sd)
        smoke_days <- bind_rows(smoke_days, temp)
        rm(temp)
      }
    } else {
      temp <- data.frame(date = shared_dates[i],
                         monitor = NA,
                         plume_within_50km = NA,
                         pm_exceeds_mean_1sd = NA,
                         pm_exceeds_mean_2sd = NA)
      smoke_days <- bind_rows(smoke_days, temp)
      rm(temp)
    }
    rm(plumes)
  } 
}

mutate_smoke_days <- smoke_days %>% 
  mutate(smoke_day_1sd = plume_within_50km * pm_exceeds_mean_1sd,
         smoke_day_2sd = plume_within_50km * pm_exceeds_mean_2sd,
         Date_Local = as.Date(date, format = "%Y%m%d")) %>% 
  rename(monitor_id = monitor)
table(mutate_smoke_days$plume_within_50km, mutate_smoke_days$pm_exceeds_mean_1sd)
table(mutate_smoke_days$plume_within_50km, mutate_smoke_days$pm_exceeds_mean_2sd)

monitor_smoke_days <- full_join(monitor_data, mutate_smoke_days, 
                                by = c("monitor_id", "Date_Local"))

st_write(monitor_smoke_days, here::here("Data", "Monitor_Smoke_Days_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' -----------------------------------------------------------------------------
#' Spot check 5% of days
#' -----------------------------------------------------------------------------

random_dates <- sample(unique(mutate_smoke_days$Date_Local), 
                       length(unique(mutate_smoke_days$Date_Local)) * 0.05)
sample_days <- filter(mutate_smoke_days, Date_Local %in% random_dates)

table(sample_days$plume_within_50km, sample_days$pm_exceeds_mean_1sd)

#' Which monitors/dates are smoke-impacted?
smoke_impacted <- filter(sample_days, smoke_day_1sd == T)
as.Date(smoke_impacted$Date_Local, format = "%Y%m%d")

#' monitor data
monitor_sf <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(month = month(Date_Local),
         year = year(Date_Local)) %>% 
  filter(year %in% years)

for (i in 1:length(random_dates)) {
  check_date_name <- paste0("hms_smoke", 
                            gsub("-", "", as.character(random_dates[i])),
                            "_AEA.csv")
  
  check_plumes <- read_csv(here::here("Data/Smoke_Data", check_date_name)) %>% 
    st_as_sf(wkt = "WKT", crs = albers) %>% 
    filter(!is.na(st_is_valid(.)))
  
  check_monitor <- filter(monitor_sf, Date_Local == as.Date(random_dates[i],
                                                            format = "%Y%m%d"))
  check_monitor_buffer <- st_buffer(check_monitor, dist = 50000)
  
  #' plot monitors and plumes
  ggplot() +
    ggtitle(paste0("Smoke plumes on ", 
                   as.Date(random_dates[i], format = "%Y%m%d"))) +
    geom_sf(data = check_monitor, color = "blue", size = 2, shape = 16) + 
    geom_sf(data = check_monitor_buffer, color = "blue", fill = NA) +
    geom_sf(data = check_plumes, color = "red", fill = NA) +
    simple_theme
  
  plot_name <- paste0("Smoke_Day_Check_", gsub("-", "", as.character(random_dates[i])), ".jpeg")
  ggsave(here::here("Figs/Smoke_Check", plot_name), device = "jpeg")
}





