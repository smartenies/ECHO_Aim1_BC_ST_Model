#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Identify "smoke impacted" days
#' Date Created: November 19, 2018
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
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

#' Coordinate reference systems 
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' -----------------------------------------------------------------------------
#' 1) Calculate long-term (2009-2019) averages by month and monitor

years <- c(2009:2019)
time_zone <- "America/Denver"

#' read in all data, summarize to long-term monthly averages
monitor_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) 

monitor_sp_data <- select(monitor_data, monitor_id, WKT)
monitor_sp_data <- unique(monitor_sp_data)

monitor_data <- monitor_data %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(month = month(Date_Local),
         year = year(Date_Local)) 

monitor_data_10yr <- filter(monitor_data, year %in% years)

monitor_means <- monitor_data_10yr %>% 
  group_by(monitor_id, month) %>% 
  summarize(monthly_mean = mean(Arithmetic_Mean, na.rm=T),
            monthly_sd = sd(Arithmetic_Mean, na.rm=T))

ggplot(monitor_means) +
  geom_line(aes(x = month, y = monthly_mean, col=factor(monitor_id))) +
  simple_theme

#' -----------------------------------------------------------------------------
#' 2) Determine if each monitor overlaps with plume (within 50 km) and has a
#' concentration 1 SD or 2 SD higher than the long term monthly average

#' loop through dates
smoke_days <- data.frame()

#' unique monitors
monitors <- unique(monitor_data$monitor_id)

#' what buffer do we want to consider?
#' AEA units are meters
buff_distance <- 50000

for(i in 1:length(monitors)) {
  
  print(paste(i, "of", length(monitors), "monitor locations"))
  
  #' A df with just one monitor
  mon_df <- filter(monitor_data, monitor_id == monitors[i])
  
  #' Dates for which we have PM data at that monitor
  pm_dates <- gsub("-", "", as.character(unique(mon_df$Date_Local)))
  
  for(j in 1:length(pm_dates)) {
    smoke_name <- paste0("hms_smoke", pm_dates[j], "_AEA.csv")
    
    #' If there's no shapefile, make each metric NA
    if(!file.exists(here::here("Data/Smoke_Data", smoke_name))) {
      temp <- data.frame(date = pm_dates[j],
                         monitor = monitors[i],
                         daily_mean_pm = NA,
                         plume_within_50km = NA,
                         pm_exceeds_mean_1sd = NA,
                         pm_exceeds_mean_2sd = NA)
      smoke_days <- bind_rows(smoke_days, temp)
      rm(temp)
      
    } else {
      plumes <- read_csv(here::here("Data/Smoke_Data", smoke_name), 
                         col_types = cols()) 
      
      if (nrow(plumes) == 0) {
        temp <- data.frame(date = pm_dates[j],
                           monitor = monitors[i],
                           daily_mean_pm = NA,
                           plume_within_50km = NA,
                           pm_exceeds_mean_1sd = NA,
                           pm_exceeds_mean_2sd = NA)
        smoke_days <- bind_rows(smoke_days, temp)
        rm(temp, plumes)
      
      } else {
        plumes_sf <- st_as_sf(plumes, wkt = "WKT", crs = albers)
        plumes_sf <- filter(plumes_sf, !is.na(st_is_valid(plumes_sf)))
        
        monitors_temp <- filter(mon_df, 
                                Date_Local == as.Date(pm_dates[j], format = "%Y%m%d")) 
        
        monitor_sf <- st_as_sf(monitors_temp, wkt = "WKT", crs = albers) %>%
          summarize(Arithmetic_Mean = mean(Arithmetic_Mean, na.rm = T))
        
        monitor_buffer <- st_buffer(monitor_sf, dist = buff_distance)
        
        #' Do any of the plumes intersect with the 50 km buffer?
        plume_intersect <- any(st_intersects(monitor_buffer, plumes_sf, sparse = F))
        
        #' is the daily mean higher than 1 sd above the long-term average?
        #' is the daily mean higher than 2 sd above the long-term average?
        long_term_mean <- filter(monitor_means, monitor_id == monitors[i] & 
                                   month == month(as.Date(pm_dates[j], format = "%Y%m%d")))
        monthly_metric_1sd <- long_term_mean$monthly_mean[1] + (1*long_term_mean$monthly_sd[1])
        monthly_metric_2sd <- long_term_mean$monthly_mean[1] + (2*long_term_mean$monthly_sd[1])
        
        pm_exceeds_1sd <- monitor_sf$Arithmetic_Mean >= monthly_metric_1sd
        pm_exceeds_2sd <- monitor_sf$Arithmetic_Mean >= monthly_metric_2sd
        
        temp <- data.frame(date = pm_dates[j],
                           monitor = monitors[i],
                           daily_mean_pm = monitor_sf$Arithmetic_Mean,
                           plume_within_50km = plume_intersect,
                           pm_exceeds_mean_1sd = pm_exceeds_1sd,
                           pm_exceeds_mean_2sd = pm_exceeds_2sd)
        smoke_days <- bind_rows(smoke_days, temp)
        rm(temp, plumes, plumes_sf)
      }
    }
  }
}

mutate_smoke_days <- smoke_days %>% 
  mutate(smoke_day_1sd = plume_within_50km * pm_exceeds_mean_1sd,
         smoke_day_2sd = plume_within_50km * pm_exceeds_mean_2sd,
         Date_Local = as.Date(date, format = "%Y%m%d")) %>% 
  rename(monitor_id = "monitor")
table(mutate_smoke_days$plume_within_50km, mutate_smoke_days$pm_exceeds_mean_1sd)
table(mutate_smoke_days$plume_within_50km, mutate_smoke_days$pm_exceeds_mean_2sd)

monitor_smoke_days <- left_join(mutate_smoke_days, monitor_sp_data,
                                by = "monitor_id")

write_csv(monitor_smoke_days, here::here("Data", "Monitor_Smoke_Days_AEA.csv"))

#' -----------------------------------------------------------------------------
#' Spot check 1% of days
#' -----------------------------------------------------------------------------

years <- c(2009:2020)

monitor_smoke_days <- read_csv(here::here("Data", "Monitor_Smoke_Days_AEA.csv")) %>%
  st_as_sf(wkt = "WKT", crs = albers)

random_dates <- sample(unique(monitor_smoke_days$Date_Local), 
                       length(unique(monitor_smoke_days$Date_Local)) * 0.01)
sample_days <- filter(monitor_smoke_days, Date_Local %in% random_dates)

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





