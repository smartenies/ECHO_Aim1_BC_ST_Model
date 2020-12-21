#' =============================================================================
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Calibrate UPAS BC transmissometry data
#' Date created: November 7, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script calibrates the BC sootscan data using montitoring data from
#' the I-25 site
#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggthemes)
library(ggspatial)
library(ggpubr)
library(viridis)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

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

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

#' -----------------------------------------------------------------------------
#' Read in the filter BC data and monitor data
#' Read in list of collocated monitors
#' -----------------------------------------------------------------------------

filter_data <- read_csv(here::here("Data", "Filter_BC.csv")) %>% 
  filter(!is.na(EndDateLocal)) %>% 
  arrange(EndDateLocal) 
monitor_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv"))

hist(filter_data$bc_ug_m3)
hist(filter_data$SampledVolume)

#' Collocated monitors:
#' I-25 Denver (080310027) 9th and Yuma: PM2.5, Black Carbon

collocated_ids <- read_csv(here::here("Data", "Collocation_Filter_IDs.csv"))

unique(collocated_ids$Location)
mon_ids <- data.frame(Location = unique(collocated_ids$Location),
                      monitor_id = c("080310028", "080310027",
                                     "080310026", "080310013"))

cal_data <- filter(filter_data, filter_id %in% collocated_ids$filter_id) %>% 
  dplyr::select(filter_id, StartDateTimeLocal, EndDateTimeLocal, is_blank, 
                indoor, bc_ug_m3, campaign, bc_below_lod, low_volume_flag, 
                ultralow_volume_flag, flow_rate_flag,
                SampledVolume, LoggedRuntime) %>%
  left_join(collocated_ids, by = c("filter_id", "campaign")) %>%
  left_join(mon_ids, by = "Location") %>% 
  filter(monitor_id == "080310027") %>%
  filter(is_blank == 0) %>% 
  filter(!is.na(bc_ug_m3))

#'How do the collocated filters compare to all filters
summary(filter_data$bc_ug_m3)
summary(cal_data$bc_ug_m3)

summary(cal_data)
table(cal_data$bc_below_lod)
table(cal_data$low_volume_flag)

#' First, drop anything below the limit of detection
cal_data <- filter(cal_data, bc_below_lod == 0)
hist(cal_data$bc_ug_m3)

#' Next, let's check out sampled volumes
hist(cal_data$bc_ug_m3)
hist(cal_data$SampledVolume)

table(cal_data$low_volume_flag, cal_data$campaign)
table(cal_data$ultralow_volume_flag, cal_data$campaign)

#' Is there a relationships between sampled volume and raw bc concentration?
plot(cal_data$SampledVolume, cal_data$bc_ug_m3)
abline(lm(bc_ug_m3 ~ SampledVolume, data = cal_data))
summary(lm(bc_ug_m3 ~ SampledVolume, data = cal_data))

#' Is there a relationship if we exclude "low volume" (< 3000 L) filters?
#' Answer appears to be no
no_lows <- filter(cal_data, low_volume_flag == 0)
plot(no_lows$SampledVolume, no_lows$bc_ug_m3)
abline(lm(bc_ug_m3 ~ SampledVolume, data = no_lows))
summary(lm(bc_ug_m3 ~ SampledVolume, data = no_lows))

#' What about just the low volume filters?
#' Answer appears to be yes
all_lows <- filter(cal_data, low_volume_flag == 1)
plot(all_lows$SampledVolume, all_lows$bc_ug_m3)
abline(lm(bc_ug_m3 ~ SampledVolume, data = all_lows))
summary(lm(bc_ug_m3 ~ SampledVolume, data = all_lows))

ggplot(cal_data, aes(x = StartDateTimeLocal, y = bc_ug_m3)) +
  geom_point(aes(color = campaign, shape = as.factor(low_volume_flag))) +
  geom_line(aes(color = campaign)) +
  geom_smooth(method = "lm") +
  facet_grid(monitor_id ~ .) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "month") +
  xlab("Date") + ylab("Time-weighted average UPAS bc\u2082.\u2085 (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_UPAS_TS.jpeg"),
       height = 6, width = 8, dpi = 500, units = "in", device = "jpeg")


#' -----------------------------------------------------------------------------
#' Plot time series for the I-25 monitoring site
#' Only site in Denver with a long-term record of BC
#' This site uses an AE-33 aethalometer
#' -----------------------------------------------------------------------------

start <- filter_data$StartDateLocal[1]
end <- filter_data$EndDateLocal[length(filter_data$EndDateLocal)]

date_seq <- seq.Date(start, end, by = "day")

co_mon <- filter(monitor_data, monitor_id %in% mon_ids$monitor_id) %>% 
  filter(Date_Local %in% date_seq)

ggplot(co_mon, aes(x = Date_Local, y = Arithmetic_Mean)) +
  geom_line(aes(color = monitor_id), show.legend = F) +
  geom_smooth(method = "lm") +
  facet_grid(monitor_id ~ .) +
  scale_color_viridis(name = "Monitoring Site", discrete = T) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "month") +
  xlab("Date") + ylab("Daily mean monitor BC (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_Monitor_TS.jpeg"),
       height = 8, width = 6, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Assign monitor BC to filters using start and end dates
#' Initial model fit
#' -----------------------------------------------------------------------------

