#' =============================================================================
#' Project: ECHO LUR
#' Date Created: July 23, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' Summary statistics for the manuscript tables
#' =============================================================================

library(sf)
library(ggplot2)
library(ggmap)
library(ggspatial)
library(ggsn)
library(ggpubr)
library(ggthemes)
library(extrafont)
library(viridis)
library(tidyverse)
library(lubridate)
library(readxl)
library(cowplot)

#' For ggplots
# loadfonts(device = "win")

simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 16, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "grey90"),
  panel.grid.major = element_line(color = "grey90"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=14),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)

simple_theme2 <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  # panel.grid.minor = element_line(color = "grey90"),
  panel.grid.minor.x = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "grey90"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(2,2,2,2), "mm"),
  legend.key = element_blank()
)

map_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 14, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border = element_blank(),
  panel.background=element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)

map_theme2 <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
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

register_google(google_api_key)

#' Choose colors for the campaigns
library(colormap)
# Defaults to 72 colors from the 'viridis' palette.
scales::show_col(colormap(), labels = T)

c1_color <- "#440154FF"
c2_color <- "#3F518BFF"
c3_color <- "#21918DFF"
c4_color <- "#61C960FF"
c5_color <- "#D6E22BFF"
cent_color <- "#FDE725FF"

#' =============================================================================
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Exploring the BC measurements to use in the ST model
#' Date created: December 31, 2020
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Note: The purpose of this script is to explore the data set and determine 
#' how we want to proceed with model development. There are additional data 
#' included in this version of the analysis because were able to conduct an 
#' additional sampling campaign in the winter/spring of 2020. 
#' =============================================================================

library(sf)
library(gstat)
library(sp)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggthemes)
library(extrafont)
library(GGally)
library(ggcorrplot)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)
library(viridis)
library(caret)
library(knitr)
library(kableExtra)

simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "grey90"),
  panel.grid.major = element_line(color = "grey90"),
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

#'------------------------------------------------------------------------------
#' Read in the data set and subset to just the outdoor filters
#' Don't worry about the "central site" filters right now
#' Define which variable we want to use-- BC calibrated using Deming regression
#'------------------------------------------------------------------------------

data_name <- "Combined_Filter_Data_AEA.csv"
all_data <- read_csv(here::here("Data", data_name))

#' Select a "calibrated" version of the data
#' For now, go with Deming regression-- accounts for variability in the
#' monitor and the UPAS data and the temporal mismatch in TWAs
all_data$bc_ug_m3 <- all_data$bc_ug_m3_dem
all_data$pm_ug_m3 <- all_data$pm_ug_m3_dem

#' List of sites that failed preliminary screening (see 15_Exploring_BC_Data.R)
#' These are all the sites (by campaign) where BC measurements had a CV > 30%
#' Note that all of these are in Campaign 4
cv_drop <- read_csv(here::here("Data/Dropped_Sites_by_Campaign.csv"))

all_filter_data <- filter(all_data, is.na(filter_id) | filter_id != "080310027") %>%
  filter(is.na(indoor) | indoor == 0) %>%
  filter(is.na(is_blank) | is_blank == 0) %>%
  filter(st_week == sample_week)

filter_data <- filter(all_data, is.na(filter_id) | filter_id != "080310027") %>%
  filter(is.na(indoor) | indoor == 0) %>%
  filter(is.na(is_blank) | is_blank == 0) %>%
  #' QA filters
  filter(is.na(bc_below_lod) | bc_below_lod == 0) %>% 
  filter(is.na(pm_below_lod) | pm_below_lod == 0) %>%
  filter(is.na(negative_pm_mass) | negative_pm_mass == 0) %>% 
  filter(is.na(ultralow_volume_flag) | ultralow_volume_flag == 0) %>%
  filter(is.na(potential_contamination) | potential_contamination == 0)

dist_data <- bind_rows(filter(filter_data, is.na(campaign) | campaign != "Campaign4"),
                       filter(filter_data, campaign == "Campaign4" & !(site_id %in% cv_drop$site_id))) %>%
  arrange(st_week)
head(dist_data$sample_week)
tail(dist_data$sample_week)
head(dist_data$st_week)
tail(dist_data$st_week)

length(unique(dist_data$filter_id))
summary(dist_data$bc_ug_m3)
quantile(dist_data$bc_ug_m3, probs = c(0.05, 0.95), na.rm = T)
sd(dist_data$bc_ug_m3, na.rm = T)
sd(dist_data$bc_ug_m3, na.rm = T) / mean(dist_data$bc_ug_m3, na.rm = T)

dist_data2 <- filter(dist_data, st_week == sample_week) %>%
  filter(!is.na(bc_ug_m3))
glimpse(dist_data2)

write_csv(dist_data2, here::here("Data", "Final_Filters_for_ST_Model.csv"))

#' All of the dropped data should be in Campaign 4
dropped_data <- filter(filter_data, campaign == "Campaign4" & site_id %in% cv_drop$site_id)
length(unique(dropped_data$filter_id))
summary(dropped_data$bc_ug_m3)
quantile(dropped_data$bc_ug_m3, probs = c(0.05, 0.95))
sd(dropped_data$bc_ug_m3)
sd(dropped_data$bc_ug_m3) / mean(dropped_data$bc_ug_m3)

write_csv(dropped_data, here::here("Data", "Dropped_Filters_for_ST_Model.csv"))

#' central site data
central_data <- filter(all_data, filter_id == "080310027") %>%
  arrange(st_week)
head(central_data$sample_week)
tail(central_data$sample_week)
head(central_data$st_week)
tail(central_data$st_week)

#' Add prefix to site_ids
unique(dist_data$site_id)
dist_data$site_id <- paste0("d_", dist_data$site_id)
unique(dist_data$site_id)

unique(central_data$site_id)
central_data$site_id <- "central"
unique(central_data$site_id)

#'------------------------------------------------------------------------------
#' When were samples collected?
#'------------------------------------------------------------------------------

dist_data2 <- filter(dist_data, st_week == sample_week) %>%
  filter(!is.na(bc_ug_m3))
nrow(dist_data2)

site_tab2 <- dist_data2 %>% 
  dplyr::select(site_id, campaign) %>% 
  group_by(site_id) %>% 
  count(campaign) %>% 
  pivot_wider(id_cols = site_id, names_from = campaign, values_from = n)
site_tab2

#' How many unique sites were included in each campaign?
camp_tab <- dplyr::select(dist_data2, campaign, site_id) %>% 
  dplyr::group_by(campaign) %>% 
  summarize(unique_sites = length(unique(site_id)))
camp_tab

#' How many filters were collected in each campaign?
camp_tab2 <- dplyr::select(dist_data2, campaign, filter_id) %>% 
  dplyr::group_by(campaign) %>% 
  summarize(unique_sites = length(unique(filter_id))) %>%
  arrange(unique_sites)
camp_tab2

#'------------------------------------------------------------------------------
#' How much data do we have?
#'------------------------------------------------------------------------------

nrow(dist_data2) #' filters
length(unique(dist_data2$campaign)) #' campaigns
length(unique(dist_data2$site_id)) #' sites

dates <- dist_data2 %>% 
  dplyr::select(StartDateTimeLocal, EndDateTimeLocal) %>% 
  arrange(StartDateTimeLocal)
earliest <- dates$StartDateTimeLocal[1]
latest <- dates$EndDateTimeLocal[nrow(dates)]
date_seq <- seq.Date(earliest, latest, by = "day")

date_df <- dist_data2 %>% 
  dplyr::select(campaign, StartDateTimeLocal, EndDateTimeLocal) %>% 
  arrange(StartDateTimeLocal) %>% 
  group_by(campaign) %>% 
  summarize(Start_Date = StartDateTimeLocal[1],
            End_Date = EndDateTimeLocal[length(EndDateTimeLocal)])
date_df

#'------------------------------------------------------------------------------
#' Sample stats?
#'------------------------------------------------------------------------------

summary(dist_data2$logged_runtime/24)
summary(dist_data2$logged_rt_volume_L)

nrow(dist_data2)
nrow(central_data)

summary(dist_data2$bc_ug_m3)
sd(dist_data2$bc_ug_m3)
quantile(dist_data2$bc_ug_m3, c(0.05, 0.25, 0.50, 0.75, 0.95))

summary(central_data$bc_ug_m3)
sd(central_data$bc_ug_m3)
quantile(central_data$bc_ug_m3, c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm = T)

#'------------------------------------------------------------------------------
#' Correlations
#'------------------------------------------------------------------------------

summary(dist_data2$pm_ug_m3)
table(dist_data$potential_contamination)

plot(dist_data2$bc_ug_m3, dist_data2$pm_ug_m3)
cor(dist_data2$bc_ug_m3, dist_data2$pm_ug_m3, method = "spearman")

dist_data3 <- filter(dist_data2, pm_ug_m3 <50)

plot(dist_data3$bc_ug_m3, dist_data3$pm_ug_m3)
cor(dist_data3$bc_ug_m3, dist_data3$pm_ug_m3, method = "spearman")
cor(dist_data3$bc_ug_m3, dist_data3$pm_ug_m3)
