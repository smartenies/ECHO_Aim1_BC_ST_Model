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
  text  = element_text(family="Arial",size = 14, color = 'black'),
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
windowsFonts(Arial=windowsFont("TT Arial"))

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
c2_color <- "#2C718EFF"
c3_color <- "#61C960FF"
cent_color <- "#FDE725FF"

#' -----------------------------------------------------------------------------
#' Read in the original data set
#' -----------------------------------------------------------------------------


data_name <- "Combined_Filter_Data_AEA.csv"
lur_data <- read_csv(here::here("Data", data_name)) %>%
  filter(!is.na(lon)) %>%
  filter(indoor == 0) %>%
  #' Just campaigns 1-3 
  filter(campaign %in% paste0("Campaign", c(1, 2, 3)))

#' How many filters were collected?
nrow(lur_data)

#' QA filters
lur_data2 <- lur_data %>%
  filter(bc_ug_m3_dem > 0) %>% 
  filter(is.na(below_lod) | below_lod == 0) %>% 
  filter(is.na(low_volume_flag) | low_volume_flag == 0) %>% 
  filter(is.na(flow_rate_flag) | flow_rate_flag == 0) %>% 
  filter(is.na(is_blank) | is_blank == 0) %>% 
  filter(is.na(negative_pm_mass) | negative_pm_mass == 0) %>% 
  filter(is.na(potential_contamination) | potential_contamination == 0) %>% 
  #' Using Ben's LOD, but not imputing values
  filter(is.na(bc_mass_ug_corrected) | bc_mass_ug_corrected >= 1.41) %>%
  rename("bc_ug_m3_raw" = "bc_ug_m3") %>%
  rename("bc_ug_m3" = "bc_ug_m3_dem")
  
#' How many usable filters?
nrow(lur_data2)

#' What percentage were usable?
(nrow(lur_data2) / nrow(lur_data)) * 100

#' On what days were filters deployed?
lur_data2$deploy_day <- wday(lur_data2$StartDateLocal, label = T) 
table(lur_data2$deploy_day)
prop.table(table(lur_data2$deploy_day))

#' Sampling duration?
summary(lur_data2$logged_runtime/24)

#' Number of unique sites?
lur_data2$site_id_lon_lat <- paste(lur_data2$lon, lur_data2$lat, sep = "_")
length(unique(lur_data2$site_id_lon_lat))

#' Number of samples per site?
site_count <- select(lur_data2, site_id_lon_lat) %>% 
  group_by(site_id_lon_lat) %>% 
  summarize(n = n())
site_count
summary(site_count$n)

#' Volume of air sampled?
summary(lur_data2$SampledVolume)

#' Campaign dates
date_df <- lur_data2 %>% 
  select(campaign, StartDateLocal, EndDateLocal) %>% 
  arrange(StartDateLocal) %>% 
  group_by(campaign) %>% 
  summarize(Start_Date = StartDateLocal[1],
            End_Date = EndDateLocal[length(EndDateLocal)])
date_df

#' Filters per campaign
filter_camp <- select(lur_data2, campaign) %>%
  group_by(campaign) %>%
  summarize(n = n())
filter_camp

#' Sites by campaign?
site_tab <- lur_data2 %>% 
  select(site_id_lon_lat, campaign) %>% 
  group_by(site_id_lon_lat) %>% 
  count(campaign) %>% 
  pivot_wider(id_cols = site_id_lon_lat, names_from = campaign, values_from = n) %>% 
  mutate(total = sum(Campaign1, Campaign2, Campaign3, na.rm = T))
site_tab

#' How many unique sites were included in each campaign?
camp_tab <- select(lur_data2, campaign, site_id_lon_lat) %>% 
  group_by(campaign) %>% 
  summarize(unique_sites = length(unique(site_id_lon_lat)))
camp_tab

#' Summary of BC concentrations
data_name <- "Combined_Filter_Data_AEA.csv"
lur_data3 <- read_csv(here::here("Data", data_name)) %>%
  filter(!is.na(lon)) %>%
  filter(indoor == 0) %>%
  #' Just campaigns 1-3 
  filter(campaign %in% paste0("Campaign", c(1, 2, 3, "X"))) %>% 
  filter(bc_ug_m3_dem > 0) %>% 
  filter(is.na(below_lod) | below_lod == 0) %>% 
  filter(is.na(low_volume_flag) | low_volume_flag == 0) %>% 
  filter(is.na(flow_rate_flag) | flow_rate_flag == 0) %>% 
  filter(is.na(is_blank) | is_blank == 0) %>% 
  filter(is.na(negative_pm_mass) | negative_pm_mass == 0) %>% 
  filter(is.na(potential_contamination) | potential_contamination == 0) %>% 
  #' Using Ben's LOD, but not imputing values
  filter(is.na(bc_mass_ug_corrected) | bc_mass_ug_corrected >= 1.41) %>%
  rename("bc_ug_m3_raw" = "bc_ug_m3") %>%
  rename("bc_ug_m3" = "bc_ug_m3_dem")

nrow(lur_data3)

camp_data <- filter(lur_data3, campaign %in% paste0("Campaign", c(1, 2, 3)))
nrow(camp_data)
summary(camp_data$bc_ug_m3)
quantile(camp_data$bc_ug_m3, probs = c(0.05, 0.95))
sd(camp_data$bc_ug_m3)

site_data <- filter(lur_data3, campaign == "CampaignX")
nrow(site_data)
summary(site_data$bc_ug_m3)
quantile(site_data$bc_ug_m3, probs = c(0.05, 0.95))
sd(site_data$bc_ug_m3)
















#' How much data do we have?
nrow_lur <- nrow(lur_data)
n_campaigns <- length(unique(lur_data$campaign))
n_sites <- length(unique(lur_data$site_id))

dates <- lur_data %>% 
  select(StartDateLocal, EndDateLocal) %>% 
  arrange(StartDateLocal)
earliest <- dates$StartDateLocal[1]
latest <- dates$EndDateLocal[nrow(dates)]
date_seq <- seq.Date(earliest, latest, by = "day")

date_df <- lur_data %>% 
  select(campaign, StartDateLocal, EndDateLocal) %>% 
  arrange(StartDateLocal) %>% 
  group_by(campaign) %>% 
  summarize(Start_Date = StartDateLocal[1],
            End_Date = EndDateLocal[length(EndDateLocal)])

#' When were samples collected?
site_tab2 <- lur_data %>% 
  select(site_id, campaign) %>% 
  group_by(site_id) %>% 
  count(campaign) %>% 
  pivot_wider(id_cols = site_id, names_from = campaign, values_from = n) %>% 
  mutate(total = sum(Campaign1, Campaign2, Campaign3, Campaign4, na.rm = T))

#' How many unique sites were included in each campaign?
camp_tab <- select(lur_data, campaign, site_id) %>% 
  group_by(campaign) %>% 
  summarize(unique_sites = length(unique(site_id)))

