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
names(all_data)
glimpse(all_data)

#' Select a "calibrated" version of the data
#' For now, go with Deming regression-- accounts for variability in the
#' monitor and the UPAS data and the temporal mismatch in TWAs
all_data$bc_ug_m3 <- all_data$bc_ug_m3_dem
all_data$pm_ug_m3 <- all_data$pm_ug_m3_dem

aqs_data <- filter(all_data, filter_id == "080310027") %>%
  filter(!is.na(bc_ug_m3)) 

lur_data <- all_data %>% filter(filter_id != "080310027") %>%
  filter(st_week == sample_week) %>%
  filter(indoor == 0) %>%
  filter(is_blank == 0) %>% 
  #' QA filters
  filter(bc_below_lod == 0) %>% 
  filter(negative_pm_mass == 0) %>% 
  filter(potential_contamination == 0)
glimpse(lur_data)

#'------------------------------------------------------------------------------
#' How much data do we have?
#'------------------------------------------------------------------------------

nrow(lur_data) #' filters
length(unique(lur_data$campaign)) #' campaigns
length(unique(lur_data$site_id)) #' sites

dates <- lur_data %>% 
  select(StartDateTimeLocal, EndDateTimeLocal) %>% 
  arrange(StartDateTimeLocal)
earliest <- dates$StartDateTimeLocal[1]
latest <- dates$EndDateTimeLocal[nrow(dates)]
date_seq <- seq.Date(earliest, latest, by = "day")

date_df <- lur_data %>% 
  select(campaign, StartDateTimeLocal, EndDateTimeLocal) %>% 
  arrange(StartDateTimeLocal) %>% 
  group_by(campaign) %>% 
  summarize(Start_Date = StartDateTimeLocal[1],
            End_Date = EndDateTimeLocal[length(EndDateTimeLocal)])
date_df

#' When were samples collected?
site_tab2 <- lur_data %>% 
  select(site_id, campaign) %>% 
  group_by(site_id) %>% 
  count(campaign) %>% 
  pivot_wider(id_cols = site_id, names_from = campaign, values_from = n)
site_tab2

#' How many unique sites were included in each campaign?
camp_tab <- select(lur_data, campaign, site_id) %>% 
  group_by(campaign) %>% 
  summarize(unique_sites = length(unique(site_id)))
camp_tab

#'------------------------------------------------------------------------------
#' What do our monitoring data look like?
#'------------------------------------------------------------------------------

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

#' PM2.5 concentrations
pm_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  filter(Sample_Duration != "1 HOUR") %>% 
  arrange(Date_Local, monitor_id)

#' Black carbon
bc_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' Temperature
temp_data <- read_csv(here::here("Data", "Monitor_TEMP_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers)%>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' NO2
no2_data <- read_csv(here::here("Data", "Monitor_NO2_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#' Smoke days
smoke_data <- read_csv(here::here("Data", "Monitor_Smoke_Days_AEA.csv")) %>%
  filter(!is.na(smoke_day_1sd)) %>% 
  filter(!is.na(monitor_id)) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>%
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id)

#'------------------------------------------------------------------------------
#' A) Understanding the sampling framework
#'------------------------------------------------------------------------------

#' When were samples collected?
as.data.frame(date_df)

#' How long did we sample? (in days)
summary(lur_data$logged_runtime/24)
ggplot(lur_data) +
  geom_histogram(aes(x = logged_runtime/24)) +
  xlab("Sample runtime (days)") +
  simple_theme

#' How much volume did we sample? (in L)
summary(lur_data$logged_rt_volume_L)
ggplot(lur_data) +
  geom_histogram(aes(x = logged_rt_volume_L)) +
  xlab("Sample volume (L)") +
  simple_theme

#' How many sites did we include by campaign
as.data.frame(camp_tab)
as.data.frame(site_tab2)

#' How many monitors do we have data from?
length(unique(pm_data$monitor_id)) #' PM2.5 monitors
length(unique(no2_data$monitor_id)) #' NO2 monitors
length(unique(bc_data$monitor_id)) #' BC monitors

#' When were monitors deployed?
ggplot(lur_data) +
  geom_segment(aes(x = StartDateTimeLocal, xend = EndDateTimeLocal, 
                   y = site_id, yend = site_id,
                   col = as.factor(campaign))) +
  scale_color_viridis(discrete = T, name = "Campaign") +
  scale_x_date(date_breaks = "2 months") +
  xlab("Sampling duration") + ylab("Distributed site ID") +
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  simple_theme

#' Where were monitors deployed?
data_name <- "Filter_Locations_AEA.csv"
sites_sf <- read_csv(here::here("Data/Filter_Data", data_name)) %>%
  filter(indoor == 0) %>%
  select(lon, lat, site_id, participant) %>%
  distinct() %>%
  st_as_sf(coords = c('lon', 'lat'), crs = ll_wgs84) %>%
  st_transform(crs = albers) 

highways <- read_csv(here::here("Data", "Highways_AEA.csv")) %>%
  st_as_sf(wkt = "WKT", crs = albers)

ggplot() +
  geom_sf(data = st_jitter(sites_sf), aes(color = "pt"), show.legend = "point") +
  geom_sf(data = highways, aes(color = "high"), show.legend = "line") +
  scale_color_manual(name = "Feature",
                     values = c("high" = "red", "pt" = "black"),
                     labels = c("high" = "Highways", "pt" = "Sites")) +
  simple_theme

#'------------------------------------------------------------------------------
#' B) Temporal trends in the data
#'------------------------------------------------------------------------------

#' Weekly data for each distributed sampling location
week_data <- select(lur_data, lon, lat,
                    StartDateTimeLocal, EndDateTimeLocal, 
                    site_id, filter_id, bc_ug_m3,
                    area_pm, area_no2, area_temp, nn_bc, sample_week, campaign,
                    low_volume_flag, logged_rt_volume_L) 

week_data$central_bc_check <- NA
week_data$central_pm_check <- NA
week_data$central_temp_check <- NA
week_data$central_no2_check <- NA

#' Calculate average BC, PM2.5, NO2, and temp across all Denver metro monitors
#' for just the sampling days (not the representative week)
for(i in 1:nrow(week_data)) {
  df <- slice(week_data, i)
  date_list <- seq.Date(df$StartDateTimeLocal, df$EndDateTimeLocal, by = "day")
  
  bc_temp <- filter(bc_data, Date_Local %in% date_list)
  week_data$central_bc_check[i] <- mean(bc_temp$Arithmetic_Mean)
  
  pm_temp <- filter(pm_data, Date_Local %in% date_list)
  week_data$central_pm_check[i] <- mean(pm_temp$Arithmetic_Mean)
  
  no2_temp <- filter(no2_data, Date_Local %in% date_list)
  week_data$central_no2_check[i] <- mean(no2_temp$Arithmetic_Mean)
  
  temp_temp <- filter(temp_data, Date_Local %in% date_list)
  week_data$central_temp_check[i] <- mean(temp_temp$Arithmetic_Mean)
}

cor(week_data$nn_bc, week_data$central_bc_check, use = "complete.obs")
cor(week_data$area_pm, week_data$central_pm_check, use = "complete.obs")
cor(week_data$area_temp, week_data$central_temp_check, use = "complete.obs")
cor(week_data$area_no2, week_data$central_no2_check, use = "complete.obs")

#' Time-weighted average BC concentrations at each distributed sampling site
ggplot() +
  geom_point(data = week_data, aes(x = sample_week, y = bc_ug_m3, color = campaign),
             alpha = 0.25) +
  geom_smooth(data = week_data, aes(x = sample_week, y = bc_ug_m3)) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  ylab("Distributed site BC for individual filters") + xlab("Sampling Week") +
  simple_theme

ggplot(week_data) +
  geom_boxplot(aes(x = sample_week, y = bc_ug_m3, color = as.factor(campaign))) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  xlab("Campaign week") + ylab("Distributed site BC concentration") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  simple_theme

#' Summary stats for each site
#' What do the summary stats look like for each location?
site_stats <- week_data %>%
  group_by(site_id) %>%
  summarize(n_filters = n(),
            mean = mean(bc_ug_m3),
            sd = sd(bc_ug_m3),
            cv = round(sd(bc_ug_m3) / mean(bc_ug_m3), 2),
            median = median(bc_ug_m3),
            IQR = IQR(bc_ug_m3),
            range = max(bc_ug_m3) - min(bc_ug_m3)) %>%
  mutate(cv_gt_50 = ifelse(abs(cv) > 0.50, 1, 0),
         cv_gt_30 = ifelse(abs(cv) > 0.30, 1, 0))
table(site_stats$cv_gt_50)
table(site_stats$cv_gt_30)

#' What do the summary stats look like for each location by campaign?
site_stats2 <- week_data %>%
  group_by(site_id, campaign) %>%
  summarize(n_filters = n(),
            mean = mean(bc_ug_m3),
            sd = sd(bc_ug_m3),
            cv = round(sd(bc_ug_m3) / mean(bc_ug_m3), 2),
            median = median(bc_ug_m3),
            IQR = IQR(bc_ug_m3),
            range = max(bc_ug_m3) - min(bc_ug_m3)) %>%
  mutate(cv_gt_50 = ifelse(abs(cv) > 0.50, 1, 0),
         cv_gt_30 = ifelse(abs(cv) > 0.30, 1, 0))
table(site_stats2$cv_gt_50, site_stats2$campaign)
table(site_stats2$cv_gt_30, site_stats2$campaign)

cv_drop <- filter(site_stats2, cv_gt_30 == 1)
cv_drop
write_csv(cv_drop, here::here("Data/Dropped_Sites_by_Campaign.csv"))

#' Time-weighted average BC concentrations at each distributed sampling site where 
#' we drop sites where the CV is > 0.3
#' All of these are in Campaign 4
week_data2 <- bind_rows(filter(week_data, campaign != "Campaign4"),
                        filter(week_data, campaign == "Campaign4" & !(site_id %in% cv_drop$site_id)))

ggplot() +
  geom_point(data = week_data2, aes(x = sample_week, y = bc_ug_m3, color = campaign),
             alpha = 0.25) +
  geom_smooth(data = week_data2, aes(x = sample_week, y = bc_ug_m3)) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  ylab("Distributed site BC for individual filters") + xlab("Sampling Week") +
  simple_theme

ggplot(week_data2) +
  geom_boxplot(aes(x = sample_week, y = bc_ug_m3, color = as.factor(campaign))) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  xlab("Campaign week") + ylab("Distributed site BC concentration") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  simple_theme

campaign_sites <- select(week_data2, campaign, site_id, filter_id) %>%
  distinct()
write_csv(campaign_sites, here::here("Data/Final_Campaign_Sites_and_Filters.csv"))

#'------------------------------------------------------------------------------
#'Temporal trends in central site monitor BC, PM, NO2 and temperature
#'------------------------------------------------------------------------------

ggplot(bc_data) +
  geom_point(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  geom_line(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  geom_smooth(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  scale_color_viridis(discrete = T, name = "Monitor ID") +
  xlab("Date") + ylab("Central site BC") +
  simple_theme

ggplot(pm_data) +
  geom_point(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id),
             size = 0.5) +
  geom_line(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  geom_smooth(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  scale_color_viridis(discrete = T, name = "Monitor ID") +
  xlab("Date") + ylab("Central site PM") +
  facet_wrap(. ~ monitor_id) +
  simple_theme

ggplot(no2_data) +
  geom_point(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id),
             size = 0.5) +
  geom_line(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  geom_smooth(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  scale_color_viridis(discrete = T, name = "Monitor ID") +
  xlab("Date") + ylab("Central site NO2") +
  facet_wrap(. ~ monitor_id) +
  simple_theme

ggplot(temp_data) +
  geom_point(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id),
             size = 0.5) +
  geom_line(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  geom_smooth(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  scale_color_viridis(discrete = T, name = "Monitor ID") +
  xlab("Date") + ylab("Central site temperature") +
  facet_wrap(. ~ monitor_id) +
  simple_theme

#'------------------------------------------------------------------------------
#' C) Comparing the central site BC variability (daily) to the distributed site BC 
#' variability (weekly)
#'------------------------------------------------------------------------------

#' With all of the sampling sites
ggplot() +
  geom_point(data = week_data, aes(x = sample_week, y = bc_ug_m3, color = "dist")) +
  geom_smooth(data = week_data, aes(x = sample_week, y = bc_ug_m3, color = "dist")) +
  geom_point(data = bc_data, aes(x = Date_Local, y = Arithmetic_Mean, color = "cent")) +
  geom_smooth(data = bc_data, aes(x = Date_Local, y = Arithmetic_Mean, color = "cent")) +
  scale_color_viridis(discrete = T, name = "Sampling location",
                      labels = c("dist" = "Distributed sites",
                                 "cent" = "Central monitoring site")) +
  ylab("Weekly BC concentrations") + xlab("Sampling Start Date") +
  simple_theme

#' Dropping the sites where the CV > 0.3
ggplot() +
  geom_point(data = week_data2, aes(x = sample_week, y = bc_ug_m3, color = "dist")) +
  geom_smooth(data = week_data2, aes(x = sample_week, y = bc_ug_m3, color = "dist")) +
  geom_point(data = bc_data, aes(x = Date_Local, y = Arithmetic_Mean, color = "cent")) +
  geom_smooth(data = bc_data, aes(x = Date_Local, y = Arithmetic_Mean, color = "cent")) +
  scale_color_viridis(discrete = T, name = "Sampling location",
                      labels = c("dist" = "Distributed sites",
                                 "cent" = "Central monitoring site")) +
  ylab("Weekly BC concentrations") + xlab("Sampling Start Date") +
  simple_theme

#'------------------------------------------------------------------------------
#' Comparing the central site BC variability (averaged to the sampling period of 
#' each distributed site sample) to the distributed site BC variability
#'------------------------------------------------------------------------------

ggplot() +
  geom_point(data = week_data, aes(x = sample_week, y = bc_ug_m3, color = "dist")) +
  geom_smooth(data = week_data, aes(x = sample_week, y = bc_ug_m3, color = "dist")) +
  geom_point(data = week_data, aes(x = sample_week, y = nn_bc, color = "cent")) +
  geom_smooth(data = week_data, aes(x = sample_week, y = nn_bc, color = "cent")) +
  scale_color_viridis(discrete = T, name = "Sampling location",
                      labels = c("dist" = "Distributed sites",
                                 "cent" = "Central monitoring site")) +
  ylab("Weekly BC concentrations ") + xlab("Sampling date") +
  simple_theme

#' Dropping the sites where the CV > 0.3
ggplot() +
  geom_point(data = week_data2, aes(x = sample_week, y = bc_ug_m3, color = "dist")) +
  geom_smooth(data = week_data2, aes(x = sample_week, y = bc_ug_m3, color = "dist")) +
  geom_point(data = week_data2, aes(x = sample_week, y = nn_bc, color = "cent")) +
  geom_smooth(data = week_data2, aes(x = sample_week, y = nn_bc, color = "cent")) +
  scale_color_viridis(discrete = T, name = "Sampling location",
                      labels = c("dist" = "Distributed sites",
                                 "cent" = "Central monitoring site")) +
  ylab("Weekly BC concentrations ") + xlab("Sampling date") +
  simple_theme

#'------------------------------------------------------------------------------
#' D) Spatial patterns of BC based on simple inverse distance weighting
#'------------------------------------------------------------------------------

site_data_sp <- select(week_data2, site_id, sample_week, bc_ug_m3, lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84) %>%
  st_transform(crs = albers) %>%
  group_by(site_id) %>%
  summarize(bc_ug_m3 = mean(bc_ug_m3, na.rm = T)) %>%
  as("Spatial")

highways_sp <- as(highways, "Spatial")

#' Surface (IDW) based on average BC at each distributed sampling location, 
#' averaged across the entire study period

library(latticeExtra)

grid <- spsample(site_data_sp, type = 'regular', n = 10000)
all_idw <- idw(site_data_sp$bc_ug_m3 ~ 1, site_data_sp, newdata = grid)
spplot(all_idw["var1.pred"]) +
  layer(sp.lines(highways_sp, col = "black", cex = 0.1))
