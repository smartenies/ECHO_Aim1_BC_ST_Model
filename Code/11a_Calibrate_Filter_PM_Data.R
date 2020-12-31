#' =============================================================================
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Calibrate UPAS PM2.5 gravimetric data
#' Date created: July 23, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script calibrates the PM2.5 gravimetric data using monitoring data from
#' the I-25 site site (080310027)
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
library(deming)

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
#' Read in the filter PM2.5 data and monitor data
#' Read in list of collocated monitors
#' -----------------------------------------------------------------------------

monitor_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv"))

filter_data <- read_csv(here::here("Data", "Filter_PM.csv")) %>% 
  filter(!is.na(EndDateLocal)) %>% 
  filter(indoor == 0) %>%
  #' QA filters
  filter(is.na(pm_below_lod) | pm_below_lod == 0) %>% 
  filter(is.na(is_blank) | is_blank == 0) %>% 
  filter(potential_contamination == 0) %>%
  arrange(EndDateLocal)  

#' Choose which calculated concentration to use
#' Going with the logged volume one
hist(filter_data$pm_ug_m3_logged_vol)
hist(filter_data$pm_ug_m3_sampled_vol)

summary(filter_data$pm_ug_m3_logged_vol)
summary(filter_data$pm_ug_m3_sampled_vol)

hist(filter_data$logged_rt_volume_L)
hist(filter_data$SampledVolume)

filter_data <- mutate(filter_data, pm_ug_m3 = pm_ug_m3_logged_vol)

#' Collocated monitors:
#' National Jewish (080310013) 1400 Jackson St, Denver, CO 80206: PM2.5
#' I-25 Denver (080310027) 9th and Yuma: PM2.5, Black Carbon
#' I-25 Globeville (080310028) 49th and Acoma: PM2.5, 
#' Navajo St. is a (080310026)  CSN site and has PM2.5

collocated_ids <- read_csv(here::here("Data", "Collocation_Filter_IDs.csv"))

unique(collocated_ids$Location)
mon_ids <- data.frame(Location = unique(collocated_ids$Location),
                      monitor_id = c("080310028", "080310027",
                                     "080310026", "080310013"))

cal_data <- filter(filter_data, filter_id %in% collocated_ids$filter_id) %>% 
  dplyr::select(filter_id, StartDateTimeLocal, EndDateTimeLocal, is_blank, 
         indoor, pm_ug_m3, campaign, pm_below_lod, low_volume_flag, 
         ultralow_volume_flag, flow_rate_flag,
         SampledVolume, logged_rt_volume_L,  LoggedRuntime) %>%
  left_join(collocated_ids, by = c("filter_id", "campaign")) %>%
  left_join(mon_ids, by = "Location") %>% 
  filter(is_blank == 0)

#'How do the collocated filters compare to all filters
summary(filter_data$pm_ug_m3)
summary(cal_data$pm_ug_m3)

summary(cal_data)
table(cal_data$pm_below_lod)
table(cal_data$low_volume_flag)

#' First, drop anything below the limit of detection
cal_data <- filter(cal_data, pm_below_lod == 0)
hist(cal_data$pm_ug_m3)

#' Next, let's check out sampled volumes
hist(cal_data$pm_ug_m3)
hist(cal_data$logged_rt_volume_L)

table(cal_data$low_volume_flag, cal_data$campaign)
table(cal_data$ultralow_volume_flag, cal_data$campaign)

#' Is there a relationships between sampled volume and raw pm concentration?
plot(cal_data$logged_rt_volume_L, cal_data$pm_ug_m3)
abline(lm(pm_ug_m3 ~ logged_rt_volume_L, data = cal_data))
summary(lm(pm_ug_m3 ~ logged_rt_volume_L, data = cal_data))

#' Is there a relationship if we exclude "low volume" (< 4000 L) filters?
#' Answer appears to be no
no_lows <- filter(cal_data, low_volume_flag == 0)
plot(no_lows$logged_rt_volume_L, no_lows$pm_ug_m3)
abline(lm(pm_ug_m3 ~ logged_rt_volume_L, data = no_lows))
summary(lm(pm_ug_m3 ~ logged_rt_volume_L, data = no_lows))

#' What about just the low volume filters?
#' Answer appears to be yes
all_lows <- filter(cal_data, low_volume_flag == 1)
plot(all_lows$SampledVolume, all_lows$pm_ug_m3)
abline(lm(pm_ug_m3 ~ SampledVolume, data = all_lows))
summary(lm(pm_ug_m3 ~ SampledVolume, data = all_lows))

ggplot(cal_data, aes(x = StartDateTimeLocal, y = pm_ug_m3)) +
  geom_point(aes(color = campaign, shape = as.factor(low_volume_flag))) +
  geom_line(aes(color = campaign)) +
  geom_smooth(method = "lm") +
  facet_grid(monitor_id ~ .) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "month") +
  xlab("Date") + ylab("Time-weighted average UPAS PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "PM_UPAS_TS.jpeg"),
       height = 6, width = 8, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Plot time series for each monitor
#' Want to get a sense of the trends for the monitoring data
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
  xlab("Date") + ylab("Daily mean monitor PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "PM_Monitor_TS.jpeg"),
       height = 8, width = 6, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Assign monitor PM2.5 to filters using start and end dates
#' Initial model fit
#' -----------------------------------------------------------------------------

#' Assign start and end PM2.5 concentrations from the monitoring data
#' This loop gets the average of daily PM2.5 measurements for the period
#' between the start and end dates for the UPAS sampler

for (i in 1:nrow(cal_data)) {
  df <- slice(cal_data, i)
  
  if(is.na(df$StartDateTimeLocal[1])) next
  
  #' Which monitor is collocated
  paired_monitor_id <- df$monitor_id[1]
  
  #' What are the start and end dates for this filter
  start_date <- df$StartDateTimeLocal[1]
  end_date <- df$EndDateTimeLocal[1]
  
  dates <- seq.Date(from = start_date, to = end_date, by = "day")
  
  #' Get mean concentration from the monitor for the period covered by the filter
  mon <- monitor_data %>% 
    filter(monitor_id == paired_monitor_id) %>% 
    filter(Date_Local %in% dates)
  
  mean_pol <- mean(mon$Arithmetic_Mean, na.rm=T)
  
  cal_data[i, "monitor_mean"] <- mean_pol
  cal_data[i, "monitor_mean_n_obs"] <- nrow(mon)
}

glimpse(cal_data)

ggplot(cal_data, aes(x = monitor_mean, y = pm_ug_m3)) +
  geom_point(aes(color = as.factor(monitor_id)), show.legend = F) +
  geom_smooth(method = "lm", show.legend = F) +
  scale_color_viridis(name = "Monitoring site", discrete = T) +
  facet_grid(monitor_id ~ .) +
  ylab("UPAS PM\u2082.\u2085 (\u03bcg/m\u00b3)") + 
  xlab("Monitor PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "PM_UPAS_vs_Mon_All_Camps.jpeg"),
       height = 5, width = 7, dpi = 500, units = "in", device = "jpeg")

#' Plot BC monitor vs UPAS by campaign
ggplot() +
  geom_smooth(data = filter(cal_data, campaign == "Campaign2" & monitor_id == "080310027"),
              aes(x = monitor_mean, y = pm_ug_m3, color = campaign),
              method = "lm", show.legend = F, se = T) +
  geom_point(data = filter(cal_data, campaign == "Campaign2" & monitor_id == "080310027"),
             aes(x = monitor_mean, y = pm_ug_m3, color = campaign)) +
  geom_smooth(data = filter(cal_data, campaign == "Campaign3" & monitor_id == "080310027"),
              aes(x = monitor_mean, y = pm_ug_m3, color = campaign),
              method = "lm", show.legend = F, se = T) +
  geom_point(data = filter(cal_data, campaign == "Campaign3" & monitor_id == "080310027"),
             aes(x = monitor_mean, y = pm_ug_m3, color = campaign)) +
  geom_smooth(data = filter(cal_data, campaign == "Campaign4" & monitor_id == "080310027"),
              aes(x = monitor_mean, y = pm_ug_m3, color = campaign),
              method = "lm", show.legend = F, se = T) +
  geom_point(data = filter(cal_data, campaign == "Campaign4" & monitor_id == "080310027"),
             aes(x = monitor_mean, y = pm_ug_m3, color = campaign)) +
  geom_smooth(data = filter(cal_data, campaign == "Campaign5" & monitor_id == "080310027"),
              aes(x = monitor_mean, y = pm_ug_m3, color = campaign),
              method = "lm", show.legend = F, se = T) +
  geom_point(data = filter(cal_data, campaign == "Campaign5" & monitor_id == "080310027"),
             aes(x = monitor_mean, y = pm_ug_m3, color = campaign)) +
  scale_color_viridis(discrete = T, name = NULL) +
  ylab("UPAS PM\u2082.\u2085 (\u03pmg/m\u00b3)") + xlab("Monitor UPAS PM\u2082.\u2085 (\u03pmg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "PM_UPAS_vs_Mon_by_Campaign_I25.jpeg"),
       height = 5, width = 7, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Linear regression model for all campaigns combined
#' Deming regression model for all campaigns combined
#' Note: using data from the I-25 monitor to match the BC measurements
#' -----------------------------------------------------------------------------

cal_data2 <- filter(cal_data, monitor_id == "080310027")

cor(cal_data2$monitor_mean, cal_data2$pm_ug_m3)
plot(cal_data2$monitor_mean, cal_data2$pm_ug_m3)
abline(lm(pm_ug_m3 ~ monitor_mean, data = cal_data2))

#' Linear Model: R2 = 0.10
i25_lm <- lm(pm_ug_m3 ~ monitor_mean, data = cal_data2)
summary(i25_lm)
par(mfrow=c(2,2))
plot(i25_lm)
par(mfrow=c(1,1))

lm_int <- unname(i25_lm$coefficients[1])
lm_slope <- unname(i25_lm$coefficients[2])

#' Deming Model
i25_dem <- deming(pm_ug_m3 ~ monitor_mean, data = cal_data2)
print(i25_dem)

dem_int <- unname(i25_dem$coefficients[1])
dem_slope <- unname(i25_dem$coefficients[2])

#' -----------------------------------------------------------------------------
#' Compare linear regression and deming regression using mean bias and RMSE
#' -----------------------------------------------------------------------------

#' MAE and RMSE- Linear model
mae(i25_lm$residuals)
rmse(i25_lm$residuals)

#' MAE and RMSE- Deming model
mae(i25_dem$residuals)
rmse(i25_dem$residuals)

plot(cal_data$monitor_mean, cal_data$bc_ug_m3, 
     main = "Regression Comparison", 
     xlab = "EPA Monitor", ylab = "UPAS Filter")
abline(i25_lm, col="red")
abline(i25_dem, col = "blue")
legend(x = "topright", col = c("red", "blue"), lty = c(1, 1), 
       legend = c("Linear", "Deming"))

#' -----------------------------------------------------------------------------
#' The scatter plots of EPA vs UPAS suggest some serious differences by season!
#' Explore linear regression and Deming regression for each campaign separately
#' -----------------------------------------------------------------------------

#' -------------------------------------
#' Campaign 2
#' Linear and Deming regression
#' -------------------------------------

cal_data_c2 <- filter(cal_data2, campaign == "Campaign2")
cor(cal_data_c2$monitor_mean, cal_data_c2$pm_ug_m3)
plot(cal_data_c2$monitor_mean, cal_data_c2$pm_ug_m3)
abline(lm(pm_ug_m3 ~ monitor_mean, data = cal_data_c2))

i25_lm_c2 <- lm(pm_ug_m3 ~ monitor_mean, data = cal_data_c2)
summary(i25_lm_c2)
par(mfrow=c(2,2))
plot(i25_lm_c2)
par(mfrow=c(1,1))

lm_int_c2 <- unname(i25_lm_c2$coefficients[1])
lm_slope_c2 <- unname(i25_lm_c2$coefficients[2])

i25_dem_c2 <- deming(pm_ug_m3 ~ monitor_mean, data = cal_data_c2)
print(i25_dem_c2)

dem_int_c2 <- unname(i25_dem_c2$coefficients[1])
dem_slope_c2 <- unname(i25_dem_c2$coefficients[2])

plot(cal_data_c2$monitor_mean, cal_data_c2$pm_ug_m3, 
     main = "Regression Comparison", 
     xlab = "EPA Monitor", ylab = "UPAS Filter")
abline(i25_lm_c2, col="red")
abline(i25_dem_c2, col = "blue")
legend(x = "topright", col = c("red", "blue"), lty = c(1, 1), legend = c("Linear", "Deming"))

#' MAE and RMSE- Linear model
mae(i25_lm_c2$residuals)
rmse(i25_lm_c2$residuals)

#' MAE and RMSE- Deming model
mae(i25_dem_c2$residuals)
rmse(i25_dem_c2$residuals)

#' -------------------------------------
#' Campaign 3
#' Linear and Deming regression
#' -------------------------------------

cal_data_c3 <- filter(cal_data2, campaign == "Campaign3")
cor(cal_data_c3$monitor_mean, cal_data_c3$pm_ug_m3)
plot(cal_data_c3$monitor_mean, cal_data_c3$pm_ug_m3)
abline(lm(pm_ug_m3 ~ monitor_mean, data = cal_data_c3))

i25_lm_c3 <- lm(pm_ug_m3 ~ monitor_mean, data = cal_data_c3)
summary(i25_lm_c3)
par(mfrow=c(2,2))
plot(i25_lm_c3)
par(mfrow=c(1,1))

lm_int_c3 <- unname(i25_lm_c3$coefficients[1])
lm_slope_c3 <- unname(i25_lm_c3$coefficients[2])

i25_dem_c3 <- deming(pm_ug_m3 ~ monitor_mean, data = cal_data_c3)
print(i25_dem_c3)

dem_int_c3 <- unname(i25_dem_c3$coefficients[1])
dem_slope_c3 <- unname(i25_dem_c3$coefficients[2])

plot(cal_data_c3$monitor_mean, cal_data_c3$pm_ug_m3, 
     main = "Regression Comparison", 
     xlab = "EPA Monitor", ylab = "UPAS Filter")
abline(i25_lm_c3, col="red")
abline(i25_dem_c3, col = "blue")
legend(x = "topright", col = c("red", "blue"), lty = c(1, 1), legend = c("Linear", "Deming"))

#' MAE and RMSE- Linear model
mae(i25_lm_c3$residuals)
rmse(i25_lm_c3$residuals)

#' MAE and RMSE- Deming model
mae(i25_dem_c3$residuals)
rmse(i25_dem_c3$residuals)

#' -------------------------------------
#' Campaign 4
#' Linear and Deming regression
#' -------------------------------------

cal_data_c4 <- filter(cal_data2, campaign == "Campaign4")
cor(cal_data_c4$monitor_mean, cal_data_c4$pm_ug_m3)
plot(cal_data_c4$monitor_mean, cal_data_c4$pm_ug_m3)
abline(lm(pm_ug_m3 ~ monitor_mean, data = cal_data_c4))

i25_lm_c4 <- lm(pm_ug_m3 ~ monitor_mean, data = cal_data_c4)
summary(i25_lm_c4)
par(mfrow=c(2,2))
plot(i25_lm_c4)
par(mfrow=c(1,1))

lm_int_c4 <- unname(i25_lm_c4$coefficients[1])
lm_slope_c4 <- unname(i25_lm_c4$coefficients[2])

i25_dem_c4 <- deming(pm_ug_m3 ~ monitor_mean, data = cal_data_c4)
print(i25_dem_c4)

dem_int_c4 <- unname(i25_dem_c4$coefficients[1])
dem_slope_c4 <- unname(i25_dem_c4$coefficients[2])

plot(cal_data_c4$monitor_mean, cal_data_c4$pm_ug_m3, 
     main = "Regression Comparison", 
     xlab = "EPA Monitor", ylab = "UPAS Filter")
abline(i25_lm_c4, col="red")
abline(i25_dem_c4, col = "blue")
legend(x = "topright", col = c("red", "blue"), lty = c(1, 1), legend = c("Linear", "Deming"))

#' MAE and RMSE- Linear model
mae(i25_lm_c4$residuals)
rmse(i25_lm_c4$residuals)

#' MAE and RMSE- Deming model
mae(i25_dem_c4$residuals)
rmse(i25_dem_c4$residuals)

#' -------------------------------------
#' Campaign 5
#' Linear and Deming regression
#' -------------------------------------

cal_data_c5 <- filter(cal_data2, campaign == "Campaign5")
cor(cal_data_c5$monitor_mean, cal_data_c5$pm_ug_m3)
plot(cal_data_c5$monitor_mean, cal_data_c5$pm_ug_m3)
abline(lm(pm_ug_m3 ~ monitor_mean, data = cal_data_c5))

i25_lm_c5 <- lm(pm_ug_m3 ~ monitor_mean, data = cal_data_c5)
summary(i25_lm_c5)
par(mfrow=c(2,2))
plot(i25_lm_c5)
par(mfrow=c(1,1))

lm_int_c5 <- unname(i25_lm_c5$coefficients[1])
lm_slope_c5 <- unname(i25_lm_c5$coefficients[2])

i25_dem_c5 <- deming(pm_ug_m3 ~ monitor_mean, data = cal_data_c5)
print(i25_dem_c5)

dem_int_c5 <- unname(i25_dem_c5$coefficients[1])
dem_slope_c5 <- unname(i25_dem_c5$coefficients[2])

plot(cal_data_c5$monitor_mean, cal_data_c5$pm_ug_m3, 
     main = "Regression Comparison", 
     xlab = "EPA Monitor", ylab = "UPAS Filter")
abline(i25_lm_c5, col="red")
abline(i25_dem_c5, col = "blue")
legend(x = "topright", col = c("red", "blue"), lty = c(1, 1), legend = c("Linear", "Deming"))

#' MAE and RMSE- Linear model
mae(i25_lm_c5$residuals)
rmse(i25_lm_c5$residuals)

#' MAE and RMSE- Deming model
mae(i25_dem_c5$residuals)
rmse(i25_dem_c5$residuals)

#' -----------------------------------------------------------------------------
#' Going to calibrate each campaign separately, with Campaigns 1 & 2 together
#' Also going to have a variable where we calibrate based on the "overall" curve
#' Going to have a calibrated value using Deming regression and linear regression
#' -----------------------------------------------------------------------------

#' -------------------------------------
#' Campaigns 1 & 2
#' -------------------------------------

filter_data_c2 <- filter(filter_data, campaign %in% c("Campaign1", "Campaign2"))

summary(cal_data_c2$pm_ug_m3)
summary(filter_data_c2$pm_ug_m3)
hist(filter_data_c2$pm_ug_m3)

#' Calibrating the filter data
filter_data_c2 <- filter_data_c2 %>%
  #' rename the concentration to specify that it's raw
  rename(pm_ug_m3_raw = pm_ug_m3) %>%
  #' Calibrated PM (using the "all campaigns" models)
  mutate(pm_ug_m3_lm_all = (pm_ug_m3_raw - lm_int) / lm_slope) %>%
  mutate(pm_ug_m3_dem_all = (pm_ug_m3_raw - dem_int) / dem_slope) %>%
  #' Calibrated PM (Linear model)
  mutate(pm_ug_m3_lm = (pm_ug_m3_raw - lm_int_c2) / lm_slope_c2) %>% 
  #' Calibrated PM (Deming model) 
  mutate(pm_ug_m3_dem = (pm_ug_m3_raw - dem_int_c2) / dem_slope_c2) 

summary(filter_data_c2$pm_ug_m3_raw)
summary(filter_data_c2$pm_ug_m3_lm)
summary(filter_data_c2$pm_ug_m3_lm_all)
summary(filter_data_c2$pm_ug_m3_dem)
summary(filter_data_c2$pm_ug_m3_dem_all)

ggplot(filter_data_c2) +
  geom_density(aes(x = pm_ug_m3_raw, color = "Raw")) +
  geom_density(aes(x = pm_ug_m3_lm, color = "Linear model")) +
  geom_density(aes(x = pm_ug_m3_lm_all, color = "Linear model (all campaigns)")) +
  geom_density(aes(x = pm_ug_m3_dem, color = "Deming model")) +
  geom_density(aes(x = pm_ug_m3_dem_all, color = "Deming model (all campaigns")) +
  xlab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name = "Data type", discrete = T) +
  simple_theme

#' -------------------------------------
#' Campaign 3
#' -------------------------------------

filter_data_c3 <- filter(filter_data, campaign == "Campaign3")

summary(cal_data_c3$pm_ug_m3)
summary(filter_data_c3$pm_ug_m3)
hist(filter_data_c3$pm_ug_m3)

#' Calibrating the filter data
filter_data_c3 <- filter_data_c3 %>%
  #' rename the concentration to specify that it's raw
  rename(pm_ug_m3_raw = pm_ug_m3) %>%
  #' Calibrated PM (using the "all campaigns" model)
  mutate(pm_ug_m3_lm_all = (pm_ug_m3_raw - lm_int) / lm_slope) %>%
  mutate(pm_ug_m3_dem_all = (pm_ug_m3_raw - dem_int) / dem_slope) %>%
  #' Calibrated PM (Linear model)
  mutate(pm_ug_m3_lm = (pm_ug_m3_raw - lm_int_c3) / lm_slope_c3) %>% 
  #' Calibrated PM (Deming model) 
  mutate(pm_ug_m3_dem = (pm_ug_m3_raw - dem_int_c3) / dem_slope_c3) 

summary(filter_data_c3$pm_ug_m3_raw)
summary(filter_data_c3$pm_ug_m3_lm)
summary(filter_data_c3$pm_ug_m3_lm_all)
summary(filter_data_c3$pm_ug_m3_dem)
summary(filter_data_c3$pm_ug_m3_dem_all)

ggplot(filter_data_c3) +
  geom_density(aes(x = pm_ug_m3_raw, color = "Raw")) +
  geom_density(aes(x = pm_ug_m3_lm, color = "Linear model")) +
  geom_density(aes(x = pm_ug_m3_lm_all, color = "Linear model (all campaigns)")) +
  geom_density(aes(x = pm_ug_m3_dem, color = "Deming model")) +
  geom_density(aes(x = pm_ug_m3_dem_all, color = "Deming model (all campaigns")) +
  xlab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name = "Data type", discrete = T) +
  simple_theme

#' -------------------------------------
#' Campaign 4
#' -------------------------------------

filter_data_c4 <- filter(filter_data, campaign == "Campaign4")

summary(cal_data_c4$pm_ug_m3)
summary(filter_data_c4$pm_ug_m3)
hist(filter_data_c4$pm_ug_m3)

#' Calibrating the filter data
filter_data_c4 <- filter_data_c4 %>%
  #' rename the concentration to specify that it's raw
  rename(pm_ug_m3_raw = pm_ug_m3) %>%
  #' Calibrated PM (using the "all campaigns" model)
  mutate(pm_ug_m3_lm_all = (pm_ug_m3_raw - lm_int) / lm_slope) %>%
  mutate(pm_ug_m3_dem_all = (pm_ug_m3_raw - dem_int) / dem_slope) %>%
  #' Calibrated PM (Linear model)
  mutate(pm_ug_m3_lm = (pm_ug_m3_raw - lm_int_c4) / lm_slope_c4) %>% 
  #' Calibrated PM (Deming model) 
  mutate(pm_ug_m3_dem = (pm_ug_m3_raw - dem_int_c4) / dem_slope_c4) 

summary(filter_data_c4$pm_ug_m3_raw)
summary(filter_data_c4$pm_ug_m3_lm)
summary(filter_data_c4$pm_ug_m3_lm_all)
summary(filter_data_c4$pm_ug_m3_dem)
summary(filter_data_c4$pm_ug_m3_dem_all)

ggplot(filter_data_c4) +
  geom_density(aes(x = pm_ug_m3_raw, color = "Raw")) +
  geom_density(aes(x = pm_ug_m3_lm, color = "Linear model")) +
  geom_density(aes(x = pm_ug_m3_lm_all, color = "Linear model (all campaigns)")) +
  geom_density(aes(x = pm_ug_m3_dem, color = "Deming model")) +
  geom_density(aes(x = pm_ug_m3_dem_all, color = "Deming model (all campaigns")) +
  xlab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name = "Data type", discrete = T) +
  simple_theme

#' -------------------------------------
#' Campaign 5
#' -------------------------------------

filter_data_c5 <- filter(filter_data, campaign == "Campaign5")

summary(cal_data_c5$pm_ug_m3)
summary(filter_data_c5$pm_ug_m3)
hist(filter_data_c5$pm_ug_m3)

#' Calibrating the filter data
filter_data_c5 <- filter_data_c5 %>%
  #' rename the concentration to specify that it's raw
  rename(pm_ug_m3_raw = pm_ug_m3) %>%
  #' Calibrated PM (using the "all campaigns" model)
  mutate(pm_ug_m3_lm_all = (pm_ug_m3_raw - lm_int) / lm_slope) %>%
  mutate(pm_ug_m3_dem_all = (pm_ug_m3_raw - dem_int) / dem_slope) %>%
  #' Calibrated PM (Linear model)
  mutate(pm_ug_m3_lm = (pm_ug_m3_raw - lm_int_c5) / lm_slope_c5) %>% 
  #' Calibrated PM (Deming model) 
  mutate(pm_ug_m3_dem = (pm_ug_m3_raw - dem_int_c5) / dem_slope_c5) 

summary(filter_data_c5$pm_ug_m3_raw)
summary(filter_data_c5$pm_ug_m3_lm)
summary(filter_data_c5$pm_ug_m3_lm_all)
summary(filter_data_c5$pm_ug_m3_dem)
summary(filter_data_c5$pm_ug_m3_dem_all)

ggplot(filter_data_c5) +
  geom_density(aes(x = pm_ug_m3_raw, color = "Raw")) +
  geom_density(aes(x = pm_ug_m3_lm, color = "Linear model")) +
  geom_density(aes(x = pm_ug_m3_lm_all, color = "Linear model (all campaigns)")) +
  geom_density(aes(x = pm_ug_m3_dem, color = "Deming model")) +
  geom_density(aes(x = pm_ug_m3_dem_all, color = "Deming model (all campaigns")) +
  xlab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name = "Data type", discrete = T) +
  simple_theme

#' -----------------------------------------------------------------------------
#' Combine the data sets
#' -----------------------------------------------------------------------------

filter_data3 <- bind_rows(filter_data_c2, filter_data_c3,
                          filter_data_c4, filter_data_c5)
unique(filter_data3$campaign)
names(filter_data3)

filter_data_comp <- filter_data3 %>% 
  dplyr::select(filter_id, campaign, pm_ug_m3_raw, pm_ug_m3_lm, pm_ug_m3_lm_all, 
                pm_ug_m3_dem) %>% 
  pivot_longer(names_to = "type", values_to = "pm", -c(filter_id, campaign))

ggplot(filter_data_comp, aes(x = as.factor(campaign), y = pm, fill = as.factor(type))) +
  ggtitle("Distribution of measured and calibrated PM2.5 by campaign") +
  geom_boxplot(alpha = 0.75) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("pm_ug_m3" = "Raw Data",
                                "pm_ug_m3_lm" = "Linear model (by campaign)",
                                "pm_ug_m3_lm_all" = "Linear model (all campaigns)",
                                "pm_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "EPA Monitors")) +
  xlab("Campaign") + ylab("PM2.5 (\u03pmg/m\u00b3)") +
  scale_y_continuous(limits = c(-5, 10)) +
  theme(legend.position = "bottom") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Calibrated_PM_by_Campaign.jpeg"),
       height = 4, width = 6, dpi = 500, units = "in", device = "jpeg")

#' Write out the calibrated data set
glimpse(filter_data3)
write_csv(filter_data3, here::here("Data", "Filter_PM_Calibrated.csv"))

#' Save the calibration models
#' Also save a date-stamped version in case we need to go back
today <- Sys.Date()
save(i25_lm, i25_lm_c2, i25_lm_c3, i25_lm_c4, i25_lm_c5,
     i25_dem, i25_dem_c2, i25_dem_c3, i25_dem_c4, i25_dem_c5,
     cal_data, cal_data_c2, cal_data_c3, cal_data_c4, cal_data_c5,
     file = here::here("Results", "Filter_PM_CalModels.rdata"))

save(i25_lm, i25_lm_c2, i25_lm_c3, i25_lm_c4, i25_lm_c5,
     i25_dem, i25_dem_c2, i25_dem_c3, i25_dem_c4, i25_dem_c5,
     cal_data, cal_data_c2, cal_data_c3, cal_data_c4, cal_data_c5,
     file = here::here("Results/Archived_Results", 
                       paste0("Filter_PM_CalModels_", today, ".rdata")))

load(here::here("Results", "Filter_PM_CalModels.rdata"))

cor(i25_dem_c2$model$pm_ug_m3, i25_dem_c2$model$monitor_mean)
cor(i25_dem_c3$model$pm_ug_m3, i25_dem_c3$model$monitor_mean)
cor(i25_dem_c4$model$pm_ug_m3, i25_dem_c4$model$monitor_mean)
cor(i25_dem_c5$model$pm_ug_m3, i25_dem_c5$model$monitor_mean)

#' -----------------------------------------------------------------------------
#' Plots for papers
#' -----------------------------------------------------------------------------

load(here::here("Results", "Filter_PM_CalModels.rdata"))

#' Plot BC monitor vs UPAS by campaign
ggplot() +
  geom_point(data = cal_data_c2,
             aes(x = monitor_mean, y = pm_ug_m3, color = campaign), size = 2) +
  geom_abline(data = cal_data_c2,
              aes(slope = lm_slope_c2, intercept = lm_int_c2,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = cal_data_c2,
              aes(slope = dem_slope_c2, intercept = dem_int_c2,
                  color = campaign, linetype = "dem"), size = 1) +
  
  geom_point(data = cal_data_c3,
             aes(x = monitor_mean, y = pm_ug_m3, color = campaign), size = 2) +
  geom_abline(data = cal_data_c3,
              aes(slope = lm_slope_c3, intercept = lm_int_c3,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = cal_data_c3,
              aes(slope = dem_slope_c3, intercept = dem_int_c3,
                  color = campaign, linetype = "dem"), size = 1) +
  
  geom_point(data = cal_data_c4,
             aes(x = monitor_mean, y = pm_ug_m3, color = campaign), size = 2) +
  geom_abline(data = cal_data_c4,
              aes(slope = lm_slope_c4, intercept = lm_int_c4,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = cal_data_c4,
              aes(slope = dem_slope_c4, intercept = dem_int_c4,
                  color = campaign, linetype = "dem"), size = 1) +
  
  geom_point(data = cal_data_c5,
             aes(x = monitor_mean, y = pm_ug_m3, color = campaign), size = 2) +
  geom_abline(data = cal_data_c5,
              aes(slope = lm_slope_c5, intercept = lm_int_c5,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = cal_data_c5,
              aes(slope = dem_slope_c5, intercept = dem_int_c5,
                  color = campaign, linetype = "dem"), size = 1) +
  
  scale_color_viridis(name = "Campaign", discrete = T,
                      labels = c("Campaign2" = "Campaign 2",
                                 "Campaign3" = "Campaign 3",
                                 "Campaign4" = "Campaign 4",
                                 "Campaign5" = "Campaign 5")) +
  scale_linetype_manual(name = "Regression", 
                        values = c("lm" = 1, "dem" = 2),
                        labels = c("lm" = "OLS", "dem" = "Deming")) +
  facet_grid(campaign ~ ., scales = "free") +
  # facet_grid(. ~ campaign, scales = "free") +
  ylab("UPAS PM2.5 (\u03pmg/m\u00b3)") + xlab("Monitor PM2.5 (\u03pmg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "PM_LM_Dem_by_Campaign.jpeg"),
       height = 7, width = 7, dpi = 500, units = "in", device = "jpeg")

#' Bland-Altman plots 
library(BlandAltmanLeh)
library(ggExtra)

scaleFUN <- function(x) sprintf("%.1f", x)

mean_diff_all <- mean(cal_data$pm_ug_m3 - cal_data$monitor_mean)
ba_plot_all <- bland.altman.plot(cal_data$pm_ug_m3, cal_data$monitor_mean, 
                                 graph.sys="ggplot2")
ba_all <- print(ba_plot_all) +
  geom_hline(aes(yintercept = mean_diff_all), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 1) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels=  scaleFUN) + scale_x_continuous(labels = scaleFUN) +
  simple_theme +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"))
ba_all

mean_diff_c2 <- mean(cal_data_c2$pm_ug_m3 - cal_data_c2$monitor_mean)
ba_plot_c2 <- bland.altman.plot(cal_data_c2$pm_ug_m3, cal_data_c2$monitor_mean, 
                                graph.sys="ggplot2")
ba_c2 <- print(ba_plot_c2) +
  geom_hline(aes(yintercept = mean_diff_c2), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels=  scaleFUN) + scale_x_continuous(labels = scaleFUN) +
  simple_theme +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"))
ba_c2

mean_diff_c3 <- mean(cal_data_c3$pm_ug_m3 - cal_data_c3$monitor_mean)
ba_plot_c3 <- bland.altman.plot(cal_data_c3$pm_ug_m3, cal_data_c3$monitor_mean, 
                                graph.sys="ggplot2")
ba_c3 <- print(ba_plot_c3) +
  geom_hline(aes(yintercept = mean_diff_c3), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels=  scaleFUN) + scale_x_continuous(labels = scaleFUN) +
  simple_theme +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"))
ba_c3

mean_diff_c4 <- mean(cal_data_c4$pm_ug_m3 - cal_data_c4$monitor_mean)
ba_plot_c4 <- bland.altman.plot(cal_data_c4$pm_ug_m3, cal_data_c4$monitor_mean, 
                                graph.sys="ggplot2")
ba_c4 <- print(ba_plot_c4) +
  geom_hline(aes(yintercept = mean_diff_c4), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels=  scaleFUN) + scale_x_continuous(labels = scaleFUN) +
  simple_theme +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"))
ba_c4

mean_diff_c5 <- mean(cal_data_c5$pm_ug_m3 - cal_data_c5$monitor_mean)
ba_plot_c5 <- bland.altman.plot(cal_data_c5$pm_ug_m3, cal_data_c5$monitor_mean, 
                                graph.sys="ggplot2")
ba_c5 <- print(ba_plot_c5) +
  geom_hline(aes(yintercept = mean_diff_c5), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels=  scaleFUN) + scale_x_continuous(labels = scaleFUN) +
  simple_theme +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"))
ba_c5

library(ggpubr)

all_ba_plots <- ggarrange(plotlist = list(ba_all),
                          labels = c("A: All Collocated Filters"),
                          ncol = 1, nrow = 1, vjust = 1, hjust = -0.15)
all_ba_plots

camp_ba_plots <- ggarrange(plotlist = list(ba_c2, ba_c3, ba_c4, ba_c5),
                           labels = c("B: Campaign 2", "C: Campaign 3",
                                      "D: Campaign 4", "E: Campaign 4"),
                           ncol = 2, nrow = 2, vjust = 1, hjust = -0.15)
camp_ba_plots

ba_plots <- annotate_figure(
  ggarrange(all_ba_plots, camp_ba_plots,
            ncol = 1, nrow = 2),
  left = text_grob("Difference: UPAS - AE-33 (\u03bcg/m\u00b3)",
                   rot = 90, face = "bold"),
  bottom = text_grob("Mean of UPAS and AE-33 BC Measurement (\u03bcg/m\u00b3)",
                     face = "bold")
)
ba_plots

ggsave(ba_plots,
       filename = here::here("Figs/Calibration", "BA_Plots_Combined_PM.jpeg"),
       height = 9, width = 7, dpi = 500, units = "in", device = "jpeg")

