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

filter_data <- read_csv(here::here("Data", "Filter_PM.csv")) %>% 
  filter(!is.na(EndDateLocal)) %>% 
  arrange(EndDateLocal) 
monitor_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv"))

hist(filter_data$pm_ug_m3)
hist(filter_data$SampledVolume)

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
         SampledVolume, LoggedRuntime) %>%
  left_join(collocated_ids, by = c("filter_id", "campaign")) %>%
  left_join(mon_ids, by = "Location") %>% 
  filter(is_blank == 0) %>% 
  filter(!is.na(pm_ug_m3))

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
hist(cal_data$SampledVolume)

table(cal_data$low_volume_flag, cal_data$campaign)
table(cal_data$ultralow_volume_flag, cal_data$campaign)

#' Is there a relationships between sampled volume and raw pm concentration?
plot(cal_data$SampledVolume, cal_data$pm_ug_m3)
abline(lm(pm_ug_m3 ~ SampledVolume, data = cal_data))
summary(lm(pm_ug_m3 ~ SampledVolume, data = cal_data))

#' Is there a relationship if we exclude "low volume" (< 3000 L) filters?
#' Answer appears to be no
no_lows <- filter(cal_data, low_volume_flag == 0)
plot(no_lows$SampledVolume, no_lows$pm_ug_m3)
abline(lm(pm_ug_m3 ~ SampledVolume, data = no_lows))
summary(lm(pm_ug_m3 ~ SampledVolume, data = no_lows))

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
ggsave(filename = here::here("Figs/Calibration", "PM_Cal_Curves.jpeg"),
       height = 5, width = 5, dpi = 500, units = "in", device = "jpeg")

#' Linear regression model for all of the data at once
par(mfrow=c(1,1))
plot(cal_data$monitor_mean, cal_data$pm_ug_m3)
abline(reg = lm(pm_ug_m3 ~ monitor_mean,  data = cal_data))
cal_lm <- lm(pm_ug_m3 ~ monitor_mean,  data = cal_data)
summary(cal_lm)
par(mfrow=c(2,2))
plot(cal_lm)
par(mfrow=c(1,1))

#' Linear regression models stratified by location
#' Which one has best preliminary fit?
#' NATIONAL JEWISH: 
njh_data <- filter(cal_data, monitor_id == "080310013")
par(mfrow=c(1,1))
plot(njh_data$monitor_mean, njh_data$pm_ug_m3)
abline(reg = lm(pm_ug_m3 ~ monitor_mean,  data = njh_data))
njh_lm <- lm(pm_ug_m3 ~ monitor_mean,  data = njh_data)
summary(njh_lm)
par(mfrow=c(2,2))
plot(njh_lm)
par(mfrow=c(1,1))

#' I25-Denver: 
i25_data <- filter(cal_data, monitor_id == "080310027")
par(mfrow=c(1,1))
plot(i25_data$monitor_mean, i25_data$pm_ug_m3)
abline(reg = lm(pm_ug_m3 ~ monitor_mean,  data = i25_data))
i25_lm <- lm(pm_ug_m3 ~ monitor_mean,  data = i25_data)
summary(i25_lm)
par(mfrow=c(2,2))
plot(i25_lm)
par(mfrow=c(1,1))

#' GLOBEVILLE: 
globe_data <- filter(cal_data, monitor_id == "080310028")
par(mfrow=c(1,1))
plot(globe_data$monitor_mean, globe_data$pm_ug_m3)
abline(reg = lm(pm_ug_m3 ~ monitor_mean,  data = globe_data))
globe_lm <- lm(pm_ug_m3 ~ monitor_mean,  data = globe_data)
summary(globe_lm)
par(mfrow=c(2,2))
plot(globe_lm)
par(mfrow=c(1,1))

#' Navajo St: 
navajo_data <- filter(cal_data, monitor_id == "080310026")
par(mfrow=c(1,1))
plot(navajo_data$monitor_mean, navajo_data$pm_ug_m3)
abline(reg = lm(pm_ug_m3 ~ monitor_mean,  data = navajo_data))
navajo_lm <- lm(pm_ug_m3 ~ monitor_mean,  data = navajo_data)
summary(navajo_lm)
par(mfrow=c(2,2))
plot(navajo_lm)
par(mfrow=c(1,1))

#' -----------------------------------------------------------------------------
#' Campaign effects?
#' Try using I-25 data since we have the most obs for that monitor
#' Also using Deming regression here
#' -----------------------------------------------------------------------------

library(deming)

#' Campaign 2
i25_c2_data <- filter(cal_data, monitor_id == "080310027" & campaign == "Campaign2")
par(mfrow=c(1,1))
plot(i25_c2_data$monitor_mean, i25_c2_data$pm_ug_m3)
abline(reg = lm(pm_ug_m3 ~ monitor_mean,  data = i25_c2_data))
i25_c2_lm <- lm(pm_ug_m3 ~ monitor_mean,  data = i25_c2_data)
summary(i25_c2_lm)
par(mfrow=c(2,2))
plot(i25_c2_lm)
par(mfrow=c(1,1))

i25_c2_dem <- deming(pm_ug_m3 ~ monitor_mean,  data = i25_c2_data)
print(i25_c2_dem)

#' Campaign 3
i25_c3_data <- filter(cal_data, monitor_id == "080310027" & campaign == "Campaign3")
par(mfrow=c(1,1))
plot(i25_c3_data$monitor_mean, i25_c3_data$pm_ug_m3)
abline(reg = lm(pm_ug_m3 ~ monitor_mean,  data = i25_c3_data))
i25_c3_lm <- lm(pm_ug_m3 ~ monitor_mean,  data = i25_c3_data)
summary(i25_c3_lm)
par(mfrow=c(2,2))
plot(i25_c3_lm)
par(mfrow=c(1,1))

i25_c3_dem <- deming(pm_ug_m3 ~ monitor_mean,  data = i25_c3_data)
print(i25_c3_dem)

#' Campaign 4
i25_c4_data <- filter(cal_data, monitor_id == "080310027" & campaign == "Campaign4")
par(mfrow=c(1,1))
plot(i25_c4_data$monitor_mean, i25_c4_data$pm_ug_m3)
abline(reg = lm(pm_ug_m3 ~ monitor_mean,  data = i25_c4_data))
i25_c4_lm <- lm(pm_ug_m3 ~ monitor_mean,  data = i25_c4_data)
summary(i25_c4_lm)
par(mfrow=c(2,2))
plot(i25_c4_lm)
par(mfrow=c(1,1))

i25_c4_dem <- deming(pm_ug_m3 ~ monitor_mean,  data = i25_c4_data)
print(i25_c4_dem)

#' Campaign 5
i25_c5_data <- filter(cal_data, monitor_id == "080310027" & campaign == "Campaign5")
par(mfrow=c(1,1))
plot(i25_c5_data$monitor_mean, i25_c5_data$pm_ug_m3)
abline(reg = lm(pm_ug_m3 ~ monitor_mean,  data = i25_c5_data))
i25_c5_lm <- lm(pm_ug_m3 ~ monitor_mean,  data = i25_c5_data)
summary(i25_c5_lm)
par(mfrow=c(2,2))
plot(i25_c5_lm)
par(mfrow=c(1,1))

i25_c5_dem <- deming(pm_ug_m3 ~ monitor_mean,  data = i25_c5_data)
print(i25_c5_dem)

#' -----------------------------------------------------------------------------
#' Give Deming regression a try- accounts for errors in "outcome"
#' Based on the plots above-- trying the globeville data set
#' -----------------------------------------------------------------------------

library(deming)

i25_dem <- deming(pm_ug_m3 ~ monitor_mean,  
                  data = filter(cal_data, monitor_id == "080310027"))
print(i25_dem)

#' -----------------------------------------------------------------------------
#' Compare linear regression and deming regression using mean bias and RMSE
#' -----------------------------------------------------------------------------

#' Linear regression model
fit_lm <- i25_lm
summary(fit_lm)

#' Linear coefficients
int_lm <- unname(fit_lm$coefficients[1])
beta_pm_lm <- unname(fit_lm$coefficients[2])

#' Deming  regression model
fit_dem <- i25_dem
print(fit_dem)

#' Deming coefficients
int_dem <- unname(fit_dem$coefficients[1])
beta_pm_dem <- unname(fit_dem$coefficients[2])

fit_monitor <- "080310027"

#' Calibrating using equations
fit_data <- filter(cal_data, monitor_id == "080310027") %>% 
  mutate(pm_ug_m3_lmcal = (pm_ug_m3 - int_lm) / beta_pm_lm,
         pm_ug_m3_demcal = (pm_ug_m3 - int_dem) / beta_pm_dem)

#' MAE and RMSE- Linear model
mae(i25_lm$residuals)
rmse(i25_lm$residuals)

#' MAE and RMSE- Deming model
mae(i25_dem$residuals)
rmse(i25_dem$residuals)

i25_cal_data <- filter(cal_data, monitor_id == "080310027")

plot(i25_cal_data$monitor_mean, i25_cal_data$pm_ug_m3, 
     main = "Regression Comparison", 
     xlab = "EPA Monitor", ylab = "UPAS Filter")
abline(i25_lm, col="red")
abline(i25_dem, col = "blue")
abline(0, 1, col = "black", lty = 2)



#' Calibrating using equation
filter_data2 <- filter_data2 %>% 
  #' Calibrated PM (adjusted linear model)
  mutate(pm_ug_m3_adjlm = (pm_ug_m3 - int_adjlm - (beta_camp3_adjlm * camp3) - 
                             (beta_camp4_adjlm * camp4)) / beta_pm_adjlm) %>% 
  #' Calibrate using crude linear model and deming model
  mutate(pm_ug_m3_lm = (pm_ug_m3 - int_lm) / beta_pm_lm,
         pm_ug_m3_dem = (pm_ug_m3 - int_dem) / beta_pm_dem)

summary(filter_data2$pm_ug_m3)
summary(filter_data2$pm_ug_m3_adjlm)
summary(filter_data2$pm_ug_m3_lm)
summary(filter_data2$pm_ug_m3_dem)
summary(filter_data2$monitor_mean)

#' -----------------------------------------------------------------------------
#' Diagnostic Plots!
#' -----------------------------------------------------------------------------

filter_data_comp <- filter_data2 %>% 
  dplyr::select(filter_id, month, season, pm_ug_m3, pm_ug_m3_lm, 
         pm_ug_m3_dem, monitor_mean) %>% 
  gather(key = type, value = pm, -filter_id, -month, -season)

ggplot(filter_data_comp, aes(x = as.factor(month), y = pm, fill = as.factor(type))) +
  ggtitle("Monthly distribution of measured and calibrated PM2.5") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("pm_ug_m3" = "Raw Data",
                                "pm_ug_m3_lm" = "Crude linear model",
                                "pm_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "EPA Monitor")) +
  xlab("Month") + ylab("PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  scale_y_continuous(limits = c(-10, 50)) +
  theme(legend.position = "bottom") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Monthly_Means_PM.jpeg"),
       height = 4, width = 6, dpi = 500, units = "in", device = "jpeg")

ggplot(filter_data_comp, aes(x = as.factor(season), y = pm, fill = as.factor(type))) +
  ggtitle("Seasonal distribution of measured and calibrated PM2.5") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("pm_ug_m3" = "Raw Data",
                                "pm_ug_m3_lm" = "Crude linear model",
                                "pm_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "EPA Monitor")) +
  scale_x_discrete(labels = c("1" = "Winter", "2" = "Spring",
                              "3" = "Summer", "4" = "Fall")) +
  xlab("Month") + ylab("PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  scale_y_continuous(limits = c(-10, 50)) +
  theme(legend.position = "bottom") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Seasonal_Means_PM.jpeg"),
       height = 4, width = 6, dpi = 500, units = "in", device = "jpeg")

#' Write out the calibrated dataset
glimpse(filter_data2)
write_csv(filter_data2, here::here("Data", "Filter_PM_Calibrated.csv"))

test <- read_csv(here::here("Data", "Filter_PM_Calibrated.csv"))

#' Save the calibration models
#' Save a date-stamped version in case we need to go back
today <- Sys.Date()
save(fit_dem, fit_lm, fit_adjlm, 
     file = here::here("Results", "Filter_PM_CalModels.rdata"))

save(fit_dem, fit_lm, fit_adjlm, 
     file = here::here("Results/Archived_Results", 
                       paste0("Filter_PM_CalModels_", today, ".rdata")))