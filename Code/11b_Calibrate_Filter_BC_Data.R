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
#' 
#' Note: each campaign is calibrated separately, with Campaigns 1 & 2 grouped 
#' together because we did not collocate in Campaign 1
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
#' Read in the filter BC data and monitor data
#' Read in list of collocated monitors
#' -----------------------------------------------------------------------------

monitor_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv"))

filter_data <- read_csv(here::here("Data", "Filter_BC.csv")) %>% 
  filter(!is.na(EndDateLocal)) %>% 
  filter(indoor == 0) %>%
  #' QA filters
  filter(is.na(is_blank) | is_blank == 0) %>% 
  filter(bc_ug_m3_logged_vol < 10) %>%
  arrange(EndDateLocal)  

#' Choose which calculated concentration to use
#' Going with the logged volume one
hist(filter_data$bc_ug_m3_logged_vol)
hist(filter_data$bc_ug_m3_sampled_vol)

summary(filter_data$bc_ug_m3_logged_vol)
summary(filter_data$bc_ug_m3_sampled_vol)

hist(filter_data$logged_rt_volume_L)
hist(filter_data$SampledVolume)

filter_data <- mutate(filter_data, bc_ug_m3 = bc_ug_m3_logged_vol)

#' Comparison of all BC concentrations
summary(filter_data$bc_ug_m3)

#' Collocated monitors:
#' I-25 Denver (080310027) 9th and Yuma: PM2.5, Black Carbon

collocated_ids <- read_csv(here::here("Data", "Collocation_Filter_IDs.csv"))

unique(collocated_ids$Location)
mon_ids <- data.frame(Location = unique(collocated_ids$Location),
                      monitor_id = c("080310028", "080310027",
                                     "080310026", "080310013"))
mon_ids

cal_data <- filter(filter_data, filter_id %in% collocated_ids$filter_id) %>% 
  dplyr::select(filter_id, StartDateTimeLocal, EndDateTimeLocal, is_blank, 
                indoor, bc_mass_ug_corrected, bc_ug_m3, campaign, 
                bc_below_lod, low_volume_flag, 
                ultralow_volume_flag, flow_rate_flag,
                SampledVolume, logged_rt_volume_L,  LoggedRuntime) %>%
  left_join(collocated_ids, by = c("filter_id", "campaign")) %>%
  left_join(mon_ids, by = "Location") %>% 
  filter(monitor_id == "080310027") %>%
  filter(!is.na(bc_ug_m3))

#' Are any of these data below the LOD?
table(cal_data$bc_below_lod, cal_data$campaign)

cal_data <- filter(cal_data, bc_below_lod == 0)
table(cal_data$bc_below_lod, cal_data$campaign)

#'How do the collocated filters compare to all filters
summary(filter_data$bc_ug_m3)
summary(cal_data$bc_ug_m3)
summary(cal_data)

#' How many filters have volumes less than 4000 L?
sum(cal_data$low_volume_flag)

#' How many filters have volumes less than 1000 L?
sum(cal_data$ultralow_volume_flag)

#' Next, let's check out sampled volumes
hist(cal_data$logged_rt_volume_L)

table(cal_data$low_volume_flag, cal_data$campaign)
table(cal_data$ultralow_volume_flag, cal_data$campaign)

#' Is there a relationships between sampled volume and raw bc concentration?
plot(cal_data$logged_rt_volume_L, cal_data$bc_ug_m3)
abline(lm(bc_ug_m3 ~ logged_rt_volume_L, data = cal_data))
summary(lm(bc_ug_m3 ~ logged_rt_volume_L, data = cal_data))

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

co_mon <- filter(monitor_data, monitor_id == "080310027") %>% 
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

glimpse(cal_data)
unique(cal_data$monitor_id)

#' Assign start and end BC concentrations from the monitoring data
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

#' Plot BC monitor vs UPAS 
ggplot(cal_data, aes(x = monitor_mean, y = bc_ug_m3)) +
  geom_point(aes(color = as.factor(campaign)), show.legend = T) +
  geom_smooth(method = "lm", show.legend = F) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  ylab("UPAS BC (\u03bcg/m\u00b3)") + 
  xlab("Monitor BC (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_UPAS_vs_Mon_All_Camps.jpeg"),
       height = 5, width = 7, dpi = 500, units = "in", device = "jpeg")

#' Plot BC monitor vs UPAS by campaign
ggplot() +
  geom_smooth(data = filter(cal_data, campaign == "Campaign2"),
              aes(x = monitor_mean, y = bc_ug_m3, color = campaign),
              method = "lm", show.legend = F, se = T) +
  geom_point(data = filter(cal_data, campaign == "Campaign2"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign)) +
  geom_smooth(data = filter(cal_data, campaign == "Campaign3"),
              aes(x = monitor_mean, y = bc_ug_m3, color = campaign),
              method = "lm", show.legend = F, se = T) +
  geom_point(data = filter(cal_data, campaign == "Campaign3"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign)) +
  geom_smooth(data = filter(cal_data, campaign == "Campaign4"),
              aes(x = monitor_mean, y = bc_ug_m3, color = campaign),
              method = "lm", show.legend = F, se = T) +
  geom_point(data = filter(cal_data, campaign == "Campaign4"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign)) +
  geom_smooth(data = filter(cal_data, campaign == "Campaign5"),
              aes(x = monitor_mean, y = bc_ug_m3, color = campaign),
              method = "lm", show.legend = F, se = T) +
  geom_point(data = filter(cal_data, campaign == "Campaign5"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign)) +
  scale_color_viridis(discrete = T, name = NULL) +
  ylab("UPAS BC (\u03bcg/m\u00b3)") + xlab("Monitor BC (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_UPAS_vs_Mon_by_Campaign.jpeg"),
       height = 5, width = 7, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Linear regression model for all campaigns combined
#' Deming regression model for all campaigns combined
#' -----------------------------------------------------------------------------

cor(cal_data$monitor_mean, cal_data$bc_ug_m3)
plot(cal_data$monitor_mean, cal_data$bc_ug_m3)
abline(lm(bc_ug_m3 ~ monitor_mean,  
          data = filter(cal_data, monitor_id == "080310027")))

#' Linear Model: R2 = 0.10
i25_lm <- lm(bc_ug_m3 ~ monitor_mean,  
             data = filter(cal_data, monitor_id == "080310027"))
summary(i25_lm)
par(mfrow=c(2,2))
plot(i25_lm)
par(mfrow=c(1,1))

lm_int <- unname(i25_lm$coefficients[1])
lm_slope <- unname(i25_lm$coefficients[2])

#' Deming Model
i25_dem <- deming(bc_ug_m3 ~ monitor_mean,  
                  data = filter(cal_data, monitor_id == "080310027"))
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

cal_data_c2 <- filter(cal_data, campaign == "Campaign2")
cor(cal_data_c2$monitor_mean, cal_data_c2$bc_ug_m3)
plot(cal_data_c2$monitor_mean, cal_data_c2$bc_ug_m3)
abline(lm(bc_ug_m3 ~ monitor_mean, data = cal_data_c2))

i25_lm_c2 <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_c2)
summary(i25_lm_c2)
par(mfrow=c(2,2))
plot(i25_lm_c2)
par(mfrow=c(1,1))

lm_int_c2 <- unname(i25_lm_c2$coefficients[1])
lm_slope_c2 <- unname(i25_lm_c2$coefficients[2])

i25_dem_c2 <- deming(bc_ug_m3 ~ monitor_mean, data = cal_data_c2)
print(i25_dem_c2)

dem_int_c2 <- unname(i25_dem_c2$coefficients[1])
dem_slope_c2 <- unname(i25_dem_c2$coefficients[2])

plot(cal_data_c2$monitor_mean, cal_data_c2$bc_ug_m3, 
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

cal_data_c3 <- filter(cal_data, campaign == "Campaign3")
cor(cal_data_c3$monitor_mean, cal_data_c3$bc_ug_m3)
plot(cal_data_c3$monitor_mean, cal_data_c3$bc_ug_m3)
abline(lm(bc_ug_m3 ~ monitor_mean, data = cal_data_c3))

i25_lm_c3 <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_c3)
summary(i25_lm_c3)
par(mfrow=c(2,2))
plot(i25_lm_c3)
par(mfrow=c(1,1))

lm_int_c3 <- unname(i25_lm_c3$coefficients[1])
lm_slope_c3 <- unname(i25_lm_c3$coefficients[2])

i25_dem_c3 <- deming(bc_ug_m3 ~ monitor_mean, data = cal_data_c3)
print(i25_dem_c3)

dem_int_c3 <- unname(i25_dem_c3$coefficients[1])
dem_slope_c3 <- unname(i25_dem_c3$coefficients[2])

plot(cal_data_c3$monitor_mean, cal_data_c3$bc_ug_m3, 
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
#' Volume might be an issue here (winter)
#' -------------------------------------

cal_data_c4 <- filter(cal_data, campaign == "Campaign4")
cor(cal_data_c4$monitor_mean, cal_data_c4$bc_ug_m3)
plot(cal_data_c4$monitor_mean, cal_data_c4$bc_ug_m3)
abline(lm(bc_ug_m3 ~ monitor_mean, data = cal_data_c4))

table(cal_data_c4$low_volume_flag)
table(cal_data_c4$ultralow_volume_flag)

cal_data_c4_v2 <- filter(cal_data, campaign == "Campaign4" & low_volume_flag == 0)
cor(cal_data_c4_v2$monitor_mean, cal_data_c4_v2$bc_ug_m3)
plot(cal_data_c4_v2$monitor_mean, cal_data_c4_v2$bc_ug_m3)
abline(lm(bc_ug_m3 ~ monitor_mean, data = cal_data_c4_v2))

i25_lm_c4 <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_c4)
summary(i25_lm_c4)
par(mfrow=c(2,2))
plot(i25_lm_c4)
par(mfrow=c(1,1))

i25_lm_c4_v2 <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_c4_v2)
summary(i25_lm_c4_v2)
par(mfrow=c(2,2))
plot(i25_lm_c4)
par(mfrow=c(1,1))

lm_int_c4 <- unname(i25_lm_c4$coefficients[1])
lm_slope_c4 <- unname(i25_lm_c4$coefficients[2])

i25_dem_c4 <- deming(bc_ug_m3 ~ monitor_mean, data = cal_data_c4)
print(i25_dem_c4)

i25_dem_c4_v2 <- deming(bc_ug_m3 ~ monitor_mean, data = cal_data_c4_v2)
print(i25_dem_c4_v2)

dem_int_c4 <- unname(i25_dem_c4$coefficients[1])
dem_slope_c4 <- unname(i25_dem_c4$coefficients[2])

plot(cal_data_c4$monitor_mean, cal_data_c4$bc_ug_m3, 
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

cal_data_c5 <- filter(cal_data, campaign == "Campaign5")
cor(cal_data_c5$monitor_mean, cal_data_c5$bc_ug_m3)
plot(cal_data_c5$monitor_mean, cal_data_c5$bc_ug_m3)
abline(lm(bc_ug_m3 ~ monitor_mean, data = cal_data_c5))

table(cal_data_c5$low_volume_flag)
table(cal_data_c5$ultralow_volume_flag)

cal_data_c5_v2 <- filter(cal_data, campaign == "Campaign5" & low_volume_flag == 0)
cor(cal_data_c5_v2$monitor_mean, cal_data_c5_v2$bc_ug_m3)
plot(cal_data_c5_v2$monitor_mean, cal_data_c5_v2$bc_ug_m3)
abline(lm(bc_ug_m3 ~ monitor_mean, data = cal_data_c5_v2))

i25_lm_c5 <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_c5)
summary(i25_lm_c5)
par(mfrow=c(2,2))
plot(i25_lm_c5)
par(mfrow=c(1,1))

lm_int_c5 <- unname(i25_lm_c5$coefficients[1])
lm_slope_c5 <- unname(i25_lm_c5$coefficients[2])

i25_dem_c5 <- deming(bc_ug_m3 ~ monitor_mean, data = cal_data_c5)
print(i25_dem_c5)

dem_int_c5 <- unname(i25_dem_c5$coefficients[1])
dem_slope_c5 <- unname(i25_dem_c5$coefficients[2])

plot(cal_data_c5$monitor_mean, cal_data_c5$bc_ug_m3, 
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
#' Going to have a calibrated value using Deming regression and linear regression
#' Using a reduced set of filters for Campaign 4 (volumes > 4000 L)
#' -----------------------------------------------------------------------------

#' -------------------------------------
#' Campaigns 1 & 2
#' -------------------------------------

filter_data_c2 <- filter(filter_data, campaign %in% c("Campaign1", "Campaign2"))

summary(cal_data_c2$bc_ug_m3)
summary(filter_data_c2$bc_ug_m3)
hist(filter_data_c2$bc_ug_m3)

#' Calibrating the filter data
filter_data_c2 <- filter_data_c2 %>%
  #' rename the concentration to specify that it's raw
  rename(bc_ug_m3_raw = bc_ug_m3) %>%
  #' Calibrated BC (Linear model)
  mutate(bc_ug_m3_lm = (bc_ug_m3_raw - lm_int_c2) / lm_slope_c2) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3_raw - dem_int_c2) / dem_slope_c2) 

summary(filter_data_c2$bc_ug_m3_raw)
summary(filter_data_c2$bc_ug_m3_lm)
summary(filter_data_c2$bc_ug_m3_dem)

ggplot(filter_data_c2) +
  geom_density(aes(x = bc_ug_m3_raw, color = "Raw")) +
  geom_density(aes(x = bc_ug_m3_lm, color = "Crude Linear model")) +
  geom_density(aes(x = bc_ug_m3_dem, color = "Deming model")) +
  xlab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name = "Data type", discrete = T) +
  simple_theme

ggplot(filter_data_c2) +
  geom_point(aes(x = bc_ug_m3_raw, y = bc_ug_m3_dem)) +
  simple_theme

filter_data_c2_long <- filter_data_c2 %>%
  select(bc_ug_m3_raw, bc_ug_m3_dem, bc_ug_m3_lm) %>%
  pivot_longer(cols = bc_ug_m3_raw:bc_ug_m3_lm, 
               names_to = "reg", values_to = "bc")

ggplot(filter_data_c2_long) +
  geom_boxplot(aes(x = as.factor(reg), y = bc))

#' -------------------------------------
#' Campaign 3
#' -------------------------------------

filter_data_c3 <- filter(filter_data, campaign == "Campaign3")
summary(filter_data_c3$bc_ug_m3)
hist(filter_data_c3$bc_ug_m3)

#' Calibrating the filter data
filter_data_c3 <- filter_data_c3 %>%
  #' rename the concentration to specify that it's raw
  rename(bc_ug_m3_raw = bc_ug_m3) %>%
  #' Calibrated BC (Linear model)
  mutate(bc_ug_m3_lm = (bc_ug_m3_raw - lm_int_c3) / lm_slope_c3) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3_raw - dem_int_c3) / dem_slope_c3) 

summary(filter_data_c3$bc_ug_m3_raw)
summary(filter_data_c3$bc_ug_m3_lm)
summary(filter_data_c3$bc_ug_m3_dem)

ggplot(filter_data_c3) +
  geom_density(aes(x = bc_ug_m3_raw, color = "Raw")) +
  geom_density(aes(x = bc_ug_m3_lm, color = "Crude Linear model")) +
  geom_density(aes(x = bc_ug_m3_dem, color = "Deming model")) +
  xlab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name = "Data type", discrete = T) +
  simple_theme

ggplot(filter_data_c3) +
  geom_point(aes(x = bc_ug_m3_raw, y = bc_ug_m3_dem)) +
  simple_theme

filter_data_c3_long <- filter_data_c3 %>%
  select(bc_ug_m3_raw, bc_ug_m3_dem, bc_ug_m3_lm) %>%
  pivot_longer(cols = bc_ug_m3_raw:bc_ug_m3_lm, 
               names_to = "reg", values_to = "bc")

ggplot(filter_data_c3_long) +
  geom_boxplot(aes(x = as.factor(reg), y = bc))

#' -------------------------------------
#' Campaign 4 (remember to drop low vols)
#' -------------------------------------

filter_data_c4 <- filter(filter_data, campaign == "Campaign4")
summary(filter_data_c4$bc_ug_m3)
summary(cal_data_c4$bc_ug_m3)
hist(filter_data_c4$bc_ug_m3)

#' Calibrating the filter data
filter_data_c4 <- filter_data_c4 %>%
  #' rename the concentration to specify that it's raw
  rename(bc_ug_m3_raw = bc_ug_m3) %>%
  #' Calibrated BC (Linear model)
  mutate(bc_ug_m3_lm = (bc_ug_m3_raw - lm_int_c4) / lm_slope_c4) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3_raw - dem_int_c4) / dem_slope_c4) 

summary(filter_data_c4$bc_ug_m3_raw)
summary(filter_data_c4$bc_ug_m3_lm)
summary(filter_data_c4$bc_ug_m3_dem)

ggplot(filter_data_c4) +
  geom_density(aes(x = bc_ug_m3_raw, color = "Raw")) +
  geom_density(aes(x = bc_ug_m3_lm, color = "Crude Linear model")) +
  geom_density(aes(x = bc_ug_m3_dem, color = "Deming model")) +
  xlab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name = "Data type", discrete = T) +
  simple_theme

ggplot(filter_data_c4) +
  geom_point(aes(x = bc_ug_m3_raw, y = bc_ug_m3_dem)) +
  simple_theme

filter_data_c4_long <- filter_data_c4 %>%
  select(bc_ug_m3_raw, bc_ug_m3_dem, bc_ug_m3_lm) %>%
  pivot_longer(cols = bc_ug_m3_raw:bc_ug_m3_lm, 
               names_to = "reg", values_to = "bc")

ggplot(filter_data_c4_long) +
  geom_boxplot(aes(x = as.factor(reg), y = bc))

#' -------------------------------------
#' Campaign 5
#' -------------------------------------

filter_data_c5 <- filter(filter_data, campaign == "Campaign5")
summary(filter_data_c5$bc_ug_m3)
summary(cal_data_c5$bc_ug_m3)
hist(filter_data_c5$bc_ug_m3)

#' Calibrating the filter data
filter_data_c5 <- filter_data_c5 %>%
  #' rename the concentration to specify that it's raw
  rename(bc_ug_m3_raw = bc_ug_m3) %>%
  #' Calibrated BC (Linear model)
  mutate(bc_ug_m3_lm = (bc_ug_m3_raw - lm_int_c5) / lm_slope_c5) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3_raw - dem_int_c5) / dem_slope_c5) 

summary(filter_data_c5$bc_ug_m3_raw)
summary(filter_data_c5$bc_ug_m3_lm)
summary(filter_data_c5$bc_ug_m3_dem)

ggplot(filter_data_c5) +
  geom_density(aes(x = bc_ug_m3_raw, color = "Raw")) +
  geom_density(aes(x = bc_ug_m3_lm, color = "Crude Linear model")) +
  geom_density(aes(x = bc_ug_m3_dem, color = "Deming model")) +
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
  dplyr::select(filter_id, campaign, bc_ug_m3_raw, bc_ug_m3_lm, bc_ug_m3_dem) %>% 
  pivot_longer(names_to = "type", values_to = "bc", -c(filter_id, campaign))

ggplot(filter_data_comp, aes(x = as.factor(campaign), y = bc, fill = as.factor(type))) +
  ggtitle("Distribution of measured and calibrated BC by campaign") +
  geom_boxplot(alpha = 0.75) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "EPA Monitors")) +
  xlab("Campaign") + ylab("BC (\u03bcg/m\u00b3)") +
  scale_y_continuous(limits = c(-5, 10)) +
  theme(legend.position = "bottom") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Calibrated_BC_by_Campaign.jpeg"),
       height = 4, width = 6, dpi = 500, units = "in", device = "jpeg")

#' Write out the calibrated data set
glimpse(filter_data3)
write_csv(filter_data3, here::here("Data", "Filter_BC_Calibrated.csv"))

#' Save the calibration models
#' Also save a date-stamped version in case we need to go back
today <- Sys.Date()
save(i25_lm, i25_lm_c2, i25_lm_c3, i25_lm_c4, i25_lm_c5,
     i25_dem, i25_dem_c2, i25_dem_c3, i25_dem_c4, i25_dem_c5,
     cal_data, cal_data_c2, cal_data_c3, cal_data_c4, cal_data_c5,
     file = here::here("Results", "Filter_BC_CalModels.rdata"))

save(i25_lm, i25_lm_c2, i25_lm_c3, i25_lm_c4, i25_lm_c5,
     i25_dem, i25_dem_c2, i25_dem_c3, i25_dem_c4, i25_dem_c5,
     cal_data, cal_data_c2, cal_data_c3, cal_data_c4, cal_data_c5,
     file = here::here("Results/Archived_Results", 
                       paste0("Filter_BC_CalModels_", today, ".rdata")))

load(here::here("Results", "Filter_BC_CalModels.rdata"))

cor(i25_dem_c2$model$bc_ug_m3, i25_dem_c2$model$monitor_mean)
cor(i25_dem_c3$model$bc_ug_m3, i25_dem_c3$model$monitor_mean)
cor(i25_dem_c4$model$bc_ug_m3, i25_dem_c4$model$monitor_mean)
cor(i25_dem_c5$model$bc_ug_m3, i25_dem_c5$model$monitor_mean)


#' -----------------------------------------------------------------------------
#' Plots for papers
#' -----------------------------------------------------------------------------

load(here::here("Results", "Filter_BC_CalModels.rdata"))

#' Plot BC monitor vs UPAS by campaign
ggplot() +
  geom_point(data = cal_data_c2,
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  geom_abline(data = cal_data_c2,
              aes(slope = lm_slope_c2, intercept = lm_int_c2,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = cal_data_c2,
              aes(slope = dem_slope_c2, intercept = dem_int_c2,
                  color = campaign, linetype = "dem"), size = 1) +
  
  geom_point(data = cal_data_c3,
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  geom_abline(data = cal_data_c3,
              aes(slope = lm_slope_c3, intercept = lm_int_c3,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = cal_data_c3,
              aes(slope = dem_slope_c3, intercept = dem_int_c3,
                  color = campaign, linetype = "dem"), size = 1) +
  
  geom_point(data = cal_data_c4,
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  geom_abline(data = cal_data_c4,
              aes(slope = lm_slope_c4, intercept = lm_int_c4,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = cal_data_c4,
              aes(slope = dem_slope_c4, intercept = dem_int_c4,
                  color = campaign, linetype = "dem"), size = 1) +
  
  geom_point(data = cal_data_c5,
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
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
  ylab("UPAS BC (\u03bcg/m\u00b3)") + xlab("Monitor BC (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_LM_Dem_by_Campaign.jpeg"),
       height = 7, width = 7, dpi = 500, units = "in", device = "jpeg")

#' Plot BC monitor vs UPAS by campaign (drop campaign 4)
ggplot() +
  geom_point(data = cal_data_c2,
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  geom_abline(data = cal_data_c2,
              aes(slope = lm_slope_c2, intercept = lm_int_c2,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = cal_data_c2,
              aes(slope = dem_slope_c2, intercept = dem_int_c2,
                  color = campaign, linetype = "dem"), size = 1) +
  
  geom_point(data = cal_data_c3,
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  geom_abline(data = cal_data_c3,
              aes(slope = lm_slope_c3, intercept = lm_int_c3,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = cal_data_c3,
              aes(slope = dem_slope_c3, intercept = dem_int_c3,
                  color = campaign, linetype = "dem"), size = 1) +
  
  # geom_point(data = cal_data_c4,
  #            aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  # geom_abline(data = cal_data_c4,
  #             aes(slope = lm_slope_c4, intercept = lm_int_c4,
  #                 color = campaign, linetype = "lm"), size = 1) +
  # geom_abline(data = cal_data_c4,
  #             aes(slope = dem_slope_c4, intercept = dem_int_c4,
  #                 color = campaign, linetype = "dem"), size = 1) +
  
  geom_point(data = cal_data_c5,
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
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
  ylab("UPAS BC (\u03bcg/m\u00b3)") + xlab("Monitor BC (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_LM_Dem_by_Campaign_235.jpeg"),
       height = 7, width = 7, dpi = 500, units = "in", device = "jpeg")

#' Bland-Altman plots 
library(BlandAltmanLeh)
library(ggExtra)

scaleFUN <- function(x) sprintf("%.1f", x)

mean_diff_all <- mean(cal_data$bc_ug_m3 - cal_data$monitor_mean)
ba_plot_all <- bland.altman.plot(cal_data$bc_ug_m3, cal_data$monitor_mean, 
                                 graph.sys="ggplot2")
ba_all <- print(ba_plot_all) +
  geom_hline(aes(yintercept = mean_diff_all), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 1) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels=  scaleFUN) + scale_x_continuous(labels = scaleFUN) +
  simple_theme +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"))
ba_all

mean_diff_c2 <- mean(cal_data_c2$bc_ug_m3 - cal_data_c2$monitor_mean)
ba_plot_c2 <- bland.altman.plot(cal_data_c2$bc_ug_m3, cal_data_c2$monitor_mean, 
                                    graph.sys="ggplot2")
ba_c2 <- print(ba_plot_c2) +
  geom_hline(aes(yintercept = mean_diff_c2), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels=  scaleFUN) + scale_x_continuous(labels = scaleFUN) +
  simple_theme +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"))
ba_c2

mean_diff_c3 <- mean(cal_data_c3$bc_ug_m3 - cal_data_c3$monitor_mean)
ba_plot_c3 <- bland.altman.plot(cal_data_c3$bc_ug_m3, cal_data_c3$monitor_mean, 
                                graph.sys="ggplot2")
ba_c3 <- print(ba_plot_c3) +
  geom_hline(aes(yintercept = mean_diff_c3), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels=  scaleFUN) + scale_x_continuous(labels = scaleFUN) +
  simple_theme +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"))
ba_c3

mean_diff_c4 <- mean(cal_data_c4$bc_ug_m3 - cal_data_c4$monitor_mean)
ba_plot_c4 <- bland.altman.plot(cal_data_c4$bc_ug_m3, cal_data_c4$monitor_mean, 
                                graph.sys="ggplot2")
ba_c4 <- print(ba_plot_c4) +
  geom_hline(aes(yintercept = mean_diff_c4), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels=  scaleFUN) + scale_x_continuous(labels = scaleFUN) +
  simple_theme +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"))
ba_c4

mean_diff_c5 <- mean(cal_data_c5$bc_ug_m3 - cal_data_c5$monitor_mean)
ba_plot_c5 <- bland.altman.plot(cal_data_c5$bc_ug_m3, cal_data_c5$monitor_mean, 
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
       filename = here::here("Figs/Calibration", "BA_Plots_Combined_BC.jpeg"),
       height = 9, width = 7, dpi = 500, units = "in", device = "jpeg")

