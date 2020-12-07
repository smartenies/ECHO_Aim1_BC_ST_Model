#' =============================================================================
#' Project: ECHO LUR
#' Date created: November 5, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script calibrates the PM2.5 gravimetric data using montitoring data from
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

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Read in the filter PM2.5 data and monitor data
#' Read in list of collocated monitors
#' -----------------------------------------------------------------------------

filter_data <- read_csv(here::here("Data", "Filter_PM.csv")) %>% 
  filter(!is.na(EndDateLocal)) %>% 
  arrange(EndDateLocal) 
monitor_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv"))
monitor_temp <- read_csv(here::here("Data", "Monitor_TEMP_Data_AEA.csv"))
monitor_rh <- read_csv(here::here("Data", "Monitor_RH_DP_Data_AEA.csv"))
monitor_wind <- read_csv(here::here("Data", "MOnitor_WIND_Data_AEA.csv"))

collocated_ids <- read_csv(here::here("Data", "Collocation_Filter_IDs.csv"))

#' Collocated monitors:
#' National Jewish (080310013) 1400 Jackson St, Denver, CO 80206: PM2.5
#' I-25 Denver (080310027) 9th and Yuma: PM2.5, Black Carbon, Temp
#' I-25 Globeville (080310028) 49th and Acoma: PM2.5, Temp, RH, Wind

collocated_ids <- collocated_ids %>% 
  mutate(monitor_id = ifelse(Location == "1400 Jackson St, Denver, CO 80206", "080310013",
                             ifelse(Location == "9th and Yuma", "080310027",
                                    "080310028")))

cal_data <- filter(filter_data, filter_id %in% collocated_ids$filter_id) %>% 
  select(filter_id, StartDateTimeLocal, EndDateTimeLocal, month, is_blank, 
         below_lod, pm_ug_m3, campaign) %>% 
  left_join(collocated_ids, by = "filter_id") %>% 
  filter(is_blank == 0) %>% 
  filter(!is.na(pm_ug_m3))

summary(cal_data)

cal_data$monitor_fac <- factor(cal_data$monitor_id, 
                               labels = c("National Jewish", "I-25 Denver",
                                          "Globeville"))

ggplot(cal_data, aes(x = StartDateTimeLocal, y = pm_ug_m3)) +
  geom_point(aes(color = campaign)) +
  geom_smooth(method = "loess") +
  facet_grid(monitor_fac ~ .) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  xlab("Date") + ylab("Time-weighted average UPAS PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "PM_UPAS_TS.jpeg"),
       height = 8, width = 6, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Plot time series for each monitor
#' Want to get a sense of the trends for the monitoring data
#' -----------------------------------------------------------------------------

start <- filter_data$StartDateLocal[1]
end <- filter_data$EndDateLocal[length(filter_data$EndDateLocal)]

date_seq <- seq.Date(start, end, by = "day")

co_mon <- filter(monitor_data, monitor_id %in% c("080310013", "080310027", "080310028")) %>% 
  filter(Date_Local %in% date_seq)

co_mon$monitor_fac <- factor(co_mon$monitor_id, 
                               labels = c("National Jewish", "I-25 Denver",
                                          "Globeville"))

ggplot(co_mon, aes(x = Date_Local, y = Arithmetic_Mean)) +
  geom_point(aes(color = monitor_id), show.legend = F) +
  geom_smooth(method = "loess") +
  facet_grid(monitor_fac ~ .) +
  scale_color_viridis(name = "Monitoring Site", discrete = T) +
  xlab("Date") + ylab("Daily mean monitor PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "PM_Monitor_TS.jpeg"),
       height = 8, width = 6, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Assign monitor PM2.5 to filters using start and end dates
#' Initial model fit
#' -----------------------------------------------------------------------------

#' Assign start and end PM2.5 concentrations from the monitoring data
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

#' Plot PM2.5 monitor vs UPAS for each location
cal_data$monitor_fac <- factor(cal_data$monitor_id, 
                               labels = c("National Jewish", "I-25 Denver",
                                          "Globeville"))

ggplot(cal_data, aes(x = monitor_mean, y = pm_ug_m3)) +
  geom_point(aes(color = as.factor(monitor_id)), show.legend = F) +
  geom_smooth(method = "lm", show.legend = F) +
  scale_color_viridis(name = "Monitoring site", discrete = T) +
  facet_grid(monitor_fac ~ .) +
  ylab("UPAS PM\u2082.\u2085 (\u03bcg/m\u00b3)") + 
  xlab("Monitor PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "PM_Cal_Curves.jpeg"),
       height = 5, width = 5, dpi = 500, units = "in", device = "jpeg")

#' Linear regression models stratified by location
#' Which one has best preliminary fit?

#' NATIONAL JEWISH: R2 = 0.21
njh_lm <- lm(pm_ug_m3 ~ monitor_mean,  
             data = filter(cal_data, monitor_id == "080310013"))
summary(njh_lm)
par(mfrow=c(2,2))
plot(njh_lm)
par(mfrow=c(1,1))

#' I25-Denver: R2 = 0.41
i25_lm <- lm(pm_ug_m3 ~ monitor_mean,  
             data = filter(cal_data, monitor_id == "080310027"))
summary(i25_lm)
par(mfrow=c(2,2))
plot(i25_lm)
par(mfrow=c(1,1))

#' GLOBEVILLE: R2 = 0.53
globe_lm <- lm(pm_ug_m3 ~ monitor_mean,  
               data = filter(cal_data, monitor_id == "080310028"))
summary(globe_lm)
par(mfrow=c(2,2))
plot(globe_lm)
par(mfrow=c(1,1))

              
#' -----------------------------------------------------------------------------
#' Try increasing fit for the I25 monitor, which also has temp
#' Get other variables for the filters located at this monitor
#' -----------------------------------------------------------------------------

cal_data2 <- filter(cal_data, monitor_id == "080310027")

#' Assign start and end TEMP from the monitoring data
for (i in 1:nrow(cal_data2)) {
  df <- slice(cal_data2, i)
  
  if(is.na(df$StartDateTimeLocal[1])) next
  
  #' Which monitor is collocated
  paired_monitor_id <- df$monitor_id[1]
  
  #' What are the start and end dates for this filter
  start_date <- df$StartDateTimeLocal[1]
  end_date <- df$EndDateTimeLocal[1]
  
  dates <- seq.Date(from = start_date, to = end_date, by = "day")
  
  #' Get mean temperature from the monitor for the period covered by the filter
  temp <- monitor_temp %>% 
    filter(monitor_id == paired_monitor_id) %>% 
    filter(Date_Local %in% dates)
  mean_temp <- mean(temp$Arithmetic_Mean, na.rm=T)
  
  cal_data2[i, "monitor_temp"] <- mean_temp
  cal_data2[i, "monitor_temp_n_obs"] <- nrow(temp)
}

summary(cal_data2)
cal_data2 <- mutate(cal_data2) %>% 
  mutate(monitor_mean_sq = monitor_mean**2,
         monitor_temp_sq = monitor_temp**2)

#' Fit linear regression model with temp; adjusted R2 = 0.73
cor(cal_data2$pm_ug_m3, cal_data2$monitor_mean)
plot(cal_data2$pm_ug_m3, cal_data2$monitor_mean)

i25_lm1 <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp, data = cal_data2)
summary(i25_lm1)
par(mfrow=c(2,2))
plot(i25_lm1)
par(mfrow=c(1,1))

#' Fit linear regression model with temp + temp_sq; adjusted R2 = 0.80
cor(cal_data2$pm_ug_m3, cal_data2$monitor_mean)
plot(cal_data2$pm_ug_m3, cal_data2$monitor_mean)

i25_lm1a <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp + monitor_temp_sq, 
               data = cal_data2)
summary(i25_lm1a)
par(mfrow=c(2,2))
plot(i25_lm1a)
par(mfrow=c(1,1))

#' Linear regression models for each campaign
#' Summer = Campaign2
cal_data2_summer <- filter(cal_data2, campaign == "Campaign2")
cor(cal_data2_summer$pm_ug_m3, cal_data2_summer$monitor_mean)
plot(cal_data2_summer$pm_ug_m3, cal_data2_summer$monitor_mean)

i25_lm1_summer <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp, 
                     data = cal_data2_summer)
summary(i25_lm1_summer)
par(mfrow=c(2,2))
plot(i25_lm1_summer)
par(mfrow=c(1,1))

#' Fall = Campaign3
cal_data2_fall <- filter(cal_data2, campaign == "Campaign3")
cor(cal_data2_fall$pm_ug_m3, cal_data2_fall$monitor_mean)
plot(cal_data2_fall$pm_ug_m3, cal_data2_fall$monitor_mean)

i25_lm1_fall <- lm(pm_ug_m3 ~ monitor_mean  + monitor_temp, 
                   data = cal_data2_fall)
summary(i25_lm1_fall)
par(mfrow=c(2,2))
plot(i25_lm1_fall)
par(mfrow=c(1,1))

#' Winter = Campaign4
cal_data2_winter <- filter(cal_data2, campaign == "Campaign4")
cor(cal_data2_winter$pm_ug_m3, cal_data2_winter$monitor_mean)
plot(cal_data2_winter$pm_ug_m3, cal_data2_winter$monitor_mean)

i25_lm1_winter <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp, 
                     data = cal_data2_winter)
summary(i25_lm1_winter)
par(mfrow=c(2,2))
plot(i25_lm1_winter)
par(mfrow=c(1,1))

#' -----------------------------------------------------------------------------
#' There seems to be a difference in how the curve fits for different campaigns
#' Use indicators for campaign and a temperature variable
#' -----------------------------------------------------------------------------

cal_data3 <- cal_data2 %>% 
  mutate(camp2 = ifelse(campaign == "Campaign2", 1, 0),
         camp3 = ifelse(campaign == "Campaign3", 1, 0),
         camp4 = ifelse(campaign == "Campaign4", 1, 0),
         camp123 = ifelse(campaign == "Campaign4", 0, 1)) 

cor(cal_data3$pm_ug_m3, cal_data3$monitor_mean)
plot(cal_data3$pm_ug_m3, cal_data3$monitor_mean)

#' Campaign2 is the reference group, separate indicators for camp3 and camp4
i25_lm2 <- lm(pm_ug_m3 ~ monitor_mean + camp3 + camp4, 
              data = cal_data3)
summary(i25_lm2)
par(mfrow=c(2,2))
plot(i25_lm2)
par(mfrow=c(1,1))

#' Add temperature
i25_lm2a <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp + camp3 + camp4, 
               data = cal_data3)
summary(i25_lm2a)
par(mfrow=c(2,2))
plot(i25_lm2a)
par(mfrow=c(1,1))

#' Add temperature squared
i25_lm2b <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp + monitor_temp_sq +
                 camp3 + camp4, 
               data = cal_data3)
summary(i25_lm2b)
par(mfrow=c(2,2))
plot(i25_lm2b)
par(mfrow=c(1,1))

#' Replace camp3 + camp4 with camp123 (co campaign 4 is the reference group)
i25_lm2c <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp + monitor_temp_sq + camp123, 
               data = cal_data3)
summary(i25_lm2c)
par(mfrow=c(2,2))
plot(i25_lm2c)
par(mfrow=c(1,1))

#' -----------------------------------------------------------------------------
#' compare i25 models
#' -----------------------------------------------------------------------------

summary(i25_lm)
summary(i25_lm1)
summary(i25_lm2) #R2 = 0.84
summary(i25_lm2a) #R2 = 0.83
summary(i25_lm2b) #R2 = 0.82
summary(i25_lm2c) #R2 = 0.79

AIC(i25_lm)
AIC(i25_lm1)
AIC(i25_lm2) #AIC = 74.4
AIC(i25_lm2a) #AIC = 75.8
AIC(i25_lm2b) #AIC = 77.7
AIC(i25_lm2c)

hist(i25_lm$residuals)
hist(i25_lm1$residuals)
hist(i25_lm2$residuals)
hist(i25_lm2a$residuals)
hist(i25_lm2b$residuals)
hist(i25_lm2c$residuals)

#' -----------------------------------------------------------------------------
#' Based on AIC, adjusted R2 and the residuals, I'm going to go with model 2 for
#' not
#' 
#' Mean bias = 2.6, RMSE = 1.04
#' -----------------------------------------------------------------------------

fit_lm <- i25_lm2
summary(fit_lm)
fit_monitor <- "080310027"

#' fit_data <- cal_data3 %>% 
#'   #' Calibrated PM
#'   mutate(pm_ug_m3_cal = predict(fit_lm))

#' Coefficients for calibration equation
int <- unname(fit_lm$coefficients[1])
beta_pm <- unname(fit_lm$coefficients[2])
beta_camp3 <- unname(fit_lm$coefficients[3])
beta_camp4 <- unname(fit_lm$coefficients[4])

#' Calibrating using equation
fit_data <- cal_data3 %>% 
  #' Calibrated PM
  mutate(pm_ug_m3_cal = (pm_ug_m3 - int - (beta_camp3 * camp3) - 
                           (beta_camp4 * camp4)) / beta_pm)

#' Mean bias and RMSE
mean_bias <- sum((fit_data$pm_ug_m3_cal - fit_data$pm_ug_m3), na.rm=T) / nrow(fit_data)
mean_bias

rmse <- sqrt(sum((fit_data$pm_ug_m3_cal - fit_data$pm_ug_m3)^2,
                 na.rm = TRUE)) / nrow(fit_data)
rmse

#' -----------------------------------------------------------------------------
#' Calibrate the filter data using the linear regression equation
#' -----------------------------------------------------------------------------

filter_data2 <- filter_data %>%
  filter(indoor == 0) %>%
  filter(is_blank == 0) %>%
  filter(potential_contamination == 0) %>%
  filter(negative_pm_mass == 0) %>%
  filter(!is.na(pm_ug_m3)) %>%
  filter(low_volume_flag == 0) %>% 
  mutate(monitor_id = fit_monitor)
summary(filter_data2)

nrow(filter_data2)
glimpse(filter_data2)

#' Assign start and end TEMP from the monitoring data
for (i in 1:nrow(filter_data2)) {
  df <- slice(filter_data2, i)
  
  if(is.na(df$StartDateTimeLocal[1])) next
  
  #' Which monitor is collocated
  paired_monitor_id <- fit_monitor
  
  #' What are the start and end dates for this filter
  start_date <- df$StartDateTimeLocal[1]
  end_date <- df$EndDateTimeLocal[1]
  
  dates <- seq.Date(from = start_date, to = end_date, by = "day")
  
  #' Get mean temperature from the monitor for the period covered by the filter
  temp <- monitor_temp %>% 
    filter(monitor_id == paired_monitor_id) %>% 
    filter(Date_Local %in% dates)
  mean_temp <- mean(temp$Arithmetic_Mean, na.rm=T)
  
  filter_data2[i, "monitor_temp"] <- mean_temp
  filter_data2[i, "monitor_temp_n_obs"] <- nrow(temp)
}

#' Add the indicator variables
filter_data2 <- filter_data2 %>% 
  mutate(camp2 = ifelse(campaign == "Campaign2", 1, 0),
         camp3 = ifelse(campaign == "Campaign3", 1, 0),
         camp4 = ifelse(campaign == "Campaign4", 1, 0)) 

# filter_data2 <- filter_data2 %>%
#   mutate(pm_ug_m3_cal = unname(predict(fit_lm, newdata = filter_data2)))

#' Calibrating using equation
filter_data2 <- filter_data2 %>% 
  #' Calibrated PM
  mutate(pm_ug_m3_cal = (pm_ug_m3 - int - (beta_camp3 * camp3) - 
                           (beta_camp4 * camp4)) / beta_pm)

summary(filter_data2$pm_ug_m3)
summary(filter_data2$pm_ug_m3_cal)

#' Mean bias and RMSE
mean_bias <- sum((filter_data2$pm_ug_m3_cal - filter_data2$pm_ug_m3), na.rm=T) / nrow(filter_data2)
mean_bias

rmse <- sqrt(sum((filter_data2$pm_ug_m3_cal - filter_data2$pm_ug_m3)^2,
                 na.rm = TRUE)) / nrow(filter_data2)
rmse

#' How do the monthly means look?
monthly_means <- filter_data2 %>%
  group_by(month) %>%
  summarize(mean_pm_raw = mean(pm_ug_m3, na.rm = T),
            min_pm_raw = min(pm_ug_m3, na.rm = T),
            max_pm_raw = max(pm_ug_m3, na.rm = T),
            mean_pm_cal = mean(pm_ug_m3_cal, na.rm = T),
            min_pm_cal = min(pm_ug_m3_cal, na.rm = T),
            max_pm_cal = max(pm_ug_m3_cal, na.rm = T))
monthly_means

seasonal_means <- filter_data2 %>%
  group_by(season) %>%
  summarize(mean_pm_raw = mean(pm_ug_m3, na.rm = T),
            min_pm_raw = min(pm_ug_m3, na.rm = T),
            max_pm_raw = max(pm_ug_m3, na.rm = T),
            mean_pm_cal = mean(pm_ug_m3_cal, na.rm = T),
            min_pm_cal = min(pm_ug_m3_cal, na.rm = T),
            max_pm_cal = max(pm_ug_m3_cal, na.rm = T))
seasonal_means

filter_data_comp <- filter_data2 %>% 
  select(filter_id, month, season, pm_ug_m3, pm_ug_m3_cal) %>% 
  gather(key = type, value = pm, -filter_id, -month, -season)

monitor_data_comp <- monitor_data %>%
  filter(County_Code %in% c("001", "005", "031")) %>% 
  select(Date_Local, Arithmetic_Mean) %>% 
  mutate(month = month(Date_Local)) %>% 
  mutate(season = ifelse(month %in% c(12, 1, 2), 1,
                         ifelse(month %in% c(3, 4, 5), 2,
                                ifelse(month %in% c(6, 7, 8), 3, 4)))) %>% 
  mutate(type = "monitor", filter_id = "EPA_Monitor") %>% 
  rename("pm" = "Arithmetic_Mean")

filter_data_comp <- bind_rows(filter_data_comp, monitor_data_comp)

ggplot(filter_data_comp, aes(x = as.factor(month), y = pm, fill = as.factor(type))) +
  ggtitle("Monthly distribution of measured and calibrated PM2.5") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("pm_ug_m3" = "Raw Data",
                                "pm_ug_m3_cal" = "Calibrated",
                                "monitor" = "EPA Monitors")) +
  xlab("Month") + ylab("PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.2, 0.8)) +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Monthly_Means_PM.jpeg"),
       height = 5, width = 5, dpi = 500, units = "in", device = "jpeg")

ggplot(filter_data_comp, aes(x = as.factor(season), y = pm, fill = as.factor(type))) +
  ggtitle("Seasonal distribution of measured and calibrated PM2.5") +
  geom_boxplot(alpha = 0.75) +
  # geom_point(position=position_jitterdodge(), color = "grey70", alpha = 0.20) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("pm_ug_m3" = "Raw Data",
                                "pm_ug_m3_cal" = "Calibrated",
                                "monitor" = "EPA Monitors")) +
  xlab("Season") + ylab("BC (\u03bcg/m\u00b3)") +
  scale_x_discrete(labels = c("1" = "Winter", "2" = "Spring",
                              "3" = "Summer", "4" = "Fall")) +
  xlab("Season") + ylab("PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.2, 0.8)) +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Seasonal_Means_PM.jpeg"),
       height = 5, width = 5, dpi = 500, units = "in", device = "jpeg")

#' Write out the calibrated dataset
glimpse(filter_data2)
write_csv(filter_data2, here::here("Data", "Filter_PM_Calibrated.csv"))
