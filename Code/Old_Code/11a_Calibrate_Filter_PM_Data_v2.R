#' =============================================================================
#' Project: ECHO LUR
#' Date created: July 23, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script calibrates the PM2.5 gravimetric data using montitoring data from
#' the I-25 site site
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
monitor_temp <- read_csv(here::here("Data", "Monitor_TEMP_Data_AEA.csv"))
# monitor_rh <- read_csv(here::here("Data", "Monitor_RH_DP_Data_AEA.csv"))
# monitor_wind <- read_csv(here::here("Data", "MOnitor_WIND_Data_AEA.csv"))

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
  dplyr::select(filter_id, StartDateTimeLocal, EndDateTimeLocal, month, is_blank, 
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
  geom_line(aes(color = campaign)) +
  geom_smooth(method = "loess") +
  facet_grid(monitor_fac ~ .) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "month") +
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
  geom_line(aes(color = monitor_id), show.legend = F) +
  geom_smooth(method = "loess") +
  facet_grid(monitor_fac ~ .) +
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
#' Give Deming regression a try- accounts for errors in "outcome"
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

#' -----------------------------------------------------------------------------
#' Try increasing fit for the I25 monitor, which also has temp
#' Get other variables for the filters located at this monitor
#' 
#' Since the PM2.5 monitor is an FRM/FEM monitor, we can be confident in the 
#' results and don't really need to use Deming regression here
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

cor(cal_data2$monitor_mean, cal_data2$pm_ug_m3)
plot(cal_data2$monitor_mean, cal_data2$pm_ug_m3)

#' Fit linear regression model with temp; adjusted R2 = 0.73
i25_lm1 <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp, data = cal_data2)
summary(i25_lm1)
par(mfrow=c(2,2))
plot(i25_lm1)
par(mfrow=c(1,1))

#' Fit linear regression model with temp + temp_sq; adjusted R2 = 0.80
i25_lm1a <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp + monitor_temp_sq, 
               data = cal_data2)
summary(i25_lm1a)
par(mfrow=c(2,2))
plot(i25_lm1a)
par(mfrow=c(1,1))

#' Linear regression models for each campaign
#' Summer = Campaign2
#' adjusted R2 = 0.77
cal_data2_summer <- filter(cal_data2, campaign == "Campaign2")
cor(cal_data2_summer$monitor_mean, cal_data2_summer$pm_ug_m3)
plot(cal_data2_summer$monitor_mean, cal_data2_summer$pm_ug_m3)

i25_lm1_summer <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp, 
                     data = cal_data2_summer)
summary(i25_lm1_summer)
par(mfrow=c(2,2))
plot(i25_lm1_summer)
par(mfrow=c(1,1))

#' Fall = Campaign3
#' Adjusted R2 = 0.47
cal_data2_fall <- filter(cal_data2, campaign == "Campaign3")
cor(cal_data2_fall$monitor_mean, cal_data2_fall$pm_ug_m3)
plot(cal_data2_fall$monitor_mean, cal_data2_fall$pm_ug_m3)

i25_lm1_fall <- lm(pm_ug_m3 ~ monitor_mean  + monitor_temp, 
                   data = cal_data2_fall)
summary(i25_lm1_fall)
par(mfrow=c(2,2))
plot(i25_lm1_fall)
par(mfrow=c(1,1))

#' Winter = Campaign4
#' Adjusted R2 = 0.79
cal_data2_winter <- filter(cal_data2, campaign == "Campaign4")
cor(cal_data2_winter$monitor_mean, cal_data2_winter$pm_ug_m3)
plot(cal_data2_winter$monitor_mean, cal_data2_winter$pm_ug_m3)

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
         camp4 = ifelse(campaign == "Campaign4", 1, 0)) 

cor(cal_data3$monitor_mean, cal_data3$pm_ug_m3)
plot(cal_data3$monitor_mean, cal_data3$pm_ug_m3)

#' Campaign2 is the reference group, separate indicators for camp3 and camp4
#' adjusted R2 = 0.84
i25_lm2 <- lm(pm_ug_m3 ~ monitor_mean + camp3 + camp4, 
              data = cal_data3)
summary(i25_lm2)
par(mfrow=c(2,2))
plot(i25_lm2)
par(mfrow=c(1,1))

#' Add temperature
#' Adjusted R2 = 0.83
i25_lm2a <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp + camp3 + camp4, 
               data = cal_data3)
summary(i25_lm2a)
par(mfrow=c(2,2))
plot(i25_lm2a)
par(mfrow=c(1,1))

#' Add temperature squared
#' Adjusted R2 = 0.82
i25_lm2b <- lm(pm_ug_m3 ~ monitor_mean + monitor_temp + monitor_temp_sq +
                 camp3 + camp4, 
               data = cal_data3)
summary(i25_lm2b)
par(mfrow=c(2,2))
plot(i25_lm2b)
par(mfrow=c(1,1))

#' -----------------------------------------------------------------------------
#' compare i25 models
#' -----------------------------------------------------------------------------

summary(i25_lm2) #R2 = 0.84
summary(i25_lm2a) #R2 = 0.83
summary(i25_lm2b) #R2 = 0.82

AIC(i25_lm2) #AIC = 74.4
AIC(i25_lm2a) #AIC = 75.8
AIC(i25_lm2b) #AIC = 77.7

hist(i25_lm2$residuals)
hist(i25_lm2a$residuals)
hist(i25_lm2b$residuals)

#' -----------------------------------------------------------------------------
#' Based on AIC, adjusted R2 and the residuals, I'm going to go with model 2 for
#' now
#' 
#' Mean bias = 1.32, RMSE = 1.61
#' -----------------------------------------------------------------------------

fit_adjlm <- i25_lm2
summary(fit_adjlm)
fit_monitor <- "080310027"

#' Coefficients for calibration equation
int_adjlm <- unname(fit_adjlm$coefficients[1])
beta_pm_adjlm <- unname(fit_adjlm$coefficients[2])
beta_camp3_adjlm <- unname(fit_adjlm$coefficients[3])
beta_camp4_adjlm <- unname(fit_adjlm$coefficients[4])

#' Calibrating the filter data
fit_data_final <- cal_data3 %>% 
  #' Calibrated PM (adjusted linear model)
  mutate(pm_ug_m3_adjlm = (pm_ug_m3 - int_adjlm - (beta_camp3_adjlm * camp3) - 
                           (beta_camp4_adjlm * camp4)) / beta_pm_adjlm) %>% 
  #' Calibrate using crude linear model and deming model
  mutate(pm_ug_m3_lm = (pm_ug_m3 - int_lm) / beta_pm_lm,
         pm_ug_m3_dem = (pm_ug_m3 - int_dem) / beta_pm_dem)
  

#' MAE and RMSE- Adjusted linear model
mae(fit_lm$residuals)
rmse(fit_lm$residuals)

#' MAE and RMSE- Crude linear model
mae(i25_lm$residuals)
rmse(i25_lm$residuals)

#' MAE and RMSE- Crude linear model
mae(i25_dem$residuals)
rmse(i25_dem$residuals)

#' -----------------------------------------------------------------------------
#' Calibrate the filter data using the adjusted linear regression equation and
#' the deming regression equation
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
  
  #' Get mean concentration from the monitor for the period covered by the filter
  mon <- monitor_data %>% 
    filter(monitor_id == paired_monitor_id) %>% 
    filter(Date_Local %in% dates)
  
  mean_pol <- mean(mon$Arithmetic_Mean, na.rm=T)
  
  filter_data2[i, "monitor_mean"] <- mean_pol
  filter_data2[i, "monitor_mean_n_obs"] <- nrow(mon)
  
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