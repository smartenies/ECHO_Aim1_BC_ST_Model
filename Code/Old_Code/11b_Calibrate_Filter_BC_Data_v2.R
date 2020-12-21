#' =============================================================================
#' Project: ECHO LUR
#' Date created: November 7, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script calibrates the BC sootscan data using montitoring data from
#' the I-25 site
#' =============================================================================

library(ggplot2)
library(ggthemes)
library(ggpubr)
library(viridis)
library(tidyverse)
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

simple_theme2 <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 14, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=14),
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
#' Read in the filter BC data and monitor data
#' Read in list of collocated monitors
#' -----------------------------------------------------------------------------

filter_data <- read_csv(here::here("Data", "Filter_BC.csv")) %>% 
  filter(!is.na(EndDateLocal)) %>% 
  arrange(EndDateLocal) 
monitor_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv"))
monitor_temp <- read_csv(here::here("Data", "Monitor_TEMP_Data_AEA.csv"))
monitor_pm <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv"))

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
         bc_ug_m3, bc_mass_ug_corrected, campaign) %>% 
  left_join(collocated_ids, by = "filter_id") %>% 
  filter(is_blank == 0) %>% 
  filter(!is.na(bc_ug_m3))

summary(cal_data)

cal_data$monitor_fac <- factor(cal_data$monitor_id, 
                               labels = c("National Jewish", "I-25 Denver",
                                          "Globeville"))

ggplot(cal_data, aes(x = StartDateTimeLocal, y = bc_ug_m3)) +
  geom_point(aes(color = campaign)) +
  geom_line(aes(color = campaign)) +
  geom_smooth(method = "loess") +
  facet_grid(monitor_fac ~ .) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "month") +
  xlab("Date") + ylab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_UPAS_TS.jpeg"),
       height = 5, width = 7, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Plot time series for the I-25 monitoring site
#' Only site in Denver with a long-term record of BC
#' This site uses an AE-33 aethalometer
#' -----------------------------------------------------------------------------

start <- as.Date("01/01/2018", format = "%m/%d/%Y")
end <- as.Date("06/01/2019", format = "%m/%d/%Y")
date_seq <- seq.Date(start, end, by = "day")

co_mon <- filter(monitor_data, monitor_id == "080310027") %>% 
  filter(Date_Local %in% date_seq)

co_mon$monitor_fac <- factor(co_mon$monitor_id, 
                               labels = c("I-25 Denver"))

ggplot(co_mon, aes(x = Date_Local, y = Arithmetic_Mean)) +
  geom_line(aes(color = monitor_id), show.legend = F) +
  geom_smooth(method = "loess") +
  scale_color_viridis(name = "Monitoring Site", discrete = T) +
  facet_grid(monitor_fac ~ .) +
  scale_x_date(labels = date_format("%m/%y"), date_breaks = "1 month") +
  xlab("Date") + ylab("Daily mean monitor BC (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_Monitor_TS.jpeg"),
       height = 5, width = 7, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Assign monitor BC to filters using start and end dates
#' Initial model fit
#' -----------------------------------------------------------------------------

cal_data <- filter(cal_data, monitor_id == "080310027")
summary(cal_data)

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
cal_data$monitor_fac <- factor(cal_data$monitor_id, 
                               labels = c("I-25 Denver"))

ggplot(cal_data, aes(x = monitor_mean, y = bc_ug_m3)) +
  geom_point(aes(color = as.factor(campaign)), show.legend = T) +
  geom_smooth(method = "lm", show.legend = F) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  ylab("UPAS BC (\u03bcg/m\u00b3)") + 
  xlab("Monitor PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_Cal_Curves.jpeg"),
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
  scale_color_viridis(discrete = T, name = NULL) +
  ylab("UPAS BC (\u03bcg/m\u00b3)") + xlab("Monitor BC (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_Cal_Curves_by_Campaign.jpeg"),
       height = 5, width = 7, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Linear regression model with no additional predictors
#' -----------------------------------------------------------------------------

cor(cal_data$monitor_mean, cal_data$bc_ug_m3)
plot(cal_data$monitor_mean, cal_data$bc_ug_m3)

#' I25-Denver: R2 = -0.05
i25_lm <- lm(bc_ug_m3 ~ monitor_mean,  
             data = filter(cal_data, monitor_id == "080310027"))
summary(i25_lm)
par(mfrow=c(2,2))
plot(i25_lm)
par(mfrow=c(1,1))

lm_int <- unname(i25_lm$coefficients[1])
lm_slope <- unname(i25_lm$coefficients[2])

#' -----------------------------------------------------------------------------
#' Give Deming regression a try- accounts for errors in both monitors
#' -----------------------------------------------------------------------------

library(deming)

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
#' Summer = Campaign2
#' lm: adjusted R2 = -0.03; MAE = ; RMSE = 
#' -------------------------------------

cal_data_summer <- filter(cal_data, campaign == "Campaign2")
cor(cal_data_summer$monitor_mean, cal_data_summer$bc_ug_m3)
plot(cal_data_summer$monitor_mean, cal_data_summer$bc_ug_m3)

i25_lm_summer <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_summer)
summary(i25_lm_summer)
par(mfrow=c(2,2))
plot(i25_lm_summer)
par(mfrow=c(1,1))

lm_int_summer <- unname(i25_lm_summer$coefficients[1])
lm_slope_summer <- unname(i25_lm_summer$coefficients[2])

i25_dem_summer <- deming(bc_ug_m3 ~ monitor_mean, data = cal_data_summer)
print(i25_dem_summer)

dem_int_summer <- unname(i25_dem_summer$coefficients[1])
dem_slope_summer <- unname(i25_dem_summer$coefficients[2])

plot(cal_data_summer$monitor_mean, cal_data_summer$bc_ug_m3, 
     main = "Regression Comparison", 
     xlab = "EPA Monitor", ylab = "UPAS Filter")
abline(i25_lm_summer, col="red")
abline(i25_dem_summer, col = "blue")
legend(x = "topright", col = c("red", "blue"), lty = c(1, 1), legend = c("Linear", "Deming"))

#' MAE and RMSE- Linear model
mae(i25_lm_summer$residuals)
rmse(i25_lm_summer$residuals)

#' MAE and RMSE- Deming model
mae(i25_dem_summer$residuals)
rmse(i25_dem_summer$residuals)

#' -------------------------------------
#' Fall = Campaign3
#' lm: adjusted R2 = -0.12
#' -------------------------------------

cal_data_fall <- filter(cal_data, campaign == "Campaign3")
cor(cal_data_fall$monitor_mean, cal_data_fall$bc_ug_m3)
plot(cal_data_fall$monitor_mean, cal_data_fall$bc_ug_m3)

i25_lm_fall <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_fall)
summary(i25_lm_fall)
par(mfrow=c(2,2))
plot(i25_lm_fall)
par(mfrow=c(1,1))

lm_int_fall <- unname(i25_lm_fall$coefficients[1])
lm_slope_fall <- unname(i25_lm_fall$coefficients[2])

i25_dem_fall <- deming(bc_ug_m3 ~ monitor_mean, data = cal_data_fall)
print(i25_dem_fall)

dem_int_fall <- unname(i25_dem_fall$coefficients[1])
dem_slope_fall <- unname(i25_dem_fall$coefficients[2])

plot(cal_data_fall$monitor_mean, cal_data_fall$bc_ug_m3, 
     main = "Regression Comparison", 
     xlab = "EPA Monitor", ylab = "UPAS Filter")
abline(i25_lm_fall, col="red")
abline(i25_dem_fall, col = "blue")
legend(x = "topright", col = c("red", "blue"), lty = c(1, 1), legend = c("Linear", "Deming"))

#' MAE and RMSE- Linear model
mae(i25_lm_fall$residuals)
rmse(i25_lm_fall$residuals)

#' MAE and RMSE- Deming model
mae(i25_dem_fall$residuals)
rmse(i25_dem_fall$residuals)

#' -------------------------------------
#' Winter = Campaign4
#' lm: adjusted R2 = 0.04
#' -------------------------------------

cal_data_winter <- filter(cal_data, campaign == "Campaign4")
cor(cal_data_winter$monitor_mean, cal_data_winter$bc_ug_m3)
plot(cal_data_winter$monitor_mean, cal_data_winter$bc_ug_m3)

i25_lm_winter <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_winter)
summary(i25_lm_winter)
par(mfrow=c(2,2))
plot(i25_lm_winter)
par(mfrow=c(1,1))

lm_int_winter <- unname(i25_lm_winter$coefficients[1])
lm_slope_winter <- unname(i25_lm_winter$coefficients[2])

i25_dem_winter <- deming(bc_ug_m3 ~ monitor_mean, data = cal_data_winter)
print(i25_dem_winter)

dem_int_winter <- unname(i25_dem_winter$coefficients[1])
dem_slope_winter <- unname(i25_dem_winter$coefficients[2])

plot(cal_data_winter$monitor_mean, cal_data_winter$bc_ug_m3, 
     main = "Regression Comparison", 
     xlab = "EPA Monitor", ylab = "UPAS Filter")
abline(i25_lm_winter, col="red")
abline(i25_dem_winter, col = "blue")
legend(x = "topright", col = c("red", "blue"), lty = c(1, 1), legend = c("Linear", "Deming"))

#' MAE and RMSE- Linear model
mae(i25_lm_winter$residuals)
rmse(i25_lm_winter$residuals)

#' MAE and RMSE- Deming model
mae(i25_dem_winter$residuals)
rmse(i25_dem_winter$residuals)

#' -----------------------------------------------------------------------------
#' Deming doesn't seem to be reducing errors in our predictions (except for 
#' maybe the winter filters)
#' Another downside-- no multivariate model (single predictor only)
#' 
#' Try increasing fit for the I25 monitor, which also has temp and PM2.5
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
  
  #' Get mean PM2.5 from the monitor for the period covered by the filter
  pm <- monitor_pm %>% 
    filter(monitor_id == paired_monitor_id) %>% 
    filter(Date_Local %in% dates)
  mean_pm <- mean(pm$Arithmetic_Mean, na.rm=T)
  
  cal_data2[i, "monitor_pm"] <- mean_pm
  cal_data2[i, "monitor_pm_n_obs"] <- nrow(pm)
}

summary(cal_data2)
cal_data2 <- mutate(cal_data2) %>% 
  mutate(monitor_temp_sq = monitor_temp**2,
         monitor_pm_sq = monitor_pm**2) %>% 
  
  # Indicator variables for campaigns
  mutate(camp2 = ifelse(campaign == "Campaign2", 1, 0),
         camp3 = ifelse(campaign == "Campaign3", 1, 0),
         camp4 = ifelse(campaign == "Campaign4", 1, 0)) 

cor(cal_data2$monitor_mean, cal_data2$bc_ug_m3)
plot(cal_data2$monitor_mean, cal_data2$bc_ug_m3)

#' Fit linear regression model with temp
#' adjusted R2 = 0.65
i25_lm1 <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp, data = cal_data2)
summary(i25_lm1)
par(mfrow=c(2,2))
plot(i25_lm1)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1$residuals)
rmse(i25_lm1$residuals)

#' Fit linear regression model with temp + temp_sq
#' adjusted R2 = 0.63
i25_lm1a <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp + monitor_temp_sq, 
               data = cal_data2)
summary(i25_lm1a)
par(mfrow=c(2,2))
plot(i25_lm1a)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1a$residuals)
rmse(i25_lm1a$residuals)

#' Fit linear regression model with pm2.5
#' adjusted R2 = -0.07
i25_lm1b <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm, 
               data = cal_data2)
summary(i25_lm1b)
par(mfrow=c(2,2))
plot(i25_lm1b)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1b$residuals)
rmse(i25_lm1b$residuals)

#' Fit linear regression model with pm2.5 + temp
#' adjusted R2 = 0.063
i25_lm1c <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_temp, 
               data = cal_data2)
summary(i25_lm1c)
par(mfrow=c(2,2))
plot(i25_lm1c)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1c$residuals)
rmse(i25_lm1c$residuals)

#' Fit linear regression model with temp and indicators for campaign
#' adjusted R2 = 0.84
i25_lm1d <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp + camp3 + camp4, 
              data = cal_data2)
summary(i25_lm1d)
par(mfrow=c(2,2))
plot(i25_lm1d)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1d$residuals)
rmse(i25_lm1d$residuals)

#' -----------------------------------------------------------------------------
#' Because there seems to be a difference in how the curve fits for different 
#' campaigns (i.e., the indicator variables improved model fit), 
#' I want to see if fitting separate curves for each campaign using the temp 
#' and PM2.5 variables helps the overall calibration
#' -----------------------------------------------------------------------------

#' -------------------------------------
#' Summer = Campaign2
#' -------------------------------------

cal_data2_summer <- filter(cal_data2, campaign == "Campaign2")
cor(cal_data2_summer$monitor_mean, cal_data2_summer$bc_ug_m3)
plot(cal_data2_summer$monitor_mean, cal_data2_summer$bc_ug_m3)

#' Fit linear regression model with temp
#' adjusted R2 = 0.01
i25_lm1_summer <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp, 
                     data = cal_data2_summer)
summary(i25_lm1_summer)
par(mfrow=c(2,2))
plot(i25_lm1_summer)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1_summer$residuals)
rmse(i25_lm1_summer$residuals)

#' Fit linear regression model with temp + temp_sq
#' adjusted R2 = 0.98
i25_lm1a_summer <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp + monitor_temp_sq, 
                     data = cal_data2_summer)
summary(i25_lm1a_summer)
par(mfrow=c(2,2))
plot(i25_lm1a_summer)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1a_summer$residuals)
rmse(i25_lm1a_summer$residuals)

#' Fit linear regression model with pm2.5
#' adjusted R2 = 0.07
i25_lm1b_summer <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm, 
                      data = cal_data2_summer)
summary(i25_lm1b_summer)
par(mfrow=c(2,2))
plot(i25_lm1b_summer)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1b_summer$residuals)
rmse(i25_lm1b_summer$residuals)

#' Fit linear regression model with pm2.5 + temp
#' adjusted R2 = -0.82
i25_lm1c_summer <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_temp, 
                      data = cal_data2_summer)
summary(i25_lm1c_summer)
par(mfrow=c(2,2))
plot(i25_lm1c_summer)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1c_summer$residuals)
rmse(i25_lm1c_summer$residuals)

#' Fit linear regression model with pm2.5 + pm2.5_sq
#' adjusted R2 = -0.20
i25_lm1d_summer <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_pm_sq, 
                    data = cal_data2_summer)
summary(i25_lm1d_summer)
par(mfrow=c(2,2))
plot(i25_lm1d_summer)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1d_summer$residuals)
rmse(i25_lm1d_summer$residuals)

#' -------------------------------------
#' Fall = Campaign3
#' -------------------------------------

cal_data2_fall <- filter(cal_data2, campaign == "Campaign3")
cor(cal_data2_fall$monitor_mean, cal_data2_fall$bc_ug_m3)
plot(cal_data2_fall$monitor_mean, cal_data2_fall$bc_ug_m3)

#' Fit linear regression model with temp
#' adjusted R2 = 0.31
i25_lm1_fall <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp, 
                     data = cal_data2_fall)
summary(i25_lm1_fall)
par(mfrow=c(2,2))
plot(i25_lm1_fall)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1_fall$residuals)
rmse(i25_lm1_fall$residuals)

#' Fit linear regression model with temp + temp_sq
#' adjusted R2 = 0.73
i25_lm1a_fall <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp + monitor_temp_sq, 
                      data = cal_data2_fall)
summary(i25_lm1a_fall)
par(mfrow=c(2,2))
plot(i25_lm1a_fall)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1a_fall$residuals)
rmse(i25_lm1a_fall$residuals)

#' Fit linear regression model with pm2.5
#' adjusted R2 = -0.11
i25_lm1b_fall <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm, 
                      data = cal_data2_fall)
summary(i25_lm1b_fall)
par(mfrow=c(2,2))
plot(i25_lm1b_fall)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1b_fall$residuals)
rmse(i25_lm1b_fall$residuals)

#' Fit linear regression model with pm2.5 + temp
#' adjusted R2 = 0.96
i25_lm1c_fall <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_temp, 
                      data = cal_data2_fall)
summary(i25_lm1c_fall)
par(mfrow=c(2,2))
plot(i25_lm1c_fall)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1c_fall$residuals)
rmse(i25_lm1c_fall$residuals)

#' Fit linear regression model with pm2.5 + pm2.5_sq
#' adjusted R2 = -0.65
i25_lm1d_fall <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_pm_sq, 
                      data = cal_data2_fall)
summary(i25_lm1d_fall)
par(mfrow=c(2,2))
plot(i25_lm1d_fall)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1d_fall$residuals)
rmse(i25_lm1d_fall$residuals)

#' -------------------------------------
#' Winter = Campaign4
#' -------------------------------------

cal_data2_winter <- filter(cal_data2, campaign == "Campaign4")
cor(cal_data2_winter$monitor_mean, cal_data2_winter$bc_ug_m3)
plot(cal_data2_winter$monitor_mean, cal_data2_winter$bc_ug_m3)

#' Fit linear regression model with temp
#' adjusted R2 = -0.28
i25_lm1_winter <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp, 
                   data = cal_data2_winter)
summary(i25_lm1_winter)
par(mfrow=c(2,2))
plot(i25_lm1_winter)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1_winter$residuals)
rmse(i25_lm1_winter$residuals)

#' Fit linear regression model with temp + temp_sq
#' adjusted R2 = -0.52
i25_lm1a_winter <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp + monitor_temp_sq, 
                    data = cal_data2_winter)
summary(i25_lm1a_winter)
par(mfrow=c(2,2))
plot(i25_lm1a_winter)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1a_winter$residuals)
rmse(i25_lm1a_winter$residuals)

#' Fit linear regression model with pm2.5
#' adjusted R2 = -0.33
i25_lm1b_winter <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm, 
                    data = cal_data2_winter)
summary(i25_lm1b_winter)
par(mfrow=c(2,2))
plot(i25_lm1b_winter)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1b_winter$residuals)
rmse(i25_lm1b_winter$residuals)

#' Fit linear regression model with pm2.5 + temp
#' adjusted R2 = 0.45
i25_lm1c_winter <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_temp, 
                    data = cal_data2_winter)
summary(i25_lm1c_winter)
par(mfrow=c(2,2))
plot(i25_lm1c_winter)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1c_winter$residuals)
rmse(i25_lm1c_winter$residuals)

#' Fit linear regression model with pm2.5 + pm2.5_sq
#' adjusted R2 = 0.77
i25_lm1d_winter <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_pm_sq, 
                      data = cal_data2_winter)
summary(i25_lm1d_winter)
par(mfrow=c(2,2))
plot(i25_lm1d_winter)
par(mfrow=c(1,1))

#' MAE and RMSE- Linear model
mae(i25_lm1d_winter$residuals)
rmse(i25_lm1d_winter$residuals)

#' -----------------------------------------------------------------------------
#' Choose I-25 models for each season
#' -----------------------------------------------------------------------------

#' -------------------------------------
#' Summer = Campaign2
#' -------------------------------------

summary(i25_lm1_summer) 
summary(i25_lm1a_summer) # Adjusted R2 = 0.985
summary(i25_lm1b_summer) 
summary(i25_lm1c_summer) 
summary(i25_lm1d_summer) 

AIC(i25_lm1_summer) 
AIC(i25_lm1a_summer) # AIC = -8.06
AIC(i25_lm1b_summer) 
AIC(i25_lm1c_summer) 
AIC(i25_lm1d_summer) 

rmse(i25_lm1_summer$residuals) 
rmse(i25_lm1a_summer$residuals) # RMSE = 0.04
rmse(i25_lm1b_summer$residuals) 
rmse(i25_lm1c_summer$residuals) 
rmse(i25_lm1d_summer$residuals) 

#' -------------------------------------
#' Fall = Campaign3
#' -------------------------------------

summary(i25_lm1_fall) 
summary(i25_lm1a_fall) 
summary(i25_lm1b_fall) 
summary(i25_lm1c_fall) # Adjusted R2 = 0.96
summary(i25_lm1d_fall) 

AIC(i25_lm1_fall) 
AIC(i25_lm1a_fall) 
AIC(i25_lm1b_fall) 
AIC(i25_lm1c_fall) # AIC = 0.38
AIC(i25_lm1d_fall) 

rmse(i25_lm1_fall$residuals) 
rmse(i25_lm1a_fall$residuals) 
rmse(i25_lm1b_fall$residuals) 
rmse(i25_lm1c_fall$residuals) # RMSE = 0.11
rmse(i25_lm1d_fall$residuals) 

#' -------------------------------------
#' Winter = Campaign4
#' -------------------------------------

summary(i25_lm1_winter) 
summary(i25_lm1a_winter) 
summary(i25_lm1b_winter) 
summary(i25_lm1c_winter) 
summary(i25_lm1d_winter) # Adjusted R2 = 0.77

AIC(i25_lm1_winter) 
AIC(i25_lm1a_winter) 
AIC(i25_lm1b_winter) 
AIC(i25_lm1c_winter) 
AIC(i25_lm1d_winter) # AIC = 1.44

rmse(i25_lm1_winter$residuals) 
rmse(i25_lm1a_winter$residuals) 
rmse(i25_lm1b_winter$residuals) 
rmse(i25_lm1c_winter$residuals) 
rmse(i25_lm1d_winter$residuals) # RMSE = 0.12

#' -----------------------------------------------------------------------------
#' Each season has different predictiors
#' Compare fully-adjusted linear models to Deming regression models
#' -----------------------------------------------------------------------------

#' -------------------------------------
#' Summer = Campaign2
#' Predictors: monitor_bc, monitor_temp, monitor_temp_sq
#' Linear model: R2 = 0.98, MAE = 0.04, RMSE = 0.04 
#' -------------------------------------

fit_adjlm_summer <- i25_lm1a_summer
summary(fit_adjlm_summer)

#' Coefficients for calibration equation
int_summer_adj <- unname(fit_adjlm_summer$coefficients[1])
beta_mon_summer_adj <- unname(fit_adjlm_summer$coefficients[2])
beta_temp_summer_adj <- unname(fit_adjlm_summer$coefficients[3])
beta_temp_sq_summer_adj <- unname(fit_adjlm_summer$coefficients[4])

#' Calibrating the filter data
fit_data_summer <- filter(cal_data2, campaign == "Campaign2") %>% 
  
  #' Calibrated BC (adjusted linear model)
  mutate(bc_ug_m3_adjlm = (bc_ug_m3 - int_summer_adj - (beta_temp_summer_adj*monitor_temp) - 
                           (beta_temp_sq_summer_adj*monitor_temp_sq)) / beta_mon_summer_adj) %>% 
  #' Calibrated BC (crude linear model) %>% 
  mutate(bc_ug_m3_lm = (bc_ug_m3 - lm_int_summer) / lm_slope_summer) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3 - dem_int_summer) / dem_slope_summer) 

#' MAE and RMSE- Adjusted linear model
mae(fit_adjlm_summer$residuals)
rmse(fit_adjlm_summer$residuals)

#' MAE and RMSE- Crude linear
mae(i25_lm_summer$residuals)
rmse(i25_lm_summer$residuals)

#' MAE and RMSE- Deming
mae(i25_dem_summer$residuals)
rmse(i25_dem_summer$residuals)

summary(fit_data_summer$bc_ug_m3)
summary(fit_data_summer$bc_ug_m3_adjlm)
summary(fit_data_summer$bc_ug_m3_lm)
summary(fit_data_summer$bc_ug_m3_dem)
summary(fit_data_summer$monitor_mean)

ggplot(fit_data_summer) +
  geom_density(aes(x = bc_ug_m3, color = "Raw")) +
  geom_density(aes(x = bc_ug_m3_adjlm, color = "Adjusted Linear model")) +
  geom_density(aes(x = bc_ug_m3_lm, color = "Crude Linear model")) +
  geom_density(aes(x = bc_ug_m3_dem, color = "Deming model")) +
  geom_density(aes(x = monitor_mean, color = "Monitor")) +
  xlab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name = "Data type", discrete = T) +
  simple_theme

#' -------------------------------------
#' Fall = Campaign3
#' Predictors: monitor_bc, monitor_pm, monitor_temp
#' R2 = 0.96, MAE = 0.09, RMSE = 0.11 
#' -------------------------------------

fit_adjlm_fall <- i25_lm1c_fall
summary(fit_adjlm_fall)

#' Coefficients for calibration equation
int_fall_adj <- unname(fit_adjlm_fall$coefficients[1])
beta_mon_fall_adj <- unname(fit_adjlm_fall$coefficients[2])
beta_pm_fall_adj <- unname(fit_adjlm_fall$coefficients[3])
beta_temp_fall_adj <- unname(fit_adjlm_fall$coefficients[4])

#' Calibrating the filter data
fit_data_fall <- filter(cal_data2, campaign == "Campaign3") %>% 
  #' Calibrated BC (linear model)
  mutate(bc_ug_m3_adjlm = (bc_ug_m3 - int_fall_adj - (beta_pm_fall_adj*monitor_pm) - 
                           (beta_temp_fall_adj*monitor_temp)) / beta_mon_fall_adj) %>% 
  #' Calibrated BC (crude linear model) %>% 
  mutate(bc_ug_m3_lm = (bc_ug_m3 - lm_int_fall) / lm_slope_fall) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3 - dem_int_fall) / dem_slope_fall) 

#' MAE and RMSE- Adjusted linear model
mae(fit_adjlm_fall$residuals)
rmse(fit_adjlm_fall$residuals)

#' MAE and RMSE- Crude linear
mae(i25_lm_fall$residuals)
rmse(i25_lm_fall$residuals)

#' MAE and RMSE- Deming
mae(i25_dem_fall$residuals)
rmse(i25_dem_fall$residuals)

summary(fit_data_fall$bc_ug_m3)
summary(fit_data_fall$bc_ug_m3_adjlm)
summary(fit_data_fall$bc_ug_m3_lm)
summary(fit_data_fall$bc_ug_m3_dem)
summary(fit_data_fall$monitor_mean)

ggplot(fit_data_fall) +
  geom_density(aes(x = bc_ug_m3, color = "Raw")) +
  geom_density(aes(x = bc_ug_m3_adjlm, color = "Adjusted Linear model")) +
  geom_density(aes(x = bc_ug_m3_lm, color = "Crude Linear model")) +
  geom_density(aes(x = bc_ug_m3_dem, color = "Deming model")) +
  geom_density(aes(x = monitor_mean, color = "Monitor")) +
  xlab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name = "Data type", discrete = T) +
  simple_theme

#' -------------------------------------
#' Winter = Campaign4
#' Predictors: monitor_bc, monitor_pm, monitor_pm_sq
#' R2 = 0.77, MAE = 0.11, RMSE = 0.12 
#' -------------------------------------

fit_adjlm_winter <- i25_lm1d_winter
summary(fit_adjlm_winter)

#' Coefficients for calibration equation
int_winter_adj <- unname(fit_adjlm_winter$coefficients[1])
beta_mon_winter_adj <- unname(fit_adjlm_winter$coefficients[2])
beta_pm_winter_adj <- unname(fit_adjlm_winter$coefficients[3])
beta_pm_sq_winter_adj <- unname(fit_adjlm_winter$coefficients[4])

#' Calibrating the filter data
fit_data_winter <- filter(cal_data2, campaign == "Campaign4") %>% 
  #' Calibrated BC (linear model)
  mutate(bc_ug_m3_adjlm = (bc_ug_m3 - int_winter_adj - (beta_pm_winter_adj*monitor_pm) - 
                           (beta_pm_sq_winter_adj*monitor_pm_sq)) / beta_mon_winter_adj) %>% 
  #' Calibrated BC (crude linear model) %>% 
  mutate(bc_ug_m3_lm = (bc_ug_m3 - lm_int_winter) / lm_slope_winter) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3 - dem_int_winter) / dem_slope_winter) 

#' MAE and RMSE- Adjusted linear model
mae(fit_adjlm_winter$residuals)
rmse(fit_adjlm_winter$residuals)

#' MAE and RMSE- Crude linear
mae(i25_lm_winter$residuals)
rmse(i25_lm_winter$residuals)

#' MAE and RMSE- Deming
mae(i25_dem_winter$residuals)
rmse(i25_dem_winter$residuals)

summary(fit_data_winter$bc_ug_m3)
summary(fit_data_winter$bc_ug_m3_adjlm)
summary(fit_data_winter$bc_ug_m3_lm)
summary(fit_data_winter$bc_ug_m3_dem)
summary(fit_data_winter$monitor_mean)

ggplot(fit_data_winter) +
  geom_density(aes(x = bc_ug_m3, color = "Raw")) +
  geom_density(aes(x = bc_ug_m3_adjlm, color = "Adjusted Linear model")) +
  geom_density(aes(x = bc_ug_m3_lm, color = "Crude Linear model")) +
  geom_density(aes(x = bc_ug_m3_dem, color = "Deming model")) +
  geom_density(aes(x = monitor_mean, color = "Monitor")) +
  xlab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name = "Data type", discrete = T) +
  simple_theme

#' -----------------------------------------------------------------------------
#' Calibrate the filter data using the adjusted linear regression equations and
#' the deming regression results
#' 
#' First, use the deming and linear regression models for the full data set
#' Second, use the season-specific models
#' 
#' In order to do this, I need to figure out which season's model I should use
#' for the first campaign (spring) because we weren't able to co-locate at this
#' site. 
#' 
#' To do this, I'm going to compare the distribution of all filters for each 
#' campaign and see which calibration curve makes the most sense. 
#' I'm open to other suggestions, of course!
#' -----------------------------------------------------------------------------

fit_monitor <- "080310027"

filter_data2 <- filter_data %>%
  filter(indoor == 0) %>%
  filter(is_blank == 0) %>%
  filter(!is.na(bc_ug_m3)) %>%
  filter(low_volume_flag == 0) %>% 
  mutate(monitor_id = fit_monitor)
summary(filter_data2)

nrow(filter_data2)
glimpse(filter_data2)

#' Assign start and end BC, TEMP and PM2.5 value from the monitoring data
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
  
  #' Get mean PM2.5 from the monitor for the period covered by the filter
  pm <- monitor_pm %>% 
    filter(monitor_id == paired_monitor_id) %>% 
    filter(Date_Local %in% dates)
  mean_pm <- mean(pm$Arithmetic_Mean, na.rm=T)
  
  filter_data2[i, "monitor_pm"] <- mean_pm
  filter_data2[i, "monitor_pm_n_obs"] <- nrow(pm)
}

#' Add the needed squared values for the fully adjusted models
filter_data2 <- filter_data2 %>% 
  mutate(monitor_temp_sq = monitor_temp**2,
         monitor_pm_sq = monitor_pm**2) %>% 
  
  # Indicator variables for campaigns
  mutate(camp2 = ifelse(campaign == "Campaign2", 1, 0),
         camp3 = ifelse(campaign == "Campaign3", 1, 0),
         camp4 = ifelse(campaign == "Campaign4", 1, 0)) 

#' -------------------------------------
#' All campaigns
#' -------------------------------------

filters_all <- filter_data2 %>% 
  #' Calibrated BC (Crude linear model)
  mutate(bc_ug_m3_lm = (bc_ug_m3 - lm_int) / lm_slope) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3 - dem_int) / dem_slope)

summary(filters_all$bc_ug_m3)
summary(filters_all$bc_ug_m3_lm)
summary(filters_all$bc_ug_m3_dem)
summary(filters_all$monitor_mean)

filter_all_comp <- filters_all %>% 
  dplyr::select(filter_id, campaign, bc_ug_m3, bc_ug_m3_lm, bc_ug_m3_dem, monitor_mean) %>% 
  gather(key = type, value = bc, -filter_id, -campaign)

ggplot(filter_all_comp, aes(x = as.factor(type), y = bc, fill = as.factor(type))) +
  ggtitle("Distribution of measured and calibrated BC: All filters") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "AE-33")) +
  xlab("Data Type") + ylab("BC (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.8, 0.8)) +
  simple_theme

#' -------------------------------------
#' Summer = Campaign2
#' Predictors: monitor_bc, monitor_temp, monitor_temp_sq
#' R2 = 0.98, MAE = 0.04, RMSE = 0.04 
#' -------------------------------------

filters_summer <- filter(filter_data2, campaign == "Campaign2") %>% 
  #' Calibrated BC (linear model)
  mutate(bc_ug_m3_adjlm = (bc_ug_m3 - int_summer_adj - (beta_temp_summer_adj*monitor_temp) - 
                           (beta_temp_sq_summer_adj*monitor_temp_sq)) / beta_mon_summer_adj) %>% 
  #' Calibrated BC (Crude linear model)
  mutate(bc_ug_m3_lm = (bc_ug_m3 - lm_int_summer) / lm_slope_summer) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3 - dem_int_summer) / dem_slope_summer)

summary(filters_summer$bc_ug_m3)
summary(filters_summer$bc_ug_m3_adjlm)
summary(filters_summer$bc_ug_m3_lm)
summary(filters_summer$bc_ug_m3_dem)
summary(filters_summer$monitor_mean)

filter_summer_comp <- filters_summer %>% 
  dplyr::select(filter_id, campaign, bc_ug_m3, bc_ug_m3_lm, bc_ug_m3_dem, monitor_mean) %>% 
  gather(key = type, value = bc, -filter_id, -campaign)

ggplot(filter_summer_comp, aes(x = as.factor(type), y = bc, fill = as.factor(type))) +
  ggtitle("Distribution of measured and calibrated BC: Summer") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "AE-33")) +
  xlab("Data Type") + ylab("BC (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.8, 0.8)) +
  simple_theme

#' -------------------------------------
#' Fall = Campaign3
#' Predictors: monitor_bc, monitor_pm, monitor_temp
#' R2 = 0.96, MAE = 0.09, RMSE = 0.11 
#' -------------------------------------

filters_fall <- filter(filter_data2, campaign == "Campaign3") %>% 
  #' Calibrated BC (linear model)
  mutate(bc_ug_m3_adjlm = (bc_ug_m3 - int_fall_adj - (beta_pm_fall_adj*monitor_pm) - 
                           (beta_temp_fall_adj*monitor_temp)) / beta_mon_fall_adj) %>% 
  #' Calibrated BC (Crude linear model)
  mutate(bc_ug_m3_lm = (bc_ug_m3 - lm_int_fall) / lm_slope_fall) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3 - dem_int_fall) / dem_slope_fall)

summary(filters_fall$bc_ug_m3)
summary(filters_fall$bc_ug_m3_adjlm)
summary(filters_fall$bc_ug_m3_lm)
summary(filters_fall$bc_ug_m3_dem)
summary(filters_fall$monitor_mean)

filter_fall_comp <- filters_fall %>% 
  dplyr::select(filter_id, campaign, bc_ug_m3, bc_ug_m3_lm, bc_ug_m3_dem, monitor_mean) %>% 
  gather(key = type, value = bc, -filter_id, -campaign)

ggplot(filter_fall_comp, aes(x = as.factor(type), y = bc, fill = as.factor(type))) +
  ggtitle("Distribution of measured and calibrated BC: fall") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "AE-33")) +
  xlab("Data Type") + ylab("BC (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.8, 0.8)) +
  simple_theme

#' -------------------------------------
#' Winter = Campaign4
#' Predictors: monitor_bc, monitor_pm, monitor_pm_sq
#' R2 = 0.77, MAE = 0.11, RMSE = 0.12 
#' -------------------------------------

filters_winter <- filter(filter_data2, campaign == "Campaign4") %>% 
  #' Calibrated BC (linear model)
  mutate(bc_ug_m3_adjlm = (bc_ug_m3 - int_winter_adj - (beta_pm_winter_adj*monitor_pm) - 
                           (beta_pm_sq_winter_adj*monitor_pm_sq)) / beta_mon_winter_adj) %>% 
  #' Calibrated BC (Crude linear model)
  mutate(bc_ug_m3_lm = (bc_ug_m3 - lm_int_winter) / lm_slope_winter) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3 - dem_int_winter) / dem_slope_winter)

summary(filters_winter$bc_ug_m3)
summary(filters_winter$bc_ug_m3_adjlm)
summary(filters_winter$bc_ug_m3_lm)
summary(filters_winter$bc_ug_m3_dem)
summary(filters_winter$monitor_mean)

filter_winter_comp <- filters_winter %>% 
  dplyr::select(filter_id, campaign, bc_ug_m3, bc_ug_m3_lm, bc_ug_m3_dem, monitor_mean) %>% 
  gather(key = type, value = bc, -filter_id, -campaign)

ggplot(filter_winter_comp, aes(x = as.factor(type), y = bc, fill = as.factor(type))) +
  ggtitle("Distribution of measured and calibrated BC: winter") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "AE-33")) +
  xlab("Data Type") + ylab("BC (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.8, 0.8)) +
  simple_theme

#' -------------------------------------
#' Spring = Campaign1
#' Remember, spring has the issue of a LOT of negative BC values
#' 
#' Spring has similar temperatures to summer but similar PM2.5 and BC 
#' concentrations as fall (based on the histograms)
#' 
#' I'm going to go with the summer calibration model for two reasons:
#' 1) Temperature was an important predictor in the adjusted LM model, and summer
#' and spring had similar temperature distributions
#' 2) The summer calibration model resulted in the lowest median BC, reflective
#' of lower concentrations in the spring
#' -------------------------------------

filters_spring <- filter(filter_data2, campaign == "Campaign1")
summary(filters_spring$bc_ug_m3)
summary(filters_summer$bc_ug_m3)
summary(filters_fall$bc_ug_m3)
summary(filters_winter$bc_ug_m3)

#' Histograms of UPAS BC
ggplot(filter_data2) +
  geom_histogram(aes(x = bc_ug_m3, color = campaign, fill = campaign)) +
  facet_grid(campaign ~ .) +
  xlab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name= "Campaign", discrete = T) +
  scale_fill_viridis(name= "Campaign", discrete = T) +
  simple_theme

#' Histograms of monitor data
ggplot(filter_data2) +
  geom_histogram(aes(x = monitor_mean, color = campaign, fill = campaign)) +
  facet_grid(campaign ~ .) +
  xlab("Time-weighted average AE-33 BC (\u03bcg/m\u00b3)") +
  scale_color_viridis(name= "Campaign", discrete = T) +
  scale_fill_viridis(name= "Campaign", discrete = T) +
  simple_theme

#' Histograms of monitor temp
ggplot(filter_data2) +
  geom_histogram(aes(x = monitor_temp, color = campaign, fill = campaign)) +
  facet_grid(campaign ~ .) +
  xlab("Time-weighted average temperature (F)") +
  scale_color_viridis(name= "Campaign", discrete = T) +
  scale_fill_viridis(name= "Campaign", discrete = T) +
  simple_theme

#' Time series of monitor PM
ggplot(filter_data2) +
  geom_histogram(aes(x = monitor_pm, color = campaign, fill = campaign)) +
  facet_grid(campaign ~ .) +
  xlab("Time-weighted average PM\u2082.\u2085 (\u03bcg/m\u00b3)") +
  scale_color_viridis(name= "Campaign", discrete = T) +
  scale_fill_viridis(name= "Campaign", discrete = T) +
  simple_theme
  
#' Try the fall models first
filters_spring_fall <- filter(filter_data2, campaign == "Campaign1") %>% 
  #' Calibrated BC (linear model)
  mutate(bc_ug_m3_adjlm = (bc_ug_m3 - int_fall_adj - (beta_pm_fall_adj*monitor_pm) - 
                           (beta_temp_fall_adj*monitor_temp)) / beta_mon_fall_adj) %>% 
  #' Calibrated BC (crude linear model) 
  mutate(bc_ug_m3_lm = (bc_ug_m3 - lm_int_fall) / lm_slope_fall) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3 - dem_int_fall) / dem_slope_fall)

summary(filters_spring_fall$bc_ug_m3)
summary(filters_spring_fall$bc_ug_m3_adjlm)
summary(filters_spring_fall$bc_ug_m3_lm)
summary(filters_spring_fall$bc_ug_m3_dem)
summary(filters_spring_fall$monitor_mean)

filter_spring_fall_comp <- filters_spring_fall %>% 
  dplyr::select(filter_id, campaign, bc_ug_m3, bc_ug_m3_lm, bc_ug_m3_dem, monitor_mean) %>% 
  gather(key = type, value = bc, -filter_id, -campaign)

ggplot(filter_spring_fall_comp, aes(x = as.factor(type), y = bc, fill = as.factor(type))) +
  ggtitle("Distribution of measured and calibrated BC: spring w/ fall calibration") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "AE-33")) +
  xlab("Data Type") + ylab("BC (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.8, 0.8)) +
  simple_theme

#' Next, try the summer model
filters_spring_summer <- filter(filter_data2, campaign == "Campaign1") %>% 
  #' Calibrated BC (linear model)
  mutate(bc_ug_m3_adjlm = (bc_ug_m3 - int_summer_adj - (beta_temp_summer_adj*monitor_temp) - 
                           (beta_temp_sq_summer_adj*monitor_temp_sq)) / beta_mon_summer_adj) %>% 
  #' Calibrated BC (crude linear model) 
  mutate(bc_ug_m3_lm = (bc_ug_m3 - lm_int_summer) / lm_slope_summer) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3 - dem_int_summer) / dem_slope_summer)

summary(filters_spring_summer$bc_ug_m3)
summary(filters_spring_summer$bc_ug_m3_adjlm)
summary(filters_spring_summer$bc_ug_m3_lm)
summary(filters_spring_summer$bc_ug_m3_dem)
summary(filters_spring_summer$monitor_mean)

filter_spring_summer_comp <- filters_spring_summer %>% 
  dplyr::select(filter_id, campaign, bc_ug_m3, bc_ug_m3_lm, bc_ug_m3_dem, monitor_mean) %>% 
  gather(key = type, value = bc, -filter_id, -campaign)

ggplot(filter_spring_summer_comp, aes(x = as.factor(type), y = bc, fill = as.factor(type))) +
  ggtitle("Distribution of measured and calibrated BC: spring w/ summer calibration") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "AE-33")) +
  xlab("Data Type") + ylab("BC (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.8, 0.8)) +
  simple_theme

#' Check the winter model as well just in case
filters_spring_winter <- filter(filter_data2, campaign == "Campaign4") %>% 
  #' Calibrated PM (linear model)
  mutate(bc_ug_m3_adjlm = (bc_ug_m3 - int_winter_adj - (beta_pm_winter_adj*monitor_pm) - 
                           (beta_pm_sq_winter_adj*monitor_pm_sq)) / beta_mon_winter_adj) %>%    
  #' Calibrated BC (crude linear model) 
  mutate(bc_ug_m3_lm = (bc_ug_m3 - lm_int_winter) / lm_slope_winter) %>% 
  #' Calibrated BC (Deming model) 
  mutate(bc_ug_m3_dem = (bc_ug_m3 - dem_int_winter) / dem_slope_winter)

summary(filters_spring_winter$bc_ug_m3)
summary(filters_spring_winter$bc_ug_m3_adjlm)
summary(filters_spring_winter$bc_ug_m3_lm)
summary(filters_spring_winter$bc_ug_m3_dem)
summary(filters_spring_winter$monitor_mean)

filter_spring_winter_comp <- filters_spring_winter %>% 
  dplyr::select(filter_id, campaign, bc_ug_m3, bc_ug_m3_lm, bc_ug_m3_dem, monitor_mean) %>% 
  gather(key = type, value = bc, -filter_id, -campaign)

ggplot(filter_spring_winter_comp, aes(x = as.factor(type), y = bc, fill = as.factor(type))) +
  ggtitle("Distribution of measured and calibrated BC: spring w/ winter calibration") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "AE-33")) +
  xlab("Data Type") + ylab("BC (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.8, 0.8)) +
  simple_theme

#' Choose a spring campaign model: should see lowest concentrations
#' Going with the summer model
summary(filters_spring_summer$bc_ug_m3_dem)
summary(filters_spring_fall$bc_ug_m3_dem)
summary(filters_spring_winter$bc_ug_m3_dem)

#' -----------------------------------------------------------------------------
#' Combine the data sets
#' -----------------------------------------------------------------------------

filter_data3 <- bind_rows(filters_spring_summer, filters_summer, 
                          filters_fall, filters_winter)

names(filter_data3)

filter_data_comp <- filter_data3 %>% 
  dplyr::select(filter_id, month, season, bc_ug_m3, 
         bc_ug_m3_lm, bc_ug_m3_dem, monitor_mean) %>% 
  gather(key = type, value = bc, -filter_id, -month, -season)

ggplot(filter_data_comp, aes(x = as.factor(month), y = bc, fill = as.factor(type))) +
  ggtitle("Monthly distribution of measured and calibrated BC") +
  geom_boxplot(alpha = 0.75) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "EPA Monitors")) +
  xlab("Month") + ylab("BC (\u03bcg/m\u00b3)") +
  scale_y_continuous(limits = c(-5, 10)) +
  theme(legend.position = "bottom") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Monthly_Means_BC.jpeg"),
       height = 4, width = 6, dpi = 500, units = "in", device = "jpeg")

ggplot(filter(filter_data_comp, !(type %in% c("bc_ug_m3_lm"))), 
       aes(x = as.factor(month), y = bc, fill = as.factor(type))) +
  ggtitle("Monthly distribution of measured and calibrated BC") +
  geom_boxplot(alpha = 0.75) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "EPA Monitors")) +
  xlab("Month") + ylab("BC (\u03bcg/m\u00b3)") +
  scale_y_continuous(limits = c(-2, 5)) +
  theme(legend.position = "bottom") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Monthly_Means_BC_No_LM.jpeg"),
       height = 4, width = 6, dpi = 500, units = "in", device = "jpeg")

ggplot(filter_data_comp, aes(x = as.factor(season), y = bc, fill = as.factor(type))) +
  ggtitle("Seasonal distribution of measured and calibrated BC") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "EPA Monitors")) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
  xlab("Season") + ylab("BC (\u03bcg/m\u00b3)") +
  scale_x_discrete(labels = c("1" = "Winter", "2" = "Spring",
                              "3" = "Summer", "4" = "Fall")) +
  scale_y_continuous(limits = c(-5, 10)) +
  xlab("Season") + ylab("BC (\u03bcg/m\u00b3)") +
  theme(legend.position = "bottom") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Seasonal_Means_BC.jpeg"),
       height = 4, width = 6, dpi = 500, units = "in", device = "jpeg")

ggplot(filter(filter_data_comp, !(type %in% c("bc_ug_m3_adjlm", "bc_ug_m3_lm"))), 
       aes(x = as.factor(season), y = bc, fill = as.factor(type))) +
  ggtitle("Seasonal distribution of measured and calibrated BC") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_lm" = "Crude linear model",
                                "bc_ug_m3_dem" = "Deming model",
                                "monitor_mean" = "EPA Monitors")) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
  xlab("Season") + ylab("BC (\u03bcg/m\u00b3)") +
  scale_x_discrete(labels = c("1" = "Winter", "2" = "Spring",
                              "3" = "Summer", "4" = "Fall")) +
  scale_y_continuous(limits = c(-2, 5)) +
  xlab("Season") + ylab("BC (\u03bcg/m\u00b3)") +
  theme(legend.position = "bottom") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Seasonal_Means_BC_NoLM.jpeg"),
       height = 4, width = 6, dpi = 500, units = "in", device = "jpeg")

#' Write out the calibrated dataset
glimpse(filter_data3)
write_csv(filter_data3, here::here("Data", "Filter_BC_Calibrated.csv"))

#' Save the calibration models
#' Also save a date-stamped version in case we need to go back
today <- Sys.Date()
save(i25_lm, i25_lm_summer, i25_lm_fall, i25_lm_winter,
     i25_dem, i25_dem_summer, i25_dem_fall, i25_dem_winter,
     fit_adjlm_summer, fit_adjlm_fall, fit_adjlm_winter,
     cal_data,
     file = here::here("Results", "Filter_BC_CalModels.rdata"))

save(i25_lm, i25_lm_summer, i25_lm_fall, i25_lm_winter,
     i25_dem, i25_dem_summer, i25_dem_fall, i25_dem_winter,
     fit_adjlm_summer, fit_adjlm_fall, fit_adjlm_winter,
     cal_data,
     file = here::here("Results/Archived_Results", 
                       paste0("Filter_BC_CalModels_", today, ".rdata")))

load(here::here("Results", "Filter_BC_CalModels.rdata"))

cor(i25_dem_summer$model$bc_ug_m3, i25_dem_summer$model$monitor_mean)
cor(i25_dem_fall$model$bc_ug_m3, i25_dem_fall$model$monitor_mean)
cor(i25_dem_winter$model$bc_ug_m3, i25_dem_winter$model$monitor_mean)


#' -----------------------------------------------------------------------------
#' Plots for papers
#' -----------------------------------------------------------------------------

load(here::here("Results", "Filter_BC_CalModels.rdata"))

#' Plot BC monitor vs UPAS by campaign
ggplot() +
  geom_point(data = filter(cal_data, campaign == "Campaign2"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  geom_abline(data = filter(cal_data, campaign == "Campaign2"),
              aes(slope = lm_slope_summer, intercept = lm_int_summer,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = filter(cal_data, campaign == "Campaign2"),
              aes(slope = dem_slope_summer, intercept = dem_int_summer,
                  color = campaign, linetype = "dem"), size = 1) +

  geom_point(data = filter(cal_data, campaign == "Campaign3"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  geom_abline(data = filter(cal_data, campaign == "Campaign3"),
              aes(slope = lm_slope_fall, intercept = lm_int_fall,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = filter(cal_data, campaign == "Campaign3"),
              aes(slope = dem_slope_fall, intercept = dem_int_fall,
                  color = campaign, linetype = "dem"), size = 1) +

  geom_point(data = filter(cal_data, campaign == "Campaign4"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  geom_abline(data = filter(cal_data, campaign == "Campaign4"),
              aes(slope = lm_slope_winter, intercept = lm_int_winter,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = filter(cal_data, campaign == "Campaign4"),
              aes(slope = dem_slope_winter, intercept = dem_int_winter,
                  color = campaign, linetype = "dem"), size = 1) +

  scale_color_viridis(name = "Season", discrete = T,
                      labels = c("Campaign2" = "Summer",
                                 "Campaign3" = "Fall",
                                 "Campaign4" = "Winter")) +
  scale_linetype_manual(name = "Regression", 
                        values = c("lm" = 1, "dem" = 2),
                        labels = c("lm" = "OLS", "dem" = "Deming")) +
  facet_grid(campaign ~ ., scales = "free") +
  # facet_grid(. ~ campaign, scales = "free") +
  ylab("UPAS BC (\u03bcg/m\u00b3)") + xlab("Monitor BC (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_LM_Dem_by_Campaign.jpeg"),
       height = 5, width = 7, dpi = 500, units = "in", device = "jpeg")

#' Plot BC monitor vs UPAS by campaign (drop campaign 4)
ggplot() +
  geom_point(data = filter(cal_data, campaign == "Campaign2"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  geom_abline(data = filter(cal_data, campaign == "Campaign2"),
              aes(slope = lm_slope_summer, intercept = lm_int_summer,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = filter(cal_data, campaign == "Campaign2"),
              aes(slope = dem_slope_summer, intercept = dem_int_summer,
                  color = campaign, linetype = "dem"), size = 1) +
  
  geom_point(data = filter(cal_data, campaign == "Campaign3"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  geom_abline(data = filter(cal_data, campaign == "Campaign3"),
              aes(slope = lm_slope_fall, intercept = lm_int_fall,
                  color = campaign, linetype = "lm"), size = 1) +
  geom_abline(data = filter(cal_data, campaign == "Campaign3"),
              aes(slope = dem_slope_fall, intercept = dem_int_fall,
                  color = campaign, linetype = "dem"), size = 1) +
  
  # geom_point(data = filter(cal_data, campaign == "Campaign4"),
  #            aes(x = monitor_mean, y = bc_ug_m3, color = campaign), size = 2) +
  # geom_abline(data = filter(cal_data, campaign == "Campaign4"),
  #             aes(slope = lm_slope_winter, intercept = lm_int_winter,
  #                 color = campaign, linetype = "lm"), size = 1) +
  # geom_abline(data = filter(cal_data, campaign == "Campaign4"),
  #             aes(slope = dem_slope_winter, intercept = dem_int_winter,
  #                 color = campaign, linetype = "dem"), size = 1) +
  
  scale_color_viridis(name = "Season", discrete = T,
                      labels = c("Campaign2" = "Late Summer",
                                 "Campaign3" = "Fall")) +
  scale_linetype_manual(name = "Regression", 
                        values = c("lm" = 1, "dem" = 2),
                        labels = c("lm" = "OLS", "dem" = "Deming")) +
  facet_grid(campaign ~ ., scales = "free") +
  # facet_grid(. ~ campaign, scales = "free") +
  ylab("UPAS BC (\u03bcg/m\u00b3)") + xlab("Monitor BC (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_LM_Dem_by_Campaigns23.jpeg"),
       height = 5, width = 7, dpi = 500, units = "in", device = "jpeg")

#' Bland-Altman plots 
library(BlandAltmanLeh)
library(ggExtra)

mean_diff_all <- mean(cal_data2$bc_ug_m3 - cal_data2$monitor_mean)
ba_plot_all <- bland.altman.plot(cal_data2$bc_ug_m3, cal_data2$monitor_mean, 
                             graph.sys="ggplot2")
ba_all <- print(ba_plot_all) +
  geom_hline(aes(yintercept = mean_diff_all), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 1) +
  xlab(NULL) + ylab(NULL) +
  simple_theme2
ba_all

mean_diff_summer <- mean(cal_data2_summer$bc_ug_m3 - cal_data2_summer$monitor_mean)
ba_plot_summer <- bland.altman.plot(cal_data2_summer$bc_ug_m3, cal_data2_summer$monitor_mean, 
                                 graph.sys="ggplot2")
ba_summer <- print(ba_plot_summer) +
  geom_hline(aes(yintercept = mean_diff_summer), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  xlab(NULL) + ylab(NULL) +
  simple_theme2
ba_summer

mean_diff_fall <- mean(cal_data2_fall$bc_ug_m3 - cal_data2_fall$monitor_mean)
ba_plot_fall <- bland.altman.plot(cal_data2_fall$bc_ug_m3, cal_data2_fall$monitor_mean, 
                                    graph.sys="ggplot2")
ba_fall <- print(ba_plot_fall) +
  geom_hline(aes(yintercept = mean_diff_fall), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  xlab(NULL) + ylab(NULL) +
  simple_theme2
ba_fall

mean_diff_winter <- mean(cal_data2_winter$bc_ug_m3 - cal_data2_winter$monitor_mean)
ba_plot_winter <- bland.altman.plot(cal_data2_winter$bc_ug_m3, cal_data2_winter$monitor_mean, 
                                  graph.sys="ggplot2")
ba_winter <- print(ba_plot_winter) +
  geom_hline(aes(yintercept = mean_diff_winter), size = 1) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  xlab(NULL) + ylab(NULL) +
  simple_theme2
ba_winter

library(ggpubr)

# ba_plots <- ggarrange(
#   annotate_figure(ggarrange(ba_all, ba_summer, ba_fall, ba_winter,
#                             labels = "AUTO", 
#                             ncol = 2, nrow = 2, align = "hv"),
#                   left = text_grob("Difference: UPAS - AE-33 (\u03bcg/m\u00b3)", 
#                                    rot = 90, face = "bold"),
#                   bottom = text_grob("Mean of UPAS and AE-33 BC Measurement (\u03bcg/m\u00b3)",
#                                      face = "bold"))
#   )

# ba_plots
# ggsave(ba_plots,
#        filename = here::here("Figs/Calibration", "BA_Plots_Combined.jpeg"),
#        height = 7, width = 7, dpi = 500, units = "in", device = "jpeg")

ba_plots2<- annotate_figure(ggarrange(ba_summer, ba_fall,
                            labels = c("A) Late summer", "B) Fall"), 
                            ncol = 2, nrow = 1, label.y = 0.9),
                            left = text_grob("Difference: UPAS - AE-33 (\u03bcg/m\u00b3)", 
                                   rot = 90, face = "bold"),
                            bottom = text_grob("Mean of UPAS and AE-33 BC Measurement (\u03bcg/m\u00b3)",
                                   face = "bold"))
                      
ba_plots2
ggsave(ba_plots2,
       filename = here::here("Figs/Calibration", "BA_Plots_Camps23.jpeg"),
       height = 4, width = 9, dpi = 500, units = "in", device = "jpeg")
