#' =============================================================================
#' Project: ECHO LUR
#' Date created: July 23, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script calibrates the BC transmissometer data using montitoring data from
#' the I-25 Denver (Yuma St.) site
#' 
#' UPDATE 10.09.19: Based on feedback from JV, swapped the monitor and upas 
#' measurements in the OLS models
#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggthemes)
library(ggspatial)
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

#' -----------------------------------------------------------------------------
#' Read in the filter BC data and monitor data
#' Read in list of collocated monitors
#' -----------------------------------------------------------------------------

filter_data <- read_csv(here::here("Data", "Filter_BC.csv")) %>% 
  filter(!is.na(EndDateLocal)) %>% 
  arrange(EndDateLocal)
monitor_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv"))
monitor_temp <- read_csv(here::here("Data", "Monitor_TEMP_Data_AEA.csv"))
monitor_smoke <- read_csv(here::here("Data", "Monitor_Smoke_Days_AEA.csv"))
monitor_pm <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv"))

collocated_ids <- read_csv(here::here("Data", "Collocation_Filter_IDs.csv"))

#' Collocated monitors:
#' National Jewish (080310013) 1400 Jackson St, Denver, CO 80206: PM2.5
#' I-25 Denver (080310027) 9th and Yuma: PM2.5, Black Carbon, Temp
#' I-25 Globeville (080310028) 49th and Acoma: PM2.5, Temp, RH, Wind

#' Because only the I-25 site has BC data, just use those filters 
collocated_ids <- collocated_ids %>% 
  mutate(monitor_id = ifelse(Location == "1400 Jackson St, Denver, CO 80206", "080310013",
                             ifelse(Location == "9th and Yuma", "080310027",
                                    "080310028"))) %>% 
  filter(monitor_id == "080310027")

cal_data <- filter(filter_data, filter_id %in% collocated_ids$filter_id) %>% 
  select(filter_id, StartDateTimeLocal, EndDateTimeLocal, is_blank, 
         bc_ug_m3, bc_ug_m3_uncorrected, campaign) %>% 
  left_join(collocated_ids, by = "filter_id") %>% 
  filter(is_blank == 0) %>% 
  filter(!is.na(bc_ug_m3)) %>% 
  filter(filter_id %in% collocated_ids$filter_id)

summary(cal_data)

ggplot(cal_data) +
  geom_line(aes(x = StartDateTimeLocal, y = bc_ug_m3, color = campaign),
             show.legend = T) +
  geom_smooth(aes(x = StartDateTimeLocal, y = bc_ug_m3), method = "loess") +
  scale_color_viridis(name = "Campaign", discrete = T) +
  xlab("Date") + ylab("Time-weighted average UPAS BC (\u03bcg/m\u00b3)") +
  scale_x_date(labels = date_format("%m-%Y")) +
  theme(legend.position = c(0.8, 0.8)) +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_UPAS_TS.jpeg"),
       height = 5, width = 5, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Plot time series for each monitor
#' Want to get a sense of the trends for the monitoring data
#' -----------------------------------------------------------------------------

start <- filter_data$StartDateLocal[1]
end <- filter_data$EndDateLocal[length(filter_data$EndDateLocal)]

date_seq <- seq.Date(start, end, by = "day")

co_mon <- filter(monitor_data, monitor_id %in% c("080310027")) %>% 
  filter(Date_Local %in% date_seq) %>% 
  mutate(month = month(Date_Local)) %>% 
  mutate(season = ifelse(month %in% c(12, 1, 2), "winter", 
                         ifelse(month %in% c(3, 4, 5), "spring",
                                ifelse(month %in% c(6, 7, 8), "summer", "fall"))))

ggplot(co_mon, aes(x = Date_Local, y = Arithmetic_Mean)) +
  geom_line(aes(color = season)) +
  geom_smooth(method = "loess") +
  scale_color_viridis(name = "Season", discrete = T) +
  xlab("Date") + ylab("Daily mean monitor BC (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.15, 0.8)) +
  scale_x_date(labels = date_format("%m-%Y")) +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_Monitor_TS.jpeg"),
       height = 5, width = 5, dpi = 500, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' Assign monitor BC time weighted averages to filters using start and end dates
#' Initial model fit
#' -----------------------------------------------------------------------------

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

#' Plot BC monitor vs UPAS for each location
cal_data$monitor_fac <- factor(cal_data$monitor_id, 
                               labels = c("I-25 Denver"))

#' Month and season variables
cal_data <- cal_data %>% 
  mutate(month = month(StartDateTimeLocal)) %>% 
  mutate(season = ifelse(month %in% c(12, 1, 2), 1,
                         ifelse(month %in% c(3, 4, 5), 2, 
                                ifelse(month %in% c(6, 7, 8), 3, 4)))) %>% 
  mutate(season_fac = as.factor(season))

ggplot(cal_data, aes(x = monitor_mean, y = bc_ug_m3)) +
  geom_smooth(method = "lm", show.legend = F) +
  geom_point(aes(color = campaign)) +
  scale_color_viridis(name = "Campaign", discrete = T) +
  ylab("UPAS BC (\u03bcg/m\u00b3)") + xlab("Monitor BC (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.1, 0.8)) +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_Cal_Curve.jpeg"),
       height = 5, width = 7, dpi = 500, units = "in", device = "jpeg")

#' Stratified by campaign
#' cal
ggplot() +
  geom_smooth(data = filter(cal_data, campaign == "Campaign2"),
              aes(x = monitor_mean, y = bc_ug_m3, color = campaign),
              method = "lm", show.legend = F, se = F) +
  geom_point(data = filter(cal_data, campaign == "Campaign2"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign)) +
  geom_smooth(data = filter(cal_data, campaign == "Campaign3"),
              aes(x = monitor_mean, y = bc_ug_m3, color = campaign),
              method = "lm", show.legend = F, se = F) +
  geom_point(data = filter(cal_data, campaign == "Campaign3"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign)) +
  geom_smooth(data = filter(cal_data, campaign == "Campaign4"),
              aes(x = monitor_mean, y = bc_ug_m3, color = campaign),
              method = "lm", show.legend = F, se = F) +
  geom_point(data = filter(cal_data, campaign == "Campaign4"),
             aes(x = monitor_mean, y = bc_ug_m3, color = campaign)) +
  # facet_grid(campaign ~ .) +
  scale_color_viridis(discrete = T, name = NULL) +
  ylab("UPAS BC (\u03bcg/m\u00b3)") + xlab("Monitor BC (\u03bcg/m\u00b3)") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "BC_Cal_Curve_By_Campaign.jpeg"),
       height = 5, width = 6, dpi = 500, units = "in", device = "jpeg")

#' Linear regression model
#' I25-Denver: R2 = -0.05
cor(cal_data$bc_ug_m3, cal_data$monitor_mean)
plot(cal_data$bc_ug_m3, cal_data$monitor_mean)

i25_lm <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data)
summary(i25_lm)
par(mfrow=c(2,2))
plot(i25_lm)
par(mfrow=c(1,1))

#' Linear regression models for each campaign
#' Summer = Campaign2
cal_data_summer <- filter(cal_data, campaign == "Campaign2")
cor(cal_data_summer$bc_ug_m3, cal_data_summer$monitor_mean)
plot(cal_data_summer$bc_ug_m3, cal_data_summer$monitor_mean)

i25_lm_summer <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_summer)
summary(i25_lm_summer)
par(mfrow=c(2,2))
plot(i25_lm_summer)
par(mfrow=c(1,1))

#' Fall = Campaign3
cal_data_fall <- filter(cal_data, campaign == "Campaign3")
cor(cal_data_fall$bc_ug_m3, cal_data_fall$monitor_mean)
plot(cal_data_fall$bc_ug_m3, cal_data_fall$monitor_mean)

i25_lm_fall <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_fall)
summary(i25_lm_fall)
par(mfrow=c(2,2))
plot(i25_lm_fall)
par(mfrow=c(1,1))

#' Winter = Campaign4
cal_data_winter <- filter(cal_data, campaign == "Campaign4")
cor(cal_data_winter$bc_ug_m3, cal_data_winter$monitor_mean)
plot(cal_data_winter$bc_ug_m3, cal_data_winter$monitor_mean)

i25_lm_winter <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_winter)
summary(i25_lm_winter)
par(mfrow=c(2,2))
plot(i25_lm_winter)
par(mfrow=c(1,1))

#' Just Summer and Fall (Campaign 2 and 3)
cal_data_sum_fall <- filter(cal_data, campaign %in% c("Campaign2", "Campaign3"))
cor(cal_data_sum_fall$bc_ug_m3, cal_data_sum_fall$monitor_mean)
plot(cal_data_sum_fall$bc_ug_m3, cal_data_sum_fall$monitor_mean)

i25_lm_sum_fall <- lm(bc_ug_m3 ~ monitor_mean, data = cal_data_sum_fall)
summary(i25_lm_sum_fall)
par(mfrow=c(2,2))
plot(i25_lm_sum_fall)
par(mfrow=c(1,1))

#' -----------------------------------------------------------------------------
#' Try increasing fit for the I-25 monitor, as this one has temperature data
#' Also try PM2.5 at the monitor
#' -----------------------------------------------------------------------------

cal_data2 <- filter(cal_data, monitor_id == "080310027")

#' Assign start and end TEMP from the monitoring data
#' Assign a smoke day variable
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

#' Fit linear regression models with temp
#' Linear regression model
cor(cal_data2$bc_ug_m3, cal_data2$monitor_mean)
plot(cal_data2$bc_ug_m3, cal_data2$monitor_mean)

i25_lm1 <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp + monitor_pm, 
              data = cal_data2)
summary(i25_lm1)
par(mfrow=c(2,2))
plot(i25_lm1)
par(mfrow=c(1,1))

#' Linear regression models for each campaign
#' Summer = Campaign2
cal_data2_summer <- filter(cal_data2, campaign == "Campaign2")
cor(cal_data2_summer$bc_ug_m3, cal_data2_summer$monitor_mean)
plot(cal_data2_summer$bc_ug_m3, cal_data2_summer$monitor_mean)

i25_lm1_summer <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp + monitor_pm, 
                     data = cal_data2_summer)
summary(i25_lm1_summer)
par(mfrow=c(2,2))
plot(i25_lm1_summer)
par(mfrow=c(1,1))

#' Fall = Campaign3
cal_data2_fall <- filter(cal_data2, campaign == "Campaign3")
cor(cal_data2_fall$bc_ug_m3, cal_data2_fall$monitor_mean)
plot(cal_data2_fall$bc_ug_m3, cal_data2_fall$monitor_mean)

i25_lm1_fall <- lm(bc_ug_m3 ~ monitor_mean  + monitor_temp + monitor_pm, 
                   data = cal_data2_fall)
summary(i25_lm1_fall)
par(mfrow=c(2,2))
plot(i25_lm1_fall)
par(mfrow=c(1,1))

#' Winter = Campaign4
cal_data2_winter <- filter(cal_data2, campaign == "Campaign4")
cor(cal_data2_winter$bc_ug_m3, cal_data2_winter$monitor_mean)
plot(cal_data2_winter$bc_ug_m3, cal_data2_winter$monitor_mean)

i25_lm1_winter <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp + monitor_pm, 
                     data = cal_data2_winter)
summary(i25_lm1_winter)
par(mfrow=c(2,2))
plot(i25_lm1_winter)
par(mfrow=c(1,1))

#' Just Summer and Fall (Campaign 2 and 3)
cal_data2_sum_fall <- filter(cal_data2, campaign %in% c("Campaign2", "Campaign3"))
cor(cal_data2_sum_fall$bc_ug_m3, cal_data2_sum_fall$monitor_mean)
plot(cal_data2_sum_fall$bc_ug_m3, cal_data2_sum_fall$monitor_mean)

i25_lm_sum_fall <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp + monitor_pm, 
                      data = cal_data2_sum_fall)
summary(i25_lm_sum_fall)
par(mfrow=c(2,2))
plot(i25_lm_sum_fall)
par(mfrow=c(1,1))

#' -----------------------------------------------------------------------------
#' Use indicators for campaign and a temperature variable
#' Use separate models for Campaigns 2 and 3 vs. Campaign 4
#' -----------------------------------------------------------------------------

cal_data3 <- cal_data2 %>% 
  mutate(camp2 = ifelse(campaign == "Campaign2", 1, 0),
         camp3 = ifelse(campaign == "Campaign3", 1, 0),
         camp4 = ifelse(campaign == "Campaign4", 1, 0),
         camp123 = ifelse(campaign == "Campaign4", 0, 1),
         monitor_temp_sq = monitor_temp^2,
         monitor_pm_sq = monitor_pm^2) 

cor(cal_data3$bc_ug_m3, cal_data3$monitor_mean)
plot(cal_data3$bc_ug_m3, cal_data3$monitor_mean)

#' Just Summer and Fall (Campaign 2 and 3)
#' Use indicator variable for Campaign 2
cal_data3_sum_fall <- filter(cal_data3, campaign %in% c("Campaign2", "Campaign3"))
cor(cal_data3_sum_fall$bc_ug_m3, cal_data3_sum_fall$monitor_mean)
plot(cal_data3_sum_fall$bc_ug_m3, cal_data3_sum_fall$monitor_mean)

#' With monitor_pm and camp2
i25_lm_sum_fall1 <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + camp2, 
                      data = cal_data3_sum_fall)
summary(i25_lm_sum_fall1)
par(mfrow=c(2,2))
plot(i25_lm_sum_fall1)
par(mfrow=c(1,1))

#' With monitor_temp, and camp2
i25_lm_sum_fall2 <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp + camp2, 
                       data = cal_data3_sum_fall)
summary(i25_lm_sum_fall2)
par(mfrow=c(2,2))
plot(i25_lm_sum_fall2)
par(mfrow=c(1,1))

#' With monitor_pm, monitor_temp, and camp2
i25_lm_sum_fall3 <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_temp + camp2, 
                      data = cal_data3_sum_fall)
summary(i25_lm_sum_fall3)
par(mfrow=c(2,2))
plot(i25_lm_sum_fall3)
par(mfrow=c(1,1))

#' With monitor_pm, monitor_temp, monitor_temp_sq, and camp2
i25_lm_sum_fall4 <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_temp + 
                         monitor_temp_sq + camp2, 
                       data = cal_data3_sum_fall)
summary(i25_lm_sum_fall4)
par(mfrow=c(2,2))
plot(i25_lm_sum_fall4)
par(mfrow=c(1,1))

#' With monitor_pm, monitor_pm_sq, monitor_temp, and camp2
i25_lm_sum_fall5 <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_pm_sq +
                         monitor_temp + camp2, 
                       data = cal_data3_sum_fall)
summary(i25_lm_sum_fall5)
par(mfrow=c(2,2))
plot(i25_lm_sum_fall5)
par(mfrow=c(1,1))

#' With monitor_pm, monitor_pm_sq, monitor_temp, monitor_temp_sq, and camp2
i25_lm_sum_fall6 <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_pm_sq +
                         monitor_temp + monitor_temp_sq + camp2, 
                       data = cal_data3_sum_fall)
summary(i25_lm_sum_fall6)
par(mfrow=c(2,2))
plot(i25_lm_sum_fall6)
par(mfrow=c(1,1))

#' Winter = Campaign4
cal_data3_winter <- filter(cal_data3, campaign == "Campaign4")
cor(cal_data3_winter$bc_ug_m3, cal_data3_winter$monitor_mean)
plot(cal_data3_winter$bc_ug_m3, cal_data3_winter$monitor_mean)

#' With monitor_pm
i25_lm_winter1 <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm, 
                       data = cal_data3_winter)
summary(i25_lm_winter1)
par(mfrow=c(2,2))
plot(i25_lm_winter1)
par(mfrow=c(1,1))

#' With monitor_temp
i25_lm_winter2 <- lm(bc_ug_m3 ~ monitor_mean + monitor_temp, 
                       data = cal_data3_winter)
summary(i25_lm_winter2)
par(mfrow=c(2,2))
plot(i25_lm_winter2)
par(mfrow=c(1,1))

#' With monitor_pm, monitor_temp
i25_lm_winter3 <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_temp, 
                       data = cal_data3_winter)
summary(i25_lm_winter3)
par(mfrow=c(2,2))
plot(i25_lm_winter3)
par(mfrow=c(1,1))

#' With monitor_pm, monitor_temp, monitor_temp_sq
i25_lm_winter4 <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_temp + monitor_temp_sq, 
                       data = cal_data3_winter)
summary(i25_lm_winter4)
par(mfrow=c(2,2))
plot(i25_lm_winter4)
par(mfrow=c(1,1))

#' With monitor_pm, monitor_pm_sq, and monitor_temp
i25_lm_winter5 <- lm(bc_ug_m3 ~ monitor_mean + monitor_pm + monitor_pm_sq + monitor_temp, 
                     data = cal_data3_winter)
summary(i25_lm_winter5)
par(mfrow=c(2,2))
plot(i25_lm_winter5)
par(mfrow=c(1,1))

#' -----------------------------------------------------------------------------
#' compare i25 models for the summer and fall campaigns
#' Looks like model 5 is the best bet-- high R2, low AIC, normalish residuals
#' -----------------------------------------------------------------------------

summary(i25_lm_sum_fall1) #R2 = 0.55
summary(i25_lm_sum_fall2) #R2 = 0.40
summary(i25_lm_sum_fall3) #R2 = 0.72
summary(i25_lm_sum_fall4) #R2 = 0.69
summary(i25_lm_sum_fall5) #R2 = 0.77
summary(i25_lm_sum_fall6) #R2 = 0.71

AIC(i25_lm_sum_fall1) #AIC = 30.67
AIC(i25_lm_sum_fall2) #AIC = 33.85
AIC(i25_lm_sum_fall3) #AIC = 25.78
AIC(i25_lm_sum_fall4) #AIC = 26.87
AIC(i25_lm_sum_fall5) #AIC = 23.76
AIC(i25_lm_sum_fall5) #AIC = 23.67

hist(i25_lm_sum_fall1$residuals)
hist(i25_lm_sum_fall2$residuals)
hist(i25_lm_sum_fall3$residuals)
hist(i25_lm_sum_fall4$residuals)
hist(i25_lm_sum_fall5$residuals)
hist(i25_lm_sum_fall6$residuals)

#' -----------------------------------------------------------------------------
#' compare i25 models for the winter campaign
#' Going to go with model 5: lowest AIC and highest R2
#' -----------------------------------------------------------------------------

summary(i25_lm_winter1) #R2 = 0.32
summary(i25_lm_winter2) #R2 = -0.27
summary(i25_lm_winter3) #R2 = 0.45
summary(i25_lm_winter4) #R2 = 0.63
summary(i25_lm_winter5) #R2 = 0.88

AIC(i25_lm_winter1) #AIC = 8.41
AIC(i25_lm_winter2) #AIC = 12.23
AIC(i25_lm_winter3) #AIC = 6.74
AIC(i25_lm_winter4) #AIC = 2.25
AIC(i25_lm_winter5) #AIC = -4.73

hist(i25_lm_winter1$residuals)
hist(i25_lm_winter2$residuals)
hist(i25_lm_winter3$residuals)
hist(i25_lm_winter4$residuals)
hist(i25_lm_winter5$residuals)

#' -----------------------------------------------------------------------------
#' Because there is uncertainty in the BC measurements both for the aethalometer
#' and the UPAS, I'm going to fit curves using Deming regression
#' 
#' Deming regression tutorial: https://www.r-bloggers.com/deming-and-passing-bablok-regression-in-r/
#' 
#' NOTE: Not sure these models are really working all that well
#' I know there is error in the aethalometer measurements, but I also seem to 
#' need several other variables that cannot be included in Deming regression.
#' I'll revisit this later...
#' -----------------------------------------------------------------------------

library(mcr)

#' Campaigns 2 and 3
i25_dem_sum_fall <- mcreg(cal_data3_sum_fall$monitor_mean, 
                          cal_data3_sum_fall$bc_ug_m3, 
                          method.reg = "Deming")
str(i25_dem_sum_fall)
i25_dem_sum_fall@para
summary(i25_lm_sum_fall5)

plot(cal_data3_sum_fall$monitor_mean, cal_data3_sum_fall$pm_ug_m3, 
     main = "Regression Comparison", 
     xlab = "EPA Monitor", ylab = "UPAS Filter")
abline(i25_lm_sum_fall5, col="red")
abline(i25_dem_sum_fall@para[1:2], col = "blue")

#' Campaign 4
i25_dem_winter <- mcreg(cal_data3_winter$monitor_mean, 
                          cal_data3_winter$bc_ug_m3, 
                          method.reg = "Deming")
str(i25_dem_winter)
i25_dem_winter@para
summary(i25_lm_winter5)

plot(cal_data3_winter$monitor_mean, cal_data3_winter$pm_ug_m3, 
     main = "Regression Comparison", 
     xlab = "EPA Monitor", ylab = "UPAS Filter")
abline(i25_lm_winter5, col="red")
abline(i25_dem_winter@para[1:2], col = "blue")

#' =============================================================================
#' Calibrating Spring, Summer, and Fall Filters
#' Campaigns 1, 2, and 3
#' 
#' Summer/Fall: R2 = 0.77, Mean bias = -2.24, RMSE = 0.77
#' Winter: R2 = 0.88, Mean bias = 3.55, RMSE = 1.62
#' =============================================================================

#' Campaigns 2 and 3
fit_lm1 <- i25_lm_sum_fall3
summary(fit_lm1)
fit_monitor <- "080310027"

#' Coefficients for calibration equation
int1 <- unname(fit_lm1$coefficients[1])
beta_mon_bc1 <- unname(fit_lm1$coefficients[2])
beta_pm1 <- unname(fit_lm1$coefficients[3])
beta_temp1 <- unname(fit_lm1$coefficients[4])
beta_camp21 <- unname(fit_lm1$coefficients[5])

#' Calibrating using equation
fit_data_sum_fall <- cal_data3_sum_fall %>% 
  #' Calibrated BC
  mutate(bc_ug_m3_cal = (bc_ug_m3 - int1 - (beta_pm1 * monitor_pm) - (beta_temp1 * monitor_temp) -
                           (beta_camp21 * camp2))/ beta_mon_bc1)

#' Mean bias and RMSE
mean_bias_sum_fall <- sum((fit_data_sum_fall$bc_ug_m3_cal - fit_data_sum_fall$bc_ug_m3), 
                          na.rm=T) / nrow(fit_data_sum_fall)
mean_bias_sum_fall

rmse_sum_fall <- sqrt(sum((fit_data_sum_fall$bc_ug_m3_cal - fit_data_sum_fall$bc_ug_m3)^2,
                          na.rm = TRUE)) / nrow(fit_data_sum_fall)
rmse_sum_fall

#' Campaign 4
fit_lm2 <- i25_lm_winter3
summary(fit_lm2)
fit_monitor <- "080310027"

#' Coefficients for calibration equation
int2 <- unname(fit_lm1$coefficients[1])
beta_mon_bc2 <- unname(fit_lm1$coefficients[2])
beta_pm2 <- unname(fit_lm1$coefficients[3])
beta_temp2 <- unname(fit_lm1$coefficients[4])

#' Calibrating using equation
fit_data_winter <- cal_data3_winter %>% 
  #' Calibrated BC
  mutate(bc_ug_m3_cal = (bc_ug_m3 - int2 - (beta_pm2 * monitor_pm) -
                           (beta_temp2 * monitor_temp)) / beta_mon_bc2)

#' Mean bias and RMSE
mean_bias_winter <- sum((fit_data_winter$bc_ug_m3_cal - fit_data_winter$bc_ug_m3), 
                          na.rm=T) / nrow(fit_data_winter)
mean_bias_winter

rmse_winter <- sqrt(sum((fit_data_winter$bc_ug_m3_cal - fit_data_winter$bc_ug_m3)^2,
                          na.rm = TRUE)) / nrow(fit_data_winter)
rmse_winter

#' -----------------------------------------------------------------------------
#' Calibrate the filter data
#' -----------------------------------------------------------------------------

filter_data2 <- filter_data %>% 
  filter(indoor == 0) %>% 
  filter(is_blank == 0) %>% 
  filter(!is.na(bc_ug_m3)) %>% 
  filter(bc_ug_m3 > 0) %>% 
  filter(low_volume_flag == 0) %>% 
  mutate(monitor_id = fit_monitor)
summary(filter_data2)

#' Get the monitor values for the filter dataset
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
  
  #' Get mean PM2.5 from the monitor for the period covered by the filter
  pm <- monitor_pm %>% 
    filter(monitor_id == paired_monitor_id) %>% 
    filter(Date_Local %in% dates)
  mean_pm <- mean(pm$Arithmetic_Mean, na.rm=T)
  
  filter_data2[i, "monitor_pm"] <- mean_pm
  filter_data2[i, "monitor_pm_n_obs"] <- nrow(pm)
}

#' Add the indicator variables
filter_data2 <- filter_data2 %>% 
  mutate(camp2 = ifelse(campaign == "Campaign2", 1, 0),
         camp3 = ifelse(campaign == "Campaign3", 1, 0),
         camp4 = ifelse(campaign == "Campaign4", 1, 0),
         monitor_temp_sq = monitor_temp^2,
         monitor_pm_sq = monitor_pm^2) 

#' Predict BC for Campaigns 1, 2, and 3
filter_data2a <- filter_data2 %>%
  filter(campaign %in% c(paste0("Campaign", c("1", "2", "3")))) %>% 
  mutate(bc_ug_m3_cal = (bc_ug_m3 - int1 - (beta_pm1 * monitor_pm) - (beta_temp1 * monitor_temp) -
                           (beta_camp21 * camp2))/ beta_mon_bc1)

summary(filter_data2a$bc_ug_m3)
summary(filter_data2a$bc_ug_m3_cal)

#' Predict BC for Campaign 4
filter_data2b <- filter_data2 %>%
  filter(campaign == "Campaign4") %>% 
  mutate(bc_ug_m3_cal = (bc_ug_m3 - int2 - (beta_pm2 * monitor_pm) -
                           (beta_temp2 * monitor_temp)) / beta_mon_bc2)

summary(filter_data2b$bc_ug_m3)
summary(filter_data2b$bc_ug_m3_cal)

#' -----------------------------------------------------------------------------
#' Diagnostic plots
#' -----------------------------------------------------------------------------

filter_data_cal <- bind_rows(filter_data2a, filter_data2b)

#' How do the monthly means look?
monthly_means <- filter_data_cal %>% 
  group_by(month) %>% 
  summarize(mean_bc_raw = mean(bc_ug_m3, na.rm = T),
            min_bc_raw = min(bc_ug_m3, na.rm = T),
            max_bc_raw = max(bc_ug_m3, na.rm = T),
            mean_bc_cal = mean(bc_ug_m3_cal, na.rm = T),
            min_bc_cal = min(bc_ug_m3_cal, na.rm = T),
            max_bc_cal = max(bc_ug_m3_cal, na.rm = T))
monthly_means

seasonal_means <- filter_data_cal %>% 
  group_by(season) %>% 
  summarize(mean_bc_raw = mean(bc_ug_m3, na.rm = T),
            min_bc_raw = min(bc_ug_m3, na.rm = T),
            max_bc_raw = max(bc_ug_m3, na.rm = T),
            mean_bc_cal = mean(bc_ug_m3_cal, na.rm = T),
            min_bc_cal = min(bc_ug_m3_cal, na.rm = T),
            max_bc_cal = max(bc_ug_m3_cal, na.rm = T))
seasonal_means

filter_data_comp <- filter_data_cal %>% 
  select(filter_id, month, season, bc_ug_m3, bc_ug_m3_cal) %>% 
  gather(key = type, value = bc, -filter_id, -month, -season)

monitor_data_comp <- monitor_data %>%
  filter(County_Code %in% c("001", "005", "031")) %>% 
  select(Date_Local, Arithmetic_Mean) %>% 
  mutate(month = month(Date_Local)) %>% 
  mutate(season = ifelse(month %in% c(12, 1, 2), 1,
                         ifelse(month %in% c(3, 4, 5), 2,
                                ifelse(month %in% c(6, 7, 8), 3, 4)))) %>% 
  mutate(type = "monitor", filter_id = "EPA_Monitor") %>% 
  rename("bc" = "Arithmetic_Mean")

filter_data_comp <- bind_rows(filter_data_comp, monitor_data_comp)

ggplot(filter_data_comp, aes(x = as.factor(month), y = bc, fill = as.factor(type))) +
  ggtitle("Monthly distribution of measured and calibrated BC") +
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                    labels = c("bc_ug_m3" = "Raw Data",
                               "bc_ug_m3_cal" = "Calibrated",
                               "monitor" = "EPA Monitor")) +
  xlab("Month") + ylab("BC (\u03bcg/m\u00b3)") +
  theme(legend.position = c(0.8, 0.2)) +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Monthly_Means_BC.jpeg"),
       height = 5, width = 5, dpi = 500, units = "in", device = "jpeg")

ggplot(filter_data_comp, aes(x = as.factor(season), y = bc, fill = as.factor(type))) +
  ggtitle("Seasonal distribution of measured and calibrated BC") +
  geom_boxplot(alpha = 0.75) +
  # geom_point(position=position_jitterdodge(), color = "grey70", alpha = 0.20) +
  scale_fill_viridis(name = "Data Type", discrete = T, 
                     labels = c("bc_ug_m3" = "Raw Data",
                                "bc_ug_m3_cal" = "Calibrated",
                                "monitor" = "EPA Monitor")) +
  xlab("Season") + ylab("BC (\u03bcg/m\u00b3)") +
  scale_x_discrete(labels = c("1" = "Winter", "2" = "Spring",
                              "3" = "Summer", "4" = "Fall")) +
  theme(legend.position = c(0.8, 0.2)) +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "Seasonal_Means_BC.jpeg"),
       height = 5, width = 5, dpi = 500, units = "in", device = "jpeg")

#' Write out the calibrated dataset
glimpse(filter_data2)

write_csv(filter_data2, here::here("Data", "Filter_BC_Calibrated.csv"))

## UPAS Metadata Plots
collocated_upas <- read_csv(here::here("Data", "Collocation_Filter_IDs.csv")) %>% 
  filter(Location == "9th and Yuma")

upas_meta <- read_csv(here::here("Data/UPAS_Data", "UPAS_Metadata.csv")) %>% 
  mutate(Campaign = gsub("Campaign", "", campaign),
         CampaignWeek = str_pad(gsub(" ", "", gsub("Week", "", week)), width = 2, pad = "0"),
         UPASserial = paste0("PS", UPASserial))
upas_links <- read_xlsx(here::here("Data/Filter_Data", "Campaign_Links.xlsx")) %>% 
  rename("filter_id" = "Filter_Code")

meta_df <- collocated_upas %>% 
  left_join(upas_links, by = "filter_id") %>% 
  left_join(upas_meta, by = c("UPASserial", "Campaign", "CampaignWeek"))

write_csv(meta_df, here::here("Data/UPAS_Data", "Summary_Yuma_St_UPAS_Metadata.csv"))

upas_log_files <- list.files(here::here("Data/UPAS_Data/Yuma_St_UPAS_Logs"))

meta_data <- data.frame()
for (i in 1:length(upas_log_files)) {
  print(i)
  temp <- read.table(here::here("Data/UPAS_Data/Yuma_St_UPAS_Logs",
                                upas_log_files[i]),
                     sep = ",", skip = 59, header = T)
  temp$UPAS <- str_sub(upas_log_files[i], start = 1, end = 6)
  meta_data <- bind_rows(meta_data, temp)
  rm(temp)
}

meta_data$localdt <- as.POSIXct(meta_data$UnixTime, origin="1970-01-01")

#' Plot some variables

#' PumpP
ggplot(meta_data, aes(x = localdt, y = PumpP)) +
  # geom_smooth(method = "loess", show.legend = F) +
  geom_point(aes(color = UPAS), cex = 0.5) +
  scale_color_viridis(name = "UPAS Serial", discrete = T) +
  ylab("PumpP") + xlab("Sample Time") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "PumpP.jpeg"),
       height = 4, width = 7, dpi = 500, units = "in", device = "jpeg")

#' PumpT
ggplot(meta_data, aes(x = localdt, y = PumpT)) +
  # geom_smooth(method = "loess", show.legend = F) +
  geom_point(aes(color = UPAS), cex = 0.5) +
  scale_color_viridis(name = "UPAS Serial", discrete = T) +
  ylab("PumpT") + xlab("Sample Time") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "PumpT.jpeg"),
       height = 4, width = 7, dpi = 500, units = "in", device = "jpeg")

#' PumpRH
ggplot(meta_data, aes(x = localdt, y = PumpRH)) +
  # geom_smooth(method = "loess", show.legend = F) +
  geom_point(aes(color = UPAS), cex = 0.5) +
  scale_color_viridis(name = "UPAS Serial", discrete = T) +
  ylab("PumpRH") + xlab("Sample Time") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "PumpRH.jpeg"),
       height = 4, width = 7, dpi = 500, units = "in", device = "jpeg")

#' Volumetric Flow Rate
ggplot(meta_data, aes(x = localdt, y = VolumetricFlowRate)) +
  # geom_smooth(method = "loess", show.legend = F) +
  geom_point(aes(color = UPAS), cex = 0.5) +
  scale_color_viridis(name = "UPAS Serial", discrete = T) +
  ylab("VolumetricFlowRate") + xlab("Sample Time") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "VolFlowRate.jpeg"),
       height = 4, width = 7, dpi = 500, units = "in", device = "jpeg")

#' AtmoRho
ggplot(meta_data, aes(x = localdt, y = AtmoRho)) +
  # geom_smooth(method = "loess", show.legend = F) +
  geom_point(aes(color = UPAS), cex = 0.5) +
  scale_color_viridis(name = "UPAS Serial", discrete = T) +
  ylab("AtmoRho") + xlab("Sample Time") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "AtmoRho.jpeg"),
       height = 4, width = 7, dpi = 500, units = "in", device = "jpeg")

#' Mass Flow
ggplot(meta_data, aes(x = localdt, y = MassFlow)) +
  # geom_smooth(method = "loess", show.legend = F) +
  geom_point(aes(color = UPAS), cex = 0.5) +
  scale_color_viridis(name = "UPAS Serial", discrete = T) +
  ylab("MassFlow") + xlab("Sample Time") +
  simple_theme
ggsave(filename = here::here("Figs/Calibration", "MassFlow.jpeg"),
       height = 4, width = 7, dpi = 500, units = "in", device = "jpeg")




