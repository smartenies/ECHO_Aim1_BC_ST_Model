#' =============================================================================
#' Project: ECHO LUR
#' Date created: January 18, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 

#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggthemes)
library(GGally)
library(ggcorrplot)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)
library(viridis)
library(caret)

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
#' Read in the dataset-- just outdoor filters
#' -----------------------------------------------------------------------------



#' -----------------------------------------------------------------------------
#' Need to choose which pm_ug_m3 measurement to use
#' -----------------------------------------------------------------------------

#' #' Compare the three pm_ug_m3 estimates using the runtime estimates:
#' #'    -Logged run time
#' #'    -Calculated run time using the UTC time stamps
#' #'    -Calculated run time using the Local time stamps
#' 
#' summary(lur_data$pm_ug_m3_raw)
#' summary(lur_data$pm_ug_m3_local_rt_vol)
#' summary(lur_data$pm_ug_m3_local_rt_vol)
#' 
#' #' set negative concentrations to NA for now
#' #' drop filter with missing metadata
#' lur_data <- lur_data %>% 
#'   filter(logged_rt_volume_m3 > 0) %>% 
#'   mutate(pm_ug_m3_raw = ifelse(pm_ug_m3_raw < 0, NA, pm_ug_m3_raw),
#'          pm_ug_m3_local_rt_vol = ifelse(pm_ug_m3_local_rt_vol < 0, NA, 
#'                                         pm_ug_m3_local_rt_vol),
#'          pm_ug_m3_local_rt_vol = ifelse(pm_ug_m3_local_rt_vol < 0, NA, 
#'                                         pm_ug_m3_local_rt_vol))
#' 
#' summary(lur_data$pm_ug_m3_raw)
#' summary(lur_data$pm_ug_m3_local_rt_vol)
#' summary(lur_data$pm_ug_m3_local_rt_vol)
#' 
#' #' Compare distributions- all points
#' ggplot(lur_data) +
#'   geom_density(aes(x = pm_ug_m3_raw, color = "logged")) +
#'   geom_density(aes(x = pm_ug_m3_local_rt_vol, color = "local_ts")) +
#'   geom_density(aes(x = pm_ug_m3_utc_rt_vol, color = "utc_ts")) +
#'   scale_color_manual(name = "Volume method",
#'                      values = c("logged" = "blue",
#'                                 "local_ts" = "red",
#'                                 "utc_ts" = "darkgreen"),
#'                      labels = c("logged" = "Logged runtime",
#'                                 "local_ts" = "Local timestamps",
#'                                 "utc_ts" = "UTC timestamps")) +
#'   xlab("PM concentration") +
#'   facet_grid(cols = vars(campaign)) +
#'   theme(legend.position = c(0.9, 0.8)) +
#'   simple_theme
#' 
#' #' Compare distributions- concentrations < 100 ug/m3
#' ggplot(filter(lur_data, pm_ug_m3_raw < 100)) +
#'   geom_density(aes(x = pm_ug_m3_raw, color = "logged")) +
#'   geom_density(aes(x = pm_ug_m3_local_rt_vol, color = "local_ts")) +
#'   geom_density(aes(x = pm_ug_m3_utc_rt_vol, color = "utc_ts")) +
#'   scale_color_manual(name = "Volume method",
#'                      values = c("logged" = "blue",
#'                                 "local_ts" = "red",
#'                                 "utc_ts" = "darkgreen"),
#'                      labels = c("logged" = "Logged runtime",
#'                                 "local_ts" = "Local timestamps",
#'                                 "utc_ts" = "UTC timestamps")) +
#'   xlab("PM concentration") +
#'   facet_grid(cols = vars(campaign)) +
#'   theme(legend.position = c(0.9, 0.8)) +
#'   simple_theme
#' 
#' #' Scatterplots comparing concentrations
#' library(scatterplot3d)
#' 
#' #' Scatterplot for all points
#' compare_df <- dplyr::select(lur_data, pm_ug_m3_raw, pm_ug_m3_local_rt_vol, pm_ug_m3_utc_rt_vol, campaign) %>%  
#'   st_set_geometry(NULL) %>% 
#'   as.data.frame()
#' 
#' colors <- c("red", "blue")
#' colors <- colors[as.numeric(as.factor(compare_df$campaign))]
#' scatterplot3d(compare_df[,1:3], color = colors)
#' 
#' #' Scatterplot for concentrations < 100 ug/m3
#' compare_df2 <- filter(compare_df, pm_ug_m3_raw < 100)
#' 
#' colors <- c("red", "blue")
#' colors <- colors[as.numeric(as.factor(compare_df2$campaign))]
#' scatterplot3d(compare_df2[,1:3], color = colors)
#' 
#' #' Correlations between the three versions of the concentration
#' metric_corr <- cor(compare_df[,1:3], use = "complete.obs")
#' ggcorrplot(metric_corr, type = "upper", method = "square",
#'            ggtheme = simple_theme, lab = T, lab_col = "white",
#'            show.diag = T)
#' 
#' #' Absolute and relative differences- all samples
#' compare_df <- compare_df %>% 
#'   mutate(logged_local_diff = pm_ug_m3_raw - pm_ug_m3_local_rt_vol,
#'          logged_local_pdiff = (pm_ug_m3_raw - pm_ug_m3_local_rt_vol) / pm_ug_m3_raw,
#'          logged_utc_diff = pm_ug_m3_raw - pm_ug_m3_utc_rt_vol,
#'          logged_utc_pdiff = (pm_ug_m3_raw - pm_ug_m3_utc_rt_vol) / pm_ug_m3_raw)
#' 
#' ggplot(compare_df) +
#'   ggtitle("Absolute differences: concentrations using logged run times and time stamp runtimes") +
#'   geom_density(aes(x = logged_local_diff, color = "local_ts")) +
#'   geom_density(aes(x = logged_utc_diff, color = "utc_ts")) +
#'   scale_color_manual(name = "Time stamps for\ncalculating volume",
#'                      values = c("local_ts" = "red",
#'                                 "utc_ts" = "blue"),
#'                      labels = c("local_ts" = "Local timestamps",
#'                                 "utc_ts" = "UTC timestamps")) +
#'   facet_grid(cols = vars(campaign)) +
#'   theme(legend.position = c(0.1, 0.8)) +
#'   simple_theme
#' 
#' ggplot(compare_df) +
#'   ggtitle("Percent differences: concentrations using logged run times and time stamp runtimes") +
#'   geom_density(aes(x = logged_local_pdiff, color = "local_ts")) +
#'   geom_density(aes(x = logged_utc_pdiff, color = "utc_ts")) +
#'   scale_color_manual(name = "Time stamps for\ncalculating volume",
#'                      values = c("local_ts" = "red",
#'                                 "utc_ts" = "blue"),
#'                      labels = c("local_ts" = "Local timestamps",
#'                                 "utc_ts" = "UTC timestamps")) +
#'   facet_grid(cols = vars(campaign)) +
#'   theme(legend.position = c(0.1, 0.8)) +
#'   simple_theme

#' At this point, I'm going to elect to go with the "logged runtime" version
#' of the pm_ug_m3_raw concentration. The three metrics are pretty damn correlated and
#' the distributions seem to match up quite well.

#' -----------------------------------------------------------------------------
#' Summarize LUR covariates and examine distributions
#' check for OLS regression assumptions:
#'      Linear relationships between outcome and predictors
#'      Normally distributed outcome/residuals
#'      Independence of residuals
#'      Constant variance (homoskedasiticy)
#'      Normally distributed errors
#' -----------------------------------------------------------------------------

names(lur_data)
lur_data2 <- lur_data %>% 
  dplyr::select(filter_id, campaign, start_iso_week, month, year, month_yr, 
         season, below_lod, below_loq, low_volume_flag, flow_rate_flag, 
         is_blank, negative_pm_mass, potential_contamination,
         negative_bc_mass, blank_corrected, pm_ug_m3, bc_ug_m3,
         elevation_50:pop_den_2500, dist_m_airport:aadt_2500,
         nearest_neighbor_pm, nearest_3_neighbors_pm,
         nearest_neighbor_bc, nearest_3_neighbors_bc, 
         nearest_neighbor_temp, nearest_3_neighbors_temp, 
         nearest_neighbor_smoke, nearest_3_neighbors_smoke)
summary(lur_data2)

#' reclassify categorical variables by creating dummy variables
lur_data2 <- lur_data2 %>% 
  mutate(winter = ifelse(season == 1, 1, 0),
         spring = ifelse(season == 2, 1, 0),
         summer = ifelse(season == 3, 1, 0),
         fall = ifelse(season == 4, 1, 0)) %>% 
  mutate(land_use_50 = fct_recode(as.factor(land_use_50),
                                   "Open_Water" = "11",
                                   "Open_Space_Dev" = "21",
                                   "Low_Intensity_Dev" = "22",
                                   "Med_Intensity_Dev" = "23",
                                   "Hi_Intensity_Dev" = "24",
                                   "Shrubland" = "52",
                                   "Grassland" = "71",
                                   "Pasture" = "81",
                                   "Cropland" = "82",
                                   "woody_Wetlands" = "90",
                                   "Herbacious_Wetlands" = "95"),
         land_use_100 = fct_recode(as.factor(land_use_100),
                                   "Open_Water" = "11",
                                   "Open_Space_Dev" = "21",
                                   "Low_Intensity_Dev" = "22",
                                   "Med_Intensity_Dev" = "23",
                                   "Hi_Intensity_Dev" = "24",
                                   "Shrubland" = "52",
                                   "Grassland" = "71",
                                   "Pasture" = "81",
                                   "Cropland" = "82",
                                   "woody_Wetlands" = "90",
                                   "Herbacious_Wetlands" = "95"),
         land_use_250 = fct_recode(as.factor(land_use_250),
                                   "Open_Water" = "11",
                                   "Open_Space_Dev" = "21",
                                   "Low_Intensity_Dev" = "22",
                                   "Med_Intensity_Dev" = "23",
                                   "Hi_Intensity_Dev" = "24",
                                   "Shrubland" = "52",
                                   "Grassland" = "71",
                                   "Pasture" = "81",
                                   "Cropland" = "82",
                                   "woody_Wetlands" = "90",
                                   "Herbacious_Wetlands" = "95"),
         land_use_500 = fct_recode(as.factor(land_use_500),
                                   "Open_Water" = "11",
                                   "Open_Space_Dev" = "21",
                                   "Low_Intensity_Dev" = "22",
                                   "Med_Intensity_Dev" = "23",
                                   "Hi_Intensity_Dev" = "24",
                                   "Shrubland" = "52",
                                   "Grassland" = "71",
                                   "Pasture" = "81",
                                   "Cropland" = "82",
                                   "woody_Wetlands" = "90",
                                   "Herbacious_Wetlands" = "95"),
         land_use_1000 = fct_recode(as.factor(land_use_1000),
                                    "Open_Water" = "11",
                                    "Open_Space_Dev" = "21",
                                    "Low_Intensity_Dev" = "22",
                                    "Med_Intensity_Dev" = "23",
                                    "Hi_Intensity_Dev" = "24",
                                    "Shrubland" = "52",
                                    "Grassland" = "71",
                                    "Pasture" = "81",
                                    "Cropland" = "82",
                                    "woody_Wetlands" = "90",
                                    "Herbacious_Wetlands" = "95"),
         land_use_2500 = fct_recode(as.factor(land_use_2500),
                                    "Open_Water" = "11",
                                    "Open_Space_Dev" = "21",
                                    "Low_Intensity_Dev" = "22",
                                    "Med_Intensity_Dev" = "23",
                                    "Hi_Intensity_Dev" = "24",
                                    "Shrubland" = "52",
                                    "Grassland" = "71",
                                    "Pasture" = "81",
                                    "Cropland" = "82",
                                    "woody_Wetlands" = "90",
                                    "Herbacious_Wetlands" = "95"))

#' For land_use variables, set Low Intensity Developed to reference group
lur_data2 <- lur_data2 %>% 
  mutate(land_use_50 = relevel(land_use_50, ref = "Low_Intensity_Dev"),
         land_use_100 = relevel(land_use_100, ref = "Low_Intensity_Dev"),
         land_use_250 = relevel(land_use_2500, ref = "Low_Intensity_Dev"),
         land_use_500 = relevel(land_use_500, ref = "Low_Intensity_Dev"),
         land_use_1000 = relevel(land_use_1000, ref = "Low_Intensity_Dev"),
         land_use_2500 = relevel(land_use_2500, ref = "Low_Intensity_Dev"))

#' filter all flagged data
lur_data3 <- lur_data2 %>% 
  filter(below_lod == 0 & low_volume_flag == 0 & flow_rate_flag == 0 &
           is_blank == 0 & negative_pm_mass == 0 & negative_bc_mass == 0 &
           potential_contamination == 0)

#' What percentage of filters remain?
nrow(lur_data3) / nrow(lur_data2) * 100

#' Histograms of continuous outcome variables
#' Remember-- linear regression requires a normally-distributed residuals
#' PM2.5 and log_transformed PM2.5
summary(lur_data3$pm_ug_m3)
ggplot(lur_data3) +
  ggtitle("PM\u2082.\u2085") +
  geom_histogram(aes(x = pm_ug_m3)) +
  simple_theme

ggplot(lur_data3) +
  ggtitle("PM\u2082.\u2085") +
  geom_qq(aes(sample = pm_ug_m3)) +
  geom_qq_line(aes(sample = pm_ug_m3)) +
  simple_theme

lur_data3$log_pm_ug_m3 <- log(lur_data3$pm_ug_m3)
ggplot(lur_data3) +
  ggtitle("Log-transformed PM\u2082.\u2085") +
  geom_histogram(aes(x = log_pm_ug_m3)) +
  simple_theme

ggplot(lur_data3) +
  ggtitle("Log-transformed PM\u2082.\u2085") +
  geom_qq(aes(sample = log_pm_ug_m3)) +
  geom_qq_line(aes(sample = log_pm_ug_m3)) +
  simple_theme

#' BC and log-transformed BC
summary(lur_data3$bc_ug_m3)
ggplot(lur_data3) +
  ggtitle("BC") +
  geom_histogram(aes(x = bc_ug_m3)) +
  simple_theme

ggplot(lur_data3) +
  ggtitle("BC") +
  geom_qq(aes(sample = bc_ug_m3)) +
  geom_qq_line(aes(sample = bc_ug_m3)) +
  simple_theme

lur_data3$log_bc_ug_m3 <- log(lur_data3$bc_ug_m3)
ggplot(lur_data3) +
  ggtitle("Log-transformed BC") +
  geom_histogram(aes(x = log_bc_ug_m3)) +
  simple_theme

ggplot(lur_data3) +
  ggtitle("Log-transformed BC") +
  geom_qq(aes(sample = log_bc_ug_m3)) +
  geom_qq_line(aes(sample = log_bc_ug_m3)) +
  simple_theme

#' -----------------------------------------------------------------------------
#' Quick and dirty single-predictor models
#' VERY preliminary model, just to see if linear regression would work here
#' Will use these tables as a starting point for building the LUR models
#' Thanks to Zev Ross for the sample code used here!
#' -----------------------------------------------------------------------------

lur_data3 <- st_set_geometry(lur_data3, NULL)

lur_preds <- dplyr::select(lur_data3, pm_ug_m3, campaign, season, year, start_iso_week,
                           elevation_50:fall) %>%
  gather(key = pred_var, value = measurement, -pm_ug_m3)
head(lur_preds)

ggplot(data = lur_preds, aes(x= measurement, y = pm_ug_m3)) +
  geom_point() +
  stat_smooth() +
  facet_wrap(~pred_var, scales = "free_x")

lm_f <- function(dat0 , yvar = "pm_ug_m3", xvar) {
  dat <- dat0[,c(yvar, xvar)]
  
  form <- paste(yvar, "~", ".")
  lm1 <- lm(as.formula(form), data = dat)
  return(summary(lm1)$r.squared)
}

#' Test function
lm_f(dat0 = lur_data3, xvar = "elevation_50")

#' For all predictor variables, what's the R2 value for the single-predictor
#' models?
#' Use these as the starting place for the LUR models

#' PM2.5
pred_vars <- colnames(lur_data3)[19:91]
rsq <- map_dbl(pred_vars, function(i) lm_f(dat0 = lur_data3, 
                                           yvar = "pm_ug_m3", xvar = i))
tibble(predictor_variables = pred_vars, rsq = rsq) %>% 
  arrange(desc(rsq))

#' Log-PM2.5
rsq <- map_dbl(pred_vars, function(i) lm_f(dat0 = lur_data3, 
                                           yvar = "log_pm_ug_m3", xvar = i))
tibble(predictor_variables = pred_vars, rsq = rsq) %>% 
  arrange(desc(rsq))

#' BC
rsq <- map_dbl(pred_vars, function(i) lm_f(dat0 = lur_data3, 
                                           yvar = "bc_ug_m3", xvar = i))
tibble(predictor_variables = pred_vars, rsq = rsq) %>% 
  arrange(desc(rsq))

#' Log-BC
rsq <- map_dbl(pred_vars, function(i) lm_f(dat0 = lur_data3, 
                                           yvar = "log_bc_ug_m3", xvar = i))
tibble(predictor_variables = pred_vars, rsq = rsq) %>% 
  arrange(desc(rsq))

