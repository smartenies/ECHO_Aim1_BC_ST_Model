#' =============================================================================
#' Project: ECHO LUR
#' Date created: August 7, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script creates the monthly LUR dataset. It aggregates filter data and 
#' time-varying covariates to the month level and recodes covariates as dummy
#' variables
#' 
#' UPDATE 10/07/19: One issue we've been having is that the calibrated BC data
#' are not great. They lack the spatial variability we need for the model. 
#' I've sent an email to our collaborators in engineering hoping to get some 
#' clarity from them. No response yet, but I'm hopeful.
#' 
#' Update 11/21/19: After talking with our engineering colleagues and Sheryl,
#' we're going to use Deming regression and fit seasonal LUR models with indicator
#' variables for each month
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
library(glmnet)

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

data_name <- "Combined_Filter_Data_AEA.csv"
lur_data <- read_csv(here::here("Data", data_name)) %>%
  filter(!is.na(lon)) %>% 
  mutate(lon2 = lon, lat2 = lat) %>% 
  st_as_sf(coords = c('lon2', 'lat2'), crs = ll_wgs84) %>%
  st_transform(crs = albers) %>% 
  filter(indoor == 0) %>% 
  filter(!is.na(month)) %>% 
  filter(pm_ug_m3 >= 0) %>% 
  filter(bc_ug_m3 >= 0)

#' Filter out flagged data
lur_data <- lur_data %>% 
  filter(below_lod == 0 & low_volume_flag == 0 & flow_rate_flag == 0 &
           is_blank == 0 & negative_pm_mass == 0 & potential_contamination == 0)
nrow(lur_data)

#' Clairfy which concentrations are "raw" and which are calibrated 
lur_data <- lur_data %>% 
  rename("pm_ug_m3_raw" = "pm_ug_m3") %>% 
  rename("bc_ug_m3_raw" = "bc_ug_m3")

#' -----------------------------------------------------------------------------
#' UPDATE 10.07.19 Specify a "use" variable:
#' Going with the Deming regression results for now
#' -----------------------------------------------------------------------------

summary(lur_data[,c("month", "pm_ug_m3_raw", "pm_ug_m3_adjlm", 
                    "pm_ug_m3_lm", "pm_ug_m3_dem")])
summary(lur_data[,c("month", "bc_ug_m3_raw", "bc_ug_m3_adjlm",
                    "bc_ug_m3_lm", "bc_ug_m3_dem")])

#' Set negative concentrations to NA
lur_data <- lur_data %>% 
  mutate(pm_ug_m3_raw = ifelse(pm_ug_m3_raw < 0, NA, pm_ug_m3_raw),
         pm_ug_m3_lm = ifelse(pm_ug_m3_lm < 0, NA, pm_ug_m3_lm),
         pm_ug_m3_dem = ifelse(pm_ug_m3_dem < 0, NA, pm_ug_m3_dem),
         pm_ug_m3_adjlm = ifelse(pm_ug_m3_adjlm < 0, NA, pm_ug_m3_adjlm),
         bc_ug_m3_raw = ifelse(bc_ug_m3_raw < 0, NA, bc_ug_m3_raw),
         bc_ug_m3_lm = ifelse(bc_ug_m3_lm < 0, NA, bc_ug_m3_lm),
         bc_ug_m3_dem = ifelse(bc_ug_m3_dem < 0, NA, bc_ug_m3_dem),
         bc_ug_m3_adjlm = ifelse(bc_ug_m3_adjlm < 0, NA, bc_ug_m3_adjlm))

summary(lur_data[,c("month", "pm_ug_m3_raw", "pm_ug_m3_adjlm", 
                    "pm_ug_m3_lm", "pm_ug_m3_dem")])
summary(lur_data[,c("month", "bc_ug_m3_raw", "bc_ug_m3_adjlm",
                    "bc_ug_m3_lm", "bc_ug_m3_dem")])

lur_data <- lur_data %>% 
  mutate(pm_ug_m3_use = pm_ug_m3_dem,
         bc_ug_m3_use = bc_ug_m3_dem)

#' Mean PM
mean(lur_data$pm_ug_m3_use)
sd(lur_data$pm_ug_m3_use)

#' Mean BC
mean(lur_data$bc_ug_m3_use, na.rm = T)
sd(lur_data$bc_ug_m3_use, na.rm = T)
 
#' Quick plots
ggplot() +
  geom_sf(data= st_jitter(lur_data, factor = 0.008), aes(color = pm_ug_m3_use), 
          show.legend = "point") +
  scale_color_viridis(name = "PM2.5 conc")

ggplot() +
  geom_sf(data= st_jitter(lur_data, factor = 0.008), aes(color = bc_ug_m3_use), 
          show.legend = "point") +
  scale_color_viridis(name = "BC conc")

#' -----------------------------------------------------------------------------
#' Clean filter data and create dummy variables as needed
#' -----------------------------------------------------------------------------

#' Do some data cleaning
lur_data1 <- lur_data %>% 
  
  #' reclassify categorical variables by creating dummy variables
  mutate(jan = ifelse(month == 1, 1, 0),
         feb = ifelse(month == 2, 1, 0),
         mar = ifelse(month == 3, 1, 0),
         apr = ifelse(month == 4, 1, 0),
         may = ifelse(month == 5, 1, 0),
         jun = ifelse(month == 6, 1, 0),
         jul = ifelse(month == 7, 1, 0),
         aug = ifelse(month == 8, 1, 0),
         sep = ifelse(month == 9, 1, 0),
         oct = ifelse(month == 10, 1, 0),
         nov = ifelse(month == 11, 1, 0),
         dec = ifelse(month == 12, 1, 0)) %>%
  
  mutate(winter = ifelse(month %in% c(12, 1, 2), 1, 0),
         spring = ifelse(month %in% c(3, 4, 5), 1, 0),
         summer = ifelse(month %in% c(6, 7, 8), 1, 0),
         autumn = ifelse(month %in% c(9, 10, 11), 1, 0)) %>%
  
  #' land use categories: open space, ag, low, med, high intensity = 1, 2, 3, 4, 5
  mutate(open_space_50 = ifelse(land_use_50 %in% c("11", "21", "52", "71", "90", "85"), 1, 0),  
         low_inten_dev_50 = ifelse(land_use_50 == "22", 1, 0),
         med_inten_dev_50 = ifelse(land_use_50 == "23", 1, 0),
         high_inten_dev_50 = ifelse(land_use_50 == "24", 1, 0),
         agriculture_50 = ifelse(land_use_50 %in% c("81", "82"), 1, 0), 
         
         open_space_100 = ifelse(land_use_100 %in% c("11", "21", "52", "71", "90", "85"), 1, 0),  
         low_inten_dev_100 = ifelse(land_use_100 == "22", 1, 0),
         med_inten_dev_100 = ifelse(land_use_100 == "23", 1, 0),
         high_inten_dev_100 = ifelse(land_use_100 == "24", 1, 0),
         agriculture_100 = ifelse(land_use_100 %in% c("81", "82"), 1, 0), 
         
         #' open space = open water, open developed, shrubland, grassland, wetlands
         #' agriculture = pasture and cropland
         
         open_space_250 = ifelse(land_use_250 %in% c("11", "21", "52", "71", "90", "85"), 1, 0),  
         low_inten_dev_250 = ifelse(land_use_250 == "22", 1, 0),
         med_inten_dev_250 = ifelse(land_use_250 == "23", 1, 0),
         high_inten_dev_250 = ifelse(land_use_250 == "24", 1, 0),
         agriculture_250 = ifelse(land_use_250 %in% c("81", "82"), 1, 0),
         
         open_space_500 = ifelse(land_use_500 %in% c("11", "21", "52", "71", "90", "85"), 1, 0), 
         low_inten_dev_500 = ifelse(land_use_500 == "22", 1, 0),
         med_inten_dev_500 = ifelse(land_use_500 == "23", 1, 0),
         high_inten_dev_500 = ifelse(land_use_500 == "24", 1, 0),
         agriculture_500 = ifelse(land_use_500 %in% c("81", "82"), 1, 0), 
         
         open_space_1000 = ifelse(land_use_1000 %in% c("11", "21", "52", "71", "90", "85"), 1, 0),  
         low_inten_dev_1000 = ifelse(land_use_1000 == "22", 1, 0),
         med_inten_dev_1000 = ifelse(land_use_1000 == "23", 1, 0),
         high_inten_dev_1000 = ifelse(land_use_1000 == "24", 1, 0),
         agriculture_1000 = ifelse(land_use_1000 %in% c("81", "82"), 1, 0), 
         
         open_space_2500 = ifelse(land_use_2500 %in% c("11", "21", "52", "71", "90", "85"), 1, 0),  
         low_inten_dev_2500 = ifelse(land_use_2500 == "22", 1, 0),
         med_inten_dev_2500 = ifelse(land_use_2500 == "23", 1, 0),
         high_inten_dev_2500 = ifelse(land_use_2500 == "24", 1, 0),
         agriculture_2500 = ifelse(land_use_2500 %in% c("81", "82"), 1, 0)) %>%  
  
  #' drop all categorical variables
  select(-c(land_use_50:land_use_2500))

summary(lur_data1)
ncol(lur_data1)

#' Write out the cleaned dataset
write_csv(lur_data1, here::here("Data", "Filter_Data_for_LUR.csv"))

today <- Sys.Date()
write_csv(lur_data1, here::here("Data/Archived_Data", 
                                paste0("Filter_Data_for_LUR", today, ".csv")))

#' -----------------------------------------------------------------------------
#' Aggregate the filter data up to the month level
#'     -First, create a long dataset where the values are separated by days
#'     -Second, use summarize to aggregate to the monthly level
#'         -both for PM and BC and all of the covariates
#'         -Won't matter for SP, but will matter for ST
#' -----------------------------------------------------------------------------

lur_month_day <-  data.frame()

for(i in 1:nrow(lur_data)) {
  df <- slice(lur_data, i)
  start <- df$StartDateLocal
  end <- df$EndDateLocal
  
  date_seq <- seq.Date(start, end, by = "day")
  
  temp <- data.frame(campaign = df$campaign,
                     filter_id = df$filter_id,
                     date_seq = date_seq)
  lur_month_day <- bind_rows(lur_month_day, temp)
}
  
lur_month_day <- left_join(lur_month_day, lur_data, by = c("campaign", "filter_id")) %>% 
  mutate(month = month(date_seq),
         year = year(date_seq))

names(lur_month_day)

#' Function to get the mode of a vector
get_mode <- function(x, na.rm = T) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#' Means for filter measurements continuous covariates 
lur_month_means <- lur_month_day %>% 
  select(lon, lat, month, year,
         pm_ug_m3_raw, pm_ug_m3_use, pm_ug_m3_lm, pm_ug_m3_dem, pm_ug_m3_adjlm, 
         bc_ug_m3_raw, bc_ug_m3_use, bc_ug_m3_lm, bc_ug_m3_dem, bc_ug_m3_adjlm, 
         Al_ug_m3:Mn_ug_m3, 
         elevation_50:impervious_2500, pop_den_50:aadt_2500,
         nearest_neighbor_pm, nearest_3_neighbors_pm,
         nearest_neighbor_bc, nearest_3_neighbors_bc,
         nearest_neighbor_temp, nearest_3_neighbors_temp) %>% 
  group_by(lon, lat, month, year) %>%
  summarize_all(list(mean), na.rm = T)

#' Binary for any smoke at a filter during the month
lur_month_smoke <- lur_month_day %>% 
  select(lon, lat, month, year, nearest_neighbor_smoke, nearest_3_neighbors_smoke) %>% 
  group_by(lon, lat, month, year) %>%
  summarize_all(list(sum), na.rm = T) %>% 
  mutate(nearest_neighbor_smoke = ifelse(nearest_neighbor_smoke > 0, 1, 0),
         nearest_3_neighbors_smoke = ifelse(nearest_3_neighbors_smoke > 0, 1, 0))

#' Modes for non-time-varying categorical variables
lur_month_modes <- lur_month_day %>% 
  select(lon, lat, month, year, land_use_50:land_use_2500) %>% 
  group_by(lon, lat, month, year) %>%
  summarize_all(list(get_mode))
  
#' Join these all together
lur_data2 <- left_join(lur_month_means, lur_month_smoke, by = c("lon", "lat", "month", "year")) %>% 
  left_join(lur_month_modes, by = c("lon", "lat", "month", "year"))

glimpse(lur_data2)
table(lur_data2$month)

#' Do some data cleaning and make the indicator variables
lur_data2 <- lur_data2 %>% 
  
  #' reclassify categorical variables by creating dummy variables
  mutate(jan = ifelse(month == 1, 1, 0),
         feb = ifelse(month == 2, 1, 0),
         mar = ifelse(month == 3, 1, 0),
         apr = ifelse(month == 4, 1, 0),
         may = ifelse(month == 5, 1, 0),
         jun = ifelse(month == 6, 1, 0),
         jul = ifelse(month == 7, 1, 0),
         aug = ifelse(month == 8, 1, 0),
         sep = ifelse(month == 9, 1, 0),
         oct = ifelse(month == 10, 1, 0),
         nov = ifelse(month == 11, 1, 0),
         dec = ifelse(month == 12, 1, 0)) %>%

  mutate(winter = ifelse(month %in% c(12, 1, 2), 1, 0),
         spring = ifelse(month %in% c(3, 4, 5), 1, 0),
         summer = ifelse(month %in% c(6, 7, 8), 1, 0),
         autumn = ifelse(month %in% c(9, 10, 11), 1, 0)) %>%
  
  #' land use indicator variables: open space, ag, low, med, high intensity = 1, 2, 3, 4, 5
  mutate(open_space_50 = ifelse(land_use_50 %in% c("11", "21", "52", "71", "90", "85"), 1, 0),  
         low_inten_dev_50 = ifelse(land_use_50 == "22", 1, 0),
         med_inten_dev_50 = ifelse(land_use_50 == "23", 1, 0),
         high_inten_dev_50 = ifelse(land_use_50 == "24", 1, 0),
         agriculture_50 = ifelse(land_use_50 %in% c("81", "82"), 1, 0), 
    
         open_space_100 = ifelse(land_use_100 %in% c("11", "21", "52", "71", "90", "85"), 1, 0),  
         low_inten_dev_100 = ifelse(land_use_100 == "22", 1, 0),
         med_inten_dev_100 = ifelse(land_use_100 == "23", 1, 0),
         high_inten_dev_100 = ifelse(land_use_100 == "24", 1, 0),
         agriculture_100 = ifelse(land_use_100 %in% c("81", "82"), 1, 0), 
         
         #' open space = open water, open developed, shrubland, grassland, wetlands
         #' agriculture = pasture and cropland
         
         open_space_250 = ifelse(land_use_250 %in% c("11", "21", "52", "71", "90", "85"), 1, 0),  
         low_inten_dev_250 = ifelse(land_use_250 == "22", 1, 0),
         med_inten_dev_250 = ifelse(land_use_250 == "23", 1, 0),
         high_inten_dev_250 = ifelse(land_use_250 == "24", 1, 0),
         agriculture_250 = ifelse(land_use_250 %in% c("81", "82"), 1, 0),
         
         open_space_500 = ifelse(land_use_500 %in% c("11", "21", "52", "71", "90", "85"), 1, 0), 
         low_inten_dev_500 = ifelse(land_use_500 == "22", 1, 0),
         med_inten_dev_500 = ifelse(land_use_500 == "23", 1, 0),
         high_inten_dev_500 = ifelse(land_use_500 == "24", 1, 0),
         agriculture_500 = ifelse(land_use_500 %in% c("81", "82"), 1, 0), 
         
         open_space_1000 = ifelse(land_use_1000 %in% c("11", "21", "52", "71", "90", "85"), 1, 0),  
         low_inten_dev_1000 = ifelse(land_use_1000 == "22", 1, 0),
         med_inten_dev_1000 = ifelse(land_use_1000 == "23", 1, 0),
         high_inten_dev_1000 = ifelse(land_use_1000 == "24", 1, 0),
         agriculture_1000 = ifelse(land_use_1000 %in% c("81", "82"), 1, 0), 
         
         open_space_2500 = ifelse(land_use_2500 %in% c("11", "21", "52", "71", "90", "85"), 1, 0),  
         low_inten_dev_2500 = ifelse(land_use_2500 == "22", 1, 0),
         med_inten_dev_2500 = ifelse(land_use_2500 == "23", 1, 0),
         high_inten_dev_2500 = ifelse(land_use_2500 == "24", 1, 0),
         agriculture_2500 = ifelse(land_use_2500 %in% c("81", "82"), 1, 0)) %>% 
  
  #' Drop land use categorical variables
  select(-c(land_use_50:land_use_2500))

summary(lur_data2)
summary(lur_data2$pm_ug_m3_use)
summary(lur_data2$bc_ug_m3_use)

lur_data2 <- filter(lur_data2, !is.na(bc_ug_m3_use))
summary(lur_data2$bc_ug_m3_use)

ncol(lur_data2)

#' Write out the cleaned dataset
write_csv(lur_data2, here::here("Data", "Monthly_Data_for_LUR.csv"))

today <- Sys.Date()
write_csv(lur_data2, here::here("Data/Archived_Data", 
                                paste0("Monthly_Data_for_LUR", today, ".csv")))
