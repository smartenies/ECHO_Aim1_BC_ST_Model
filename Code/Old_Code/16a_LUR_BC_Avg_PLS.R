#' =============================================================================
#' Project: ECHO LUR
#' Date Created: April 22, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' Preliminary LUR for BC
#' Averaging all data at the sampling locations-- essentially taking time out
#' of the equation. What do long-term spatial trends in the data look like? Can
#' we reasonably predict them using the variables we have?
#' 
#' Updated 3/10/20:
#' We need to have a strategy for choosing our predictors before fitting the 
#' spatiotemporal model (based on Josh Keller's work with the MESA Air study). 
#' He used partial least squares regression in his work, which is the ultimate
#' plan for this study. However, given a March 16 deadline to submit an extended
#' abstract, I'm going to use LASSO for now and base the selection of predictors
#' on the "averaged" model
#' 
#' Updated 4/22/20:
#' This script attempts to fit a PLS model to identify the best set of spatial
#' predictors to use in the ST model. 
#' 
#' To keep things simple for now, I'm going to use the long-term average BC 
#' concentration measured at each site (similar to Sampson et al., 2013). 
#' Keller et al. (2016) came up with a way to use the spatiotemporal data, so 
#' that might be a next step
#' 
#' To try to achieve this, I'm following the tutorial outlined here:
#' https://rpubs.com/omicsdata/pls
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

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Arial",size = 12, color = 'black'),
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

#' -----------------------------------------------------------------------------
#' Read in the dataset-- 
#' Aggregate to the "annual" level at each location (using lon/lat as the ID)
#' -----------------------------------------------------------------------------

data_name <- "Combined_Filter_Data_AEA.csv"
lur_data <- read_csv(here::here("Data", data_name)) %>% 
  filter(!is.na(lon)) %>% 
  filter(indoor == 0) %>% 
  filter(bc_ug_m3_dem > 0)

#' Select a "calibrated" version of the data
#' For now, go with Deming regression-- accounts for variability in the
#' monitor and the UPAS data

lur_data <- lur_data %>% 
  rename("pm_ug_m3_raw" = "pm_ug_m3") %>% 
  rename("pm_ug_m3" = "pm_ug_m3_dem") %>% 
  rename("bc_ug_m3_raw" = "bc_ug_m3") %>% 
  rename("bc_ug_m3" = "bc_ug_m3_dem")

dist_data <- filter(lur_data, filter_id != "080310027")
central_data <- filter(lur_data, filter_id == "080310027")

#' Add a unique site ID
lur_data <- mutate(lur_data, site_id_lonlat = paste(lon, lat, sep = "_"))

ids <- select(lur_data, site_id_lonlat) %>% 
  distinct() %>% 
  mutate(site_id = seq_along(site_id_lonlat))

lur_data <- left_join(lur_data, ids, by = "site_id_lonlat") %>% 
  mutate(site_id = ifelse(filter_id == "080310027", "central", site_id))

#' Study-wide data for each sampling location
site_sp <- select(lur_data, site_id, 
                  elevation_50:impervious_2500,open_50:aadt_2500) %>% 
  distinct()

site_st <- select(lur_data, site_id, bc_ug_m3, nn_pm, nn_no2, nn_bc, nn_temp,
                  area_pm, area_no2, area_temp, idw_pm, idw_no2, idw_temp) %>% 
  filter(!is.na(bc_ug_m3))  %>% 
  group_by(site_id) %>% 
  summarize(bc_ug_m3 = mean(bc_ug_m3, na.rm = T),
            nn_pm = mean(nn_pm, na.rm=T),
            nn_no2 = mean(nn_no2, na.rm=T),
            nn_temp = mean(nn_temp, na.rm=T),
            nn_bc = mean(nn_bc, na.rm=T),
            area_pm = mean(area_pm, na.rm=T),
            area_no2 = mean(area_no2, na.rm=T),
            area_temp = mean(area_temp, na.rm=T),
            idw_pm = mean(idw_pm, na.rm=T),
            idw_no2 = mean(idw_no2, na.rm=T),
            idw_temp = mean(idw_temp, na.rm=T))

site_data <- left_join(site_sp, site_st, by = "site_id")
head(site_data)

names(site_data)
cor(site_data[,-c(1)])

#' -----------------------------------------------------------------------------
#' Check distribution of BC
#' -----------------------------------------------------------------------------

#' BC and log_transformed BC
ggplot(site_data) +
  ggtitle("BC") +
  geom_histogram(aes(x = bc_ug_m3)) +
  simple_theme

ggplot(site_data) +
  ggtitle("BC") +
  geom_qq(aes(sample = bc_ug_m3)) +
  geom_qq_line(aes(sample = bc_ug_m3)) +
  simple_theme

ggplot(site_data) +
  ggtitle("Log-transformed BC") +
  geom_histogram(aes(x = log(bc_ug_m3))) +
  simple_theme

ggplot(site_data) +
  ggtitle("Log-transformed BC") +
  geom_qq(aes(sample = log(bc_ug_m3))) +
  geom_qq_line(aes(sample = log(bc_ug_m3))) +
  simple_theme

#' ----------------------------------------------------------------------------- 
#' Look at some preliminary models using only selected spatial predictors
#' ----------------------------------------------------------------------------- 

bc_lur_data <- site_data %>% 
  dplyr::select(bc_ug_m3, tree_cover_50:pop_den_2500,dist_m_airport:aadt_2500)
glimpse(bc_lur_data)
summary(bc_lur_data)

#' ----------------------------------------------------------------------------- 
#' ----------------------------------------------------------------------------- 
#' Narrow the list of candidate predictors
#' Fit the PLS model
#' Identify the best component
#' ----------------------------------------------------------------------------- 
#' ----------------------------------------------------------------------------- 

library(MASS)
library(caret)
library(lars)
library(pls)

nrow(bc_lur_data) #' number of observations
ncol(bc_lur_data) - 1 #' number of potential predictors

#' log_transform BC
#' Also going to scale the predictors
preds <- colnames(bc_lur_data)[-c(1)]

bc_lur_data <- bc_lur_data %>%
  mutate(log_bc = log(bc_ug_m3)) %>%
  mutate_at(preds, scale) %>%
  dplyr::select(-c(bc_ug_m3))

summary(bc_lur_data$log_bc)
summary(dplyr::select(bc_lur_data, preds))

View(cor(bc_lur_data))

#' -----------------------------------------------------------------------------
#' Fit the PLS model and use it to predict BC
#' -----------------------------------------------------------------------------

#' Try fitting the PLS model
set.seed(123)
bc_pls <- plsr(log_bc ~ ., data = bc_lur_data, validation = "LOO")
bc_pls
summary(bc_pls)

#' Predict using the model
bc_pls_pred <- predict(bc_pls)

# Find the number of dimensions with lowest cross validation error
cv <- RMSEP(bc_pls)
best_mod <- which.min(cv$val[estimate = "adjCV", , ]) - 1
best_mod

# Rerun the model
pls.model = plsr(pref ~ ., data = colas, ncomp = best.dims)

#' -----------------------------------------------------------------------------
#' Examining the results
#' -----------------------------------------------------------------------------

#' PLS RMSEP
validationplot(bc_pls, val.type="RMSEP")
validationplot(bc_pls, val.type="RMSEP", ncomp = c(1:50))

#' PLS R2
validationplot(bc_pls, val.type="R2", ncomp = c(1:50))

#' Idenitfy which number of components has the lowest RMSEP
bc_pls_RMSEP <- RMSEP(bc_pls, estimate="CV")
plot(bc_pls_RMSEP, main="RMSEP PLS", xlab="components")
min_comp <- which.min(bc_pls_RMSEP$val)
points(min_comp, min(bc_pls_RMSEP$val), pch=1, col="red", cex=1.5)
min_comp

#' Plot some of the components
plot(bc_pls, ncomp = 1, asp=1, line=TRUE)
plot(bc_pls, ncomp = 5, asp=1, line=TRUE)
plot(bc_pls, ncomp = 10, asp=1, line=TRUE)
plot(bc_pls, ncomp = 20, asp=1, line=TRUE)

#' -----------------------------------------------------------------------------
#' Save PLS model
#' -----------------------------------------------------------------------------

# save(file = here::here("Results", "BC_PLS_Model.Rdata"))

