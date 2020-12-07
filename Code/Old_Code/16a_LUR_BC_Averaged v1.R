#' =============================================================================
#' Project: ECHO LUR
#' Date Created: Febrary 17, 2020
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

#' Add a unique site ID
lur_data <- mutate(lur_data, site_id_lonlat = paste(lon, lat, sep = "_"))

ids <- select(lur_data, site_id_lonlat) %>% 
  distinct() %>% 
  mutate(site_id = seq_along(site_id_lonlat))

lur_data <- left_join(lur_data, ids, by = "site_id_lonlat")

#' Study-wide data for each sampling location
#' Not considering temperature or wildfire smoke right now
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
  select(site_id, bc_ug_m3, elevation_50:aadt_2500)
glimpse(bc_lur_data)
summary(bc_lur_data)

#' ----------------------------------------------------------------------------- 
#' ----------------------------------------------------------------------------- 
#' Narrow the list of candidate predictors
#' 
#' Update 12.04.19: Going to go with the methods used by the DEEDS team
#' - Remove variables with a %diff (highest to lowest) of <10%
#' - Remove variables with a CV of <0.1
#' - Remove variables if they have a correlation > 0.95 with another variable
#' 
#' Notes:
#' - Technically temp and BC don't meet the criteria, but we want to force these
#'   in the model to make sure that we can hindcast properly
#' ----------------------------------------------------------------------------- 
#' ----------------------------------------------------------------------------- 

#' How many predictors are we starting with?
#' 92
ncol(select(bc_lur_data, -c(bc_ug_m3)))

#' Looking at the continuous predictors now
names(bc_lur_data)
bc_continuous <- select(bc_lur_data, elevation_50:aadt_2500)

#' Calculate CV and percent diff
bc_continuous_cv <- gather(bc_continuous) %>% 
  group_by(key) %>% 
  summarize(mean = mean(value, na.rm=T),
            sd = sd(value, na.rm=T),
            max = max(value, na.rm=T),
            min = min(value, na.rm=T)) %>% 
  mutate(pct_diff = (max - min) / max,
         cv = sd / mean)
as.data.frame(bc_continuous_cv)

#' Which variables don't meet the 10% variability criterion?
#' Although technically nearest_3_neighbors BC doesnt make the cut,
#' going to keep them in to force them into the model
bc_continuous_drop <- filter(bc_continuous_cv, pct_diff < 0.10 | cv < 0.10)
bc_continuous_drop

drop_vars2 <- bc_continuous_drop$key[1:6]
drop_vars2

#' Calculate correlations
bc_continuous_cor <- cor(bc_continuous, use = "complete")
ggcorrplot(bc_continuous_cor, type = "upper", method = "square",
           ggtheme = simple_theme, lab = T, lab_col = "white",
           show.diag = T)

#' Which variables don't meet the r < 0.95 criterion?
#' Use pop density; drop pop count
#' Drop tree cover 50 to tree cover 500
#' Drop AADT in 100 m 
bc_continuous_cor[bc_continuous_cor < 0.95] <- ""
View(bc_continuous_cor)

drop_vars2 <- c(drop_vars2, "tree_cover_50", "tree_cover_100", "tree_cover_250", "tree_cover_500",
                "pop_ct_50", "pop_ct_100", "pop_ct_250", "pop_ct_500", "pop_ct_1000", "pop_ct_2500",
                "aadt_50")
drop_vars2

#' Now how many predictors do we have?
#' 77 candidate predictors
bc_lur_data2 <- bc_lur_data %>%
  select(-c(drop_vars2))
names(bc_lur_data2)

ncol(select(bc_lur_data2, -bc_ug_m3))
summary(bc_lur_data2)

#' Fit some linear regression models
#' Just AADT
bc_lm1 <- lm(log(bc_ug_m3) ~ aadt_250, data = bc_lur_data2)
summary(bc_lm1)
par(mfrow=c(2,2)) 
plot(bc_lm1, main = "Just AADT within 250 m: log-transformed outcome")
par(mfrow=c(1,1))
hist(bc_lm1$residuals)

#' AADT + high intensity dev
bc_lm2 <- lm(log(bc_ug_m3) ~ aadt_250 + high_int_2500, data = bc_lur_data2)
summary(bc_lm2)
par(mfrow=c(2,2)) 
plot(bc_lm2, main = "AADT (250m) and High Intensity Dev (2500): log-transformed outcome")
par(mfrow=c(1,1))
hist(bc_lm2$residuals)

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' METHOD 1: Use LASSO (caret package)
#' 
#' LASSO: Least Absolute Shrinkage and Selection Operator
#' Coefficients CAN shrink to 0 in this one
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------

library(caret)

bc_lur_data4 <- select(bc_lur_data, bc_ug_m3, start_predictors)

lambda <- 10^seq(-3, 3, length = 100)
tc_l = trainControl("cv", number = 10)
tg_l = expand.grid(alpha = 1, lambda = lambda)

log_bc_lasso2 <- train(log(bc_ug_m3) ~ ., data = bc_lur_data4,
                       method = "glmnet", trControl = tc_l, tuneGrid = tg_l)

log_bc_lasso2$resample
plot(log_bc_lasso2)

#' The plot above suggests that the lambda search window is too wide
lambda2 <- 10^seq(-2, 1, length = 100)
tc_l = trainControl("cv", number = 10)
tg_l2 = expand.grid(alpha = 1, lambda = lambda2)

log_bc_lasso3 <- train(log(bc_ug_m3) ~ ., data = bc_lur_data4,
                       method = "glmnet", trControl = tc_l, tuneGrid = tg_l2)

log_bc_lasso3$resample
plot(log_bc_lasso3)

#' The plot above suggests that the lambda search window is still a little too wide
lambda3 <- 10^seq(-2, -0.5, length = 100)
tc_l = trainControl("cv", number = 10)
tg_l3 = expand.grid(alpha = 1, lambda = lambda3)

log_bc_lasso4 <- train(log(bc_ug_m3) ~ ., data = bc_lur_data4,
                       method = "glmnet", trControl = tc_l, tuneGrid = tg_l3)

log_bc_lasso4$resample
plot(log_bc_lasso4)

getTrainPerf(log_bc_lasso4)
plot(log_bc_lasso4$finalModel)
arrange(log_bc_lasso4$results, RMSE) %>% head
log_bc_lasso4$bestTune
log_bc_lasso_coef4 <- coef(log_bc_lasso4$finalModel, log_bc_lasso4$bestTune$lambda)
log_bc_lasso_coef4

varImp(log_bc_lasso4)

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' METHOD 3: Use LASSO (CAST package) with leave-location-out CV
#' 
#' CAST package in R to faciliate model building and validation
#' A good tutorial here: https://cran.r-project.org/web/packages/CAST/vignettes/CAST-intro.html
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------

library(CAST)

bc_lur_data5 <- select(bc_lur_data, site_id, bc_ug_m3, start_predictors)

sp_indices <- CreateSpacetimeFolds(bc_lur_data5, spacevar = "site_id",
                                   k = length(unique(bc_lur_data5$site_id)))
sp_indices

lambda3 <- 10^seq(-2, -0.5, length = 100)
tg_l3 = expand.grid(alpha = 1, lambda = lambda3)

log_bc_lasso5 <- train(log(bc_ug_m3) ~ ., data = bc_lur_data4,
                       method = "glmnet", tuneGrid = tg_l3,
                       trControl = trainControl(method = "cv",
                                                index = sp_indices$index))
plot(log_bc_lasso5)
varImp(log_bc_lasso5)

getTrainPerf(log_bc_lasso5)
plot(log_bc_lasso5$finalModel)
arrange(log_bc_lasso5$results, RMSE) %>% head
log_bc_lasso5$bestTune
log_bc_lasso_coef5 <- coef(log_bc_lasso5$finalModel, log_bc_lasso5$bestTune$lambda)
log_bc_lasso_coef5

varImp(log_bc_lasso5)

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' METHOD 3: Build a random forest model (CAST package) with leave-location-out CV
#' 
#' CAST package in R to faciliate model building and validation
#' A good tutorial here: https://cran.r-project.org/web/packages/CAST/vignettes/CAST-intro.html
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------

predictors <- names(select(bc_lur_data5, -c("bc_ug_m3", "site_id")))
predictors
 
log_bc_rf <- train(bc_lur_data5[,predictors], log(bc_lur_data5$bc_ug_m3),
                   method = "rf", tuneLength = 1, importance = T,
                   trControl = trainControl(method = "cv", 
                                            index = sp_indices$index))
log_bc_rf
plot(varImp(log_bc_rf))

