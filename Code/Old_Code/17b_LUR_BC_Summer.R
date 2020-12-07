#' =============================================================================
#' Project: ECHO LUR
#' Date created: January 18, 2019
#' Date updated: September 5, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' Preliminary LUR models for BC
#' 
#' In the first approach we subset the variables and then use LASSO to generate 
#' the final model.
#' 
#' In the second approach we use the CAST package in R to do a random forest
#' model and validate using leave-location-out and leave-time-out CV
#' 
#' The final model is saved as a .rdata for use later to predict grid
#' 
#' Update 12/2/19: Because of some of the seasonal differences I've been seeing
#' in the data, I'm now doing to fit separate seasonal models for the LUR 
#' 
#' This script focuses on summer (June, July, and August)
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
#' Read in the dataset-- cleaned with dummy variables
#' Aggregated to the monthly level at each location (lon/lat)
#' 
#' Subset to just summer months
#' -----------------------------------------------------------------------------

lur_data <- read_csv(here::here("Data", "Monthly_Data_for_LUR.csv")) %>% 
  filter(month %in% c(6, 7, 8))
glimpse(lur_data)

#' Which variable should we use? See 16_LUR_Data_set_Cleaned.R
#' _dem are the calibrated TWAs based on deming regression

lur_data$bc_ug_m3 <- lur_data$bc_ug_m3_dem
summary(lur_data$bc_ug_m3)
sd(lur_data$bc_ug_m3)

#' Check distribution of BC
#' BC and log_transformed BC
ggplot(lur_data) +
  ggtitle("BC") +
  geom_histogram(aes(x = bc_ug_m3)) +
  simple_theme

ggplot(lur_data) +
  ggtitle("BC") +
  geom_qq(aes(sample = bc_ug_m3)) +
  geom_qq_line(aes(sample = bc_ug_m3)) +
  simple_theme

ggplot(lur_data) +
  ggtitle("Log-transformed BC") +
  geom_histogram(aes(x = log(bc_ug_m3))) +
  simple_theme

ggplot(lur_data) +
  ggtitle("Log-transformed BC") +
  geom_qq(aes(sample = log(bc_ug_m3))) +
  geom_qq_line(aes(sample = log(bc_ug_m3))) +
  simple_theme

#' ----------------------------------------------------------------------------- 
#' Look at some preliminary models using only selected predictors
#' ----------------------------------------------------------------------------- 

bc_lur_data <- lur_data %>% 
  select(lon, lat, bc_ug_m3, elevation_50:aadt_2500,
         nearest_neighbor_pm:nearest_3_neighbors_smoke,
         open_space_50:agriculture_2500)
glimpse(bc_lur_data)
summary(bc_lur_data)

#' Save this here for predictions and mapping later!
bc_locations <- bc_lur_data %>% 
  mutate(lon2 = lon, lat2 = lat) %>% 
  st_as_sf(coords = c("lon2", "lat2"), crs = ll_wgs84) %>% 
  st_transform(crs = albers)

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
#' 99
ncol(select(bc_lur_data, -c(lon, lat, bc_ug_m3)))

#' Looking at the continuous predictors now
names(bc_lur_data)
bc_continuous <- select(bc_lur_data, elevation_50:nearest_3_neighbors_temp)

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
#' Although technically nearest_3_neighbors temp did't make the cut,
#' going to keep them in to force them into the model
bc_continuous_drop <- filter(bc_continuous_cv, pct_diff < 0.10 | cv < 0.10)
bc_continuous_drop

drop_vars2 <- bc_continuous_drop$key[c(1:6,8)]
drop_vars2

#' Calculate correlations
bc_continuous_cor <- cor(bc_continuous, use = "complete")
ggcorrplot(bc_continuous_cor, type = "upper", method = "square",
           ggtheme = simple_theme, lab = T, lab_col = "white",
           show.diag = T)

#' Which variables don't meet the 10% variability criterion?
#' Use pop density; drop pop count
#' Drop tree cover in 50 m
#' Drop AADT in 100 m
bc_continuous_cor[bc_continuous_cor < 0.95] <- ""
View(bc_continuous_cor)

drop_vars2 <- c(drop_vars2, "tree_cover_50", "aadt_100",
                "pop_ct_50", "pop_ct_100", "pop_ct_250",
                "pop_ct_500", "pop_ct_1000", "pop_ct_2500",
                "aadt_50")
drop_vars2

#' Which are the reference values for the land use category variables? open space
drop_vars2 <- c(drop_vars2, 
                "open_space_50", "open_space_100", "open_space_250",
                "open_space_500", "open_space_1000", "open_space_2500",
                "nearest_neighbor_bc", "nearest_neighbor_temp", 
                "nearest_neighbor_pm", "nearest_neighbor_smoke")  
drop_vars2

#' Now how many predictors do we have?
#' 75 candidate predictors
bc_lur_data2 <- bc_lur_data %>%
  select(-c(lon, lat)) %>% 
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

bc_lm1a <- lm(bc_ug_m3 ~ aadt_250, data = bc_lur_data2)
summary(bc_lm1a)
par(mfrow=c(2,2)) 
plot(bc_lm1a, main = "Just AADT within 250 m: untransformed outcome")
par(mfrow=c(1,1))
hist(bc_lm1a$residuals)

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' METHOD 1: Log-transformed BC
#' Use RIDGE, LASSO and EN to select predictors using the caret package in R
#' Test stepwise AIC method to further reduce the number of predictors
#' ----------------------------------------------------------------------------- 
#' -----------------------------------------------------------------------------

#' Which predictors are we starting with?
start_predictors <- names(select(bc_lur_data2, -bc_ug_m3))
start_predictors

bc_lur_data3 <- select(bc_lur_data, bc_ug_m3, start_predictors) 
summary(bc_lur_data3)

library(caret)
library(glmnet)

# Set seed for reproducibility
set.seed(100)

#' Compare some alternatives for reducing the number of variables
#' First up, RIDGE
#' Coefficients don't shrink to zero, so not able to reduce predictors
lambda <- 10^seq(-3, 3, length = 100)
tc_r = trainControl("cv", number = 10)
tg_r = expand.grid(alpha = 0, lambda = lambda)

log_bc_ridge <- train(log(bc_ug_m3) ~ ., data = bc_lur_data3,
                      method = "glmnet", trControl = tc_r, tuneGrid = tg_r)

log_bc_ridge$resample
sd(log(bc_lur_data3$bc_ug_m3))

plot(log_bc_ridge)
getTrainPerf(log_bc_ridge)
plot(log_bc_ridge$finalModel)
arrange(log_bc_ridge$results, RMSE) %>% head
log_bc_ridge$bestTune
log_bc_ridge_coef <- coef(log_bc_ridge$finalModel, log_bc_ridge$bestTune$lambda)
log_bc_ridge_coef

#' Next, LASSO
#' When alpha = 1, LASSO is implemented
#' Coefficients CAN shrink to 0 in this one
lambda <- 10^seq(-3, 3, length = 100)
tc_l = trainControl("cv", number = 10)
tg_l = expand.grid(alpha = 1, lambda = lambda)

log_bc_lasso <- train(log(bc_ug_m3) ~ ., data = bc_lur_data3,
                      method = "glmnet", trControl = tc_l, tuneGrid = tg_l)

log_bc_lasso$resample
sd(log(bc_lur_data3$bc_ug_m3))

plot(log_bc_lasso)
getTrainPerf(log_bc_lasso)
plot(log_bc_lasso$finalModel)
arrange(log_bc_lasso$results, RMSE) %>% head
log_bc_lasso$bestTune
log_bc_lasso_coef<- coef(log_bc_lasso$finalModel, log_bc_lasso$bestTune$lambda)
log_bc_lasso_coef

varImp(log_bc_lasso)

#' Try elastic net for variable selection
#' Elastic net combines RIDGE and LASSO
#' Uses two tuning parameters: lambda and alpha
#' Note that the model requires a "centered" variable and "standardized" 
#' predictors, so we center standardize as part of the model specification 
#' (preProcess)

tc_e <- trainControl("cv", number = 10)
log_bc_glmnet <- train(log(bc_ug_m3) ~ ., data = bc_lur_data3, 
                       preProcess = c("center", "scale"),
                       method = "glmnet", trControl = tc_e)
log_bc_glmnet
plot(log_bc_glmnet)
getTrainPerf(log_bc_glmnet)
plot(log_bc_glmnet$finalModel)
arrange(log_bc_glmnet$results, RMSE) %>% head
log_bc_glmnet$bestTune
log_bc_enet_coef <- coef(log_bc_glmnet$finalModel, log_bc_glmnet$bestTune$lambda)
log_bc_enet_coef

#' Compare these three models
#' look for smallest median RMSE and largest Rsquared
log_bc_mods <- resamples(list(ridge = log_bc_ridge, lasso = log_bc_lasso, en = log_bc_glmnet))
summary(log_bc_mods)

#' ----------------------------------------------------------------------------- 
#' Try using the LASSO coefficients in a stepwise-AIC linear model
#' log-transformed BC here
#' ----------------------------------------------------------------------------- 

log_bc_lasso_coef_df <- data.frame(name = log_bc_lasso_coef@Dimnames[[1]][log_bc_lasso_coef@i + 1], 
                                   coefficient = log_bc_lasso_coef@x)
log_bc_lasso_coef_df
log_bc_lasso_list <- as.character(log_bc_lasso_coef_df$name[-1])
log_bc_lasso_list

bc_lur_data4 <- select(bc_lur_data, bc_ug_m3, log_bc_lasso_list)
names(bc_lur_data4)

#' Examine relationships with predictors
cor(bc_lur_data4$tree_cover_100, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$impervious_250, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$impervious_1000, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$dist_m_military, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$dist_m_mines, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$dist_m_npl_sites, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$dist_m_og_wells, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$len_m_highways_50, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$len_m_highways_100, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$len_m_highways_500, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$len_m_highways_2500, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$len_m_major_roads_50, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$aadt_500, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$aadt_2500, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$nearest_3_neighbors_pm, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$nearest_3_neighbors_bc, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$nearest_3_neighbors_temp, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$nearest_3_neighbors_smoke, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$med_inten_dev_50, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$high_inten_dev_50, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$agriculture_50, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$low_inten_dev_100, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$high_inten_dev_100, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$agriculture_100, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$low_inten_dev_250, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$agriculture_250, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$med_inten_dev_500, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$med_inten_dev_1000, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$agriculture_2500, log(bc_lur_data4$bc_ug_m3))

plot(bc_lur_data4$tree_cover_100, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$impervious_250, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$impervious_1000, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$dist_m_military, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$dist_m_mines, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$dist_m_npl_sites, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$dist_m_og_wells, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$len_m_highways_50, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$len_m_highways_100, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$len_m_highways_500, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$len_m_highways_2500, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$len_m_major_roads_50, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$aadt_500, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$aadt_2500, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$nearest_3_neighbors_pm, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$nearest_3_neighbors_bc, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$nearest_3_neighbors_temp, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$nearest_3_neighbors_smoke, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$med_inten_dev_50, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$high_inten_dev_50, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$agriculture_50, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$low_inten_dev_100, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$high_inten_dev_100, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$agriculture_100, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$low_inten_dev_250, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$agriculture_250, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$med_inten_dev_500, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$med_inten_dev_1000, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$agriculture_2500, log(bc_lur_data4$bc_ug_m3))

#' fit the AIC model with 10-fold CV
tc_a = trainControl("cv", number = 10)
log_bc_aic <- train(log(bc_ug_m3) ~ ., data = select(bc_lur_data, bc_ug_m3, log_bc_lasso_list), 
                    method = "lmStepAIC", trace = FALSE, trControl = tc_a)

summary(log_bc_aic)
log_bc_aic$results

log_bc_aic_list <- rownames(summary(log_bc_aic)[["coefficients"]])[-1]
log_bc_aic_list 

#' ----------------------------------------------------------------------------- 
#' Compare models with AIC-selected coefficients and LASSO-selected coefficients
#' Validate the LASSO and AIC models with 10-fold cross validation
#' ----------------------------------------------------------------------------- 

log_bc_lasso_list # 32 predictors
log_bc_aic_list  # 21 predictors

tc <- trainControl(method = "cv", number = 10)

#' 10-fold cross validation: LASSO-selected predictors
#' R2 = 0.83; RMSE = 0.06
log_bc_lasso_cv <- train(log(bc_ug_m3) ~ ., 
                         data = select(bc_lur_data4, bc_ug_m3, log_bc_lasso_list),
                         method = "lm", trControl = tc)
log_bc_lasso_cv
log_bc_lasso_cv$finalModel
varImp(log_bc_lasso_cv)

bc_lur_data4$pred_log_bc_lasso <- predict(log_bc_lasso_cv)

#' 10-fold cross validation: AIC-selected predictors
#' R2 = 0.86; RMSE = 0.05
log_bc_aic_cv <- train(log(bc_ug_m3) ~ ., 
                       data = select(bc_lur_data4, bc_ug_m3, log_bc_aic_list),
                       method = "lm", trControl = tc)
log_bc_aic_cv
log_bc_aic_cv$finalModel
varImp(log_bc_aic_cv)

bc_lur_data4$pred_log_bc_aic <- predict(log_bc_aic_cv)

ggplot(bc_lur_data4) +
  geom_point(aes(x = log(bc_ug_m3), y = pred_log_bc_aic, col = "aic", shape = "aic")) +
  geom_point(aes(x = log(bc_ug_m3), y = pred_log_bc_lasso, col = "las", shape = "las")) +
  geom_abline(aes(slope = 1, intercept = 0), color = "black", linetype = 2) +
  scale_color_viridis(name = "Model", discrete = T,
                      labels = c("aic" = "AIC", "las" = "LASSO")) +
  scale_shape_manual(name = "Model",
                     values = c("aic" = 15, "las" = 16),
                     labels = c("aic" = "AIC", "las" = "LASSO")) +
  xlab("Measured log(BC)") + ylab("Predicted log(BC)") +
  simple_theme
ggsave(filename = here::here("Figs/Model_Dev", "Log_BC_Meas_vs_Pred_Summer.jpeg"), 
       device = "jpeg", width = 5, height = 5, units = "in")


#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Save the models!
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------

sum_log_bc_aic_cv <- log_bc_aic_cv
sum_log_bc_lasso_cv <- log_bc_lasso_cv

save(sum_log_bc_aic_cv, sum_log_bc_lasso_cv, 
     file = here::here("Results", "BC_LUR_Models_Summer.rdata"))

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Predict concentrations for the grid cells
#' Use the AIC model and log-transformed BC
#' ----------------------------------------------------------------------------- 
#' -----------------------------------------------------------------------------

load(here::here("Results", "BC_LUR_Models_Summer.rdata"))

grid_data <- read_csv(here::here("Data", "Combined_Grid_Data_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

bc_cols <- names(sum_log_bc_aic_cv$finalModel$coefficients)[-1]
bc_cols

grid_data2 <- select(grid_data, month, year, bc_cols) %>% 
  filter(month %in% c(6, 7, 8)) %>% 
  na.omit()

grid_data2$pred_log_bc <- predict(sum_log_bc_aic_cv, newdata = grid_data2)

#' -----------------------------------------------------------------------------
#' Make some maps to check things out
#' ----------------------------------------------------------------------------- 

map_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Arial",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))

#' June 2018
df_plot1 <- filter(grid_data2, month == 6 & year == 2018)

st_bbox(df_plot1)
x_min <- unname(st_bbox(df_plot1)["xmin"])
x_max <- unname(st_bbox(df_plot1)["xmax"]) 
y_min <- unname(st_bbox(df_plot1)["ymin"])
y_max <- unname(st_bbox(df_plot1)["ymax"])

plot1 <- ggplot(df_plot1) +
  geom_sf(aes(fill = pred_log_bc), color = NA) +
  scale_fill_viridis(name = paste("log(BC):", 
                                  "Jun 2018",
                                  sep = "\n")) +
  north(x.min = x_min, x.max = x_max,
        y.min =  y_min, y.max = y_max,
        symbol = 12, location = "topleft", scale = 0.05,
        anchor = c(x = x_min - x_min*0.005,
                   y = y_max - y_max*0.001)) +
  scalebar(x.min = x_min, x.max = x_max,
           y.min =  y_min, y.max = y_max,
           dist_unit = "km", dist = 5, transform = F,
           st.bottom = F, st.size = 3, st.color = "white",
           box.color = "white", height = 0.01,
           location = "bottomleft",
           anchor = c(x = x_min - x_min*0.005,
                      y = y_min + y_min*0.001)) +
  xlab("") + ylab("") +
  map_theme
plot1

plot_name1 <- paste0("BC_Jun_2018_Summer.jpeg")
ggsave(here::here("Figs", plot_name1),
       device = "jpeg", height= 5, width = 6, dpi = 500, units = "in")

#' July, 2018
df_plot2 <- filter(grid_data2, month == 7 & year == 2018)

st_bbox(df_plot2)
x_min <- unname(st_bbox(df_plot2)["xmin"])
x_max <- unname(st_bbox(df_plot2)["xmax"]) 
y_min <- unname(st_bbox(df_plot2)["ymin"])
y_max <- unname(st_bbox(df_plot2)["ymax"])

plot2 <- ggplot(df_plot2) +
  geom_sf(aes(fill = pred_log_bc), color = NA) +
  scale_fill_viridis(name = paste("log(BC):",  
                                  "Jul 2018",
                                  sep = "\n")) +
  north(x.min = x_min, x.max = x_max,
        y.min =  y_min, y.max = y_max,
        symbol = 12, location = "topleft", scale = 0.05,
        anchor = c(x = x_min - x_min*0.005,
                   y = y_max - y_max*0.001)) +
  scalebar(x.min = x_min, x.max = x_max,
           y.min =  y_min, y.max = y_max,
           dist_unit = "km", dist = 5, transform = F,
           st.bottom = F, st.size = 3, st.color = "white",
           box.color = "white", height = 0.01,
           location = "bottomleft",
           anchor = c(x = x_min - x_min*0.005,
                      y = y_min + y_min*0.001)) +
  xlab("") + ylab("") +
  map_theme
plot2

plot_name2 <- paste0("BC_Jul_2018_Summer.jpeg")
ggsave(here::here("Figs", plot_name2),
       device = "jpeg", height= 5, width = 6, dpi = 500, units = "in")

#' August, 2018
df_plot3 <- filter(grid_data2, month == 8 & year == 2018)

st_bbox(df_plot3)
x_min <- unname(st_bbox(df_plot3)["xmin"])
x_max <- unname(st_bbox(df_plot3)["xmax"]) 
y_min <- unname(st_bbox(df_plot3)["ymin"])
y_max <- unname(st_bbox(df_plot3)["ymax"])

plot3 <- ggplot(df_plot3) +
  geom_sf(aes(fill = pred_log_bc), color = NA) +
  scale_fill_viridis(name = paste("log(BC):", 
                                  "Aug 2018",
                                  sep = "\n")) +
  north(x.min = x_min, x.max = x_max,
        y.min =  y_min, y.max = y_max,
        symbol = 12, location = "topleft", scale = 0.05,
        anchor = c(x = x_min - x_min*0.005,
                   y = y_max - y_max*0.001)) +
  scalebar(x.min = x_min, x.max = x_max,
           y.min =  y_min, y.max = y_max,
           dist_unit = "km", dist = 5, transform = F,
           st.bottom = F, st.size = 3, st.color = "white",
           box.color = "white", height = 0.01,
           location = "bottomleft",
           anchor = c(x = x_min - x_min*0.005,
                      y = y_min + y_min*0.001)) +
  xlab("") + ylab("") +
  map_theme
plot3

plot_name3 <- paste0("BC_Aug_2018_Summer.jpeg")
ggsave(here::here("Figs", plot_name3),
       device = "jpeg", height= 5, width = 6, dpi = 500, units = "in")