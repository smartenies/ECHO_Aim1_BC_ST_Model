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
#' Read in the dataset-- cleaned with dummy variables
#' Aggregated to the monthly level at each location (lon/lat)
#' -----------------------------------------------------------------------------

lur_data <- read_csv(here::here("Data", "Monthly_Data_for_LUR.csv"))
glimpse(lur_data)

#' Which variable should we use? See 16_LUR_Data_set_Cleaned.R
#' _dem are the calibrated monthly TWAs based on deming regression

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
         winter, spring, summer, autumn,
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
#' 103
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
#' Although technically nearest_3_neighbors BC and temp don't make the cut,
#' going to keep them in to force them into the model
bc_continuous_drop <- filter(bc_continuous_cv, pct_diff < 0.10 | cv < 0.10)
bc_continuous_drop

drop_vars2 <- bc_continuous_drop$key
drop_vars2

#' Calculate correlations
bc_continuous_cor <- cor(bc_continuous, use = "complete")
ggcorrplot(bc_continuous_cor, type = "upper", method = "square",
           ggtheme = simple_theme, lab = T, lab_col = "white",
           show.diag = T)

#' Which variables don't meet the 10% variability criterion?
#' Use pop density; drop pop count
#' Drop tree cover in 250 m
#' Drop AADT in 100 m 
bc_continuous_cor[bc_continuous_cor < 0.95] <- ""
View(bc_continuous_cor)

drop_vars2 <- c(drop_vars2, "tree_cover_250", "aadt_100",
                "pop_ct_50", "pop_ct_100", "pop_ct_250",
                "pop_ct_500", "pop_ct_1000", "pop_ct_2500")
drop_vars2

#' Which are the reference values for the land use category variables? open space
#' Which is the reference season? Autumn
drop_vars2 <- c(drop_vars2, 
                "open_space_50", "open_space_100", "open_space_250",
                "open_space_500", "open_space_1000", "open_space_2500",
                "autumn")  
drop_vars2

table(bc_lur_data$summer)
table(bc_lur_data$winter)
table(bc_lur_data$spring)

#' Now how many predictors do we have?
#' 83 candidate predictors
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
cor(bc_lur_data4$pop_den_50, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$dist_m_cafos, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$dist_m_og_wells, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$len_m_major_roads_100, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$aadt_500, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$nearest_3_neighbors_bc, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$nearest_3_neighbors_temp, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$winter, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$spring, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$summer, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$low_inten_dev_50, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$low_inten_dev_100, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$high_inten_dev_100, log(bc_lur_data4$bc_ug_m3))
cor(bc_lur_data4$med_inten_dev_1000, log(bc_lur_data4$bc_ug_m3))

plot(bc_lur_data4$tree_cover_100, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$pop_den_50, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$dist_m_cafos, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$dist_m_og_wells, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$len_m_major_roads_100, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$aadt_500, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$nearest_3_neighbors_bc, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$nearest_3_neighbors_temp, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$winter, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$spring, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$summer, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$low_inten_dev_50, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$low_inten_dev_100, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$high_inten_dev_100, log(bc_lur_data4$bc_ug_m3))
plot(bc_lur_data4$med_inten_dev_1000, log(bc_lur_data4$bc_ug_m3))

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

log_bc_lasso_list # 15 predictors
log_bc_aic_list  # 8 predictors

tc <- trainControl(method = "cv", number = 10)

#' 10-fold cross validation: LASSO-selected predictors
#' R2 = 0.44; RMSE = 0.25
log_bc_lasso_cv <- train(log(bc_ug_m3) ~ ., 
                         data = select(bc_lur_data4, bc_ug_m3, log_bc_lasso_list),
                         method = "lm", trControl = tc)
log_bc_lasso_cv
log_bc_lasso_cv$finalModel
varImp(log_bc_lasso_cv)

bc_lur_data4$pred_log_bc_lasso <- predict(log_bc_lasso_cv)

#' 10-fold cross validation: AIC-selected predictors
#' R2 = 0.44; RMSE = 0.25
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
ggsave(filename = here::here("Figs/Model_Dev", "Log_BC_Meas_vs_Pred_All_Obs.jpeg"), 
       device = "jpeg", width = 5, height = 5, units = "in")

#' -----------------------------------------------------------------------------
#' What happens if we drop from really low measured BC values?
#' Remove the 10 lowest BC meaurements
#' All occured in the spring and winter
#' All below the 2.5th percentile
#' -----------------------------------------------------------------------------

summary(bc_lur_data$bc_ug_m3)
hist(bc_lur_data$bc_ug_m3)

boxplot(bc_lur_data$bc_ug_m3)

quantile(bc_lur_data$bc_ug_m3, probs = c(0.01, 0.025, 0.05))
quantile(bc_lur_data$bc_ug_m3, probs = c(0.95, 0.975, 0.99))

removed_bc <- bc_lur_data %>% 
  filter(bc_ug_m3 < quantile(bc_lur_data$bc_ug_m3, probs = c(0.025))) %>% 
  arrange(bc_ug_m3)
glimpse(removed_bc)
table(removed_bc$spring)
table(removed_bc$summer)
table(removed_bc$autumn)
table(removed_bc$winter)

nrow(removed_bc)

bc_lur_data_A <- bc_lur_data %>% 
  filter(bc_ug_m3 >= quantile(bc_lur_data$bc_ug_m3, probs = c(0.025))) 

summary(bc_lur_data_A$bc_ug_m3)
hist(bc_lur_data_A$bc_ug_m3)

bc_lur_data3_A <- select(bc_lur_data_A, bc_ug_m3, start_predictors) 
summary(bc_lur_data3_A)

#' Retry the LASSO model
#' When alpha = 1, LASSO is implemented
#' Coefficients CAN shrink to 0 in this one
lambda_A <- 10^seq(-3, 3, length = 100)
tc_l_A = trainControl("cv", number = 10)
tg_l_A = expand.grid(alpha = 1, lambda = lambda_A)

log_bc_lasso_A <- train(log(bc_ug_m3) ~ ., data = bc_lur_data3_A,
                        method = "glmnet", trControl = tc_l_A, tuneGrid = tg_l_A)

log_bc_lasso_A$resample
sd(log(bc_lur_data3_A$bc_ug_m3))

plot(log_bc_lasso_A)
getTrainPerf(log_bc_lasso_A)
plot(log_bc_lasso_A$finalModel)
arrange(log_bc_lasso_A$results, RMSE) %>% head
log_bc_lasso_A$bestTune
log_bc_lasso_coef_A<- coef(log_bc_lasso_A$finalModel, log_bc_lasso_A$bestTune$lambda)
log_bc_lasso_coef_A

varImp(log_bc_lasso_A)

log_bc_lasso_coef_df_A <- data.frame(name = log_bc_lasso_coef_A@Dimnames[[1]][log_bc_lasso_coef_A@i + 1], 
                                     coefficient = log_bc_lasso_coef_A@x)
log_bc_lasso_coef_df_A
log_bc_lasso_list_A <- as.character(log_bc_lasso_coef_df_A$name[-1])
log_bc_lasso_list_A

#' Refit the _AIC model
#' fit the AIC model with 10-fold CV
tc_a_A = trainControl("cv", number = 10)
log_bc_aic_A <- train(log(bc_ug_m3) ~ ., data = select(bc_lur_data3_A, bc_ug_m3, log_bc_lasso_list_A), 
                      method = "lmStepAIC", trace = FALSE, trControl = tc_a_A)

summary(log_bc_aic_A)
log_bc_aic_A$results

log_bc_aic_list_A <- rownames(summary(log_bc_aic_A)[["coefficients"]])[-1]
log_bc_aic_list_A 

#' Compare models with AIC-selected coefficients and LASSO-selected coefficients
#' Validate the LASSO and AIC models with 10-fold cross validation

log_bc_lasso_list_A # 10 predictors
log_bc_aic_list_A  # 8 predictors

tc <- trainControl(method = "cv", number = 10)

#' 10-fold cross validation: LASSO-selected predictors
#' R2 = 0.67; RMSE = 0.16
log_bc_lasso_cv_A <- train(log(bc_ug_m3) ~ ., 
                           data = select(bc_lur_data_A, bc_ug_m3, log_bc_lasso_list_A),
                           method = "lm", trControl = tc)
log_bc_lasso_cv_A
log_bc_lasso_cv_A$finalModel
varImp(log_bc_lasso_cv_A)

bc_lur_data_A$pred_log_bc_lasso_A <- predict(log_bc_lasso_cv_A)

#' 10-fold cross validation: AIC-selected predictors
#' R2 = 0.67; RMSE = 0.16
log_bc_aic_cv_A <- train(log(bc_ug_m3) ~ ., 
                         data = select(bc_lur_data_A, bc_ug_m3, log_bc_aic_list_A),
                         method = "lm", trControl = tc)
log_bc_aic_cv_A
log_bc_aic_cv_A$finalModel
varImp(log_bc_aic_cv_A)

bc_lur_data_A$pred_log_bc_aic_A <- predict(log_bc_aic_cv_A)

ggplot(bc_lur_data_A) +
  geom_point(aes(x = log(bc_ug_m3), y = pred_log_bc_aic_A, col = "aic", shape = "aic")) +
  geom_point(aes(x = log(bc_ug_m3), y = pred_log_bc_lasso_A, col = "las", shape = "las")) +
  geom_abline(aes(slope = 1, intercept = 0), color = "black", linetype = 2) +
  scale_color_viridis(name = "Model", discrete = T,
                      labels = c("aic" = "AIC", "las" = "LASSO")) +
  scale_shape_manual(name = "Model",
                     values = c("aic" = 15, "las" = 16),
                     labels = c("aic" = "AIC", "las" = "LASSO")) +
  xlab("Measured log(BC)") + ylab("Predicted log(BC)") +
  simple_theme
ggsave(filename = here::here("Figs/Model_Dev", "Log_BC_Meas_vs_Pred_Drop_Low_Vals.jpeg"), 
       device = "jpeg", width = 5, height = 5, units = "in")

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' METHOD 1: Untransformed BC
#' Use RIDGE, LASSO and EN to select predictors using the caret package in R
#' Test stepwise AIC method to further reduce the number of predictors
#' ----------------------------------------------------------------------------- 
#' -----------------------------------------------------------------------------

#' Compare some alternatives for reducing the number of variables
#' First up, RIDGE
#' Coefficients don't shrink to zero, so not able to reduce predictors
lambda <- 10^seq(-3, 3, length = 100)
tc_r = trainControl("cv", number = 10)
tg_r = expand.grid(alpha = 0, lambda = lambda)

bc_ridge <- train(bc_ug_m3 ~ ., data = bc_lur_data3,
                  method = "glmnet", trControl = tc_r, tuneGrid = tg_r)

bc_ridge$resample
sd(bc_lur_data3$bc_ug_m3)

plot(bc_ridge)
getTrainPerf(bc_ridge)
plot(bc_ridge$finalModel)
arrange(bc_ridge$results, RMSE) %>% head
bc_ridge$bestTune
bc_ridge_coef <- coef(bc_ridge$finalModel, bc_ridge$bestTune$lambda)
bc_ridge_coef

#' Next, LASSO
#' When alpha = 1, LASSO is implemented
#' Coefficients CAN shrink to 0 in this one
lambda <- 10^seq(-3, 3, length = 100)
tc_l = trainControl("cv", number = 10)
tg_l = expand.grid(alpha = 1, lambda = lambda)

bc_lasso <- train(bc_ug_m3 ~ ., data = bc_lur_data3,
                  method = "glmnet", trControl = tc_l, tuneGrid = tg_l)

bc_lasso$resample
sd(bc_lur_data3$bc_ug_m3)

plot(bc_lasso)
getTrainPerf(bc_lasso)
plot(bc_lasso$finalModel)
arrange(bc_lasso$results, RMSE) %>% head
bc_lasso$bestTune
bc_lasso_coef<- coef(bc_lasso$finalModel, bc_lasso$bestTune$lambda)
bc_lasso_coef

varImp(bc_lasso)

#' Try elastic net for variable selection
#' Elastic net combines RIDGE and LASSO
#' Uses two tuning parameters: lambda and alpha
#' Note that the model requires a "centered" variable and "standardized" 
#' predictors, so we center standardize as part of the model specification 
#' (preProcess)

tc_e <- trainControl("cv", number = 10)
bc_glmnet <- train(bc_ug_m3 ~ ., data = bc_lur_data3, 
                   preProcess = c("center", "scale"),
                   method = "glmnet", trControl = tc_e)
bc_glmnet
plot(bc_glmnet)
getTrainPerf(bc_glmnet)
plot(bc_glmnet$finalModel)
arrange(bc_glmnet$results, RMSE) %>% head
bc_glmnet$bestTune
bc_enet_coef <- coef(bc_glmnet$finalModel, bc_glmnet$bestTune$lambda)
bc_enet_coef

#' Compare these three models
#' look for smallest median RMSE and largest Rsquared
bc_mods <- resamples(list(ridge = bc_ridge, lasso = bc_lasso, en = bc_glmnet))
summary(bc_mods)

#' ----------------------------------------------------------------------------- 
#' Try using the LASSO coefficients in a stepwise-AIC linear model
#' Untransformed BC here
#' ----------------------------------------------------------------------------- 

lasso_coef_df <- data.frame(name = bc_lasso_coef@Dimnames[[1]][bc_lasso_coef@i + 1], 
                            coefficient = bc_lasso_coef@x)
lasso_coef_df
bc_lasso_list <- as.character(lasso_coef_df$name[-1])
bc_lasso_list <- c(bc_lasso_list, "summer")
bc_lasso_list

bc_lur_data5 <- select(bc_lur_data, bc_ug_m3, bc_lasso_list)
names(bc_lur_data5)

#' Examine relationships with predictors
cor(bc_lur_data5$tree_cover_100, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$pop_den_50, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$pop_den_100, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$dist_m_cafos, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$dist_m_og_wells, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$len_m_highways_100, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$len_m_major_roads_100, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$aadt_50, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$aadt_500, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$nearest_3_neighbors_bc, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$nearest_3_neighbors_temp, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$winter, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$spring, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$summer, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$low_inten_dev_50, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$high_inten_dev_100, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$low_inten_dev_500, bc_lur_data5$bc_ug_m3)
cor(bc_lur_data5$med_inten_dev_1000, bc_lur_data5$bc_ug_m3)

plot(bc_lur_data5$tree_cover_100, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$pop_den_50, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$pop_den_100, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$dist_m_cafos, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$dist_m_og_wells, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$len_m_highways_100, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$len_m_major_roads_100, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$aadt_50, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$aadt_500, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$nearest_3_neighbors_bc, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$nearest_3_neighbors_temp, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$winter, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$spring, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$summer, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$low_inten_dev_50, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$high_inten_dev_100, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$low_inten_dev_500, bc_lur_data5$bc_ug_m3)
plot(bc_lur_data5$med_inten_dev_1000, bc_lur_data5$bc_ug_m3)

#' fit the AIC model with 10-fold CV
tc_a = trainControl("cv", number = 10)
bc_aic <- train(bc_ug_m3 ~ ., data = select(bc_lur_data, bc_ug_m3, bc_lasso_list), 
                method = "lmStepAIC", trace = FALSE, trControl = tc_a)

summary(bc_aic)
bc_aic$results

bc_aic_list <- rownames(summary(bc_aic)[["coefficients"]])[-1]
bc_aic_list 

#' ----------------------------------------------------------------------------- 
#' Compare models with AIC-selected coefficients and LASSO-selected coefficients
#' Validate the LASSO and AIC models with 10-fold cross validation
#' ----------------------------------------------------------------------------- 

bc_lasso_list # 18 predictors
bc_aic_list  # 9 predictors

tc <- trainControl(method = "cv", number = 10)

#' 10-fold cross validation: LASSO-selected predictors
#' R2 = 0.50; RMSE = 0.30
bc_lasso_cv <- train(bc_ug_m3 ~ ., 
                     data = select(bc_lur_data5, bc_ug_m3, bc_lasso_list),
                     method = "lm", trControl = tc)
bc_lasso_cv
bc_lasso_cv$finalModel
varImp(bc_lasso_cv)

bc_lur_data5$pred_bc_lasso <- predict(bc_lasso_cv)

#' 10-fold cross validation: AIC-selected predictors
#' R2 = 0.52; RMSE = 0.29
bc_aic_cv <- train(bc_ug_m3 ~ ., 
                   data = select(bc_lur_data5, bc_ug_m3, bc_aic_list),
                   method = "lm", trControl = tc)
bc_aic_cv
bc_aic_cv$finalModel
varImp(bc_aic_cv)

bc_lur_data5$pred_bc_aic <- predict(bc_aic_cv)

ggplot(bc_lur_data5) +
  geom_point(aes(x = bc_ug_m3, y = pred_bc_aic, col = "aic", shape = "aic")) +
  geom_point(aes(x = bc_ug_m3, y = pred_bc_lasso, col = "las", shape = "las")) +
  geom_abline(aes(slope = 1, intercept = 0), color = "black", linetype = 2) +
  scale_color_viridis(name = "Model", discrete = T,
                      labels = c("aic" = "AIC", "las" = "LASSO")) +
  scale_shape_manual(name = "Model",
                     values = c("aic" = 15, "las" = 16),
                     labels = c("aic" = "AIC", "las" = "LASSO")) +
  xlab("Measured BC") + ylab("Predicted BC") +
  simple_theme
ggsave(filename = here::here("Figs/Model_Dev", "BC_Meas_vs_Pred_All_Obs.jpeg"), 
       device = "jpeg", width = 5, height = 5, units = "in")

#' -----------------------------------------------------------------------------
#' What happens if we drop from really low measured BC values?
#' Remove the 10 lowest BC meaurements
#' All occured in the spring and winter
#' All below the 2.5th percentile
#' ----------------------------------------------------------------------------- 

summary(bc_lur_data$bc_ug_m3)
hist(bc_lur_data$bc_ug_m3)

boxplot(bc_lur_data$bc_ug_m3)

quantile(bc_lur_data$bc_ug_m3, probs = c(0.01, 0.025, 0.05))
quantile(bc_lur_data$bc_ug_m3, probs = c(0.95, 0.975, 0.99))

removed_bc <- bc_lur_data %>% 
  filter(bc_ug_m3 < quantile(bc_lur_data$bc_ug_m3, probs = c(0.025))) %>% 
  arrange(bc_ug_m3)
glimpse(removed_bc)
table(removed_bc$spring)
table(removed_bc$summer)
table(removed_bc$autumn)
table(removed_bc$winter)

nrow(removed_bc)

bc_lur_data_A <- bc_lur_data %>% 
  filter(bc_ug_m3 >= quantile(bc_lur_data$bc_ug_m3, probs = c(0.025))) 

summary(bc_lur_data_A$bc_ug_m3)
hist(bc_lur_data_A$bc_ug_m3)

bc_lur_data5_A <- select(bc_lur_data_A, bc_ug_m3, start_predictors) 
summary(bc_lur_data5_A)

#' Retry the LASSO model
#' When alpha = 1, LASSO is implemented
#' Coefficients CAN shrink to 0 in this one
lambda_A <- 10^seq(-3, 3, length = 100)
tc_l_A = trainControl("cv", number = 10)
tg_l_A = expand.grid(alpha = 1, lambda = lambda_A)

bc_lasso_A <- train(bc_ug_m3 ~ ., data = bc_lur_data5_A,
                    method = "glmnet", trControl = tc_l_A, tuneGrid = tg_l_A)

bc_lasso_A$resample
sd(bc_lur_data5_A$bc_ug_m3)

plot(bc_lasso_A)
getTrainPerf(bc_lasso_A)
plot(bc_lasso_A$finalModel)
arrange(bc_lasso_A$results, RMSE) %>% head
bc_lasso_A$bestTune
bc_lasso_coef_A<- coef(bc_lasso_A$finalModel, bc_lasso_A$bestTune$lambda)
bc_lasso_coef_A

varImp(bc_lasso_A)

bc_lasso_coef_df_A <- data.frame(name = bc_lasso_coef_A@Dimnames[[1]][bc_lasso_coef_A@i + 1], 
                                 coefficient = bc_lasso_coef_A@x)
bc_lasso_coef_df_A
bc_lasso_list_A <- as.character(bc_lasso_coef_df_A$name[-1])
bc_lasso_list_A

#' Refit the _AIC model
#' fit the AIC model with 10-fold CV
tc_a_A = trainControl("cv", number = 10)
bc_aic_A <- train(bc_ug_m3 ~ ., data = select(bc_lur_data5_A, bc_ug_m3, bc_lasso_list_A), 
                  method = "lmStepAIC", trace = FALSE, trControl = tc_a_A)

summary(bc_aic_A)
bc_aic_A$results

bc_aic_list_A <- rownames(summary(bc_aic_A)[["coefficients"]])[-1]
bc_aic_list_A 

#' Compare models with AIC-selected coefficients and LASSO-selected coefficients
#' Validate the LASSO and AIC models with 10-fold cross validation

bc_lasso_list_A # 9 predictors
bc_aic_list_A  # 7 predictors

tc <- trainControl(method = "cv", number = 10)

#' 10-fold cross validation: LASSO-selected predictors
#' R2 = 0.64; RMSE = 24
bc_lasso_cv_A <- train(bc_ug_m3 ~ ., 
                       data = select(bc_lur_data5_A, bc_ug_m3, bc_lasso_list_A),
                       method = "lm", trControl = tc)
bc_lasso_cv_A
bc_lasso_cv_A$finalModel
varImp(bc_lasso_cv_A)

bc_lur_data5_A$pred_bc_lasso_A <- predict(bc_lasso_cv_A)

#' 10-fold cross validation: AIC-selected predictors
#' R2 = 0.65; RMSE = 0.24
bc_aic_cv_A <- train(bc_ug_m3 ~ ., 
                     data = select(bc_lur_data5_A, bc_ug_m3, bc_aic_list_A),
                     method = "lm", trControl = tc)
bc_aic_cv_A
bc_aic_cv_A$finalModel
varImp(bc_aic_cv_A)

bc_lur_data5_A$pred_bc_aic_A <- predict(bc_aic_cv_A)

ggplot(bc_lur_data5_A) +
  geom_point(aes(x = bc_ug_m3, y = pred_bc_aic_A, col = "aic", shape = "aic")) +
  geom_point(aes(x = bc_ug_m3, y = pred_bc_lasso_A, col = "las", shape = "las")) +
  geom_abline(aes(slope = 1, intercept = 0), color = "black", linetype = 2) +
  scale_color_viridis(name = "Model", discrete = T,
                      labels = c("aic" = "AIC", "las" = "LASSO")) +
  scale_shape_manual(name = "Model",
                     values = c("aic" = 15, "las" = 16),
                     labels = c("aic" = "AIC", "las" = "LASSO")) +
  xlab("Measured BC") + ylab("Predicted BC") +
  simple_theme
ggsave(filename = here::here("Figs/Model_Dev", "BC_Meas_vs_Pred_Drop_Low_Vals.jpeg"), 
       device = "jpeg", width = 5, height = 5, units = "in")

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Save the models!
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------

save(bc_aic_cv, bc_lasso_cv, bc_aic_cv_A, bc_lasso_cv_A, 
     log_bc_aic_cv, log_bc_lasso_cv, log_bc_aic_cv_A, log_bc_lasso_cv_A, 
     file = here::here("Results", "BC_LUR_Models.rdata"))

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Predict concentrations for the grid cells
#' Use the LASSO model and log-transformed BC
#' 
#' Use the version of the model where we drop the 10 lowest observations
#' ----------------------------------------------------------------------------- 
#' -----------------------------------------------------------------------------

load(here::here("Results", "BC_LUR_MOdels.rdata"))

grid_data <- read_csv(here::here("Data", "Combined_Grid_Data_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

aic_cols <- names(log_bc_aic_cv$finalModel$coefficients)[-1]
aic_cols

grid_data2 <- select(grid_data, month, year, aic_cols) %>% 
  na.omit()

grid_data2$pred_log_bc_aic <- predict(log_bc_aic_cv, newdata = grid_data2)

aic_cols_A <- names(bc_aic_cv_A$finalModel$coefficients)[-1]
aic_cols_A

grid_data2_A <- select(grid_data, month, year, aic_cols_A) %>% 
  na.omit()

grid_data2_A$pred_log_bc_aic <- predict(bc_aic_cv_A, newdata = grid_data2_A)

#' -----------------------------------------------------------------------------
#' Make some maps to check things out
#' Use AIC model with the lowest concentrations dropped
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
df_plot1 <- filter(grid_data2_A, month == 6 & year == 2018)

st_bbox(df_plot1)
x_min <- unname(st_bbox(df_plot1)["xmin"])
x_max <- unname(st_bbox(df_plot1)["xmax"]) 
y_min <- unname(st_bbox(df_plot1)["ymin"])
y_max <- unname(st_bbox(df_plot1)["ymax"])

jun <- ggplot(df_plot1) +
  geom_sf(aes(fill = pred_log_bc_aic), color = NA) +
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
jun

plot_name1 <- paste0("BC_Jun_2018.jpeg")
ggsave(here::here("Figs", plot_name1),
       device = "jpeg", height= 5, width = 6, dpi = 500, units = "in")

#' September, 2018
df_plot2 <- filter(grid_data2_A, month == 9 & year == 2018)

st_bbox(df_plot2)
x_min <- unname(st_bbox(df_plot2)["xmin"])
x_max <- unname(st_bbox(df_plot2)["xmax"]) 
y_min <- unname(st_bbox(df_plot2)["ymin"])
y_max <- unname(st_bbox(df_plot2)["ymax"])

sep <- ggplot(df_plot2) +
  geom_sf(aes(fill = pred_log_bc_aic), color = NA) +
  scale_fill_viridis(name = paste("log(BC):",  
                                  "Sep 2018",
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
sep

plot_name2 <- paste0("BC_Sep_2018.jpeg")
ggsave(here::here("Figs", plot_name2),
       device = "jpeg", height= 5, width = 6, dpi = 500, units = "in")

df_plot3 <- filter(grid_data2_A, month == 1 & year == 2019)

st_bbox(df_plot3)
x_min <- unname(st_bbox(df_plot3)["xmin"])
x_max <- unname(st_bbox(df_plot3)["xmax"]) 
y_min <- unname(st_bbox(df_plot3)["ymin"])
y_max <- unname(st_bbox(df_plot3)["ymax"])

jan <- ggplot(df_plot3) +
  geom_sf(aes(fill = pred_log_bc_aic), color = NA) +
  scale_fill_viridis(name = paste("log(BC):", 
                                  "Jan 2019",
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
jan

plot_name3 <- paste0("BC_Jan_2019.jpeg")
ggsave(here::here("Figs", plot_name3),
       device = "jpeg", height= 5, width = 6, dpi = 500, units = "in")

#' #' -----------------------------------------------------------------------------
#' #' -----------------------------------------------------------------------------
#' #' METHOD 2: 
#' #' Use the 
#' #' 
#' #' Methods reported in Beckerman et al. (2013) Atmospheric Environment
#' #' ----------------------------------------------------------------------------- 
#' #' -----------------------------------------------------------------------------


#' #' -----------------------------------------------------------------------------
#' #' -----------------------------------------------------------------------------
#' #' METHOD 3: 
#' #' Use a random forest model to select predictors
#' #' Develop separate models for each season
#' #' CAST package in R to faciliate model building and validation
#' #' A good tutorial here: https://cran.r-project.org/web/packages/CAST/vignettes/CAST-intro.html
#' #' 
#' #' Methods reported in Meyer et al. (2018) Env Mod & Soft
#' #' 
#' #' Need a temporal variable, so using month indicators; jan is reference month
#' #' ----------------------------------------------------------------------------- 
#' #' -----------------------------------------------------------------------------
#' 
#' names(lur_data)
#' bc_lur_data5 <- lur_data %>% 
#'   select(lon, lat, bc_ug_m3, start_predictors, month) %>% 
#'   na.omit() %>% 
#'   mutate(month = as.factor(month), space_id = paste(lon, lat, sep = "_"))
#' glimpse(bc_lur_data5)
#' 
#' #' ----------------------------------------------------------------------------- 
#' #' Random forest model training
#' #' Setting tuneLength to 1 to avoid hyperparameter tuning
#' #' According to Meyer, RF is relatively insensitive to tuning
#' #' Going to include a 5-fold cross validation
#' #' ----------------------------------------------------------------------------- 
#' 
#' library(CAST)
#' 
#' predictors <- names(select(bc_lur_data5, -c("bc_ug_m3", "space_id", "lon", "lat")))
#' predictors
#' 
#' rf_model0 <- train(bc_lur_data5[,predictors], log(bc_lur_data5$bc_ug_m3),
#'                    method = "rf", tuneLength = 1, importance = T,
#'                    trControl = trainControl(method = "cv", number = 5))
#' rf_model0
#' plot(varImp(rf_model0))
#' 
#' #' ----------------------------------------------------------------------------- 
#' #' Random forest model training
#' #' Using target-oriented validation: leave-location-out
#' #' ----------------------------------------------------------------------------- 
#' 
#' sp_indices <- CreateSpacetimeFolds(bc_lur_data5, spacevar = "space_id")
#' sp_indices
#' 
#' rf_model1 <- train(bc_lur_data5[,predictors], log(bc_lur_data5$bc_ug_m3),
#'                    method = "rf", tuneLength = 1, importance = T,
#'                    trControl = trainControl(method = "cv", 
#'                                             index = sp_indices$index))
#' rf_model1
#' plot(varImp(rf_model1))
#' 
#' #' ----------------------------------------------------------------------------- 
#' #' Random forest model training
#' #' Using target-oriented validation: leave-time-out
#' #' ----------------------------------------------------------------------------- 
#' 
#' t_indices <- CreateSpacetimeFolds(bc_lur_data5, timevar = "month")
#' t_indices
#' 
#' rf_model2 <- train(bc_lur_data5[,predictors], log(bc_lur_data5$bc_ug_m3),
#'                    method = "rf", tuneLength = 1, importance = T,
#'                    trControl = trainControl(method = "cv", 
#'                                             index = t_indices$index))
#' rf_model2
#' plot(varImp(rf_model2))
#' 
#' #' ----------------------------------------------------------------------------- 
#' #' Random forest model training
#' #' Forward feature selection to help reduce model overfitting
#' #' Want to use R2 as the evaluation metric
#' #' Just using the spatial folds
#' #' ----------------------------------------------------------------------------- 
#' 
#' rf_model3 <- ffs(bc_lur_data5[,predictors], log(bc_lur_data5$bc_ug_m3),
#'                  metric = "Rsquared", verbose = F, 
#'                  method = "rf", tuneLength = 1, importance = T,
#'                  trControl = trainControl(method = "cv", 
#'                                           index = sp_indices$index))
#' rf_model3
#' plot(varImp(rf_model3))
#' plot_ffs(rf_model3)
#' 
#' #' ----------------------------------------------------------------------------- 
#' #' Random forest model training
#' #' Forward feature selection to help reduce model overfitting
#' #' Want to use R2 as the evaluation metric
#' #' Going to use leave-time-out cross validation
#' #' ----------------------------------------------------------------------------- 
#' 
#' t_indices <- CreateSpacetimeFolds(bc_lur_data5, timevar = "month")
#' 
#' rf_model4 <- ffs(bc_lur_data5[,predictors], log(bc_lur_data5$bc_ug_m3),
#'                  metric = "Rsquared", verbose = F, 
#'                  method = "rf", tuneLength = 1, importance = T,
#'                  trControl = trainControl(method = "cv", 
#'                                           index = t_indices$index))
#' rf_model4
#' plot(varImp(rf_model4))
#' plot_ffs(rf_model4)
#' 
#' #' ----------------------------------------------------------------------------- 
#' #' Random forest model training
#' #' Forward feature selection to help reduce model overfitting
#' #' Want to use R2 as the evaluation metric
#' #' Going to use leave-time-out cross validation
#' #' ----------------------------------------------------------------------------- 
#' 
#' sp_t_indices <- CreateSpacetimeFolds(bc_lur_data5, spacevar = "space_id", 
#'                                   timevar = "month")
#' 
#' rf_model5 <- ffs(bc_lur_data5[,predictors], log(bc_lur_data5$bc_ug_m3),
#'                  metric = "Rsquared", verbose = F, 
#'                  method = "rf", tuneLength = 1, importance = T,
#'                  trControl = trainControl(method = "cv", 
#'                                           index = sp_t_indices$index))
#' rf_model5
#' plot(varImp(rf_model5))
#' plot_ffs(rf_model5)
#' 
#' #' ----------------------------------------------------------------------------- 
#' #' Pick the final RF model
#' #' ----------------------------------------------------------------------------- 
#' 
#' rf_model1
#' rf_model1$selectedvars
#' 
#' rf_model2
#' rf_model2$selectedvars
#' 
#' rf_model3
#' rf_model3$selectedvars
#' 
#' rf_model4
#' rf_model4$selectedvars
#' 
#' rf_model5
#' rf_model5$selectedvars
#' 
#' bc_final_rf <- rf_model4
#' 
#' #' ----------------------------------------------------------------------------- 
#' #' Predict BC concentrations at the sampled locations
#' #' ----------------------------------------------------------------------------- 
#' 
#' bc_lur_data5$pred_log_bc_ug_m3_rf <- predict(bc_final_rf)
#' summary(bc_lur_data5$pred_log_bc_ug_m3_rf)
#' summary(log(bc_lur_data$bc_ug_m3))
#' 
#' bc_locations <- bc_locations %>% 
#'   left_join(select(bc_lur_data5, lon, lat, month, pred_log_bc_ug_m3_rf),
#'             by = c("lon", "lat", "month"))
#' 
#' #' ----------------------------------------------------------------------------- 
#' #' ----------------------------------------------------------------------------- 
#' #' Compare measured and predicted BC
#' #' Save the final results
#' #' ----------------------------------------------------------------------------- 
#' #' ----------------------------------------------------------------------------- 
#' 
#' ggplot(bc_locations) +
#'   geom_point(aes(x = log(bc_ug_m3), y = pred_log_bc_ug_m3_aic, col = "aic", shape = "aic")) +
#'   geom_point(aes(x = log(bc_ug_m3), y = pred_log_bc_ug_m3_lasso, col = "las", shape = "las")) +
#'   geom_point(aes(x = log(bc_ug_m3), y = pred_log_bc_ug_m3_rf, col = "rf", shape = "rf")) +
#'   geom_abline(aes(slope = 1, intercept = 0), color = "black", linetype = 2) +
#'   scale_color_viridis(name = "Model", discrete = T,
#'                       labels = c("aic" = "AIC", "las" = "LASSO", "rf" = "Random Forest")) +
#'   scale_shape_manual(name = "Model",
#'                      values = c("aic" = 15, "las" = 16, "rf" = 17),
#'                      labels = c("aic" = "AIC", "las" = "LASSO", "rf" = "Random Forest")) +
#'   xlab("Measured log(BC)") + ylab("Predicted log(BC)") +
#'   theme(legend.position = c(0.8, 0.2)) +
#'   simple_theme
#' ggsave(filename = here::here("Figs/Model_Dev", "BC_Meas_vs_Pred.jpeg"), device = "jpeg",
#'        width = 5, height = 5, units = "in")
#' 
#' cor(log(bc_locations$bc_ug_m3), bc_locations$pred_log_bc_ug_m3_aic) 
#' cor(log(bc_locations$bc_ug_m3), bc_locations$pred_log_bc_ug_m3_lasso) 
#' cor(log(bc_locations$bc_ug_m3), bc_locations$pred_log_bc_ug_m3_rf) 
#' 
#' #' ----------------------------------------------------------------------------- 
#' #' Save the final models and final dataset for further evaluation
#' #' -----------------------------------------------------------------------------
#' 
#' st_write(bc_locations, here::here("Results", 
#'                                   "BC_Filter_LUR_Results_Campaign1.csv"),
#'          layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
#' 
#' save(bc_final_aic, bc_final_aic_cv, bc_final_lasso, bc_final_lasso_cv, bc_final_rf,
#'      file = here::here("Results", "BC_LUR_Models_Campaign1.rdata"))
#' 



#' ----------------------------------------------------------------------------- 
#' -----------------------------------------------------------------------------   
#' Look at each group of variables and select the single best predicitor
#' Need to keep the number of candidate predictors <20 if possible
#' 
#' NOTE: stopped using this section of code as of 12.04.19
#' ----------------------------------------------------------------------------- 
#' ----------------------------------------------------------------------------- 

#' #' single predictor LM
#' lm_f <- function(dat0 , yvar, xvar) {
#'   dat <- dat0[,c(yvar, xvar)]
#'   
#'   form <- paste(yvar, "~", ".")
#'   lm1 <- lm(as.formula(form), data = dat)
#'   return(summary(lm1)$r.squared)
#' }
#' 
#' drop_vars2 <- data.frame()
#' 
#' #' how many variables in each group should we try?
#' #' Start with 1
#' #' Which version of BC should we use?
#' keep_n <- c(1:2)
#' yvar <- "log_bc_ug_m3"
#' 
#' #' Elevation
#' elevation_vars <- c("elevation_50", "elevation_100", "elevation_250", 
#'                     "elevation_500", "elevation_1000", "elevation_2500")
#' rsq <- map_dbl(elevation_vars, function(i) lm_f(dat0 = bc_lur_data, xvar = i, yvar = yvar))
#' elevation_results <- tibble(predictor_variables = elevation_vars, rsq = rsq) %>% 
#'   arrange(desc(rsq))
#' elevation_results
#' 
#' drop_vars2 <- bind_rows(drop_vars2, elevation_results[-keep_n,])
#' 
#' #' Tree cover
#' tree_cover_vars <- c("tree_cover_50", "tree_cover_100", "tree_cover_250", 
#'                      "tree_cover_500", "tree_cover_1000", "tree_cover_2500")
#' rsq <- map_dbl(tree_cover_vars, function(i) lm_f(dat0 = bc_lur_data, xvar = i, yvar = yvar))
#' tree_cover_results <- tibble(predictor_variables = tree_cover_vars, rsq = rsq) %>% 
#'   arrange(desc(rsq))
#' tree_cover_results
#' 
#' drop_vars2 <- bind_rows(drop_vars2, tree_cover_results[-keep_n,])
#' 
#' #' Impervious surfaces
#' impervious_vars <- c("impervious_50", "impervious_100", "impervious_250", 
#'                      "impervious_500", "impervious_1000", "impervious_2500")
#' rsq <- map_dbl(impervious_vars, function(i) lm_f(dat0 = bc_lur_data, xvar = i, yvar = yvar))
#' impervious_results <- tibble(predictor_variables = impervious_vars, rsq = rsq) %>% 
#'   arrange(desc(rsq))
#' impervious_results
#' 
#' drop_vars2 <- bind_rows(drop_vars2, impervious_results[-keep_n,])
#' 
#' #' Population density
#' pop_den_vars <- c("pop_den_50", "pop_den_100", "pop_den_250", 
#'                   "pop_den_500", "pop_den_1000", "pop_den_2500")
#' rsq <- map_dbl(pop_den_vars, function(i) lm_f(dat0 = bc_lur_data, xvar = i, yvar = yvar))
#' pop_den_results <- tibble(predictor_variables = pop_den_vars, rsq = rsq) %>% 
#'   arrange(desc(rsq))
#' pop_den_results
#' 
#' drop_vars2 <- bind_rows(drop_vars2, pop_den_results[-keep_n,])
#' 
#' #' Population count
#' pop_ct_vars <- c("pop_ct_50", "pop_ct_100", "pop_ct_250", 
#'                  "pop_ct_500", "pop_ct_1000", "pop_ct_2500")
#' rsq <- map_dbl(pop_ct_vars, function(i) lm_f(dat0 = bc_lur_data, xvar = i, yvar = yvar))
#' pop_ct_results <- tibble(predictor_variables = pop_ct_vars, rsq = rsq) %>% 
#'   arrange(desc(rsq))
#' pop_ct_results
#' 
#' drop_vars2 <- bind_rows(drop_vars2, pop_ct_results[-keep_n,])
#' 
#' #' Length of highways
#' len_m_highways_vars <- c("len_m_highways_50", "len_m_highways_100", "len_m_highways_250", 
#'                          "len_m_highways_500", "len_m_highways_1000", "len_m_highways_2500")
#' rsq <- map_dbl(len_m_highways_vars, function(i) lm_f(dat0 = bc_lur_data, xvar = i, yvar = yvar))
#' len_m_highways_results <- tibble(predictor_variables = len_m_highways_vars, rsq = rsq) %>% 
#'   arrange(desc(rsq))
#' len_m_highways_results
#' 
#' drop_vars2 <- bind_rows(drop_vars2, len_m_highways_results[-keep_n,])
#' 
#' #' Length of major_roads
#' len_m_major_roads_vars <- c("len_m_major_roads_50", "len_m_major_roads_100", "len_m_major_roads_250", 
#'                             "len_m_major_roads_500", "len_m_major_roads_1000", "len_m_major_roads_2500")
#' rsq <- map_dbl(len_m_major_roads_vars, function(i) lm_f(dat0 = bc_lur_data, xvar = i, yvar = yvar))
#' len_m_major_roads_results <- tibble(predictor_variables = len_m_major_roads_vars, rsq = rsq) %>% 
#'   arrange(desc(rsq))
#' len_m_major_roads_results
#' 
#' drop_vars2 <- bind_rows(drop_vars2, len_m_major_roads_results[-keep_n,])
#' 
#' #' AADT
#' aadt_vars <- c("aadt_50", "aadt_100", "aadt_250", "aadt_500", "aadt_1000", "aadt_2500")
#' rsq <- map_dbl(aadt_vars, function(i) lm_f(dat0 = bc_lur_data, xvar = i, yvar = yvar))
#' aadt_results <- tibble(predictor_variables = aadt_vars, rsq = rsq) %>% 
#'   arrange(desc(rsq))
#' aadt_results
#' 
#' drop_vars2 <- bind_rows(drop_vars2, aadt_results[-keep_n,])
#' 
#' #' land use category
#' land_use_cat_vars <- c("land_use_50_cat", "land_use_100_cat", "land_use_250_cat", 
#'                        "land_use_500_cat", "land_use_1000_cat", "land_use_2500_cat")
#' rsq <- map_dbl(land_use_cat_vars, function(i) lm_f(dat0 = bc_lur_data, xvar = i, yvar = yvar))
#' land_use_cat_results <- tibble(predictor_variables = land_use_cat_vars, rsq = rsq) %>% 
#'   arrange(desc(rsq))
#' land_use_cat_results
#' 
#' land_use_buffers <- as.numeric(gsub(".*?([0-9]+).*", "\\1", land_use_cat_results$predictor_variables[-keep_n]))
#' cats <- c("open_space_", "low_inten_dev_", "med_inten_dev_", "high_inten_dev_",
#'           "agriculture_")
#' drop_land_use_buffers <- apply(expand.grid(cats, land_use_buffers), 1, paste, collapse="")
#' drop_land_use_buffers <- gsub(" ", "", drop_land_use_buffers)
#' drop_land_use_buffers
#' 
#' #' Want to drop all land use categorical variables (use dummies in analysis)
#' drop_vars2 <- bind_rows(drop_vars2, land_use_cat_results)
#' 
#' #' List of variables to drop
#' drop_vars2 <- drop_vars2$predictor_variables
#' drop_vars2
#' 
#' #' Drop "reference" dummy variables and any variables that aren't quite needed
#' #' (Months that aren't included, etc.)
#' #' Not using seasons
#' #' May is the reference month
#' #' "Open space" is reference land use category
#' bc_lur_data2a <- bc_lur_data %>% 
#'   select(-one_of(drop_vars2), -one_of(drop_land_use_buffers),
#'          -open_space_50,  -open_space_100, -open_space_250, 
#'          -open_space_500, -open_space_1000, -open_space_2500) %>%
#'   select(-month) %>% 
#'   select(-c(spring, summer)) %>% 
#'   select(-contains("nearest_neighbor")) %>% 
#'   select(-log_bc_ug_m3) # just for comparisons 
#' glimpse(bc_lur_data2a)
#' names(bc_lur_data2a)
#' 
#' bc_lur_data2 <- bc_lur_data2a %>% 
#'   select(-c(lon, lat))
