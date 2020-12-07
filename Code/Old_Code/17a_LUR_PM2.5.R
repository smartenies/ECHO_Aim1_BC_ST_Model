#' =============================================================================
#' Project: ECHO LUR
#' Date created: January 18, 2019
#' Date updated: September 5, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' Preliminary LUR models for PM2.5. 
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
#' -----------------------------------------------------------------------------

lur_data <- read_csv(here::here("Data/Final_Data", "Data_for_LUR.csv"))
glimpse(lur_data)

#' Which variable should we use? See 16_LUR_Data_set_Cleaned.R
#' For PM: 
#' _use are the calibrated TWAs based on demin regression

lur_data <- mutate(lur_data, pm_ug_m3 = pm_ug_m3_use)

#' Check distribution of PM2.5
#' PM2.5 and log_transformed PM2.5
summary(lur_data$pm_ug_m3)
ggplot(lur_data) +
  ggtitle("PM\u2082.\u2085") +
  geom_histogram(aes(x = pm_ug_m3)) +
  simple_theme

ggplot(lur_data) +
  ggtitle("PM\u2082.\u2085") +
  geom_qq(aes(sample = pm_ug_m3)) +
  geom_qq_line(aes(sample = pm_ug_m3)) +
  simple_theme

ggplot(lur_data) +
  ggtitle("Log-transformed PM\u2082.\u2085") +
  geom_histogram(aes(x = log(pm_ug_m3))) +
  simple_theme

ggplot(lur_data) +
  ggtitle("Log-transformed PM\u2082.\u2085") +
  geom_qq(aes(sample = log(pm_ug_m3))) +
  geom_qq_line(aes(sample = log(pm_ug_m3))) +
  simple_theme

#' ----------------------------------------------------------------------------- 
#' Look at some preliminary models using only selected predictors
#' ----------------------------------------------------------------------------- 

pm_lur_data <- lur_data %>% 
  select(lon, lat, month, pm_ug_m3, nearest_neighbor_pm:agriculture_2500) %>%  
  na.omit() %>% 
  mutate(log_pm_ug_m3 = log(pm_ug_m3),
         month = as.factor(month))
glimpse(pm_lur_data)

#' Save this here for predictions and mapping later!
pm_locations <- pm_lur_data %>% 
  mutate(lon2 = lon, lat2 = lat) %>% 
  st_as_sf(coords = c("lon2", "lat2"), crs = ll_wgs84) %>% 
  st_transform(crs = albers)

#' Which of the dummy variables can be dropped? All values are equal to 0
#' i.e., which categories don't have representation?
pm_drop <- select_if(pm_lur_data, function(x){all(x == 0)})
drop_vars <- colnames(pm_drop)
drop_vars

pm_lur_data <- select(pm_lur_data, -drop_vars)
names(pm_lur_data)

#' ----------------------------------------------------------------------------- 
#' ----------------------------------------------------------------------------- 
#' Narrow the list of candidate predictors
#' Look at each group of variables and select the single best predicitor
#' Need to keep the number of candidate predictors <20 if possible
#' ----------------------------------------------------------------------------- 
#' ----------------------------------------------------------------------------- 

#' single predictor LM
lm_f <- function(dat0 , yvar, xvar) {
  dat <- dat0[,c(yvar, xvar)]
  
  form <- paste(yvar, "~", ".")
  lm1 <- lm(as.formula(form), data = dat)
  return(summary(lm1)$r.squared)
}

drop_vars2 <- data.frame()

#' how many variables in each group should we try?
#' Start with 1
#' Which version of PM2.5 should we use?
keep_n <- c(1:1)
yvar <- "log_pm_ug_m3"

#' Elevation
elevation_vars <- c("elevation_50", "elevation_100", "elevation_250", 
                    "elevation_500", "elevation_1000", "elevation_2500")
rsq <- map_dbl(elevation_vars, function(i) lm_f(dat0 = pm_lur_data, xvar = i, yvar = yvar))
elevation_results <- tibble(predictor_variables = elevation_vars, rsq = rsq) %>% 
  arrange(desc(rsq))
elevation_results

drop_vars2 <- bind_rows(drop_vars2, elevation_results[-keep_n,])

#' Tree cover
tree_cover_vars <- c("tree_cover_50", "tree_cover_100", "tree_cover_250", 
                     "tree_cover_500", "tree_cover_1000", "tree_cover_2500")
rsq <- map_dbl(tree_cover_vars, function(i) lm_f(dat0 = pm_lur_data, xvar = i, yvar = yvar))
tree_cover_results <- tibble(predictor_variables = tree_cover_vars, rsq = rsq) %>% 
  arrange(desc(rsq))
tree_cover_results

drop_vars2 <- bind_rows(drop_vars2, tree_cover_results[-keep_n,])

#' Impervious surfaces
impervious_vars <- c("impervious_50", "impervious_100", "impervious_250", 
                     "impervious_500", "impervious_1000", "impervious_2500")
rsq <- map_dbl(impervious_vars, function(i) lm_f(dat0 = pm_lur_data, xvar = i, yvar = yvar))
impervious_results <- tibble(predictor_variables = impervious_vars, rsq = rsq) %>% 
  arrange(desc(rsq))
impervious_results

drop_vars2 <- bind_rows(drop_vars2, impervious_results[-keep_n,])

#' Population density
pop_den_vars <- c("pop_den_50", "pop_den_100", "pop_den_250", 
                  "pop_den_500", "pop_den_1000", "pop_den_2500")
rsq <- map_dbl(pop_den_vars, function(i) lm_f(dat0 = pm_lur_data, xvar = i, yvar = yvar))
pop_den_results <- tibble(predictor_variables = pop_den_vars, rsq = rsq) %>% 
  arrange(desc(rsq))
pop_den_results

drop_vars2 <- bind_rows(drop_vars2, pop_den_results[-keep_n,])

#' Population count
pop_ct_vars <- c("pop_ct_50", "pop_ct_100", "pop_ct_250", 
                  "pop_ct_500", "pop_ct_1000", "pop_ct_2500")
rsq <- map_dbl(pop_ct_vars, function(i) lm_f(dat0 = pm_lur_data, xvar = i, yvar = yvar))
pop_ct_results <- tibble(predictor_variables = pop_ct_vars, rsq = rsq) %>% 
  arrange(desc(rsq))
pop_ct_results

drop_vars2 <- bind_rows(drop_vars2, pop_ct_results[-keep_n,])

#' Length of highways
len_m_highways_vars <- c("len_m_highways_50", "len_m_highways_100", "len_m_highways_250", 
                         "len_m_highways_500", "len_m_highways_1000", "len_m_highways_2500")
rsq <- map_dbl(len_m_highways_vars, function(i) lm_f(dat0 = pm_lur_data, xvar = i, yvar = yvar))
len_m_highways_results <- tibble(predictor_variables = len_m_highways_vars, rsq = rsq) %>% 
  arrange(desc(rsq))
len_m_highways_results

drop_vars2 <- bind_rows(drop_vars2, len_m_highways_results[-keep_n,])

#' Length of major_roads
len_m_major_roads_vars <- c("len_m_major_roads_50", "len_m_major_roads_100", "len_m_major_roads_250", 
                            "len_m_major_roads_500", "len_m_major_roads_1000", "len_m_major_roads_2500")
rsq <- map_dbl(len_m_major_roads_vars, function(i) lm_f(dat0 = pm_lur_data, xvar = i, yvar = yvar))
len_m_major_roads_results <- tibble(predictor_variables = len_m_major_roads_vars, rsq = rsq) %>% 
  arrange(desc(rsq))
len_m_major_roads_results

drop_vars2 <- bind_rows(drop_vars2, len_m_major_roads_results[-keep_n,])

#' AADT
aadt_vars <- c("aadt_50", "aadt_100", "aadt_250", "aadt_500", "aadt_1000", "aadt_2500")
rsq <- map_dbl(aadt_vars, function(i) lm_f(dat0 = pm_lur_data, xvar = i, yvar = yvar))
aadt_results <- tibble(predictor_variables = aadt_vars, rsq = rsq) %>% 
  arrange(desc(rsq))
aadt_results

drop_vars2 <- bind_rows(drop_vars2, aadt_results[-keep_n,])

#' land use category
land_use_cat_vars <- c("land_use_50_cat", "land_use_100_cat", "land_use_250_cat", 
                       "land_use_500_cat", "land_use_1000_cat", "land_use_2500_cat")
rsq <- map_dbl(land_use_cat_vars, function(i) lm_f(dat0 = pm_lur_data, xvar = i, yvar = yvar))
land_use_cat_results <- tibble(predictor_variables = land_use_cat_vars, rsq = rsq) %>% 
  arrange(desc(rsq))
land_use_cat_results

land_use_buffers <- as.numeric(gsub(".*?([0-9]+).*", "\\1", land_use_cat_results$predictor_variables[-keep_n]))
cats <- c("open_space_", "low_inten_dev_", "med_inten_dev_", "high_inten_dev_",
          "agriculture_")
drop_land_use_buffers <- apply(expand.grid(cats, land_use_buffers), 1, paste, collapse="")
drop_land_use_buffers <- gsub(" ", "", drop_land_use_buffers)
drop_land_use_buffers

#' Want to drop all land use categorical variables (use dummies in analysis)
drop_vars2 <- bind_rows(drop_vars2, land_use_cat_results)

#' List of variables to drop
drop_vars2 <- drop_vars2$predictor_variables
drop_vars2

#' Drop "reference" dummy variables and any variables that aren't quite needed
#' (Months that aren't included, etc.)
#' Not using seasons
#' May is the reference month
#' "Open space" is reference land use category
pm_lur_data2a <- pm_lur_data %>% 
  select(-one_of(drop_vars2), -one_of(drop_land_use_buffers),
         -open_space_50,  -open_space_100, -open_space_250, 
         -open_space_500, -open_space_1000, -open_space_2500) %>%
  select(-month) %>% 
  select(-log_pm_ug_m3) # just for comparisons 
glimpse(pm_lur_data2a)
names(pm_lur_data2a)

pm_lur_data2 <- pm_lur_data2a %>% 
  select(-c(lon, lat))

vars_corr <- cor(select(pm_lur_data2, -c(jan:autumn)), use = "complete.obs")
ggcorrplot(vars_corr, type = "upper", method = "square",
           ggtheme = simple_theme, lab = T, lab_col = "white",
           show.diag = T)

#' Fit some linear regression models
#' Just AADT
pm_lm1 <- lm(log(pm_ug_m3) ~ aadt_1000, data = pm_lur_data2)
summary(pm_lm1)
par(mfrow=c(2,2)) 
plot(pm_lm1, main = "Just AADT within 1000 m: log-transformed outcome")
par(mfrow=c(1,1))
hist(pm_lm1$residuals)

pm_lm1a <- lm(pm_ug_m3 ~ aadt_1000, data = pm_lur_data2)
summary(pm_lm1a)
par(mfrow=c(2,2)) 
plot(pm_lm1a, main = "Just AADT within 1000 m: untransformed outcome")
par(mfrow=c(1,1))
hist(pm_lm1a$residuals)

#' Linear regression and with all possible covariates
#' Linear: Multiple R2 = 36% (Adjusted R2 = 25%)
pm_lm2 <- lm(log(pm_ug_m3) ~ ., data = pm_lur_data2)
summary(pm_lm2)
par(mfrow=c(2,2)) 
plot(pm_lm2, main = "All covariates: log-transformed outcome")
par(mfrow=c(1,1))
hist(pm_lm2$residuals)

pm_lm2a <- lm(pm_ug_m3 ~ ., data = pm_lur_data2)
summary(pm_lm2a)
par(mfrow=c(2,2)) 
plot(pm_lm2a, main = "All covariates: untransformed outcome")
par(mfrow=c(1,1))
hist(pm_lm2a$residuals)

#' Based on the models above, going to use log-transformed outcome

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' METHOD 1: 
#' Use LASSO to select predictors using the caret package in R
#' Test stepwise AIC method to further reduce the number of predictors
#' ----------------------------------------------------------------------------- 
#' -----------------------------------------------------------------------------

#' Which predictors are we starting with?
start_predictors <- names(select(pm_lur_data2, -pm_ug_m3,
                                 -c(jan:autumn), -c(contains("neighbor_"))))
start_predictors

pm_lur_data3 <- select(pm_lur_data, pm_ug_m3, start_predictors) 

library(caret)
library(glmnet)

# Set seed for reproducibility
set.seed(100)

#' Linear regression with 10-fold cross validation
#' Get warnings due to collinear covariates
tc <- trainControl(method = "cv", number = 10)
pm_cv <- train(log(pm_ug_m3) ~ ., data = pm_lur_data3,
               method = "lm", trControl = tc)
pm_cv
pm_cv$results
pm_cv$finalModel

#' Now going to compare some alternatives for reducing the number of variables
#' First up, RIDGE
lambda <- 10^seq(-3, 3, length = 100)
tc_r = trainControl("cv", number = 10)
tg_r = expand.grid(alpha = 0, lambda = lambda)

pm_ridge <- train(log(pm_ug_m3) ~ ., data = pm_lur_data3,
                  method = "glmnet", trControl = tc_r, tuneGrid = tg_r)

pm_ridge$resample
sd(log(pm_lur_data3$pm_ug_m3))

plot(pm_ridge)
getTrainPerf(pm_ridge)
plot(pm_ridge$finalModel)
arrange(pm_ridge$results, RMSE) %>% head
pm_ridge$bestTune
ridge_coef <- coef(pm_ridge$finalModel, pm_ridge$bestTune$lambda)
ridge_coef

#' Next, LASSO
#' When alpha = 1, LASSO is implemented
#' Coefficients CAN shrink to 0 in this one
lambda <- 10^seq(-3, 3, length = 100)
tc_l = trainControl("cv", number = 10)
tg_l = expand.grid(alpha = 1, lambda = lambda)

pm_lasso <- train(log(pm_ug_m3) ~ ., data = pm_lur_data3,
                  method = "glmnet", trControl = tc_l, tuneGrid = tg_l)

pm_lasso$resample
sd(log(pm_lur_data3$pm_ug_m3))

plot(pm_lasso)
getTrainPerf(pm_lasso)
plot(pm_lasso$finalModel)
arrange(pm_lasso$results, RMSE) %>% head
pm_lasso$bestTune
lasso_coef<- coef(pm_lasso$finalModel, pm_lasso$bestTune$lambda)
lasso_coef

varImp(pm_lasso)

#' Try elastic net for variable selection
#' Elastic net combines RIDGE and LASSO
#' Uses two tuning parameters: lambda and alpha
#' Note that the model requires a "centered" variable and "standardized" 
#' predictors, so we center standardize as part of the model specification 
#' (preProcess)

tc_e <- trainControl("cv", number = 10)
pm_glmnet <- train(log(pm_ug_m3) ~ ., data = pm_lur_data3, 
                   preProcess = c("center", "scale"),
                   method = "glmnet", trControl = tc_e)
pm_glmnet
plot(pm_glmnet)
getTrainPerf(pm_glmnet)
plot(pm_glmnet$finalModel)
arrange(pm_glmnet$results, RMSE) %>% head
pm_glmnet$bestTune
enet_coef <- coef(pm_glmnet$finalModel, pm_glmnet$bestTune$lambda)
enet_coef

#' Compare these three models
#' look for smallest median RMSE and largest Rsquared
mods <- resamples(list(ridge = pm_ridge, lasso = pm_lasso, en = pm_glmnet))
summary(mods)

#' ----------------------------------------------------------------------------- 
#' Try using the LASSO coefficients in a stepwise-AIC linear model
#' Has smallest median RMSE (0.24) and highest median R2 (0.36)
#' ----------------------------------------------------------------------------- 

coef_df <- data.frame(name = lasso_coef@Dimnames[[1]][lasso_coef@i + 1], 
                      coefficient = lasso_coef@x)
coef_df
coef_list <- as.character(coef_df$name[-1])
coef_list

coef_list <- c(coef_list, "low_inten_dev_1000", "med_inten_dev_1000", 
               "agriculture_1000") 
coef_list

pm_lur_data4 <- select(pm_lur_data, pm_ug_m3, coef_list)
names(pm_lur_data4)

pm_aic <- train(log(pm_ug_m3) ~ ., data = pm_lur_data4, 
                method = "lmStepAIC", trace = FALSE)

summary(pm_aic)
pm_aic$results

pm_aic_coef <- rownames(summary(pm_aic)[["coefficients"]])[-1]

#' ----------------------------------------------------------------------------- 
#' Compare AIC model to LASSO model
#' ----------------------------------------------------------------------------- 

pm_final_aic <- lm(log(pm_ug_m3) ~ ., 
                   data = select(pm_lur_data4, pm_ug_m3, pm_aic_coef))
summary(pm_final_aic)

pm_final_lasso <- lm(log(pm_ug_m3) ~ ., data = pm_lur_data4)
summary(pm_final_lasso)

#' RMSE
rmse(pm_final_aic$residuals)
rmse(pm_final_lasso$residuals)

#' MAE
mae(pm_final_aic$residuals)
mae(pm_final_lasso$residuals)

#' ----------------------------------------------------------------------------- 
#' Validate the LASSO model with cross validation
#' -----------------------------------------------------------------------------

#' leave-one-out cross validation: LASSO
tc <- trainControl(method = "LOOCV")
pm_final_cv <- train(log(pm_ug_m3) ~ ., data = pm_lur_data4,
                     method = "lm", trControl = tc)
pm_final_cv
varImp(pm_final_cv)

jpeg(here::here("Figs/Model_Dev", "PM_Final_Linear_Model_Diagnostics.jpeg"))
par(mfrow=c(2,2)) 
plot(pm_final_lasso, main = "Linear model with LASSO predictors")
dev.off()
par(mfrow=c(1,1)) 

hist(pm_final_lasso$residuals,
     main = "PM2.5 Residuals",
     xlab = "Residuals")

#' Predict PM2.5 concentrations at the sampled locations
#' Use the datset with the geometry data!
pm_locations$pred_log_pm_ug_m3 <- predict(pm_final_lasso)
summary(pm_locations$pred_log_pm_ug_m3)
summary(log(pm_locations$pm_ug_m3))

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' METHOD 2: 
#' Use a random forest model to select predictors
#' CAST package in R to faciliate model building and validation
#' A good tutorial here: https://cran.r-project.org/web/packages/CAST/vignettes/CAST-intro.html
#' 
#' Methods reported in Meyer et al. (2018) Env Mod & Soft
#' 
#' Need a temporal variable, so using month indicators; jan is reference month
#' ----------------------------------------------------------------------------- 
#' -----------------------------------------------------------------------------

names(lur_data)
pm_lur_data5 <- lur_data %>% 
  select(lon, lat, pm_ug_m3, start_predictors, month) %>% 
  na.omit() %>% 
  mutate(month = as.factor(month), space_id = paste(lon, lat, sep = "_"))
glimpse(pm_lur_data5)

#' ----------------------------------------------------------------------------- 
#' Random forest model training
#' Setting tuneLength to 1 to avoid hyperparameter tuning
#' According to Meyer, RF is relatively insensitive to tuning
#' Going to include a 5-fold cross validation
#' ----------------------------------------------------------------------------- 

library(CAST)

predictors <- names(select(pm_lur_data5, -c("pm_ug_m3", "space_id", "lon", "lat")))
predictors

rf_model0 <- train(pm_lur_data5[,predictors], log(pm_lur_data5$pm_ug_m3),
                   method = "rf", tuneLength = 1, importance = T,
                   trControl = trainControl(method = "cv", number = 5))
rf_model0
plot(varImp(rf_model0))

#' ----------------------------------------------------------------------------- 
#' Random forest model training
#' Using target-oriented validation: leave-location-out
#' ----------------------------------------------------------------------------- 

sp_indices <- CreateSpacetimeFolds(pm_lur_data5, spacevar = "space_id")
sp_indices

rf_model1 <- train(pm_lur_data5[,predictors], log(pm_lur_data5$pm_ug_m3),
                   method = "rf", tuneLength = 1, importance = T,
                   trControl = trainControl(method = "cv", 
                                            index = sp_indices$index))
rf_model1
plot(varImp(rf_model1))

#' ----------------------------------------------------------------------------- 
#' Random forest model training
#' Using target-oriented validation: leave-time-out
#' ----------------------------------------------------------------------------- 

t_indices <- CreateSpacetimeFolds(pm_lur_data5, timevar = "month")
t_indices

rf_model2 <- train(pm_lur_data5[,predictors], log(pm_lur_data5$pm_ug_m3),
                   method = "rf", tuneLength = 1, importance = T,
                   trControl = trainControl(method = "cv", 
                                            index = t_indices$index))
rf_model2
plot(varImp(rf_model2))

#' ----------------------------------------------------------------------------- 
#' Random forest model training
#' Forward feature selection to help reduce model overfitting
#' Want to use R2 as the evaluation metric
#' Just using the spatial folds
#' ----------------------------------------------------------------------------- 

rf_model3 <- ffs(pm_lur_data5[,predictors], log(pm_lur_data5$pm_ug_m3),
                 metric = "Rsquared", verbose = F, 
                 method = "rf", tuneLength = 1, importance = T,
                 trControl = trainControl(method = "cv", 
                                          index = sp_indices$index))
rf_model3
plot(varImp(rf_model3))
plot_ffs(rf_model3)

#' ----------------------------------------------------------------------------- 
#' Random forest model training
#' Forward feature selection to help reduce model overfitting
#' Want to use R2 as the evaluation metric
#' Going to use leave-location-and-time-out cross validation
#' ----------------------------------------------------------------------------- 

sp_t_indices <- CreateSpacetimeFolds(pm_lur_data5, spacevar = "space_id", 
                                     timevar = "month")

rf_model4 <- ffs(pm_lur_data5[,predictors], log(pm_lur_data5$pm_ug_m3),
                 metric = "Rsquared", verbose = F, 
                 method = "rf", tuneLength = 1, importance = T,
                 trControl = trainControl(method = "cv", 
                                          index = sp_t_indices$index))
rf_model4
plot(varImp(rf_model4))
plot_ffs(rf_model4)

#' ----------------------------------------------------------------------------- 
#' Pick the final RF model
#' ----------------------------------------------------------------------------- 

rf_model1
rf_model1$selectedvars

rf_model2
rf_model2$selectedvars

rf_model3
rf_model3$selectedvars

rf_model4
rf_model4$selectedvars

pm_final_rf <- rf_model4

#' ----------------------------------------------------------------------------- 
#' Predict PM2.5 concentrations at the sampled locations
#' ----------------------------------------------------------------------------- 

pm_lur_data5$pred_log_pm_ug_m3_rf <- predict(pm_final_rf)
summary(pm_lur_data5$pred_log_pm_ug_m3_rf)
summary(log(pm_lur_data$pm_ug_m3))

pm_locations <- pm_locations %>% 
  left_join(select(pm_lur_data5, lon, lat, month, pred_log_pm_ug_m3_rf),
            by = c("lon", "lat", "month"))

#' ----------------------------------------------------------------------------- 
#' ----------------------------------------------------------------------------- 
#' Compare measured and predicted PM2.5
#' Save the final results
#' ----------------------------------------------------------------------------- 
#' ----------------------------------------------------------------------------- 

ggplot(pm_locations) +
  geom_point(aes(x = log(pm_ug_m3), y = pred_log_pm_ug_m3, col = "lm", shape = "lm")) +
  geom_point(aes(x = log(pm_ug_m3), y = pred_log_pm_ug_m3_rf, col = "rf", shape = "rf")) +
  geom_abline(aes(slope = 1, intercept = 0), color = "black", linetype = 2) +
  scale_color_viridis(name = "Model", discrete = T,
                      labels = c("lm" = "Linear", "rf" = "Random Forest")) +
  scale_shape_manual(name = "Model",
                     values = c("lm" = 17, "rf" = 19),
                     labels = c("lm" = "Linear", "rf" = "Random Forest")) +
  xlab("Measured log(PM\u2082.\u2085)") + ylab("Predicted log(PM\u2082.\u2085)") +
  theme(legend.position = c(0.8, 0.2)) +
  simple_theme
ggsave(filename = here::here("Figs/Model_Dev", "PM_Meas_vs_Pred.jpeg"), device = "jpeg",
       width = 5, height = 5, units = "in")

#' ----------------------------------------------------------------------------- 
#' Save the final linear regression model and final dataset for further evaluation
#' -----------------------------------------------------------------------------

st_write(pm_locations, here::here("Data/Final_Data", "PM_Filter_LUR_Results.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

save(pm_final_lasso, file = here::here("Results", "PM_LUR_Linear_Model.rdata"))
save(pm_final_rf, file = here::here("Results", "PM_LUR_RForest_Model.rdata"))

