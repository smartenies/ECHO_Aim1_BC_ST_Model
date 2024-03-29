#' =============================================================================
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Choose spatial covariates
#' Date created: February 17, 2020
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
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
#' Updated 4/27/20:
#' Due to some issues fitting the ST model using all four campaigns, I'm going 
#' to drop Campaign 4 and see if that helps. Campaign 4 is really different from
#' the other three in terms of concentrations and variability.
#' 
#' Updated 1/4/21:
#' After doing some additional data cleaning of the campaigns 4 and 5 data, I'm
#' adding some of the campaign 4 data back in and including campaign 5 data
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
  text  = element_text(size = 12, color = 'black'),
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
#' Read in the data set-- 
#' Aggregate to the "annual" level at each location (using lon/lat as the ID)
#' -----------------------------------------------------------------------------

dist_data <- read_csv(here::here("Data", "Final_Filters_for_ST_Model.csv"))

sites_by_camp <- dplyr::select(dist_data, site_id, campaign) %>%
  group_by(site_id, campaign) %>%
  summarize(n_filters = n()) %>%
  pivot_wider(names_from = campaign, values_from = n_filters) 
sites_by_camp

sites_by_camp2 <- sites_by_camp %>%
  mutate(Campaign1 = ifelse(is.na(Campaign1), 0, 1),
         Campaign2 = ifelse(is.na(Campaign2), 0, 1),
         Campaign3 = ifelse(is.na(Campaign3), 0, 1),
         Campaign4 = ifelse(is.na(Campaign4), 0, 1),
         Campaign5 = ifelse(is.na(Campaign5), 0, 1)) %>%
  mutate(n_campaigns = Campaign1 + Campaign2 + Campaign3 + Campaign4 + Campaign5)
sites_by_camp2

#' Study-wide data for each sampling location
#' Not considering temperature or wildfire smoke right now
names(dist_data)
site_sp <- dplyr::select(dist_data, site_id, 
                  elevation_50:impervious_2500,
                  open_50:aadt_2500) %>% 
  distinct()

site_bc <- dplyr::select(dist_data, site_id, bc_ug_m3) %>%
  group_by(site_id) %>% 
  summarize(n_filters = n(),
            bc_ug_m3 = mean(bc_ug_m3, na.rm = T))

site_data <- left_join(site_bc, site_sp, by = "site_id") %>%
  filter(n_filters > 4)
glimpse(site_data)
summary(site_data$bc_ug_m3)
length(unique(site_data$bc_ug_m3))

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
  dplyr::select(bc_ug_m3, elevation_50:aadt_2500)
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
#' ----------------------------------------------------------------------------- 
#' ----------------------------------------------------------------------------- 

#' How many predictors are we starting with?
#' 91
ncol(dplyr::select(bc_lur_data, -c(bc_ug_m3)))

#' Looking at the continuous predictors now
names(bc_lur_data)
bc_continuous <- dplyr::select(bc_lur_data, elevation_50:aadt_2500)

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
#' Currently just elevation
bc_continuous_drop <- filter(bc_continuous_cv, pct_diff < 0.10 | cv < 0.10)
bc_continuous_drop

drop_vars2 <- bc_continuous_drop$key
drop_vars2

#' Calculate correlations
bc_continuous_cor <- cor(bc_continuous, use = "complete")
ggcorrplot(bc_continuous_cor, type = "upper", method = "square",
           ggtheme = simple_theme, lab = T, lab_col = "white",
           show.diag = T)

#' Which variables don't meet the r < 0.95 criterion?
flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut]
  )
}
bc_cor_flat <- flattenCorrMatrix(cormat = bc_continuous_cor)
bc_cor_flat

bc_cor_flat_check <- filter(bc_cor_flat, abs(cor) > 0.95)
bc_cor_drop <- unique(bc_cor_flat_check$row)
bc_cor_drop
bc_cor_drop <- bc_cor_drop[c(1:6,10:12,15,17:19,21)]

drop_vars2 <- c(drop_vars2, bc_cor_drop) 
drop_vars2 <- unique(drop_vars2)
drop_vars2

drop_vars2 <- unique(drop_vars2)
drop_vars2

#' Now how many predictors do we have?
#' 76 candidate predictors
bc_lur_data2 <- bc_lur_data %>%
  dplyr::select(-(all_of(drop_vars2)))
names(bc_lur_data2)

ncol(dplyr::select(bc_lur_data2, -bc_ug_m3))
summary(bc_lur_data2)

bc_lur_data2a <- bc_lur_data2

summary(bc_lur_data2a)

#' Scale all of the continuous predictors
bc_lur_data2a <- bc_lur_data2a %>% 
  mutate_at(.vars = -c(1), scale)
summary(bc_lur_data2a)

#' Fit some linear regression models
#' Just AADT
bc_lm1 <- lm(log(bc_ug_m3) ~ aadt_1000, data = bc_lur_data2a)
summary(bc_lm1)
par(mfrow=c(2,2)) 
plot(bc_lm1, main = "Just AADT within 1000 m: log-transformed outcome")
par(mfrow=c(1,1))
hist(bc_lm1$residuals)

#' AADT + tree cover
bc_lm2 <- lm(log(bc_ug_m3) ~ aadt_1000 + tree_cover_1000, data = bc_lur_data2a)
summary(bc_lm2)
par(mfrow=c(2,2)) 
plot(bc_lm2, main = "AADT (1000 m) and Tree Cover (1000 m): log-transformed outcome")
par(mfrow=c(1,1))
hist(bc_lm2$residuals)

summary(bc_lur_data2a$aadt_1000)
summary(bc_lur_data2a$tree_cover_1000)

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' METHOD 1: Use LASSO (caret package)
#' 
#' LASSO: Least Absolute Shrinkage and Selection Operator
#' Coefficients CAN shrink to 0 in this one
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------

library(caret)
set.seed(10000)

start_predictors <- colnames(bc_lur_data2a)[which(!(colnames(bc_lur_data2a) %in% 
                                                     c("bc_ug_m3", "site_id")))]

bc_lur_data4 <- dplyr::select(bc_lur_data2a, bc_ug_m3, all_of(start_predictors))

lambda <- 10^seq(-3, 3, length = 100)
tc_l = trainControl("cv", number = 10)
tg_l = expand.grid(alpha = 1, lambda = lambda)

log_bc_lasso <- train(log(bc_ug_m3) ~ ., data = bc_lur_data4,
                      method = "glmnet", trControl = tc_l, tuneGrid = tg_l)

log_bc_lasso$resample
plot(log_bc_lasso)

#' The plot above suggests that the lambda search window is too wide
lambda2 <- 10^seq(-2, 1, length = 100)
tc_l = trainControl("cv", number = 10)
tg_l2 = expand.grid(alpha = 1, lambda = lambda2)

log_bc_lasso2 <- train(log(bc_ug_m3) ~ ., data = bc_lur_data4,
                       method = "glmnet", trControl = tc_l, tuneGrid = tg_l2)

log_bc_lasso2$resample
plot(log_bc_lasso2)

#' The plot above suggests that the lambda search window is still a little too wide
lambda3 <- 10^seq(-2.5, -0.75, length = 100)
tc_l = trainControl("cv", number = 10)
tg_l3 = expand.grid(alpha = 1, lambda = lambda3)

log_bc_lasso3 <- train(log(bc_ug_m3) ~ ., data = bc_lur_data4,
                       method = "glmnet", trControl = tc_l, tuneGrid = tg_l3)

log_bc_lasso3$resample
plot(log_bc_lasso3)

log_bc_lasso_coef3 <- coef(log_bc_lasso3$finalModel, log_bc_lasso3$bestTune$lambda) 
log_bc_lasso_coef3

#' the lambda3 sequence looks good
getTrainPerf(log_bc_lasso3)
plot(log_bc_lasso3$finalModel)
arrange(log_bc_lasso3$results, RMSE) %>% head
log_bc_lasso3$bestTune
log_bc_lasso_coef3 <- coef(log_bc_lasso3$finalModel, log_bc_lasso3$bestTune$lambda)
log_bc_lasso_coef3

varImp(log_bc_lasso3)

#' Linear model with the LASSO predictors
lasso_coef_df <- data.frame(name = log_bc_lasso_coef3@Dimnames[[1]][log_bc_lasso_coef3@i + 1],
                            coefficient = log_bc_lasso_coef3@x)
covars <- as.character(lasso_coef_df$name[-1])
covars 

lasso_lm_df <- dplyr::select(bc_lur_data4, bc_ug_m3, all_of(covars))
summary(lasso_lm_df)

lasso_lm_model <- lm(log(bc_ug_m3) ~ ., data = lasso_lm_df)
summary(lasso_lm_model)

par(mfrow=c(2,2)) 
plot(lasso_lm_model, main = "LM with LASSO Predictors")
par(mfrow=c(1,1))
hist(lasso_lm_model$residuals)

#' -----------------------------------------------------------------------------
#' Save LASSO model
#' -----------------------------------------------------------------------------

save(log_bc_lasso3, log_bc_lasso_coef3, 
     file = here::here("Results", "BC_LASSO_Model_5C.Rdata"))

library(caret)
load(here::here("Results", "BC_LASSO_Model_5C.Rdata"))
getTrainPerf(log_bc_lasso3)
plot(log_bc_lasso3$finalModel)
arrange(log_bc_lasso3$results, RMSE) %>% head
log_bc_lasso3$bestTune