#' =============================================================================
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Predict BC (2018 estimates) for the 250 m grid
#' Date created: January 4, 2021
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description:
#' This script uses the ST model from script 16B to make predictions at grid 
#' locations and plots maps
#' 
#' Note: This script uses just the ST data from 2018 on, because the script that
#' was running on the server crashed. I'll try to update this as soon as possible,
#' but for now at least we'll have a map for 2018 to use for the grants
#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggspatial)
library(ggthemes)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)
library(viridis)

library(SpatioTemporal)
library(numDeriv)
library(Matrix)
library(plotrix)
library(maps)

simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "grey90"),
  panel.grid.major = element_line(color = "grey90"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)

map_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)

map_theme2 <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Prep the data
#' -----------------------------------------------------------------------------

lur_data_all <- read_csv(here::here("Data", "Final_BC_Data_Set_for_ST_Model.csv"))

#' Log-transformed BC observations with NA values for the missing dates
#' Note that campaign 5 has duplicate measures, so we need to average them
# bc_check <- lur_data_all %>%
#   group_by(site_id) %>%
#   summarize(all_na = ifelse(all(is.na(bc_ug_m3)), 1, 0)) %>%
#   filter(all_na == 1)
# bc_check

# lur_data <- filter(lur_data_all, !(site_id %in% bc_check$site_id))
lur_data <- lur_data_all

bc_obs <- lur_data %>%
  dplyr::select(site_id, st_week, bc_ug_m3) %>%
  group_by(site_id, st_week) %>%
  summarize(bc_ug_m3 = mean(bc_ug_m3, na.rm = T)) %>%
  mutate(bc_ug_m3 = ifelse(is.nan(bc_ug_m3), NA, bc_ug_m3)) %>%
  mutate(log_bc = log(bc_ug_m3)) %>%
  dplyr::select(-bc_ug_m3) 

#' Pivot to a wide data frame
bc_obs2 <- bc_obs %>%
  pivot_wider(id_cols = st_week, names_from = site_id, values_from = log_bc) %>%
  # names_from = site_id, values_from = bc_ug_m3) %>%
  as.data.frame() %>%
  arrange(st_week)
rownames(bc_obs2) <- bc_obs2$st_week
bc_obs2$st_week <- NULL
bc_obs2 <- as.matrix(bc_obs2)

bc_weeks <- rownames(bc_obs2)
tail(bc_weeks)

#' Spatial covariates (scaled)
load(here::here("Results", "BC_LASSO_Model_5C.Rdata"))

lasso_coef_df <- data.frame(name = log_bc_lasso_coef3@Dimnames[[1]][log_bc_lasso_coef3@i + 1],
                            coefficient = log_bc_lasso_coef3@x)
covars <- as.character(lasso_coef_df$name[-1])
covars 

covar_fun <- paste("~", paste(covars, collapse = " + "))
covar_fun

bc_sp_cov <- select(lur_data, site_id, lon, lat, all_of(covars)) %>%
  distinct() #%>%
  # mutate_at(.vars = vars(all_of(covars)),
  #           scale)
summary(bc_sp_cov)

bc_sp_cov <- bc_sp_cov %>%
  rename(ID = site_id) %>%
  mutate(lon2 = lon, lat2 = lat) %>%
  st_as_sf(coords = c("lon2", "lat2"), crs = ll_wgs84) %>%
  st_transform(crs = albers)
sp_coords <- do.call(rbind, st_geometry(bc_sp_cov)) %>% as_tibble()
names(sp_coords) <- c("x", "y")

bc_sp_cov <- bind_cols(bc_sp_cov, sp_coords) %>%
  st_set_geometry(NULL) %>%
  as.data.frame()
head(bc_sp_cov)

bc_sp_cov$type <- ifelse(bc_sp_cov$ID == "central", "central", "dist")
bc_sp_cov$type <- as.factor(bc_sp_cov$type)
summary(bc_sp_cov)
cor(bc_sp_cov[,covars])

#' NO2, and smoke spatiotemporal predictors
#' NO2 at each site estimated using IDW
bc_st_no2 <- dplyr::select(lur_data, site_id, st_week, idw_no2) %>%
  distinct() %>%
  arrange(st_week) %>%
  pivot_wider(id_cols = st_week,
              names_from = site_id, values_from = idw_no2) %>%
  as.data.frame()
rownames(bc_st_no2) <- bc_st_no2$st_week
bc_st_no2$st_week <- NULL
bc_st_no2 <- as.matrix(bc_st_no2)

no2_weeks <- rownames(bc_st_no2)
tail(no2_weeks)
setdiff(bc_weeks, no2_weeks)

#' Smoke day indicator variable based on WFS paper method (see Brey and Fischer, 2016)
#' based on any smoke variable in the area using the 2 sd cut-off (area_smoke_2sd)
bc_st_smk <- dplyr::select(lur_data, site_id, st_week, area_smoke_2sd) %>%
  distinct() %>%
  arrange(st_week) %>%
  pivot_wider(id_cols = st_week,
              names_from = site_id, values_from = area_smoke_2sd) %>%
  as.data.frame()
rownames(bc_st_smk) <- bc_st_smk$st_week
bc_st_smk$st_week <- NULL
bc_st_smk <- as.matrix(bc_st_smk)

smk_weeks <- rownames(bc_st_smk)
tail(smk_weeks)
tail(bc_weeks)
setdiff(bc_weeks, smk_weeks)

#' -----------------------------------------------------------------------------
#' Read in the necessary data sets for the grid and format for the ST model
#' Note: these are the unobserved locations
#' -----------------------------------------------------------------------------

#' Grid SP predictors
grid_sp <- read_csv(here::here("Data", "Spatial_Covariates_Grid_250_m_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

grid_cent <- st_centroid(grid_sp) %>% 
  st_transform(crs = ll_wgs84)

sp_coords <- do.call(rbind, st_geometry(grid_cent)) %>%
  as_tibble() %>% setNames(c("lon","lat"))
grid_sp <- bind_cols(grid_sp, sp_coords)

grid_cent2 <- st_centroid(grid_sp)

sp_coords2 <- do.call(rbind, st_geometry(grid_cent2)) %>%
  as_tibble() %>% setNames(c("x","y"))
grid_sp <- bind_cols(grid_sp, sp_coords2) %>% 
  st_set_geometry(NULL) %>%
  as.data.frame()

names(grid_sp)

grid_sp_cov <- dplyr::select(grid_sp, grid_id, lon, lat, x, y, all_of(covars)) %>%
  distinct() #%>%
  #mutate_at(.vars = vars(all_of(covars)), scale)

grid_sp_cov <- grid_sp_cov %>%
  rename(ID = grid_id) %>% 
  mutate(ID = paste0("g_", ID)) %>% 
  mutate(type = ifelse(ID == "central", "central", "dist")) %>% 
  mutate(type = as.factor(type))

head(grid_sp_cov)
class(grid_sp_cov)
cor(grid_sp_cov[,c(6:ncol(grid_sp_cov)-1)])

#' Grid ST predictors
#' Need to read in specific files-- due to the size of the grid and the weekly
#' resolution, we needed to save each week's ST variables separately

grid_start <- as.Date("01/01/2018", format = "%m/%d/%Y")
grid_end <- as.Date("12/31/2018", format = "%m/%d/%Y")
grid_dates <- seq.Date(grid_start, grid_end, by = "day")
grid_weeks <- unique(as.character(as.Date(cut(as.Date(grid_dates), "week"))))

#' grid_st_pm <- data.frame()
#' grid_st_no2 <- data.frame()
#' grid_st_temp <- data.frame()
#' grid_st_smk <- data.frame()
#' 
#' for(i in 1:length(grid_weeks)) {
#' # for(i in 1:5) {
#'   if(i %% 5 == 0) print(paste0("Week ", i, " of ", length(grid_weeks)))
#'   
#'   if(file.exists(here::here("Data/Grid_ST_Data", 
#'                             paste0("grid_st_cov_", grid_weeks[i], ".csv")))) {
#'     temp_st <- read_csv(here::here("Data/Grid_ST_Data", 
#'                                    paste0("grid_st_cov_", grid_weeks[i], ".csv")))
#'     
#'     # Separate wide data frames for each variable
#'     temp_pm <- select(temp_st, grid_id, week_id, idw_pm) %>%
#'       pivot_wider(id_cols = week_id,
#'                   names_from = grid_id, values_from = idw_pm) %>%
#'       as.data.frame()
#'     
#'     temp_no2 <- select(temp_st, grid_id, week_id, idw_no2) %>%
#'       pivot_wider(id_cols = week_id,
#'                   names_from = grid_id, values_from = idw_no2) %>%
#'       as.data.frame()
#'     
#'     temp_temp <- select(temp_st, grid_id, week_id, idw_temp) %>%
#'       pivot_wider(id_cols = week_id,
#'                   names_from = grid_id, values_from = idw_temp) %>%
#'       as.data.frame()
#'     
#'     temp_smk <- select(temp_st, grid_id, week_id, area_smoke_2sd) %>%
#'       pivot_wider(id_cols = week_id,
#'                   names_from = grid_id, values_from = area_smoke_2sd) %>%
#'       as.data.frame()
#'     
#'     #' Bind rows
#'     grid_st_pm <- bind_rows(grid_st_pm, temp_pm)
#'     grid_st_no2 <- bind_rows(grid_st_no2, temp_no2)
#'     grid_st_temp <- bind_rows(grid_st_temp, temp_temp)
#'     grid_st_smk <- bind_rows(grid_st_smk, temp_smk)
#'     
#'     # Remove objects
#'     rm(temp_st, temp_pm, temp_no2, temp_temp, temp_smk)
#'   }
#' }
#' 
#' rownames(grid_st_pm) <- grid_st_pm$week_id
#' grid_st_pm$week_id <- NULL
#' grid_st_pm <- as.matrix(grid_st_pm)
#' 
#' rownames(grid_st_no2) <- grid_st_no2$week_id
#' grid_st_no2$week_id <- NULL
#' grid_st_no2 <- as.matrix(grid_st_no2)
#' 
#' rownames(grid_st_temp) <- grid_st_temp$week_id
#' grid_st_temp$week_id <- NULL
#' grid_st_temp <- as.matrix(grid_st_temp)
#' 
#' rownames(grid_st_smk) <- grid_st_smk$week_id
#' grid_st_smk$week_id <- NULL
#' grid_st_smk <- as.matrix(grid_st_smk)
#' 
#' save(grid_st_pm, grid_st_no2, grid_st_temp, grid_st_smk,
#'      file = here::here("Data", "Grid_ST_Matrices.rdata"))

load(here::here("Data", "Grid_ST_Matrices.rdata"))
colnames(grid_st_pm) <- paste0("g_", colnames(grid_st_pm))
colnames(grid_st_no2) <- paste0("g_", colnames(grid_st_no2))
colnames(grid_st_temp) <- paste0("g_", colnames(grid_st_temp))
colnames(grid_st_smk) <- paste0("g_", colnames(grid_st_smk))

#' Grid OBS (NA for now!)
grid_obs <- matrix(data = NA, 
                   ncol = length(unique(grid_sp$grid_id)),
                   nrow = length(grid_weeks))
dim(grid_obs)
rownames(grid_obs) <- grid_weeks
colnames(grid_obs) <- paste0("g_", unique(grid_sp$grid_id))

#' -----------------------------------------------------------------------------
#' Combine the data sets
#' -----------------------------------------------------------------------------

#' Observations
bc_obs_df <- as.data.frame(bc_obs) %>%
  filter(st_week %in% as.Date(grid_weeks)) %>%
  mutate(st_week = as.character(st_week)) %>%
  pivot_wider(names_from = site_id, values_from = log_bc)

grid_obs_df <- as.data.frame(grid_obs)

nrow(bc_obs_df) == nrow(grid_obs_df)

obs <- as.data.frame(bind_cols(bc_obs_df, grid_obs_df)) 
rownames(obs) <- obs$st_week
obs$st_week <- NULL
obs <- as.matrix(obs)

head(rownames(obs))
head(colnames(obs))

#' SP predictors
sp_cov <- bind_rows(bc_sp_cov, grid_sp_cov) %>%
  mutate_at(.vars = vars(all_of(covars)),
            scale)
sp_cov$type <- as.factor(sp_cov$type)
glimpse(sp_cov)

#' ST predictors
#' NO2
bc_st_no2_df <- as.data.frame(bc_st_no2)
bc_st_no2_df$week <- rownames(bc_st_no2)

grid_st_no2_df <- as.data.frame(grid_st_no2)
grid_st_no2_df$week <- rownames(grid_st_no2)

st_no2 <- left_join(bc_st_no2_df, grid_st_no2_df, by = "week")
rownames(st_no2) <- st_no2$week
st_no2$week <- NULL
st_no2 <- as.matrix(st_no2)
st_no2 <- st_no2[rownames(st_no2) %in% rownames(obs),]

#' smoke
bc_st_smk_df <- as.data.frame(bc_st_smk)
bc_st_smk_df$week <- rownames(bc_st_smk)

grid_st_smk_df <- as.data.frame(grid_st_smk)
grid_st_smk_df$week <- rownames(grid_st_smk)

st_smk <- left_join(bc_st_smk_df, grid_st_smk_df, by = "week")
rownames(st_smk) <- st_smk$week
st_smk$week <- NULL
st_smk <- as.matrix(st_smk)
st_smk <- st_smk[rownames(st_smk) %in% rownames(obs),]

#' -----------------------------------------------------------------------------
#' Create the model objects
#' -----------------------------------------------------------------------------

full.data <- createSTdata(obs = obs,
                          covars = sp_cov,
                          SpatioTemporal = list(bc_st_no2 = st_no2,
                                                bc_st_smk = st_smk))
full.data
D <- createDataMatrix(full.data)
D

#' Trend data for the model based on long-term NO2 and BC at the central sites
#' These data are stored in the Denver_ST_Model_B.rdata object
#' Need to filter this to just the year we are including right now

load(here::here("Results", "Denver_ST_Model_B.rdata"))

new_trend <- denver.data.B$trend %>% 
  filter(date %in% as.Date(rownames(obs)))
# mutate(year = year(date)) %>% 
# filter(year %in% c("2016", "2017", "2018", "2019")) 

# Add the smooth temporal basis functions to the ST data object
full.data$trend <- new_trend

head(full.data$trend)
plot(full.data$trend$date, full.data$trend$V2, col = 1, pch = 16, cex = 0.5,
     xlab = "Date", ylab = "Pollutants (scaled)")
points(full.data$trend$date, full.data$trend$V1, col = 2, pch = 16, cex = 0.5)
lines(full.data$trend$date, full.data$trend$V2, col = 1)
lines(full.data$trend$date, full.data$trend$V1, col = 2)

#' Check out the fit for some sites
par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(full.data, "obs", ID="d_1", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend d_1")
plot(full.data, "res", ID="d_1", xlab="", ylab="BC (log ug/m3)")
plot(full.data, "acf", ID="d_1")
plot(full.data, "pacf", ID="d_1")

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(full.data, "obs", ID="d_41", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend d_41")
plot(full.data, "res", ID="d_41", xlab="", ylab="BC (log ug/m3)")
plot(full.data, "acf", ID="d_41")
plot(full.data, "pacf", ID="d_41")

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(full.data, "obs", ID="d_53", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend d_53")
plot(full.data, "res", ID="d_53", xlab="", ylab="BC (log ug/m3)")
plot(full.data, "acf", ID="d_53")
plot(full.data, "pacf", ID="d_53")

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(full.data, "obs", ID="central", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend central")
plot(full.data, "res", ID="central", xlab="", ylab="BC (log ug/m3)")
plot(full.data, "acf", ID="central")
plot(full.data, "pacf", ID="central")

names(full.data$covars)

LUR <- list(covar_fun, covar_fun, covar_fun)

cov.beta <-  list(covf = c("iid", "iid", "iid"), nugget = T)
cov.nu <- list(covf = "exp", nugget = T, random.effect = FALSE)
locations <- list(coords = c("x","y"), long.lat = c("lon","lat"), others= "type")

full.model <- createSTmodel(full.data, LUR = LUR,
                            ST = c("bc_st_no2", "bc_st_smk"),
                            cov.beta = cov.beta, cov.nu = cov.nu,
                            locations = locations)
full.model

#' -----------------------------------------------------------------------------
#' Following the tutorial, I'm going to use the parameters estimated for the
#' "final" model selected in "17a_Fitting_BC_ST_Model.Rmd"
#' -----------------------------------------------------------------------------

load(here::here("Results", "Denver_ST_Model_B.rdata"))
denver.model.B
 
x <- coef(est.denver.model.B, pars = "cov")$par 
x

#' Compute the predictions 
#' E.1 computes predictions for all of the locations (observed and unobserved)
#' Getting the long-term average and the unbiased tranformation back to the 
#' original units

E.1 <- predict(full.model, x, LTA = T, transform="unbiased", pred.var=FALSE)
print(E.1)

save(E.1, file = here::here("Results", "Grid_LTA_Preds_2018.rdata"))

#' -----------------------------------------------------------------------------
#' Check out the predicted values
#' -----------------------------------------------------------------------------

#' load(here::here("Results", "Grid_LTA_Preds_2018.rdata"))
#' 
#' #' Long term averages for 2018
#' lta_preds <- as.data.frame(E.1$EX) %>% 
#'   mutate(week = as.Date(rownames(E.1$EX)))
#' 
#' lta_weeks <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day") 
#' lta_sites <- colnames(lta_preds)[which(str_detect(colnames(lta_preds), "g_"))]
#' 
#' lta_preds_avg <- lta_preds %>% 
#'   filter(week %in% lta_weeks) %>% 
#'   summarize_at(vars(all_of(lta_sites)), mean, na.rm = TRUE)
#' dim(lta_preds_avg)
#' 
#' grid_lta_preds <- lta_preds_avg %>% 
#'   pivot_longer(cols = starts_with("g_"), names_to = "grid_id", values_to = "EX") %>% 
#'   mutate(grid_id = gsub("_mean", "", grid_id))
#' head(grid_lta_preds)
#' 
#' summary(grid_lta_preds$EX)

#' -----------------------------------------------------------------------------
#' Plot LTA for grid locations
#' -----------------------------------------------------------------------------

# grid_sf <- read_csv(here::here("Data", "Spatial_Covariates_Grid_250_m_AEA.csv")) %>% 
#   mutate(grid_id = paste0("g_", grid_id))
# 
# grid_sf_lta <- left_join(grid_sf, grid_lta_preds, by = "grid_id") %>% 
#   st_as_sf(wkt = "WKT", crs = albers)
# 
# highways <- read_csv(here::here("Data", "Highways_AEA.csv")) %>% 
#   st_as_sf(wkt = "WKT", crs = albers)
# highways_crop <- st_crop(highways, st_bbox(grid_sf_lta))
# 
# ggplot() +
#   geom_sf(data = grid_sf_lta, aes(fill = EX), color = NA) +
#   geom_sf(data = highways_crop, color = "white", size = 1) +
#   scale_fill_viridis(name = "2018 mean\n BC (\u03BCg/m\u00B3)",
#                      option = "magma", 
#                      breaks = waiver(), n.breaks = 6) +
#   annotation_scale(data = grid_sf_lta, plot_unit = "m", 
#                    location = "bl", width_hint = 0.5,
#                    pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
#                    text_cex = 1.5, bar_cols = c("grey50", "white"),
#                    line_col = "white", text_col = "white", text_face = "bold") +
#   annotation_north_arrow(data = grid_sf_lta, 
#                          location = "bl", which_north = "grid", 
#                          pad_x = unit(0.75, "cm"), pad_y = unit(1.5, "cm"),
#                          style = north_arrow_minimal(line_col = "white", 
#                                                      fill = "white", 
#                                                      line_width = 1.5,
#                                                      text_col = "white",
#                                                      text_face = "bold",
#                                                      text_size = 14)) +
#   xlab("") + ylab("") +
#   map_theme +
#   theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
#   theme(axis.ticks = element_blank(),
#         axis.text = element_blank()) +
#   theme(legend.position = c(0.81, 0.8),
#         legend.background = element_rect(fill = "white"),
#         legend.title = element_text(size = 12, face = "bold"),
#         legend.text= element_text(size = 12, face = "bold")) +
#   theme(panel.border = element_blank())
# ggsave(here::here("Figs", "BC_LTA_2018_v1.jpeg"),
#        height = 7, width = 6, units = "in", device = "jpeg", dpi = 750)

ggplot() +
  geom_sf(data = grid_sf_lta, aes(fill = EX), color = NA) +
  geom_sf(data = highways_crop, color = "white", size = 1) +
  scale_fill_viridis(name = "2018 mean\n BC (\u03BCg/m\u00B3)",
                     breaks = waiver(), n.breaks = 6) +
  annotation_scale(data = grid_sf_lta, plot_unit = "m", 
                   location = "bl", width_hint = 0.5,
                   pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
                   text_cex = 1.5, bar_cols = c("grey50", "white"),
                   line_col = "white", text_col = "white", text_face = "bold") +
  annotation_north_arrow(data = grid_sf_lta, 
                         location = "bl", which_north = "grid", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_minimal(line_col = "white", 
                                                     fill = "white", 
                                                     line_width = 1.5,
                                                     text_col = "white",
                                                     text_face = "bold",
                                                     text_size = 12)) +
  xlab("") + ylab("") +
  map_theme +
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  theme(legend.position = "c(0.81, 0.8)",
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text= element_text(size = 12, face = "bold")) +
  theme(panel.border = element_blank())
ggsave(here::here("Figs", "BC_Grid_LTA_2018.jpeg"),
       height = 7, width = 6, units = "in", device = "jpeg", dpi = 750)

#' 
#' #' -----------------------------------------------------------------------------
#' #' Boxplot of weekly predicted BC at each of the sampling locations 
#' #' grouped by month
#' #' -----------------------------------------------------------------------------
#' 
#' denver <- st_read(here::here("Data", "Colorado_County_Boundaries.shp")) %>% 
#'   filter(CNTY_FIPS == "031") %>% 
#'   st_transform(crs = albers)
#' plot(st_geometry(denver))
#' 
#' library(lubridate)
#' 
#' #' Which site IDs are residential locations?
#' dist_sites <- select(lur_data, site_id, WKT) %>% 
#'   filter(site_id != "central") %>% 
#'   distinct() %>% 
#'   st_as_sf(wkt = "WKT", crs = albers)
#' 
#' part_sites <- select(lur_data, participant, needs_geocoding, site_id, WKT) %>% 
#'   filter(!is.na(needs_geocoding)) %>% 
#'   filter(needs_geocoding == 1 | participant == 1) %>% 
#'   distinct() %>% 
#'   st_as_sf(wkt = "WKT", crs = albers)
#' not_part <- c("d_1", "d_3")
#' 
#' res_sites <- filter(part_sites, !(site_id %in% not_part))
#' den_res_sites <- res_sites[denver,]
#' den_sites <- dist_sites[denver,]
#' 
#' #' Weekly predictions for 2018 (all distributed sites, n = 60)
#' week_preds <- as.data.frame(E.1$EX) %>% 
#'   mutate(week = as.Date(rownames(E.1$EX)))
#' 
#' week_weeks <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day") 
#' week_sites <- colnames(week_preds)[which(str_detect(colnames(week_preds), "d_"))]
#' 
#' box_preds <- week_preds %>% 
#'   select(week, all_of(week_sites)) %>% 
#'   filter(week %in% week_weeks) %>% 
#'   mutate(month = month(week))
#' 
#' box_data <- box_preds %>% 
#'   pivot_longer(-c(week, month), names_to = "location", values_to = "pred")
#' summary(box_data)
#' 
#' #' Weekly predictions for 2018 (all residential sites; n = 31)
#' box_preds2 <- week_preds %>% 
#'   select(week, all_of(res_sites$site_id)) %>% 
#'   filter(week %in% week_weeks) %>% 
#'   mutate(month = month(week))
#' 
#' box_data2 <- box_preds2 %>% 
#'   pivot_longer(-c(week, month), names_to = "location", values_to = "pred")
#' summary(box_data2)
#' 
#' #' Weekly predictions for 2018 (all distirbuted sites in Denver; n = 22)
#' box_preds3 <- week_preds %>% 
#'   select(week, all_of(den_sites$site_id)) %>% 
#'   filter(week %in% week_weeks) %>% 
#'   mutate(month = month(week))
#' 
#' box_data3 <- box_preds3 %>% 
#'   pivot_longer(-c(week, month), names_to = "location", values_to = "pred")
#' summary(box_data3)
#' 
#' #' Weekly predictions for 2018 (all residential sites in Denver; n = 10)
#' box_preds4 <- week_preds %>% 
#'   select(week, all_of(den_res_sites$site_id)) %>% 
#'   filter(week %in% week_weeks) %>% 
#'   mutate(month = month(week))
#' 
#' box_data4 <- box_preds4 %>% 
#'   pivot_longer(-c(week, month), names_to = "location", values_to = "pred")
#' summary(box_data4)
#' 
#' #' Box plot of weekly BC predicted at all distributed sites grouped by month
#' box_summary <- box_data %>% 
#'   group_by(month) %>% 
#'   summarize(median = median(pred)) %>% 
#'   arrange(desc(median))
#' box_summary  
#' 
#' month.name
#' 
#' box_plot <- ggplot(box_data) +
#'   geom_boxplot(aes(x = as.factor(month), y = pred, color = as.factor(month))) +
#'   scale_color_viridis(name = "Month", discrete = T) +
#'   xlab("Month") + ylab("Distributed site BC concentration (\u03BCg/m\u00B3): Predicted") +
#'   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
#'   scale_x_discrete(labels = month.name) +
#'   simple_theme
#' box_plot
#' ggsave(filename = here::here("Figs", "Pred_BC_At_Dist_Sites.jpeg"),
#'        height = 5, width = 8, units = "in", device = "jpeg", dpi = 500)
#' 
#' 
#' #' -----------------------------------------------------------------------------
#' #' Plot LTA for grid locations within Denver county only
#' #' -----------------------------------------------------------------------------
#' 
#' library(smoothr)
#' denver <- st_read(here::here("Data", "Colorado_County_Boundaries.shp")) %>% 
#'   filter(CNTY_FIPS == "031") %>% 
#'   st_transform(crs = albers)
#' plot(st_geometry(denver))
#' 
#' denver2 <- fill_holes(denver, 10000000)
#' plot(st_geometry(denver2))
#' 
#' grid_sf <- read_csv(here::here("Data", "Spatial_Covariates_Grid_250_m_AEA.csv")) %>% 
#'   mutate(grid_id = paste0("g_", grid_id))
#' 
#' grid_sf_lta <- left_join(grid_sf, grid_lta_preds, by = "grid_id") %>% 
#'   st_as_sf(wkt = "WKT", crs = albers)
#' 
#' plot(st_geometry(grid_sf_lta))
#' 
#' grid_sf_lta_crop <- grid_sf_lta[denver2,]
#' 
#' plot(st_geometry(grid_sf_lta_crop))
#' plot(st_geometry(denver2), add = T)
#' 
#' highways <- read_csv(here::here("Data", "Highways_AEA.csv")) %>% 
#'   st_as_sf(wkt = "WKT", crs = albers)
#' highways_crop <- highways[denver2,]
#' 
#' ggplot() +
#'   geom_sf(data = grid_sf_lta_crop, aes(fill = EX), color = NA) +
#'   geom_sf(data = highways_crop, color = "white", size = 1) +
#'   scale_fill_viridis(name = "2018 mean BC (\u03BCg/m\u00B3)",
#'                      option = "magma", 
#'                      breaks = waiver(), n.breaks = 6) +
#'   annotation_scale(data = grid_sf_lta_crop, plot_unit = "m", 
#'                    location = "br", width_hint = 0.5,
#'                    pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
#'                    text_cex = 1.5,
#'                    line_col = "black", text_col = "black", text_face = "bold") +
#'   annotation_north_arrow(data = grid_sf_lta_crop, 
#'                          location = "br", which_north = "grid", 
#'                          pad_x = unit(0.75, "cm"), pad_y = unit(1.6, "cm"),
#'                          style = north_arrow_minimal(line_col = "black", 
#'                                                      fill = "black", 
#'                                                      line_width = 1.5,
#'                                                      text_col = "black",
#'                                                      text_face = "bold",
#'                                                      text_size = 14)) +
#'   xlab("") + ylab("") +
#'   map_theme +
#'   theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
#'   theme(axis.ticks = element_blank(),
#'         axis.text = element_blank()) +
#'   theme(legend.position = c(0.8, 0.35),
#'         legend.background = element_rect(fill = "transparent"),
#'         legend.title = element_text(size = 18, face = "bold"),
#'         legend.text= element_text(size = 16, face = "bold")) +
#'   theme(panel.border = element_blank())
#' ggsave(here::here("Figs", "BC_LTA_Denver_2018_v1.jpeg"),
#'        height = 6, width = 8, units = "in", device = "jpeg", dpi = 750)
#' 
#' ggplot() +
#'   geom_sf(data = grid_sf_lta_crop, aes(fill = EX), color = NA) +
#'   geom_sf(data = highways_crop, color = "white", size = 1) +
#'   scale_fill_viridis(name = "2018 mean BC (\u03BCg/m\u00B3)",
#'                      breaks = waiver(), n.breaks = 6) +
#'   annotation_scale(data = grid_sf_lta_crop, plot_unit = "m", 
#'                    location = "br", width_hint = 0.5,
#'                    pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
#'                    text_cex = 1.5,
#'                    line_col = "black", text_col = "black", text_face = "bold") +
#'   annotation_north_arrow(data = grid_sf_lta_crop, 
#'                          location = "br", which_north = "grid", 
#'                          pad_x = unit(0.75, "cm"), pad_y = unit(1.5, "cm"),
#'                          style = north_arrow_minimal(line_col = "black", 
#'                                                      fill = "black", 
#'                                                      line_width = 1.6,
#'                                                      text_col = "black",
#'                                                      text_face = "bold",
#'                                                      text_size = 12)) +
#'   xlab("") + ylab("") +
#'   map_theme +
#'   theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
#'   theme(axis.ticks = element_blank(),
#'         axis.text = element_blank()) +
#'   theme(legend.position = c(0.75, 0.3),
#'         legend.background = element_rect(fill = "white"),
#'         legend.title = element_text(size = 18, face = "bold"),
#'         legend.text= element_text(size = 16, face = "bold")) +
#'   theme(panel.border = element_blank())
#' ggsave(here::here("Figs", "BC_LTA_Denver_2018_v2.jpeg"),
#'        height = 6, width = 8, units = "in", device = "jpeg", dpi = 750)
#' 
#' 
#' #' -----------------------------------------------------------------------------
#' #' Downtown Denver with an inset map showing the bounding box
#' #' -----------------------------------------------------------------------------
#' 
#' register_google(google_api_key)
#' 
#' denver <- st_read(here::here("Data", "Colorado_County_Boundaries.shp")) %>% 
#'   filter(CNTY_FIPS == "031") %>% 
#'   st_transform(crs = ll_wgs84)
#' plot(st_geometry(denver))
#' 
#' grid_pts <- data.frame(lon = c(-105.032454, -105.032454, -104.940688, -104.940688),
#'                        lat = c(39.70, 39.80, 39.70, 39.80)) %>% 
#'   st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84) 
#' grid_box <- st_convex_hull(st_union(grid_pts))
#' plot(st_geometry(grid_pts))
#' plot(st_geometry(grid_box), add = T)
#' 
#' grid_bounds <- data.frame(lon = c(-105.035, -105.035, -104.85, -104.85),
#'                           lat = c(39.70, 39.80, 39.70, 39.80)) %>% 
#'   st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84) %>% 
#'   st_transform(crs = albers)
#' 
#' grid_box2 <- st_transform(grid_box, crs = albers)
#' 
#' highways2 <- read_csv(here::here("Data", "Highways_AEA.csv")) %>% 
#'   st_as_sf(wkt = "WKT", crs = albers) %>% 
#'   st_transform(crs = ll_wgs84)
#' highways2_crop <- highways2[grid_box,]
#' 
#' grid_sf_lta2 <- grid_st_lta
#' 
#' #' Base map of the area
#' base_map <- get_map(location = "Denver, CO", zoom = 11, maptype = "roadmap")
#' ggmap(base_map)
#' attr(base_map, "bb")
#' 
#' g1 <- ggmap(base_map) +
#'   geom_sf(data = grid_box, fill = NA, col = "red", size = 3, inherit.aes = F) +
#'   xlab("") + ylab("") +
#'   ylim(c(39.65, 39.85)) + xlim(c(-105.15, -104.80)) +
#'   map_theme2
#' g1
#' 
#' g2 <- ggplot() +
#'   geom_sf(data = grid_bounds, color = "transparent") +
#'   geom_sf(data = grid_sf_lta[grid_box2,], aes(fill = EX), color = NA) +
#'   geom_sf(data = highways2_crop, color = "white", size = 2) +
#'   scale_fill_viridis(name = "2018 mean\n BC (\u03BCg/m\u00B3)",
#'                      breaks = waiver(), n.breaks = 6) +
#'   annotation_scale(data = grid_sf_lta[grid_box2,], plot_unit = "m", 
#'                    location = "bl", width_hint = 0.5,
#'                    pad_x = unit(1.25, "cm"), 
#'                    pad_y = unit(1.1, "cm"),
#'                    text_cex = 1, bar_cols = c("grey90", "black"),
#'                    line_col = "black", text_col = "black", text_face = "bold") +
#'   annotation_north_arrow(data = grid_sf_lta[grid_box2,], 
#'                          location = "bl", which_north = "grid", 
#'                          pad_x = unit(1.5, "cm"), 
#'                          pad_y = unit(.75, "cm"),
#'                          style = north_arrow_minimal(line_col = "black", 
#'                                                      fill = "black", 
#'                                                      line_width = 1.5,
#'                                                      text_col = "black",
#'                                                      text_face = "bold",
#'                                                      text_size = 12)) +
#'   xlab("") + ylab("") +
#'   map_theme2 +
#'   # theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
#'   theme(legend.position = c(0.65, 0.65),
#'         legend.background = element_rect(fill = "white"),
#'         legend.title = element_text(size = 12, face = "bold"),
#'         legend.text= element_text(size = 12, face = "bold")) +
#'   theme(panel.border = element_blank())
#' g2