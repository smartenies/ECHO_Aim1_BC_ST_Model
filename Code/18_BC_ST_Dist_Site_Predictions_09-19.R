#' =============================================================================
#' Project: ECHO LUR
#' Date created: July 8, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script uses the ST model from script 16B to make predictions at the 
#' distributed site locations from 2009-2019
#' 
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
#' Read in the observed location (i.e., filter and aethalometer) data
#' Format it for the ST model
#' -----------------------------------------------------------------------------

#' Filter out unusable filters
data_name <- "Combined_Filter_Data_AEA.csv"
lur_data <- read_csv(here::here("Data", data_name)) %>% 
  filter(!is.na(lon)) %>% 
  filter(indoor == 0) %>% 
  filter(bc_ug_m3_dem > 0) %>% 
  filter(is.na(below_lod) | below_lod == 0) %>% 
  filter(is.na(low_volume_flag) | low_volume_flag == 0) %>% 
  filter(is.na(flow_rate_flag) | flow_rate_flag == 0) %>% 
  filter(is.na(is_blank) | is_blank == 0) %>% 
  filter(is.na(negative_pm_mass) | negative_pm_mass == 0) %>% 
  filter(is.na(potential_contamination) | potential_contamination == 0) %>% 
  filter(is.na(bc_mass_ug_corrected) | bc_mass_ug_corrected >= 0.49) %>% 
  filter(campaign %in% paste0("Campaign", c(1, 2, 3, "X")))

#' Rename the concentrations (using deming regression here)
lur_data <- lur_data %>%
  rename("bc_ug_m3_raw" = "bc_ug_m3") %>%
  rename("bc_ug_m3" = "bc_ug_m3_dem")

#' Summarize distributed and central site BC
dist <- filter(lur_data, campaign != "CampaignX")
central <- filter(lur_data, campaign == "CampaignX")

summary(dist$bc_ug_m3)
sd(dist$bc_ug_m3)

summary(central$bc_ug_m3)
sd(central$bc_ug_m3)

#' Format the date variable to be Year-WeekNo. (starts on Sunday)
#' Convert that back to the first day of the week
lur_data <- lur_data %>%
  mutate(sample_week_no = format(StartDateLocal, "%Y-%W")) %>%
  mutate(sample_week = as.Date(cut(as.Date(StartDateLocal), "week")))

lur_data <- mutate(lur_data, site_id_lonlat = paste(lon, lat, sep = "_"))
ids <- read_csv(here::here("Data", "ST_Model_Site_IDs.csv"))

lur_data <- left_join(lur_data, ids, by = "site_id_lonlat") %>%
  mutate(site_id = ifelse(filter_id == "080310027", "central", site_id)) %>%
  arrange(sample_week)

lur_years <- seq(year(lur_data$sample_week[1]),
                 year(lur_data$sample_week[nrow(lur_data)]),
                 by = 1)
lur_years

lur_data2 <- filter(lur_data, campaign %in% c(paste0("Campaign", c(1, 2, 3, "X")))) %>% 
  mutate(year = year(StartDateLocal))

#' BC observations
#' With log transformation
bc_obs <- lur_data2 %>%
  select(site_id, sample_week, bc_ug_m3) %>%
  mutate(log_bc = log(bc_ug_m3)) %>%
  select(-bc_ug_m3) %>%
  pivot_wider(id_cols = sample_week,
              names_from = site_id, values_from = log_bc) %>%
  # names_from = site_id, values_from = bc_ug_m3) %>%
  as.data.frame() %>%
  arrange(sample_week)
rownames(bc_obs) <- bc_obs$sample_week
bc_obs$sample_week <- NULL
bc_obs <- as.matrix(bc_obs)

bc_weeks <- rownames(bc_obs)
head(rownames(bc_obs))
head(colnames(bc_obs))
class(bc_obs)
dim(bc_obs)

#' Which spatial predictors are we using?
load(here::here("Results", "BC_LASSO_Model_3C.Rdata"))
lasso_coef_df <- data.frame(name = log_bc_lasso_coef3@Dimnames[[1]][log_bc_lasso_coef3@i + 1],
                            coefficient = log_bc_lasso_coef3@x)
covars <- as.character(lasso_coef_df$name[-c(1:2)])
covars 

covar_fun <- paste("~", paste(covars, collapse = " + "))
covar_fun

#' Spatial predictors
bc_sp_cov <- select(lur_data2, site_id, lon, lat, all_of(covars)) %>%
  distinct() %>%
mutate_at(.vars = vars(covars),
          scale)

bc_sp_cov <- bc_sp_cov %>%
  rename(ID = site_id) %>%
  mutate(lon2 = lon, lat2 = lat) %>%
  st_as_sf(coords = c("lon2", "lat2"), crs = ll_wgs84) %>%
  st_transform(crs = albers)
sp_coords <- do.call(rbind, st_geometry(bc_sp_cov)) %>%
  as_tibble() %>% setNames(c("x","y"))
bc_sp_cov <- bind_cols(bc_sp_cov, sp_coords) %>%
  st_set_geometry(NULL) %>%
  as.data.frame()

bc_sp_cov$type <- ifelse(bc_sp_cov$ID == "central", "central", "dist")
bc_sp_cov$type <- as.factor(bc_sp_cov$type)
# bc_sp_cov$type

head(bc_sp_cov)
class(bc_sp_cov)
cor(bc_sp_cov[,c(4:16)])

#' Spatiotemporal predictors
st_covariates_file_name <- "ST_Covariates_Filters_LongTerm_AEA.csv"
st_covariates <- read_csv(here::here("Data", st_covariates_file_name)) %>% 
  mutate(filter_id = as.character(filter_id)) %>% 
  rename("site_id_lonlat" = "site_id",
         "sample_week" = "week") %>% 
  filter(campaign %in% paste0("Campaign", c(1, 2, 3, "X")))

st_covariates <- st_covariates %>% 
  mutate(site_id_lonlat = ifelse(site_id_lonlat == "-104.9440188_39.9372463999999",
                                 "-104.9440188_39.9372464", site_id_lonlat)) %>% 
  mutate(site_id_lonlat = ifelse(site_id_lonlat == "-104.8374204_39.5970357000001",
                                 "-104.8374204_39.5970357", site_id_lonlat))

st_covariates <- st_covariates %>% 
  filter(site_id_lonlat %in% unique(lur_data2$site_id_lonlat)) %>% 
  left_join(ids, by = "site_id_lonlat") %>% 
  mutate(site_id = ifelse(filter_id == "080310027", "central", site_id)) %>% 
  # mutate(site_id = ifelse(site_id == "central", site_id, paste0("d_", site_id))) %>% 
  select(site_id, sample_week, nn_pm:units_smoke) %>%
  distinct()

bc_st_no2 <- select(st_covariates, site_id, sample_week, idw_no2) %>%
  pivot_wider(id_cols = sample_week,
              names_from = site_id, values_from = idw_no2) %>%
  as.data.frame()
rownames(bc_st_no2) <- bc_st_no2$sample_week
bc_st_no2$sample_week <- NULL
bc_st_no2 <- as.matrix(bc_st_no2)

bc_st_smk <- select(st_covariates, site_id, sample_week, area_smoke_2sd) %>%
  pivot_wider(id_cols = sample_week,
              names_from = site_id, values_from = area_smoke_2sd) %>%
  as.data.frame()
rownames(bc_st_smk) <- bc_st_smk$sample_week
bc_st_smk$sample_week <- NULL
bc_st_smk <- as.matrix(bc_st_smk)

#' Make sure bc_obs and the st covariates have the same time span
#' add NA values to the missing bc_obs dates
bc_obs <- bc_obs[which(rownames(bc_obs) %in% rownames(bc_st_no2)),]

#' missing dates?
missing_dates <- setdiff(rownames(bc_st_no2), rownames(bc_obs))

bc_obs_missing <- matrix(data = NA, 
                         ncol = ncol(bc_obs),
                         nrow = length(missing_dates))
colnames(bc_obs_missing) <- colnames(bc_obs)
rownames(bc_obs_missing) <- missing_dates

bc_obs <- rbind(bc_obs_missing, bc_obs)

#' -----------------------------------------------------------------------------
#' Create the new data object and the model object
#' -----------------------------------------------------------------------------

full.data <- createSTdata(obs = bc_obs,
                          covars = bc_sp_cov,
                          SpatioTemporal = list(bc_st_no2 = bc_st_no2,
                                                bc_st_smk = bc_st_smk))
full.data
D <- createDataMatrix(full.data)

#' -----------------------------------------------------------------------------
#' Read in the model object, update trend data
#' -----------------------------------------------------------------------------

load(here::here("Results", "Denver_ST_Model_4.2.rdata"))
denver.model.4.2

full.data$trend <- denver.data4.2$trend
full.data

covars <- names(full.data$covars)[4:16]

covar_fun <- paste("~", paste(covars, collapse = " + "))
covar_fun

LUR <- list(covar_fun, covar_fun, covar_fun)

cov.beta <-  list(covf = c("iid", "iid", "iid"), nugget = T)
cov.nu <- list(covf = "exp", nugget = T, random.effect = FALSE)
locations <- list(coords = c("x","y"), long.lat = c("lon","lat"), others= "type")

full.model <- createSTmodel(full.data, LUR = LUR,
                            ST = c("bc_st_no2", "bc_st_smk"),
                            cov.beta = cov.beta, cov.nu = cov.nu,
                            locations = locations)
full.model

x <- coef(est.denver.model.4.2, pars = "cov")$par 
x

#' Compute the predictions 
#' E.1 computes predictions for all of the locations (observed and unobserved)
#' Getting the long-term average and the unbiased tranformation back to the 
#' original units
E.1 <- predict(full.model, x, LTA = T, transform="unbiased", pred.var=FALSE)
print(E.1)

save(E.1, file = here::here("Results", "Dist_LTA_Preds_09-19.rdata"))

#' -----------------------------------------------------------------------------
#' Check out the predicted values
#' -----------------------------------------------------------------------------

load(here::here("Results", "Dist_LTA_Preds_09-19.rdata"))

#' Long term averages for 2018
lta_preds <- as.data.frame(E.1$EX) %>% 
  mutate(week = as.Date(rownames(E.1$EX)))

lta_weeks <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day") 
lta_sites <- colnames(lta_preds)[which(str_detect(colnames(lta_preds), "g_"))]

lta_preds_avg <- lta_preds %>% 
  filter(week %in% lta_weeks) %>% 
  summarize_at(vars(all_of(lta_sites)), list(mean = mean))
dim(lta_preds_avg)

grid_lta_preds <- lta_preds_avg %>% 
  pivot_longer(cols = starts_with("g_"), names_to = "grid_id", values_to = "EX") %>% 
  mutate(grid_id = gsub("_mean", "", grid_id))
head(grid_lta_preds)

summary(grid_lta_preds$EX)

#' -----------------------------------------------------------------------------
#' Plot LTA for grid locations
#' -----------------------------------------------------------------------------

grid_sf <- read_csv(here::here("Data", "Spatial_Covariates_Grid_250_m_AEA.csv")) %>% 
  mutate(grid_id = paste0("g_", grid_id))

grid_sf_lta <- left_join(grid_sf, grid_lta_preds, by = "grid_id") %>% 
  st_as_sf(wkt = "WKT", crs = albers)

highways <- read_csv(here::here("Data", "Highways_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
highways_crop <- st_crop(highways, st_bbox(grid_sf_lta))

ggplot() +
  geom_sf(data = grid_sf_lta, aes(fill = EX), color = NA) +
  geom_sf(data = highways_crop, color = "white", size = 1) +
  scale_fill_viridis(name = "2018 mean\n BC (\u03BCg/m\u00B3)",
                     option = "magma", 
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
                                                     text_size = 14)) +
  xlab("") + ylab("") +
  map_theme +
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  theme(legend.position = c(0.81, 0.8),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text= element_text(size = 12, face = "bold")) +
  theme(panel.border = element_blank())
ggsave(here::here("Figs", "BC_LTA_2018_v1.jpeg"),
       height = 7, width = 6, units = "in", device = "jpeg", dpi = 750)

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
  theme(legend.position = c(0.81, 0.8),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text= element_text(size = 12, face = "bold")) +
  theme(panel.border = element_blank())
ggsave(here::here("Figs", "BC_LTA_2018_v2.jpeg"),
       height = 7, width = 6, units = "in", device = "jpeg", dpi = 750)


#' -----------------------------------------------------------------------------
#' Boxplot of weekly predicted BC at each of the sampling locations 
#' grouped by month
#' -----------------------------------------------------------------------------

denver <- st_read(here::here("Data", "Colorado_County_Boundaries.shp")) %>% 
  filter(CNTY_FIPS == "031") %>% 
  st_transform(crs = albers)
plot(st_geometry(denver))

library(lubridate)

#' Which site IDs are residential locations?
dist_sites <- select(lur_data, site_id, WKT) %>% 
  filter(site_id != "central") %>% 
  distinct() %>% 
  st_as_sf(wkt = "WKT", crs = albers)

part_sites <- select(lur_data, participant, needs_geocoding, site_id, WKT) %>% 
  filter(!is.na(needs_geocoding)) %>% 
  filter(needs_geocoding == 1 | participant == 1) %>% 
  distinct() %>% 
  st_as_sf(wkt = "WKT", crs = albers)
not_part <- c("d_1", "d_3")

res_sites <- filter(part_sites, !(site_id %in% not_part))
den_res_sites <- res_sites[denver,]
den_sites <- dist_sites[denver,]

#' Weekly predictions for 2018 (all distributed sites, n = 60)
week_preds <- as.data.frame(E.1$EX) %>% 
  mutate(week = as.Date(rownames(E.1$EX)))

week_weeks <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day") 
week_sites <- colnames(week_preds)[which(str_detect(colnames(week_preds), "d_"))]

box_preds <- week_preds %>% 
  select(week, all_of(week_sites)) %>% 
  filter(week %in% week_weeks) %>% 
  mutate(month = month(week))

box_data <- box_preds %>% 
  pivot_longer(-c(week, month), names_to = "location", values_to = "pred")
summary(box_data)

#' Weekly predictions for 2018 (all residential sites; n = 31)
box_preds2 <- week_preds %>% 
  select(week, all_of(res_sites$site_id)) %>% 
  filter(week %in% week_weeks) %>% 
  mutate(month = month(week))

box_data2 <- box_preds2 %>% 
  pivot_longer(-c(week, month), names_to = "location", values_to = "pred")
summary(box_data2)

#' Weekly predictions for 2018 (all distirbuted sites in Denver; n = 22)
box_preds3 <- week_preds %>% 
  select(week, all_of(den_sites$site_id)) %>% 
  filter(week %in% week_weeks) %>% 
  mutate(month = month(week))

box_data3 <- box_preds3 %>% 
  pivot_longer(-c(week, month), names_to = "location", values_to = "pred")
summary(box_data3)

#' Weekly predictions for 2018 (all residential sites in Denver; n = 10)
box_preds4 <- week_preds %>% 
  select(week, all_of(den_res_sites$site_id)) %>% 
  filter(week %in% week_weeks) %>% 
  mutate(month = month(week))

box_data4 <- box_preds4 %>% 
  pivot_longer(-c(week, month), names_to = "location", values_to = "pred")
summary(box_data4)

#' Box plot of weekly BC predicted at all distributed sites grouped by month
box_summary <- box_data %>% 
  group_by(month) %>% 
  summarize(median = median(pred)) %>% 
  arrange(desc(median))
box_summary  

month.name

box_plot <- ggplot(box_data) +
  geom_boxplot(aes(x = as.factor(month), y = pred, color = as.factor(month))) +
  scale_color_viridis(name = "Month", discrete = T) +
  xlab("Month") + ylab("Distributed site BC concentration (\u03BCg/m\u00B3): Predicted") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_x_discrete(labels = month.name) +
  simple_theme
box_plot
ggsave(filename = here::here("Figs", "Pred_BC_At_Dist_Sites.jpeg"),
       height = 5, width = 8, units = "in", device = "jpeg", dpi = 500)


#' -----------------------------------------------------------------------------
#' Plot LTA for grid locations within Denver county only
#' -----------------------------------------------------------------------------

library(smoothr)
denver <- st_read(here::here("Data", "Colorado_County_Boundaries.shp")) %>% 
  filter(CNTY_FIPS == "031") %>% 
  st_transform(crs = albers)
plot(st_geometry(denver))

denver2 <- fill_holes(denver, 10000000)
plot(st_geometry(denver2))

grid_sf <- read_csv(here::here("Data", "Spatial_Covariates_Grid_250_m_AEA.csv")) %>% 
  mutate(grid_id = paste0("g_", grid_id))

grid_sf_lta <- left_join(grid_sf, grid_lta_preds, by = "grid_id") %>% 
  st_as_sf(wkt = "WKT", crs = albers)

plot(st_geometry(grid_sf_lta))

grid_sf_lta_crop <- grid_sf_lta[denver2,]

plot(st_geometry(grid_sf_lta_crop))
plot(st_geometry(denver2), add = T)

highways <- read_csv(here::here("Data", "Highways_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
highways_crop <- highways[denver2,]

ggplot() +
  geom_sf(data = grid_sf_lta_crop, aes(fill = EX), color = NA) +
  geom_sf(data = highways_crop, color = "white", size = 1) +
  scale_fill_viridis(name = "2018 mean BC (\u03BCg/m\u00B3)",
                     option = "magma", 
                     breaks = waiver(), n.breaks = 6) +
  annotation_scale(data = grid_sf_lta_crop, plot_unit = "m", 
                   location = "br", width_hint = 0.5,
                   pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
                   text_cex = 1.5,
                   line_col = "black", text_col = "black", text_face = "bold") +
  annotation_north_arrow(data = grid_sf_lta_crop, 
                         location = "br", which_north = "grid", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(1.6, "cm"),
                         style = north_arrow_minimal(line_col = "black", 
                                                     fill = "black", 
                                                     line_width = 1.5,
                                                     text_col = "black",
                                                     text_face = "bold",
                                                     text_size = 14)) +
  xlab("") + ylab("") +
  map_theme +
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  theme(legend.position = c(0.8, 0.35),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text= element_text(size = 16, face = "bold")) +
  theme(panel.border = element_blank())
ggsave(here::here("Figs", "BC_LTA_Denver_2018_v1.jpeg"),
       height = 6, width = 8, units = "in", device = "jpeg", dpi = 750)

ggplot() +
  geom_sf(data = grid_sf_lta_crop, aes(fill = EX), color = NA) +
  geom_sf(data = highways_crop, color = "white", size = 1) +
  scale_fill_viridis(name = "2018 mean BC (\u03BCg/m\u00B3)",
                     breaks = waiver(), n.breaks = 6) +
  annotation_scale(data = grid_sf_lta_crop, plot_unit = "m", 
                   location = "br", width_hint = 0.5,
                   pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
                   text_cex = 1.5,
                   line_col = "black", text_col = "black", text_face = "bold") +
  annotation_north_arrow(data = grid_sf_lta_crop, 
                         location = "br", which_north = "grid", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_minimal(line_col = "black", 
                                                     fill = "black", 
                                                     line_width = 1.6,
                                                     text_col = "black",
                                                     text_face = "bold",
                                                     text_size = 12)) +
  xlab("") + ylab("") +
  map_theme +
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  theme(legend.position = c(0.75, 0.3),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text= element_text(size = 16, face = "bold")) +
  theme(panel.border = element_blank())
ggsave(here::here("Figs", "BC_LTA_Denver_2018_v2.jpeg"),
       height = 6, width = 8, units = "in", device = "jpeg", dpi = 750)


#' -----------------------------------------------------------------------------
#' Downtown Denver with an inset map showing the bounding box
#' -----------------------------------------------------------------------------

register_google(google_api_key)

denver <- st_read(here::here("Data", "Colorado_County_Boundaries.shp")) %>% 
  filter(CNTY_FIPS == "031") %>% 
  st_transform(crs = ll_wgs84)
plot(st_geometry(denver))

grid_pts <- data.frame(lon = c(-105.032454, -105.032454, -104.940688, -104.940688),
                       lat = c(39.70, 39.80, 39.70, 39.80)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84) 
grid_box <- st_convex_hull(st_union(grid_pts))
plot(st_geometry(grid_pts))
plot(st_geometry(grid_box), add = T)

grid_bounds <- data.frame(lon = c(-105.035, -105.035, -104.85, -104.85),
                          lat = c(39.70, 39.80, 39.70, 39.80)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84) %>% 
  st_transform(crs = albers)

grid_box2 <- st_transform(grid_box, crs = albers)

highways2 <- read_csv(here::here("Data", "Highways_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  st_transform(crs = ll_wgs84)
highways2_crop <- highways2[grid_box,]

grid_sf_lta2 <- grid_st_lta

#' Base map of the area
base_map <- get_map(location = "Denver, CO", zoom = 11, maptype = "roadmap")
ggmap(base_map)
attr(base_map, "bb")

g1 <- ggmap(base_map) +
  geom_sf(data = grid_box, fill = NA, col = "red", size = 3, inherit.aes = F) +
  xlab("") + ylab("") +
  ylim(c(39.65, 39.85)) + xlim(c(-105.15, -104.80)) +
  map_theme2
g1

g2 <- ggplot() +
  geom_sf(data = grid_bounds, color = "transparent") +
  geom_sf(data = grid_sf_lta[grid_box2,], aes(fill = EX), color = NA) +
  geom_sf(data = highways2_crop, color = "white", size = 2) +
  scale_fill_viridis(name = "2018 mean\n BC (\u03BCg/m\u00B3)",
                     breaks = waiver(), n.breaks = 6) +
  annotation_scale(data = grid_sf_lta[grid_box2,], plot_unit = "m", 
                   location = "bl", width_hint = 0.5,
                   pad_x = unit(1.25, "cm"), 
                   pad_y = unit(1.1, "cm"),
                   text_cex = 1, bar_cols = c("grey90", "black"),
                   line_col = "black", text_col = "black", text_face = "bold") +
  annotation_north_arrow(data = grid_sf_lta[grid_box2,], 
                         location = "bl", which_north = "grid", 
                         pad_x = unit(1.5, "cm"), 
                         pad_y = unit(.75, "cm"),
                         style = north_arrow_minimal(line_col = "black", 
                                                     fill = "black", 
                                                     line_width = 1.5,
                                                     text_col = "black",
                                                     text_face = "bold",
                                                     text_size = 12)) +
  xlab("") + ylab("") +
  map_theme2 +
  # theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
  theme(legend.position = c(0.65, 0.65),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text= element_text(size = 12, face = "bold")) +
  theme(panel.border = element_blank())
g2
