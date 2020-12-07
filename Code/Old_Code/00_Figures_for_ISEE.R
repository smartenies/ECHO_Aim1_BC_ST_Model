#' =============================================================================
#' Project: ECHO LUR
#' Date created: August 12, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' Some figures for LUR predictors
#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggpubr)
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

poster_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 20, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=16),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)

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

map_theme2 <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Arial",size = 24, color = 'black'),
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
windowsFonts(Arial=windowsFont("TT Arial"))
options(scipen = 9999) #avoid scientific notation

#' Google API for ggmap
register_google(key = google_api_key)

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' Viridis colors for the maps (5 levels - the highest to avoid yellow)
colors <- c("#440154FF", "#31688EFF", "#35B779FF", "#B8DE29FF")

#' -----------------------------------------------------------------------------
#' Read in the LUR data and final model
#' -----------------------------------------------------------------------------

#' Final LUR filter dataset
bc_lur_data <- read_csv(here::here("Data/Final_Data", "BC_LUR_Final_Data.csv"))
lur_vars <- names(bc_lur_data)[-1]
lur_vars 

#' Final LUR lm
load(here::here("Results", "BC_LUR_Final_Model.rdata"))
summary(bc_final_lm)

#' -----------------------------------------------------------------------------
#' Read in the grid dataset-- 
#' Subset to just predictors needed: pop_den, temp, AADT, impervious surface
#' -----------------------------------------------------------------------------

grid_data <- read_csv(here::here("Data/Final_Data", "Combined_Grid_Data_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  select(month_yr, lur_vars) %>%
  filter(month_yr == "8_2018")
names(grid_data)
glimpse(grid_data)

st_bbox(grid_data)
x_min <- unname(st_bbox(grid_data)["xmin"])
x_max <- unname(st_bbox(grid_data)["xmax"]) 
y_min <- unname(st_bbox(grid_data)["ymin"])
y_max <- unname(st_bbox(grid_data)["ymax"])

aadt <- ggplot(filter(grid_data)) +
  geom_sf(aes(fill = aadt_50/1000), color = NA) +
  scale_fill_viridis(name = "(1000's)") +
  north(x.min = x_min, x.max = x_max,
        y.min =  y_min, y.max = y_max,
        symbol = 4, location = "topleft", scale = 0.05,
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
  map_theme + theme(legend.title = element_blank())

temp <- ggplot(filter(grid_data)) +
  geom_sf(aes(fill = nearest_neighbor_temp), color = NA) +
  scale_fill_viridis(name = "Temp") +
  north(x.min = x_min, x.max = x_max,
        y.min =  y_min, y.max = y_max,
        symbol = 4, location = "topleft", scale = 0.05,
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
  map_theme + theme(legend.title = element_blank())

pop <- ggplot(filter(grid_data)) +
  geom_sf(aes(fill = pop_den_2500/1000), color = NA) +
  scale_fill_viridis(name = "1000's") +
  north(x.min = x_min, x.max = x_max,
        y.min =  y_min, y.max = y_max,
        symbol = 4, location = "topleft", scale = 0.05,
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
  map_theme + theme(legend.title = element_blank())

imp <- ggplot(filter(grid_data)) +
  geom_sf(aes(fill = impervious_1000), color = NA) +
  scale_fill_viridis(name = "Imperv.\nsurface") +
  north(x.min = x_min, x.max = x_max,
        y.min =  y_min, y.max = y_max,
        symbol = 4, location = "topleft", scale = 0.05,
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
  map_theme + theme(legend.title = element_blank())

pred_plots <- ggarrange(aadt, temp, pop, imp,
                        labels = c("A) AADT (1000 vehicles/day)", 
                                   "B) Temperature (\u00B0F)", 
                                   "C) Population density (1000's/sq. km)", 
                                   "D) Impervious surface (%)"),
                        ncol = 2, nrow = 2,
                        hjust = -0.155, vjust = 1.2)
pred_plots
ggsave(here::here("Figs/ISEE_Poster", "Predictors.jpeg"),
       device = "jpeg", height= 10, width = 10, dpi = 1000, units = "in")


#' -----------------------------------------------------------------------------
#' Map of predicted BC concentrations in August, 2018
#' -----------------------------------------------------------------------------

grid_data2 <- read_csv(here::here("Results", "BC_LUR_Grid_Results.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

df <- filter(grid_data2, month_yr == "8_2018")

st_bbox(df)
x_min <- unname(st_bbox(df)["xmin"])
x_max <- unname(st_bbox(df)["xmax"]) 
y_min <- unname(st_bbox(df)["ymin"])
y_max <- unname(st_bbox(df)["ymax"])

ggplot(data = df) +
  geom_sf(aes(fill = bc_ug_m3_pred), color = NA) +
  scale_fill_viridis(name = paste("BC (\u03BCg/m\u00B3)")) +
  north(x.min = x_min, x.max = x_max,
        y.min =  y_min, y.max = y_max,
        symbol = 4, location = "topleft", scale = 0.05,
        anchor = c(x = x_min - x_min*0.005,
                   y = y_max - y_max*0.001)) +
  scalebar(x.min = x_min, x.max = x_max,
           y.min =  y_min, y.max = y_max,
           dist_unit = "km", dist = 5, transform = F,
           st.bottom = F, st.size = 6, st.color = "white",
           box.color = "white", height = 0.01,
           location = "bottomleft",
           anchor = c(x = x_min - x_min*0.005,
                      y = y_min + y_min*0.001)) +
  xlab("") + ylab("") +
  map_theme2

plot_name <- paste0("BC_August_2018.jpeg")
ggsave(here::here("Figs/ISEE_Poster", plot_name),
       device = "jpeg", height= 15, width = 15, dpi = 1000, units = "in")


#' -----------------------------------------------------------------------------
#' Boxplot of monthly BC by distance to highway
#' -----------------------------------------------------------------------------

bc_all <- read_csv(here::here("Data/Final_Data", "Data_for_LUR.csv")) %>% 
  select(lon, lat, month, bc_ug_m3, dist_m_highways)

distance_q1 <- quantile(bc_all$dist_m_highways, probs = 0.25, na.rm = T)
distance_q2 <- quantile(bc_all$dist_m_highways, probs = 0.50, na.rm = T)
distance_q3 <- quantile(bc_all$dist_m_highways, probs = 0.75, na.rm = T)

distance_q3
distance_q2
distance_q1

bc_all <- bc_all %>% 
  mutate(distance_cat = ifelse(dist_m_highways <= distance_q1, 4,
                               ifelse(dist_m_highways > distance_q1 & 
                                        dist_m_highways <= distance_q2, 3, 
                                      ifelse(dist_m_highways > distance_q2 & 
                                               dist_m_highways <= distance_q3, 2, 1))),
         season = ifelse(month %in% c(12, 1, 2), "winter", 
                         ifelse(month %in% c(3, 4, 5), "spring",
                                ifelse(month %in% c(6, 7, 8), "summer", "fall")))) %>% 
  mutate(season_fac = factor(season, levels = c("spring", "summer", "fall", "winter")))

ggplot(bc_all) +
  geom_boxplot(aes(x = as.factor(season), y = bc_ug_m3, 
                   col = as.factor(distance_cat))) +
  scale_color_manual(name = "Distance (m)\nto highways",
                     labels = c("1530 +", "759 - < 1530", "281 - < 759", "< 281"),
                     values = colors) +
  scale_x_discrete(limits = c("spring", "summer", "fall", "winter"),
                   labels = c("Spring", "Summer", "Fall", "Winter")) +
  ylab("Mean BC (\u03BCg/m\u00B3)") + xlab("Season") +
  poster_theme
ggsave(here::here("Figs/ISEE_Poster", "Monthly_Means_by_Season_and_Dist.jpeg"),
       device = "jpeg", height= 7, width = 9, dpi = 1000, units = "in")
