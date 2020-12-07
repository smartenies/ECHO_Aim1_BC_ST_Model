#' =============================================================================
#' Project: ECHO LUR
#' Date created: August 12, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' Predicting BC for the entire 250 m grid
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
options(scipen = 9999) #avoid scientific notation

#' Google API for ggmap
register_google(key = google_api_key)

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

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
#' Subset to just predictors needed
#' -----------------------------------------------------------------------------

grid_data <- read_csv(here::here("Data/Final_Data", "Combined_Grid_Data_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  select(month_yr, lur_vars)
glimpse(grid_data)

#' -----------------------------------------------------------------------------
#' Predict BC for each grid 
#' ----------------------------------------------------------------------------- 

grid_data$bc_ug_m3_pred <- predict(bc_final_lm, newdata = grid_data)
summary(grid_data$bc_ug_m3_pred)

#' -----------------------------------------------------------------------------
#' Save Dataset
#' -----------------------------------------------------------------------------

st_write(grid_data, here::here("Results", "BC_LUR_Grid_Results.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' -----------------------------------------------------------------------------
#' Summarize monthly concentrations for 2018 and 2019
#' -----------------------------------------------------------------------------

grid_data <- read_csv(here::here("Results", "BC_LUR_Grid_Results.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

grid_summary <- filter(grid_data) %>% 
  st_set_geometry(NULL) %>%
  filter(!is.na(bc_ug_m3_pred)) %>% 
  group_by(month_yr) %>% 
  summarize(mean_sd = paste0(round(mean(bc_ug_m3_pred, na.rm=T), 2),
                             " (", 
                             round(sd(bc_ug_m3_pred, na.rm=T), 2), ")"),
            min = round(min(bc_ug_m3_pred, na.rm=T), 2),
            median = round(median(bc_ug_m3_pred, na.rm=T), 2),
            max = round(max(bc_ug_m3_pred, na.rm=T), 2)) %>% 
  rowwise() %>% 
  mutate(month = as.numeric(str_split(month_yr, "_")[[1]][[1]]),
         year = as.numeric(str_split(month_yr, "_")[[1]][[2]])) %>%
  arrange(year, month) %>% 
  select(year, month, mean_sd:max)
grid_summary  

write_csv(grid_summary, here::here("Results", "Grid_Summary_Stats.csv"))

#' -----------------------------------------------------------------------------
#' Map BC concentrations for each month of the sampling campaigns
#' ----------------------------------------------------------------------------- 

month_list <- unique(grid_data$month_yr)
month_list

#' Dictionary for dates
month_dict <- list(
  "1_2018" = "Jan, 2018",
  "2_2018" = "Feb, 2018",
  "3_2018" = "Mar, 2018",
  "4_2018" = "Apr, 2018",
  "5_2018" = "May, 2018",
  "6_2018" = "Jun, 2018",
  "7_2018" = "Jul, 2018",
  "8_2018" = "Aug, 2018",
  "9_2018" = "Sep, 2018",
  "10_2018" = "Oct, 2018",
  "11_2018" = "Nov, 2018",
  "12_2018" = "Dec, 2018",
  "1_2019" = "Jan, 2019",
  "2_2019" = "Feb, 2019",
  "3_2019" = "Mar, 2019",
  "4_2019" = "Apr, 2019"
)

month_list <- names(month_dict)
month_list

for (i in 1:length(month_list)) {
  df <- filter(grid_data, month_yr == month_list[i])
  
  st_bbox(df)
  x_min <- unname(st_bbox(df)["xmin"])
  x_max <- unname(st_bbox(df)["xmax"]) 
  y_min <- unname(st_bbox(df)["ymin"])
  y_max <- unname(st_bbox(df)["ymax"])
  
  ggplot(df) +
    geom_sf(aes(fill = bc_ug_m3_pred), color = NA) +
    scale_fill_viridis(name = paste("BC (\u03BCg/m\u00B3):", 
                                    month_dict[[month_list[i]]],
                                    sep = "\n"),
                       option = "plasma") +
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
    map_theme
  
  plot_name <- paste0("BC_", month_list[i], ".jpeg")
  ggsave(here::here("Figs", plot_name),
         device = "jpeg", height= 5, width = 6, dpi = 500, units = "in")
}

#' -----------------------------------------------------------------------------
#' Map of four representative months for the markdown
#' ----------------------------------------------------------------------------- 

grid_data <- read_csv(here::here("Results", "BC_LUR_Grid_Results.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

st_bbox(grid_data)
x_min <- unname(st_bbox(grid_data)["xmin"])
x_max <- unname(st_bbox(grid_data)["xmax"]) 
y_min <- unname(st_bbox(grid_data)["ymin"])
y_max <- unname(st_bbox(grid_data)["ymax"])

jan <- ggplot(filter(grid_data, month_yr == "1_2018")) +
  geom_sf(aes(fill = bc_ug_m3_pred), color = NA) +
  scale_fill_viridis(name = paste("BC (\u03BCg/m\u00B3):", 
                                  "Jan, 2018",
                                  sep = "\n")) +
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
  map_theme
jan

apr <- ggplot(filter(grid_data, month_yr == "4_2018")) +
  geom_sf(aes(fill = bc_ug_m3_pred), color = NA) +
  scale_fill_viridis(name = paste("BC (\u03BCg/m\u00B3):", 
                                  "Apr, 2018",
                                  sep = "\n")) +
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
  map_theme
apr

jul <- ggplot(filter(grid_data, month_yr == "7_2018")) +
  geom_sf(aes(fill = bc_ug_m3_pred), color = NA) +
  scale_fill_viridis(name = paste("BC (\u03BCg/m\u00B3):", 
                                  "Jul, 2018",
                                  sep = "\n")) +
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
  map_theme
jul

oct <- ggplot(filter(grid_data, month_yr == "10_2018")) +
  geom_sf(aes(fill = bc_ug_m3_pred), color = NA) +
  scale_fill_viridis(name = paste("BC (\u03BCg/m\u00B3):", 
                                  "Oct, 2018",
                                  sep = "\n"),
                     option = "plasma") +
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
  map_theme

bc_plots <- ggarrange(jan, apr, jul, oct,
                      labels = c("A", "B", "C", "D"),
                      ncol = 2, nrow = 2)
bc_plots
ggsave(here::here("Figs/ISEE_Poster", "BC_Month_Comparison.jpeg"),
       device = "jpeg", height= 10, width = 10, dpi = 1000, units = "in")
