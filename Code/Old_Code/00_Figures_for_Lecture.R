#' =============================================================================
#' Project: ECHO LUR
#' Date created: March 2, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script cleans the PM2.5 gravimetric data for each UPAS sampling campaign
#' and calculates the TWA for each filter
#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggthemes)
library(ggpubr)
library(ggspatial)
library(viridis)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)

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
  # legend.key = element_blank(),
  legend.key = element_rect(colour = "transparent", fill = "white")
)

map_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)

windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' Google API for ggmap
register_google(key = google_api_key)

#' -----------------------------------------------------------------------------
#' Some plots of important land use characteristics
#' -----------------------------------------------------------------------------

#' Highways
highways <- read_csv(here::here("Data", "Highways_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

jpeg(here::here("Figs/Lecture", "highways.jpeg"))
plot(st_geometry(highways), col = "red")
dev.off()

#' Oil and gas wells
wells <- read_csv(here::here("Data", "OG_Wells_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

jpeg(here::here("Figs/Lecture", "wells.jpeg"))
plot(st_geometry(wells), col = "blue") 
dev.off()

#' Parks
parks <- read_csv(here::here("Data", "Parks_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

jpeg(here::here("Figs/Lecture", "parks.jpeg"))
plot(st_geometry(highways), col = "grey80")
plot(st_geometry(parks), col = "darkgreen", add=T) 
dev.off()

#' -----------------------------------------------------------------------------
#' Boxplots showing grid vs. sampled sites
#' -----------------------------------------------------------------------------

data_name <- "Final_Filter_Dataset_AEA.csv"
filter_data <- read_csv(here::here("Final_Data", data_name)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  filter(indoor == 0)

grid_data_name <- "Final_Grid_Dataset_AEA.csv"
grid_data <- read_csv(here::here("Final_Data", grid_data_name)) %>% 
  st_as_sf(wkt = "WKT", crs = albers)

var1_filter <- data.frame(a = "Sample\nLocations", b = filter_data$elevation_2500) 
var1_grid <- data.frame(a = "Prediction\nGrid", b = grid_data$elevation_2500) 

var1_data <- rbind(var1_filter, var1_grid)

ggplot(var1_data) +
  geom_boxplot(aes(x = a, y = b, fill = a), show.legend = F) +
  coord_flip() +
  ylab("Mean elevation (ft) in 2500 m buffer") + xlab("") +
  simple_theme
ggsave(filename = here::here("Figs/Lecture", "Elevation Comparison.jpeg"),
       device = "jpeg", height = 4, width = 5, units = "in")  


var2_filter <- data.frame(a = "Sample\nLocations", b = filter_data$aadt_250) 
var2_grid <- data.frame(a = "Prediction\nGrid", b = grid_data$aadt_250) 

var2_data <- rbind(var2_filter, var2_grid)

ggplot(var2_data) +
  geom_boxplot(aes(x = a, y = b, fill = a), show.legend = F) +
  coord_flip() +
  ylab("Mean AADT (vehicles/day) in 250 m buffer") + xlab("") +
  simple_theme
ggsave(filename = here::here("Figs/Lecture", "AADT Comparison.jpeg"),
       device = "jpeg", height = 4, width = 5, units = "in")  

#' -----------------------------------------------------------------------------
#' Sample Locations
#' -----------------------------------------------------------------------------

filter_map_pts <- select(filter_data, participant)

#' Map of campaign sampling locations (residences jittered)
#' Map in ggmap
base_map <- get_map(location = "Commerce City, CO", zoom = 10)
ggmap(base_map)
attr(base_map, "bb")

#ggmap(base_map) +
ggplot() +
  geom_sf(data = st_jitter(filter(filter_map_pts, participant == 1), 0.02), 
          aes(fill = "res", color = "res"), inherit.aes = F, show.legend = "point") +
  geom_sf(data = filter(filter_data, participant == 0),
          aes(fill = "com", color = "com"), inherit.aes = F, show.legend = "point") +
  geom_sf(data = highways, color = "grey50") +
  scale_color_manual(name = "Sampling\nLocation Type",
                     values = c("com" = "red", "res" = "blue"),
                     labels = c("com" = "Community", "res" = "Residence")) +
  scale_fill_manual(name = "Sampling\nLocation Type",
                    values = c("com" = "red", "res" = "blue"),
                    labels = c("com" = "Community", "res" = "Residence")) +
  # north(x.min = -105.3726, x.max = -104.4937,
  #       y.min =  39.5, y.max = 40.14455,
  #       symbol = 12, location = "bottomright", scale = 0.05) +
  # scalebar(x.min = -105.3726, x.max = -104.4937,
  #          y.min =  39.5, y.max = 40.14455,
  #          dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
  #          height = 0.01) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.15, 0.85)) +
  map_theme

fig_name <- "Campaign_Sampling_Sites.jpeg"
ggsave(filename = here::here("Figs/Lecture", fig_name), 
       device = "jpeg", dpi=500, units = "in", height = 6, width = 5)

#' -----------------------------------------------------------------------------
#' point buffer
#' -----------------------------------------------------------------------------

point <- filter_map_pts[1,]
point_100 <- st_buffer(point, dist = 100)
point_250 <- st_buffer(point, dist = 250)
point_500 <- st_buffer(point, dist = 500)
point_1000 <- st_buffer(point, dist = 1000)
point_2500 <- st_buffer(point, dist = 2500)

ggplot() +
  geom_sf(data = point, color = "red") +
  geom_sf(data = point_100, fill = NA, color = "black") +
  geom_sf(data = point_250, fill = NA, color = "black") +
  geom_sf(data = point_500, fill = NA, color = "black") +
  geom_sf(data = point_1000, fill = NA, color = "black") +
  geom_sf(data = point_2500, fill = NA, color = "black") +
  simple_theme
fig_name <- "Buffer example.jpeg"
ggsave(filename = here::here("Figs/Lecture", fig_name), 
       device = "jpeg", dpi=500, units = "in", height = 4, width = 4)

#' -----------------------------------------------------------------------------
#' time series of PM and Ozone in the southern front range
#' -----------------------------------------------------------------------------

#' County codes for Denver, Adams, Arapaho, El Paso, Pueblo, Teller, Fremont,
#' Douglas, Jefferson, Broomfield, Custer

counties <- c("014", "031", "001", "005", "059", "035", "119", "039", "041",
              "043", "027", "101")

pm <- read_csv(here::here("Data/Monitor_PM_Data_AEA.csv")) %>% 
  mutate(year = year(Date_Local)) %>% 
  filter(year %in% c(2011)) %>% 
  filter(County_Code %in% counties)
  
o3 <- read_csv(here::here("Data/Monitor_O3_Data_AEA_2.csv")) %>% 
  mutate(year = year(Date_Local)) %>% 
  filter(year %in% c(2011)) %>% 
  filter(County_Code %in% counties)

length(unique(pm$monitor_id))
length(unique(o3$monitor_id))

pm_mean <- pm %>% 
  group_by(Date_Local) %>% 
  summarize(mean = mean(Arithmetic_Mean))

o3_mean <- o3 %>% 
  group_by(Date_Local) %>% 
  summarize(mean = mean(daily_max))

pm_plot <- ggplot() +
  geom_point(data = pm, aes(x = Date_Local, y = Arithmetic_Mean, 
                            color = "monitors", size = "monitors")) +
  # geom_line(data = pm_mean, aes(x = Date_Local, y = mean, 
  #                                color = "mean", size = "mean")) +
  geom_smooth(data = pm, aes(x = Date_Local, y = Arithmetic_Mean,
                                color = "mean", size = "mean"),
              method = "loess") +
  # geom_smooth(data = pm, aes(x = Date_Local, y = Arithmetic_Mean,
  #                                 color = "mean", size = "mean"),
  #             method = "gam", formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(name = "",
                     values = c("monitors" = "gray",
                                "mean" = "black"),
                     labels = c("monitors" = "All monitors",
                                "mean" = "LOESS")) +
  scale_size_manual(name = "",
                    values = c("monitors" = 0.5,
                              "mean" = 1.5),
                    labels = c("monitors" = "All monitors",
                              "mean" = "LOESS")) +
  theme(legend.position = c(0.2, 0.8)) +
  xlab("Date") + ylab("Daily mean PM\u2082.\u2085 (\u03BCg/m\u00B3)") +
  simple_theme +
  guides(color=guide_legend(override.aes=list(fill = NA,
                                              color = c("black", "gray"),
                                              linetype = c(1, NA),
                                              shape = c(NA, 20),
                                              size = c(1.5, 1.5))))
pm_plot
  
o3_plot <- ggplot() +
  geom_point(data = o3, aes(x = Date_Local, y = daily_max*1000, 
                            color = "monitors", size = "monitors")) +
  geom_smooth(data = o3, aes(x = Date_Local, y = daily_max*1000,
                             color = "mean", size = "mean"),
              method = "loess") +
  scale_color_manual(name = "",
                     values = c("monitors" = "gray",
                                "mean" = "black"),
                     labels = c("monitors" = "All monitors",
                                "mean" = "LOESS")) +
  scale_size_manual(name = "",
                    values = c("monitors" = 0.5,
                               "mean" = 1.5),
                    labels = c("monitors" = "All monitors",
                               "mean" = "LOESS")) +
  theme(legend.position = c(0.2, 0.8)) +
  xlab("Date") + ylab("Max daily 8-hour mean O\u2083 (ppb)") +
  simple_theme +
  guides(color=guide_legend(override.aes=list(fill = NA,
                                              color = c("black", "gray"),
                                              linetype = c(1, NA),
                                              shape = c(NA, 20),
                                              size = c(1.5, 1.5))))
o3_plot

library(ggpubr)
ts_plots <- ggarrange(pm_plot, o3_plot, ncol = 2, nrow = 1)
ts_plots

ggsave(ts_plots,
       filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Monitor_TS_Plot.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 4, width = 8)


#' -----------------------------------------------------------------------------
#' time series of PM and Ozone in the southern front range and all of CO
#' -----------------------------------------------------------------------------

#' County codes for Denver, Adams, Arapaho, El Paso, Pueblo, Teller, Fremont,
#' Douglas, Jefferson, Broomfield, Custer

counties <- c("014", "031", "001", "005", "059", "035", "119", "039", "041",
              "043", "027", "101")

pm <- read_csv(here::here("Data/Monitor_PM_Data_AEA.csv")) %>% 
  mutate(year = year(Date_Local)) %>% 
  filter(year %in% c(2011)) %>% 
  filter(POC == 1)

pm_sub <- pm %>% 
  filter(County_Code %in% counties)

o3 <- read_csv(here::here("Data/Monitor_O3_Data_AEA.csv")) %>% 
  mutate(year = year(Date_Local)) %>% 
  filter(year %in% c(2011))  %>% 
  filter(POC == 1)

o3_sub <- o3 %>% 
  filter(County_Code %in% counties)

length(unique(pm$monitor_id))
length(unique(pm_sub$monitor_id))
length(unique(o3$monitor_id))
length(unique(o3_sub$monitor_id))

pm_mean <- pm %>% 
  group_by(Date_Local) %>% 
  summarize(mean = mean(Arithmetic_Mean))

pm_sub_mean <- pm_sub %>% 
  group_by(Date_Local) %>% 
  summarize(mean = mean(Arithmetic_Mean))

o3_mean <- o3 %>% 
  group_by(Date_Local) %>% 
  summarize(mean = mean(daily_8h_max))

o3_sub_mean <- o3_sub %>% 
  group_by(Date_Local) %>% 
  summarize(mean = mean(daily_8h_max))

pm_plot <- ggplot() +
  geom_point(data = pm, aes(x = Date_Local, y = Arithmetic_Mean, 
                            color = "monitors", size = "monitors")) +
  geom_smooth(data = pm, aes(x = Date_Local, y = Arithmetic_Mean,
                             color = "loess", size = "loess"),
              method = "loess") +
  geom_hline(aes(yintercept = mean(pm$Arithmetic_Mean), 
             color = "mean", size = "mean"), linetype = 2) +
  scale_color_manual(name = "",
                     values = c("monitors" = "gray",
                                "loess" = "black",
                                "mean" = "red"),
                     labels = c("monitors" = "Monitors",
                                "loess" = "LOESS",
                                "mean" = "Mean concentration")) +
  scale_size_manual(name = "",
                    values = c("monitors" = 0.5,
                               "loess" = 1.5,
                               "mean" = 1),
                    labels = c("monitors" = "Monitors",
                               "loess" = "LOESS",
                               "mean" = "Mean concentration")) +
  theme(legend.position = c(0.75, 0.75)) +
  xlab("Date") + ylab("Daily mean PM\u2082.\u2085 (\u03BCg/m\u00B3)") +
  simple_theme +
  guides(color=guide_legend(override.aes=list(fill = NA,
                                              color = c("black", "red", "gray"),
                                              linetype = c(1, 2, NA),
                                              shape = c(NA, NA, 20),
                                              size = c(1.5, 1, 1.5))))
pm_plot

pm_sub_plot <- ggplot() +
  geom_point(data = pm_sub, aes(x = Date_Local, y = Arithmetic_Mean, 
                            color = "monitors", size = "monitors"),
             show.legend = F) +
  geom_smooth(data = pm_sub, aes(x = Date_Local, y = Arithmetic_Mean,
                             color = "loess", size = "loess"),
              method = "loess", show.legend = F) +
  geom_hline(aes(yintercept = mean(pm_sub$Arithmetic_Mean), 
                 color = "mean", size = "mean"), linetype = 2, show.legend = F) +
  scale_color_manual(name = "",
                     values = c("monitors" = "gray",
                                "loess" = "black",
                                "mean" = "red"),
                     labels = c("monitors" = "Monitors",
                                "loess" = "LOESS",
                                "mean" = "Mean concentration")) +
  scale_size_manual(name = "",
                    values = c("monitors" = 0.5,
                               "loess" = 1.5,
                               "mean" = 1),
                    labels = c("monitors" = "Monitors",
                               "loess" = "LOESS",
                               "mean" = "Mean concentration")) +
  theme(legend.position = c(0.8, 0.9)) +
  xlab("Date") + ylab("Daily mean PM\u2082.\u2085 (\u03BCg/m\u00B3)") +
  simple_theme
pm_sub_plot

o3_plot <- ggplot() +
  geom_point(data = o3, aes(x = Date_Local, y = daily_8h_max*1000, 
                            color = "monitors", size = "monitors"),
             show.legend = F) +
  geom_smooth(data = o3, aes(x = Date_Local, y = daily_8h_max*1000,
                             color = "loess", size = "loess"),
              method = "loess", show.legend = F) +
  geom_hline(aes(yintercept = mean(o3$daily_8h_max)*1000, 
                 color = "mean", size = "mean"), linetype = 2, show.legend = F) +
  scale_color_manual(name = "",
                     values = c("monitors" = "gray",
                                "loess" = "black",
                                "mean" = "red"),
                     labels = c("monitors" = "Monitors",
                                "loess" = "LOESS",
                                "mean" = "Mean concentration")) +
  scale_size_manual(name = "",
                    values = c("monitors" = 0.5,
                               "loess" = 1.5,
                               "mean" = 1),
                    labels = c("monitors" = "Monitors",
                               "loess" = "LOESS",
                               "mean" = "Mean concentration")) +
  theme(legend.position = c(0.8, 0.9)) +
  xlab("Date") + ylab("Max daily 8-hour mean O\u2083 (ppb)") +
  simple_theme                                              
o3_plot

o3_sub_plot <- ggplot() +
  geom_point(data = o3_sub, aes(x = Date_Local, y = daily_8h_max*1000, 
                            color = "monitors", size = "monitors"),
             show.legend = F) +
  geom_smooth(data = o3_sub, aes(x = Date_Local, y = daily_8h_max*1000,
                             color = "loess", size = "loess"),
              method = "loess", show.legend = F) +
  geom_hline(aes(yintercept = mean(o3_sub$daily_8h_max)*1000, 
                 color = "mean", size = "mean"), linetype = 2, show.legend = F) +
  scale_color_manual(name = "",
                     values = c("monitors" = "gray",
                                "loess" = "black",
                                "mean" = "red"),
                     labels = c("monitors" = "Monitors",
                                "loess" = "LOESS",
                                "mean" = "Mean concentration")) +
  scale_size_manual(name = "",
                    values = c("monitors" = 0.5,
                               "loess" = 1.5,
                               "mean" = 1),
                    labels = c("monitors" = "Monitors",
                               "loess" = "LOESS",
                               "mean" = "Mean concentration")) +
  theme(legend.position = c(0.8, 0.9)) +
  xlab("Date") + ylab("Max daily 8-hour mean O\u2083 (ppb)") +
  simple_theme
o3_sub_plot

monitor_plots <- ggarrange(
  annotate_figure(ggarrange(pm_plot, o3_plot, 
                            labels = c("A: PM Monitors", 
                                       "B: Ozone Monitors"), 
                            ncol = 2, nrow = 1),
                  left = text_grob("All Monitors in Colorado", rot = 90, face = "bold")),
  annotate_figure(ggarrange(pm_sub_plot, o3_sub_plot, 
                            labels = c("C: PM Monitors", 
                                       "D: Ozone Monitors"), 
                            ncol = 2, nrow = 1),
                  left = text_grob("Southern Front Range Monitors", rot = 90, face = "bold")),
  ncol = 1, nrow = 2)
monitor_plots

ggsave(monitor_plots,
       filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Monitor_TS_Plot.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 8, width = 8)


