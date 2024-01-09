#' =============================================================================
#' Project: ECHO LUR
#' Date Created: March 13, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' Code to make figures for manuscripts, etc.
#' =============================================================================

library(sf)
library(ggplot2)
library(ggmap)
library(ggspatial)
library(ggsn)
library(ggpubr)
library(ggthemes)
library(extrafont)
library(viridis)
library(tidyverse)
library(lubridate)
library(readxl)
library(cowplot)

#' For ggplots
# loadfonts(device = "win")

simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 16, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "grey90"),
  panel.grid.major = element_line(color = "grey90"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=14),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)

simple_theme2 <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  # panel.grid.minor = element_line(color = "grey90"),
  panel.grid.minor.x = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "grey90"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(2,2,2,2), "mm"),
  legend.key = element_blank()
)

map_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 14, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border = element_blank(),
  panel.background=element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
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

options(scipen = 9999) #avoid scientific notation

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

register_google(google_api_key)

#' Choose colors for the campaigns
library(colormap)
# Defaults to 72 colors from the 'viridis' palette.
scales::show_col(colormap(), labels = T)

c1_color <- "#440154FF"
c2_color <- "#3F518BFF"
c3_color <- "#21918DFF"
c4_color <- "#61C960FF"
c5_color <- "#D6E22BFF"
cent_color <- "#FDE725FF"

#' -----------------------------------------------------------------------------
#' Monitoring locations (PM2.5, BC, NO2)
#' -----------------------------------------------------------------------------

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

#' PM2.5 concentrations
pm_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  filter(Sample_Duration != "1 HOUR") %>% 
  arrange(Date_Local, monitor_id) %>%
  filter(monitor_id != '080310013') %>%
  st_transform(crs = ll_wgs84)
pm_mons <- select(pm_data, monitor_id)
pm_mons <- unique(pm_mons)

#' Black carbon
bc_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id) %>%
  mutate(Arithmetic_Mean = ifelse(Arithmetic_Mean <= 0.005, NA, Arithmetic_Mean)) %>%
  st_transform(crs = ll_wgs84) 
bc_mons <- select(bc_data, monitor_id)
bc_mons <- unique(bc_mons)

#' NO2
no2_data <- read_csv(here::here("Data", "Monitor_NO2_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id) %>%
  mutate(Arithmetic_Mean = ifelse(Arithmetic_Mean <= 0, NA, Arithmetic_Mean)) %>%
  filter(monitor_id != "080590006") %>%
  st_transform(crs = ll_wgs84)
no2_mons <- select(no2_data, monitor_id)
no2_mons <- unique(no2_mons)

#' Base map of the area
base_map <- get_map(location = "Denver, CO", zoom = 10)
ggmap(base_map)
attr(base_map, "bb")

ggmap(base_map, darken = c(0.6, "white")) +
  #ggplot() +
  geom_sf(data = st_jitter(pm_mons, factor = 0.01),
          aes(fill = "pm", color = "pm", shape = "pm"),
          inherit.aes = F, show.legend = "point", size = 3) +
  geom_sf(data = st_jitter(no2_mons, factor = 0.01),
          aes(fill = "no2", color = "no2", shape = "no2"),
          inherit.aes = F, show.legend = "point", size = 3) +
  geom_sf(data = st_jitter(bc_mons, factor = 0.01),
          aes(fill = "bc", color = "bc", shape = "bc"),
          inherit.aes = F, show.legend = "point", size = 3) +
  scale_color_manual(name = "Pollutant", #guide= "legend",
                     values = c("pm" = "#3C4F8AFF",
                                "bc" = "#FDE725FF",
                                "no2" = "#5CC863FF"),
                     labels = c("pm" = "PM\u2082.\u2085",
                                "bc" = "Black carbon",
                                "no2" = "NO\u2082")) +
  scale_fill_manual(name = "Pollutant", #guide= "legend",
                    values = c("pm" = "#3C4F8AFF",
                               "bc" = "#FDE725FF",
                               "no2" = "#5CC863FF"),
                    labels = c("pm" = "PM\u2082.\u2085",
                               "bc" = "Black carbon",
                               "no2" = "NO\u2082")) +
  scale_shape_manual(name = "Pollutant", #guide= "legend",
                     values = c("pm" = 15,
                                "bc" = 16,
                                "no2" = 17),
                     labels = c("pm" = "PM\u2082.\u2085",
                                "bc" = "Black carbon",
                                "no2" = "NO\u2082")) +
  annotation_scale(data = pm_mons, #plot_unit = "m", 
                   location = "br", width_hint = 0.5,
                   #pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
                   text_cex = 1,
                   line_col = "black", text_col = "black", text_face = "bold") +
  annotation_north_arrow(data = pm_mons, 
                         location = "tr", which_north = "grid", 
                         #pad_x = unit(0.75, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_minimal(line_col = "black", 
                                                     fill = "black", 
                                                     line_width = 1.6,
                                                     text_col = "black",
                                                     text_face = "bold",
                                                     text_size = 12)) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.75, 0.25),#"right",
        legend.text = element_text(size = 16, color = 'black'),
        text=element_text(family="Helvetica")) +
  # guides(shape = guide_legend(override.aes = list(color = "#3B528BFF",
  #                                                 shape = 19))) +
  map_theme

fig_name <- "EPA_Monitoring_Locations.jpeg"
ggsave(filename = here::here("Figs", fig_name),
       device = "jpeg", dpi=500, units = "in", height = 5, width = 5)


#' -----------------------------------------------------------------------------
#' Figure 1: Sampling locations by campaign and number of samples collected
#' -----------------------------------------------------------------------------

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

lur_data_sf <- read_csv(here::here("Data", "Final_BC_Data_Set.csv")) %>%
  filter(!is.na(bc_ug_m3)) %>%
  mutate(lon2 = lon, lat2 = lat) %>%
  st_as_sf(coords = c('lon2', 'lat2'), crs = ll_wgs84) %>%
  select(site_id, filter_id, campaign)

camp1 <- filter(lur_data_sf, campaign == "Campaign1") %>%
  group_by(site_id) %>%
  summarize(count = n()) 
camp2 <- filter(lur_data_sf, campaign == "Campaign2") %>%
  group_by(site_id) %>%
  summarize(count = n()) 
camp3 <- filter(lur_data_sf, campaign == "Campaign3") %>%
  group_by(site_id) %>%
  summarize(count = n()) 
camp4 <- filter(lur_data_sf, campaign == "Campaign4") %>%
  group_by(site_id) %>%
  summarize(count = n()) 
camp5 <- filter(lur_data_sf, campaign == "Campaign5") %>%
  group_by(site_id) %>%
  summarize(count = n()) 
central <- filter(lur_data_sf, campaign == "CampaignX") %>%
  group_by(site_id) %>%
  summarize(count = n()) 

#' Central Site
bc_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id) %>%
  mutate(Arithmetic_Mean = ifelse(Arithmetic_Mean <= 0.005, NA, Arithmetic_Mean)) %>%
  st_transform(crs = ll_wgs84) 
bc_mons <- select(bc_data, monitor_id)
bc_mons <- unique(bc_mons)

#' Base map of the area
base_map <- get_map(location = "Denver, CO", zoom = 10)
ggmap(base_map)
attr(base_map, "bb")

c1_map <- ggmap(base_map, darken = c(0.6, "white")) +
  geom_sf(data = st_jitter(camp1, 0.01), fill = c1_color, color = "black", 
          aes(shape = "dist"), inherit.aes = F, size = 3) +
  geom_sf(data = bc_mons, aes(shape = "cent"), fill = cent_color, color = "black", 
          inherit.aes = F, size = 3) +
  scale_shape_manual(name = "Site Type",
                     values = c("dist" = 21,
                                "cent" = 24),
                     labels = c("dist" = "Distributed (UPAS)",
                                "cent" = "Central Site BC Monitor")) +
  # annotation_scale(data = camp1, #plot_unit = "m", 
  #                  location = "br", width_hint = 0.5,
  #                  pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
  #                  text_cex = 1.5,
  #                  line_col = "black", text_col = "black", text_face = "bold") +
  # annotation_north_arrow(data = camp1, 
  #                        location = "br", which_north = "grid", 
  #                        pad_x = unit(0.75, "cm"), pad_y = unit(1.5, "cm"),
  #                        style = north_arrow_minimal(line_col = "black", 
  #                                                    fill = "black", 
  #                                                    line_width = 1.6,
  #                                                    text_col = "black",
  #                                                    text_face = "bold",
  #                                                    text_size = 8)) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.15, 0.25)) +
  xlim(-105.3, -104.625) + ylim(39.5, 40.0) +
  map_theme
c1_map

c2_map <- ggmap(base_map, darken = c(0.6, "white")) +
  geom_sf(data = st_jitter(camp2, 0.01), fill = c2_color, color = "black", 
          aes(shape = "dist"), inherit.aes = F, size = 3) +
  geom_sf(data = bc_mons, aes(shape = "cent"), fill = cent_color, color = "black", 
          inherit.aes = F, size = 3) +
  scale_shape_manual(name = "Site Type",
                     values = c("dist" = 21,
                                "cent" = 24),
                     labels = c("dist" = "Distributed (UPAS)",
                                "cent" = "Central Site BC Monitor")) +
  # annotation_scale(data = camp2, #plot_unit = "m", 
  #                  location = "br", width_hint = 0.5,
  #                  pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
  #                  text_cex = 1.5,
  #                  line_col = "black", text_col = "black", text_face = "bold") +
  # annotation_north_arrow(data = camp2, 
  #                        location = "br", which_north = "grid", 
  #                        pad_x = unit(0.75, "cm"), pad_y = unit(1.5, "cm"),
  #                        style = north_arrow_minimal(line_col = "black", 
  #                                                    fill = "black", 
  #                                                    line_width = 1.6,
  #                                                    text_col = "black",
  #                                                    text_face = "bold",
  #                                                    text_size = 8)) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.15, 0.25)) +
  xlim(-105.3, -104.625) + ylim(39.5, 40.0) +
  map_theme
c2_map

c3_map <- ggmap(base_map, darken = c(0.6, "white")) +
  geom_sf(data = st_jitter(camp3, 0.01), fill = c3_color, color = "black", 
          aes(shape = "dist"), inherit.aes = F, size = 3) +
  geom_sf(data = bc_mons, aes(shape = "cent"), fill = cent_color, color = "black", 
          inherit.aes = F, size = 3) +
  scale_shape_manual(name = "Site Type",
                     values = c("dist" = 21,
                                "cent" = 24),
                     labels = c("dist" = "Distributed (UPAS)",
                                "cent" = "Central Site BC Monitor")) +
  annotation_scale(data = camp3, #plot_unit = "m", 
                   location = "br", width_hint = 0.5,
                   pad_x = unit(0.75, "cm"), pad_y = unit(0.25, "cm"),
                   #text_cex = 1.5,
                   line_col = "black", text_col = "black", text_face = "bold") +
  annotation_north_arrow(data = camp3, 
                         location = "br", which_north = "grid", 
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.75, "cm"),
                         style = north_arrow_minimal(line_col = "black", 
                                                     fill = "black", 
                                                     line_width = 0.5,
                                                     text_col = "black",
                                                     text_face = "bold",
                                                     text_size = 10)) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.15, 0.25)) +
  xlim(-105.3, -104.625) + ylim(39.5, 40.0) +
  map_theme
c3_map

c4_map <- ggmap(base_map, darken = c(0.6, "white")) +
  geom_sf(data = st_jitter(camp4, 0.01), fill = c4_color, color = "black", 
          aes(shape = "dist"), inherit.aes = F, size = 3) +
  geom_sf(data = bc_data, aes(shape = "cent"), fill = cent_color, color = "black", 
          inherit.aes = F, size = 3) +
  scale_shape_manual(name = "Site Type",
                     values = c("dist" = 21,
                                "cent" = 24),
                     labels = c("dist" = "Distributed (UPAS)",
                                "cent" = "Central Site BC Monitor")) +
  # annotation_scale(data = camp4, #plot_unit = "m", 
  #                  location = "br", width_hint = 0.5,
  #                  pad_x = unit(0.75, "cm"), pad_y = unit(0.25, "cm"),
  #                  #text_cex = 1.5,
  #                  line_col = "black", text_col = "black", text_face = "bold") +
  # annotation_north_arrow(data = camp4, 
  #                        location = "br", which_north = "grid", 
  #                        pad_x = unit(0.5, "cm"), pad_y = unit(0.75, "cm"),
  #                        style = north_arrow_minimal(line_col = "black", 
  #                                                    fill = "black", 
  #                                                    line_width = 0.5,
  #                                                    text_col = "black",
  #                                                    text_face = "bold",
  #                                                    text_size = 10)) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.15, 0.25)) +
  xlim(-105.3, -104.625) + ylim(39.5, 40.0) +
  map_theme
c4_map

c5_map <- ggmap(base_map, darken = c(0.6, "white")) +
  geom_sf(data = st_jitter(camp5, 0.01), fill = c5_color, color = "black", 
          aes(shape = "dist"), inherit.aes = F, size = 3) +
  geom_sf(data = bc_mons, aes(shape = "cent"), fill = cent_color, color = "black", 
          inherit.aes = F, size = 3) +
  scale_shape_manual(name = "Site Type",
                     values = c("dist" = 21,
                                "cent" = 24),
                     labels = c("dist" = "Distributed (UPAS)",
                                "cent" = "Central Site BC Monitor")) +
  # annotation_scale(data = camp5, #plot_unit = "m", 
  #                  location = "br", width_hint = 0.5,
  #                  pad_x = unit(0.75, "cm"), pad_y = unit(0.25, "cm"),
  #                  #text_cex = 1.5,
  #                  line_col = "black", text_col = "black", text_face = "bold") +
  # annotation_north_arrow(data = camp5, 
  #                        location = "br", which_north = "grid", 
  #                        pad_x = unit(0.5, "cm"), pad_y = unit(0.75, "cm"),
  #                        style = north_arrow_minimal(line_col = "black", 
  #                                                    fill = "black", 
  #                                                    line_width = 0.5,
  #                                                    text_col = "black",
  #                                                    text_face = "bold",
  #                                                    text_size = 10)) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.15, 0.25)) +
  xlim(-105.3, -104.625) + ylim(39.5, 40.0) +
  map_theme
c5_map

ggarrange(c1_map, c2_map, c3_map, c4_map, c5_map, ncol = 3, nrow = 2,
          labels = c("A: Campaign 1", "B: Campaign 2", "C: Campaign 3",
                     "D: Campaign 4", "E: Campaign5"),
          common.legend = T, legend = "bottom",
          hjust = -0.25)

ggsave(file = here::here("Figs", "Sample_Locations_by_Campaign.jpeg"),
       device = "jpeg", units = "in", height = 9, width = 11, dpi = 500)

#' -----------------------------------------------------------------------------
#' Figure 1a: Map of residential sampling locations (residences jittered)
#' -----------------------------------------------------------------------------

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

lur_data_sf <- read_csv(here::here("Data", "Final_BC_Data_Set.csv"),
                        guess_max = 10000) %>%
  filter(!is.na(bc_ug_m3)) %>%
  mutate(lon2 = lon, lat2 = lat) %>%
  st_as_sf(coords = c('lon2', 'lat2'), crs = ll_wgs84) %>%
  filter(participant == 1) %>%
  select(site_id) %>%
  distinct()

glimpse(lur_data_sf)

#' Map in ggmap
base_map <- get_map(location = "Commerce City, CO", zoom = 10)
ggmap(base_map)
attr(base_map, "bb")

ggmap(base_map) +
#ggplot() +
  geom_sf(data = st_jitter(lur_data_sf, 0.02), 
  #geom_sf(data = lur_data_sf, 
          aes(fill = "res", color = "res"), inherit.aes = F, show.legend = "point") +
  # geom_sf(data = filter(filter_data, participant == 0),
  #         aes(fill = "com", color = "com"), inherit.aes = F, show.legend = "point") +
  #geom_sf(data = highways, color = "grey50") +
  scale_color_manual(name = "Sampling\nLocations",
                     values = c("res" = "blue"),
                     labels = c("res" = "Participant")) +
  scale_fill_manual(name = "Sampling\nLocations",
                    values = c("res" = "blue"),
                    labels = c("res" = "Participant")) +
annotation_scale(data = lur_data_sf, #plot_unit = "m",
                 location = "br", width_hint = 0.5,
                 pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"),
                 #text_cex = 1.5,
                 line_col = "black", text_col = "black", text_face = "bold") +
annotation_north_arrow(data = lur_data_sf,
                       location = "br", which_north = "grid",
                       pad_x = unit(0.5, "cm"), pad_y = unit(0.75, "cm"),
                       style = north_arrow_minimal(line_col = "black",
                                                   fill = "black",
                                                   line_width = 0.5,
                                                   text_col = "black",
                                                   text_face = "bold",
                                                   text_size = 10)) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.15, 0.85)) +
  map_theme
ggsave(filename = here::here("Figs", "jittered_participant_locations.jpeg"),
       height = 7, width = 7, units = "in", device = "jpeg", dpi = 500)

#' -----------------------------------------------------------------------------
#' Figure 2: Boxplots of BC concentrations by week
#' -----------------------------------------------------------------------------

lur_data_sf <- read_csv(here::here("Data", "Final_Filters_for_ST_Model.csv")) %>%
  filter(!is.na(bc_ug_m3)) %>%
  mutate(lon2 = lon, lat2 = lat) %>%
  st_as_sf(coords = c('lon2', 'lat2'), crs = ll_wgs84) %>%
  select(site_id, filter_id, campaign, st_week, bc_ug_m3) %>%
  filter(campaign != "CampaignX")
  
nrow(lur_data_sf)

box_data <- lur_data_sf %>%
  select(site_id, campaign, st_week, bc_ug_m3) 

camp_names <- c("Campaign 1", "Campaign 2", "Campaign 3",
                "Campaign 4", "Campaign 5")
names(camp_names) <- c("Campaign1", "Campaign2", "Campaign3",
                       "Campaign4", "Campaign5")
box_plot <- ggplot() +
  geom_boxplot(data = filter(box_data, campaign != "Campaign5"),
               aes(x = as.Date(st_week), y = bc_ug_m3, group = st_week, 
                   color = as.factor (campaign), fill = as.factor(campaign))) +
  geom_point(data = filter(box_data, campaign == "Campaign5"),
               aes(x = as.Date(st_week), y = bc_ug_m3, group = st_week, 
                   color = as.factor (campaign), fill = as.factor(campaign)),
             shape = 21, ) +
  scale_color_manual(name = "Campaign",
                     labels = c("Campaign 1", "Campaign 2", "Campaign 3",
                                "Campaign 4", "Campaign 5"),
                     values = c(c1_color, c2_color, c3_color, c4_color, "black")) +
  scale_fill_manual(name = "Campaign",
                     labels = c("Campaign 1", "Campaign 2", "Campaign 3",
                                "Campaign 4", "Campaign 5"),
                     values = c(NA, NA, NA, NA, c5_color)) +
  xlab("Sampling week") + ylab("Distributed site BC concentration (\u03BCg/m\u00B3)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_wrap(. ~ campaign, nrow = 1, scales = "free_x", 
             labeller = labeller(campaign = camp_names)) +
  scale_x_date(date_labels = "%m-%d-%Y", date_breaks = "1 weeks") +
  simple_theme2 +
  theme(legend.position = "none")
box_plot
ggsave(filename = here::here("Figs", "UPAS_BC_Boxplot_by_Campaign.jpeg"),
       height = 4.5, width = 7, units = "in", device = "jpeg", dpi = 500)

#' -----------------------------------------------------------------------------
#' Figure 3: Time trends
#' -----------------------------------------------------------------------------

library(SpatioTemporal)
library(numDeriv)
library(Matrix)
library(plotrix)
library(maps)

source(here::here("Code", "functions_model.R"))

load(here::here("Results", "Denver_ST_Model_B.rdata"))
names(denver.data.B)

trend_df <- as.data.frame(denver.data.B$trend)
site_df <- as.data.frame(denver.data.B$obs)

#' Layout for the plots
#' Three rows, two columns
jpeg(here::here("Figs", "Basis_Function_Plot.jpg"), 
     width = 7, height = 7, units = "in", res = 500)

#' Layout for the plots
#' Three rows, two columns
layout(matrix(c(1,1,2,2,3,3,4,4,5,5), 5, 2, byrow = TRUE),
       heights = c(2, 1.5, 1.5, 1.5, 1.5))
par(mar = c(2, 2, 2, 2), oma=c(2, 2, 2, 2))
#' Basis functions
plot(denver.data.B$trend$date, denver.data.B$trend$V1, type = "l",
     lty = 1, cex = 2, main = "",
     xlab = "", ylab = "Scaled pollutant concentrations")
title("A) Temporal trend functions", adj = 0)
lines(denver.data.B$trend$date, denver.data.B$trend$V2, 
      lty = 2, cex = 2)
legend(x = as.Date("2017-01-01"), y = 2.75, bty = "n",
       legend=c("Function 1", "Function 2"),
       lty=1:2, cex=1)

#' Obs at the central site
plot(denver.data.B, "obs", ID="central", xlab = "", 
     ylab="Central site BC (log \u03BCg/m\u00B3)", main = "") 
title("B) Central site monitor", adj = 0)
# text(x = as.Date("2016-07-01"), y = 1.75, labels = "Central site monitor",
#      cex = 1.5)
# plot(denver.data.B, "acf", ID="central", xlab = "", ylab = "ACF", main = "")

#' Obs at site d_5
plot(denver.data.B, "obs", ID="d_5", xlab = "", 
     ylab="UPAS BC (log \u03BCg/m\u00B3)", main = "") 
title("C) Distributed site no. 5", adj = 0)
# text(x = as.Date("2016-07-01"), y = 3, labels = "Distributed Site no. 15",
#      cex = 1.5)
# plot(denver.data.B, "acf", ID="d_15", xlab = "", ylab = "ACF", main = "")

#' Obs at site d_15
plot(denver.data.B, "obs", ID="d_15", xlab = "", 
     ylab="UPAS BC (log \u03BCg/m\u00B3)", main = "") 
title("D) Distributed site no. 15", adj = 0)
# text(x = as.Date("2016-07-01"), y = 3, labels = "Distributed Site no. 15",
#      cex = 1.5)
# plot(denver.data.B, "acf", ID="d_15", xlab = "", ylab = "ACF", main = "")

#' Obs at site d_30
plot(denver.data.B, "obs", ID="d_40", xlab = "", 
     ylab="UPAS BC (log \u03BCg/m\u00B3)", main = "") 
title("E) Distributed site no. 40", adj = 0)
# text(x = as.Date("2016-07-01"), y = 3, labels = "Distributed Site no. 15",
#      cex = 1.5)
# plot(denver.data.B, "acf", ID="d_15", xlab = "", ylab = "ACF", main = "")

mtext("Standarized (unitless) pollutant measure", side=2, line=0,
      outer=TRUE, cex=1, las=0)

dev.off()
par(mfrow=c(1,1))

names(denver.model.B)
print(est.denver.model.B)

#' -----------------------------------------------------------------------------
#' Figure 4: Cross-validation results
#' -----------------------------------------------------------------------------

library(SpatioTemporal)
library(numDeriv)
library(Matrix)
library(plotrix)
library(maps)

source(here::here("Code", "functions_model.R"))

load(here::here("Results", "Denver_ST_Model_B.rdata"))
print(est.denver.model.B)

#' Get CV predictions for the plot
pred.B.cv.log <- predictCV(denver.model.B, est.denver.B.cv, LTA = T,
                             transform="unbiased")

names(pred.B.cv.log)
summary(pred.B.cv.log)

#' #' What does this look like in base R?
# par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
# plot(pred.B.cv.log, "obs", ID="all", pch=c(19,NA), cex=.25, lty=c(NA,2),
#      col=c("ID", "black", "grey"),
#      ylim=c(-1,2),
#      xlab="Observations", ylab="Predictions",
#      main="Cross-validation BC (log ug/m3)")
# with(pred.B.cv.log$pred.LTA, plotCI(obs, EX.pred, uiw=1.96*sqrt(VX.pred),
#                                       xlab="Observations", ylab="Predictions",
#                                       main="Temporal average BC (ug/m3)"))
# abline(0, 1, col="grey")

#' Plotting in ggplot2
cv_data <- as.data.frame(pred.B.cv.log$pred.obs) %>%
  filter(ID != "central") %>%
  mutate(month = month(date)) %>%
  mutate(season = ifelse(month %in% c(12, 1, 2), 1, 
                         ifelse(month %in% c(3, 4, 5), 2, 
                                ifelse(month %in% c(6, 7, 8), 3, 4))))
head(cv_data)

lta_data <- as.data.frame(pred.B.cv.log$pred.LTA)

cv_plot <- ggplot(data = cv_data) +
  coord_equal() +
  #geom_point(aes(x = obs, y = EX.pred, color = as.factor(season))) +
  geom_point(aes(x = obs, y = EX.pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey50") +
  # scale_color_viridis(name = "Season", discrete = T,
                      # labels = c("1" = "Winter", "2" = "Spring",
                      #            "3" = "Summer", "4" = "Fall")) +
  xlab("BC Observations (\u03BCg/m\u00B3)") + ylab("BC Predictions (\u03BCg/m\u00B3)") +
  xlim(c(0.7, 2.5)) + ylim(c(0.7, 2.5)) +
  simple_theme +
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = c(0.85, 0.2),
        #legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))
cv_plot

lta_plot <- ggplot(data = lta_data) +
  coord_fixed() +
  geom_pointrange(aes(x = obs, y = EX.pred,
                      ymin = EX.pred - (1.96*sqrt(VX.pred)),
                      ymax = EX.pred + (1.96*sqrt(VX.pred))),
                  color = "grey50") +
  geom_point(aes(x = obs, y = EX.pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey50") +
  xlab("BC Observations (\u03BCg/m\u00B3)") + ylab("BC Predictions (\u03BCg/m\u00B3)") +
  xlim(c(0.75, 2.25)) + ylim(c(0.75, 2.25)) +
  simple_theme +
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))
lta_plot

ggarrange(cv_plot, lta_plot, ncol = 2,
          labels = c("A: Weekly BC observations", "B: Long-term average BC"),
          hjust = -0.25)
ggsave(file = here::here("Figs", "Model_CV_Results.jpeg"),
       device = "jpeg", units = "in", height = 5, width = 8.75, dpi = 500)

#' -----------------------------------------------------------------------------
#' Figure 5: Inset plot LTA for grid locations in downtown Denver
#' -----------------------------------------------------------------------------

load(here::here("Results", "Grid_LTA_Preds_2018.rdata"))

source(here::here("Code", "functions_model.R"))

#' Long term averages for 2018
lta_preds <- as.data.frame(E.1$EX) %>% 
  mutate(week = as.Date(rownames(E.1$EX)))

lta_weeks <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day") 
lta_sites <- colnames(lta_preds)[which(str_detect(colnames(lta_preds), "g_"))]

lta_preds_avg <- lta_preds %>% 
  filter(week %in% lta_weeks) %>% 
  summarize_at(vars(all_of(lta_sites)), mean, na.rm = TRUE)
dim(lta_preds_avg)

grid_lta_preds <- lta_preds_avg %>% 
  pivot_longer(cols = starts_with("g_"), names_to = "grid_id", values_to = "EX") %>% 
  mutate(grid_id = gsub("_mean", "", grid_id))
head(grid_lta_preds)

summary(grid_lta_preds$EX)

grid_sf <- read_csv(here::here("Data", "Spatial_Covariates_Grid_250_m_AEA.csv")) %>% 
  mutate(grid_id = paste0("g_", grid_id))

grid_sf_lta <- left_join(grid_sf, grid_lta_preds, by = "grid_id") %>% 
  st_as_sf(wkt = "WKT", crs = albers)

# denver <- st_read(here::here("Data", "Colorado_County_Boundaries.shp")) %>%
#   filter(CNTY_FIPS == "031") %>%
#   st_transform(crs = ll_wgs84)
# plot(st_geometry(denver))

grid_pts <- data.frame(lon = c(-105.05, -105.05, -104.85, -104.85),
                       lat = c(39.65, 39.85, 39.65, 39.85)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84)

grid_box <- st_convex_hull(st_union(grid_pts))
plot(st_geometry(grid_pts))
plot(st_geometry(grid_box), add = T)

# grid_bounds <- data.frame(lon = c(-105.035, -105.035, -104.85, -104.85),
#                           lat = c(39.70, 39.80, 39.70, 39.80)) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84) %>%
#   st_transform(crs = albers)

grid_box2 <- st_transform(grid_box, crs = albers)

highways2 <- read_csv(here::here("Data", "Highways_AEA.csv")) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>%
  st_transform(crs = ll_wgs84)
highways2_crop <- highways2[grid_box,]

#' Base map of the area
base_map <- get_map(location = "Denver, CO", zoom = 10, maptype = "roadmap")
ggmap(base_map)
attr(base_map, "bb")

g1 <- ggmap(base_map, extent = "device") +
  geom_sf(data = grid_box, fill = NA, col = "red", size = 0.75, inherit.aes = F) +
  xlab("") + ylab("") +
  # ylim(c(39.625, 39.875)) + xlim(c(-105.15, -104.8)) +
  theme(plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  map_theme2
g1

g2 <- ggplot() +
  #geom_sf(data = grid_bounds, color = "transparent") +
  geom_sf(data = grid_sf_lta[grid_box2,], aes(fill = EX), color = NA) +
  geom_sf(data = highways2_crop, color = "white", size = 0.5) +
  scale_fill_viridis(name = "2018 mean\n BC (\u03BCg/m\u00B3)",
                     breaks = waiver(), n.breaks = 6) +
  annotation_scale(data = grid_sf_lta[grid_box2,], plot_unit = "m",
                   location = "bl", width_hint = 0.5,
                   pad_x = unit(1.5, "cm"),
                   #pad_y = unit(1.1, "cm"),
                   text_cex = 1, bar_cols = c("grey90", "black"),
                   line_col = "black", text_col = "black", text_face = "bold") +
  annotation_north_arrow(data = grid_sf_lta[grid_box2,],
                         location = "bl", which_north = "grid",
                         #pad_x = unit(1.5, "cm"),
                         #pad_y = unit(.75, "cm"),
                         style = north_arrow_minimal(line_col = "black",
                                                     fill = "black",
                                                     line_width = 1.5,
                                                     text_col = "black",
                                                     text_face = "bold",
                                                     text_size = 12)) +
  xlab("") + ylab("") +
  map_theme2 +
  # theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
  theme(legend.position = c(1.1, 0.65),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text= element_text(size = 12, face = "bold")) +
  theme(panel.border = element_blank())
g2

inset_map = ggdraw() +
  draw_plot(g2) +
  draw_plot(g1, x = 0.625, y = 0.125, width = 0.35, height = 0.35)
inset_map
ggsave(file = here::here("Figs", "Pred_BC_Downtown_2018.jpeg"),
       device = "jpeg", units = "in", height = 6, width = 7, dpi = 500)

#' -----------------------------------------------------------------------------
#' Figure 6: Boxplots of distributed site predictions 
#' -----------------------------------------------------------------------------

library(SpatioTemporal)
library(numDeriv)
library(Matrix)
library(plotrix)
library(maps)

source(here::here("Code", "functions_model.R"))

load(here::here("Results", "Denver_ST_Model_B.rdata"))
denver.model.B

x.B <- coef(est.denver.model.B, pars = "cov")$par 
x.B

E.B <- predict(denver.model.B, est.denver.model.B, LTA = T, 
               transform="unbiased", pred.var=FALSE)
print(E.B)

week_preds <- as.data.frame(E.B$EX) %>% 
  mutate(week = as.Date(rownames(E.B$EX)),
         year = year(as.Date(rownames(E.B$EX))))

week_sites <- colnames(week_preds)[which(str_detect(colnames(week_preds), "d_"))]

box_preds <- week_preds %>% 
  select(week, all_of(week_sites)) %>% 
  #filter(week %in% week_weeks) %>% 
  mutate(month = month(week),
         year = year(week)) %>% 
  filter(year %in% c(2009:2020))

box_data <- box_preds %>% 
  pivot_longer(-c(week, month, year), names_to = "location", values_to = "pred")
summary(box_data)

#' Box plot of weekly BC predicted at all distributed sites grouped by month
box_summary <- box_data %>% 
  group_by(month) %>% 
  summarize(median = median(pred, na.rm=T)) %>% 
  arrange(desc(median))
box_summary  

pred_box_plot <- ggplot(box_data) +
  geom_boxplot(aes(x = as.factor(month), y = pred), #, color = as.factor(month)),
               show.legend = F) +
  #scale_color_viridis(name = "Month", discrete = T) +
  facet_wrap(. ~ year, scales = "fixed") +
  xlab("") + ylab("Weekly distributed site BC concentration (\u03BCg/m\u00B3): Predicted") +
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_x_discrete(labels = str_sub(month.name, 1, 1)) +
  simple_theme
pred_box_plot
ggsave(file = here::here("Figs", "Weekly_BC_Dist_Site_Preds.jpeg"),
       device = "jpeg", units = "in", height = 7, width = 10, dpi = 500)


#' -----------------------------------------------------------------------------
#' Graphical abstract: Monitor locations, time series, full grid predictions 
#' -----------------------------------------------------------------------------

load(here::here("Results", "Grid_LTA_Preds_2018.rdata"))

#' Long term averages for 2018
lta_preds <- as.data.frame(E.1$EX) %>% 
  mutate(week = as.Date(rownames(E.1$EX)))

lta_weeks <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day") 
lta_sites <- colnames(lta_preds)[which(str_detect(colnames(lta_preds), "g_"))]

lta_preds_avg <- lta_preds %>% 
  filter(week %in% lta_weeks) %>% 
  summarize_at(vars(all_of(lta_sites)), mean, na.rm = TRUE)
dim(lta_preds_avg)

grid_lta_preds <- lta_preds_avg %>% 
  pivot_longer(cols = starts_with("g_"), names_to = "grid_id", values_to = "EX") %>% 
  mutate(grid_id = gsub("_mean", "", grid_id))
head(grid_lta_preds)

summary(grid_lta_preds$EX)

grid_sf <- read_csv(here::here("Data", "Spatial_Covariates_Grid_250_m_AEA.csv")) %>% 
  mutate(grid_id = paste0("g_", grid_id))

grid_sf_lta <- left_join(grid_sf, grid_lta_preds, by = "grid_id") %>% 
  st_as_sf(wkt = "WKT", crs = albers)

highways <- read_csv(here::here("Data", "Highways_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers)
highways_crop <- st_crop(highways, st_bbox(grid_sf_lta))

lta_map <- ggplot() +
  geom_sf(data = grid_sf_lta, aes(fill = EX), color = "transparent", show.legend = F) +
  geom_sf(data = highways_crop, color = "white", size = 0.5, show.legend = F) +
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
  # theme(legend.position = c(0.81, 0.8),
  #       legend.background = element_rect(fill = "white"),
  #       legend.title = element_text(size = 12, face = "bold"),
  #       legend.text= element_text(size = 12, face = "bold")) +
  theme(panel.border = element_blank())
lta_map
ggsave(file = here::here("Figs", "Full_Grid_Mean_BC_2018_For_GA.jpeg"),
       device = "jpeg", units = "in", height = 7, width = 5, dpi = 500)

#' Time trends for three distributed sites
load(here::here("Results", "Denver_ST_Model_B.rdata"))
names(denver.data.B)

trend_df <- as.data.frame(denver.data.B$trend)
site_df <- as.data.frame(denver.data.B$obs)

jpeg(here::here("Figs", "Basis_Function_For_GA.jpg"), 
     width = 7, height = 5, units = "in", res = 500)
layout(matrix(c(1,1,2,2,3,3), 3, 2, byrow = TRUE),
       heights = c(1.5, 1.5, 1.5))
par(mar = c(2, 2, 2, 2))

#' Obs at site d_1
plot(denver.data.B, "obs", ID="d_1", xlab = "", 
     ylab="UPAS BC (log \u03BCg/m\u00B3)", main = "") 

#' Obs at site d_15
plot(denver.data.B, "obs", ID="d_15", xlab = "", 
     ylab="UPAS BC (log \u03BCg/m\u00B3)", main = "") 

#' Obs at site d_35
plot(denver.data.B, "obs", ID="d_35", xlab = "", 
     ylab="UPAS BC (log \u03BCg/m\u00B3)", main = "") 

dev.off()
par(mfrow=c(1,1))

#' Sampling locations
#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

lur_data_sf2 <- read_csv(here::here("Data", "Final_BC_Data_Set.csv")) %>%
  filter(!is.na(bc_ug_m3)) %>%
  mutate(lon2 = lon, lat2 = lat) %>%
  st_as_sf(coords = c('lon2', 'lat2'), crs = ll_wgs84) %>%
  select(site_id, filter_id, campaign)
lur_data_sf2 <- select(lur_data_sf2, site_id)
lur_data_sf2 <- unique(lur_data_sf2)
nrow(lur_data_sf2)

#' Base map of the area
base_map <- get_map(location = "Denver, CO", zoom = 10)
ggmap(base_map)
attr(base_map, "bb")

upas_map <- ggmap(base_map) +
  geom_sf(data = st_jitter(lur_data_sf2, 0.01), fill = c1_color, color = "black", 
          shape = 21, inherit.aes = F, size = 3) +
  annotation_scale(data = lur_data_sf2, #plot_unit = "m",
                   location = "br", width_hint = 0.5,
                   #pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
                   text_cex = 1.5,
                   line_col = "black", text_col = "black", text_face = "bold") +
  annotation_north_arrow(data = lur_data_sf,
                         location = "tr", which_north = "grid",
                         #pad_x = unit(0.75, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_minimal(line_col = "black",
                                                     fill = "black",
                                                     line_width = 1.6,
                                                     text_col = "black",
                                                     text_face = "bold",
                                                     text_size = 8)) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.15, 0.25)) +
  xlim(-105.3, -104.625) + ylim(39.5, 40.0) +
  map_theme
upas_map
ggsave(file = here::here("Figs", "Locations_Map_For_GA.jpeg"),
       device = "jpeg", units = "in", height = 5, width = 5, dpi = 500)

#' -----------------------------------------------------------------------------
#' Figure S1-S3: Time Series for BC, NO2, and PM2.5 monitoring data
#' -----------------------------------------------------------------------------

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

#' NO2 concentrations
no2_data <- read_csv(here::here("Data", "Monitor_NO2_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id) %>%
  mutate(Arithmetic_Mean = ifelse(Arithmetic_Mean <= 0, NA, Arithmetic_Mean))
head(no2_data$Date_Local)
tail(no2_data$Date_Local)

#' Drop monitor 080590006 because it was installed too recently and there isn't a long-term record
no2_data <- filter(no2_data, monitor_id != "080590006")
length(unique(no2_data$monitor_id))

no2_ts2 <- ggplot(data = no2_data, aes(x = Date_Local, y = log(Arithmetic_Mean))) +
  geom_line() +
  #geom_point(shape = 20, size = 0.5) +
  #geom_smooth(se = F, color = "black", method = "gam") +
  scale_color_viridis(name = "Monitor ID", discrete = T) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") +
  facet_wrap(. ~ monitor_id, scales = "fixed", ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Date") + ylab("log(NO\u2082)") +
  simple_theme +
  theme(text=element_text(family="Helvetica"))
no2_ts2
ggsave(here::here("Figs", "SI_Fig_NO2_Time_Series.jpeg"),
       device = "jpeg", units = "in", height = 6, width = 8, dpi = 500)

#' BC
bc_cent_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  arrange(Date_Local, monitor_id) %>%
  mutate(Arithmetic_Mean = ifelse(Arithmetic_Mean <= 0.005, NA, Arithmetic_Mean))
head(bc_cent_data$Date_Local)
tail(bc_cent_data$Date_Local)

#' Add an identifier to differentiate these measurements from collocated pollutants at the same site
bc_cent_data$monitor_id <- paste0(bc_cent_data$monitor_id, "_bc")
unique(bc_cent_data$monitor_id)

bc_ts2 <- ggplot(data = bc_cent_data, aes(x = Date_Local, y = log(Arithmetic_Mean))) +
  geom_line() +
  #geom_point(aes(color = as.factor(monitor_id)), shape = 20, size = 1) +
  #geom_smooth(se = F, color = "black") +
  scale_color_viridis(name = "Monitor ID", discrete = T) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") +
  #facet_wrap(. ~ monitor_id, scales = "fixed", ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Date") + ylab("log(BC)") +
  simple_theme +
  theme(text=element_text(family="Helvetica"))
bc_ts2
ggsave(here::here("Figs", "SI_Fig_BC_Time_Series.jpeg"),
       device = "jpeg", units = "in", height = 3, width = 4, dpi = 500)

#' PM Measurements
pm_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(County_Code = str_sub(monitor_id, start = 3, end = 5)) %>%
  filter(County_Code %in% counties) %>% 
  filter(Sample_Duration != "1 HOUR") %>% 
  filter(!(monitor_id == "080310023" & is.na(Pollutant_Standard))) %>%
  arrange(Date_Local, monitor_id)
head(pm_data$Date_Local)
tail(pm_data$Date_Local)

#' Exclude monitors with really short-term records
pm_data <- filter(pm_data, monitor_id != '080310013')
length(unique(pm_data$monitor_id))

# Log-transformed PM2.5
pm_ts2 <- ggplot(data = pm_data, aes(x = Date_Local, y = log(Arithmetic_Mean))) +
  geom_line() +
  #geom_point(aes(color = as.factor(monitor_id)), shape = 20, size = 1) +
  #geom_smooth(se = F, color = "black", method = "gam") +
  scale_color_viridis(name = "Monitor ID", discrete = T) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") +
  facet_wrap(. ~ monitor_id, scales = "fixed", ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Date") + ylab("log(PM\u2082.\u2085)") +
  simple_theme +
  theme(text=element_text(family="Helvetica"))
pm_ts2
ggsave(here::here("Figs", "SI_Fig_PM2.5_Time_Series.jpeg"),
       device = "jpeg", units = "in", height = 9, width = 8, dpi = 500)

#' -----------------------------------------------------------------------------
#' Figure S4: Fitted trends at each UPAS site
#' -----------------------------------------------------------------------------

library(SpatioTemporal)
library(numDeriv)
library(Matrix)
library(plotrix)
library(maps)

load(here::here("Results", "Denver_ST_Model_B.rdata"))
names(denver.data.B)
as.numeric(unique(gsub("d_", "", denver.data.B$obs$ID))[-1])

trend_df <- as.data.frame(denver.data.B$trend)
site_df <- as.data.frame(denver.data.B$obs)

#' Groups of 10
jpeg(here::here("Figs", "S6_A_Basis_Function_Plot_Dist1-8.jpg"), 
     width = 8, height = 12, units = "in", res = 500)

layout(matrix(c(1,2,3,4,5,6,7,8,9,10), 10, 1, byrow = TRUE),
       heights = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5))
par(mar = c(2, 2, 2, 2), oma=c(2, 2, 2, 2))

#' Obs at the distirbuted Site
plot(denver.data.B, "obs", ID="d_1", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 1"), adj = 0)

plot(denver.data.B, "obs", ID="d_2", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 2"), adj = 0)

plot(denver.data.B, "obs", ID="d_3", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 3"), adj = 0)

plot(denver.data.B, "obs", ID="d_4", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 4"), adj = 0)

plot(denver.data.B, "obs", ID="d_5", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 5"), adj = 0)

plot(denver.data.B, "obs", ID="d_6", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 6"), adj = 0)

plot(denver.data.B, "obs", ID="d_7", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 7"), adj = 0)

plot(denver.data.B, "obs", ID="d_8", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 8"), adj = 0)

# plot(denver.data.B, "obs", ID="d_", xlab = "", 
#      ylab="", main = "") 
# title(expression("Distributed Site No. "), adj = 0)
# 
# plot(denver.data.B, "obs", ID="d_", xlab = "", 
#      ylab="", main = "") 
# title(expression("Distributed Site No. "), adj = 0)

mtext(expression("Standarized (unitless) BC concentration"), side=2, line=0,
      outer=TRUE, cex=1, las=0)
dev.off()
par(mfrow=c(1,1))

jpeg(here::here("Figs", "S6_B_Basis_Function_Plot_Dist10-19.jpg"), 
     width = 8, height = 12, units = "in", res = 500)

layout(matrix(c(1,2,3,4,5,6,7,8,9,10), 10, 1, byrow = TRUE),
       heights = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5))
par(mar = c(2, 2, 2, 2), oma=c(2, 2, 2, 2))

#' Obs at the distirbuted Site
plot(denver.data.B, "obs", ID="d_10", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 10"), adj = 0)

plot(denver.data.B, "obs", ID="d_11", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 11"), adj = 0)

plot(denver.data.B, "obs", ID="d_12", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 12"), adj = 0)

plot(denver.data.B, "obs", ID="d_13", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 13"), adj = 0)

plot(denver.data.B, "obs", ID="d_14", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 14"), adj = 0)

plot(denver.data.B, "obs", ID="d_15", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 15"), adj = 0)

plot(denver.data.B, "obs", ID="d_16", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 16"), adj = 0)

plot(denver.data.B, "obs", ID="d_17", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 17"), adj = 0)

plot(denver.data.B, "obs", ID="d_18", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 18"), adj = 0)

plot(denver.data.B, "obs", ID="d_19", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 19"), adj = 0)

mtext(expression("Standarized (unitless) BC concentration"), side=2, line=0,
      outer=TRUE, cex=1, las=0)
dev.off()
par(mfrow=c(1,1))

jpeg(here::here("Figs", "S6_C_Basis_Function_Plot_Dist20-29.jpg"), 
     width = 8, height = 12, units = "in", res = 500)

layout(matrix(c(1,2,3,4,5,6,7,8,9,10), 10, 1, byrow = TRUE),
       heights = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5))
par(mar = c(2, 2, 2, 2), oma=c(2, 2, 2, 2))

#' Obs at the distirbuted Site
plot(denver.data.B, "obs", ID="d_20", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 20"), adj = 0)

plot(denver.data.B, "obs", ID="d_22", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 22"), adj = 0)

plot(denver.data.B, "obs", ID="d_22", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 22"), adj = 0)

plot(denver.data.B, "obs", ID="d_23", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 23"), adj = 0)

# plot(denver.data.B, "obs", ID="d_24", xlab = "", 
#      ylab="", main = "") 
# title(expression("Distributed Site No. 24"), adj = 0)

plot(denver.data.B, "obs", ID="d_25", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 25"), adj = 0)

plot(denver.data.B, "obs", ID="d_26", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 26"), adj = 0)

# plot(denver.data.B, "obs", ID="d_27", xlab = "", 
#      ylab="", main = "") 
# title(expression("Distributed Site No. 27"), adj = 0)

plot(denver.data.B, "obs", ID="d_28", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 28"), adj = 0)

plot(denver.data.B, "obs", ID="d_29", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 29"), adj = 0)

mtext(expression("Standarized (unitless) BC concentration"), side=2, line=0,
      outer=TRUE, cex=1, las=0)
dev.off()
par(mfrow=c(1,1))

jpeg(here::here("Figs", "S6_D_Basis_Function_Plot_Dist30-39.jpg"), 
     width = 8, height = 12, units = "in", res = 500)

layout(matrix(c(1,2,3,4,5,6,7,8,9,10), 10, 1, byrow = TRUE),
       heights = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5))
par(mar = c(2, 2, 2, 2), oma=c(2, 2, 2, 2))

#' Obs at the distirbuted Site
# plot(denver.data.B, "obs", ID="d_30", xlab = "", 
#      ylab="", main = "") 
# title(expression("Distributed Site No. 30"), adj = 0)

plot(denver.data.B, "obs", ID="d_33", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 33"), adj = 0)

plot(denver.data.B, "obs", ID="d_33", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 33"), adj = 0)

plot(denver.data.B, "obs", ID="d_33", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 33"), adj = 0)

plot(denver.data.B, "obs", ID="d_34", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 34"), adj = 0)

plot(denver.data.B, "obs", ID="d_35", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 35"), adj = 0)

plot(denver.data.B, "obs", ID="d_36", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 36"), adj = 0)

plot(denver.data.B, "obs", ID="d_37", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 37"), adj = 0)

plot(denver.data.B, "obs", ID="d_38", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 38"), adj = 0)

plot(denver.data.B, "obs", ID="d_39", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 39"), adj = 0)

mtext(expression("Standarized (unitless) BC concentration"), side=2, line=0,
      outer=TRUE, cex=1, las=0)
dev.off()
par(mfrow=c(1,1))

jpeg(here::here("Figs", "S6_E_Basis_Function_Plot_Dist40-49.jpg"), 
     width = 8, height = 12, units = "in", res = 500)

layout(matrix(c(1,2,3,4,5,6,7,8,9,10), 10, 1, byrow = TRUE),
       heights = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5))
par(mar = c(2, 2, 2, 2), oma=c(2, 2, 2, 2))

#' Obs at the distributed Site
plot(denver.data.B, "obs", ID="d_40", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 40"), adj = 0)

plot(denver.data.B, "obs", ID="d_44", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 44"), adj = 0)

plot(denver.data.B, "obs", ID="d_44", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 44"), adj = 0)

plot(denver.data.B, "obs", ID="d_44", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 44"), adj = 0)

plot(denver.data.B, "obs", ID="d_44", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 44"), adj = 0)

plot(denver.data.B, "obs", ID="d_45", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 45"), adj = 0)

plot(denver.data.B, "obs", ID="d_46", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 46"), adj = 0)

plot(denver.data.B, "obs", ID="d_47", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 47"), adj = 0)

plot(denver.data.B, "obs", ID="d_48", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 48"), adj = 0)

plot(denver.data.B, "obs", ID="d_49", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 49"), adj = 0)

mtext(expression("Standarized (unitless) BC concentration"), side=2, line=0,
      outer=TRUE, cex=1, las=0)
dev.off()
par(mfrow=c(1,1))

jpeg(here::here("Figs", "S6_F_Basis_Function_Plot_Dist50-59.jpg"), 
     width = 8, height = 12, units = "in", res = 500)

layout(matrix(c(1,2,3,4,5,6,7,8,9,10), 10, 1, byrow = TRUE),
       heights = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5))
par(mar = c(2, 2, 2, 2), oma=c(2, 2, 2, 2))

#' Obs at the distributed Site
plot(denver.data.B, "obs", ID="d_50", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 50"), adj = 0)

plot(denver.data.B, "obs", ID="d_55", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 55"), adj = 0)

plot(denver.data.B, "obs", ID="d_55", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 55"), adj = 0)

plot(denver.data.B, "obs", ID="d_55", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 55"), adj = 0)

plot(denver.data.B, "obs", ID="d_54", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 54"), adj = 0)

plot(denver.data.B, "obs", ID="d_55", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 55"), adj = 0)

plot(denver.data.B, "obs", ID="d_56", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 56"), adj = 0)

plot(denver.data.B, "obs", ID="d_57", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 57"), adj = 0)

plot(denver.data.B, "obs", ID="d_58", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 58"), adj = 0)

# plot(denver.data.B, "obs", ID="d_59", xlab = "",
#      ylab="", main = "")
# title(expression("Distributed Site No. 59"), adj = 0)

mtext(expression("Standarized (unitless) BC concentration"), side=2, line=0,
      outer=TRUE, cex=1, las=0)
dev.off()
par(mfrow=c(1,1))

jpeg(here::here("Figs", "S6_G_Basis_Function_Plot_Dist60-69.jpg"), 
     width = 8, height = 12, units = "in", res = 500)

layout(matrix(c(1,2,3,4,5,6,7,8,9,10), 10, 1, byrow = TRUE),
       heights = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5))
par(mar = c(2, 2, 2, 2), oma=c(2, 2, 2, 2))

#' Obs at the distributed Site
plot(denver.data.B, "obs", ID="d_60", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 60"), adj = 0)

plot(denver.data.B, "obs", ID="d_66", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 66"), adj = 0)

plot(denver.data.B, "obs", ID="d_66", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 66"), adj = 0)

plot(denver.data.B, "obs", ID="d_66", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 66"), adj = 0)

plot(denver.data.B, "obs", ID="d_64", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 64"), adj = 0)

plot(denver.data.B, "obs", ID="d_66", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 66"), adj = 0)

plot(denver.data.B, "obs", ID="d_66", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 66"), adj = 0)

plot(denver.data.B, "obs", ID="d_67", xlab = "",
     ylab="", main = "")
title(expression("Distributed Site No. 67"), adj = 0)

plot(denver.data.B, "obs", ID="d_68", xlab = "", 
     ylab="", main = "") 
title(expression("Distributed Site No. 68"), adj = 0)

# plot(denver.data.B, "obs", ID="d_69", xlab = "",
#      ylab="", main = "")
# title(expression("Distributed Site No. 69"), adj = 0)

mtext(expression("Standarized (unitless) BC concentration"), side=2, line=0,
      outer=TRUE, cex=1, las=0)
dev.off()
par(mfrow=c(1,1))

#' -----------------------------------------------------------------------------
#' Trend plots for monitors

library(SpatioTemporal)
library(numDeriv)
library(Matrix)
library(plotrix)
library(maps)

load(here::here("Results", "Denver_ST_Monitors.rdata"))
names(pol_STdata2)

trend_df <- as.data.frame(pol_STdata2$trend)
site_df <- as.data.frame(pol_STdata2$obs) 

pm_site_df <- filter(site_df, str_detect(ID, "_pm"))
pm_site_df$ID <- gsub("_pm", "", pm_site_df$ID)

no2_site_df <- filter(site_df, str_detect(ID, "_no2"))
no2_site_df$ID <- gsub("_no2", "", no2_site_df$ID)

jpeg(here::here("Figs", "S4_Basis_Function_Plot_PM.jpg"), 
     width = 8, height = 12, units = "in", res = 500)

layout(matrix(c(1,2,3,4,5,6,7,8,9,10), 10, 1, byrow = TRUE),
       heights = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5))
par(mar = c(2, 2, 2, 2), oma=c(2, 2, 2, 2))

#' Obs at the central sites
plot(pol_STdata2, "obs", ID="080010006_pm", xlab = "", 
     ylab="", main = "") 
title(expression("PM"[2.5]*" Monitor: 080010006"), adj = 0)

plot(pol_STdata2, "obs", ID="080010008_pm", xlab = "", 
     ylab="", main = "") 
title(expression("PM"[2.5]*" Monitor: 080010008"), adj = 0)

plot(pol_STdata2, "obs", ID="080050005_pm", xlab = "", 
     ylab="", main = "") 
title(expression("PM"[2.5]*" Monitor: 080050005"), adj = 0)

plot(pol_STdata2, "obs", ID="080130003_pm", xlab = "", 
     ylab="", main = "") 
title(expression("PM"[2.5]*" Monitor: 080130003"), adj = 0)

plot(pol_STdata2, "obs", ID="080130012_pm", xlab = "", 
     ylab="", main = "") 
title(expression("PM"[2.5]*" Monitor: 080130012"), adj = 0)

plot(pol_STdata2, "obs", ID="080310002_pm", xlab = "", 
     ylab="", main = "") 
title(expression("PM"[2.5]*" Monitor: 080310002"), adj = 0)

plot(pol_STdata2, "obs", ID="080310023_pm", xlab = "", 
     ylab="", main = "") 
title(expression("PM"[2.5]*" Monitor: 080310023"), adj = 0)

plot(pol_STdata2, "obs", ID="080310025_pm", xlab = "", 
     ylab="", main = "") 
title(expression("PM"[2.5]*" Monitor: 080310025"), adj = 0)

plot(pol_STdata2, "obs", ID="080310026_pm", xlab = "", 
     ylab="", main = "") 
title(expression("PM"[2.5]*" Monitor: 080310026"), adj = 0)

plot(pol_STdata2, "obs", ID="080310027_pm", xlab = "", 
     ylab="", main = "") 
title(expression("PM"[2.5]*" Monitor: 080310027"), adj = 0)

mtext(expression("Standarized (unitless) PM"[2.5]*" concentration"), side=2, line=0,
      outer=TRUE, cex=1, las=0)

dev.off()
par(mfrow=c(1,1))

jpeg(here::here("Figs", "S5_Basis_Function_Plot_NO2.jpg"), 
     width = 8, height = 8, units = "in", res = 500)

layout(matrix(c(1,2,3,4,5), 5, 1, byrow = TRUE),
       heights = c(1.5, 1.5, 1.5, 1.5, 1.5))
par(mar = c(2, 2, 2, 2), oma=c(2, 2, 2, 2))

#' Obs at the central sites
plot(pol_STdata2, "obs", ID="080013001_no2", xlab = "", 
     ylab="", main = "") 
title(expression("NO"[2]*" Monitor: 080130001"), adj = 0)

plot(pol_STdata2, "obs", ID="080310002_no2", xlab = "", 
     ylab="", main = "") 
title(expression("NO"[2]*" Monitor: 080310002"), adj = 0)

plot(pol_STdata2, "obs", ID="080310026_no2", xlab = "", 
     ylab="", main = "") 
title(expression("NO"[2]*" Monitor: 080310026"), adj = 0)

plot(pol_STdata2, "obs", ID="080310027_no2", xlab = "", 
     ylab="", main = "") 
title(expression("NO"[2]*" Monitor: 080310027"), adj = 0)

plot(pol_STdata2, "obs", ID="080310028_no2", xlab = "", 
     ylab="", main = "") 
title(expression("NO"[2]*" Monitor: 080310028"), adj = 0)

mtext(expression("Standarized (unitless) NO"[2]*" concentration"), side=2, line=0,
      outer=TRUE, cex=1, las=0)

dev.off()
par(mfrow=c(1,1))

