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
  text  = element_text(family="Arial",size = 14, color = 'black'),
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
windowsFonts(Arial=windowsFont("TT Arial"))

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
c2_color <- "#2C718EFF"
c3_color <- "#61C960FF"
cent_color <- "#FDE725FF"

#' -----------------------------------------------------------------------------
#' Figures for A&WMA Extended Abstract
#' -----------------------------------------------------------------------------
#' 
#' data_name <- "Combined_Filter_Data_AEA.csv"
#' lur_data_sf <- read_csv(here::here("Data", data_name)) %>%
#'   filter(!is.na(lon)) %>% 
#'   mutate(lon2 = lon, lat2 = lat) %>% 
#'   st_as_sf(coords = c('lon2', 'lat2'), crs = ll_wgs84) %>%
#'   st_transform(crs = albers) %>%
#'   filter(indoor == 0) %>%
#'   filter(bc_ug_m3_dem > 0) %>%
#'   rename("pm_ug_m3_raw" = "pm_ug_m3") %>%
#'   rename("pm_ug_m3" = "pm_ug_m3_dem") %>%
#'   rename("bc_ug_m3_raw" = "bc_ug_m3") %>%
#'   rename("bc_ug_m3" = "bc_ug_m3_dem") %>% 
#'   filter(campaign %in% paste0("Campaign", c(1, 2, 3, "X")))
#' 
#' lur_data_sf <- lur_data_sf %>% 
#'   mutate(sample_week_no = format(StartDateLocal, "%Y-%W")) #%>% 
#'   #mutate(sample_week = as.Date(cut(as.Date(StartDateLocal), "week")))
#' 
#' #' Add a unique site ID
#' lur_data_sf <- mutate(lur_data_sf, site_id_lonlat = paste(lon, lat, sep = "_"))
#' 
#' ids <- select(lur_data_sf, site_id_lonlat) %>% 
#'   distinct(site_id_lonlat) %>% 
#'   mutate(site_id = paste("d", seq_along(site_id_lonlat), sep = "_"))
#' 
#' lur_data_sf <- st_join(lur_data_sf, ids, by = "site_id_lonlat") %>% 
#'   mutate(site_id = ifelse(filter_id == "080310027", "central", site_id))
#' 
#' loc_means <- lur_data_sf %>% 
#'   select(site_id, bc_ug_m3) %>% 
#'   group_by(site_id) %>% 
#'   summarize(bc_ug_m3 = mean(bc_ug_m3, na.rm = T),
#'             count = n()) %>% 
#'   st_transform(crs = ll_wgs84) %>% 
#'   filter(site_id != "central")
#' 
#' highways <- read_csv(here::here("Data", "Highways_AEA.csv")) %>%
#'   st_as_sf(wkt = "WKT", crs = albers) %>% 
#'   st_transform(crs = ll_wgs84)
#' 
#' loc_mean_plot <- ggplot() +
#'   geom_sf(data = loc_means, aes(color = bc_ug_m3, size = count), 
#'           show.legend = "point") +
#'   geom_sf(data = highways, color = "red", show.legend = "line") +
#'   scale_color_viridis(name = "BC (ug/m3)") +
#'   scale_size_continuous(name = "No. of samples\ncollected") +
#'   north(x.min = -105.3726, x.max = -104.4937,
#'         y.min =  39.5, y.max = 40.14455,
#'         symbol = 12, location = "bottomright", scale = 0.05) +
#'   ggsn::scalebar(x.min = -105.3726, x.max = -104.4937,
#'                  y.min =  39.45, y.max = 40.14455, transform = T, dist_unit = "km",
#'                  dist = 10, model="WGS84", st.bottom = F, st.size = 3,
#'                  height = 0.01) +
#'   xlab("") + ylab("") +
#'   simple_theme +
#'   theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
#'   theme(axis.ticks = element_blank(),
#'         axis.text = element_blank())
#' loc_mean_plot
#' ggsave(filename = here::here("Figs", "AWMA_Fig1A.jpeg"), 
#'        height = 5, width = 6, units = "in", device = "jpeg", dpi = 400)
#' 
#' box_data <- lur_data_sf %>% 
#'   select(site_id, campaign, week, bc_ug_m3) %>% 
#'   filter(campaign != "CampaignX") %>% 
#'   mutate(week_id = paste(campaign, week, sep = "-"))
#' 
#' box_plot <- ggplot(box_data) +
#'   geom_boxplot(aes(x = week_id, y = bc_ug_m3, color = as.factor(campaign))) +
#'   scale_color_viridis(name = "Campaign", discrete = T) +
#'   xlab("Campaign week") + ylab("Distributed site BC concentration (ug/m3)") +
#'   # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
#'   scale_x_discrete(labels = seq(1:length(unique(box_data$week_id)))) +
#'   simple_theme
#' box_plot
#' ggsave(filename = here::here("Figs", "AWMA_Fig1B.jpeg"), 
#'        height = 5, width = 7, units = "in", device = "jpeg", dpi = 400)

#' -----------------------------------------------------------------------------
#' Monitoring locations (PM2.5, BC, NO2)
#' -----------------------------------------------------------------------------

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

#' PM2.5 concentrations
pm_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  filter(County_Code %in% counties) %>% 
  filter(Sample_Duration != "1 HOUR") %>% 
  select(monitor_id) %>% 
  distinct(monitor_id) %>% 
  filter(monitor_id != '080310013')
pm_data <- st_transform(pm_data, crs = ll_wgs84)

#' Black carbon
bc_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv")) %>%
  filter(!is.na(Arithmetic_Mean)) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>%
  filter(County_Code %in% counties) %>%
  select(monitor_id) %>%
  distinct(monitor_id)
bc_data <- st_transform(bc_data, crs = ll_wgs84)

#' NO2
no2_data <- read_csv(here::here("Data", "Monitor_NO2_Data_AEA.csv")) %>%
  filter(!is.na(Arithmetic_Mean)) %>%
  st_as_sf(wkt = "WKT", crs = albers)%>%
  filter(County_Code %in% counties) %>%
  select(monitor_id) %>%
  distinct(monitor_id) %>% 
  filter(monitor_id != "080590006")
no2_data <- st_transform(no2_data, crs = ll_wgs84)

#' Base map of the area
base_map <- get_map(location = "Denver, CO", zoom = 10)
ggmap(base_map)
attr(base_map, "bb")

ggmap(base_map) +
  #ggplot() +
  geom_sf(data = pm_data,
          aes(fill = "pm", color = "pm", shape = "pm"),
          inherit.aes = F, show.legend = "point", size = 3) +
  geom_sf(data = bc_data,
          aes(fill = "bc", color = "bc", shape = "bc"),
          inherit.aes = F, show.legend = "point", size = 3) +
  geom_sf(data = no2_data,
          aes(fill = "no2", color = "no2", shape = "no2"),
          inherit.aes = F, show.legend = "point", size = 3) +
  scale_color_manual(name = "Pollutant", #guide= "legend",
                     values = c("pm" = "#440154FF",
                                "bc" = "#39568CFF",
                                "no2" = "#1F968BFF"),
                     labels = c("pm" = "PM\u2082.\u2085",
                                "bc" = "Black carbon",
                                "no2" = "NO\u2082")) +
  scale_fill_manual(name = "Pollutant", #guide= "legend",
                    values = c("pm" = "#440154FF",
                               "bc" = "#39568CFF",
                               "no2" = "#1F968BFF"),
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
  annotation_scale(data = pm_data, #plot_unit = "m", 
                   location = "br", width_hint = 0.5,
                   pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
                   text_cex = 1.5,
                   line_col = "black", text_col = "black", text_face = "bold") +
  annotation_north_arrow(data = pm_data, 
                         location = "br", which_north = "grid", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_minimal(line_col = "black", 
                                                     fill = "black", 
                                                     line_width = 1.6,
                                                     text_col = "black",
                                                     text_face = "bold",
                                                     text_size = 12)) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.75, 0.85),
        legend.text = element_text(family="Calibri",size = 16, color = 'black')) +
  # guides(shape = guide_legend(override.aes = list(color = "#3B528BFF",
  #                                                 shape = 19))) +
  map_theme

fig_name <- "Monitoring_Locations.jpeg"
ggsave(filename = here::here("Figs", fig_name),
       device = "jpeg", dpi=500, units = "in", height = 5, width = 5)


#' -----------------------------------------------------------------------------
#' Figure 1: Sampling locations by campaign and number of samples collected
#' -----------------------------------------------------------------------------

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

data_name <- "Combined_Filter_Data_AEA.csv"
lur_data_sf <- read_csv(here::here("Data", data_name)) %>%
  filter(!is.na(lon)) %>% 
  filter(indoor == 0) %>% 
  filter(bc_ug_m3_dem > 0) %>% 
  filter(is.na(below_lod) | below_lod == 0) %>% 
  filter(is.na(low_volume_flag) | low_volume_flag == 0) %>% 
  filter(is.na(flow_rate_flag) | flow_rate_flag == 0) %>% 
  filter(is.na(is_blank) | is_blank == 0) %>% 
  filter(is.na(negative_pm_mass) | negative_pm_mass == 0) %>% 
  filter(is.na(potential_contamination) | potential_contamination == 0) %>% 
  filter(is.na(bc_mass_ug_corrected) | bc_mass_ug_corrected >= 1.41) %>%  
  mutate(lon2 = lon, lat2 = lat) %>%
  st_as_sf(coords = c('lon2', 'lat2'), crs = ll_wgs84) %>%
  st_transform(crs = albers) %>%
  rename("bc_ug_m3_raw" = "bc_ug_m3") %>%
  rename("bc_ug_m3" = "bc_ug_m3_dem") %>% 
  filter(campaign %in% paste0('Campaign', c(1, 2, 3, "X"))) %>% 
  st_transform(crs = ll_wgs84)

camp1 <- filter(lur_data_sf, campaign == "Campaign1") %>%
  group_by(WKT) %>%
  summarize(count = n()) %>% 
  st_transform(crs = ll_wgs84) 
camp2 <- filter(lur_data_sf, campaign == "Campaign2") %>%
  group_by(WKT) %>%
  summarize(count = n()) %>% 
  st_transform(crs = ll_wgs84)
camp3 <- filter(lur_data_sf, campaign == "Campaign3") %>%
  group_by(WKT) %>%
  summarize(count = n()) %>% 
  st_transform(crs = ll_wgs84)
central <- filter(lur_data_sf, campaign == "CampaignX") %>%
  group_by(WKT) %>%
  summarize(count = n()) %>% 
  st_transform(crs = ll_wgs84)

#' Central Site
bc_data <- read_csv(here::here("Data", "Monitor_BC_Data_AEA.csv")) %>%
  filter(!is.na(Arithmetic_Mean)) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>%
  filter(County_Code %in% counties) %>%
  select(monitor_id) %>%
  distinct(monitor_id)
bc_data <- st_transform(bc_data, crs = ll_wgs84)

#' Base map of the area
base_map <- get_map(location = "Denver, CO", zoom = 10)
ggmap(base_map)
attr(base_map, "bb")

c1_map <- ggmap(base_map) +
  geom_sf(data = st_jitter(camp1, 0.01), fill = c1_color, color = "black", 
          aes(shape = "dist"), inherit.aes = F, size = 3) +
  geom_sf(data = bc_data, aes(shape = "cent"), fill = cent_color, color = "black", 
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

c2_map <- ggmap(base_map) +
  geom_sf(data = st_jitter(camp2, 0.01), fill = c2_color, color = "black", 
          aes(shape = "dist"), inherit.aes = F, size = 3) +
  geom_sf(data = bc_data, aes(shape = "cent"), fill = cent_color, color = "black", 
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

c3_map <- ggmap(base_map) +
  geom_sf(data = st_jitter(camp3, 0.01), fill = c3_color, color = "black", 
          aes(shape = "dist"), inherit.aes = F, size = 3) +
  geom_sf(data = bc_data, aes(shape = "cent"), fill = cent_color, color = "black", 
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

ggarrange(c1_map, c2_map, c3_map, ncol = 3,
          labels = c("A: Campaign 1", "B: Campaign 2", "C: Campaign 3"),
          common.legend = T, legend = "bottom",
          hjust = -0.25)

ggsave(file = here::here("Figs", "Sample_Locations_by_Campaign.jpeg"),
       device = "jpeg", units = "in", height = 4.25, width = 10, dpi = 500)

#' -----------------------------------------------------------------------------
#' Figure 2: Boxplots of BC concentrations by week
#' -----------------------------------------------------------------------------

data_name <- "Combined_Filter_Data_AEA.csv"
lur_data_sf <- read_csv(here::here("Data", data_name)) %>%
  filter(!is.na(lon)) %>% 
  filter(indoor == 0) %>% 
  filter(bc_ug_m3_dem > 0) %>% 
  filter(is.na(below_lod) | below_lod == 0) %>% 
  filter(is.na(low_volume_flag) | low_volume_flag == 0) %>% 
  filter(is.na(flow_rate_flag) | flow_rate_flag == 0) %>% 
  filter(is.na(is_blank) | is_blank == 0) %>% 
  filter(is.na(negative_pm_mass) | negative_pm_mass == 0) %>% 
  filter(is.na(potential_contamination) | potential_contamination == 0) %>% 
  filter(is.na(bc_mass_ug_corrected) | bc_mass_ug_corrected >= 1.41) %>%  
  mutate(lon2 = lon, lat2 = lat) %>%
  st_as_sf(coords = c('lon2', 'lat2'), crs = ll_wgs84) %>%
  st_transform(crs = albers) %>%
  rename("pm_ug_m3_raw" = "pm_ug_m3") %>%
  rename("pm_ug_m3" = "pm_ug_m3_lm") %>%
  rename("bc_ug_m3_raw" = "bc_ug_m3") %>%
  rename("bc_ug_m3" = "bc_ug_m3_dem") %>% 
  filter(campaign %in% paste0('Campaign', c(1, 2, 3)))
  
nrow(lur_data_sf)

lur_data_sf <- lur_data_sf %>%
  mutate(sample_week_no = format(StartDateLocal, "%Y-%W")) %>%
  mutate(sample_week = as.Date(cut(as.Date(StartDateLocal), "week")))

#' Add a unique site ID
ids <- read_csv(here::here("Data", "ST_Model_Site_IDs.csv"))

lur_data_sf <- mutate(lur_data_sf, site_id_lonlat = paste(lon, lat, sep = "_"))
lur_data_sf <- left_join(lur_data_sf, ids, by = "site_id_lonlat") %>%
  mutate(site_id = ifelse(filter_id == "080310027", "central", site_id))

box_data <- lur_data_sf %>%
  select(site_id, campaign, sample_week, bc_ug_m3) 

box_plot <- ggplot(box_data) +
  geom_boxplot(aes(x = as.Date(sample_week), y = bc_ug_m3, group = sample_week, 
               color = as.factor(campaign))) +
  scale_color_manual(name = "Campaign",
                     labels = c("Campaign 1", "Campaign 2", "Campaign 3"),
                     values = c(c1_color, c2_color, c3_color)) +
  xlab("Sampling week") + ylab("Distributed site BC concentration (\u03BCg/m\u00B3)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_x_date(date_labels = "%m-%d-%Y", date_breaks = "2 weeks") +
  simple_theme2 +
  theme(legend.position = c(0.15, 0.8))
box_plot
ggsave(filename = here::here("Figs", "UPAS_BC_Boxplot.jpeg"),
       height = 4.5, width = 6, units = "in", device = "jpeg", dpi = 500)

#' -----------------------------------------------------------------------------
#' Figure 3: Time trends
#' -----------------------------------------------------------------------------

library(SpatioTemporal)
library(numDeriv)
library(Matrix)
library(plotrix)
library(maps)

load(here::here("Results", "Denver_ST_Model_B.rdata"))
names(denver.data.B)

trend_df <- as.data.frame(denver.data.B$trend)
site_df <- as.data.frame(denver.data.B$obs)

jpeg(here::here("Figs", "Basis_Function_Plot.jpg"), 
     width = 7, height = 5, units = "in", res = 500)

#' Layout for the plots
#' Three rows, two columns
# layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE),
#        heights = c(2, 1.5, 1.5))
layout(matrix(c(1,1,2,2,3,3), 3, 2, byrow = TRUE),
       heights = c(2, 1.5, 1.5))
par(mar = c(2, 2, 2, 2))
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

#' Obs at site d_34
plot(denver.data.B, "obs", ID="d_15", xlab = "", 
     ylab="UPAS BC (log \u03BCg/m\u00B3)", main = "") 
title("C) Distributed site no. 15", adj = 0)
# text(x = as.Date("2016-07-01"), y = 3, labels = "Distributed Site no. 15",
#      cex = 1.5)
# plot(denver.data.B, "acf", ID="d_15", xlab = "", ylab = "ACF", main = "")

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

load(here::here("Results", "Denver_ST_Model_B.rdata"))

#' Get CV predictions for the plot
pred.B.cv.log <- predictCV(denver.model.B, est.denver.B.cv,
                             LTA = T, transform="unbiased")

names(pred.B.cv.log)
summary(pred.B.cv.log)

#' #' What does this look like in base R?
#' par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
#' plot(pred.B.cv.log, "obs", ID="all", pch=c(19,NA), cex=.25, lty=c(NA,2),
#'      col=c("ID", "black", "grey"),
#'      ylim=c(-1,2),
#'      xlab="Observations", ylab="Predictions",
#'      main="Cross-validation BC (log ug/m3)")
#' with(pred.B.cv.log$pred.LTA, plotCI(obs, EX.pred, uiw=1.96*sqrt(VX.pred),
#'                                       xlab="Observations", ylab="Predictions",
#'                                       main="Temporal average BC (ug/m3)"))
#' abline(0, 1, col="grey")

#' Plotting in ggplot2
cv_data <- as.data.frame(pred.B.cv.log$pred.obs)
head(cv_data)

lta_data <- as.data.frame(pred.B.cv.log$pred.LTA)

cv_plot <- ggplot(data = cv_data) +
  coord_equal() +
  geom_point(aes(x = obs, y = EX.pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey50") +
  xlab("BC Observations (\u03BCg/m\u00B3)") + ylab("BC Predictions (\u03BCg/m\u00B3)") +
  xlim(c(0.7, 2.5)) + ylim(c(0.7, 2.5)) +
  simple_theme +
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))
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

denver <- st_read(here::here("Data", "Colorado_County_Boundaries.shp")) %>%
  filter(CNTY_FIPS == "031") %>%
  st_transform(crs = ll_wgs84)
plot(st_geometry(denver))

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
  geom_sf(data = highways2_crop, color = "white", size = 1) +
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
  filter(year %in% c(2009:2018))

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
  geom_sf(data = grid_sf_lta, aes(fill = EX), color = NA, show.legend = F) +
  geom_sf(data = highways_crop, color = "white", size = 1, show.legend = F) +
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

data_name <- "Combined_Filter_Data_AEA.csv"
lur_data_sf2 <- read_csv(here::here("Data", data_name)) %>%
  filter(!is.na(lon)) %>% 
  filter(indoor == 0) %>% 
  filter(bc_ug_m3_dem > 0) %>% 
  filter(is.na(below_lod) | below_lod == 0) %>% 
  filter(is.na(low_volume_flag) | low_volume_flag == 0) %>% 
  filter(is.na(flow_rate_flag) | flow_rate_flag == 0) %>% 
  filter(is.na(is_blank) | is_blank == 0) %>% 
  filter(is.na(negative_pm_mass) | negative_pm_mass == 0) %>% 
  filter(is.na(potential_contamination) | potential_contamination == 0) %>% 
  filter(is.na(bc_mass_ug_corrected) | bc_mass_ug_corrected >= 1.41) %>%  
  mutate(lon2 = lon, lat2 = lat) %>%
  st_as_sf(coords = c('lon2', 'lat2'), crs = ll_wgs84) %>%
  st_transform(crs = albers) %>%
  rename("bc_ug_m3_raw" = "bc_ug_m3") %>%
  rename("bc_ug_m3" = "bc_ug_m3_dem") %>% 
  filter(campaign %in% paste0('Campaign', c(1, 2, 3, "X"))) %>% 
  st_transform(crs = ll_wgs84) %>% 
  mutate(site_id_lon_lat = paste(lon, lat, sep = "_")) %>%
  select(site_id_lon_lat) %>%
  distinct(site_id_lon_lat)

nrow(lur_data_sf2)

#' Base map of the area
base_map <- get_map(location = "Denver, CO", zoom = 10)
ggmap(base_map)
attr(base_map, "bb")

upas_map <- ggmap(base_map) +
  geom_sf(data = st_jitter(lur_data_sf2, 0.01), fill = c1_color, color = "black", 
          shape = 21, inherit.aes = F, size = 3) +
  annotation_scale(data = lur_data_sf, #plot_unit = "m",
                   location = "br", width_hint = 0.5,
                   pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
                   text_cex = 1.5,
                   line_col = "black", text_col = "black", text_face = "bold") +
  annotation_north_arrow(data = lur_data_sf,
                         location = "br", which_north = "grid",
                         pad_x = unit(0.75, "cm"), pad_y = unit(1.5, "cm"),
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
