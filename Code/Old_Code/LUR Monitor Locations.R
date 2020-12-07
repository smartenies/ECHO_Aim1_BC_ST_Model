#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 Land Use Regression
#' 
#' Task: Identify which grid cells contain an HS participant or community
#' monitor
#' 
#' Author: Sheena Martenies
#' Date Created: May 2, 2018
#' Contact: sheena.martenies@colostate.edu
#' -----------------------------------------------------------------------------

#' Load required libraries
library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(tidyverse)
library(readxl)

#' For mapping
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
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
windowsFonts(Calibri=windowsFont("TT Calibri"))

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm_13 <- "+init=epsg:26913"

#' Participant IDs to drop
drop <- c(104, 106, 113, 118)

#' read in grid data 
grid_250 <- st_read(here::here("Data", "Grid_250_m_AEA.csv"),
                    stringsAsFactors = F, wkt = "WKT", crs = albers) 

#' 1 km buffer around the grid
grid_bound <- st_buffer(st_union(grid_250), dist = 1000)

#' -----------------------------------------------------------------------------
#' 1) Identify which grid cells contain an interested HS participant
#'    NOTE: Must have access to the UCDenver server to run this section of the
#'    code
#' -----------------------------------------------------------------------------

p_path <- "P:/Martenies/Shared Files/"

#' Read in geocoded participant addresses
hs_add <- st_read(paste0(p_path, "HealthyStartII_Geocoded_Final.shp"))

#' Idenitfy interested participants
hs_list <- read_excel(path=paste0(p_path,
                                 "ECHO Aim 1 Participant Contact Tracker 20180403.xlsx"),
                      skip=1, sheet=1) %>%
  filter(Interested == "Yes") %>%
  select(PID)

hs_int_add <- hs_add %>%
  filter(PID %in% hs_list$PID)
nrow(hs_int_add)

#' Project points to the same CRS as the grid
#' Identify interested participants within the study boundary
hs <- st_transform(hs_int_add, crs=albers)
hs <- hs[grid_bound,]
hs$seq_id <- 100+seq(1:(nrow(hs)))

plot(st_geometry(grid_bound), col=NA, border="red",
     main="Interested HS participants")
plot(st_geometry(hs), add=T)

#' Remove participant information and save
hs <- hs[,c(13,24)]
st_write(hs, here::here("Data/HS_Data", "Interested Participants.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

hs_key <- st_set_geometry(hs, NULL)
write_csv(hs_key, here::here("Data/HS_Data", "HS ID Key.csv"))

#' -----------------------------------------------------------------------------
#' 2) Identify potenital monitoring locations
#' -----------------------------------------------------------------------------

#' Community Monitors
mon <- read_excel(here::here("Data/Raw_Geo_Data", "Community Monitoring Sites v2.xlsx"),
                  sheet = 2) %>%
  rename(seq_id = "Sequential ID") %>% 
  filter(!(seq_id == 224)) %>%
  filter(!is.na(Longitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs=ll_wgs84) %>%
  st_transform(crs=albers)

plot(st_geometry(grid_bound), col=NA, border="red",
     main="Potenital community monitor sites")
plot(st_geometry(mon), add=T)

#' ID the city each of these locations is in
cities <- st_read(here::here("Data/Raw_Geo_Data", "Munibounds.shp")) %>%
  st_transform(crs=albers)

mon <- st_join(mon, cities[,"first_city"])

st_write(mon, here::here("Data/HS_Data", "All Community Monitors.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' New potenital sites for the Fall campaign (LEAD volunteers)
new_mon_xl <- read_xlsx(here::here("Data/Raw_Geo_Data", "Fall Campaign New Sites.xlsx")) %>% 
  filter(!is.na(Address))

new_mon_geo <- mutate_geocode(new_mon_xl, Address, source = "dsk")
new_mon <- filter(new_mon_geo, !is.na(lon1)) %>% 
  st_as_sf(coords = c("lon1", "lat1"), crs = 4326)
plot(st_geometry(new_mon))

#' -----------------------------------------------------------------------------
#' 3) Mapping monitor locations
#' -----------------------------------------------------------------------------

#' map monitor locations
bound_ll <- st_transform(grid_bound, crs=ll_wgs84)
mon_ll <- st_transform(mon, crs=ll_wgs84)
hs_ll <- st_transform(hs, crs=ll_wgs84)

#' base map for the area
base_map <- get_map(location=unname(st_bbox(bound_ll)), maptype="roadmap",
                    zoom = 10, source="google")

ggmap(base_map) +
  ggtitle("Monitor locations (participants jittered)") +
  geom_sf(data=st_sf(bound_ll), 
          inherit.aes = F, color="black", fill=NA, size=1) +
  geom_sf(data=mon_ll, 
          inherit.aes = F, aes(color="mon", fill="mon"), pch=20, size=3) +
  geom_sf(data=st_jitter(hs, factor=0.02), 
          inherit.aes = F, aes(color="hs", fill="hs"), pch=20, size=3) +
  scale_color_manual(name="Monitor Type",
                     values = c("mon" = "blue", "hs" = "red"),
                     labels = c("mon" = "Community", "hs" = "Participant")) +
  scale_fill_manual(name="Monitor Type",
                     values = c("mon" = "blue", "hs" = "red"),
                     labels = c("mon" = "Community", "hs" = "Participant")) +
  theme(legend.position = c(0.9, 0.9)) +
  coord_sf(ylim=c(39.5, 40.05)) +
  simple_theme
ggsave(here::here("Figs", "Proposed Participant and Community Locations.jpeg"), 
       device="jpeg")

#' Dropping extra participants
hs_sub <- hs %>%
  filter(!(seq_id %in% drop))

ggmap(base_map) +
  ggtitle("Monitor locations (participants jittered)") +
  geom_sf(data=st_sf(bound_ll), 
          inherit.aes = F, color="black", fill=NA, size=1) +
  geom_sf(data=mon_ll, 
          inherit.aes = F, aes(color="mon", fill="mon"), pch=20, size=3) +
  geom_sf(data=st_jitter(hs_sub, factor=0.02), 
          inherit.aes = F, aes(color="hs", fill="hs"), pch=20, size=3) +
  scale_color_manual(name="Monitor Type",
                     values = c("mon" = "blue", "hs" = "red"),
                     labels = c("mon" = "Community", "hs" = "Participant")) +
  scale_fill_manual(name="Monitor Type",
                    values = c("mon" = "blue", "hs" = "red"),
                    labels = c("mon" = "Community", "hs" = "Participant")) +
  theme(legend.position = c(0.9, 0.9)) +
  coord_sf(ylim=c(39.5, 40.05)) +
  simple_theme
ggsave(here::here("Figs", "Proposed Participant and Community Locations.jpeg"), 
       device="jpeg")


#' -----------------------------------------------------------------------------
#' 4) Identify which grid cells contain a community monitor or participant
#' -----------------------------------------------------------------------------

#' Identify HS grid cells
#' Dropping extra participants
hs_sub <- hs %>%
  filter(!(seq_id %in% drop))

grid_hs_ids <- st_join(grid_250, hs_sub) %>%
  filter(!(is.na(seq_id))) %>%
  select(grid_id)

#' Identify community monitor grid cells
grid_mon_ids <- st_join(grid_250, mon) %>%
  filter(!(is.na(seq_id))) %>%
  select(grid_id)

grid_ids <- c(grid_hs_ids$grid_id, grid_mon_ids$grid_id)

grid_mon<- grid_250 %>%
  mutate(hs = ifelse(grid_id %in% grid_hs_ids$grid_id, 1, 0),
         cm = ifelse(grid_id %in% grid_mon_ids$grid_id, 1, 0),
         mon = ifelse(grid_id %in% grid_ids, 1, 0))

#' Check
sum(grid_mon$hs)
sum(grid_mon$cm)
sum(grid_mon$mon)

plot(grid_mon[,"mon"])

st_write(hs, here::here("Data", "Grid 250 m with monitors.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

