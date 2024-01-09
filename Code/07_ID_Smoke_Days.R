#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Identify "smoke impacted" days
#' Date Created: November 19, 2018
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' -----------------------------------------------------------------------------

#' Load required libraries
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(sf)
library(ggplot2)
library(ggthemes)
library(rvest)
library(Hmisc)

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

#' Coordinate reference systems 
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' -----------------------------------------------------------------------------
#' 1) Calculate long-term (2009-2019) averages by month and monitor

years <- c(2009:2019)
time_zone <- "America/Denver"

#' read in all data, summarize to long-term monthly averages
monitor_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) 

monitor_sp_data <- select(monitor_data, monitor_id, WKT)
monitor_sp_data <- unique(monitor_sp_data)

monitor_data <- monitor_data %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(month = month(Date_Local),
         year = year(Date_Local)) 

monitor_data_10yr <- filter(monitor_data, year %in% years)

monitor_means <- monitor_data_10yr %>% 
  group_by(monitor_id, month) %>% 
  summarize(monthly_mean = mean(Arithmetic_Mean, na.rm=T),
            monthly_sd = sd(Arithmetic_Mean, na.rm=T))

ggplot(monitor_means) +
  geom_line(aes(x = month, y = monthly_mean, col=factor(monitor_id))) +
  simple_theme

#' -----------------------------------------------------------------------------
#' 2) Determine if each monitor overlaps with plume (within 50 km) and has a
#' concentration 1 SD or 2 SD higher than the long term monthly average

#' loop through dates
smoke_days <- data.frame()

#' unique monitors
monitors <- unique(monitor_data$monitor_id)

#' what buffer do we want to consider?
#' AEA units are meters
buff_distance <- 50000

for(i in 1:length(monitors)) {
  
  print(paste(i, "of", length(monitors), "monitor locations"))
  
  #' A df with just one monitor
  mon_df <- filter(monitor_data, monitor_id == monitors[i])
  
  #' Dates for which we have PM data at that monitor
  pm_dates <- gsub("-", "", as.character(unique(mon_df$Date_Local)))
  
  for(j in 1:length(pm_dates)) {
    smoke_name <- paste0("hms_smoke", pm_dates[j], "_AEA.csv")
    
    #' If there's no shapefile, make each metric NA
    if(!file.exists(here::here("Data/Smoke_Data", smoke_name))) {
      temp <- data.frame(date = pm_dates[j],
                         monitor = monitors[i],
                         daily_mean_pm = NA,
                         plume_within_50km = NA,
                         pm_exceeds_mean_1sd = NA,
                         pm_exceeds_mean_2sd = NA)
      smoke_days <- bind_rows(smoke_days, temp)
      rm(temp)
      
    } else {
      plumes <- read_csv(here::here("Data/Smoke_Data", smoke_name), 
                         col_types = cols()) 
      
      if (nrow(plumes) == 0) {
        temp <- data.frame(date = pm_dates[j],
                           monitor = monitors[i],
                           daily_mean_pm = NA,
                           plume_within_50km = NA,
                           pm_exceeds_mean_1sd = NA,
                           pm_exceeds_mean_2sd = NA)
        smoke_days <- bind_rows(smoke_days, temp)
        rm(temp, plumes)
        
      } else {
        plumes_sf <- st_as_sf(plumes, wkt = "WKT", crs = albers)
        plumes_sf <- filter(plumes_sf, !is.na(st_is_valid(plumes_sf)))
        
        monitors_temp <- filter(mon_df, 
                                Date_Local == as.Date(pm_dates[j], format = "%Y%m%d")) 
        
        monitor_sf <- st_as_sf(monitors_temp, wkt = "WKT", crs = albers) %>%
          summarize(Arithmetic_Mean = mean(Arithmetic_Mean, na.rm = T))
        
        monitor_buffer <- st_buffer(monitor_sf, dist = buff_distance)
        
        #' Do any of the plumes intersect with the 50 km buffer?
        plume_intersect <- any(st_intersects(monitor_buffer, plumes_sf, sparse = F))
        
        #' is the daily mean higher than 1 sd above the long-term average?
        #' is the daily mean higher than 2 sd above the long-term average?
        long_term_mean <- filter(monitor_means, monitor_id == monitors[i] & 
                                   month == month(as.Date(pm_dates[j], format = "%Y%m%d")))
        monthly_metric_1sd <- long_term_mean$monthly_mean[1] + (1*long_term_mean$monthly_sd[1])
        monthly_metric_2sd <- long_term_mean$monthly_mean[1] + (2*long_term_mean$monthly_sd[1])
        
        pm_exceeds_1sd <- monitor_sf$Arithmetic_Mean >= monthly_metric_1sd
        pm_exceeds_2sd <- monitor_sf$Arithmetic_Mean >= monthly_metric_2sd
        
        temp <- data.frame(date = pm_dates[j],
                           monitor = monitors[i],
                           daily_mean_pm = monitor_sf$Arithmetic_Mean,
                           plume_within_50km = plume_intersect,
                           pm_exceeds_mean_1sd = pm_exceeds_1sd,
                           pm_exceeds_mean_2sd = pm_exceeds_2sd)
        smoke_days <- bind_rows(smoke_days, temp)
        rm(temp, plumes, plumes_sf)
      }
    }
  }
}

mutate_smoke_days <- smoke_days %>% 
  mutate(smoke_day_1sd = plume_within_50km * pm_exceeds_mean_1sd,
         smoke_day_2sd = plume_within_50km * pm_exceeds_mean_2sd,
         Date_Local = as.Date(date, format = "%Y%m%d")) %>% 
  rename(monitor_id = "monitor")
table(mutate_smoke_days$plume_within_50km, mutate_smoke_days$pm_exceeds_mean_1sd)
table(mutate_smoke_days$plume_within_50km, mutate_smoke_days$pm_exceeds_mean_2sd)

monitor_smoke_days <- left_join(mutate_smoke_days, monitor_sp_data,
                                by = "monitor_id")

write_csv(monitor_smoke_days, here::here("Data", "Monitor_Smoke_Days_AEA.csv"))

#' -----------------------------------------------------------------------------
#' Spot check 1% of days
#' -----------------------------------------------------------------------------

years <- c(2009:2020)

monitor_smoke_days <- read_csv(here::here("Data", "Monitor_Smoke_Days_AEA.csv")) %>%
  st_as_sf(wkt = "WKT", crs = albers)

random_dates <- sample(unique(monitor_smoke_days$Date_Local), 
                       length(unique(monitor_smoke_days$Date_Local)) * 0.01)
sample_days <- filter(monitor_smoke_days, Date_Local %in% random_dates)

table(sample_days$plume_within_50km, sample_days$pm_exceeds_mean_1sd)

#' Which monitors/dates are smoke-impacted?
smoke_impacted <- filter(sample_days, smoke_day_1sd == T)
as.Date(smoke_impacted$Date_Local, format = "%Y%m%d")

#' monitor data
monitor_sf <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(month = month(Date_Local),
         year = year(Date_Local)) %>% 
  filter(year %in% years)

for (i in 1:length(random_dates)) {
  check_date_name <- paste0("hms_smoke", 
                            gsub("-", "", as.character(random_dates[i])),
                            "_AEA.csv")
  
  check_plumes <- read_csv(here::here("Data/Smoke_Data", check_date_name)) %>% 
    st_as_sf(wkt = "WKT", crs = albers) %>% 
    filter(!is.na(st_is_valid(.)))
  
  check_monitor <- filter(monitor_sf, Date_Local == as.Date(random_dates[i],
                                                            format = "%Y%m%d"))
  check_monitor_buffer <- st_buffer(check_monitor, dist = 50000)
  
  #' plot monitors and plumes
  ggplot() +
    ggtitle(paste0("Smoke plumes on ", 
                   as.Date(random_dates[i], format = "%Y%m%d"))) +
    geom_sf(data = check_monitor, color = "blue", size = 2, shape = 16) + 
    geom_sf(data = check_monitor_buffer, color = "blue", fill = NA) +
    geom_sf(data = check_plumes, color = "red", fill = NA) +
    simple_theme
  
  plot_name <- paste0("Smoke_Day_Check_", gsub("-", "", as.character(random_dates[i])), ".jpeg")
  ggsave(here::here("Figs/Smoke_Check", plot_name), device = "jpeg")
}

#' -----------------------------------------------------------------------------
#' Check "missing" PM2.5 Days
#' In 2012, there is a 6 week period where the PM2.5 monitoring network did not
#' report data (thanks to a QA/QC issue)
#' I'm checking these days to see if there were smoke plumes in the area
#' No smoke plumes means I can set smoke == 0 in the prediction model
#' -----------------------------------------------------------------------------

counties <- c("001", "005", "013", "014", "031", "059")

#' Monitor data
monitor_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) 

monitor_sp_data <- select(monitor_data, monitor_id, WKT)
monitor_sp_data <- unique(monitor_sp_data) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>%
  mutate(county = str_sub(monitor_id, 3, 5)) %>%
  filter(county %in% counties)
plot(st_geometry(monitor_sp_data)) 

monitor_buffer <- st_buffer(monitor_sp_data, dist = 50000)

dates <- seq.Date(as.Date("2012-02-15"), by = "3 days", length = 18)
dates

plume_check <- tibble()

for (i in 1:length(dates)) {
  check_date_name <- paste0("hms_smoke", 
                            gsub("-", "", as.character(dates[i])),
                            "_AEA.csv")
  
  check_plumes <- read_csv(here::here("Data/Smoke_Data", check_date_name)) %>% 
    st_as_sf(wkt = "WKT", crs = albers) %>% 
    filter(!is.na(st_is_valid(.)))
  
  check_intersects <- st_intersects(monitor_buffer, check_plumes, sparse = F)
  check_n_plumes <- rowSums(check_intersects)
  
  temp <- tibble(date = dates[i],
                 monitor_id = monitor_buffer$monitor_id,
                 n_plumes = check_n_plumes)
  
  plume_check <- bind_rows(plume_check, temp)
  rm(temp)
}

plume_check

#' There appear to be two days when small plumes intersect with the monitor buffers
plume_check_1 <- filter(plume_check, n_plumes > 0)

mar10_plumes <- read_csv(here::here("Data/Smoke_Data", "hms_smoke20120310_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  filter(!is.na(st_is_valid(.)))
mar22_plumes <- read_csv(here::here("Data/Smoke_Data", "hms_smoke20120322_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  filter(!is.na(st_is_valid(.)))

#' Plotting these plumes
plot(st_geometry(mar10_plumes), border = "red")
plot(st_geometry(mar22_plumes), add= T, border = "blue")
plot(st_geometry(monitor_buffer), add = T)

#' The NOAA site does not make note of these as significant smoke plumes
#' See: https://www.ssd.noaa.gov/PS/FIRE/2012_archive_smoke.html

#' The 2sd above monthly mean threshold for these monitors are:
thresholds <- monitor_means %>%
  mutate(threshold_1sd = monthly_mean + 1*monthly_sd,
         threshold_2sd = monthly_mean + 2*monthly_sd)
thresholds

#' The concentrations reported for these monitors on the CDPHE website (which
#' are not QC'ed and my not be correct) do not exceed the thresholds used to 
#' calculate the smoke metric. 
pc_list <- c(88101)
pc_names <- c("88101")
dates2 <- as.character(format(dates, "%m%d%Y"))
pol <- 1

if(!dir.exists(here::here("Data"))) dir.create(here::here("Data"))
if(!dir.exists(here::here("Data/Temp"))) dir.create(here::here("Data/Temp"))

output <- data.frame()
  
for (i in 1:length(dates2)) {
  m <- substr(dates2[i],1,2)
  d <- substr(dates2[i],3,4)
  y <- substr(dates2[i],5,8)
    
  url <- paste("https://www.colorado.gov/airquality/param_summary.aspx?",
               "parametercode=", pc_list[pol], "&seeddate=", m, "%2f", d, "%2f", y,
               "&export=False", sep="")
    
  try_dl <- tryCatch(
    download.file(url, destfile = here::here("Data/Temp", "cdphe_temp.html")),
    error = function(e) e
  )
    
  if(!inherits(try_dl, "error")){
    #' original HTML includes breaks that are not preserved by html_table      
    #' #' need to download the data, substitute the breaks, and then get the data
    #' See: https://stackoverflow.com/questions/30989543/r-scraping-an-html-
    #' table-with-rvest-when-there-are-missing-tr-tags
      
    download.file(url, destfile = here::here("Data/Temp", "cdphe_temp.html"))
    ap_html <- readChar(here::here("Data/Temp", "cdphe_temp.html"),
                        file.info(here::here("Data/Temp", "cdphe_temp.html"))$size)
    ap_html <- gsub("<br />", "_", ap_html)
    ap_data <- read_html(ap_html)
      
    nodes <- html_nodes(ap_data, xpath = "//table")
    table <- html_table(nodes)[[3]]
      
    #' Clean up the table
    colnames(table) <- table[1,] #' column names are in first and last rows
    table <- table[-c(1, nrow(table)-1, nrow(table)),] #' drop unnecessary rows
      
    #' For ozone, PM, and NO2 there is a "metric key" column because there are actually
    #' two/three values reported for each monitor and hour (depending on pollutant)
    #' you can either keep the multiple metrics (first block) or extract the 
    #' 1-hour measurement (second block)
      
    #' keep all measurements for PM2.5, ozone, or NO2
    # if(pc_names[pol] %in% c("88101", "44201")) {
    #   colnames(table)[2] <- "metric_key"
    # }
      
    #' Just keep the one-hour measurements (first of the three)
    if(pc_names[pol] %in% c("88101", "44201", "42602")) {
      table <- table[,-c(2)]
      table_long <- pivot_longer(table, names_to = "monitor", values_to = "metrics", 
                                 -c(contains("MST"))) %>%
        mutate(metrics = as.list(str_split(metrics, "_"))) 
        table_long$one_hr <- as.vector(as.numeric(lapply(table_long$metrics, `[[`, 1))) 
        table <- pivot_wider(table_long, names_from = "monitor", values_from = "one_hr",
                             contains("MST"))
        rm(table_long)
    }
      
    #' adding a date stamp and a parameter code to match the EPA data sets
    table$Date_Local <- dates2[i]
    table$Parameter_Code <- pc_list[pol]
      
    #' append to data frame
    output <- bind_rows(output, table)      
    print(dates2[i])
    rm(table)
    file.remove(here::here("Data/Temp", "cdphe_temp.html"))
  }
}
  
#' remove symbols from the column names
colnames(output) <- gsub("\\*\\*", "", colnames(output))
colnames(output)[1] <- c("hour_MST")

glimpse(output)

monitor_url <- "https://www.colorado.gov/airquality/site_description.aspx"

monitors <- read_html(monitor_url)
monitor_data_html <- html_nodes(monitors, "p")
monitor_data_text <- as.data.frame(html_text(monitor_data_html))

monitor_data_text[,1] <- as.character(monitor_data_text[,1])
colnames(monitor_data_text) <- "monitor_info"

#' Extract monitor_id, lon, and lat
monitor_info <- monitor_data_text %>%
  rowwise %>%
  mutate(monitor_loc = str_extract_all(monitor_info, "\\([^()]+\\)")[[1]][1]) %>%
  mutate(monitor_loc = gsub("\\(", "", monitor_loc)) %>%
  mutate(monitor_loc = gsub("\\)", "", monitor_loc)) %>%
  mutate(monitor_id = str_sub(str_split_fixed(monitor_info, "AQS ID: ", 2)[2],
                              start = 1, end = 9)) %>%
  mutate(lon = as.numeric(str_sub(str_split_fixed(monitor_info, "Longitude: ", 2)[2],
                                  start = 1, end = 11))) %>%
  mutate(lat = as.numeric(str_sub(str_split_fixed(monitor_info, "Latitude: ", 2)[2],
                                  start = 1, end = 9)))

monitor_info

cdphe_data <- output %>%
  pivot_longer(cols = -c(hour_MST, Date_Local, Parameter_Code),
               names_to = "monitor_loc", values_to = "pm") %>%
  left_join(monitor_info, by = "monitor_loc") %>%
  mutate(Date_Local = as.Date(Date_Local, format = "%m%d%Y")) %>%
  group_by(monitor_id, Date_Local) %>%
  dplyr::summarize(Arithmetic_Mean = mean(pm, na.rm = T)) %>%
  mutate(county = str_sub(monitor_id, 3, 5)) %>%
  filter(county %in% counties) %>%
  mutate(month = month(Date_Local)) %>%
  left_join(thresholds, by = c("monitor_id", "month")) %>%
  mutate(pm_exceeds_mean_1sd = ifelse(Arithmetic_Mean > threshold_1sd, 1, 0),
         pm_exceeds_mean_2sd = ifelse(Arithmetic_Mean > threshold_2sd, 1, 0)) %>%
  filter(!is.na(monthly_mean))
  
cdphe_data
table(cdphe_data$pm_exceeds_mean_1sd)
table(cdphe_data$pm_exceeds_mean_2sd)

#' Caluclate the same metrics as above
#' Because the plumes are very small during this window and because the CDPHE 
#' monitoring data generally does not indicate that the 2SD threshold was exceeded, 
#' add these data to the smoke plume data set to make sure we have complete 
#' data for predictions
plume_check2 <- plume_check %>%
  mutate(Date_Local = as.Date(date)) %>% select(-date) %>%
  mutate(plume_within_50km = ifelse(n_plumes > 0, 1, 0)) %>%
  select(-n_plumes)
table(plume_check2$plume_within_50km)
glimpse(plume_check2)

glimpse(cdphe_data)

monitor_smoke_days_update <- cdphe_data %>%
  left_join(plume_check2, by = c("monitor_id", "Date_Local")) %>%
  filter(!is.na(Arithmetic_Mean)) %>%
  select(monitor_id, daily_mean_pm = Arithmetic_Mean, plume_within_50km,
         pm_exceeds_mean_1sd, pm_exceeds_mean_2sd, Date_Local) %>%
  mutate(smoke_day_1sd = plume_within_50km * pm_exceeds_mean_1sd,
         smoke_day_2sd = plume_within_50km * pm_exceeds_mean_2sd) %>%
  left_join(monitor_sp_data, by = "monitor_id") %>%
  mutate(date = as.character(format(Date_Local, "%Y%m%d")))
glimpse(monitor_smoke_days_update) 
summary(monitor_smoke_days_update)

table(monitor_smoke_days_update$smoke_day_1sd)
table(monitor_smoke_days_update$smoke_day_2sd) # This is the variable in the model

#' Tack these data on the to "monitor smoke days" object from above
#' First, make sure these dates are missing in the original file
monitor_smoke_days <- read_csv(here::here("Data", "Monitor_Smoke_Days_AEA.csv")) %>%
  mutate(date = as.character(date))
glimpse(monitor_smoke_days)
table(monitor_smoke_days$smoke_day_2sd)

#' Should be nrow(date_check) == 0
date_check <- filter(monitor_smoke_days, Date_Local %in% dates) %>%
  filter(monitor_id %in% monitor_smoke_days_update$monitor_id)
nrow(date_check)

monitor_smoke_days2 <- bind_rows(monitor_smoke_days, monitor_smoke_days_update) %>%
  arrange(monitor_id, Date_Local)
table(monitor_smoke_days2$smoke_day_2sd)

write_csv(monitor_smoke_days2, here::here("Data", "Monitor_Smoke_Days_Updated_AEA.csv"))
