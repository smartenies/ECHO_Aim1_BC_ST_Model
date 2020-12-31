#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Scrape the EPA AQS air quality and meteorology data
#' Date created: October 25, 2018
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' NOTE: Due to some issues with the API, going to just download the summary
#' files from the EPA website and subset to the locations we need in the 
#' cleaning script
#' 
#' Updated 11.24.20: Transferring code and data to UIUC computer
#' ----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(rvest)
library(Hmisc)

if(!dir.exists(here::here("Data"))) dir.create(here::here("Data"))
if(!dir.exists(here::here("Data/Temp"))) dir.create(here::here("Data/Temp"))

years <- c(2009:2020)
met_vars <- c("WIND", "PRESS", "TEMP", "RH_DP")

aqs_path <- here::here("Secondary_Data", "EPA_Air_Quality_System_Data")

for (i in 1:length(years)) {
  
  #' Daily PM2.5
  aqs_url <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_88101_", 
                    years[i], ".zip")
  
  download.file(aqs_url, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
  
  #' Daily met variables
  for (j in 1:length(met_vars)) {
    met_url <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_", met_vars[j],  
                      "_", years[i], ".zip")
    
    #' Download zipfile from the EPA website and unzip
    download.file(met_url, destfile = here::here("Data/Temp", "temp.zip"))
    unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
  }
  
  #' daily mean ozone data
  aqs_url2 <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_44201_", 
                     years[i], ".zip")
  
  download.file(aqs_url2, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
  
  #' daily 8h max ozone data
  aqs_url3 <- paste0("https://aqs.epa.gov/aqsweb/airdata/8hour_44201_", 
                     years[i], ".zip")
  
  download.file(aqs_url3, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
  
  #' PM2.5 speciation data
  aqs_url4 <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_SPEC_", 
                    years[i], ".zip")
  
  download.file(aqs_url4, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
  
  #' Daily NO2
  aqs_url5 <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_42602_", 
                     years[i], ".zip")
  
  download.file(aqs_url5, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
}


#' -----------------------------------------------------------------------------
#' Also need most recent air pollution, temp, RH, and wind data
#' from CDPHE website, since AQS data is delayed by at least a quarter
#' -----------------------------------------------------------------------------

start_date <- as.Date("2016-01-01")
end_date <- Sys.Date()
dates <- seq(start_date, end_date, by="1 day")
dates <- format(dates, "%m%d%Y")

#' Parameter codes
#' Information on these parameter codes is available from EPA:
#' https://aqs.epa.gov/aqsweb/documents/codetables/methods_all.html
#' You can also deduce what they are based on the URLs for the specific 
#' pollutants on the CDPHE website
#' 
#' 88101: PM2.5
#' 44201: O3
#' 42602: NO2
#' 62101: Temp
#' 62201: Relative Humidity
#' 61101: Wind Speed

# pc_list <- c(88101, 44201, 42602, 62101, 62201, 61101)
# pc_names <- c("88101", "44201", "42602", "TEMP", "RH_DP", "WIND")

pc_list <- c(88101, 44201, 42602, 62101)
pc_names <- c("88101", "44201", "42602", "TEMP")

if(!dir.exists(here::here("Data"))) dir.create(here::here("Data"))
if(!dir.exists(here::here("Data/Temp"))) dir.create(here::here("Data/Temp"))

for (pol in 1:length(pc_list)) {
  output <- data.frame()
  
  for (i in 1:length(dates)) {
    m <- substr(dates[i],1,2)
    d <- substr(dates[i],3,4)
    y <- substr(dates[i],5,8)
    
    url <- paste("https://www.colorado.gov/airquality/param_summary.aspx?",
                 "parametercode=", pc_list[pol], "&seeddate=", m, "%2f", d, "%2f", y,
                 "&export=False", sep="")
    
    try_dl <- tryCatch(
      download.file(url, destfile = here::here("Data/Temp", "cdphe_temp.html")),
      error = function(e) e
    )
    
    if(!inherits(try_dl, "error")){
      #' original HTML includes breaks that are not preserved by html_table
      #' need to download the data, substitute the breaks, and then get the data
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
      table$Date_Local <- dates[i]
      table$Parameter_Code <- pc_list[pol]
      
      #' append to data frame
      output <- bind_rows(output, table)
      
      print(dates[i])
      rm(table)
      file.remove(here::here("Data/Temp", "cdphe_temp.html"))
    }
  }
  
  #' remove symbols from the column names
  colnames(output) <- gsub("\\*\\*", "", colnames(output))
  colnames(output)[1] <- c("hour_MST")
  
  output_name <- paste0(pc_names[pol], "_CDPHE.csv")
  write_csv(output, here::here("Secondary_Data", "CDPHE_Air_Quality_Data", output_name))
}

#' -----------------------------------------------------------------------------
#' Monitor data from the CDPHE website
#' -----------------------------------------------------------------------------

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

monitor_file_name <- "CDPHE_Monitors.csv"
write_csv(monitor_info, here::here("Secondary_Data", "CDPHE_Air_Quality_Data", monitor_file_name))