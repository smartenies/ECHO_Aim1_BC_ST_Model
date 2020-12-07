#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 Land Use Regression
#' 
#' Task: Scrape recent ozone data from the AQS monitors in the Denver metro
#' area
#' 
#' Author: Sheena Martenies
#' Date Created: May 3, 2018
#' Contact: sheena.martenies@colostate.edu
#' -----------------------------------------------------------------------------

#' Load required libraries
library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(tidyverse)
library(readxl)
library(rvest)
library(XML)
library(htmltab)
library(writexl)

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

#' -----------------------------------------------------------------------------
#' 1) Scrape data from CDPHE website (most recent data)
#' -----------------------------------------------------------------------------

start_date <- as.Date("2016-01-01")
#end_date <- as.Date("2018-04-23")
end_date <- Sys.Date()
dates <- seq(start_date, end_date, by="1 day")
dates <- format(dates, "%m%d%Y")

output <- data.frame()

#' Parameter code
#' PM2.5 (Local Conditions) = 88101
#' Ozone = 44201 
pc <- 44201

for (i in 657:length(dates)) {
  m <- substr(dates[i],1,2)
  d <- substr(dates[i],3,4)
  y <- substr(dates[i],5,8)
  
  url <- paste("https://www.colorado.gov/airquality/param_summary.aspx?",
               "parametercode=", pc, "&seeddate=", m, "%2f", d, "%2f", y,
               "&export=False", sep="")
  
  #cdphe <- read_html(url)
  #nodes <- html_nodes(cdphe, xpath = "//table")
  
  #' orginal HTML includes breaks that are not preserved by html_table
  #' need to download the data, substitute the breaks, and then get the data
  #' See: https://stackoverflow.com/questions/30989543/r-scraping-an-html-
  #' table-with-rvest-when-there-are-missing-tr-tags
  
  download.file(url, destfile = "./Temp/temp.html")
  ap_html <- readChar("./Temp/temp.html", file.info("./Temp/temp.html")$size)
  ap_html <- gsub("<br />", "-", ap_html)
  ap_data <- read_html(ap_html)
  
  nodes <- html_nodes(ap_data, xpath = "//table")
  table <- html_table(nodes)[[3]]
  
  #' Clean up the table
  colnames(table) <- table[1,] #' column names are in first and last rows
  colnames(table)[2] <- "metric_key"
  table <- table[-c(1, nrow(table)-1, nrow(table)),] #' drop unnecessary rows
  
  table$date <- dates[i]
  write.table(table, 
              file=paste("./AQS_Data/Daily_Data/", pc, "_", y, m, d, ".txt", sep=""),
              row.names=F)

  #' append to data frame
  output <- bind_rows(output, table)
  
  print(dates[i])
  rm(table)
  if (file.exists("./Temp/temp.html")) file.remove("./Temp/temp.html")
}

colnames(output) <- gsub("\\*\\*", "", colnames(output))
colnames(output)[1:2] <- c("hour_MST", "metric_key")

write.table(output, file=paste("./AQS_Data/", pc, "_",
                               as.character(start_date), "_", as.character(end_date), 
                               ".txt", sep=""),
            row.names = F, sep=",")
rm(output)
#' -----------------------------------------------------------------------------
#' 2) Summarize design values at each monitor
#' -----------------------------------------------------------------------------

o3 <- read.table(paste("./AQS_Data/", pc, "_",
                       as.character(start_date), "_", as.character(end_date), 
                       ".txt", sep=""),
                 stringsAsFactors = F, header=T, sep=",")

o3 <- o3 %>%
  mutate(date = as.Date(str_pad(date, 8, pad="0", side="left"), 
                        format="%m%d%Y", tz="MST")) %>%
  mutate(day = group_indices(.,date)) %>%
  group_by(date) %>%
  mutate(hour = seq_along(hour_MST)) %>%
  select(date, hour_MST, day, hour, metric_key, everything())
  


o3_wide <- temp
o3_wide <- o3_wide[,c(1,25:29,2:24)]
colnames(o3_wide) <- gsub("Sample.Measurement", "O3", colnames(o3_wide))

rm(temp)

#' caluclate the max 8-hour average for each day
#' See FR Vol 80 No 206 Pg 65459 for definition of the design value
o3_d8hmax <- data.frame(date=unique(o3_wide[,c("date")]))
o3_d8hmax$date <- as.character(o3_d8hmax$date)
o3_d8hmax$day <- seq(1:nrow(o3_d8hmax))
o3_d8hmax$month <- substr(o3_d8hmax$date, 6, 7)
o3_d8hmax$year <- substr(o3_d8hmax$date, 1, 4)
day_list <- unique(o3_wide$day)

monitor_ids2 <- paste("O3", monitor_ids, sep=".")
col_ids <- colnames(o3_wide)[which(!(colnames(o3_wide) %in% monitor_ids2))]

n <- length(col_ids)

for (i in 1:(ncol(o3_wide)-n)) {
  name <- colnames(o3_wide)[i+n]
  temp_df <- data.frame(day = as.numeric(),
                        max = as.numeric())
  
  for (j in 1:length(day_list)) {
    days <- c(day_list[j], day_list[j+1])
    df <- o3_wide[which(o3_wide$day %in% days), c(3, 4, i+n)]
    df$hour2 <- -1 + seq(1:nrow(df))
    
    avg_list <- list()
    a <- 16 #should have 17 moving averages starting at 7:00am and ending at 11:00pm
    
    for (k in 7:(7+a)) {
      hours <- seq(from=k, to=k+7)
      conc<- df[which(df$hour2 %in% hours),3]
      #' need at least 6 hourly concentrations to calculate mean
      avg_list[k-6] <- ifelse(sum(!is.na(conc)) >= 6, mean(conc, na.rm=T), NA)
    }
    
    b <- unlist(avg_list)
    
    #' only valid if there are >= 13 8-hr means
    max <- ifelse(sum(!is.na(b)) >= 13, max(b, na.rm=T), NA)
    
    temp <- data.frame(day = days[1], max = max)
    temp_df <- rbind(temp_df, temp)
  }
  o3_d8hmax <- merge(o3_d8hmax, temp_df, by="day")
  colnames(o3_d8hmax)[ncol(o3_d8hmax)] <- name
  print(name)
}

save(o3, o3_d8hmax, file="./Data/Air Quality/ozone.RData")


