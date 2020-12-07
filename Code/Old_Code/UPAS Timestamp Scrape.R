#' -----------------------------------------------------------------------------
#' Project: UPAS Metadata Scrape
#' Date created: May 29, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This code examines the UPAS montior metadata timestamps. The first field
#' campaign data had issues with "time drift", so we want to see what's going
#' on with the metadata
#' -----------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(writexl)
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
  legend.position = c(0.2,0.9),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

#' Idenitfy the directory with the UPAS data files
#' Change "upas_home" before each analysis
#' Change the output path if you want to specifiy where to put the summary files

upas_home <- "T:/Rsch-MRS/ECHO/Campaign Data/Campaign1/Week3/data logs/"
output_path <- "T:/Rsch-MRS/ECHO/Campaign Data/Campaign1/Week3/"
week <- "3"

#' Need to create an output folder
if(!(exists(paste(output_path, "Timestamp Plots", sep="")))) {
  dir.create(paste(output_path, "Timestamp Plots", sep=""))
}
plot_fldr <- paste(output_path, "Timestamp Plots/", sep="")

#' Get a list of the files in the upas_home folder
upas <- list.files(path=upas_home, full.names = T, recursive = F)
length(upas)

#' Empty data frame for metadata
metadata <- data.frame()

#' Open each file, extract the relevant metadata
for (i in 1:length(upas)) {
  upas_name <- read.table(upas[i], stringsAsFactors = F, nrows = 1, 
                          sep=",", header=T) %>%
    select(2)
  
  meta <- read.table(upas[i], stringsAsFactors = F, skip = 59,
                     sep=",", header=T) %>%
    mutate(serial_number = upas_name[1,1])
  
  metadata <- bind_rows(metadata, meta)
  rm(upas_name, meta)
}

#' Save the full metadata df and the smaller runtimes one
#' Feel free to add other variables here!
write_xlsx(metadata,
           path=paste(output_path, "UPAS Timestamps.xlsx", sep=""))

#' Plot the time stamps to see where they are different
#' MDT is 6 hours behind UTC
metadata <- read_excel(path = paste(output_path, "UPAS Timestamps.xlsx", sep=""))
upas <- unique(metadata$serial_number)

time_stamp_data <- data.frame()

for (i in 1:length(upas)) {
  meta <- filter(metadata, serial_number == upas[i])
  interval <- as.numeric(substr(meta$SampleTime[2], 6, 7)) - as.numeric(substr(meta$SampleTime[1], 6, 7))
  
  timestamps <- meta %>%
    select(serial_number, SampleTime, UnixTime, DateTimeUTC, DateTimeLocal) %>%
    mutate(DateTimeUTC = gsub("T", " ", DateTimeUTC),
           DateTimeLocal = gsub("T", " ", DateTimeLocal)) %>%
    mutate(DateTimeUTC = as.POSIXct(DateTimeUTC, tz="GMT", format="%Y-%m-%d %H:%M:%S"),
           DateTimeLocal = as.POSIXct(DateTimeLocal, 
                                      tz="America/Denver", format="%Y-%m-%d %H:%M:%S"),
           
           #' POSIXct uses seconds, so add 21600 seconds to get UTC from local time
           Local_to_UTC = DateTimeLocal + (6*60*60),
           elapsed_time_s = ((as.numeric(rownames(.)) - 1) * interval)) %>%
    mutate(elapsed_time_h = elapsed_time_s / (60*60))
  
  
  utc_lm <- lm(elapsed_time_s ~ DateTimeUTC, timestamps)
  utc_slope <- summary(utc_lm)$coefficients[2]
  
  local_lm <- lm(elapsed_time_s ~ DateTimeLocal, timestamps)
  summary(local_lm)
  local_slope <- summary(local_lm)$coefficients[2]
  
  ggplot(timestamps) +
    ggtitle(paste("Week:", week, "UPAS Serial No.", upas[i])) +
    geom_line(aes(x=DateTimeUTC, y=elapsed_time_h, col="utc"), size = 3) +
    geom_line(aes(x=DateTimeLocal, y=elapsed_time_h, col="local"), size = 1) +
    scale_color_manual(name= "Time Stamp TZ",
                       values = c("utc" = "red", "local" = "blue"),
                       labels = c("utc" = paste("UTC (slope = ", utc_slope, ")"), 
                                  "local" = paste("Local (slope =", local_slope, ")"))) +
    xlab("Time Stamp") + ylab("Elapsed sample time (h)") +
    simple_theme
  
  ggsave(filename = paste(plot_fldr, upas[i], " Elapsed vs Timestamp.jpeg", sep=""),
         device = "jpeg", dpi=400)
  
  
  time_correlation <- cor(as.numeric(timestamps$DateTimeUTC), 
                          as.numeric(timestamps$DateTimeLocal))
  
  ggplot(timestamps) +
    ggtitle(paste("Week:", week, "UPAS Serial No.", upas[i])) +
    geom_point(aes(x=DateTimeUTC, y=DateTimeLocal), col= "black", size = 1) +
    xlab("Time Stamp (UTC)") + ylab("Time Stamp (Local)") +
    geom_text(aes(x = DateTimeUTC[1], y = DateTimeLocal[nrow(timestamps)]),
              hjust = -1, 
              label = paste("Correlation coef =", time_correlation)) +
    simple_theme
  
  ggsave(filename = paste(plot_fldr, upas[i], " UTC vs Local.jpeg", sep=""),
         device = "jpeg", dpi=400)
  
  temp <- data.frame(campaign_week = week,
                     UPAS = upas[i],
                     utc_slope = utc_slope,
                     local_slope = local_slope,
                     time_correlation = time_correlation)
  time_stamp_data <- rbind(time_stamp_data, temp)
  rm(temp)
}

write_xlsx(time_stamp_data,
           path=paste(output_path, "UPAS Timestamp Comparisons.xlsx", sep=""))



