#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 Land Use Regression
#' 
#' Task: Analyze the UAS pilot data to see how comparable measurements between
#' monitors are-- need to calibrate UPAS meausurements using the co-located
#' monitoring data and this only works if the monitors are similar enough
#' 
#' Author: Sheena Martenies
#' 
#' Date Created: July 7, 2019
#' 
#' Contact: sheena.martenies@colostate.edu
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(ggthemes)

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
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

data_path <- "R:/RSTOR-Magzamen/Research/Projects/ECHO_Aim1/Raw_Data/UPAS_Pilot_Testing/"

#' Data from the last pilot test we did in April of 2018 (right before field work)
pilot_data <- read_xlsx(paste0(data_path, "FilterColocationTests11Dec17.xlsx"),
                        sheet = "4_17_18", skip = 1)
colnames(pilot_data) <- tolower(gsub(" ", "_", colnames(pilot_data)))
summary(pilot_data)

#' One filter had a negative TWA and the note says it was sent off to AST
#' Therefore, dropping this monitor from analysis
pilot_data <- filter(pilot_data, serial_number != "PS0431")

#' Comparing rooftop data
rooftop <- filter(pilot_data, location == "Facilities Rooftop") %>% 
  select(serial_number, twa_ug_m3) %>% 
  mutate(test = "with_case")

mean(rooftop$twa_ug_m3, na.rm=T)
sd(rooftop$twa_ug_m3, na.rm=T) / mean(rooftop$twa_ug_m3, na.rm=T)
plot(rooftop$twa_ug_m3)

ggplot(data = rooftop) +
  geom_boxplot(aes(x = "Rooftop Test", y = twa_ug_m3)) +
  theme(legend.position = c(0.8, 0.8)) +
  simple_theme

#' Data from the first rooftop pilot test in December of 2017
pilot_data2 <- read_xlsx(paste0(data_path, "FilterColocationTests11Dec17.xlsx"),
                        sheet = "Roof test", skip = 1) 
colnames(pilot_data2) <- tolower(gsub(" ", "_", colnames(pilot_data2)))
pilot_data2 <- filter(pilot_data2, !is.na(serial_number) & serial_number != "Blank") %>% 
  rename("twa_ug_m3" = 'twa_(µg/m3)') %>% 
  mutate(twa_ug_m3 = as.numeric(twa_ug_m3)) %>% 
  select(serial_number, twa_ug_m3) %>% 
  mutate(test = "no_case")
summary(pilot_data2)

#' One filter had a really high TWA (more than double the others)
#' Possibly contaminated?
#' Therefore, dropping this monitor from analysis
pilot_data2 <- filter(pilot_data2, serial_number != "PS0414")

mean(pilot_data2$twa_ug_m3, na.rm=T)
sd(pilot_data2$twa_ug_m3, na.rm=T) / mean(pilot_data2$twa_ug_m3, na.rm=T)
plot(pilot_data2$twa_ug_m3)

#' Box plots for the TWAs for each rooftop test
plot_data <- bind_rows(rooftop, pilot_data2)

ggplot(data = plot_data) +
  geom_boxplot(aes(x = test, y = twa_ug_m3, col = as.factor(test))) +
  theme(legend.position = c(0.8, 0.8)) +
  simple_theme

write_csv(plot_data, here::here("Data", "UPAS_Pilot_Test_Results.csv"))





