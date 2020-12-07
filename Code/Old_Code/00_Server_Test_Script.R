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

x <- data.frame(message = "Hello World")

write_csv(x, here::here("Data", "test.csv"))
