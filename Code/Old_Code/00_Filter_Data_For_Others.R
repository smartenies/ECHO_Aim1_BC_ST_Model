#' =============================================================================
#' Project: ECHO LUR
#' Date created: August 2, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script contains code to make subsets of the filter data for other
#' ECHO researchers
#' =============================================================================

library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)

#' -----------------------------------------------------------------------------
#' Filter data for Sherry:
#'     -Paired filters only
#'     -PM2.5 and BC in the same dataset
#'     -Reduced number of variables included
#' NOTE: As of 8/2/2019, I don't yet have "calibrated" filter measurements of 
#' PM2.5 and black carbon, so for now I'm just making Sherry a dataset with the
#' clean but uncalibrated values
#' NOTE: As of 8/7/2019, the outdoor PM and BC have been calibrated, but the 
#' indoor measurements have not, so still going with the "raw" data
#' -----------------------------------------------------------------------------

pm_filter <- read_csv(here::here("Data", "Filter_PM.csv")) %>% 
  select(filter_id, campaign, week, StartDateLocal, EndDateLocal, 
         low_volume_flag, flow_rate_flag, below_lod, below_loq,
         negative_pm_mass, potential_contamination, indoor, 
         is_paired, is_blank, participant_id, indoor_monitor_id, paired_id,
         pm_ug_m3) %>% 
  filter(is_paired == 1)

bc_filter <- read_csv(here::here("Data", "Filter_BC.csv")) %>% 
  select(filter_id, blank_corrected_bc = blank_corrected, bc_ug_m3_uncorrected, 
         bc_ug_m3, bc_ug_m3, is_paired) %>% 
  filter(is_paired == 1) %>% 
  select(-is_paired)

filter_data <- left_join(pm_filter, bc_filter, by = "filter_id") %>% 
  select(filter_id:negative_pm_mass, blank_corrected_bc, 
         potential_contamination:pm_ug_m3, bc_ug_m3, 
         bc_ug_m3_uncorrected)

write_csv(filter_data, here::here("Data/Shared_Data", "Paired_Filter_Data.csv"))

#' Make this a wide dataset based on participant ID and campaign
participant_data <- filter_data %>% 
  select(campaign, week, participant_id, paired_id) %>% 
  distinct()

paired_pm <- filter_data %>% 
  select(participant_id, indoor, pm_ug_m3) %>% 
  spread(indoor, pm_ug_m3) %>%
  rename("indoor_pm" = "1", "outdoor_pm" = "0")

paired_bc <- filter_data %>% 
  select(participant_id, indoor, bc_ug_m3) %>% 
  spread(indoor, bc_ug_m3) %>%
  rename("indoor_bc" = "1", "outdoor_bc" = "0")

paired_ids <- filter_data %>% 
  select(participant_id, indoor, filter_id) %>% 
  spread(indoor, filter_id) %>%
  rename("indoor_filter_id" = "1", "outdoor_filter_id" = "0")

wide_data <- left_join(participant_data, paired_ids, by = "participant_id") %>% 
  left_join(paired_pm, by = "participant_id") %>% 
  left_join(paired_bc, by = "participant_id")

#' Add back in flags for low_volume, <LOD, negative PM mass, and potential contamination
flagged_filters <- filter_data %>% 
  select(filter_id, low_volume_flag, below_lod, negative_pm_mass, potential_contamination) %>% 
  filter(low_volume_flag == 1 | below_lod == 1 | negative_pm_mass == 1 | potential_contamination == 1)

wide_data <- wide_data %>% 
  mutate(indoor_flagged = ifelse(indoor_filter_id %in% flagged_filters$filter_id, 1, 0),
         outdoor_flagged = ifelse(outdoor_filter_id %in% flagged_filters$filter_id, 1, 0))

write_csv(wide_data, here::here("Data/Shared_Data", "Paired_Filter_Data_by_Participant.csv"))

