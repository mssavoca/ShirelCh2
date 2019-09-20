#################################################################################
# Lunge rates and prey consumption calculations and visualizations for Balaenids ----
#################################################################################

# load data and packages and fuctions 
library(data.table)
library(readxl)
library(forcats)
library(tidyverse)
library(ggstance)
library(mgcv)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggpubr)


SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}

# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus
abbr_binom <- function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}


# read in data from Will G
Balaenid_data <- read_csv("Balaenid Dive Info For Matt_final.csv") %>% 
  mutate(Species = case_when(CommonName == "Bowhead Whale" ~ "Balaena mysticetus",
                             CommonName == "North Atlantic Right Whale" ~ "Eubalaena glacialis"),
         SpeciesCode = case_when(CommonName == "Bowhead Whale" ~ "bm",
                                 CommonName == "North Atlantic Right Whale" ~ "eg"),
         Dive_time_min = `Dive Time (s)`/60,
         Dive_time_hr = `Dive Time (s)`/60/60,
         fluking_during_bottom_sec = `Bottom Time (s)`*(`Fluking During Bottom (%)`/100),
         fluking_during_bottom_min = fluking_during_bottom_sec/60,
         fluking_during_bottom_hr = fluking_during_bottom_sec/60/60)

Balaenid_data_sum <- Balaenid_data %>% 
  group_by(ID, SpeciesCode) %>% 
  mutate(total_tag_time_hr = `Total Seconds on Whale`/60/60,
         water_filtered_m3_hr = `Volume Flow Rate (m3 per s)`*60*60) %>% 
  summarise(total_dive_time_hr = sum(Dive_time_hr),
            total_fluking_during_bottom_hr = sum(fluking_during_bottom_hr),
            total_tag_time_hr = first(total_tag_time_hr),
            water_filtered_m3_hr = first(water_filtered_m3_hr),
            prop_time_feeding = total_fluking_during_bottom_hr/total_tag_time_hr) %>% 
  ungroup %>% 
  mutate(total_water_filtered_m3 = water_filtered_m3_hr*total_fluking_during_bottom_hr,
         est_total_prey_consumed_kg = case_when(SpeciesCode == "bm" ~ (total_water_filtered_m3*6)/1000,     # Prey density data from van der Hoop et al 2018, Figure 9
                                                SpeciesCode == "eg" ~ (total_water_filtered_m3*170)/1000),    # may go with a range of low (), likely, 
         est_prey_consumed_kg_hr = est_total_prey_consumed_kg/total_tag_time_hr)

Balaenid_prey_ingestion <- Balaenid_data_sum %>% 
  group_by(SpeciesCode) %>% 
  summarise(wgt_prey_kg_hr = weighted.mean(est_prey_consumed_kg_hr, total_tag_time_hr)) %>% 
  mutate(wgt_prey_kg_day = case_when(SpeciesCode == "bm" ~ wgt_prey_kg_hr*14,
                                     SpeciesCode == "eg" ~ wgt_prey_kg_hr*14))



