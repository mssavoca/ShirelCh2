##################################################################
# Lunge rates and prey consumption calculations and visualizations ----
##################################################################

# load data and packages 
library(ggplot2)
library(dplyr)
library(data.table)
library(readxl)
library(forcats)
library(tidyverse)
library(mgcv)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggpubr)

load("~/Documents/Research Data/Whale research/ShirelCh2/rates.RData")

# Species-specific average length data (from Shirel's paper), to recalculate average engulfment capacity
v_data <- read_csv("MWmeasurements.csv") %>% 
  group_by(Species) %>% 
  dplyr::summarize(med_TLm = median(TLm)) %>% 
  rename(CommonName = Species) %>% 
  mutate(SpeciesCode = case_when(CommonName ==  "Blue Whale" ~ "bw",
                                 CommonName == "Fin Whale"~ "bp",
                                 CommonName ==  "Humpback Whale" ~ "mn",
                                 CommonName == "Minke Whale" ~ "bb", 
                                 CommonName == "Bryde's Whale" ~ "be",
                                 CommonName == "Sei Whale" ~ "bs"))
# # v_data$L <- v_data$MW*0.9766  #creates column that converts MW (kg) to liters

# Allometric equations from Shirel's paper
# creating fucntions from Shirel's paper for MW (in kg) for engulfment capacity in liters for each species where we have a known length
engulf_allo <- tribble(
  ~SpeciesCode, ~slope,   ~intercept,
  "bb",     3.10910,  0.69969,
  "be",     3.1453,   0.5787,
  "bp",     3.54883,  0.15604,
  "bw",     3.667316, -0.014078,
  "mn",     3.24603,  0.85934
)

# read in whale population data. Current data from IUCN 2019 Redlist, whaling data compiled in Rocha et al. 2014
pop_data <- read_excel("Filtration project_Whale population and feeding information.xlsx", sheet = 1)


# older file with droned lengths
old_lengths <- read_excel("ALLPRHS 2.5.2019.xls") 

# Skips first two rows
tag_guide <- read_excel("TAG GUIDE_2019-0806-010125.xlsx", skip = 2) %>% 
  rename(Study_Area = `Study_Area     _`,
         SpeciesCode = `Spec      _`) %>% 
  left_join(old_lengths, by = "ID")


lunge_rates <- lunge_rates %>% 
  mutate_at(vars(Rate), ~replace(., is.nan(.), 0))


# Master filtration rate file
filtration_master <- lunge_rates %>% 
  left_join(select(tag_guide, ID, Study_Area, whaleLength), by = "ID") %>% 
  mutate(   
    # Make whale length a number
    whaleLength = parse_number(whaleLength),
    SpeciesCode = substr(ID,1,2),
    # Replace NAs in location with "SoCal"
    Study_Area = replace_na(Study_Area, "SoCal"),
    CommonName = case_when(
      SpeciesCode == "bw" ~ "Blue Whale",
      SpeciesCode == "bp" ~ "Fin Whale",
      SpeciesCode == "mn" ~ "Humpback Whale",
      SpeciesCode %in% c("ba", "bb") ~ "Minke Whale", 
      TRUE ~ "Bryde's Whale"),
    Species = case_when(
      SpeciesCode == "bw" ~ "Balaenoptera musculus",
      SpeciesCode == "bp" ~ "Balaenoptera physalus",
      SpeciesCode == "mn" ~ "Megaptera novaeangliae",
      SpeciesCode %in% c("bb","ba") ~ "Balaenoptera bonaerensis", 
      SpeciesCode == "be" ~ "Balaenoptera edeni"),
    Region = case_when(
        Study_Area %in% c("Monterey", "SoCal", "Cordell Bank", "San Diego", "WA Coast")  ~ "Eastern North Pacific",
        Study_Area %in% c("Stellwagen", "Norway", "Azores", "Greenland") ~ "North Atlantic",
        Study_Area == "South Africa" ~ "South Africa",
        Study_Area == "Antarctic" ~ "Antarctic",
        Study_Area == "Chile" ~ "Chile")) %>% 
  # Join population numbers
  left_join(select(pop_data, -c(SpeciesCode, `Current Range`)), by = "Species") %>% 
  # Join species-median size data
  left_join(select(v_data, -CommonName), by = "SpeciesCode") %>% 
  # Join allometric equations
  left_join(engulf_allo, by = "SpeciesCode") %>% 
  # Calculate engulfment volumes
  mutate(
    BestLengthEst = coalesce(whaleLength, med_TLm),
    Engulfment_L = BestLengthEst ^ slope * 10 ^ intercept * 0.9766,
    EngulfVolPerHr = Engulfment_L*Rate)


#plot of feeding rates for ENP species 
