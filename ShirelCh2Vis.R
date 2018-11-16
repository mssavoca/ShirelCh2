####################################
## Analyses and visualizations for Shirel's second chapter
####################################

library(ggplot2)
library(dplyr)
library(data.table)
library(readxl)

# read in data
f_data <- read_excel("FiltrationMetadataMASTER.xls")

#removing unwanted columns
f_data <- within(f_data, rm(Note, Hours, seconds))

# add a species column using the first two letters of Individual ID
f_data$Species <- substr(f_data$`Individual ID`,1,2)

# adding columns
f_data$TotalLunges <- f_data$DayLunges + f_data$NightLunges + f_data$TwilightLunges
f_data$TotalHours <- f_data$DayHours + f_data$NightHours + f_data$TwilightHours
f_data$LungesPerHour <- f_data$TotalLunges/f_data$TotalHours
