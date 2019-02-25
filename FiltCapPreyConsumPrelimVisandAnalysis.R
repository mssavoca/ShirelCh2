#################################################################################
## Analyses and visualizations for Filtration capacity and prey consumption paper
#################################################################################

library(ggplot2)
library(dplyr)
library(data.table)
library(readxl)
library(forcats)
library(tidyverse)
library(mgcv)
library(lme4)
library(lmerTest)


# formula for standard error
SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}

##########################################
# load, clean, combine and summarize data
##########################################
# get Alex Boersma's illustrations for figures
imgBw <- png::readPNG("./Blue-min.png")
rastBw <- grid::rasterGrob(imgBw, interpolate = T)
imgBp <- png::readPNG("./Fin-min.png")
rastBp <- grid::rasterGrob(imgBp, interpolate = T)
imgBe <- png::readPNG("./Bryde's-min.png")
rastBe <- grid::rasterGrob(imgBe, interpolate = T)
imgMn <- png::readPNG("./Humpback-min_optimized.png")
rastMn <- grid::rasterGrob(imgMn, interpolate = T)
imgBb <- png::readPNG("./Minke-min_optimized.png")
rastBb <- grid::rasterGrob(imgBb, interpolate = T)

f_data <- read_excel("ALLPRHS 2.5.2019.xls")

tag_guide <- read_excel("TAG GUIDE_2.18.19.xlsx")
tag_guide=tag_guide[c(2:nrow(tag_guide)),] ##get rid of first two rows
colnames(tag_guide)=as.character(unlist(tag_guide[1,])) # makes first row the header
tag_guide=tag_guide[-1,] # remove first row, leaving header

#joining f_data and tag guide to have location data
f_data <- left_join(f_data, tag_guide[ , c("ID", "Study_Area     _")], by = "ID")
names(f_data)[names(f_data) == "Study_Area     _"] <- "Study_Area"
f_data$Study_Area <- replace_na(f_data$Study_Area, "SoCal")   # Replace NAs in location with "SoCal"
f_data <- within(f_data, rm(notes)) #removing unwanted columns and rows

v_data <- read_excel("mwmrmeasures.xlsx")
v_data <- v_data[,c(1:3)] #keeps only columns 1-3
names(v_data)[names(v_data) == 'Species'] <- 'CommonName'
v_data$L <- v_data$MW*0.9766  #creates column that converts MW (kg) to liters


# creating fucntions from Shirel's paper for MW for engulfment capacity in liters for each species where we have a known length
bw_L <- function(x) {
  (x^3.667316*10^-0.014078)*0.9766}

bp_L <- function(x) {
  (x^3.54883*10^0.15604)*0.9766}

mn_L <- function(x) {
  (x^3.24603*10^0.85934)*0.9766}

ba_L <- function(x) {
  (x^3.10910*10^0.69969)*0.9766}

be_L <- function(x) {
  (x^3.1453*10^0.5787)*0.9766}  

# creating a new column where I recalcuate all the engulfment capacities using the functions above
v_data$Recalc_L <- ifelse(v_data$CommonName == "Blue Whale", bw_L(v_data$TLm), 
                          ifelse(v_data$CommonName == "Fin Whale", bp_L(v_data$TLm),
                                 ifelse(v_data$CommonName == "Humpback Whale", mn_L(v_data$TLm),
                                        ifelse(v_data$CommonName == "Minke Whale", ba_L(v_data$TLm), be_L(v_data$TLm)))))

# create table looking at averages of MW 
v_data_species <- v_data %>% group_by(CommonName) %>% 
  summarize(Mean_L = mean(L), Mean_Recalc_L = mean(Recalc_L), 
            Med_L = median(L), Med_Recalc_L = median(Recalc_L), 
            Mean_TL = mean(TLm), Med_TL = median(TLm))

v_data <- left_join(v_data, v_data_species, by = "CommonName") 


# cleaning data and adding columns
f_data = f_data %>% 
  mutate(Species = substr(f_data$ID,1,2),
         CommonName = ifelse(Species == "bw", "Blue Whale",
                             ifelse(Species == "bp", "Fin Whale",
                                    ifelse(Species == "mn", "Humpback Whale",
                                           ifelse(Species == "bb", "Minke Whale", "Bryde's Whale")))),
         TotalLunges = dayhourslunges + nighthourslunges + twilightzonelunges,
         TotalHours = dayhours + nighthours + twilightzone,
         LungesPerHour = TotalLunges/TotalHours,
         LungesPerDayHour = dayhourslunges/dayhours,
         LungesPerNightHour = nighthourslunges/nighthours,
         LungesPerTwHour = twilightzonelunges/twilightzone,
         Length = as.numeric(gsub(" m", "", f_data$whaleLength)),
         EmpEngulfCap = ifelse(CommonName == "Blue Whale", bw_L(Length), 
                               ifelse(CommonName == "Fin Whale", bp_L(Length),
                                      ifelse(CommonName == "Humpback Whale", mn_L(Length),
                                             ifelse(CommonName == "Minke Whale", ba_L(Length), be_L(Length)))))) %>% 
  select(-whaleLength) %>% 
  separate(prey, into = c("Prey", "Prey notes"), sep = " ") %>% 
  drop_na(Prey) %>% 
  filter(!Prey %in% c("Milk", "N")) %>% 
  mutate(PreyClean = case_when(
    Prey %in% c("Anchovies", "Fish", "Herring", "SandLance", "Sardines")  ~ "Fish-feeding",
    Prey %in% c("Inverts", "Krill") ~ "Krill-feeding"), 
    Region = case_when(
      Study_Area %in% c("Monterey", "SoCal", "Cordell Bank", "San Diego", "WA Coast")  ~ "Eastern North Pacific",
      Study_Area %in% c("Stellwagen", "Norway", "Azores", "Greenland") ~ "North Atlantic",
      Study_Area == "South Africa" ~ "South Africa",
      Study_Area == "Antarctic" ~ "Antarctic",
      Study_Area == "Chile" ~ "Chile"))



# combining feeding rates and engulfment volume dataframes into what we need
fv_data <- f_data %>% 
  left_join(v_data_species, by = "CommonName") %>% 
  mutate(EngulfVolPerHr = LungesPerHour*Med_Recalc_L,
         EngulfVolPerDayHr = LungesPerDayHour*Med_Recalc_L) %>% 
  select(-c(Med_L, Mean_L))

# looking at differences in feeding rates for tags on a full day or less than a day
full_day = fv_data %>% filter(Species == "bw" & TotalHours > 23) 
hist(full_day$LungesPerHour)
median(full_day$LungesPerHour)

part_day = fv_data %>% filter(Species == "bw" & TotalHours < 15) 
hist(part_day$LungesPerHour)
median(part_day$LungesPerHour)

# load data
d_full_NULL <- read_csv("Cetacea model output NULL_EXTANT.csv") %>% 
  mutate(Species = ifelse(Species == "bonarensis", "bonaerensis", Species))
d_full_BOUT <- read_csv("Cetacea model output BOUT_EXTANT.csv") %>% 
  mutate(Species = ifelse(Species == "bonarensis", "bonaerensis", Species))

RorqualData <- read_csv("lunge_rates_from_Paolo.csv") %>% 
  mutate(`deployment-time_h` = `deployment-time_secs`/60/60)


# # sweet tidy code from Max
d_sum_NULL <- d_full_NULL %>%
  group_by(Genus, Species) %>%
  summarize(wgtMeanNULL_wt_g = weighted.mean(`Prey W (g)`, Percent),
            medNULL_wt_g = median(`Prey W (g)`),
            wgtMeanNULL_E = weighted.mean(`Energy (kJ)`, Percent),
            medNULL_E = median(`Energy (kJ)`))

d_sum_BOUT = d_full_BOUT %>% 
  group_by(Genus, Species) %>% 
  summarize(wgtMeanBOUT_wt_g = weighted.mean(`Prey W (g)`, Percent), 
            medBOUT_wt_g = median(`Prey W (g)`), 
            wgtMeanBOUT_E = weighted.mean(`Energy (kJ)`, Percent),
            medBOUT_E = median(`Energy (kJ)`))

OdontoceteData <- read_csv("foragestats_combined_ko2.csv") %>% 
  separate(Species, into = c("Genus", "Species"), sep = "_") %>% 
  left_join(d_sum_NULL, by = c("Genus", "Species"))
##FIXME, I think this works though
OdontoceteData<- OdontoceteData %>% 
  left_join(d_sum_BOUT, by = c("Genus", "Species"))

# REMEMBER: OdontoceteData is all of the data we need. 
# Add rorqual data to odontocete data
cetacean_data <- left_join(OdontoceteData, RorqualData, by = "ID") %>%
  # feeding events are lunges, buzzes for rorquals, odontocetes
  mutate(TotalFeedingEvents = coalesce(total_lunges, total_buzz_count),
         TotalTagTime_h = coalesce(`deployment-time_h`, total_duration_h)) %>% 
  # drop unknown species
  drop_na(Species) %>% 
  mutate(feeding_rate = TotalFeedingEvents / TotalTagTime_h)          # FEEDING RATE

# Gather scenarios, view output
scenario_data <- cetacean_data %>% 
  filter(TotalTagTime_h > 23, Species %in% c("musculus", "physalus", "novaeangliae", "bonaerensis")) %>%   #  & sonar_exp %in% c("none", NA) # possibly includ sonar exp, but it takes away several blues and one fin whale
  select(ID, Species, feeding_rate, wgtMeanNULL_wt_g:medBOUT_E) %>%
  gather(scenario, prey_wgt_g, c(wgtMeanNULL_wt_g, medNULL_wt_g, wgtMeanBOUT_wt_g, medBOUT_wt_g)) %>% 
#  gather(scenario_E, prey_E, c(wgtMeanNULL_E, medNULL_E, wgtMeanBOUT_E, medBOUT_E)) %>%  Switch these as necessary
     mutate(hourly_prey_in_g = prey_wgt_g * feeding_rate,
         scenario_type = ifelse(scenario %in% c("wgtMeanNULL_wt_g", "medNULL_wt_g"), "NULL", "BOUT"),
         calc_type = ifelse(scenario %in% c("wgtMeanNULL_wt_g", "wgtMeanBOUT_wt_g"), "mean", "med")) %>% 
  mutate(prey_wt_d_t = (hourly_prey_in_g*6)/1000/1000) %>% 
  group_by(Species, scenario, scenario_type, calc_type) %>% 
  summarize(mean_hourly_prey_in_g = mean(hourly_prey_in_g),
            mean_prey_wt_d_t = mean(prey_wt_d_t)) 


###########################################
# preliminary plots for prey consumption
###########################################

# run for plot of hours, up to one day
resolution <- 100
# Calculate feeding over time (for plotting)
max_hours <- 24
plot_data <- tibble(t = seq(0, 24, length.out = resolution),
                    dummy = 1) %>%
  full_join(mutate(scenario_data, dummy = 1)) %>%
  select(-dummy) %>%
  filter(Species %in% c("musculus", "physalus", "novaeangliae", "bonaerensis")) %>%
  mutate(prey_consumed_day = mean_hourly_prey_in_g * t / 1e6)
# Plot scenarios
PreyConsumptionbyHour <-ggplot(plot_data,
       aes(x = t, 
           y = prey_consumed_day, 
           linetype = calc_type, 
           color = scenario_type)) +
  geom_line() +
  theme_bw() +
  facet_grid(~Species) +
  scale_y_continuous(breaks=seq(0,140,20)) +
  labs(x = "Hours feeding", y = "Prey consumed by individual (metric tons)") +
  geom_text(aes(y = prey_consumed_day + 2, x = 23, 
                label = format(prey_consumed_day, digits = 0)),
            data = filter(plot_data, t == max(t)))
#Save pdf of plot
dev.copy2pdf(file="PreyConsumptionbyHour.pdf", width=14, height=8)

# run for plot of months
resolution <- 100
# Calculate feeding over time (for plotting)
max_months <- 6
plot_data <- tibble(t = seq(0, 24 * 30 * max_months, length.out = resolution),
                    dummy = 1) %>%
  full_join(mutate(scenario_data, dummy = 1)) %>%
  select(-dummy) %>%
  filter(Species %in% c("musculus", "physalus", "novaeangliae", "bonaerensis")) %>%
  mutate(prey_consumed_month = mean_hourly_prey_in_g * t / 1e6)
# Plot scenarios
PreyConsumptionbyMonth <- ggplot(plot_data,
       aes(x = t / 24 / 30, 
           y = prey_consumed_month, 
           linetype = calc_type, 
           color = scenario_type)) +
  facet_grid(~Species) +
  geom_line() +
  theme_bw() +
  labs(x = "Months feeding", y = "Prey consumed per individual (metric tons)") +
  geom_text(aes(y = prey_consumed_month + 400, x =  5.5,
                label = format(prey_consumed_month, digits = 0)),
            data = filter(plot_data, t == max(t)))
#Save pdf of plot
dev.copy2pdf(file="PreyConsumptionbyMonth.pdf", width=14, height=8)


# read in data
prey_predict <- read_excel("PreyIngestPredict.xlsx") %>% mutate(dummy = 1)
prey_predict_w_M <- tibble(M_kg = seq(5000,120000,5000), dummy =1) %>%
  full_join(prey_predict, by = "dummy") %>% 
  select(-"dummy") %>% 
  mutate(R = `Intercept (A)`*M_kg^`Exponent (B)`)

# plot predictions from literature, with ours

ingest_predict_plot <- ggplot(prey_predict_w_M, aes(log10(M_kg), log10(R))) +
  geom_line(data = filter(prey_predict_w_M, !`Reference(s)` %in%  c("Savoca et al., this study (lower bound)", 
                                                                    "Savoca et al., this study (upper bound)",
                                                                    "Savoca et al., this study (best estimate)")), 
            aes(color = str_wrap(`Reference(s)`, 20)), size = 1.15) +
  geom_line(data = filter(prey_predict_w_M, `Reference(s)` == "Savoca et al., this study (best estimate)"), 
            color = str_wrap("dodgerblue4", 20), size = 1.15, linetype = "dashed") +
  geom_line(data = filter(prey_predict_w_M, `Reference(s)` == "Savoca et al., this study (lower bound)"), 
            color = str_wrap("dodgerblue2", 20), size = 1.15, linetype = "dotted") +
  geom_line(data = filter(prey_predict_w_M, `Reference(s)` == "Savoca et al., this study (upper bound)"), 
            color = str_wrap("dodgerblue2", 20), size = 1.15, linetype = "dotted") +
 # annotation_custom(rastBw, xmin = 4.5, xmax = 5.2, ymin = 3.55, ymax = 4.3) +
 # annotation_custom(rastMn, xmin = 4.1, xmax = 4.5, ymin = 3.25, ymax = 3.75) +
 # annotation_custom(rastBb, xmin = 3.7, xmax = 3.9, ymin = 2.5, ymax = 3) +
  ylim(1.9,5.25) +
  labs(x = "log[Body mass (kg)]", y ="log[Daily ration (kg)]", color = "Reference(s)") +
  theme_bw() +
  theme(legend.key.height=unit(1, "cm"))
ingest_predict_plot



###########################################
# preliminary plots for filtration capacity
###########################################

pal <- c("Minke Whale" = "firebrick3", "Bryde's Whale" = "darkorchid3",  "Humpback Whale" = "gray30", "Fin Whale" = "chocolate3", "Blue Whale" = "dodgerblue2" )
Shape <- c("Minke Whale" = 15, "Bryde's Whale" = 8,  "Humpback Whale" = 17, "Fin Whale" = 18, "Blue Whale" = 19 )

EmpEngulfCapPLot <- ggplot(fv_data, aes(Length, EmpEngulfCap)) +
  geom_point(inherit.aes = T, aes(shape = CommonName, color = CommonName), size = 2) + 
  labs(col="Common name", shape = "Common name") +
  geom_smooth(method = lm) +
  xlab("Length (m)") + ylab("Engulfment capacity (L)") +
  ggtitle("Engulfment capacity by length for whales measured by drone") +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values=Shape)+
  theme_bw()
EmpEngulfCapPLot 

fv_data$Species <- fct_relevel(fv_data$Species, "be","bb","mn","bp","bw")

v1 <- ggplot(filter(fv_data, TotalHours > 2 & Prey == "Krill" & TotalLunges > 0), aes(x = Species, y = EngulfVolPerDayHr, color = CommonName)) + 
  geom_point(inherit.aes = T, alpha = 0.8, position = position_jitter(width = .25)) + 
  geom_boxplot(inherit.aes = T, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values=Shape) +
  ylab("Filtration capacity (liters d-1 h)") + ggtitle("Water volume filtered per hour (fish feeding whales)") +
  theme_bw()
v1 + scale_x_discrete(labels=c("be" = "bryde's\nwhale", "bb" = "minke\nwhale", "mn" = "humpback\nwhale", "bp" = "fin\nwhale", "bw" ="blue\nwhale")) +
  theme(legend.position="none")


g <- ggplot(data = my_datal, aes(y = Sensitivity, x = EmotionCondition, fill = EmotionCondition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Sensitivity, color = EmotionCondition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  # coord_flip() +
  theme_bw() +
  raincloud_theme


##########################
# Prelim stats exploration
##########################




###############################################
# for funsies, some back of the envelope calcs:
###############################################

# volume of water in upper 300m of Monterey Bay
((pi*20000^2*300)/2)*1000 # this equals 1.884956e+14 (9.42478e+13 for half cylinder), a blue whale filters ~1000000 per hour, or 24000000 per day
1.884956e+14/12000000 # the number of blue whale days it takes to filter the bay

# for the world's oceans, see: https://www.ngdc.noaa.gov/mgg/global/etopo1_ocean_volumes.html

# in the Southern Ocean
(71800000*0.1)*1000000000000  # total volume in L of water in the upper 10% of water column (~327m) Southern Ocean 
2000*12000000 # approximate amount of water filtered per day by current population of Southern Ocean blue whales 
350000*12000000 # approximate amount of water filtered per day by pre-exploitation population of Southern Ocean blue whales 
750000*12000000 # approximate amount of water filtered per day by pre-exploitation population of Southern Ocean fin whales 
((4.2e+12 + 9e+12)*120/7.18e+18)*100 # Percentage of the upper 327m of the Southern Ocean that the pre-exploitation population of blue and fin whales 
