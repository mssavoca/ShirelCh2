#################################################################################
## Analyses and visualizations for Filtration capacity and prey consumption paper
#################################################################################

#############################
# Load functions and packages
#############################

library(ggplot2)
library(dplyr)
library(data.table)
library(readxl)
library(forcats)
library(tidyverse)
library(mgcv)
library(lme4)
library(lmerTest)
library(ggpubr)


# formula for standard error
SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}

# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus
abbr_binom = function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}


##############################################
# get Alex Boersma's illustrations for figures
##############################################
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


##########################################
# load, clean, combine and summarize data
##########################################

# read in data for direct predictions of daily ration (R) from literature
prey_predict <- read_excel("PreyIngestPredict.xlsx") %>% mutate(dummy = 1)
prey_predict_w_M <- tibble(M_kg = seq(5000,120000,5000), dummy =1) %>%
  full_join(prey_predict, by = "dummy") %>% 
  select(-"dummy") %>% 
  mutate(R = `Intercept (A)`*M_kg^`Exponent (B)`,
         R_compressed_90days = (R*365)/90,
         R_compressed_120days = (R*365)/120)


# read in data for predictions from BMR-->FMR of daily ration (R) from literature
#formulas: 

# krill diet percentages (from Pauly et al. 1998)
# Z-values (proportion krill in diet)
# bw = 1
# bp = 0.8
# bb - 0.9
# ba = 0.65
# bbor = 0.8
# be = 0.4
# mn = 0.55

# average weight (in kg) from Jeremy's data or Lockyer 1976
# bw = 93000
# bp = 53000
# bb/ba = 6700
# bbor = mean(c(15,18.5, 15.5,17, 17,18.5)*1000)  # From Wiki, from Lockyer 1976
# OR  mean(c(8.53, 10.25, 11.38, 11.28, 16.08, 15.56, 8.58, 13.32, 9.90, 10.61, 13.80, 15.76, 12.94, 
# #             8.89, 16.36, 12.34, 12.22, 21.62)*1000)
# be = mean(c(11.98,11.77,13.87,13.46,11.75,13.76,11.49,11.64,11.93,13.61,8.39,13.91,15.43,14.12,13.78,
#             13.91,12.55, 12.46, 11.89, 12.79, 11.32, 9.99, 16.15, 15.96, 14.85, 12.78,15.47)*1000) 
# mn = 36000


# estimate of BMR extrapolated from Kleiber 1975: 
  # BMR = 293.1*(M^0.75)
# estimate of ADMR ~ FMR from Lavigne, Leaper and Lavigne 2007, Barlow et al. 2008: 
    # ADMR = beta*BMR, where beat is a value between 2-5 (see Barlow et al. 2008 p.287 for more)
# estimate of R from Leaper and Lavigne 2007, Barlow et al. 2008:
  # R = ADMR/(0.8*[3900*Z + 5450*(1-Z)])
# to compute prey intake per day when feeding if ALL feeding is compresseded into 120 days
  # (R*365)/120

prey_predict_from_BMR <- read_excel("PreyIngestPredict.xlsx", sheet = 2) %>% 
  mutate(KleiberBMR = 293.1*M_kg^0.75,
         dummy = 1)
BMRtoFMRprojection <- tibble(beta = seq(2,5,0.5), dummy = 1) %>% 
  full_join(prey_predict_from_BMR, by = "dummy") %>% 
  select(-dummy) %>% 
  mutate(ADMR = beta*KleiberBMR,
         R  = ADMR/(0.8*((3900*Z)+5400*(1-Z))),
         R_compressed_90days = (R*365)/90,
         R_compressed_120days = (R*365)/120)


# estimating amount of food consumed over the course of a feeding season (see Lockyer 2007).
# using the equation I developed with Dave Cade:E_preyseason = ((W_blubber*E_blubber)/A) + (((beta*293.1*M^0.75)*D_feeding)/A)
sims <- crossing(W_blubber = 18500,
                 E_blubber = seq(27e3, 37e3, by = 1e3),
                 A = 0.8,
                 beta = seq(2, 5, by = 0.5),
                 M = 42240,
                 D_feeding = seq(60, 182.5, by = 10)) %>% 
  mutate(E_preyseason = ((W_blubber*E_blubber)/A) + (((beta*293.1*M^0.75)*D_feeding)/A))

sims_plot <- sims %>% 
  ggplot(aes(E_preyseason)) +
  geom_histogram() +
  theme_classic()

sims_summ <- summarize_at(sims, vars(E_preyseason), list(mean, sd, median, min, max))



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
# full_day = fv_data %>% filter(Species == "bw" & TotalHours > 23) 
# hist(full_day$LungesPerHour)
# median(full_day$LungesPerHour)
# fv_data %>% group_by(CommonName) %>% filter(TotalHours > 24 & TotalLunges > 0) %>% summarize(n_distinct(ID), mean(LungesPerHour)) %>% View
# 
# part_day = fv_data %>% filter(Species == "bw" & TotalHours < 15) 
# hist(part_day$LungesPerHour)
# median(part_day$LungesPerHour)

# read in whale population data. Current data from IUCN 2019 Redlist, whaleing data compiled in Rocha et al. 2014
pop_data <- read_excel("Filtration project_Whale population and feeding information.xlsx", sheet = 1)

# load data
d_full_NULL <- read_csv("Cetacea model output NULL_EXTANT.csv") %>% 
  mutate(Species = ifelse(Species == "bonarensis", "bonaerensis", Species))
d_full_BOUT <- read_csv("Cetacea model output BOUT_EXTANT.csv") %>% 
  mutate(Species = ifelse(Species == "bonarensis", "bonaerensis", Species))

RorqualData <- read_csv("lunge_rates_from_Paolo.csv") %>% 
 left_join(f_data, by= "ID") %>% 
  mutate(`deployment-time_h` = `deployment-time_secs`/60/60,
         SpeciesCode = substr(ID,1,2),)  %>% 
  select(-c(species)) %>% 
  arrange(dayhours, ID)

write_csv(RorqualData, path = "RorqualDataforTimeDay.csv")



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
cetacean_data <- left_join(OdontoceteData, RorqualData, by = c("ID")) %>%
  # feeding events are lunges, buzzes for rorquals, odontocetes
  mutate(TotalFeedingEvents = coalesce(total_lunges, total_buzz_count),
         TotalTagTime_h = coalesce(`deployment-time_h`, total_duration_h)) %>% 
  drop_na(Species) %>% 
  mutate(feeding_rate = TotalFeedingEvents / TotalTagTime_h,
         SpeciesCode = substr(ID,1,2),)  %>%       # FEEDING RATE
  left_join(pop_data, by = "SpeciesCode") %>% 
  rename(Species = Species.y)
cetacean_data$SpeciesCode <- sub("ba", "bb", cetacean_data$SpeciesCode)


# 
# # Gather scenarios, view output
# scenario_data <- cetacean_data %>% 
#   filter(TotalTagTime_h > 24, Species %in% c("musculus", "physalus", "novaeangliae", "bonaerensis")) %>%   #  & sonar_exp %in% c("none", NA) # possibly includ sonar exp, but it takes away several blues and one fin whale
#   select(ID, Species, feeding_rate, wgtMeanNULL_wt_g:medBOUT_E) %>%
#   gather(scenario, prey_wgt_g, c(wgtMeanNULL_wt_g, medNULL_wt_g, wgtMeanBOUT_wt_g, medBOUT_wt_g)) %>% 
#   #  gather(scenario_E, prey_E, c(wgtMeanNULL_E, medNULL_E, wgtMeanBOUT_E, medBOUT_E)) %>%  Switch these as necessary
#   mutate(hourly_prey_in_g = prey_wgt_g * feeding_rate,
#          scenario_type = ifelse(scenario %in% c("wgtMeanNULL_wt_g", "medNULL_wt_g"), "NULL", "BOUT"),
#          calc_type = ifelse(scenario %in% c("wgtMeanNULL_wt_g", "wgtMeanBOUT_wt_g"), "mean", "med")) %>% 
#   mutate(prey_wt_d_t = (hourly_prey_in_g*6)/1000/1000) %>% 
#   group_by(Species, scenario, scenario_type, calc_type) %>% 
#   summarize(mean_hourly_prey_in_g = mean(hourly_prey_in_g),
#             mean_prey_wt_d_t = mean(prey_wt_d_t)) 


# combine two dataframes into one monster dataframe
cetacean_data$total_lunges <- as.double(cetacean_data$total_lunges) # need to do this conversion for coalesce to work below
vol_master_data <- cetacean_data %>%
  filter(SpeciesCode %in% c("bw", "bp", "mn", "bb", "be")) %>% 
  select(ID, Species, Body_length_m, TotalTagTime_h, feeding_rate, total_lunges, sonar_exp) %>% 
  full_join(fv_data, by = "ID") %>% 
  mutate(LungesPerHour = coalesce(feeding_rate, LungesPerHour),
         TotalTagTime_h = coalesce(TotalTagTime_h, TotalHours),
         TotalLunges = coalesce(TotalLunges, total_lunges),  
         SpeciesCode = substr(ID,1,2),
         AvgLength = case_when(SpeciesCode == "bw" ~ 25.2,
                               SpeciesCode == "bp" ~ 20.2,
                               SpeciesCode == "mn" ~ 14,
                               SpeciesCode == "be" ~ 14.5, 
                               SpeciesCode %in% c("ba","bb") ~ 7.8),
         MW_est_L = case_when(SpeciesCode == "bw" ~ bw_L(AvgLength),
                              SpeciesCode == "bp" ~ bp_L(AvgLength),
                              SpeciesCode == "mn" ~ mn_L(AvgLength),
                              SpeciesCode == "be" ~ be_L(AvgLength), 
                              SpeciesCode %in% c("ba","bb") ~ ba_L(AvgLength)),
  sonar_exp = replace_na(sonar_exp, "none"),
  PreyClean = replace_na(PreyClean, "Krill-feeding"),
  EngulfVolPerHr = LungesPerHour*MW_est_L) %>% 
  left_join(pop_data, by = "SpeciesCode") %>% 
  select(-c(Index, Species, Body_length_m, TotalHours, total_lunges, feeding_rate, TagOn, TagOff, `Prey notes`, `Current Range`, Species.x,
            `Number removed by 20th century whaling (N. Hemisphere)`, `Number removed by 20th century whaling (S. Hemisphere)`)) %>% 
  rename(Species = Species.y)
vol_master_data$SpeciesCode <- sub("ba", "bb", vol_master_data$SpeciesCode)


# View summary info on how many tags in this dataset
vol_master_data %>% group_by(SpeciesCode) %>% 
  filter(TotalTagTime_h > 2 & TotalLunges > 0 & sonar_exp =="none") %>% 
  summarize(n_distinct(ID),
            meanVFD = mean(EngulfVolPerHr*24),
            seVFD = SE(EngulfVolPerHr*24)) %>% View



###########################################
# preliminary plots for filtration capacity
###########################################

pal <- c("ba" = "gold3", "bb" = "firebrick3", "be" = "darkorchid3",  "mn" = "gray30", "bp" = "chocolate3", "bw" = "dodgerblue2" )
Shape <- c("ba" = 10, "bb" = 15, "be" = 8, "mn" = 17, "bp" = 18, "bw" = 19)

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

EmpEngulfCapMod <- lmer(EmpEngulfCap ~ Length + (1|Species), data = fv_data)
summary(EmpEngulfCapMod)

vol_master_data$SpeciesCode <- fct_relevel(vol_master_data$SpeciesCode, "be", "bb", "ba","mn","bp","bw")

v_all_deploy <- ggplot(filter(vol_master_data, TotalTagTime_h > 2 & TotalLunges > 0 & sonar_exp =="none"),
                      aes(x = fct_reorder(SpeciesCode, EngulfVolPerHr, fun = median), y = EngulfVolPerHr, color = SpeciesCode, shape = SpeciesCode)) + 
  geom_point(inherit.aes = T, alpha = 0.8, position = position_jitter(width = .25)) + 
  geom_boxplot(inherit.aes = T, guides = FALSE, outlier.shape = NA, alpha = 0) +
  facet_grid(.~PreyClean, scales = "free_x") +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = Shape) +
  scale_y_log10(labels = scales::comma, limits = c(5000, 5000000)) + 
  xlab("Species") + ylab("Filtration capacity (liters per hour)") + 
  ggtitle("Water filtered per individual per hour (all delpoyments >2 hours)") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1, face="italic"),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12))
v_all_deploy + theme(legend.position="none") +
  scale_x_discrete(labels=c("ba" = "B. acutorostrata", "be" = "B. edeni", "bb" = "B. bonaerensis", "mn" = "M. novaeangliae", "bp" = "B. physalus", "bw" = "B. musculus")) 


# summary results table for water filtration per individual per hour
vol_summ <- vol_master_data %>% 
    group_by(SpeciesCode) %>% 
    filter(TotalTagTime_h > 2 & TotalLunges > 0 & sonar_exp =="none", PreyClean == "Krill-feeding") %>% 
    summarize(MeanWaterFiltered = mean(EngulfVolPerHr),
              SEWaterFiltered = SE(EngulfVolPerHr))



v_24_deploy <- ggplot(filter(vol_master_data, TotalTagTime_h > 24 & TotalLunges > 0 & sonar_exp =="none", PreyClean =="Krill-feeding"),
                      aes(x = reorder(SpeciesCode, EngulfVolPerHr, FUN = median), y = EngulfVolPerHr*24, color = SpeciesCode, shape = SpeciesCode)) + 
  geom_point(inherit.aes = T, alpha = 0.8, position = position_jitter(width = .25)) + 
  geom_boxplot(inherit.aes = T, guides = FALSE, outlier.shape = NA, alpha = 0) +
  facet_grid(.~PreyClean, scales = "free_x") +
  scale_colour_manual(values = pal) +
  scale_y_log10(labels = scales::comma) + 
  scale_shape_manual(values = Shape) +
  xlab("Species") + ylab("VFD (liters per day)") + 
  ggtitle("Water filtered per individual per day (tags on >24 hours)") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1, face="italic"),
        axis.text.y = element_text(size=12),       
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12))
v_24_deploy + theme(legend.position="none") +
  scale_x_discrete(labels=c("ba" = "B. acutorostrata", "be" = "B. edeni", "bb" = "B. bonaerensis", "mn" = "M. novaeangliae", "bp" = "B. physalus", "bw" = "B. musculus")) 


# now for varying hours feeding within a day

vol_master_for_join <- vol_master_data %>% 
  mutate(dummy = 1)
vol_master_varying_HrperD <- tibble(hours_feeding = seq(1,12,1), dummy = 1) %>% 
  full_join(vol_master_for_join, by = "dummy") %>% 
  select(-dummy) %>% 
  mutate(TotalWaterFiltered = hours_feeding*EngulfVolPerHr)

SpCodetoName <- c("bb" = "B. bonaerensis", 
                  "be" = "B. edeni", 
                  "bw" = "B. musculus", 
                  "bp" = "B. physalus", 
                  "mn" = "M. novaeangliae")

v_HrperDfish <- ggplot(filter(vol_master_varying_HrperD, TotalTagTime_h > 2 & TotalLunges > 0 & sonar_exp =="none", PreyClean == "Fish-feeding"),
                       aes(x = hours_feeding, y = TotalWaterFiltered, color = SpeciesCode, shape = SpeciesCode)) + 
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.3) + 
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), color = "black", size = 0.5) +
  facet_grid(.~SpeciesCode, labeller = as_labeller(SpCodetoName)) +
  scale_colour_manual(values = pal,
                      labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_shape_manual(values = Shape,
                     labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_x_continuous(breaks=seq(0, 12, 3)) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Hours feeding") + ylab("Total water filtered (liters)") + 
  ggtitle("Water filtered per individual per day (all deployments >2 hours)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12, face = "italic"))
v_HrperDfish + theme(legend.position="none")

vol_master_varying_HrperD$SpeciesCode <- fct_relevel(vol_master_varying_HrperD$SpeciesCode, "be", "bb","mn","bp","bw")
v_HrperDkrill <- ggplot(filter(vol_master_varying_HrperD, TotalTagTime_h > 2 & TotalLunges > 0 & sonar_exp =="none", PreyClean == "Krill-feeding"),
                       aes(x = hours_feeding, y = TotalWaterFiltered, color = SpeciesCode, shape = SpeciesCode)) + 
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.3) + 
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), color = "black", size = 0.5) +
  facet_grid(.~SpeciesCode, labeller = as_labeller(SpCodetoName)) +
  scale_colour_manual(values = pal,
                      labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_shape_manual(values = Shape,
                     labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_x_continuous(breaks=seq(0, 12, 3)) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Hours feeding") + ylab("Total water filtered (liters)") + 
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12, face = "italic"))
v_HrperDkrill + theme(legend.position="none")

ggarrange(v_HrperDfish, v_HrperDkrill, 
          labels = c("A", "B"), # THIS IS SO COOL!!
          legend = "none",
          ncol = 1, nrow = 2)


# annual amount of water filtered per individual, varying days
vol_master_for_year_join <- vol_master_varying_HrperD %>% 
  mutate(dummy = 1)
vol_master_varying_DperYr <- tibble(days_feeding = seq(60,182.5,10), dummy = 1) %>% 
  full_join(vol_master_for_year_join, by = "dummy") %>% 
  select(-dummy) %>% 
  mutate(TotalAnnualWaterFiltered = days_feeding*TotalWaterFiltered)
 

v_DperYrfish <- ggplot(filter(vol_master_varying_DperYr, hours_feeding %in% (6:12) & TotalTagTime_h > 2 & TotalLunges > 0 & sonar_exp =="none", PreyClean == "Fish-feeding"),
                       aes(x = days_feeding, y = TotalAnnualWaterFiltered, color = SpeciesCode, shape = SpeciesCode)) +
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.2) +
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), color = "black", size = 0.5) +
  facet_grid(.~SpeciesCode, labeller = as_labeller(SpCodetoName)) +
  scale_colour_manual(values = pal,
                      labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_shape_manual(values = Shape,
                     labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Days feeding") + ylab("Total water filtered (liters)") +
  ggtitle("Water filtered per individual per year (all deployments >2 hours)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12, face = "italic"))
v_DperYrfish + theme(legend.position="none")

vol_master_varying_DperYr$SpeciesCode <- fct_relevel(vol_master_varying_DperYr$SpeciesCode, "be", "bb","mn","bp","bw")
v_DperYrkrill <- ggplot(filter(vol_master_varying_DperYr, hours_feeding %in% (6:12) & TotalTagTime_h > 2 & TotalLunges > 0 & sonar_exp =="none", PreyClean == "Krill-feeding"),
                       aes(x = days_feeding, y = TotalAnnualWaterFiltered, color = SpeciesCode, shape = SpeciesCode)) +
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.2) +
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), color = "black", size = 0.5) +
  facet_grid(.~SpeciesCode, labeller = as_labeller(SpCodetoName)) +
  scale_colour_manual(values = pal,
                      labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_shape_manual(values = Shape,
                     labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Days feeding") + ylab("Total water filtered (liters)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12, face = "italic"))
v_DperYrkrill + theme(legend.position="none")

ggarrange(v_DperYrfish, v_DperYrkrill, 
          labels = c("A", "B"), # THIS IS SO COOL!!
          legend = "none",
          ncol = 1, nrow = 2)
  
# annual amount of water filtered current global populartion, varying days
v_DperYrfish_currentpop <- ggplot(filter(vol_master_varying_DperYr, hours_feeding %in% (6:12) & TotalTagTime_h > 2 & TotalLunges > 0 & sonar_exp =="none", PreyClean == "Fish-feeding"),
                       aes(x = days_feeding, y = TotalAnnualWaterFiltered*`Population estimate`, color = SpeciesCode, shape = SpeciesCode)) +
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.2) +
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), color = "black", size = 0.5) +
  facet_grid(.~SpeciesCode, labeller = as_labeller(SpCodetoName)) +
  scale_colour_manual(values = pal,
                      labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_shape_manual(values = Shape,
                     labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Days feeding") + ylab("Total water filtered (liters)") +
  ggtitle("Water filtered annually by current global population (all deployments >2 hours)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12, face = "italic"))
v_DperYrfish_currentpop + theme(legend.position="none")

v_DperYrkrill_currentpop <- ggplot(filter(vol_master_varying_DperYr, hours_feeding %in% (6:12) & TotalTagTime_h > 2 & TotalLunges > 0 & sonar_exp =="none", PreyClean == "Krill-feeding"),
                        aes(x = days_feeding, y = TotalAnnualWaterFiltered*`Population estimate`, color = SpeciesCode, shape = SpeciesCode)) +
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.2) +
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), color = "black", size = 0.5) +
  facet_grid(.~SpeciesCode, labeller = as_labeller(SpCodetoName)) +
  scale_colour_manual(values = pal,
                      labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_shape_manual(values = Shape,
                     labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Days feeding") + ylab("Total water filtered (liters)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12, face = "italic"))
v_DperYrkrill_currentpop + theme(legend.position="none")


ggarrange(v_DperYrfish_currentpop, v_DperYrkrill_currentpop, 
          labels = c("A", "B"), # THIS IS SO COOL!!
          legend = "none",
          ncol = 1, nrow = 2)


# annual amount of water filtered historical global population, varying days
v_DperYrfish_histpop <- ggplot(filter(vol_master_varying_DperYr, hours_feeding %in% (6:12) & TotalTagTime_h > 2 & TotalLunges > 0 & sonar_exp =="none", PreyClean == "Fish-feeding"),
                                  aes(x = days_feeding, y = TotalAnnualWaterFiltered*`Total removed`, color = SpeciesCode, shape = SpeciesCode)) +
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.2) +
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), color = "black", size = 0.5) +
  facet_grid(.~SpeciesCode, labeller = as_labeller(SpCodetoName)) +
  scale_colour_manual(values = pal,
                      labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_shape_manual(values = Shape,
                     labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Days feeding") + ylab("Total water filtered (liters)") +
  ggtitle("Annual filtration capacity removed by 20th century whaling (all deployments >2 hours)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12, face = "italic"))
v_DperYrfish_currentpop + theme(legend.position="none")

v_DperYrkrill_histpop <- ggplot(filter(vol_master_varying_DperYr, hours_feeding %in% (6:12) & TotalTagTime_h > 2 & TotalLunges > 0 & sonar_exp =="none", PreyClean == "Krill-feeding"),
                                   aes(x = days_feeding, y = TotalAnnualWaterFiltered*`Total removed`, color = SpeciesCode, shape = SpeciesCode)) +
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.2) +
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), color = "black", size = 0.5) +
  facet_grid(.~SpeciesCode, labeller = as_labeller(SpCodetoName)) +
  scale_colour_manual(values = pal,
                      labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_shape_manual(values = Shape,
                     labels = c("B. acutorostrata", "B. bonaerensis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Days feeding") + ylab("Total water filtered (liters)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12, face = "italic"))
v_DperYrkrill_histpop + theme(legend.position="none")


ggarrange(v_DperYrfish_histpop, v_DperYrkrill_histpop, 
          labels = c("A", "B"), # THIS IS SO COOL!!
          legend = "none",
          ncol = 1, nrow = 2)


###########################################
# preliminary plots for prey consumption
########################################### 
# plot predictions from literature, with ours
pal <- c("Balaenoptera acutorostrata" = "gold3", "Balaenoptera bonaerensis" = "firebrick3", "Balaenoptera edeni" = "darkorchid3", "Balaenoptera borealis" = "black",  "Megaptera novaeangliae" = "gray30", "Balaenoptera physalus" = "chocolate3", "Balaenoptera musculus" = "dodgerblue2" )
Shape <- c("Balaenoptera acutorostrata" = 10, "Balaenoptera bonaerensis" = 15, "Balaenoptera edeni" = 8, "Balaenoptera borealis" = 7, "Megaptera novaeangliae" = 17, "Balaenoptera physalus" = 18, "Balaenoptera musculus" = 19)


ingest_directpredict_plot <- ggplot(prey_predict_w_M, aes(log10(M_kg), log10(R))) +
  geom_line(data = filter(prey_predict_w_M, !`Reference(s)` %in%  c("Savoca et al., this study (lower bound)", 
                                                                    "Savoca et al., this study (upper bound)",
                                                                    "Savoca et al., this study (best estimate)")), 
            aes(color = str_wrap(`Reference(s)`, 20)), size = 1.15) +
  # geom_line(data = filter(prey_predict_w_M, `Reference(s)` == "Savoca et al., this study (best estimate)"),
  #           color = str_wrap("dodgerblue4", 20), size = 1.15, linetype = "dashed") +
  # geom_line(data = filter(prey_predict_w_M, `Reference(s)` == "Savoca et al., this study (lower bound)"),
  #           color = str_wrap("dodgerblue2", 20), size = 1.15, linetype = "dotted") +
  # geom_line(data = filter(prey_predict_w_M, `Reference(s)` == "Savoca et al., this study (upper bound)"),
  #           color = str_wrap("dodgerblue2", 20), size = 1.15, linetype = "dotted") +
  # annotation_custom(rastBw, xmin = 4.5, xmax = 5.2, ymin = 3.5, ymax = 4.3) +
  # annotation_custom(rastMn, xmin = 4.1, xmax = 4.5, ymin = 3.15, ymax = 3.7) +
  # annotation_custom(rastBb, xmin = 3.7, xmax = 3.9, ymin = 2.5, ymax = 3) +
  #ylim(2.5,4.5) +
  labs(x = "log[Body mass (kg)]", y ="log[Daily ration (R) in kg]", color = "Reference(s)") +
  theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.key.height = unit(1, "cm")) 
ingest_directpredict_plot

#Save pdf of plot
dev.copy2pdf(file="Ingest_predict_plot_woSavocaLines.pdf", width=13, height=8)


ingest_MSpredict_plot <- ggplot(BMRtoFMRprojection, aes(log10(M_kg), log10(R), color = Species, shape = Species)) +
  geom_point() + 
  #geom_smooth() +
  scale_colour_manual(values = pal,
                      labels = c("B. acutorostrata", "B. bonaerensis", "B. borealis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  scale_shape_manual(values = Shape,
                     labels = c("B. acutorostrata", "B. bonaerensis", "B. borealis", "B. edeni", "B. musculus", "B. physalus", "M. novaeangliae")) +
  #ylim(2.5,4.5) +
  labs(x = "log[Body mass (kg)]", y ="log[Daily ration (R) in kg]") +
  theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))
ingest_MSpredict_plot


# plot of prey consumed per hour (krill only; all deployments >2 hours)
# Prepare data
Prey_consumpt_hr <- cetacean_data %>% 
  filter(TotalTagTime_h > 2, SpeciesCode %in% c("bw", "ba", "bp", "mn", "bb") & sonar_exp %in% c("none", NA)) %>%  
  select(ID, Species, feeding_rate, wgtMeanNULL_wt_g:medBOUT_E, TotalTagTime_h, `Population estimate`, `Total removed`) %>%
  gather(scenario, prey_wgt_g, c(wgtMeanNULL_wt_g, medNULL_wt_g, wgtMeanBOUT_wt_g, medBOUT_wt_g)) %>% 
  #  gather(scenario_E, prey_E, c(wgtMeanNULL_E, medNULL_E, wgtMeanBOUT_E, medBOUT_E)) %>%  Switch these as necessary
  mutate(SpeciesCode = substr(ID,1,2), 
        hourly_prey_in_g = prey_wgt_g * feeding_rate,
         hourly_prey_in_kg = hourly_prey_in_g/1000,
         scenario_type = ifelse(scenario %in% c("wgtMeanNULL_wt_g", "medNULL_wt_g"), "NULL", "BOUT"),
         calc_type = ifelse(scenario %in% c("wgtMeanNULL_wt_g", "wgtMeanBOUT_wt_g"), "mean", "med")) %>% 
  mutate_if(is.character, as.factor) %>% 
  unite("scenario_calc", c("scenario_type", "calc_type"), sep = "_", remove = FALSE)
Prey_consumpt_hr$SpeciesCode <- sub("ba", "bb", Prey_consumpt_hr$SpeciesCode)
Prey_consumpt_hr$Species <- sub("Balaenoptera acutorostrata", "Balaenoptera bonaerensis", Prey_consumpt_hr$Species)
Prey_consumpt_hr <- mutate(Prey_consumpt_hr, Binomial = abbr_binom(Species))

Prey_consumpt_hr %>% group_by(SpeciesCode) %>% summarise(n_distinct(ID)) %>% View

#now plot
pal <- c("ba" = "gold3", "bb" = "firebrick3", "be" = "darkorchid3",  "mn" = "gray30", "bp" = "chocolate3", "bw" = "dodgerblue2" )
Shape <- c("ba" = 10, "bb" = 15, "be" = 8, "mn" = 17, "bp" = 18, "bw" = 19)
Prey_consumpt_hr$SpeciesCode <- fct_relevel(Prey_consumpt_hr$SpeciesCode, "bb", "ba","mn","bp","bw")

Prey_consumpt_hr_plot <- ggplot(Prey_consumpt_hr, aes(x = fct_reorder(Binomial, hourly_prey_in_kg, fun = median), 
                                                      y = hourly_prey_in_kg, 
                                                         color = SpeciesCode, shape = SpeciesCode)) +
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.8, position = position_jitterdodge(jitter.width=0.9)) + 
  geom_boxplot(inherit.aes = T, aes(group = SpeciesCode), guides = FALSE, outlier.shape = NA, alpha = 0) +
  facet_grid(.~scenario_calc) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = Shape) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Species") + ylab("Prey consumption per hour (kg)") + 
  ggtitle("Krill consumed per individual per hour (all deployments >2 hours)") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1, face = "italic"),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12))
Prey_consumpt_hr_plot + theme(legend.position="none")

#summary table for results section
prey_summ <- Prey_consumpt_hr %>% 
  group_by(Species, scenario_calc) %>% 
  summarize(MeanKrillConsumed_kg = mean(hourly_prey_in_kg),
            SEMeanKrillConsumed_kg = SE(hourly_prey_in_kg))


#boxplot of prey consumed per day (tags on >24h)
preyconsumpt_24_deploy <- ggplot(filter(Prey_consumpt_hr, TotalTagTime_h > 24), 
                                 aes(x = fct_reorder(Binomial, hourly_prey_in_kg, fun = median), y = hourly_prey_in_kg*24, 
                                                       color = SpeciesCode, shape = SpeciesCode)) +
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.8, position = position_jitterdodge(jitter.width=0.9)) + 
  geom_boxplot(inherit.aes = T, aes(group = SpeciesCode), guides = FALSE, outlier.shape = NA, alpha = 0) +
  facet_grid(.~scenario_calc) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = Shape) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Species") + ylab("Prey consumption per day (kg)") + 
  ggtitle("Krill consumed per day (tags on >24 hours)") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1, face = "italic"),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12))
preyconsumpt_24_deploy + theme(legend.position="none")


# now for varying hours feeding within a day
prey_master_for_join <- Prey_consumpt_hr %>% 
  mutate(dummy = 1)
prey_master_varying_HrperD <- tibble(hours_feeding = seq(1,12,1), dummy = 1) %>% 
  full_join(prey_master_for_join, by = "dummy") %>% 
  select(-dummy) %>% 
  mutate(TotalPreyConsumed_kg = hours_feeding*hourly_prey_in_kg)

prey_HrperDkrill <- ggplot(filter(prey_master_varying_HrperD, TotalTagTime_h > 2),
                        aes(x = hours_feeding, y = TotalPreyConsumed_kg, color = SpeciesCode, shape = SpeciesCode)) + 
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.3) + 
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), size = 0.5) +
  facet_grid(Binomial~scenario_calc) +
  scale_colour_manual(name = "Species",
                      values = pal, 
                      labels = c( "B. bonaerensis", "M. novaeangliae", "B. physalus", "B. musculus")) +
  scale_shape_manual(name = "Species",
                     values = Shape,
                     labels = c( "B. bonaerensis", "M. novaeangliae", "B. physalus", "B. musculus")) +
  scale_x_continuous(breaks=seq(0, 12, 3)) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Hours feeding") + ylab("Total prey consumed (kg)") +
  ggtitle("Krill consumed per day (all deployments >2 hours)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 10, face="italic"))
prey_HrperDkrill + theme(legend.position = "none")


# now for varying days feeding within a year
prey_master_for_year_join <- prey_master_varying_HrperD %>% 
  mutate(dummy = 1)
prey_master_varying_DperYr <- tibble(days_feeding = seq(60,182.5,10), dummy = 1) %>% 
  full_join(prey_master_for_year_join, by = "dummy") %>% 
  select(-dummy) %>% 
  mutate(TotalAnnualPreyConsumed_kg = days_feeding*TotalPreyConsumed_kg)

prey_DperYrkrill <- ggplot(filter(prey_master_varying_DperYr, hours_feeding %in% (6:12) & TotalTagTime_h > 2),
                        aes(x = days_feeding, y = (TotalAnnualPreyConsumed_kg)/0.83, color = SpeciesCode, shape = SpeciesCode)) +  # multiply by 1.17 to get estimate of MAC, see Eq. (8) 
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.2) +
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), color = "black", size = 0.75) +
  facet_grid(Binomial~scenario_calc) +
  scale_colour_manual(name = "Species",
                      values = pal, 
                      labels = c( "B. bonaerensis", "B. acutorostrata", "M. novaeangliae", "B. physalus", "B. musculus")) +
  scale_shape_manual(name = "Species",
                     values = Shape,
                     labels = c( "B. bonaerensis", "B. acutorostrata", "M. novaeangliae", "B. physalus", "B. musculus")) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Days of high-intensity feeding") + ylab("Total prey consumed (kg)") +
  ggtitle("Krill consumed per year (all deployments >2 hours)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 10, face="italic"))
prey_DperYrkrill + theme(legend.position="none")



# annual amount of krill consumed, current global population, varying days
prey_DperYrkrill_currentpop <- ggplot(filter(prey_master_varying_DperYr, hours_feeding %in% (6:12) & TotalTagTime_h > 2),
                           aes(x = days_feeding, y = (TotalAnnualPreyConsumed_kg/0.83)*`Population estimate`, 
                               color = SpeciesCode, shape = SpeciesCode)) +  # divide by 0.83 to get estimate of MAC, see Eq. (8) 
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.2) +
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), color = "black", size = 0.75) +
  facet_grid(Binomial~scenario_calc) +
  scale_colour_manual(name = "Species",
                      values = pal, 
                      labels = c( "B. bonaerensis", "B. acutorostrata", "M. novaeangliae", "B. physalus", "B. musculus")) +
  scale_shape_manual(name = "Species",
                     values = Shape,
                     labels = c( "B. bonaerensis", "B. acutorostrata", "M. novaeangliae", "B. physalus", "B. musculus")) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Days of high-intensity feeding") + ylab("Total prey consumed (kg)") +
  ggtitle("Krill consumed annually by current global population (all deployments >2 hours)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 10, face="italic"))
prey_DperYrkrill_currentpop + theme(legend.position="none")

# annual amount of krill consumed, population removed by 20th century whaling, varying days
prey_DperYrkrill_histpop <- ggplot(filter(prey_master_varying_DperYr, hours_feeding %in% (6:12) & TotalTagTime_h > 2),
                                      aes(x = days_feeding, y = (TotalAnnualPreyConsumed_kg/0.83)*`Total removed`, 
                                          color = SpeciesCode, shape = SpeciesCode)) +  # divide by 0.83 to get estimate of MAC, see Eq. (8) 
  geom_point(inherit.aes = T, aes(group = SpeciesCode), alpha = 0.2) +
  geom_smooth(inherit.aes = T, aes(group = SpeciesCode), color = "black", size = 0.75) +
  facet_grid(Binomial~scenario_calc) +
  scale_colour_manual(name = "Species",
                      values = pal, 
                      labels = c( "B. bonaerensis", "B. acutorostrata", "M. novaeangliae", "B. physalus", "B. musculus")) +
  scale_shape_manual(name = "Species",
                     values = Shape,
                     labels = c( "B. bonaerensis", "B. acutorostrata", "M. novaeangliae", "B. physalus", "B. musculus")) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Days of high-intensity feeding") + ylab("Total prey consumed (kg)") +
  ggtitle("Annual lost krill consumption due to 20th century whaling (all deployments >2 hours)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 10, face="italic"))
prey_DperYrkrill_histpop + theme(legend.position="none")


# fish density of 7.8 kg m-3 (based on length-weight relationships from Rodríguez‐Romero et al. 2009, 
#                             a school packing density density of 1 body length cubed per fish, 
#                             and a representative fish length of 12 cm), 
# and energy density of 6 MJ/kg (Tirelli et al. 2006; Dubreuil & Petitgas 2009) were used.

#Prepare data
Anch_data <- vol_master_data %>% 
  filter(SpeciesCode == "mn" & PreyClean == "Fish-feeding" & Study_Area %in% c("Monterey", "SoCal")) %>% 
  mutate(Med_Recalc_m3 = Med_Recalc_L/1000,
         EngulfVolPerHr_m3 = LungesPerDayHour*Med_Recalc_m3,
         Max_anch_mouthful_kg = EngulfVolPerHr_m3*7.8,
         Max_anch_perhour_kg = Max_anch_mouthful_kg*LungesPerDayHour)
Anch_data_for_join <- Anch_data %>% 
  mutate(dummy = 1)
Anch_master_varying_HrperD <- tibble(hours_feeding = seq(1,12,1), dummy = 1) %>% 
  full_join(Anch_data_for_join, by = "dummy") %>% 
  select(-dummy) %>% 
  filter(TotalTagTime_h > 2 & TotalLunges > 0) %>% 
  mutate(MaxDailyAnchConsumed_kg = hours_feeding*Max_anch_perhour_kg)

pal <- c("ba" = "gold3", "bb" = "firebrick3", "be" = "darkorchid3",  "mn" = "gray30", "bp" = "chocolate3", "bw" = "dodgerblue2" )
Shape <- c("ba" = 10, "bb" = 15, "be" = 8, "mn" = 17, "bp" = 18, "bw" = 19)

Anch_consumpt_hr_plot <- ggplot(Anch_master_varying_HrperD, aes(x = hours_feeding, y = MaxDailyAnchConsumed_kg*0.4)) +
  geom_point(color = "gray30", shape = 17, alpha = 0.8) + 
  geom_smooth(inherit.aes = T, color = "blue", size = 0.75) +
  scale_x_continuous(breaks=seq(0, 12, 2)) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Daytime hours feeding") + ylab("Anchovy consumption per day (kg)") + 
  ggtitle("Humpback whale anchovy consumption (all deployments >2 hours)") +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"))
Anch_consumpt_hr_plot + theme(legend.position="none")
  
Anch_master_for_year_join <- Anch_master_varying_HrperD %>% 
  mutate(dummy = 1)
Anch_master_varying_DperYr <- tibble(days_feeding = seq(60,182.5,10), dummy = 1) %>% 
  full_join(Anch_master_for_year_join, by = "dummy") %>% 
  select(-dummy) %>% 
  mutate(MaxAnnualAnchConsumed_kg = days_feeding*MaxDailyAnchConsumed_kg)


Anch_consumpt_yr_plot <- ggplot(filter(Anch_master_varying_DperYr, hours_feeding %in% (6:12)), 
                                aes(x = days_feeding, y = MaxAnnualAnchConsumed_kg*0.4)) +
  geom_point(color = "gray30", shape = 17, alpha = 0.8) + 
  geom_smooth(inherit.aes = T, color = "blue", size = 0.75) +
  scale_y_log10(labels = scales::comma) + 
  xlab("Days feeding") + ylab("Anchovy consumption per year (kg)") + 
  theme_bw() +
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"))
Anch_consumpt_yr_plot + theme(legend.position="none")

ggarrange(Anch_consumpt_hr_plot, Anch_consumpt_yr_plot, 
          labels = c("A", "B"), # THIS IS SO COOL!!
          legend = "none",
          ncol = 1, nrow = 2)

#################################################################################
# for funsies, some back of the envelope calcs on filtraton capacity comparisons:
#################################################################################

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


# for the Great Lakes, see: https://www.glerl.noaa.gov/education/ourlakes/lakes.html
# the whaled population of blue and fin whales each filtered approximately the volume of Lake Ontario per year, which is 1,639,000,000,000,000


##########################
# Prelim stats exploration
##########################

######################
# Older code below here
######################

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

