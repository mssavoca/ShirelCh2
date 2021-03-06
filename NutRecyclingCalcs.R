##############################################################
# Calculation of nutrient recycling potential of baleen whales 
##############################################################


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
# to compute prey intake per day when feeding if ALL feeding is compressed into 120 days
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


# read in whale population data. Current data from IUCN 2019 Redlist, whaleing data compiled in Rocha et al. 2014
pop_data <- read_excel("Filtration project_Whale population and feeding information.xlsx", sheet = 1)

# load data
d_full_NULL <- read_csv("Cetacea model output NULL_EXTANT.csv") %>% 
  mutate(Species = ifelse(Species == "bonarensis", "bonaerensis", Species))
d_full_BOUT <- read_csv("Cetacea model output BOUT_EXTANT.csv") %>% 
  mutate(Species = ifelse(Species == "bonarensis", "bonaerensis", Species))

f_data <- read_excel("ALLPRHS 2.5.2019.xls")

tag_guide <- read_excel("TAG GUIDE_2.18.19.xlsx")
tag_guide=tag_guide[c(2:nrow(tag_guide)),] ##get rid of first two rows
colnames(tag_guide)=as.character(unlist(tag_guide[1,])) # makes first row the header
tag_guide=tag_guide[-1,] # remove first row, leaving header

v_data <- read_excel("mwmrmeasures.xlsx")
v_data <- v_data[,c(1:3)] #keeps only columns 1-3
names(v_data)[names(v_data) == 'Species'] <- 'CommonName'
v_data$L <- v_data$MW*0.9766  #creates column that converts MW (kg) to liters

# creating fucntions from Shirel's paper for MW (in kg) for engulfment capacity in liters for each species where we have a known length
bw_L <- function(x) {
  (x^3.667316*10^-0.014078)*0.9766}   # change to 0.9737098 CHANGE!!!

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

#joining f_data and tag guide to have location data
f_data <- left_join(f_data, tag_guide[ , c("ID", "Study_Area     _")], by = "ID")
names(f_data)[names(f_data) == "Study_Area     _"] <- "Study_Area"
f_data$Study_Area <- replace_na(f_data$Study_Area, "SoCal")   # Replace NAs in location with "SoCal"
f_data <- within(f_data, rm(notes)) #removing unwanted columns and rows
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
f_data$LungesPerDayHour[is.nan(f_data$LungesPerDayHour)] <- NA
f_data$LungesPerTwHour[is.nan(f_data$LungesPerTwHour)] <- NA
f_data$LungesPerNightHour[is.nan(f_data$LungesPerNightHour)] <- NA

RorqualData <- read_csv("lunge_rates_from_Paolo.csv") %>% 
  left_join(f_data, by= "ID") %>% 
  mutate(`deployment-time_h` = `deployment-time_secs`/60/60,
         SpeciesCode = substr(ID,1,2),)  %>% 
  select(-c(species)) %>% 
  arrange(dayhours, ID)
RorqualData$SpeciesCode[RorqualData$SpeciesCode == "ba"] <- "bb"

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
  left_join(v_data_species, by = "CommonName") %>% 
  mutate(EngulfVolPerHr = LungesPerHour*Med_Recalc_L,
         EngulfVolPerDayHr = LungesPerDayHour*Med_Recalc_L) %>% 
  select(-c(Med_L, Mean_L)) %>% 
  left_join(pop_data, by = "SpeciesCode") %>% 
  #drop_na(Species) %>% 
  mutate(feeding_rate = TotalFeedingEvents / TotalTagTime_h,    # FEEDING RATE
         SpeciesCode = substr(ID,1,2),
         geoMeanNULL_wt_g_DC = case_when(
           Species =="Balaenoptera musculus" ~ 0.629646079*79862.436,        # kg to g multiply by 1000, m3 to l divide by 1000, so they cancel each other out; The second number is the Med_recalc_L from vol_data_species
           Species =="Balaenoptera physalus" ~ 0.620866221*60010.441,        # data from BaleenWhaleForagingDistBigKrill100Bins.xlsx
           Species =="Megaptera novaeangliae" ~ 0.608799363*25683.815,
           Species =="Balaenoptera bonaerensis" ~ 0.498822479*3069.295),
         MedNULL_wt_g_DC = case_when(
           Species =="Balaenoptera musculus" ~ 0.705480231*79862.436,        #kg to g multiply by 1000, m3 to l divide by 1000, so they cancel each other out; The second number is the Med_recalc_L from vol_data_species
           Species =="Balaenoptera physalus" ~ 0.657933225*60010.441,        # data from BaleenWhaleForagingDistBigKrill100Bins.xlsx
           Species =="Megaptera novaeangliae" ~ 0.657933225*25683.815,
           Species =="Balaenoptera bonaerensis" ~ 0.497702356*3069.295), 
         geoMeanBOUT_wt_g_DC = case_when(
           Species =="Balaenoptera musculus" ~ 1.665454025*79862.436,        #kg to g multiply by 1000, m3 to l divide by 1000, so they cancel each other out; The second number is the Med_recalc_L from vol_data_species
           Species =="Balaenoptera physalus" ~ 1.895174907*60010.441,        # data from BaleenWhaleForagingDistBigKrillBout100Bins.xlsx
           Species =="Megaptera novaeangliae" ~ 1.365715958*25683.815,
           Species =="Balaenoptera bonaerensis" ~ 0.555868207*3069.295),
         MedBOUT_wt_g_DC = case_when(
           Species =="Balaenoptera musculus" ~ 1.873817423*79862.436,        #kg to g multiply by 1000, m3 to l divide by 1000, so they cancel each other out; The second number is the Med_recalc_L from vol_data_species
           Species =="Balaenoptera physalus" ~ 2.15443469*60010.441,        # data from BaleenWhaleForagingDistBigKrillBout100Bins.xlsx
           Species =="Megaptera novaeangliae" ~ 1.519911083*25683.815,
           Species =="Balaenoptera bonaerensis" ~ 0.572236766*3069.295)
  )      
#rename(Species = Species.y)
cetacean_data$SpeciesCode <- sub("ba", "bb", cetacean_data$SpeciesCode)
cetacean_data$Species <- sub("Balaenoptera acutorostrata", "Balaenoptera bonaerensis", cetacean_data$Species)

Prey_consumpt_hr <- cetacean_data %>% 
  filter(TotalTagTime_h > 2, Species %in% c("Balaenoptera musculus", "Balaenoptera physalus", "Megaptera novaeangliae", "Balaenoptera bonaerensis") & sonar_exp %in% c("none", NA)) %>%  
  select(ID, Species, feeding_rate, wgtMeanNULL_wt_g:medBOUT_E, TotalTagTime_h, `Population estimate`, `Total removed`,
         geoMeanNULL_wt_g_DC, MedNULL_wt_g_DC, geoMeanBOUT_wt_g_DC, MedBOUT_wt_g_DC) %>%
  gather(scenario, prey_wgt_g, c(wgtMeanNULL_wt_g, medNULL_wt_g, wgtMeanBOUT_wt_g, medBOUT_wt_g,
                                 geoMeanNULL_wt_g_DC, MedNULL_wt_g_DC, geoMeanBOUT_wt_g_DC, MedBOUT_wt_g_DC)) %>% 
  #  gather(scenario_E, prey_E, c(wgtMeanNULL_E, medNULL_E, wgtMeanBOUT_E, medBOUT_E)) %>%  Switch these as necessary
  mutate(SpeciesCode = substr(ID,1,2), 
         hourly_prey_in_g = prey_wgt_g * feeding_rate,
         hourly_prey_in_kg = hourly_prey_in_g/1000,
         scenario_type = ifelse(scenario %in% c("wgtMeanNULL_wt_g", "medNULL_wt_g", "geoMeanNULL_wt_g_DC", "MedNULL_wt_g_DC"), "NULL", "BOUT"),
         calc_type = ifelse(scenario %in% c("wgtMeanNULL_wt_g", "wgtMeanBOUT_wt_g", "geoMeanNULL_wt_g_DC", "geoMeanBOUT_wt_g_DC"), "mean", "med")) %>% 
  mutate_if(is.character, as.factor) %>% 
  unite("scenario_calc", c("scenario_type", "calc_type"), sep = "_", remove = FALSE)
Prey_consumpt_hr$SpeciesCode <- sub("ba", "bb", Prey_consumpt_hr$SpeciesCode)
Prey_consumpt_hr$Species <- sub("Balaenoptera acutorostrata", "Balaenoptera bonaerensis", Prey_consumpt_hr$Species)
Prey_consumpt_hr <- mutate(Prey_consumpt_hr, Binomial = abbr_binom(Species))

# now for varying hours feeding within a day
prey_master_for_join <- Prey_consumpt_hr %>% 
  mutate(dummy = 1)
prey_master_varying_HrperD <- tibble(hours_feeding = seq(6,12,1), dummy = 1) %>%  # assumed a feeding day was feeding for 6-12 hours
  full_join(prey_master_for_join, by = "dummy") %>% 
  select(-dummy) %>% 
  mutate(TotalPreyConsumed_kg = hours_feeding*hourly_prey_in_kg)

excreta_day_master <- prey_master_varying_HrperD %>% 
  filter(scenario %in% c("geoMeanNULL_wt_g_DC", "MedNULL_wt_g_DC", "geoMeanBOUT_wt_g_DC", "MedBOUT_wt_g_DC")) %>% 
  mutate(total_feces_kg = TotalPreyConsumed_kg*0.8,    # From Doughty et al. 2016, Roman and McCarthy 2010
         total_Fe_recycled_kg = total_feces_kg*0.000146,    # whale fecal average from Ratnarajah et al. 2014
         total_N_recycled_kg = total_feces_kg*0.0056,     # see Roman et al. 2016, converted moles PON to g to kg  
         total_P_recycled_kg = total_feces_kg*0.0089)     # whale fecal average from Ratnarajah et al. 2014

# different scenarios
## consider that 90% of ingested iron is excreted** My 2014 paper. So consider iron reservoir in krill consumed by whales
## consider total feces excreted, likely around 75-80% wet weight of what's consumed (see Doughty et al. 2016)
### multiply total weight of feces by concentration of different elements in Fe

pal <- c("ba" = "gold3", "bb" = "firebrick3", "be" = "darkorchid3",  "mn" = "gray30", "bp" = "chocolate3", "bw" = "dodgerblue2" )
Shape <- c("ba" = 10, "bb" = 15, "be" = 8, "mn" = 17, "bp" = 18, "bw" = 19)

ExcretedFe_HrperD <- ggplot(filter(excreta_day_master, TotalTagTime_h > 2 & scenario %in% c("geoMeanNULL_wt_g_DC", "MedNULL_wt_g_DC", "geoMeanBOUT_wt_g_DC", "MedBOUT_wt_g_DC")),
                           aes(x = hours_feeding, y = total_Fe_recycled_kg, color = SpeciesCode, shape = SpeciesCode)) + 
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
  xlab("Hours feeding") + ylab("Total Fe excreted (kg)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 10, face="italic"))
ExcretedFe_HrperD + theme(legend.position = "none")


# now for varying days feeding within a year
excreta_day_master_year_join <- excreta_day_master %>% 
  mutate(dummy = 1)
excreta_year_master <- tibble(days_feeding = seq(60,182.5,10), dummy = 1) %>% 
  full_join(excreta_day_master_year_join, by = "dummy") %>% 
  select(-dummy) %>% 
  mutate(TotalAnnualPreyConsumed_kg = days_feeding*TotalPreyConsumed_kg,
         total_annual_feces_kg = TotalAnnualPreyConsumed_kg*0.8,    # From Doughty et al. 2016, Roman and McCarthy 2010
         total_annual_Fe_recycled_kg = total_annual_feces_kg*0.000146,    # whale fecal average from Ratnarajah et al. 2014
         total_annual_N_recycled_kg = total_annual_feces_kg*0.0056,     # see Roman et al. 2016, converted moles PON to g to kg  
         total_annual_P_recycled_kg = total_annual_feces_kg*0.0089)    # whale fecal average from Ratnarajah et al. 2014

NutRecTableDisc <- excreta_year_master %>% filter(days_feeding == 120) %>% 
  group_by(Binomial) %>% 
  summarise(Individual_Fe = median(total_annual_Fe_recycled_kg),
            Individual_Fe_SE = SE(total_annual_Fe_recycled_kg),
            Population_Fe = median(total_annual_Fe_recycled_kg*`Population estimate`),
            Population_Fe_SE = SE(total_annual_Fe_recycled_kg*`Population estimate`),
            Individual_N = median(total_annual_N_recycled_kg),
            Individual_N_SE = SE(total_annual_N_recycled_kg),
            Population_N = median(total_annual_N_recycled_kg*`Population estimate`),
            Population_N_SE = SE(total_annual_N_recycled_kg*`Population estimate`),
            Individual_P = median(total_annual_P_recycled_kg),
            Individual_P_SE = SE(total_annual_P_recycled_kg),
            Population_P = median(total_annual_P_recycled_kg*`Population estimate`),
            Population_P_SE = SE(total_annual_P_recycled_kg*`Population estimate`))
View(NutRecTableDisc)

ExcretedFe_DperYr <- ggplot(filter(excreta_year_master, TotalTagTime_h > 2 & scenario %in% c("geoMeanNULL_wt_g_DC", "MedNULL_wt_g_DC", "geoMeanBOUT_wt_g_DC", "MedBOUT_wt_g_DC")),
                            aes(x = days_feeding, y = total_annual_Fe_recycled_kg, color = SpeciesCode, shape = SpeciesCode)) + 
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
  xlab("days feeding") + ylab("Total Fe excreted (kg)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 10, face="italic"))
ExcretedFe_DperYr + theme(legend.position = "none")





# estimating amount of food consumed over the course of a feeding season (see Lockyer 2007).
# using the equation Nr_ind = (MDC*prop_excreted)*nut_conc
nutrient_sims <- crossing(TotalPreyConsumed_kg = 18500,
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

