####################################
## Analyses and visualizations for Shirel's second chapter
####################################

library(ggplot2)
library(dplyr)
library(data.table)
library(readxl)
library(forcats)
library(tidyverse)
library(mgcv)
library(rpart)

# read in data
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
  separate(prey, into = c("Prey", "Prey notes"), sep = " ")


# combining dataframes into what we need
fv_data <- f_data %>% 
  left_join(v_data_species, by = "CommonName") %>% 
  mutate(EngulfVolPerHr = LungesPerHour*Med_Recalc_L,
         EngulfVolPerDayHr = LungesPerDayHour*Med_Recalc_L) %>% 
  select(-c(Med_L, Mean_L))


# # change level ordering
# f_data$Species <- as.factor(f_data$Species)
# f_data$Species <- fct_relevel(f_data$Species, "be","bw","bp","mn","bb")


##########################
# Prelim stats exploration
##########################

# GAMMs with both Odontocetes and Mysticetes in the model
feeding_rate_model <- aov(LungesPerHour ~ Species+Prey+Study_Area,  data=filter(f_data, Prey =="Krill"))

feeding_rate_model <- rpart(LungesPerHour ~ Species+Prey+Study_Area, data=f_data, method = "class", cp =)

### $gam to look at gam effects. $lme to look at random effects.
summary(feeding_rate_model)
TukeyHSD(feeding_rate_model)

plot.gam(feeding_rate_model)

library(car)
Anova(feeding_rate_model, type = 3)

#####################################
## Plots
#####################################

# preliminary plots for feeding rates for deployments of >2 total hours
f_data$Species <- fct_relevel(f_data$Species, "bb","be","mn","bp","bw")
p1 <- ggplot(filter(fv_data, TotalHours > 2 & Prey == "Krill" & TotalLunges > 0), aes(x = Species, y = LungesPerHour, shape = Species)) + 
            geom_point(inherit.aes = T) + geom_jitter(inherit.aes = T) + 
            geom_boxplot(inherit.aes = T, alpha = 0.3, outlier.size = 0) +
            ylab("Number of lunges per hour") + theme_bw()
p1 + scale_x_discrete(labels=c("bb" = "minke\nwhale", "mn" = "humpback\nwhale", "bp" = "fin\nwhale", "bw" ="blue\nwhale")) +
    theme(legend.position="none")


p1_mn <- ggplot(filter(f_data, Species == "mn" & TotalHours > 2 & prey != "Milk" & TotalLunges > 0), 
                aes(x = prey, y = LungesPerHour, shape = prey)) + 
  geom_point(inherit.aes = T) + geom_jitter(inherit.aes = T) + geom_boxplot(inherit.aes = T, alpha = 0.3) +
  theme_bw()
p1_mn


p2 <- ggplot(filter(f_data, TotalHours > 2 & prey == "Krill"), aes(x = Species, y = LungesPerDayHour, shape = Species)) + 
  geom_point(inherit.aes = T) + geom_jitter(inherit.aes = T) + geom_boxplot(inherit.aes = T, alpha = 0.3) +
  theme_bw()
p2


p3 <- ggplot(f_data, aes(x = Species, y = LungesPerNightHour, shape = Species)) + 
  geom_point(inherit.aes = T) + geom_jitter(inherit.aes = T) + geom_boxplot(inherit.aes = T, alpha = 0.3) +
  theme_bw()
p3


# feeding rates by total length
MeasuredWhales <- filter(fv_data, TotalHours > 2 & Prey == "Krill" & TotalLunges > 0)
p4 <- ggplot(MeasuredWhales, aes(Length, EmpEngulfCap)) +
            geom_point(inherit.aes = T, aes(shape = CommonName)) +  
            geom_smooth(method = lm) +
            theme_bw()
p4

summary(p4)

# these premliminary results are counterinuitive, but relationship is significant
m1 <- lm(MeasuredWhales$LungesPerHour~MeasuredWhales$Length)
summary(m1)

m2 <- lm(MeasuredWhales$LungesPerDayHour~MeasuredWhales$Length)
summary(m2)


# preliminary plots for filtration capacity
fv_data$Species <- fct_relevel(fv_data$Species, "be","bb","mn","bp","bw")
v1 <- ggplot(filter(fv_data, TotalHours > 2 & Prey == "Krill" & TotalLunges > 0), aes(x = Species, y = EngulfVolPerDayHr, shape = Species)) + 
  geom_point(inherit.aes = T) + geom_jitter(inherit.aes = T) + geom_boxplot(inherit.aes = T, alpha = 0.3) +
  ylab("Filtration capacity (liters per hour)") + theme_bw()
v1 + scale_x_discrete(labels=c("bb" = "minke\nwhale", "mn" = "humpback\nwhale", "bp" = "fin\nwhale", "bw" ="blue\nwhale")) +
  theme(legend.position="none")



###############################################
# for funsies, some back of the envelope calcs:
###############################################

# volume of water in upper 300m of Monterey Bay
((pi*20000^2*300)/2)*1000 # this equals 1.884956e+14 (9.42478e+13 for half cylinder), a blue whale filters ~1000000 per hour, or 24000000 per day
1.884956e+14/12000000 # the number of blue whale days it takes to filter the bay

# for the world's oceans, see: https://www.ngdc.noaa.gov/mgg/global/etopo1_ocean_volumes.html

# in the Southern Ocean
(1335000000*0.1)*1000000000000  # total volume in L of water in the upper 10% of water column (~368m) Southern Ocean 
2000*12000000 # approximate amount of water filtered by current population of Southern Ocean blue whales 
350000*12000000 # approximate amount of water filtered by pre-exploitation population of Southern Ocean blue whales 
1.335e+20/4.2e+12
