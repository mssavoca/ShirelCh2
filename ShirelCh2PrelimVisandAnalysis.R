####################################
## Analyses and visualizations for Shirel's second chapter
####################################

library(ggplot2)
library(dplyr)
library(data.table)
library(readxl)
library(forcats)
library(tidyverse)
library(mgsub)
library(mgcv)
library(gam.plot)
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

v_data <- read_excel("mwmrmeasures.xlsx")

#removing unwanted columns and rows
f_data <- within(f_data, rm(notes))

#f_data <- filter(f_data, prey != "Milk")  # not working, also deleting rows with NAs
v_data <- v_data[,c(1:3)] #keeps only columns 1-3
names(v_data)[names(v_data) == 'Species'] <- 'CommonName'
v_data$L <- v_data$MW*0.9766  #creates column that converts MW (kg) to liters

# create table looking at averages of MW 
v_data_species <- v_data %>% group_by(CommonName) %>% 
  summarize(Mean_L = mean(L), Med_L = median(L), 
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
        Length = as.numeric(gsub(" m", "", f_data$whaleLength))) %>% 
  select(-whaleLength) %>% 
  separate(prey, into = c("Prey", "Prey notes"), sep = " ")


f_data$CommonName = ifelse(f_data$Species == "bw", "Blue Whale",
                           ifelse(f_data$Species == "bp", "Fin Whale",
                                  ifelse(f_data$Species == "mn", "Humpback Whale",
                                         ifelse(f_data$Species == "bb", "Minke Whale", "Bryde's Whale"))))

# adding column with average engulfment volume by species, averages from v_data_species
f_data$EngulfmentVolume <- ifelse(f_data$Species == "be", 15498.377,
                                  ifelse(f_data$Species == "bw", 55349.402, 
                                         ifelse(f_data$Species == "bp", 56682.353,
                                                ifelse(f_data$Species == "mn",25091.473, 2755.317))))

f_data$EngulfVolPerHr <- f_data$LungesPerHour*f_data$EngulfmentVolume

# change level ordering
f_data$Species <- as.factor(f_data$Species)
f_data$Species <- fct_relevel(f_data$Species, "be","bw","bp","mn","bb")

# Prelim stats, GAMMs

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
p1 <- ggplot(filter(f_data, TotalHours > 2 & Prey == "Krill" & TotalLunges > 0), aes(x = Species, y = LungesPerHour, shape = Species)) + 
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
MeasuredWhales <- filter(f_data, TotalHours > 2 & prey == "Krill" & Species == "bw" & TotalLunges > 0)
p4 <- ggplot(MeasuredWhales, aes(Length, LungesPerDayHour)) +
            geom_point(inherit.aes = T, shape = MeasuredWhales$Species) +  
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
f_data$Species <- fct_relevel(f_data$Species, "be","bb","mn","bw","bp")
v1 <- ggplot(filter(f_data, TotalHours > 2 & prey == "Krill" & TotalLunges > 0), aes(x = Species, y = EngulfVolPerHr, shape = Species)) + 
  geom_point(inherit.aes = T) + geom_jitter(inherit.aes = T) + geom_boxplot(inherit.aes = T, alpha = 0.3) +
  ylab("Filtration capacity (liters per hour)") + theme_bw()
v1 + scale_x_discrete(labels=c("bb" = "minke\nwhale", "mn" = "humpback\nwhale", "bp" = "fin\nwhale", "bw" ="blue\nwhale")) +
  theme(legend.position="none")


