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
library(lme4)
library(nlme)
library(lmerTest)

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
  separate(prey, into = c("Prey", "Prey notes"), sep = " ") %>% 
  drop_na(Prey) %>% 
  filter(!Prey %in% c("Milk", "N")) %>% 
  mutate(PreyClean = case_when(
    Prey %in% c("Anchovies", "Fish", "Herring", "SandLance", "Sardines")  ~ "Fish-feeding",
    Prey %in% c("Inverts", "Krill") ~ "Krill-feeding"
  ))  



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

a = filter(fv_data, TotalHours >2 & TotalLunges >0, PreyClean =="Krill")
hist(log(a$LungesPerHour))

# log transformed fgorcing it to be normal, so no need for a generalized linear model
m1 <- lm(log(LungesPerHour + 1) ~ PreyClean, data = a)
summary(m1)
TukeyHSD(aov(m1))

m2 <- lm(log(LungesPerHour + 1) ~ Species, data = filter(a, PreyClean == "Fish"))
summary(m2)
TukeyHSD(aov(m2))

m3 <- lmer(log(LungesByDaySection + 1) ~ DaySection + (1|ID),
            filter(f_data_daysection, TotalHours > 2 & PreyClean == "Krill-feeding" & CommonName %in% c("Blue Whale", "Humpback Whale")))
summary(m3)
OR

m3_nlme <- lme(log(LungesByDaySection + 1) ~ DaySection ,random=~1|ID, 
               data=filter(f_data_daysection, TotalHours > 2 & PreyClean == "Krill-feeding" & CommonName %in% c("Blue Whale", "Humpback Whale")))
anova(m3_nlme)


u=filter(a,PreyClean=="Krill")
m3 <- lm(log(LungesPerHour + 1) ~ Species, data = a)
summary(m3)
TukeyHSD(aov(m3))



# m2 <- lm(log(LungesPerHour + 1) ~ Species*PreyClean, data = a)
# summary(m2)
# 
# m3 <- lm(log(LungesByDaySection + 1) ~ Species*PreyClean + DaySection, data = filter(f_data, TotalHours >2))
# TukeyHSD(aov(m3))
# 
# # GAMMs with both Odontocetes and Mysticetes in the model
# feeding_rate_model <- aov(LungesPerHour ~ Species + Prey + Study_Area,  data=filter(f_data, Prey =="Krill"))
# 
# feeding_rate_model <- rpart(LungesPerHour ~ Species+Prey+Study_Area, data=f_data, method = "class", cp =)
# 
# ### $gam to look at gam effects. $lme to look at random effects.
# summary(feeding_rate_model)
# TukeyHSD(feeding_rate_model)
# 
# plot.gam(feeding_rate_model)
# 
# library(car)
# Anova(feeding_rate_model, type = 3)

#####################################
## Plots
#####################################
pal <- c("Minke Whale" = "firebrick3", "Bryde's Whale" = "darkorchid3",  "Humpback Whale" = "gray30", "Fin Whale" = "chocolate3", "Blue Whale" = "dodgerblue2" )
Shape <- c("Minke Whale" = 15, "Bryde's Whale" = 8,  "Humpback Whale" = 17, "Fin Whale" = 18, "Blue Whale" = 19 )

# preliminary plots for feeding rates for deployments of >2 total hours

LungebyPrey <- ggplot(filter(f_data, TotalHours > 2 & TotalLunges > 0), aes(x = PreyClean, y = LungesPerHour)) + 
  geom_point(inherit.aes = T, alpha = 0.8, position = position_jitter(width = .25)) + 
  geom_boxplot(inherit.aes = T, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  ylab("Lunges per hour") + xlab("Prey type") + 
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
LungebyPrey


f_data$Species <- as.factor(fct_relevel(f_data$Species, "be","bw","bp","mn","bb"))

LungebySpecies <- ggplot(filter(f_data, TotalHours > 2 & TotalLunges > 0), aes(x = Species, y = LungesPerHour, shape = CommonName, color =  CommonName)) + 
  geom_point(inherit.aes = T, alpha = 0.8, position = position_jitter(width = .25)) + 
  geom_boxplot(inherit.aes = T, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  facet_grid(.~PreyClean, scales = "free_x") +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values=Shape) +
  ylab("Lunges per hour") + xlab("Species") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, size = 16), 
        strip.text.x = element_text(size = 12))
LungebySpecies + scale_x_discrete(labels=c("be" = "Bryde's whale", "mn" = "humpback whale", "bw" = "blue whale", "bp" = "fin whale", "bb" = "minke whale")) +
    theme(legend.position="none")



f_data_daysection <-  gather(f_data, DaySection, LungesByDaySection, 19:21)
f_data$LungesByDaySection[f_data$LungesByDaySection == "NaN"] <- 0 

LungebyDaySection <- ggplot(filter(f_data_daysection, TotalHours > 2 & PreyClean == "Krill-feeding" & CommonName %in% c("Blue Whale", "Humpback Whale")), 
                            aes(x = DaySection, y = LungesByDaySection, shape = CommonName, color =  CommonName)) + 
  geom_point(inherit.aes = T, alpha = 0.8, position = position_jitter(width = .25)) + 
  geom_boxplot(inherit.aes = T, guides = FALSE, outlier.shape = NA, alpha = 0.5, aes(group = DaySection)) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values=Shape) +
  ylab("Lunges per hour") + xlab("Section of day") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, size = 16), 
        strip.text.x = element_text(size = 12))
LungebyDaySection + scale_x_discrete(labels=c("be" = "Bryde's whale", "mn" = "humpback whale", "bw" = "blue whale", "bp" = "fin whale", "bb" = "minke whale")) +
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
p4 <- ggplot(MeasuredWhales, aes(Length, LungesPerDayHour)) +
            geom_point(inherit.aes = T) +  
            geom_smooth(method = lm) +
            theme_bw()
p4

summary(p4)



# # these premliminary results are counterinuitive, but relationship is significant
# m1 <- lm(MeasuredWhales$LungesPerHour~MeasuredWhales$Length)
# summary(m1)
# 
# m2 <- lm(MeasuredWhales$LungesPerDayHour~MeasuredWhales$Length)
# summary(m2)

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

v1 <- ggplot(filter(fv_data, TotalHours > 2 & Prey != "Krill" & TotalLunges > 0), aes(x = Species, y = EngulfVolPerDayHr, color = CommonName)) + 
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
