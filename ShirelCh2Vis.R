####################################
## Analyses and visualizations for Shirel's second chapter
####################################

library(ggplot2)
library(dplyr)
library(data.table)
library(readxl)
library(forcats)

# read in data
f_data <- read_excel("ALLPRHS 12.17.2018.xls")
v_data <- read_excel("mwmrmeasures.xlsx")

#removing unwanted columns and rows
f_data <- within(f_data, rm(notes))
#f_data <- filter(f_data, prey != "Milk")  # not working, also deleting rows with NAs
v_data <- v_data[,c(1:3)] #keeps only columns 1-3

# add a species column using the first two letters of Individual ID
f_data$Species <- substr(f_data$ID,1,2)

# adding columns
f_data$TotalLunges <- f_data$dayhourslunges + 
                      f_data$nighthourslunges + 
                      f_data$twilightzonelunges
f_data$TotalHours <- f_data$dayhours + f_data$nighthours + f_data$twilightzone
f_data$LungesPerHour <- f_data$TotalLunges/f_data$TotalHours
f_data$LungesPerDayHour <- f_data$dayhourslunges/f_data$dayhours
f_data$LungesPerNightHour <- f_data$nighthourslunges/f_data$nighthours
f_data$LungesPerTwHour <- f_data$twilightzonelunges/f_data$twilightzone
f_data$Length <- as.numeric(f_data$Length)

v_data$L <- v_data$MW*0.9766  #creates column that converts MW (kg) to liters

# create table looking at averages of MW 
v_data_species <- v_data %>% group_by(Species) %>% 
                  summarize(Mean_L = mean(L), Med_L = median(L), 
                            Mean_TL = mean(TLm), Med_TL = median(TLm))

# adding column with average engulfment volume by species, averages from v_data_species
f_data$EngulfmentVolume <- ifelse(f_data$Species == "be", 15498.377,
                                  ifelse(f_data$Species == "bw", 55349.402, 
                                         ifelse(f_data$Species == "bp", 56682.353,
                                                ifelse(f_data$Species == "mn",25091.473, 2755.317))))

f_data$EngulfVolPerHr <- f_data$LungesPerHour*f_data$EngulfmentVolume

# change level ordering
f_data$Species <- as.factor(f_data$Species)
f_data$Species <- fct_relevel(f_data$Species, "be","bw","bp","mn","bb")


#####################################
## Plots
#####################################

# preliminary plots for feeding rates for deployments of >2 total hours
f_data$Species <- fct_relevel(f_data$Species, "bb","be","mn","bp","bw")
p1 <- ggplot(filter(f_data, TotalHours > 2 & prey == "Krill" & TotalLunges > 0), aes(x = Species, y = LungesPerHour, shape = Species)) + 
            geom_point(inherit.aes = T) + geom_jitter(inherit.aes = T) + geom_boxplot(inherit.aes = T, alpha = 0.3) +
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


