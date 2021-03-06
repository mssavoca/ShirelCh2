## Figure 5 ########

# Packages and Functions ----

install.packages("plot3D")
library("plot3D")
library(tidyverse)
library(lme4)
library(MCMCglmm)

#Input Data ----
#raw data
TotalWaterPerDive <- read.csv("Figure 5 filtration paper data.csv") %>%
  filter(TL > 6) %>% 
  filter(Lunge_Count>0) %>% 
  mutate(SpeciesCode = substr(ID, 2, 3),
         ID = str_remove_all(ID, "[']"),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"))    
 
  
# Data tidying ----  

TotalWaterPerDive_summary <- TotalWaterPerDive %>% ## add species
  mutate(SpeciesCode = substr(ID, 1, 2),
         ID = str_remove_all(ID, "[']"),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis")) %>% 
  summarise(MR_mean = mean(MR),
            MW_mean = mean(MW))
 
### Summaries ####
#ID Summary 

TotalWaterPerDive_ID_Summary <- TotalWaterPerDive %>% 
  group_by(ID) %>% 
  summarise(MW_mean = mean(MW),
            MR_mean = mean(MR),
            TL= mean(TL)) %>% 
  mutate(SpeciesCode = substr(ID, 1, 2))


#Species Summary
TotalWaterPerDive_Species_Summary <- TotalWaterPerDive %>% 
  group_by(SpeciesCode) %>% 
  summarise(MW_max = max(MW),
            MR_max = max(MR),
            TL = mean(TL))


TotalWater_IDMeans <- ggplot(data = TotalWaterPerDive_ID_Summary, 
                       aes(x=TL, y= MR_mean)) +
  geom_point(aes(shape = SpeciesCode, 
                 color = SpeciesCode),
             size=5) +
  geom_smooth(method = lm) +
  labs(x = "Total Length (m)",
       y = "Max Mass-Specific") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  guides(shape=guide_legend("Species")) +
  guides(color=guide_legend("Species"))+
  theme(legend.text = element_text(size=10, 
                                   face="italic")) 

TotalWater_IDMeans

TotalWater_IDMeans_glm <- lmer(log10(MR_mean) ~ log10(TL) + (1|SpeciesCode), 
                         data = TotalWaterPerDive_ID_Summary)
summary(LungeMeans_glm) #slope is -0.07794


IDLungeMeanslm <- lm(log10(MR_mean) ~ log10(TL), data = TotalWaterPerDive_ID_Summary)
summary(IDLungeMeanslm) #slope is 1.1168


#Species Means

TotalWater_SpeciesMeans <- ggplot(data = TotalWaterPerDive_Species_Summary, 
                             aes(x=TL, y= MR_max)) +
  geom_point(aes(shape = SpeciesCode, 
                 color = SpeciesCode),
             size=5) +
  geom_smooth(method = lm) +
  labs(x = "Total Length (m)",
       y = "Max Mass-Specific") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  guides(shape=guide_legend("Species")) +
  guides(color=guide_legend("Species"))+
  theme(legend.text = element_text(size=10, 
                                   face="italic")) 

TotalWater_SpeciesMeans

# TotalWater_SpeciesMeans_glm <- lmer(log10(MR_max) ~ log10(TL) + (1|SpeciesCode), 
#                                data = TotalWaterPerDive_Species_Summary)
# summary(TotalWater_SpeciesMeans_glm) #can't work with so few


SpeciesLungeMeanslm <- lm(log10(MR_max) ~ log10(TL), data = TotalWaterPerDive_Species_Summary)
summary(SpeciesLungeMeanslm) #slope is 0.06841



# Plotting ----

#total water per dive (classed by depth) by total length raw data style, not mass specific 

TotalWaterByTL <- ggplot(data = TotalWaterPerDive , aes (x = log10(TL), y = log10(Total.Water.Per.Dive..kg.))) +
  geom_point(aes(shape = SciName,
                 color = SciName,
                 size = 4)) +  
  geom_smooth(method= lm) +
  # scale_color_gradientn(colours = c("skyblue2",
  #                                   "deepskyblue2",
  #                                   "dodgerblue2", "royalblue",
  #                                   "mediumblue", "navy", "midnightblue")) +
  labs(x = "log Total Length (m)",
       y = "log Total Water Per Dive (kg)") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  guides(color=guide_legend("Species")) +
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10, 
                                   face="italic"))

TotalWaterByTL

TotalWaterByTL_lm <- lm(log10(Total.Water.Per.Dive..kg.) ~ log10(TL), data = TotalWaterPerDive)
summary(TotalWaterByTL_lm )



# Stats for TotalWaterByTL 

MWlmer <- lmer(log10(Total.Water.Per.Dive..kg.) ~ log10(TL) + (1|ID), 
           data = TotalWaterPerDive)
summary(MWlmer) # slope of purge 1 is 1.79075, has no problem converging 


#MWlm <- lm(log10(Total.Water.Per.Dive..kg.) ~ log10(TL), data = TotalWaterPerDive)
#summary(MWlm) #slope of purge 1 is 1.93951

MCMCglmm_MW_TL <- MCMCglmm(log10(Total.Water.Per.Dive..kg.) ~ log10(TL),
                           random = ~ ID,
                           data = TotalWaterPerDive, 
                           family = "gaussian",
                           nitt = 11000, thin = 1, burnin = 1000,
                           pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                           verbose = TRUE)
summary(MCMCglmm_MW_TL)



### total water per dive divided by mass, by TL, raw data style

TotalWater_MassSpecific <- ggplot(data = TotalWaterPerDive, 
                                  aes (y = log10(MR), x = log10(TL))) +
  geom_point(aes(shape=SciName, color = SciName, size = 4)) +
  geom_smooth(method = lm) +
  # scale_color_gradientn(colours = c("skyblue2",
  #                                   "deepskyblue2",
  #                                   "dodgerblue2", "royalblue",
  #                                   "mediumblue", "navy", "midnightblue")) +
  labs(x = "log Total Length (m)",
       y = "log MR") +
  #geom_abline(slope = 1.10363, intercept =  -0.98516, linetype = "solid", color="red") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  labs(x = "log Total Length (m)") +
  labs(y = "log Mass-Specific Water Per Dive") +
  guides(color=guide_legend("Lunge Depth (m)")) +
  guides(size=guide_legend("Lunge Depth (m)")) +
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10, 
                                   face="italic"))

TotalWater_MassSpecific

TotalWater_MassSpecific_lm <- lm(log10(MR) ~ log10(TL), data = TotalWaterPerDive)
summary(TotalWater_MassSpecific_lm )



# Stats for TotalWaterByTL 


MRlmer <- lmer(log10(MR) ~ log10(TL) + (1|ID), 
               data = TotalWaterPerDive)
summary(MRlmer) #

MRlm <- lm(log10(MR) ~ log10(TL), data = TotalWaterPerDive)
summary(MRlm) #

MCMCglmm_MR_TL <- MCMCglmm(log10(MR) ~ log10(TL),
                           random = ~ ID,
                           data = TotalWaterPerDive, 
                           family = "gaussian",
                           nitt = 11000, thin = 1, burnin = 1000,
                           pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                           verbose = TRUE)
summary(MCMCglmm_MR_TL)



## this graph is how many more times than body size is the water swallowed on average per lunge
MR <- ggplot() +
  geom_point(data = TotalWaterPerDive_summary, aes(x = fct_reorder(ID, MR_mean), y = MR_mean, color = ))+
  theme(axis.text.x = element_text(angle = 90))
MR




# GLMM  ----

# TotalWaterPerDive_GLMM <- TotalWaterPerDive %>% 
#   filter(Mean_Depth >50) %>% 
#   mutate(SpeciesCode = substr(ID, 1, 2),
#          ID = str_remove_all(ID, "[']"),
#          SciName = case_when(
#            SpeciesCode == "bw" ~ "Balaenoptera musculus",
#            SpeciesCode == "bp" ~ "Balaenoptera physalus",
#            SpeciesCode == "mn" ~ "Megaptera novaeangliae",
#            SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"),
#          TL_z = as.numeric(scale(TL)),
#          Mean_Depth_z = as.numeric(scale(Mean_Depth)),
#          MW_z = as.numeric(scale(MW)))
# 
# hist(log10(TotalWaterPerDive_GLMM$TL))
# hist(log10(TotalWaterPerDive_GLMM$Mean_Depth))
# hist(log10(TotalWaterPerDive_GLMM$MW))
# 
# 
# hist(scale(TotalWaterPerDive_GLMM$TL_z))


# # trying a 3D plot ----
# #very basic, but you'll get the general idea
# 
# scatter3D(x = log10(LungesPerDive_water$TL), y = log10(LungesPerDive_water$MR), z = LungesPerDive_water$Mean_Depth, phi = 0, bty ="g",
#           xlab = "log Total Length (m)",
#           ylab ="log Total Water Per Dive (s)", # are the unite correct here?
#           zlab = "Lunge Depth (m)")
# 
# # for more info, and to  customize, see: http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization



