## Figure 5 ########

# Packages and FUnctions ----

install.packages("plot3D")
library("plot3D")
library(tidyverse)

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
  group_by(ID) %>% 
  summarise(MR_mean = mean(MR),
            MW_mean = mean(MW))


# Plotting ----

#total water per dive (classed by depth) by total length raw data style, not mass specific 

TotalWaterByTL <- ggplot() +
  geom_point(data = TotalWaterPerDive, aes (y = log10(Total.Water.Per.Dive..kg.), x = log10(TL), shape=SciName, color = Mean_Depth, size = Mean_Depth), alpha = 0.8)+  
  scale_color_gradientn(colours = c("skyblue2",
                                    "deepskyblue2",
                                    "dodgerblue2", "royalblue",
                                    "mediumblue", "navy", "midnightblue")) +
  labs(x = "log Total Length (m)",
       y = "log Total Water Per Dive (kg)") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  guides(color=guide_legend("Lunge Depth (m)")) +
  guides(size=guide_legend("Lunge Depth (m)")) +
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10, 
                                   face="italic"))

TotalWaterByTL

### total water per dive, made mass specific by TL, raw data style
TotalWater_MassSpecific <- ggplot() +
  geom_point(data = TotalWaterPerDive, 
             aes (y = log10(MR), x = log10(TL), 
                  shape=SciName, color = Mean_Depth, size = Mean_Depth), alpha = 0.8)+  
  scale_color_gradientn(colours = c("skyblue2",
                                    "deepskyblue2",
                                    "dodgerblue2", "royalblue",
                                    "mediumblue", "navy", "midnightblue")) +
  labs(x = "log Total Length (m)",
       y = "log MR") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  guides(color=guide_legend("Lunge Depth (m)")) +
  guides(size=guide_legend("Lunge Depth (m)")) +
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10, 
                                   face="italic"))

TotalWater_MassSpecific


## this graph is how many more times than body size is the water swallowed on average per lunge
MR <- ggplot() +
  geom_point(data = TotalWaterPerDive_summary, aes(x = fct_reorder(ID, MR_mean), y = MR_mean, color = ))+
  theme(axis.text.x = element_text(angle = 90))
MR




# GLMM  ----

TotalWaterPerDive_GLMM <- TotalWaterPerDive %>% 
  filter(Mean_Depth >50) %>% 
  mutate(SpeciesCode = substr(ID, 1, 2),
         ID = str_remove_all(ID, "[']"),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"),
         TL_z = as.numeric(scale(TL)),
         Mean_Depth_z = as.numeric(scale(Mean_Depth)),
         MW_z = as.numeric(scale(MW)))

hist(log10(TotalWaterPerDive_GLMM$TL))
hist(log10(TotalWaterPerDive_GLMM$Mean_Depth))
hist(log10(TotalWaterPerDive_GLMM$MW))


hist(scale(TotalWaterPerDive_GLMM$TL_z))


# trying a 3D plot ----
#very basic, but you'll get the general idea

scatter3D(x = log10(LungesPerDive_water$TL), y = log10(LungesPerDive_water$MR), z = LungesPerDive_water$Mean_Depth, phi = 0, bty ="g",
          xlab = "log Total Length (m)",
          ylab ="log Total Water Per Dive (s)", # are the unite correct here?
          zlab = "Lunge Depth (m)")

# for more info, and to  customize, see: http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization



