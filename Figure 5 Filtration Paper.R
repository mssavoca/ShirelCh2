## Figure 5 ########

# load packages and data ----

library(tidyverse)

LungesPerDive_water <- read.csv("Figure 5 filtration paper data.csv") %>%
  filter(TL > 6) %>% 
  filter(Lunge_Count>0) %>% 
  mutate(SpeciesCode = substr(ID, 2, 3),
         ID = str_remove_all(ID, "[']"),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"))    
#raw data 
  
# data tidying ----  

LungesPerDive_water_summary <- LungesPerDive_water %>% ## add species
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


# plotting ----

#total water per dive (classed by depth) by total length raw data style, not mass specific 

TotalWaterByTL <- ggplot() +
  geom_point(data = LungesPerDive_water, aes (y = log10(Total.Water.Per.Dive..kg.), x = log10(TL), shape=SciName, color = Mean_Depth, size = Mean_Depth), alpha = 0.8)+  
  scale_color_gradientn(colours = c("skyblue2",
                                    "deepskyblue2",
                                    "dodgerblue2", "royalblue",
                                    "mediumblue", "navy", "midnightblue")) +
  labs(x = "log Total Length (m)",
       y = "log Total Water Per Dive (s)") +
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
  geom_point(data = LungesPerDive_water, aes (y = log10(MR), x = log10(TL), shape=SciName, color = Mean_Depth, size = Mean_Depth), alpha = 0.8)+  
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
  geom_point(data = LungesPerDive_MR_summary, aes(x = fct_reorder(ID, MR_mean), y = MR_mean, color = ))+
  theme(axis.text.x = element_text(angle = 90))
MR



