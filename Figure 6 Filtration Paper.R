############# Dive Time vs TL ###############


install.packages("tidyverse")
library(tidyverse)

DiveTime <- read.csv("C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 2- Filtration/Figure 6 filtration paper data.csv") %>%
  filter(TL > 6) %>% 
  filter(Lunge_Count>0) %>% 
  filter(Mean_Depth >8) %>% # min minke body length
  mutate(SpeciesCode = substr(ID, 2, 3),
         ID = str_remove_all(ID, "[']"),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"))    
#raw data 


#raw data graph as points
DiveTimebyTL_points <- ggplot() +
  geom_point(data = DiveTime, aes (y = log10(Dive_Length), x = log10(TL), shape=SciName, color = Mean_Depth, size = Mean_Depth), alpha = 0.8)+  
  scale_color_gradientn(colours = c("skyblue2",
                                    "deepskyblue2",
                                    "dodgerblue2", "royalblue",
                                    "mediumblue", "navy", "midnightblue")) +
  labs(x = "log Total Length (m)",
       y = "log Dive Length") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  guides(color=guide_legend("Lunge Depth (m)")) +
  guides(size=guide_legend("Lunge Depth (m)")) +
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10, 
                                   face="italic"))

DiveTimebyTL_points


#graph as raincloud

DiveTime_summary <- DiveTime


DiveTimebyTL_cloud <- ggplot( data = DiveTime, aes(y = log10(Dive_Length), x = log10(TL), shape = SciName, color = SciName, fill= SciName)) +
  geom_violin(trim = FALSE) 

DiveTimebyTL_cloud  #want to add mean 


