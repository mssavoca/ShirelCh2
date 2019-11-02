# Figure 3 Filtration Paper

library(data.table)
library(readxl)
library(forcats)
library(tidyverse)
library(ggstance)
library(mgcv)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggpubr)


# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus
abbr_binom <- function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}

SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}

LungesPerDive_raw_csv <- read_csv("C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 2- Filtration/Figure 2 filtration paper data.csv")

LungesPerDive_raw <- LungesPerDive_raw_csv %>% 
  filter(Lunge_Count >0, TL >6) %>% 
  mutate(SpeciesCode = substr(ID, 2, 3),
         ID = str_remove_all(ID, "[']"),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"), 
         AvgLength = case_when(
           SciName == "Balaenoptera musculus" ~ "22",
           SciName == "Balaenoptera physalus" ~ "17",
           SciName == "Megaptera novaeangliae" ~ "11",
           SciName == "Balaenoptera bonaerensis" ~ "8"), 
         dive_50 = ifelse(Mean_Depth <= 50, "N", "Y"),
         dive_100 = ifelse(Mean_Depth <= 100, "N", "Y"),
         dive_150 = ifelse(Mean_Depth <= 150, "N", "Y")) %>% 
  pivot_longer(cols = dive_50:dive_150, names_to = "depthcat", values_to = "response") %>% 
  group_by(ID) %>% 
  mutate(foragingdivecount = n_distinct(Dive_Num),
         depthcat = factor(depthcat),
         response = factor(response)) 
        
         

LungesPerDive_summary <- LungesPerDive_raw %>% 
  group_by(ID) %>% 
  filter(deep_dive == "Y") %>% 
  summarise(lunge_count_mean = mean(Lunge_Count),
            lunge_count_median = median(Lunge_Count),
            lunge_count_max = max(Lunge_Count),
            foragingdivecount = n_distinct(Dive_Num)) %>% 
  mutate(SpeciesCode = substr(ID, 1, 2))
  


species_summary <- LungesPerDive_raw %>% 
  filter(response == "Y") %>% 
  group_by(depthcat, SpeciesCode) %>% 
  summarise(wgt_mean = weighted.mean(Lunge_Count, foragingdivecount),
            SE_lunge_count = SE(Lunge_Count))




############## Plot Fig 2 raw data ###################

Fig2 <-  ggplot(data = filter(LungesPerDive_raw, deep_dive == "Y"), 
                aes(x=TL, y= Lunge_Count, # shape = abbr_binom(SciName), 
                    color = Mean_Depth, size = Mean_Depth), alpha = 0.8) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_color_gradientn(colours = c("skyblue2",
                                    "deepskyblue2",
                                    "dodgerblue2", "royalblue",
                                    "mediumblue", "navy", "midnightblue"), #blues to mimic ocean depth
                        name = "Mean Depth (m)") +
  scale_size_continuous(range = c(0.5, 4)) +
  #ylim(0, 1.5) +
  labs(x = "log Total Length (m)",
       y = "Lunges Per Dive") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  guides(color=guide_legend("Lunge Depth (m)")) +
  guides(size=guide_legend("Lunge Depth (m)")) +
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10, 
                                   face="italic")) 
  

Fig2


############### PLot Fig 2 species summary ##############

lungesperdive_bar <- ggplot(data = species_summary, 
                            aes( x= reorder(SpeciesCode, -wgt_mean), y = wgt_mean, fill = depthcat)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Species",
       y = "Weighted Mean, Lunges Per Dive") +
  theme_classic()
lungesperdive_bar





