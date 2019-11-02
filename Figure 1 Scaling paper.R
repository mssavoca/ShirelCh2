#Figure 1 Filtration Paper

## Engulfment Groups

library(plyr)
library(smatr)
library(ggplot2)
library(tidyverse)

engulfment <- read.csv("C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 1- Scaling/lomrmwmeasures.csv")
engulfmentmw <- engulfment %>% 
  rename(LogTL = LogTLm) %>% 
  select(LogTL, LogMW, Species) %>% 
  filter(Species %in% c("Blue Whale", "Fin Whale", "Humpback Whale", "Minke Whale"))


engulfMW <- ggplot(engulfmentmw, aes(x = LogTL, y = LogMW)) +
  geom_point(aes(color=Species, shape= Species), size = 2)
engulfMW  
  

## Baleen Area

BA <- read.csv("C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 2- Filtration/BAforols.csv")
baleenarea <- BA %>% 
  select(LogTL, LogBA, Species) %>% 
  filter(Species %in% c("Blue Whale", "Fin Whale", "Humpback Whale", "Minke Whale"))



baleen <- ggplot(baleenarea, aes(x = LogTL, y = LogBA)) +
  geom_point(aes(color=Species, shape= Species), size = 2)

baleen

## combine two data frames
Fig1 <- ggplot() +
  geom_point(data = engulfmentmw, 
             aes(x = LogTL, y = LogMW, 
                  color=Species, shape= Species), size = 2.5) +
  geom_point(data = baleenarea, 
             aes(x = LogTL, y = LogBA, 
                 color=Species, shape= Species), size = 2.5) +
  geom_smooth(data = engulfmentmw, 
             aes(x = LogTL, y = LogMW
                 ), method = "lm",se = FALSE, color = "red") +
  geom_smooth(data = baleenarea, 
             aes(x = LogTL, y = LogBA
                ), method = "lm", se = FALSE) + 
  geom_abline(slope = 2, linetype = 2) + 
  geom_abline(slope = 3, linetype = 3) + 
  scale_y_continuous(sec.axis = sec_axis(~., name = "Baleen Area [m]")) +
  xlim(0, 1.8) +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))


Fig1

coef(lm(LogMW ~ LogTL, data = engulfmentmw))
coef(lm(LogBA ~ LogTL, data = filter(baleenarea, LogTL >0)))


