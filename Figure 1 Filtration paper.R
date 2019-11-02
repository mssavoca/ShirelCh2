#Figure 1 Filtration Paper

## Engulfment Groups

library(plyr)
library(smatr)
library(ggplot2)
library(tidyverse)

# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus
abbr_binom <- function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}

engulfment <- read_csv("C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 1- Scaling/lomrmwmeasures.csv")
engulfmentmw <- engulfment %>% 
  mutate(SpeciesFull = case_when(
    Species == "Blue Whale" ~ "B. musculus",
    Species == "Fin Whale" ~ "B. physalus",
    Species == "Humpback Whale" ~ "M. novaeangliae",
    Species == "Minke Whale" ~ "B. bonaerensis")) %>% 
  filter(Species %in% c("Blue Whale", "Fin Whale", "Humpback Whale", "Minke Whale"))


engulfMW <- ggplot(engulfmentmw, aes(x = LogTL, y = LogMW)) +
  geom_point(aes(color=Species, shape= Species), size = 2)
engulfMW  
  

## Baleen Area

BA <- read_csv("C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 2- Filtration/BAforols.csv")
baleenarea <- BA %>% 
  select(LogTL, LogBA, Species) %>% 
  mutate(SpeciesFull = case_when(
    Species == "Blue Whale" ~ "B. musculus",
    Species == "Fin Whale" ~ "B. physalus",
    Species == "Humpback Whale" ~ "M. novaeangliae",
    Species == "Minke Whale" ~ "B. bonaerensis")) %>% 
  filter(Species %in% c("Blue Whale", "Fin Whale", "Humpback Whale", "Minke Whale"))
  



baleen <- ggplot(baleenarea, aes(x = LogTL, y = LogBA)) +
  geom_point(aes(color=Species, shape= Species), size = 2)

baleen

## combine two data frames
Fig1 <- ggplot() +
  geom_point(data = engulfmentmw,
             aes(x = LogTL, y = LogMW,
                  color= SpeciesFull, shape = SpeciesFull), size = 2.5, alpha = .6) +
  geom_point(data = baleenarea,
             aes(x = LogTL, y = LogBA,
                 color= SpeciesFull, shape= SpeciesFull), size = 2.5, alpha = .6) +
  geom_smooth(data = engulfmentmw, 
             aes(x = LogTL, y = LogMW, color = SpeciesFull), 
             method = "lm",se = FALSE) +
  geom_smooth(data = engulfmentmw, 
              aes(x = LogTL, y = LogMW), 
              method = "lm",se = FALSE, color="red") +
  geom_smooth(data = baleenarea, 
             aes(x = LogTL, y = LogBA, color= SpeciesFull),
              method = "lm", se = FALSE) + 
  geom_smooth(data = baleenarea, 
              aes(x = LogTL, y = LogBA), 
              method = "lm", se = FALSE) +
  geom_abline(slope = 2, intercept = -1.8, linetype = "dashed", color="blue") + 
  geom_abline(slope = 3, intercept = 1, linetype = "dashed", color="red") + 
  scale_y_continuous(sec.axis = sec_axis(~., name = "Baleen Area [m]")) +
  xlim(0.5, 1.8) +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))+
  guides(color=guide_legend("Species")) +
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10, 
                                   face="italic"))
Fig1




coef(lm(LogMW ~ LogTL, data = engulfmentmw))
coef(lm(LogBA ~ LogTL, data = filter(baleenarea, LogTL >0)))


