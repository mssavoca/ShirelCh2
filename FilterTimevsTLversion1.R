#Figure 4 Filtration Paper

## TL by Filter Time

# Packages and Functions ----
install.packages("gridExtra")
install.packages("ggpubr")
install.packages("rstanarm")

library(ggpubr)
library(ggplot2)
library(rstanarm)
library(tidyverse)
library(lme4)

# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus
abbr_binom <- function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}

# Input Data ----
FilterTimebySize <- read_csv("FilterTimesMin.csv") %>%
  filter(TL > 6) %>% #made it 6 m
  filter(meandepthoflunge > 50) %>% 
  mutate(SpeciesCode = substr(whaleID, 1, 2),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"),
         Mean_Lunges_z = as.numeric(scale(meanlungesforagedives)),
         Mean_Depth_z = as.numeric(scale(meandepthoflunge)),
         TL_z = as.numeric(scale(TL)),
         Dive_Length_z = as.numeric(scale(meanforagedivelength)))


# Plot All Data ----
## I'm using the min filter times sheet because that should contain the most filter times and the greatest range of depth

FilterbySize <- ggplot() +
  geom_point(data = FilterTimebySize, aes (y = log10(meanpurge1), x = log10(TL), shape=abbr_binom(SciName), color = meandepthoflunge, size = meandepthoflunge), alpha = 0.8)+  
  scale_color_gradientn(colours = c("skyblue2",
                                    "deepskyblue2",
                                    "dodgerblue2", "royalblue",
                                    "mediumblue", "navy", "midnightblue"))+
  geom_smooth(data = FilterTimebySize, aes(x = log10(TL), y = log10(meanpurge1)), method = "lm",se = FALSE, color = "red") +
  geom_abline(slope = 1.8, linetype = 2) +
  labs(x = "log Total Length (m)",
       y = "log Purge Time (s)") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  guides(color=guide_legend("Lunge Depth (m)")) +
  guides(size=guide_legend("Lunge Depth (m)")) +
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10, 
                                   face="italic"))

FilterbySize


# GLMM for Filter Time ----

#plot to determine distribution
hist(log10(FilterTimebySize$meanpurge1))
hist(scale(FilterTimebySize$meanpurge1))
hist(FilterTimebySize$meanpurge1)
hist(log10(FilterTimebySize$TL))
hist(log10(FilterTimebySize$meandepthoflunge))



FilterTimeGLMM <- lmer(meanpurge1 ~ TL_z +
                          (1 | whaleID), data = FilterTimebySize, family = "gaussian")
summary(FilterTimeGLMM)

Example1<- ggplot() +
  geom_point(data = FilterTimesMin, 
             aes(y = log(meanforagedivelength), x = log(meandepthoflunge)))
Example1

# Graphs by Depth ----

##Min
FilterTimesMin <- read_csv("FilterTimesMin.csv") %>%
  mutate(SpeciesCode = substr(whaleID, 1, 2),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"))

MinFilter <- ggplot() +
  geom_point(data = FilterTimesMin, 
             aes(y = log(meanpurge1), x = log(TL)), aes(color = SpeciesCode)) +
  geom_smooth(data = FilterTimesMin, aes(x = log(TL), y = log(meanpurge1)), method = "lm",se = FALSE, color = "red") +
  geom_abline(slope = 1.8, linetype = 2) 
  
MinFilter

coef(lm(log(meanpurge1) ~ log(TL), data = FilterTimesMin))
(Intercept)     log(TL) 
-1.484574    1.757067


Fig1Filter <- ggplot() +
  geom_point(data = FilterTimesMin, 
             aes(x = TL, y = meanpurge1, 
                 color=SpeciesCode, shape= SciName), size = 2.5) +
  geom_point(data = FilterTimesMin, 
             aes(x = TL, y = meanpurge2, 
                 color=SpeciesCode, shape= SciName), size = 2.5) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "mean purge 2")) +
  xlim(0.5, 1.8) +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

Fig1Filter



## Fifty
FilterTimes50 <- read_csv("C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 2- Filtration/Files for Filtration Chapter/FilterTimes 1092019/FilterTimes50.csv") %>%
  mutate(SpeciesCode = substr(whaleID, 1, 2),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"))

FiftyFilter <- ggplot(data = FilterTimes50, 
                    aes(y = log(meanpurge1), x = log(TL))) +
  geom_point(data = FilterTimes50, aes(color = SpeciesCode)) +
  geom_smooth(data = FilterTimes50, aes(x = log(TL), y = log(meanpurge1)), method = "lm",se = FALSE, color = "red") +
  geom_abline(slope = 1.8, linetype = 2) 

FiftyFilter

coef(lm(log(meanpurge1) ~ log(TL), data = FilterTimes50))
(Intercept)     log(TL) 
-1.239665    1.686283


## Hundred

FilterTimes100 <- read_csv("C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 2- Filtration/Files for Filtration Chapter/FilterTimes 1092019/FilterTimes100.csv") %>%
  mutate(SpeciesCode = substr(whaleID, 1, 2),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"))

HundredFilter <- ggplot(data = FilterTimes100, 
                      aes(y = log(meanpurge1), x = log(TL))) +
  geom_point(data = FilterTimes100, aes(color = SpeciesCode)) +
  geom_smooth(data = FilterTimes100, aes(x = log(TL), y = log(meanpurge1)), method = "lm",se = FALSE, color = "red") +
  geom_abline(slope = 1.8, linetype = 2) 

HundredFilter

coef(lm(log(meanpurge1) ~ log(TL), data = FilterTimes100))
(Intercept)     log(TL) 
-1.414190    1.740372 


## Hundred Fifty
FilterTimes150 <- read_csv("C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 2- Filtration/Files for Filtration Chapter/FilterTimes 1092019/FilterTimes150.csv") %>%
  mutate(SpeciesCode = substr(whaleID, 1, 2),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"))

HundredFiftyFilter <- ggplot(data = FilterTimes150, 
                        aes(y = log(meanpurge1), x = log(TL))) +
  geom_point(data = FilterTimes150, aes(color = SpeciesCode)) +
  geom_smooth(data = FilterTimes150, aes(x = log(TL), y = log(meanpurge1)), method = "lm",se = FALSE, color = "red") +
  geom_abline(slope = 1.8, linetype = 2) 

HundredFiftyFilter

coef(lm(log(meanpurge1) ~ log(TL), data = FilterTimes150)) #isnt workin from some reason


## Graph All Together

Fig3 <- ggarrange(MinFilter, FiftyFilter, HundredFilter, HundredFiftyFilter,
                    labels = c("Min", "Fifty", "HUndred", "HUndredFifty"),
                    ncol = 2, nrow = 2)
Fig3
