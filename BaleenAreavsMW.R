#Figure 1 Filtration Paper

## Load Packages and Functions ----

install.packages("smatr")

library(plyr)
library(smatr)
library(ggplot2)
library(tidyverse)
library(lme4)

# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus
abbr_binom <- function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}

# Input Data and Arrange ----

# Engulfment  ----
engulfment <- read.csv("lomrmwmeasures.csv")
engulfmentmw <- engulfment %>% 
  unite("whaleID", whaleIDSp:whaleIDNumber, remove = FALSE) %>% 
  select(-c(Input:Species.1)) %>% 
  mutate(SpeciesFull = case_when(
    Species == "Blue Whale" ~ "B. musculus",
    Species == "Fin Whale" ~ "B. physalus",
    Species == "Humpback Whale" ~ "M. novaeangliae",
    Species == "Minke Whale" ~ "B. bonaerensis")) %>% 
  filter(Species %in% c("Blue Whale", "Fin Whale", "Humpback Whale", "Minke Whale"))


engulfMW <- ggplot(engulfmentmw, aes(x = log10(TL), y = log10(MW))) +
  geom_point(aes(color=Species, shape= Species), size = 2)
engulfMW  
  

# Baleen Area ----

BA <- read_csv("C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 2- Filtration/BAforols.csv")
baleenarea <- BA %>% 
  mutate(SpeciesFull = case_when(
    Species == "Blue Whale" ~ "B. musculus",
    Species == "Fin Whale" ~ "B. physalus",
    Species == "Humpback Whale" ~ "M. novaeangliae",
    Species == "Minke Whale" ~ "B. bonaerensis")) %>% 
  filter(Species %in% c("Blue Whale", "Fin Whale", "Humpback Whale", "Minke Whale"))
  



baleen <- ggplot(baleenarea, aes(x = log10(TL), y = log10(BA))) +
  geom_point(aes(color=Species, shape= Species), size = 2)

baleen

# Combine two data frames -----
Fig1 <- ggplot() +
  geom_point(data = engulfmentmw,
             aes(x = log10(TL), y = log10(MW),
                  color= SpeciesFull, shape = SpeciesFull), size = 2.5, alpha = .6) +
  geom_point(data = baleenarea,
             aes(x = log10(TL), y = log10(BA),
                 color= SpeciesFull, shape= SpeciesFull), size = 2.5, alpha = .6) +
  # #geom_smooth(data = engulfmentmw, 
  #            aes(x = log10(TL), y = log10(MW), color = SpeciesFull), 
  #            method = "lm",se = FALSE)+ 
  # geom_smooth(data = engulfmentmw, 
  #             aes(x = log10(TL), y = log10(MW)), 
  #             method = "lm",se = FALSE, color="red") +
  # geom_smooth(data = baleenarea, 
  #            aes(x = log10(TL), y = log10(BA), color= SpeciesFull),
  #             method = "lm", se = FALSE) + 
  # geom_smooth(data = baleenarea, 
  #             aes(x = log10(TL), y = log10(BA)), 
  #             method = "lm", se = FALSE) +
  geom_abline(slope = 1.8248, intercept = -1.8553, linetype = "dashed", color="orange", lwd = 1) + 
  geom_abline(slope = 3.707748, intercept = 0.108239, linetype = "dashed", color="blue", lwd = 1) + 
  #geom_abline(slope = 1.7902, intercept = -0.6915, linetype = "dashed", color="purple", lwd = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~., name = "Baleen Area [m]")) +
  xlim(0.5, 1.8) +
  labs(x = "log TL (m)") +
  labs(y = "log MW (L)") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))+
  guides(color=guide_legend("Species")) +
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10, 
                                   face="italic"))

Fig1


# GLMM and MCMC ----

# Regular lm for MWs
coef(lm(LogMW ~ LogTL, data = engulfmentmw))

#Regular lm for BA
coef(lm(LogBA ~ LogTL, data = filter(baleenarea, LogTL >0)))

#GLMM for MW
# does not converge for some reason 

MW_GLMM1 <- lmer(log10(MW) ~ log10(TL) + (1|SpeciesFull), 
           data = engulfmentmw)
summary(MW_GLMM1)

#GLMM for BA 
#no need to log10 here because I already logged it in excel (don't know why I did but I did)
BA_GLMM1 <- lmer(log10(BA) ~ log10(TL) + (1|SpeciesFull), 
                data = baleenarea)
summary(BA_GLMM1)

#MCMC for MW


MCMCglmm_MW_TL <- MCMCglmm(log10(MW) ~ log10(TL),
                           random = ~ SpeciesFull, #should I use species of whaleID? I chose SpeciesFull because technically none are individuals 
                                                    #They are all based off approximations from other whales.
                           data = engulfmentmw, 
                           family = "gaussian",
                           nitt = 11000, thin = 1, burnin = 1000,
                           pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                           verbose = TRUE)
summary(MCMCglmm_MW_TL)


# MCMC for BA

MCMCglmm_BA_TL <- MCMCglmm(log10(BA) ~ log10(TL),
                           random = ~ SpeciesFull, #should I use species of whaleID? I chose SpeciesFull because technically none are individuals 
                                                   #They are all based off approximations from other whales.
                           data = baleenarea, 
                           family = "gaussian",
                           nitt = 11000, thin = 1, burnin = 1000,
                           pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                           verbose = TRUE)
summary(MCMCglmm_BA_TL)




