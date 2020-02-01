# Figure 3 Filtration Paper - this file includes the lunges per dive plot and the glmm for that data

# Packages and Functions ---- 
library(data.table)
library(ggpubr)
library(readxl)
library(forcats)
library(tidyverse)
library(mgcv)
library(lme4)
library(ggpubr)
library(ggplot2)
library(effects)
library(sjPlot)

install.packages("sjPlot")



# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus 
abbr_binom <- function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}

SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}

# Data Input ----
LungesPerDive_raw_csv <- read_csv("Figure 2 filtration paper data.csv")

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
         dive_150 = ifelse(Mean_Depth <= 150, "N", "Y"),
         SpeciesFull = case_when(
           SciName == "Balaenoptera musculus" ~ "B. musculus",
           SciName == "Balaenoptera physalus" ~ "B. physalus",
           SciName == "Megaptera novaeangliae" ~ "M. novaeangliae",
           SciName == "Balaenoptera bonaerensis" ~ "B. bonaerensis")) %>% 
  pivot_longer(cols = dive_50:dive_150, names_to = "depthcat", values_to = "response") %>% 
  group_by(ID) %>% 
  mutate(foragingdivecount = n_distinct(Dive_Num),
         depthcat = factor(depthcat),
         response = factor(response)) 





# Manipulations ----         

LungesPerDive_summary_ID <- LungesPerDive_raw %>% 
  group_by(ID) %>% 
  filter(response == "Y") %>% 
  summarise(
    #lunge_count_mean = mean(Lunge_Count),
            #lunge_count_median = median(Lunge_Count),
            lunge_count_max = max(Lunge_Count),
            foragingdivecount = n_distinct(Dive_Num),
            Meandepth_ofmaxlunge = ifelse()
            TL = mean(TL)) %>% 
  mutate(SpeciesCode = substr(ID, 1, 2)) %>% 
  filter(lunge_count_max > 1)


LungesPerDive_summary_Species <- LungesPerDive_raw %>% 
  group_by(SpeciesCode, depthcat) %>% 
  filter(response == "Y") %>% 
  summarise(lunge_count_mean = mean(Lunge_Count),
            SE_lunge_count_mean = SE(Lunge_Count),
            lunge_count_median = median(Lunge_Count),
            SE_lunge_count_median = SE(Lunge_Count),
            lunge_count_max = max(Lunge_Count),
            foragingdivecount = n_distinct(Dive_Num),
            TL = mean(TL))

  

# Weighted --

species_summary_weighted <- LungesPerDive_raw %>% 
  filter(response == "Y") %>% 
  group_by(depthcat, SpeciesCode) %>% 
  summarise(wgt_mean = weighted.mean(Lunge_Count, foragingdivecount),
            SE_lunge_count = SE(Lunge_Count))



# Plotting Means by ID ----

IDLungeMeans <- ggplot(data = LungesPerDive_summary_ID, 
                     aes(x=TL, y= lunge_count_max)) +
  geom_point(aes(shape = SpeciesCode, 
                 color = SpeciesCode)) +
  geom_smooth(method = lm) +
  scale_size_continuous(range = c(0.5, 4)) +
  labs(x = "Total Length (m)",
       y = "Max Lunges Per Dive") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  guides(shape=guide_legend("Species")) +
  guides(color=guide_legend("Species"))+
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10, 
                                   face="italic")) 

IDLungeMeans

# 
# IDLungeMeans_glmm <- lmer(log10(lunge_count_max) ~ log10(TL) + (1|SpeciesCode), 
#            data = LungesPerDive_summary_ID)
# summary(IDLungeMeans_glmm) #slope is -0.3442

IDLungeMeanslm <- lm(log10(lunge_count_max) ~ log10(TL), data = LungesPerDive_summary_ID)
summary(IDLungeMeanslm) #slope is -0.3442




SpeciesLungeMeans <- ggplot(data = LungesPerDive_summary_Species, 
                       aes(x=TL, y= lunge_count_max)) +
  geom_point(aes(shape = SpeciesCode, 
                 color = SpeciesCode),
                size=5) +
  geom_smooth(method = lm) +
  labs(x = "Total Length (m)",
       y = "Max Lunges Per Dive") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  guides(shape=guide_legend("Species")) +
  guides(color=guide_legend("Species"))+
  theme(legend.text = element_text(size=10, 
                                   face="italic")) 

SpeciesLungeMeans

# SpeciesLungeMeans_glm <- lmer(log10(lunge_count_max) ~ log10(TL) + (1|SpeciesCode), 
#                        data = LungesPerDive_summary_Species)
# summary(SpeciesLungeMeans_glm) #glm doesnt work! 

SpeciesLungeMeanslm <- lm(log10(lunge_count_max) ~ log10(TL), data = LungesPerDive_summary_Species)
summary(SpeciesLungeMeanslm) #slope is -0.6146


#Plotting all data - Lunge Count vs TL ----

Fig2 <-  ggplot(data = LungesPerDive_raw, 
                aes(x=log10(TL), y= log10(Lunge_Count),  shape = abbr_binom(SciName), 
                    color = Mean_Depth, size = Mean_Depth), alpha = 0.8) +
  geom_point() +
  #geom_smooth(method = lm) +
  scale_color_gradientn(colours = c("skyblue2",
                                    "deepskyblue2",
                                    "dodgerblue2", "royalblue",
                                    "mediumblue", "navy", "midnightblue"), #blues to mimic ocean depth
                        name = "Mean Depth (m)") +
  scale_size_continuous(range = c(0.5, 4)) +
  #ylim(0, 1.5) +
  labs(x = " log Total Length (m)",
       y = " log Lunges Per Dive") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) + 
  guides(color=guide_legend("Lunge Depth (m)")) +
  guides(size=guide_legend("Lunge Depth (m)")) +
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10, 
                                   face="italic")) 
  

Fig2


#Plot Fig 2 species summary ----

lungesperdive_bar <- ggplot(data = species_summary, 
                            aes( x= reorder(SpeciesCode, -wgt_mean), y = wgt_mean, fill = depthcat)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Species",
       y = "Weighted Mean, Lunges Per Dive") +
  theme_classic()
lungesperdive_bar





# Plot by Depth Bins ----
#- Jeremy had an idea to have a three panel plot. Looks better as violin

#Points ----
Fiftyandbelow <- LungesPerDive_raw %>% 
  filter(depthcat == "dive_50",
         response == "Y")
Hundredandbelow <- LungesPerDive_raw %>% 
  filter(depthcat == "dive_100",
         response =="Y")
HundredFiftyandbelow <- LungesPerDive_raw %>% 
  filter(depthcat == "dive_150",
         response =="Y")


PlotFifty <- ggplot(data = Fiftyandbelow, 
                        aes(x=TL, y= Lunge_Count, color = SpeciesCode, shape = SpeciesCode))+
               geom_point()
PlotFifty

PlotHundred <- ggplot(data = Hundredandbelow, 
                    aes(x=TL, y= Lunge_Count, color = SpeciesCode, shape = SpeciesCode))+
  geom_point()
PlotHundred

PlotHundredFifty <- ggplot(data = HundredFiftyandbelow, 
                    aes(x=TL, y= Lunge_Count, color = SpeciesCode, shape = SpeciesCode))+
  geom_point()
PlotHundredFifty



#Violin ----
Fiftyandbelow <- LungesPerDive_raw %>% 
  filter(depthcat == "dive_50",
         response == "Y")
Hundredandbelow <- LungesPerDive_raw %>% 
  filter(depthcat == "dive_100",
         response =="Y")
HundredFiftyandbelow <- LungesPerDive_raw %>% 
  filter(depthcat == "dive_150",
         response =="Y")


PlotFifty <- ggplot(data = Fiftyandbelow, aes(x=log10(TL), y= Lunge_Count, fill = SpeciesFull)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  labs(x = "log Total Length (m)",
       y = "Lunges Per Dive") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))+
  guides(fill = guide_legend("Species"))+
  theme(legend.text = element_text(size=10, 
                                   face="italic"))+
  theme_classic()

PlotFifty

PlotHundred <- ggplot(data = Hundredandbelow, 
                    aes(x=log10(TL), y= Lunge_Count,  fill = SpeciesFull))+
  geom_violin() +
  geom_boxplot(width = 0.1) +
  labs(x = "log Total Length (m)",
       y = "Lunges Per Dive") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))+
  guides(fill = guide_legend("Species"))+
  theme(legend.text = element_text(size=10, 
                                   face="italic"))+
  theme_classic()


PlotHundredFifty <- ggplot(data = HundredFiftyandbelow, 
                      aes(x=log10(TL), y= Lunge_Count,  fill = SpeciesFull))+
  geom_violin()+
  geom_boxplot(width = 0.1) +
  labs(x = "log Total Length (m)",
       y = "Lunges Per Dive") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))+
  guides(fill = guide_legend("Species"))+
  theme(legend.text = element_text(size=10, 
                                   face="italic"))+
  theme_classic()


ggarrange(PlotFifty, PlotHundred, PlotHundredFifty + 
            rremove("x.text"), 
          labels = c("50m and Deeper", 
                     "100m and Deeper", 
                     "150m and Deeper"),
          ncol = 3, nrow = 1) +
  theme_classic() 





#Statistics for Lunge Count - GLMM ----
GLMM <- read_csv("GLMM data.csv") %>% 
  filter(TL > 6, Lunge_Count >0, Mean_Depth > 50, Dive_Length < 3000) %>% 
  mutate(SpeciesCode = substr(ID, 2, 3),
         ID = str_remove_all(ID, "[']"),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"),
         TL_z = as.numeric(scale(TL)),
         Mean_Depth_z = as.numeric(scale(Mean_Depth)),
         Dive_Length_z = as.numeric(scale(Dive_Length)))


# Plot Dive Length ----


DiveLength_TL <-  ggplot(data = GLMM, aes(x=log10(TL), y= log10(Dive_Length))) +
  geom_point(aes(shape = SciName,
                 color = SciName,
                 size = 4)) + 
  geom_smooth(method= lm) +
    # scale_color_gradientn(colours = c("skyblue2",
    #                                 "deepskyblue2",
    #                                 "dodgerblue2", "royalblue",
    #                                 "mediumblue", "navy", "midnightblue"), #blues to mimic ocean depth
    #                     name = "Mean Depth (m)") +
  scale_size_continuous(range = c(0.5, 4)) +
  #ylim(0, 1.5) +
  labs(x = "log Total Length (m)",
       y = "log Dive Duration (s)") +
  theme_classic() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) +
  guides(color=guide_legend("Species")) +
  guides(shape=guide_legend("Species")) +
  theme(legend.text = element_text(size=10,
                                   face="italic"))


DiveLength_TL

DiveLength_TL_lm <- lm(log10(Dive_Length) ~ log10(TL), data = GLMM)
summary(DiveLength_TL_lm)




DiveLength_TL<- lmer(log10(Dive_Length) ~ log10(TL) + (1|ID), 
           data = GLMM)
summary(DiveLength_TL)



#Plot to determine distribution. should these be scaled or logged?
hist(log10(GLMM$Lunge_Count))
hist(log10(GLMM$TL))
hist(log10(GLMM$Mean_Depth))
plot(data=)


LungeCountGLMM <- glmer(Lunge_Count ~ Mean_Depth_z + #winner winner chicken dinner. why dont we scale lunge count?
                  TL_z + 
                  Dive_Length_z + 
                  (1| ID), 
                data = GLMM, family = "poisson")


plot_model(LungeCountGLMM)+
  xlab("Predictors")+
  geom_point(size =6)+
  ylim(0.6, 1.3)+
  ggtitle("")+
  theme_bw()+
  theme(legend.text = element_text(size=10, 
                                   face="italic")) +
  geom_hline(yintercept = 1)

plot_model(LungeCountGLMM, type = "re") #shows which indiv had high or low number of dives 
  
  
#effect of depth on lunge cout is positive, meaning that with increasing depth, there are more lunges
#effect of tl on lunge count is negative, meaning that larger animals lunge less
#effect of dive length on lunge count is positive, menaing that with increasing dive length there are more lunges


GLMMSummary_LungeCount <- summary(LungeCountGLMM)
capture.output(GLMMSummary_LungeCount, file = "GLMMSummary_LungeCount.txt")


plot(allEffects(LungeCountGLMM)) #shows the relationship shown the the summary

#Attempt at an MCMC --
MCMCglmm_LungePerDive <- MCMCglmm(Lunge_Count ~ Mean_Depth_z +
                                     TL_z + 
                                     Dive_Length_z,
                           random = ~ ID,
                           data = GLMM, 
                           family = "poisson",
                           nitt = 11000, thin = 1, burnin = 1000,
                           pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                           verbose = TRUE)
summary(MCMCglmm_LungePerDive)


model_param_values <- as.data.frame(MCMCglmm_FT_TL$Sol) #plot quantile 97.5 and 2.5 






#exponentiating the coefficient makes it an odds, like in: odds are 5:1 on a horse (exp(beta_0) = 5). 
#Effects are then multiplicative, like in: under condition x, the odds increase by factor 2 (exp(beta_x) = 2)


# includes speciescode as a variable. this isn't as useful as including TL. they are redundant if both included. 
#this model is slightly less good than whale2
# whale3 <- glmer(Lunge_Count ~ Mean_Depth_z + 
#                   SpeciesCode + 
#                   Dive_Length_z + 
#                   (1| ID), 
#                 data = GLMM, family = "poisson")

AIC(whale2, whale3)

hist(resid(whale2))
plot(GLMM$TL,resid(whale2))
plot(GLMM$Mean_Depth, resid(whale2))
plot(GLMM$Dive_Length, resid(whale2))
plot(GLMM$SpeciesCode, resid(whale2)) #plot resid of model against species code if dispersed the affect of species does not explain any more variation in the data
