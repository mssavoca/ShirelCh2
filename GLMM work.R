### GLMM Attempt ###

# Packages and Libraries ----
library(readr)
library(tidyverse)
library(car)
library(MASS)
library(glmm)
library(lme4)
install.packages("glmm")


# Input Data ----
GLMM <- read_csv("GLMM data.csv") %>% 
  filter(TL > 6) %>% 
  filter(Lunge_Count>0) %>% 
  filter(Mean_Depth >50) %>% 
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
         

#Plot to determine distribution
hist(log10(GLMM$Lunge_Count))
hist(log10(GLMM$TL))
hist(log10(GLMM$Mean_Depth))


whale2 <- glmer(Lunge_Count ~ Mean_Depth_z + #winner winner chicken dinner
                  TL_z + 
                  Dive_Length_z + 
                  (1| ID), 
                data = GLMM, family = "poisson")
summary(whale2)


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




# Old Attempts ----
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(whale)

# whale <- glmer(Lunge_Count ~ log10(Mean_Depth) + log10(TL) + SpeciesCode + (1| ID), data = GLMM, family = "poisson")
# summary(whale)
# 
# hist(resid(whale))
# plot(GLMM$TL,resid(whale))
# plot()

str(GLMM)
head(GLMM) # variables of interest are lunge count and dive length 
GLMM$Lunge_Count.t <- GLMM$Lunge_Count
qqp(GLMM$Lunge_Count.t, "norm")
qqp(GLMM$Lunge_Count.t, "lnorm")

nbinom <- fitdistr(GLMM$Lunge_Count.t, "Negative Binomial")
qqp(GLMM$Lunge_Count.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

poisson <- fitdistr(GLMM$Lunge_Count.t, "Poisson")
qqp(GLMM$Lunge_Count.t, "pois", poisson$estimate) #lambda?

gamma <- fitdistr(GLMM$Lunge_Count.t, "gamma")
qqp(GLMM$Lunge_Count.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])


ggplot(data = GLMM, aes(x = log10(TL), y = Lunge_Count)) +
  geom_point() +
  geom_smooth(method = lm)





