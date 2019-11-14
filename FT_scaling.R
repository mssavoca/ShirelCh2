##############################
# Load packages and data ----
##############################


library(tidyverse)
library(lme4)
install.packages("MCMCglmm")
library(MCMCglmm)


FilterTime_Raw <- read_csv("FilterTime_Raw.csv") 
FilterTime_Raw <- FilterTime_Raw %>% 
  select(-c(X1, X1_1, TL.x)) %>% 
  #rename(c("TL" = "TL.y")) %>% 
  mutate(whaleID = factor(whaleID))


m1 <- lmer(log10(purge1) ~ log10(TL.y) + (1|whaleID), 
           data = FilterTime_Raw)
summary(m1) # slope of purge 1 is 1.79075, has no problem converging 


purgelm <- lm(log10(purge1) ~ log10(TL.y), data = FilterTime_Raw)
summary(purgelm) #slope of purge 1 is 1.93951

# MCMCglmm so that we can get a distribution of parameter estimates and thus a confidence interval of the slope---- 
MCMCglmm_FT_TL <- MCMCglmm(log10(purge1) ~ log10(TL.y),
                           random = ~ whaleID,
                           data = FilterTime_Raw, 
                           family = "gaussian",
                           nitt = 11000, thin = 1, burnin = 1000,
                           pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                           verbose = TRUE)
summary(MCMCglmm_FT_TL)


model_param_values <- as.data.frame(MCMCglmm_FT_TL$Sol) #plot quantile 97.5 and 2.5 


# plot parameter distributions
slope_distributions <- ggplot(model_param_values) +
  geom_density(aes(`log10(TL.y)`), color = "dark blue") +
  labs(x = "slope parameter distribution") +
  geom_vline(xintercept = 1.9,  linetype = "dashed", color = "dark blue") +   # predicted slope
  geom_vline(xintercept = 1.8,  linetype = "dotted", color = "red") +           # slope for the scaling of baleen area
  geom_vline(xintercept = 3.7,  linetype = "dotted", color = "red") +           # slope for the scaling of MW
  xlim(1,3.72) +
  theme_classic()
slope_distributions 

dev.copy2pdf(file="slope_distributions.pdf", width=4, height=5)