#### Load Packages ####
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Load Packages
pkgTest("tidyverse")
pkgTest("ggplot2")
pkgTest("lubridate")
pkgTest("R.matlab")
pkgTest("rstudioapi")

# Set the Working Directory to the path of source file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)


#### Import MasterLunges Per Dive ####
filename <- file.choose() #Dive Sheet for Filter Times
dives <- read_csv(filename)

dives <- dives %>% 
  mutate(whaleID = str_remove_all(ID, "[']")) #remove ' and add whaleID 

#### Load Filter Times ####
## Select the masterLungeTable file
lungeFile <-file.choose() #called master lunges and dives
lunges  <- read_csv(lungeFile)
lunges$Dive_Num <- NA
lunges$TL <- NA

## Append DiveNum to Lunge Table
for(i in 1:length(dives$whaleID)) {
  for(j in 1:length(lunges$LungeI)){
    if(lunges$whaleID[j] == dives$whaleID[i] && lunges$LungeI[j] >= dives$startI[i] && lunges$LungeI[j] <= dives$endI[i])
      lunges$Dive_Num[j] = dives$Dive_Num[i]
      #lunges$TL[j] = dives$TL[i]
  }
}


## Append TL to Lunge Table--- joining to left side which is lunges. will match the length of thing on left side 
lunges <- left_join(lunges,select(dives,c("whaleID", "Dive_Num", "TL") ), by = c("whaleID", "Dive_Num"))


FilterTime_Raw <- read_csv("FilterTime_Raw.csv") %>% 
  select(-c(X1, X1_1, TL.x)) %>% 
  rename(TL = "TL.y") %>% 
  mutate(whaleID = factor(whaleID))


# .CSV with Completed Lunge Table ----
write.table(lunges, file = "CompleteLungesandDives.csv", sep = ",", col.names = NA,
               qmethod = "double")



# Filter Time by Size (Raw) ----
FilterTime_Raw <- lunges%>%
  filter(TL.y > 6, depth >50, purge1 >1) %>% 
  mutate(SpeciesCode = substr(whaleID, 1, 2),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"),
         whaleID = factor(whaleID),
         TL_z = as.numeric(scale(TL.y)),
         Depth_z = as.numeric(scale(depth))) 

write.table(FilterTime_Raw, file = "FilterTime_Raw.csv", sep = ",", col.names = NA,
            qmethod = "double")




FilterPlot <- ggplot(data = FilterTime_Raw, aes(y = log10(purge1), x = log10(TL.y))) +
                       geom_point(aes(color = SpeciesCode), alpha = .1)+
  geom_smooth(method = "lm") + 
  geom_abline(slope = 1.93951, intercept = -0.88938, linetype = "solid", color="red") #lmer+
  

FilterPlot


FilterPlot_violin <- ggplot(data = FilterTime_Raw, aes(y = log(purge1), x = log(TL.y), fill = SciName)) +
  geom_violin() + 
  geom_abline(slope = 1.93951, intercept = -0.88938, linetype = "solid", color="red")

FilterPlot_violin

# GLMM for Filter Time ----
# This uses the raw data to provide more data points

#plot to determine distribution
hist(log10(FilterTime_Raw$purge1)) #looks poisson to me 
hist(scale(FilterTime_Raw$purge1))
hist(FilterTime_Raw$purge1)
hist(log10(FilterTime_Raw$TL_z))
hist(scale(FilterTime_Raw$TL.y))


FilterTimeGLMM1 <- lmer(log10(purge1) ~ log10(TL.y) +
                (1|whaleID),
                data = FilterTime_Raw)
#takes into account that each indiv should be accounted for and each species should be accounted for 
#need to do lmer because the lunges are nested within an inivid and if you dont do lmer you wont be taking those into cacount

summary(FilterTimeGLMM1)
plot(allEffects(FilterTimeGLMM1))



coef(lm(log10(purge1) ~ log10(TL.y), data = FilterTime_Raw))




# Filter Time by Size (Means) ----
FilterTimebySize <- read_csv("FilterTimesMin.csv") %>%
  filter(TL > 6) %>% #made it 6 m
  filter(meandepthoflunge > 50) %>% 
  mutate(SpeciesCode = substr(whaleID, 1, 2),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"))

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
  




