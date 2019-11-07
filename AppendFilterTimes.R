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
filename <- file.choose()
dives <- read_csv(filename)

#### Load Filter Times ####
## Select the masterLungeTable file
lungeFile <-file.choose()
lunges  <- read_csv(lungeFile)

lunges$Dive_Num <- NA
lunges$TLm <-  NA

## Append DiveNum to Lunge Table
for(i in 1:length(dives$whaleID)) {
  for(j in 1:length(lunges$LungeI)){
    if(lunges$whaleID[j] == dives$whaleID[i] && lunges$LungeI[j] >= dives$startI[i] && lunges$LungeI[j] <= dives$endI[i])
      lunges$Dive_Num[j] = dives$divenumber[i]
  }
}

## Append TL to Lunge Table---
for(i in 1:length(dives$whaleID)) {
  for(j in 1:length(lunges$LungeI)){
    if(lunges$whaleID[j] == dives$whaleID[i] && lunges$LungeI[j] == dives$TL[i])
      lunges$TLm[j] = dives$TL[i]
  }
}


write.table(lunges, file = "MasterLungesandDives.csv", sep = ",", col.names = NA,
              + qmethod = "double")

# Graphing ----
FilterTimesMin <- read_csv("MasterLungesandDives.csv") %>%
  mutate(SpeciesCode = substr(whaleID, 1, 2),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"))
         # TL = case_when(
         #   whaleID == "bb180304-42" ~ 8.12,
         #   whaleID == "bb180304-45" ~ 8.58,
         #   whaleID == "bb190224-48" ~ 5.76,
         #   ))

MinFilter <- ggplot() +
  geom_point(data = FilterTimesMin, 
             aes(y = log(meanpurge1), x = log(TL)), aes(color = SpeciesCode)) +
  geom_smooth(data = FilterTimesMin, aes(x = log(TL), y = log(meanpurge1)), method = "lm",se = FALSE, color = "red") +
  geom_abline(slope = 1.8, linetype = 2) 

MinFilter




