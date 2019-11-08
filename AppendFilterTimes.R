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


write.table(lunges, file = "CompleteLungesandDives.csv", sep = ",", col.names = NA,
               qmethod = "double")



# Graphing ----
FilterTimes <- lunges%>%
  filter(TL.y > 6) %>% #made it 6 m
  filter(depth > 50) %>% 
  mutate(SpeciesCode = substr(whaleID, 1, 2),
         SciName = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis"))
#          

FilterPlot <- ggplot(data = FilterTimes, aes(x = TL.y, y= purge1)) +
  geom_point()

FilterPlot

FilterPlot <- ggplot() +
  geom_point(data = FilterTimes, 
             aes(y = log(purge1), x = log(TL.y)), aes(color = SpeciesCode)) 
  geom_smooth(data = FilterTimes, aes(x = log(TL.y), y = log(purge1)), method = "lm",se = FALSE, color = "red") +
  geom_abline(slope = 1.8, linetype = 2) 

FilterPlot




