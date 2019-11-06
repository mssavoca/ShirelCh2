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
## Append DuveNum to Lunge Table
for(i in 1:length(dives$whaleID)) {
  for(j in 1:length(lunges$LungeI)){
    if(lunges$whaleID[j] == dives$whaleID[i] && lunges$LungeI[j] >= dives$startI[i] && lunges$LungeI[j] <= dives$endI[i])
      lunges$Dive_Num[j] = dives$divenumber[i]
  }
}






## Export Summary Files
write.csv(dataSum, file=paste(depid,"-HourlyForagingSummary", ".csv",sep=""))
write.csv(foragingSum, file=paste(depid,"-DailyForagingSummary", ".csv",sep=""))

