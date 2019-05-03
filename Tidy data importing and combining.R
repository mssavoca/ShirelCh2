####################################
# Ultimate code cleaning
####################################

## SpeciesCode will be "key" variable


# Species-specific average length data (from Shirel's paper), to recalculate average engulfment capacity
v_data <- read_excel("mwmrmeasures.xlsx") %>% 
  select(1:3) %>% 
  group_by(Species) %>% 
  dplyr::summarize(med_TLm = median(TLm)) %>% 
  rename(CommonName = Species) %>% 
  mutate(SpeciesCode = case_when(CommonName ==  "Blue Whale" ~ "bw",
                                 CommonName == "Fin Whale"~ "bp",
                                 CommonName ==  "Humpback Whale" ~ "mn",
                                 CommonName == "Minke Whale" ~ "bb", 
                                 CommonName == "Bryde's Whale" ~ "be",
                                 CommonName == "Sei Whale" ~ "bs"))
# # v_data$L <- v_data$MW*0.9766  #creates column that converts MW (kg) to liters

# Allometric equations from Shirel's paper
# creating fucntions from Shirel's paper for MW (in kg) for engulfment capacity in liters for each species where we have a known length
engulf_allo <- tribble(
  ~SpeciesCode, ~slope,   ~intercept,
  "bb",     3.10910,  0.69969,
  "be",     3.1453,   0.5787,
  "bp",     3.54883,  0.15604,
  "bw",     3.667316, -0.014078,
  "mn",     3.24603,  0.85934
)


# Skips first two rows
tag_guide <- read_excel("TAG GUIDE_2.18.19.xlsx", skip = 2) %>% 
  rename(Study_Area = `Study_Area     _`,
         SpeciesCode = `Spec      _`)

# read in whale population data. Current data from IUCN 2019 Redlist, whaling data compiled in Rocha et al. 2014
pop_data <- read_excel("Filtration project_Whale population and feeding information.xlsx", sheet = 1)


# Load prey distributions from Jeremy's paper
d_full_NULL <- read_csv("Cetacea model output NULL_EXTANT.csv") %>% 
  mutate(Species = ifelse(Species == "bonarensis", "bonaerensis", Species),
         SpeciesCode = case_when(Species ==  "musculus" ~ "bw",
                                 Species == "physalus"~ "bp",
                                 Species ==  "novaeangliae" ~ "mn",
                                 Species == "bonaerensis" ~ "bb"))
d_full_BOUT <- read_csv("Cetacea model output BOUT_EXTANT.csv") %>% 
  mutate(Species = ifelse(Species == "bonarensis", "bonaerensis", Species),
         SpeciesCode = case_when(Species ==  "musculus" ~ "bw",
                                 Species == "physalus"~ "bp",
                                 Species ==  "novaeangliae" ~ "mn",
                                 Species == "bonaerensis" ~ "bb"))

# summarize prey distributions from Jeremy's paper
d_sum_NULL <- d_full_NULL %>%
  group_by(SpeciesCode) %>%
  dplyr::summarize(wgtMeanNULL_wt_g = weighted.mean(`Prey W (g)`, Percent),
                   medNULL_wt_g = median(`Prey W (g)`),
                   wgtMeanNULL_E = weighted.mean(`Energy (kJ)`, Percent),
                   medNULL_E = median(`Energy (kJ)`))
d_sum_NULL <- na.omit(d_sum_NULL)

d_sum_BOUT = d_full_BOUT %>% 
  group_by(SpeciesCode) %>% 
  dplyr::summarize(wgtMeanBOUT_wt_g = weighted.mean(`Prey W (g)`, Percent), 
                   medBOUT_wt_g = median(`Prey W (g)`), 
                   wgtMeanBOUT_E = weighted.mean(`Energy (kJ)`, Percent),
                   medBOUT_E = median(`Energy (kJ)`))
d_sum_NULL <- na.omit(d_sum_NULL)



# Rorqual feeding data from Jeremy's scaling paper
RorqualData <- read_csv("lunge_rates_from_Paolo.csv") %>% 
  mutate(`deployment-time_h` = `deployment-time_secs`/60/60,
         SpeciesCode = substr(ID,1,2),)  %>% 
  select(-c(species))


#joining dataframes into the master to be used for all analyses
master_filtprey_data <- read_excel("ALLPRHS 2.5.2019.xls") %>% 
  left_join(select(RorqualData, -c(prh_start, prh_stop, prh_fps, `deployment-time_secs`, SpeciesCode)), by = "ID") %>%   # OR SHOULD THIS BE FULL JOIN?!?!?!?
  left_join(select(tag_guide, ID, Study_Area), by = "ID") %>% 
  # Replace NAs in location with "SoCal"
  mutate(Study_Area = replace_na(Study_Area, "SoCal"),
         # Make whale length a number
         whaleLength = parse_number(whaleLength),
         # First two chars of ID is the species code Note: ba/bb confusion
         SpeciesCode = substr(ID, 1, 2),
         CommonName = case_when(
           SpeciesCode == "bw" ~ "Blue Whale",
           SpeciesCode == "bp" ~ "Fin Whale",
           SpeciesCode == "mn" ~ "Humpback Whale",
           SpeciesCode %in% c("ba", "bb") ~ "Minke Whale", 
           TRUE ~ "Bryde's Whale"),
         Species = case_when(
           SpeciesCode == "bw" ~ "Balaenoptera musculus",
           SpeciesCode == "bp" ~ "Balaenoptera physalus",
           SpeciesCode == "mn" ~ "Megaptera novaeangliae",
           SpeciesCode == "bb" ~ "Balaenoptera bonaerensis", 
           SpeciesCode == "be" ~ "Balaenoptera edeni")) %>% 
  select(-notes) %>% 
  # Join species-median size data
  left_join(select(v_data, -CommonName), by = "SpeciesCode") %>% 
  # Join allometric equations
  left_join(engulf_allo, by = "SpeciesCode") %>% 
  # Calculate engulfment volumes
  mutate(BestLengthEst = coalesce(whaleLength, med_TLm),
         Engulfment_L = BestLengthEst ^ slope * 10 ^ intercept * 0.9766,
         # Feeding rates
         TotalLunges = dayhourslunges + nighthourslunges + twilightzonelunges,
         TotalHours = dayhours + nighthours + twilightzone,
         LungesPerHour = TotalLunges/TotalHours,
         LungesPerDayHour = dayhourslunges/dayhours,
         LungesPerNightHour = nighthourslunges/nighthours,
         LungesPerTwHour = twilightzonelunges/twilightzone) %>%
  # Replace NaNs with NAs in LungesPer* columns
  mutate_at(c("LungesPerDayHour", "LungesPerNightHour", "LungesPerTwHour"), 
            function(col) if_else(!is.nan(col), col, NA_real_)) %>% 
  # Prey data
  separate(prey, into = c("Prey", "Prey notes"), sep = " ") %>% 
  drop_na(Prey) %>% 
  filter(!Prey %in% c("Milk", "N")) %>% 
  mutate(PreyClean = case_when(
    Prey %in% c("Anchovies", "Fish", "Herring", "SandLance", "Sardines")  ~ "Fish-feeding",
    Prey %in% c("Inverts", "Krill") ~ "Krill-feeding"), 
    Region = case_when(
      Study_Area %in% c("Monterey", "SoCal", "Cordell Bank", "San Diego", "WA Coast")  ~ "Eastern North Pacific",
      Study_Area %in% c("Stellwagen", "Norway", "Azores", "Greenland") ~ "North Atlantic",
      Study_Area == "South Africa" ~ "South Africa",
      Study_Area == "Antarctic" ~ "Antarctic",
      Study_Area == "Chile" ~ "Chile"))


