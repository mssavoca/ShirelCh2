##################################################################
# Lunge rates and prey consumption calculations and visualizations ----
##################################################################

# load data and packages 
library(data.table)
library(readxl)
library(forcats)
library(tidyverse)
library(ggstance)
library(mgcv)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggpubr)

# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus
abbr_binom = function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}


load("~/Documents/Research Data/Whale research/ShirelCh2/rates.RData")

# Species-specific average length data (from Shirel's paper), to recalculate average engulfment capacity
v_data <- read_csv("MWmeasurements.csv") %>% 
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


# Species specific krill prey weight per m^3, from DEC's file BaleenWhaleForagingDistBigKrill100Bins, I think this is the NULL distribution
# Gives mean biomass of krill in kg per m^3
prey_dist <- tribble(
  ~SpeciesCode, ~ln_mean,          ~ln_sd,
  "bb",     log(10^-0.302053984),  log(10^(0.402095747^2))^0.5,    # E. superba
  "bp",     log(10^-0.207001968),  log(10^(0.397598067^2))^0.5,    # T.spin
  "bw",     log(10^-0.200903497),  log(10^(0.391739563^2))^0.5,    # T.spin
  "mn",     log(10^-0.21552581),  log(10^(0.400348263^2))^0.5      # T.spin
)

# read in whale population data. Current data from IUCN 2019 Redlist, whaling data compiled in Rocha et al. 2014
pop_data <- read_excel("Filtration project_Whale population and feeding information.xlsx", sheet = 1)


# Skips first two rows
tag_guide <- read_excel("TAG GUIDE_9.5.19.xlsx", skip = 2) %>% 
  rename(Study_Area = `Study_Area     _`,
         SpeciesCode = `Spec      _`,
         whaleLength = `Drone  _`) 


lunge_rates <- lunge_rates %>% 
  mutate_at(vars(Rate), ~replace(., is.nan(.), 0))


# Master filtration rate file
filtration_master <- lunge_rates %>% 
  left_join(select(tag_guide, ID, Study_Area, whaleLength), by = "ID") %>% 
  mutate(   
    # Make whale length a number
    whaleLength = parse_number(whaleLength),
    SpeciesCode = substr(ID,1,2),
    # Replace NAs in location with "SoCal"
    Study_Area = replace_na(Study_Area, "SoCal"),
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
      SpeciesCode %in% c("bb","ba") ~ "Balaenoptera bonaerensis", 
      SpeciesCode == "be" ~ "Balaenoptera edeni"),
    Region = case_when(
        Study_Area %in% c("Monterey", "SoCal", "Cordell Bank", "San Diego", "WA Coast")  ~ "Eastern North Pacific",
        Study_Area %in% c("Stellwagen", "Norway", "Azores", "Greenland") ~ "North Atlantic",
        Study_Area == "South Africa" ~ "South Africa",
        Study_Area == "Antarctic" ~ "Antarctic",
        Study_Area == "Chile" ~ "Chile")) %>% 
  # Join population numbers
  left_join(select(pop_data, -c(SpeciesCode, `Current Range`)), by = "Species") %>% 
  # Join species-median size data
  left_join(select(v_data, -CommonName), by = "SpeciesCode") %>% 
  # Join allometric equations
  left_join(engulf_allo, by = "SpeciesCode") %>% 
  # Calculate engulfment volumes
  mutate(
    BestLengthEst = coalesce(whaleLength, med_TLm),
    Engulfment_L = BestLengthEst ^ slope * 10 ^ intercept * 0.9766,
    EngulfVolPerHr = Engulfment_L*Rate) %>% 
  left_join(prey_dist, by = "SpeciesCode") %>% 
  ungroup


#plot of feeding rates per hour for ENP species 
pal <- c("B. bonaerensis" = "firebrick3", "B. edeni" = "darkorchid3",  "M. novaeangliae" = "gray30", "B. physalus" = "chocolate3", "B. musculus" = "dodgerblue2")

ENP_rates <- filtration_master %>% 
  filter(SpeciesCode != "bb" & prey_general == "Krill" & Region == "Eastern North Pacific") %>% 
  ggplot(aes(abbr_binom(Species), Rate, color = abbr_binom(Species))) +
  geom_boxplot() +
  geom_jitter() +
  scale_colour_manual(values = pal) +
  labs(x = "Species",
       y = "Feeding rate (lunges per hour)") + 
  facet_wrap(~Phase) +
  theme_classic() +
  theme(legend.position = "none")
ENP_rates


#determining daily feeding rates ----
# test example

# rows <- slice(filtration_master, 1:8)

rows_wide <- filtration_master %>% 
  select(ID, Species, Phase, Rate, `Time (hours)`) %>% 
  pivot_wider(names_from = Phase, values_from = c(Rate, `Time (hours)`))

result <- tibble(day_rate = base::sample(rows_wide$Rate_Day, prob = rows_wide$`Time (hours)_Day`, size = 1e4, replace = TRUE),
                 tw_rate = base::sample(rows_wide$Rate_Twilight, prob = rows_wide$`Time (hours)_Twilight`, size = 1e4, replace = TRUE),
                 night_rate = base::sample(rows_wide$Rate_Night, prob = rows_wide$`Time (hours)_Night`, size = 1e4, replace = TRUE),
                 daily_rate = day_rate*16 + tw_rate*2 + night_rate *6)




# Seems to be working
sample_rates <- function(rows, keys){
  rows_wide <- rows %>% 
    select(ID, Phase, Rate, `Time (hours)`, whaleLength) %>% 
    pivot_wider(names_from = Phase, values_from = c(Rate, `Time (hours)`))
  
  sample2 <- function(x, prob, size, replace) {
    if (all(prob == 0)) {
      0
    } else {
      sample(x, prob = prob, size = size, replace = replace)
    }
  }
  result <- tibble(day_rate = sample2(rows_wide$Rate_Day, prob = rows_wide$`Time (hours)_Day`, size = 1e4, replace = TRUE),
                   tw_rate = sample2(rows_wide$Rate_Twilight, prob = rows_wide$`Time (hours)_Twilight`, size = 1e4, replace = TRUE),
                   night_rate = sample2(rows_wide$Rate_Night, prob = rows_wide$`Time (hours)_Night`, size = 1e4, replace = TRUE),
                   daily_rate = day_rate*16 + tw_rate*2 + night_rate *6,
                   length_distrib = sample(na.omit(rows_wide$whaleLength), size = 1e4, replace = TRUE),
                   slope = rows$slope[1],
                   intercept = rows$intercept[1],
                   sp_sp_prey_mass_per_m3 = rlnorm(1e4, rows$ln_mean, rows$ln_sd))
}


d_strapped <- filtration_master %>% 
  filter(prey_general == "Krill") %>% 
  group_by(Species) %>% 
  group_modify(sample_rates) %>% 
  mutate(measured_engulfment_cap_m3 = (length_distrib ^ slope * 10 ^ intercept * 0.9766)/1000,
         prey_mass_per_lunge = sp_sp_prey_mass_per_m3*measured_engulfment_cap_m3,
         prey_mass_per_day_kg = prey_mass_per_lunge*daily_rate)


# feeding rates plots ----
pal <- c("B. bonaerensis" = "firebrick3", "B. edeni" = "darkorchid3",  "M. novaeangliae" = "gray30", "B. physalus" = "chocolate3", "B. musculus" = "dodgerblue2")

ENP_daily_rate <- ggplot(d_strapped, 
                         aes(x = abbr_binom(Species), y = daily_rate, fill = abbr_binom(Species))) +
  geom_flat_violin(position = position_nudge(x = 0.1, y = 0), alpha = .8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  coord_flip() + 
  scale_fill_manual(values = pal) +
  labs(x = "Species",
       y = "Lunges per day") + 
  theme_classic(base_size = 14) +
  theme(legend.position = "none")
ENP_daily_rate



ENP_daily_biomass_ingested <- ggplot(d_strapped, 
                         aes(x = abbr_binom(Species), y = prey_mass_per_day_kg/1000, fill = abbr_binom(Species))) +
  geom_flat_violin(position = position_nudge(x = 0.1, y = 0), alpha = .8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  ylim(0,50) +
  coord_flip() + 
  scale_fill_manual(values = pal) +
  labs(x = "Species",
       y = "Tonnes of prey consumed per day") + 
  theme_classic(base_size = 14) +
  theme(legend.position = "none")
ENP_daily_biomass_ingested





MN_ENP_daily_rate <- filtration_master %>% 
  filter(SpeciesCode == "mn" & Region == "Eastern North Pacific") %>% 
  group_by(Species) %>% 
  group_modify(sample_rates) %>% 
  ggplot(aes(x = prey_general, y = daily_rate, fill = prey_general)) +
  geom_flat_violin(position = position_nudge(x = 0.1, y = 0), alpha = .8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  coord_flip() + 
  #scale_fill_manual(values = pal) +
  labs(x = "Species",
       y = "Lunges per day") + 
  theme_classic(base_size = 14) +
  theme(legend.position = "none")




# Extra code below here ----


source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))



select(Region, Species, ID, Phase, Rate, Dur = `Time (hours)`) %>%

d_ENP_mn = filter(filtration_master, Phase == "Day" & Region == "Eastern North Pacific" & SpeciesCode == "mn" & prey_general == "Krill")
t_ENP_mn = filter(filtration_master, Phase == "Twilight" & Region == "Eastern North Pacific" & SpeciesCode == "mn" & prey_general == "Krill")
n_ENP_mn = filter(filtration_master, Phase == "Night" & Region == "Eastern North Pacific" & SpeciesCode == "mn" & prey_general == "Krill")
d_ENP_bp = filter(filtration_master, Phase == "Day" & Region == "Eastern North Pacific" & SpeciesCode == "bp")
t_ENP_bp = filter(filtration_master, Phase == "Twilight" & Region == "Eastern North Pacific" & SpeciesCode == "bp")
n_ENP_bp = filter(filtration_master, Phase == "Night" & Region == "Eastern North Pacific" & SpeciesCode == "bp")
d_ENP_bw = filter(filtration_master, Phase == "Day" & Region == "Eastern North Pacific" & SpeciesCode == "bw")
t_ENP_bw = filter(filtration_master, Phase == "Twilight" & Region == "Eastern North Pacific" & SpeciesCode == "bw")
n_ENP_bw = filter(filtration_master, Phase == "Night" & Region == "Eastern North Pacific" & SpeciesCode == "bw")

  
mn_wgt_hourly_rates <- tibble(
  Day = base::sample(d_ENP_mn$Rate, prob = d_ENP_mn$`Time (hours)`, size = 1e4, replace = TRUE), 
  Twilight = base::sample(t_ENP_mn$Rate, prob = t_ENP_mn$`Time (hours)`, size = 1e4, replace = TRUE), 
  Night = base::sample(n_ENP_mn$Rate, prob = n_ENP_mn$`Time (hours)`, size = 1e4, replace = TRUE),
  Species = "M. novaeangliae")


bp_wgt_hourly_rates <- tibble(
  Day = base::sample(d_ENP_bp$Rate, prob = d_ENP_bp$`Time (hours)`, size = 1e4, replace = TRUE), 
#  Twilight = base::sample(t_ENP_bp$Rate, prob = t_ENP_bp$`Time (hours)`, size = 1e4, replace = TRUE),   #doesnt work because theres no data 
#  Night = base::sample(n_ENP_bp$Rate, prob = n_ENP_bp$`Time (hours)`, size = 1e4, replace = TRUE),      #doesnt work because theres no data
  Species = "B. physalus")

bw_wgt_hourly_rates <- tibble(
  Day = base::sample(rows$Rate, prob = rows$`Time (hours)`, size = 1e4, replace = TRUE), 
  Twilight = base::sample(t_ENP_bw$Rate, prob = t_ENP_bw$`Time (hours)`, size = 1e4, replace = TRUE), 
  Night = base::sample(n_ENP_bw$Rate, prob = n_ENP_bw$`Time (hours)`, size = 1e4, replace = TRUE),
  Species = "B. musculus")


ENP_wgt_hourly_rates <- rbind(mn_wgt_hourly_rates, bw_wgt_hourly_rates) %>% 
  mutate(daily_rate = Day*16 + Twilight*2 + Night *6,
         Species = factor(Species))








ggplot(ENP_wgt_hourly_rates, aes(x = Species, y = daily_rate, fill = Species)) +
  geom_violin() +
  geom_jitter(width = 0.15, size = 0.1) +
  theme_classic()

ENP_daily_rates <- ggplot(ENP_wgt_hourly_rates,
                          aes(daily_rate, Species, fill = Species))+
  #geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
  geom_density() +
  #geom_violin() +
  geom_point(position = position_jitter(width = .15), size = .25)+
  scale_fill_manual(values = pal) +
  facet_grid(.~Species) +
  theme_classic(base_size = 14)
ENP_daily_rates



# Max's complicated stuff ----
day_hours <- tribble(
  ~Location, ~d_hours,   ~Tw_hours, ~N_hours,
  "Temperate",     16, 2, 6, 
  "Polar",     19,  4, 1
)

d = filter(filtration_master, Phase == "Day")
t = filter(filtration_master, Phase == "Twilight")
n = filter(filtration_master, Phase == "Night")

filtration_master %>% 
  ungroup %>% 
  filter(Region %in% c("Antarctic", "Eastern North Pacific")) %>% 
  select(Region, Species, ID, Phase, Rate, Dur = `Time (hours)`) %>%
  group_by(Species, Region) %>% 
  group_modify(function(data, key) {
    d <- filter(data, Phase == "Day")
    t <- filter(data, Phase == "Twilight")
    n <- filter(data, Phase == "Night")
    
    if (nrow())
      
      sz <- 1e2
    tryCatch(
      tibble(
        Day = base::sample(d$Rate, prob = d$Dur, size = sz, replace = TRUE), 
        Twilight = base::sample(t$Rate, prob = t$Dur, size = sz, replace = TRUE), 
        Night = base::sample(n$Rate, prob = n$Dur, size = sz, replace = TRUE)),
      error = function(e) browser())
  }) %>% View



