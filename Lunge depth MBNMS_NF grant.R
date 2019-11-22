#############################################################
# Lunge depth by species plot for Shirel's Nancy Foster Grant
#############################################################


library(tidyverse)

lunge_depths_in_MBNMS <- read_csv("lunge_depth_for_SKR_inMBNMS.csv") %>% 
  mutate(binomial_short = case_when(species == "bp" ~ "B. physalus",
                                    species == "bw" ~ "B. musculus",
                                    species == "mn" ~ "M. novaeangliae"))


# sample size; all whales here are labeled as krill-feeding whales in the tag guide
sample_size <- lunge_depths_in_MBNMS %>% 
  group_by(binomial_short) %>% 
  summarise(sample_size = n_distinct(ID))


pal <- c("M. novaeangliae" = "gray30", "B. physalus" = "chocolate3", "B. musculus" = "dodgerblue2" )

feeding_depth_by_sp <- ggplot(lunge_depths_in_MBNMS, aes(lunge_depth, color = binomial_short)) +
  #geom_density() +
  geom_freqpoly() + # my preference is for this one
  #geom_histogram() +
  facet_wrap(.~binomial_short, scales = "free") + 
  coord_flip() +
  scale_x_reverse() +
  scale_color_manual(values = pal) +
  labs(x = "Feeding depth (m)") +
  theme_classic(base_size = 18) +
  theme(legend.position = "none",
        strip.text = element_text(face = "italic"))
feeding_depth_by_sp
