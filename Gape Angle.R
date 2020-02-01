# Starting with FilterTime_Raw...

# Calculate mouth open time as a proxy for the magnitude of the engulfment
FilterTime_mot <- mutate(FilterTime_Raw, mot = (MC - MO) / 10) # in seconds

# Starting with Minkes...
FilterTime_bb <- filter(FilterTime_mot, SpeciesCode == "bb")
# Fit a linear model to filter time ~ engulfment 
bb_lm <- lm(purge1 ~ mot, data = FilterTime_bb)
summary(bb_lm)
# Plot, color coded by individual
ggplot(FilterTime_bb, aes(mot, purge1)) +
  geom_point(aes(color = factor(whaleID)), shape = 1) +
  geom_abline(intercept = coef(bb_lm)[1],
              slope = coef(bb_lm)[2],
              color = "red") +
  labs(caption = sprintf("y = %.2fx + %.2f",coef(bb_lm)[2], coef(bb_lm)[1])) +
  theme_minimal()
