#################################################################################
## Analyses and visualizations for foraging depth in relation to plastic ingestion
#################################################################################

OdontoceteData <- read_csv("foragestats_combined_ko2.csv")
  OdontoceteData$Species <- sub("_", " ", OdontoceteData$Species)

# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus
abbr_binom = function(binom) {
    paste(str_sub(binom, 1, 1), 
          str_extract(binom, " .*"), 
          sep = ".")
  }
OdontoceteData <- mutate(OdontoceteData, Binomial = abbr_binom(Species))


forage_depth <- ggplot(OdontoceteData, aes(fct_reorder(Binomial, -Depth_m_median, fun = median, .desc =TRUE), 
                                           -Depth_m_median, color = Grouping)) +
  geom_boxplot() + geom_point(aes(alpha = 0.9)) + geom_jitter() +
  geom_hline(yintercept= -200, linetype="dashed", color = "red") +
  geom_hline(yintercept= -1000, linetype="dotted", color = "red") +
  annotate("text", x = 3, y=c(-700, -1300), 
           label = c("disphotic (twilight) zone", "aphotic (midnight) zone"),
                     fontface = 'bold') +
  theme_bw() +
  xlab("Species") + ylab("Median foraging depth (m)") +
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1, face = "italic"),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=13, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"))
forage_depth + theme(legend.position="top") +   scale_alpha(guide = 'none')
