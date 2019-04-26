##############################################################
# Sensitivity analyses for water filtration and prey consumption 
##############################################################

#############################
# Load functions and packages
#############################

library(tidyverse)
library(scales)
library(lmodel2)
library(ggsci)
library(pse)

# Utility functions ----------------------------------------------------------

# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus
abbr_binom = function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}

# Labels for logarithmic scales
log_labels <- trans_format("log10", math_format(10^.x))





# Parameter CDFs
# rf (Empirical)
# Quantiles of the empirical feed rate distribution
rf_q <- transmute(Prey_consumpt_hr,
            ID, 
            Species, 
            rf_h = feeding_rate) %>% 
  drop_na %>% 
  group_by(Species) %>% 
  group_map(~ tibble(q_rf = list(function(p, ...) quantile(.x$rf_h, p)))) %>% 
  ungroup

#create min max table of prey weight
prey_wt_range <- Prey_consumpt_hr %>% 
  group_by(Species) %>% 
  summarise(min_prey_wt = min(prey_wgt_g),
            max_prey_wt = max(prey_wgt_g))


# My prey consumption model will be: MDC = hours feeding*feeding rate*prey density(the different scenarios),  also include *pouch fullness* later

# Vectorized function for calculating Esonar for sensitivity analysis
MDC_krill_fun <- function(prey_master_varying_HrperD) {
  with(prey_master_varying_HrperD, 
       {
         # MDC in kg
         hours_feeding * feeding_rate * p_wt * p_full / 1000
       }
  )
}


# Sensitivity analysis using pse package
feeding_data <- left_join(rf_q, prey_wt_range, by = "Species")

feeding_data %>% 
  group_by(Species) %>% 
  group_map(function(data, key) {
    # List of model parameters
    param <- c("hours_feeding", "feeding_rate", "p_wt", "p_full")
    # List of parameter distribution functions
    q <- list(hours_feeding = qunif, 
              feeding_rate = data$q_rf[[1]],
              p_wt = qunif,
              p_full = qunif)
    # List of distribution function parameters
    q_arg <- list(hours_feeding = list(min = 1, max = 12),
                  feeding_rate = list(),
                  p_wt = list(min = data$min_prey_wt, max = data$max_prey_wt),
                  p_full = list(min = 0.5, max = 1))
    
    # latin hypercube sample of parameter space
    feeding_LHS <- pse::LHS(MDC_krill_fun, param, 200, q, q_arg)
    
    SA_result <- feeding_LHS$data %>% 
      mutate(MDC_kg = feeding_LHS$res[,1,1])
    
    # Scatter
    SA_result %>% 
      gather(param, value, hours_feeding:p_full) %>%
      ggplot(aes(value, MDC_kg)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~ param, scales = "free_x") +
      labs(title = key$Species) +
      theme_minimal()
    ggsave(sprintf("figs/feeding_SA/%s_scatter.pdf", key$Species),
           width = 9,
           height = 6)
    
    # ECDF
    SA_result %>% 
      ggplot(aes(MDC_kg)) +
      stat_ecdf() +
      geom_vline(xintercept = mean(SA_result$MDC_kg),
                 linetype = "dashed") +
      labs(title = key$Species) +
      theme_minimal() 
    ggsave(sprintf("figs/feeding_SA/%s_ecdf.pdf", key$Species),
           width = 9,
           height = 6)
    
    tibble(mean_MDC = mean(SA_result$MDC_kg),
           median_MDC = median(SA_result$MDC_kg),
           iqr_MDC = IQR(SA_result$MDC_kg))
  })



# List of model parameters
param <- c("hours_feeding", "feeding_rate", "p_wt")
# List of parameter distribution functions
q <- list(hours_feeding = qunif, 
          feeding_rate = filter(rf_q, Species == "Balaenoptera musculus")$q_rf[[1]],
          h_max = ,
          p_dens =  filter(p_dens_q, Species == "Balaenoptera musculus" & scenario =="medBOUT_wt_g")$p_dens_q[[1]],  # NOT WORKING
          p_full = qunif)  # Still need to add this in somehow

# List of distribution function parameters
q_arg <- list(rf_h = list(),
              Ep_kJ = list(),
              ff_mult = list(min = 0.2, max = 5),
              CL_int = list(min = 1.46 / 2, max = 1.46 * 2),
              CL_slope = list(min = 0.0005 / 2, max = 0.0005 * 2))
# Latin hypercube sample of parameter space
bw_LHS <- pse::LHS(MDC_krill_fun, param, 200, q, q_arg)
# ECDF of model outputs
pse::plotecdf(bw_LHS)
# Scatter of model outputs w.r.t. parameters
pse::plotscatter(bw_LHS)
