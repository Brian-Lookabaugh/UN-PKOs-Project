############################################################################
###############-----------PKO Sensitivity Analysis-----------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "sensemakr", # Sensitivity Analysis
  install = FALSE
)

# Create Base Model With No Matching
m1 <- lm(lgdppc ~ pko + lmilper + lpop + ldeaths + wardur + democracy, data = merged)

# Create the Sensitivity Estimates
sens_results_deaths <- sensemakr(m1, treatment = "pko", benchmark_covariates = "ldeaths",
                          kd = 1:2)

sens_results_dem <- sensemakr(m1, treatment = "pko", benchmark_covariates = "democracy",
                              kd = 1)

# Plot Sensitivity Estimates
