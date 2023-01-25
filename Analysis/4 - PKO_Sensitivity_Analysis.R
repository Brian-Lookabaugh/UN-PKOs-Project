############################################################################
###############-----------PKO Sensitivity Analysis-----------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "sensemakr", # Sensitivity Analysis
  install = FALSE
)

# Create Base Model With No Matching
merged <- merged %>% # Renaming for Better Label on the Plot
  rename(Democracy = democracy)

m1 <- lm(lgdppc ~ pko + lmilper + lpop + Deaths + wardur + Democracy, data = merged)

# Create and Visualize the Sensitivity Estimates
sens_results_dem <- sensemakr(m1, treatment = "pko", benchmark_covariates = "Democracy",
                              kd = c(0.25, 0.5, 1))

# Renaming Labels for Better Punctuation
x_lab <- expression(Partial ~ R^2 ~ of ~ Confounders(s) ~ With ~ the ~ Outcome)
y_lab <- expression(Partial ~ R^2 ~ of ~ Confounders(s) ~ With ~ the ~ Treatment)

plot(sens_results_dem, 
     ylab = x_lab, 
     xlab = y_lab)

dev.off()

