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
  rename(MPpc = lmilper)

m1 <- lm(lgdppc ~ pko + MPpc + lpop + ldeaths + wardur + democracy, 
         data = merged)

# Create and Visualize the Sensitivity Estimates
sens_results <- sensemakr(m1, treatment = "pko", 
                          benchmark_covariates = "MPpc",
                          kd = c(0.25, 0.5))

# Renaming Labels for Better Punctuation
x_lab <- expression(Partial ~ R^2 ~ of ~ Confounders(s) ~ With ~ the ~ Outcome)
y_lab <- expression(Partial ~ R^2 ~ of ~ Confounders(s) ~ With ~ the ~ Treatment)

plot(sens_results, 
     ylab = x_lab, 
     xlab = y_lab)

dev.off()

