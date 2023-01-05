############################################################################
###############-----------PKO IPW/Matching Analysis----------###############
############################################################################

pacman::p_load(
  "tidyverse" # Data Manipulation and Visualization
)

# Check for Zero-Inflation in Outcome Variables
merged <- merged %>% # Generate Dummies for Outcome That Equal 1 if Var > 0
  mutate(sbd_greater = if_else(
    sb_death > 0, 1, 0
  )) %>%
  mutate(sbe_greater = if_else(
    sb_event > 0, 1, 0
  )) %>%
  mutate(nsbd_greater = if_else(
    nsb_death > 0, 1, 0
  )) %>%
  mutate(nsbe_greater = if_else(
    nsb_event > 0, 1, 0
  )) %>%
  mutate(osvd_greater = if_else(
    osv_death > 0, 1, 0
  )) %>%
  mutate(osve_greater = if_else(
    osv_event > 0, 1, 0
  ))

#######-------State-Based-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths

# IPW: Dummy PKO Treatment, ATT, Events

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths

# Matching: CEM, Dummy PKO Treatment, ATT, Events

# Covariate Adjustment, Deaths

# Covariate Adjustment, Events

# Plot Effects

# Regression Table

#######-------Non-State Based-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths

# IPW: Dummy PKO Treatment, ATT, Events

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths

# Matching: CEM, Dummy PKO Treatment, ATT, Events

# Covariate Adjustment, Deaths

# Covariate Adjustment, Events

# Plot Effects

# Regression Table

#######-------One-Sided Violence-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths

# IPW: Dummy PKO Treatment, ATT, Events

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths

# Matching: CEM, Dummy PKO Treatment, ATT, Events

# Covariate Adjustment, Deaths

# Covariate Adjustment, Events

# Plot Effects

# Regression Table
