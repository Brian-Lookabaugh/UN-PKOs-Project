############################################################################
###############-----------PKO IPW/Matching Analysis----------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "MASS", # Running Negative Binomials
  "pscl", # Zero-Inflated Models
  "sandwich", # Adjusting Standard Errors
  "lmtest", # Re-calculate Models With Updated Standard Errors
  install = FALSE
)

# Check for Zero-Inflation in Outcome Variables
merged_ipw <- merged %>% # Generate Dummies for Outcome That Equal 1 if Var > 0
  mutate(sbd_any = if_else(
    sb_death > 0, 1, 0
  )) %>%
  mutate(sbe_any = if_else(
    sb_event > 0, 1, 0
  )) %>%
  mutate(nsbd_any = if_else(
    nsb_death > 0, 1, 0
  )) %>%
  mutate(nsbe_any = if_else(
    nsb_event > 0, 1, 0
  )) %>%
  mutate(osvd_any = if_else(
    osv_death > 0, 1, 0
  )) %>%
  mutate(osve_any = if_else(
    osv_event > 0, 1, 0
  ))

#######-------State-Based-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths
m1 <- zeroinfl(sb_death ~ pko | sbd_any,
               dist = "negbin",
               data = merged_ipw,
               weights = ipw)

# Count Unique Values for Cluster Variable to Specify Degrees of Freedom
merged_ipw %>%
  filter(sb_death !is.na(.)) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m1_robust <- coeftest(m1, vcov = vcovHC, type = "HC1", df = , cluster = ~ccode)

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
