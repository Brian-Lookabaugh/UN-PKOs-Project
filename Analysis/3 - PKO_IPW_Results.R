############################################################################
###############-----------PKO IPW/Matching Analysis----------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "MASS", # Running Negative Binomials
  "pscl", # Zero-Inflated Models
  "sandwich", # Adjusting Standard Errors
  "lmtest", # Re-calculate Models With Updated Standard Errors
  "modelsummary", # Comparing Models
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
m1 <- zeroinfl(sb_death ~ pko | pko,
               dist = "negbin",
               data = merged_ipw,
               weights = ipw)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths
m2 <- zeroinfl(sb_death ~ pko | pko,
               dist = "negbin",
               data = merged_mmatch,
               weights = weights)

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths
m3 <- zeroinfl(sb_death ~ pko | pko,
               dist = "negbin",
               data = merged_cmatch,
               weights = weights)

# Covariate Adjustment, Deaths
m4 <- zeroinfl(sb_death ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | 
                 pko + lgdppc + lnatres + lmilper + lpop + civ_war,
               dist = "negbin",
               data = merged)

# IPW: Dummy PKO Treatment, ATT, Events
m5 <- zeroinfl(sb_event ~ pko | pko,
               dist = "negbin",
               data = merged_ipw,
               weights = ipw)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events
m6 <- zeroinfl(sb_event ~ pko | pko,
               dist = "negbin",
               data = merged_mmatch,
               weights = weights)

# Matching: CEM, Dummy PKO Treatment, ATT, Events
m7 <- zeroinfl(sb_event ~ pko | pko,
               dist = "negbin",
               data = merged_cmatch,
               weights = weights)

# Covariate Adjustment, Events
m8 <- zeroinfl(sb_event ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | 
                 pko + lgdppc + lnatres + lmilper + lpop + civ_war,
               dist = "negbin",
               data = merged)

# Plot Effects

# Regression Table

#######-------Non-State Based-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths
m9 <- zeroinfl(nsb_death ~ pko | pko,
               dist = "negbin",
               data = merged_ipw,
               weights = ipw)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths
m10 <- zeroinfl(nsb_death ~ pko | pko,
               dist = "negbin",
               data = merged_mmatch,
               weights = weights)

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths
m11 <- zeroinfl(nsb_death ~ pko | pko,
               dist = "negbin",
               data = merged_cmatch,
               weights = weights)

# Covariate Adjustment, Deaths
m12 <- zeroinfl(nsb_death ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | 
                  pko + lgdppc + lnatres + lmilper + lpop + civ_war,
               dist = "negbin",
               data = merged)

# IPW: Dummy PKO Treatment, ATT, Events
m13 <- zeroinfl(nsb_event ~ pko | pko,
               dist = "negbin",
               data = merged_ipw,
               weights = ipw)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events
m14 <- zeroinfl(nsb_event ~ pko | pko,
                dist = "negbin",
                data = merged_mmatch,
                weights = weights)

# Matching: CEM, Dummy PKO Treatment, ATT, Events
m15 <- zeroinfl(nsb_event ~ pko | pko,
                dist = "negbin",
                data = merged_cmatch,
                weights = weights)

# Covariate Adjustment, Events
m16 <- zeroinfl(nsb_event ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | 
                  pko + lgdppc + lnatres + lmilper + lpop + civ_war,
                dist = "negbin",
                data = merged)

# Plot Effects

# Regression Table

#######-------One-Sided Violence-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths
m17 <- zeroinfl(osv_death ~ pko | pko,
               dist = "negbin",
               data = merged_ipw,
               weights = ipw)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths
m18 <- zeroinfl(osv_death ~ pko | pko,
                dist = "negbin",
                data = merged_mmatch,
                weights = weights)

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths
m19 <- zeroinfl(osv_death ~ pko | pko,
                dist = "negbin",
                data = merged_cmatch,
                weights = weights)

# Covariate Adjustment, Deaths
m20 <- zeroinfl(osv_death ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | 
                  pko + lgdppc + lnatres + lmilper + lpop + civ_war,
                dist = "negbin",
                data = merged)

# IPW: Dummy PKO Treatment, ATT, Events
m21 <- zeroinfl(osv_event ~ pko | pko,
                dist = "negbin",
                data = merged_ipw,
                weights = ipw)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events
m22 <- zeroinfl(osv_event ~ pko | pko,
                dist = "negbin",
                data = merged_mmatch,
                weights = weights)

# Matching: CEM, Dummy PKO Treatment, ATT, Events
m23 <- zeroinfl(osv_event ~ pko | pko,
                dist = "negbin",
                data = merged_cmatch,
                weights = weights)

# Covariate Adjustment, Events
m24 <- zeroinfl(osv_event ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | 
                  pko + lgdppc + lnatres + lmilper + lpop + civ_war,
                dist = "negbin",
                data = merged)

# Plot Effects

# Regression Table

#######-------Conflict-Level Analysis-------#######
# Assess Zero-Inflation

# IPW: Deaths

# NN Matching: Deaths

# CEM: Deaths

# Regression Adjustment: Deaths

# IPW: Events

# NN Matching: Events

# CEM: Events

# Regression Adjustment: Events

# Plot Effects

# Generate Tables

#######-------Post-Conflict Level Analysis-------#######
# Assess Zero-Inflation

# IPW: Deaths

# NN Matching: Deaths

# CEM: Deaths

# Regression Adjustment: Deaths

# IPW: Events

# NN Matching: Events

# CEM: Events

# Regression Adjustment: Events

# Plot Effects

# Generate Tables
