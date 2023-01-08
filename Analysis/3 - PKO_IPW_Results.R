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
m1 <- zeroinfl(sb_death ~ pko | X,
               dist = "negbin",
               data = merged_ipw,
               weights = ipw)

# Count Unique Values for Cluster Variable to Specify Degrees of Freedom
merged_ipw %>%
  drop_na(sb_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m1_robust <- coeftest(m1, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths
m2 <- zeroinfl(sb_death ~ pko | X,
               dist = "negbin",
               data = merged_mmatch,
               weights = weights)

merged_mmatch %>%
  drop_na(sb_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m2_robust <- coeftest(m2, vcov = vcovHC, type = "HC1", df = 34, cluster = ~ccode)

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths
m3 <- zeroinfl(sb_death ~ pko | X,
               dist = "negbin",
               data = merged_cmatch,
               weights = weights)

merged_cmatch %>%
  drop_na(sb_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m3_robust <- coeftest(m3, vcov = vcovHC, type = "HC1", df = 31, cluster = ~ccode)

# Covariate Adjustment, Deaths
m4 <- zeroinfl(sb_death ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | X,
               dist = "negbin",
               data = merged)

merged %>%
  drop_na(sb_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m4_robust <- coeftest(m4, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# IPW: Dummy PKO Treatment, ATT, Events
m5 <- zeroinfl(sb_event ~ pko | X,
               dist = "negbin",
               data = merged_ipw,
               weights = ipw)

merged_ipw %>%
  drop_na(sb_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m5_robust <- coeftest(m5, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events
m6 <- zeroinfl(sb_event ~ pko | X,
               dist = "negbin",
               data = merged_mmatch,
               weights = weights)

merged_mmatch %>%
  drop_na(sb_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m6_robust <- coeftest(m6, vcov = vcovHC, type = "HC1", df = 34, cluster = ~ccode)

# Matching: CEM, Dummy PKO Treatment, ATT, Events
m7 <- zeroinfl(sb_event ~ pko | X,
               dist = "negbin",
               data = merged_cmatch,
               weights = weights)

merged_cmatch %>%
  drop_na(sb_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m7_robust <- coeftest(m7, vcov = vcovHC, type = "HC1", df = 31, cluster = ~ccode)

# Covariate Adjustment, Events
m8 <- zeroinfl(sb_event ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | X,
               dist = "negbin",
               data = merged)

merged %>%
  drop_na(sb_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m8_robust <- coeftest(m8, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# Plot Effects

# Regression Table

#######-------Non-State Based-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths
m9 <- zeroinfl(nsb_death ~ pko | X,
               dist = "negbin",
               data = merged_ipw,
               weights = ipw)

# Count Unique Values for Cluster Variable to Specify Degrees of Freedom
merged_ipw %>%
  drop_na(nsb_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m9_robust <- coeftest(m9, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths
m10 <- zeroinfl(nsb_death ~ pko | X,
               dist = "negbin",
               data = merged_mmatch,
               weights = weights)

merged_mmatch %>%
  drop_na(nsb_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m10_robust <- coeftest(m10, vcov = vcovHC, type = "HC1", df = 34, cluster = ~ccode)

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths
m11 <- zeroinfl(nsb_death ~ pko | X,
               dist = "negbin",
               data = merged_cmatch,
               weights = weights)

merged_cmatch %>%
  drop_na(nsb_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m11_robust <- coeftest(m11, vcov = vcovHC, type = "HC1", df = 31, cluster = ~ccode)

# Covariate Adjustment, Deaths
m12 <- zeroinfl(nsb_death ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | X,
               dist = "negbin",
               data = merged)

merged %>%
  drop_na(nsb_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m12_robust <- coeftest(m12, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# IPW: Dummy PKO Treatment, ATT, Events
m13 <- zeroinfl(nsb_event ~ pko | X,
               dist = "negbin",
               data = merged_ipw,
               weights = ipw)

merged_ipw %>%
  drop_na(nsb_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m13_robust <- coeftest(m13, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events
m14 <- zeroinfl(nsb_event ~ pko | X,
                dist = "negbin",
                data = merged_mmatch,
                weights = weights)

merged_mmatch %>%
  drop_na(nsb_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m14_robust <- coeftest(m14, vcov = vcovHC, type = "HC1", df = 34, cluster = ~ccode)

# Matching: CEM, Dummy PKO Treatment, ATT, Events
m15 <- zeroinfl(nsb_event ~ pko | X,
                dist = "negbin",
                data = merged_cmatch,
                weights = weights)

merged_cmatch %>%
  drop_na(nsb_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m15_robust <- coeftest(m15, vcov = vcovHC, type = "HC1", df = 31, cluster = ~ccode)

# Covariate Adjustment, Events
m16 <- zeroinfl(nsb_event ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | X,
                dist = "negbin",
                data = merged)

merged %>%
  drop_na(nsb_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m16_robust <- coeftest(m16, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# Plot Effects

# Regression Table

#######-------One-Sided Violence-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths
m17 <- zeroinfl(osv_death ~ pko | X,
               dist = "negbin",
               data = merged_ipw,
               weights = ipw)

merged_ipw %>%
  drop_na(osv_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m17_robust <- coeftest(m17, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths
m18 <- zeroinfl(osv_death ~ pko | X,
                dist = "negbin",
                data = merged_mmatch,
                weights = weights)

merged_mmatch %>%
  drop_na(osv_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m18_robust <- coeftest(m18, vcov = vcovHC, type = "HC1", df = 34, cluster = ~ccode)

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths
m19 <- zeroinfl(osv_death ~ pko | X,
                dist = "negbin",
                data = merged_cmatch,
                weights = weights)

merged_cmatch %>%
  drop_na(osv_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m19_robust <- coeftest(m19, vcov = vcovHC, type = "HC1", df = 31, cluster = ~ccode)

# Covariate Adjustment, Deaths
m20 <- zeroinfl(osv_death ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | X,
                dist = "negbin",
                data = merged)

merged %>%
  drop_na(osv_death) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m20_robust <- coeftest(m20, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# IPW: Dummy PKO Treatment, ATT, Events
m21 <- zeroinfl(osv_event ~ pko | X,
                dist = "negbin",
                data = merged_ipw,
                weights = ipw)

merged_ipw %>%
  drop_na(osv_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m21_robust <- coeftest(m21, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events
m22 <- zeroinfl(osv_event ~ pko | X,
                dist = "negbin",
                data = merged_mmatch,
                weights = weights)

merged_mmatch %>%
  drop_na(osv_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m22_robust <- coeftest(m22, vcov = vcovHC, type = "HC1", df = 34, cluster = ~ccode)

# Matching: CEM, Dummy PKO Treatment, ATT, Events
m23 <- zeroinfl(osv_event ~ pko | X,
                dist = "negbin",
                data = merged_cmatch,
                weights = weights)

merged_cmatch %>%
  drop_na(osv_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m23_robust <- coeftest(m23, vcov = vcovHC, type = "HC1", df = 31, cluster = ~ccode)

# Covariate Adjustment, Events
m24 <- zeroinfl(osv_event ~ pko + lgdppc + lnatres + lmilper + lpop + civ_war | X,
                dist = "negbin",
                data = merged)

merged %>%
  drop_na(osv_event) %>%
  summarize(distinct_ccode = n_distinct(ccode))

m24_robust <- coeftest(m24, vcov = vcovHC, type = "HC1", df = 78, cluster = ~ccode)

# Plot Effects

# Regression Table
