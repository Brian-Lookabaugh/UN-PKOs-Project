############################################################################
###############------------PKO IPW/Matching Set Up-----------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "broom", # Converting Model Output to Data Frames
  "WeightIt", # IPW
  "MatchIt", # Matching
  "cobalt", # Assessing Balance
  install = FALSE
)

#######-------IPW-------#######
# Generate Propensity Scores Manually to Investigate Extreme Propensity Scores
prop_pko_model <- glm(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war,
                      family = binomial(link = "logit"),
                      data = merged)

merged_ipw <- augment_columns(prop_pko_model, merged,
                             type.predict = "response") %>%
  rename(propensity = .fitted) %>%
  # Filter Propensity Scores Less than 0.05 (There are None > 0.95)
  filter(propensity >= 0.05)

# Generate the Weights
pko_weights <- weightit(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war,
                        data = merged,
                        estimand = "ATT",
                        method = "ps")

# Merge the Weights Into the Data Set
merged_ipw <- merged %>%
  mutate(ipw = pko_weights$weights)

#######-------Mahalanobis Distance Matching-------#######
# Generate the Matches
merged_mmatch <- matchit(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war,
                     data = merged,
                     method = "nearest",
                     estimand = "ATT",
                     distance = "mahalanobis",
                     replace = TRUE)

#######-------Coarsened Exact Matching-------#######

merged_cmatch <- matchit(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war,
                     data = merged,
                     method = "cem",
                     estimand = "ATT")

# Assess Balance
bal.tab(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war, # Not Weighted/Matched
        data = merged,
        estimand = "ATT",
        thresholds = c(m = .05))

bal.tab(pko_weights, # IPW Weighted
        stats = c("m", "v"),
        thresholds = c(m = .05))

bal.tab(merged_mmatch, # NN Matched
        stats = c("m", "v"),
        thresholds = c(m = .05))

bal.tab(merged_cmatch, # CEM Matched
        stats = c("m", "v"),
        thresholds = c(m = .05))

# Generating New Names for Confounders for Visualization
v_names <- data.frame(old = c("lnatres", "lgdppc", "lpop", "lmilper", "civ_war"),
                      new = c("Natural Resources pc", 
                              "GDP pc", "Population", "Military Personnel pc",
                              "Civil War")
)

# Covariate Balance Plots
cb_lplot <- love.plot(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war,
                      data = merged, estimand = "ATT",
                      weights = list(w1 = get.w(pko_weights),
                                     w2 = get.w(merged_mmatch),
                                     w3 = get.w(merged_cmatch)),
                      abs = TRUE,
                      stars = "raw",
                      line = TRUE,
                      thresholds = c(m = .05),
                      var.order = "unadjusted",
                      var.names = v_names,
                      colors = c("#440154", "#2d708e", "#52c569", "#c2df23"),
                      sample.names = c("Original", "IPW", "NN Matching", "CEM")
                      ) +
  labs(caption = "* indicates the reporting of raw difference in means") +
  theme(plot.caption.position = "plot")

ggsave(
  "cb_lplot.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)

# Density and Bar Plots (Not for Paper, just EDA)

# IPW
ipw_den_gdp <- bal.plot(pko_weights, var.name = "lgdppc", which = "both")
ipw_den_milper <- bal.plot(pko_weights, var.name = "lmilper", which = "both")
ipw_den_natres <- bal.plot(pko_weights, var.name = "lnatres", which = "both")
ipw_den_pop <- bal.plot(pko_weights, var.name = "lpop", which = "both")
ipw_bar_civ <- bal.plot(pko_weights, var.name = "civ_war", which = "both")

# NN Matching
mmatch_den_gdp <- bal.plot(merged_mmatch, var.name = "lgdppc", which = "both")
mmatch_den_milper <- bal.plot(merged_mmatch, var.name = "lmilper", which = "both")
mmatch_den_natres <- bal.plot(merged_mmatch, var.name = "lnatres", which = "both")
mmatch_den_pop <- bal.plot(merged_mmatch, var.name = "lpop", which = "both")
mmatch_bar_civ <- bal.plot(merged_mmatch, var.name = "civ_war", which = "both")

# CEM
cmatch_den_gdp <- bal.plot(merged_cmatch, var.name = "lgdppc", which = "both")
cmatch_den_milper <- bal.plot(merged_cmatch, var.name = "lmilper", which = "both")
cmatch_den_natres <- bal.plot(merged_cmatch, var.name = "lnatres", which = "both")
cmatch_den_pop <- bal.plot(merged_cmatch, var.name = "lpop", which = "both")
cmatch_bar_civ <- bal.plot(merged_cmatch, var.name = "civ_war", which = "both")

# Convert Matches to Data Set
merged_mmatch <- match.data(merged_mmatch)
merged_cmatch <- match.data(merged_cmatch)

