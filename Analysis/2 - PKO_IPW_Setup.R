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

# Balancing (Numerical and Graphic Evaluation)
bal.tab(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war, # Not Weighted 
        data = merged,
        estimand = "ATT",
        thresholds = c(m = .05))

bal.tab(pko_weights, # Weighted
        stats = c("m", "v"),
        thresholds = c(m = .05))

# Generating New Names for Confounders for Visualization
v_names <- data.frame(old = c("lnatres", "lgdppc", "lpop", "lmilper", "civ_war"),
                      new = c("Natural Resources per capita", 
                              "GDP per capita", "Population", "Military Personnel per capita",
                              "Civil War")
)

ipw_lplot <- love.plot(pko_weights, binary = "std", threshold = .05, 
                       drop.distance = TRUE, # Removing Propensity Score from Viz
                       var.order = "unadjusted", var.names = v_names,
                       colors = c("#862781", "#e34e65"))

ggsave(
  "ipw_lplot.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)

#######-------Mahalanobis Distance Matching-------#######
# Generate the Matches
merged_mmatch <- matchit(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war,
                     data = merged,
                     method = "nearest",
                     estimand = "ATT",
                     distance = "mahalanobis")

# Balancing (Numerical and Graphic Evaluation)
bal.tab(merged_mmatch, # Matched
        stats = c("m", "v"),
        thresholds = c(m = .05))

mmatch_lplot <- love.plot(merged_mmatch, binary = "std", threshold = .05,
                       var.order = "unadjusted", var.names = v_names,
                       colors = c("#862781", "#e34e65"))

ggsave(
  "mmatch_lplot.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)

# Include Only the Matched Data
merged_mmatch <- match.data(merged_mmatch)

#######-------Coarsened Exact Matching-------#######

merged_cmatch <- matchit(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war,
                     data = merged,
                     method = "cem",
                     estimand = "ATT")

# Balancing (Numerical and Graphic Evaluation)
bal.tab(merged_cmatch, # Matched
        stats = c("m", "v"),
        thresholds = c(m = .05))

cmatch_lplot <- love.plot(merged_cmatch, binary = "std", threshold = .05,
                          var.order = "unadjusted", var.names = v_names,
                          colors = c("#862781", "#e34e65"))

ggsave(
  "cmatch_lplot.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)

# Include Only the Matched Data
merged_cmatch <- match.data(merged_cmatch)

