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

merged <- augment_columns(prop_pko_model, merged,
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
merged <- merged %>%
  mutate(ipw = pko_weights$weights)

# Balancing (Numerical and Graphic Evaluation)


#######-------Mahalanobis Distance Matching-------#######
# Generate the Matches
m_matched <- matchit(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war,
                     data = merged,
                     method = "nearest",
                     estimand = "ATT",
                     distance = "mahalanobis")

# Balancing (Numerical and Graphic Evaluation)

# Include Only the Matched Data
m_matched <- match.data(m_matched)

#######-------Coarsened Exact Matching-------#######

c_matched <- matchit(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war,
                     data = merged,
                     method = "cem",
                     estimand = "ATT")

# Balancing (Numerical and Graphic Evaluation)

# Include Only the Matched Data
c_matched <- match.data(c_matched)

