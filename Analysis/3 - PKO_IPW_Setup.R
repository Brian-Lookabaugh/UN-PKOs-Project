############################################################################
###############------------PKO IPW/Matching Set Up-----------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "broom", # Converting Model Output to Data Frames
  "WeightIt", # IPW
  "MatchIt", # Matching
  install = FALSE
)

# Disable Scientific Notation
options(scipen = 999)

#######-------IPW-------#######
# Generate the Weights
pko_weights <- weightit(pko ~ lnatres + lgdppc + lpop + lmilper + civ_War,
                        data = merged,
                        estimand = "ATT",
                        method = "ps")

# Merge the Weights Into the Data Set
merged <- merged %>%
  mutate(ipw = pko_weights$weights)

# Trim Extreme Weights

# Balancing

#######-------Mahalanobis Distance Matching-------#######
# Generate the Matches
m_matched <- matchit(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war,
                     data = merged,
                     method = "nearest",
                     estimand = "ATT",
                     distance = "mahalanobis")

m_matched <- match.data(m_matched)

# Balancing

#######-------Coarsened Exact Matching-------#######

c_matched <- matchit(pko ~ lnatres + lgdppc + lpop + lmilper + civ_war,
                     data = merged,
                     method = "cem",
                     estimand = "ATT")

c_matched <- match.data(c_matched)

# Balancing

