############################################################################
###############------------PKO IPW/Matching Set Up-----------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "PanelMatch", # Matching/Weighting Set-Up With Panel Data
  install = FALSE
)

# Create a Treatment-Variation Plot
merged <- merged %>%
  mutate(year = as.integer(year))

merged <- as.data.frame(merged)

merged <- merged %>% # Drop Observations Pre-1989 and Post-2007 for Plot
  filter(year >= 1989, year < 2008)

tv_plot <- DisplayTreatment(
  unit.id = "stateabb",
  time.id = "year",
  xlab = "Year",
  ylab = "Countries",
  y.size = 7,
  title = "PKO Treatment Across Countries and Years",
  legend.position = "bottom",
  legend.labels = c("No PKO", "PKO"),
  hide.x.tick.label = TRUE,
  treatment = "pko",
  data = merged
) + 
  theme(axis.text.x = element_text(angle = 0, size = 6.5, vjust = 0.5)) +
  scale_x_discrete(breaks = c(1990, 1995, 2000, 2005))

ggsave(
  "tv_plot.png",
  width = 6,
  height = 8,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/UN PKOs Project/Graphics"
)

# Create Mahalanobis Nearest-Neighbor (NN) Matched Set: 4 Lags, Up to 5 Matches
merged <- merged %>%
  mutate(ccode = as.integer(ccode)) %>%
  select(-c(stateabb)) # PanelMatch Will Not Run With Non-Numeric/Integer Data

nn_match_5 <- PanelMatch(lag = 4,
                       time.id = "year",
                       unit.id = "ccode",
                       treatment = "pko",
                       refinement.method = "mahalanobis",
                       size.match = 5,
                       data = merged,
                       covs.formula = ~ 
                         I(lag(lpop, 1:4)) +
                         I(lag(lmilper, 1:4)) +
                         I(lag(ldeaths, 1:4)) +
                         I(lag(wardur, 1:4)),
                       qoi = "att",
                       outcome.var = "lgdppc",
                       lead = 0:4,
                       use.diagonal.variance.matrix = TRUE,
                       restrict.control.period = 4)

# Create Mahalanobis Nearest-Neighbor (NN) Matched Set: 4 Lags, Up to 10 Matches
nn_match_10 <- PanelMatch(lag = 4,
                       time.id = "year",
                       unit.id = "ccode",
                       treatment = "pko",
                       refinement.method = "mahalanobis",
                       size.match = 10,
                       data = merged,
                       covs.formula = ~ 
                         I(lag(lpop, 1:4)) +
                         I(lag(lmilper, 1:4)) +
                         I(lag(ldeaths, 1:4)) +
                         I(lag(wardur, 1:4)),
                       qoi = "att",
                       outcome.var = "lgdppc",
                       lead = 0:4,
                       use.diagonal.variance.matrix = TRUE,
                       restrict.control.period = 4)

# Create IPW Weighted Set: 4 Lags
ipw <- PanelMatch(lag = 4,
                        time.id = "year",
                        unit.id = "ccode",
                        treatment = "pko",
                        refinement.method = "ps.weight",
                        data = merged,
                        covs.formula = ~
                          I(lag(lpop, 1:4)) +
                          I(lag(lmilper, 1:4)) +
                          I(lag(ldeaths, 1:4)) +
                          I(lag(wardur, 1:4)),
                        qoi = "att",
                        outcome.var = "lgdppc",
                        lead = 0:4,
                        restrict.control.period = 4)

# Covariate Balance Plot Post-Refinement
# Create a Matched Set Without Refinement
no_ref <- PanelMatch(lag = 4,
                     time.id = "year",
                     unit.id = "ccode",
                     treatment = "pko",
                     refinement.method = "none",
                     data = merged,
                     covs.formula = ~
                       I(lag(lpop, 1:4)) +
                       I(lag(lmilper, 1:4)) +
                       I(lag(ldeaths, 1:4)) +
                       I(lag(wardur, 1:4)),
                     size.match = 5,
                     qoi = "att",
                     outcome.var = "lgdppc",
                     lead = 0:4,
                     restrict.control.period = 4)

balance_scatter(matched_set_list = list(nn_match_5$att, nn_match_10$att), 
                           data = merged,
                           covariates = c("lpop", "lmilper", "wardur", "ldeaths"),
                           x.axis.label = "Before Refinement",
                           y.axis.label = "After Refinement")
