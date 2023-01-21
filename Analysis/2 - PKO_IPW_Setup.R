############################################################################
###############------------PKO IPW/Matching Set Up-----------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "PanelMatch", # Matching/Weighting Set-Up With Panel Data
  "grDevices", # Converting Base Graphics to ggplot-like Objects
  "cowplot", # Combining Plots
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

# Covariate Balance Plot Post-Refinement
merged <- merged %>%
  mutate(ccode = as.integer(ccode)) %>%
  select(-c(stateabb)) # PanelMatch Will Not Run With Non-Numeric/Integer Data

# NN Matching - 5 Matches - 1 Lag
nn_match_5_1 <- PanelMatch(lag = 1,
                         time.id = "year",
                         unit.id = "ccode",
                         treatment = "pko",
                         refinement.method = "mahalanobis",
                         size.match = 5,
                         data = merged,
                         covs.formula = ~ 
                           I(lag(lpop, 1)) +
                           I(lag(lmilper, 1)) +
                           I(lag(ldeaths, 1)) +
                           I(lag(wardur, 1)),
                         qoi = "att",
                         outcome.var = "lgdppc",
                         lead = 0:4,
                         use.diagonal.variance.matrix = TRUE,
                         restrict.control.period = 1)

nn_5_1_plot <- balance_scatter(nn_match_5_1,
                               data = merged,
                               covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                               main = "One Year Lag",
                               x.axis.label = "",
                               y.axis.label = "Mahalanobis Matching - Up to 5 Matches")

nn_5_1_plot <- recordPlot()

# NN Matching - 5 Matches - 2 Lags
nn_match_5_2 <- PanelMatch(lag = 2,
                           time.id = "year",
                           unit.id = "ccode",
                           treatment = "pko",
                           refinement.method = "mahalanobis",
                           size.match = 5,
                           data = merged,
                           covs.formula = ~ 
                             I(lag(lpop, 1:2)) +
                             I(lag(lmilper, 1:2)) +
                             I(lag(ldeaths, 1:2)) +
                             I(lag(wardur, 1:2)),
                           qoi = "att",
                           outcome.var = "lgdppc",
                           lead = 0:4,
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 2)

nn_5_2_plot <- balance_scatter(nn_match_5_2,
                               data = merged,
                               covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                               main = "Two Year Lag",
                               x.axis.label = "",
                               y.axis.label = "")

nn_5_2_plot <- recordPlot()

# NN Matching - 5 Matches - 3 Lags
nn_match_5_3 <- PanelMatch(lag = 3,
                           time.id = "year",
                           unit.id = "ccode",
                           treatment = "pko",
                           refinement.method = "mahalanobis",
                           size.match = 5,
                           data = merged,
                           covs.formula = ~ 
                             I(lag(lpop, 1:3)) +
                             I(lag(lmilper, 1:3)) +
                             I(lag(ldeaths, 1:3)) +
                             I(lag(wardur, 1:3)),
                           qoi = "att",
                           outcome.var = "lgdppc",
                           lead = 0:4,
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 3)

nn_5_3_plot <- balance_scatter(nn_match_5_3,
                               data = merged,
                               covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                               main = "Three Year Lag",
                               x.axis.label = "",
                               y.axis.label = "")

nn_5_3_plot <- recordPlot()

# NN Matching - 5 Matches - 4 Lags
nn_match_5_4 <- PanelMatch(lag = 4,
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

nn_5_4_plot <- balance_scatter(nn_match_5_4,
                               data = merged,
                               covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                               main = "Four Year Lag",
                               x.axis.label = "",
                               y.axis.label = "")

nn_5_4_plot <- recordPlot()

# NN Matching - 10 Matches - 1 Lag
nn_match_10_1 <- PanelMatch(lag = 1,
                          time.id = "year",
                          unit.id = "ccode",
                          treatment = "pko",
                          refinement.method = "mahalanobis",
                          size.match = 10,
                          data = merged,
                          covs.formula = ~ 
                            I(lag(lpop, 1)) +
                            I(lag(lmilper, 1)) +
                            I(lag(ldeaths, 1)) +
                            I(lag(wardur, 1)),
                          qoi = "att",
                          outcome.var = "lgdppc",
                          lead = 0:4,
                          use.diagonal.variance.matrix = TRUE,
                          restrict.control.period = 1)

nn_10_1_plot <- balance_scatter(nn_match_10_1,
                               data = merged,
                               covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                               main = "",
                               x.axis.label = "",
                               y.axis.label = "Mahalanobis Matching - Up to 10 Matches")

nn_10_1_plot <- recordPlot()

# NN Matching - 10 Matches - 2 Lags
nn_match_10_2 <- PanelMatch(lag = 2,
                            time.id = "year",
                            unit.id = "ccode",
                            treatment = "pko",
                            refinement.method = "mahalanobis",
                            size.match = 10,
                            data = merged,
                            covs.formula = ~ 
                              I(lag(lpop, 1:2)) +
                              I(lag(lmilper, 1:2)) +
                              I(lag(ldeaths, 1:2)) +
                              I(lag(wardur, 1:2)),
                            qoi = "att",
                            outcome.var = "lgdppc",
                            lead = 0:4,
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 2)

nn_10_2_plot <- balance_scatter(nn_match_10_2,
                                data = merged,
                                covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                                main = "",
                                x.axis.label = "",
                                y.axis.label = "")

nn_10_2_plot <- recordPlot()

# NN Matching - 10 Matches - 3 Lags
nn_match_10_3 <- PanelMatch(lag = 3,
                            time.id = "year",
                            unit.id = "ccode",
                            treatment = "pko",
                            refinement.method = "mahalanobis",
                            size.match = 10,
                            data = merged,
                            covs.formula = ~ 
                              I(lag(lpop, 1:3)) +
                              I(lag(lmilper, 1:3)) +
                              I(lag(ldeaths, 1:3)) +
                              I(lag(wardur, 1:3)),
                            qoi = "att",
                            outcome.var = "lgdppc",
                            lead = 0:4,
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 3)

nn_10_3_plot <- balance_scatter(nn_match_10_3,
                                data = merged,
                                covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                                main = "",
                                x.axis.label = "",
                                y.axis.label = "")

nn_10_3_plot <- recordPlot()

# NN Matching - 10 Matches - 4 Lags
nn_match_10_4 <- PanelMatch(lag = 4,
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

nn_10_4_plot <- balance_scatter(nn_match_10_4,
                                data = merged,
                                covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                                main = "",
                                x.axis.label = "",
                                y.axis.label = "")

nn_10_4_plot <- recordPlot()

# IPW - 1 Lag
ipw_1 <- PanelMatch(lag = 1,
                  time.id = "year",
                  unit.id = "ccode",
                  treatment = "pko",
                  refinement.method = "ps.weight",
                  data = merged,
                  covs.formula = ~
                    I(lag(lpop, 1)) +
                    I(lag(lmilper, 1)) +
                    I(lag(ldeaths, 1)) +
                    I(lag(wardur, 1)),
                  qoi = "att",
                  outcome.var = "lgdppc",
                  lead = 0:4,
                  restrict.control.period = 1)

ipw_1_plot <- balance_scatter(ipw_1,
                                data = merged,
                                covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                                main = "",
                                x.axis.label = "",
                                y.axis.label = "Inverse Probability Weighting")

ipw_1_plot <- recordPlot()

# IPW - 2 Lags
ipw_2 <- PanelMatch(lag = 2,
                  time.id = "year",
                  unit.id = "ccode",
                  treatment = "pko",
                  refinement.method = "ps.weight",
                  data = merged,
                  covs.formula = ~
                    I(lag(lpop, 1:2)) +
                    I(lag(lmilper, 1:2)) +
                    I(lag(ldeaths, 1:2)) +
                    I(lag(wardur, 1:2)),
                  qoi = "att",
                  outcome.var = "lgdppc",
                  lead = 0:4,
                  restrict.control.period = 2)

ipw_2_plot <- balance_scatter(ipw_2,
                              data = merged,
                              covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                              main = "",
                              x.axis.label = "",
                              y.axis.label = "")

ipw_2_plot <- recordPlot()

# IPW - 3 Lags
ipw_3 <- PanelMatch(lag = 3,
                  time.id = "year",
                  unit.id = "ccode",
                  treatment = "pko",
                  refinement.method = "ps.weight",
                  data = merged,
                  covs.formula = ~
                    I(lag(lpop, 1:3)) +
                    I(lag(lmilper, 1:3)) +
                    I(lag(ldeaths, 1:3)) +
                    I(lag(wardur, 1:3)),
                  qoi = "att",
                  outcome.var = "lgdppc",
                  lead = 0:4,
                  restrict.control.period = 3)

ipw_3_plot <- balance_scatter(ipw_3,
                              data = merged,
                              covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                              main = "",
                              x.axis.label = "",
                              y.axis.label = "")

ipw_3_plot <- recordPlot()

# IPW - 4 Lags
ipw_4 <- PanelMatch(lag = 4,
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

ipw_4_plot <- balance_scatter(ipw_4,
                              data = merged,
                              covariates = c("lpop", "lmilper", "ldeaths", "wardur"),
                              main = "",
                              x.axis.label = "",
                              y.axis.label = "")

ipw_4_plot <- recordPlot()

# Create the Composite Covariate Balance Plot

ggsave(
  "covbal_plot.png",
  width = 6,
  height = 8,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/UN PKOs Project/Graphics"
)

