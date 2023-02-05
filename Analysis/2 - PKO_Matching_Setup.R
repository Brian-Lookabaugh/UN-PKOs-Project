#################################################################################
###############--------------PKO Matching/IPW Setup---------------###############
#################################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "PanelMatch", # Matching/Weighting Set-Up With Panel Data
  install = FALSE
)

################################################################################
####################### Create a Treatment-Variation Plot ######################
################################################################################
merged <- merged %>%
  mutate(year = as.integer(year))

merged <- as.data.frame(merged)

merged <- merged %>% # Drop Observations Pre-1994 and Post-2007 for Plot
  filter(year >= 1994, year < 2008) 

tv_plot <- DisplayTreatment(
  unit.id = "stateabb",
  time.id = "year",
  xlab = "Year",
  ylab = "Countries",
  y.size = 7,
  title = "",
  legend.position = "bottom",
  legend.labels = c("No PKO", "PKO"),
  hide.x.tick.label = TRUE,
  treatment = "pko",
  data = merged
) + 
  theme(axis.text.x = element_text(angle = 0, size = 6.5, vjust = 0.5)) +
  scale_x_discrete(breaks = c(1995, 2000, 2005))

ggsave(
  "tv_plot.png",
  width = 6,
  height = 8,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/UN PKOs Project/Graphics"
)

################################################################################
########## Create a Covariate Balance Scatter Plot (PKO Deployment) ############
################################################################################
merged <- merged %>%
  mutate(ccode = as.integer(ccode)) %>%
  select(-c(stateabb)) # PanelMatch Will Not Run With Non-Numeric/Integer Data

# NN Matching - 5 Matches - 1 Lag
nn_match_5_1 <- PanelMatch(lag = 1,
                         time.id = "year",
                         unit.id = "ccode",
                         treatment = "pko_onset",
                         refinement.method = "mahalanobis",
                         size.match = 5,
                         data = merged,
                         covs.formula = ~ 
                           I(lag(lpop, 1)) +
                           I(lag(lmilper, 1)) +
                           I(lag(ldeaths, 1)) +
                           I(lag(wardur, 1)) +
                           I(lag(democracy, 1)),
                         qoi = "att",
                         outcome.var = "lgdppc",
                         use.diagonal.variance.matrix = TRUE,
                         restrict.control.period = 1)

# NN Matching - 5 Matches - 2 Lags
nn_match_5_2 <- PanelMatch(lag = 2,
                           time.id = "year",
                           unit.id = "ccode",
                           treatment = "pko_onset",
                           refinement.method = "mahalanobis",
                           size.match = 5,
                           data = merged,
                           covs.formula = ~ 
                             I(lag(lpop, 1:2)) +
                             I(lag(lmilper, 1:2)) +
                             I(lag(ldeaths, 1:2)) +
                             I(lag(wardur, 1:2)) +
                             I(lag(democracy, 1:2)),
                           qoi = "att",
                           outcome.var = "lgdppc",
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 2)

# NN Matching - 5 Matches - 3 Lags
nn_match_5_3 <- PanelMatch(lag = 3,
                           time.id = "year",
                           unit.id = "ccode",
                           treatment = "pko_onset",
                           refinement.method = "mahalanobis",
                           size.match = 5,
                           data = merged,
                           covs.formula = ~ 
                             I(lag(lpop, 1:3)) +
                             I(lag(lmilper, 1:3)) +
                             I(lag(ldeaths, 1:3)) +
                             I(lag(wardur, 1:3)) +
                             I(lag(democracy, 1:3)),
                           qoi = "att",
                           outcome.var = "lgdppc",
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 3)

# NN Matching - 5 Matches - 4 Lags
nn_match_5_4 <- PanelMatch(lag = 4,
                           time.id = "year",
                           unit.id = "ccode",
                           treatment = "pko_onset",
                           refinement.method = "mahalanobis",
                           size.match = 5,
                           data = merged,
                           covs.formula = ~ 
                             I(lag(lpop, 1:4)) +
                             I(lag(lmilper, 1:4)) +
                             I(lag(ldeaths, 1:4)) +
                             I(lag(wardur, 1:4)) +
                             I(lag(democracy, 1:4)),
                           qoi = "att",
                           outcome.var = "lgdppc",
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 4)

# NN Matching - 10 Matches - 1 Lag
nn_match_10_1 <- PanelMatch(lag = 1,
                          time.id = "year",
                          unit.id = "ccode",
                          treatment = "pko_onset",
                          refinement.method = "mahalanobis",
                          size.match = 10,
                          data = merged,
                          covs.formula = ~ 
                            I(lag(lpop, 1)) +
                            I(lag(lmilper, 1)) +
                            I(lag(ldeaths, 1)) +
                            I(lag(wardur, 1)) +
                            I(lag(democracy, 1)),
                          qoi = "att",
                          outcome.var = "lgdppc",
                          use.diagonal.variance.matrix = TRUE,
                          restrict.control.period = 1)

# NN Matching - 10 Matches - 2 Lags
nn_match_10_2 <- PanelMatch(lag = 2,
                            time.id = "year",
                            unit.id = "ccode",
                            treatment = "pko_onset",
                            refinement.method = "mahalanobis",
                            size.match = 10,
                            data = merged,
                            covs.formula = ~ 
                              I(lag(lpop, 1:2)) +
                              I(lag(lmilper, 1:2)) +
                              I(lag(ldeaths, 1:2)) +
                              I(lag(wardur, 1:2)) +
                              I(lag(democracy, 1:2)),
                            qoi = "att",
                            outcome.var = "lgdppc",
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 2)

# NN Matching - 10 Matches - 3 Lags
nn_match_10_3 <- PanelMatch(lag = 3,
                            time.id = "year",
                            unit.id = "ccode",
                            treatment = "pko_onset",
                            refinement.method = "mahalanobis",
                            size.match = 10,
                            data = merged,
                            covs.formula = ~ 
                              I(lag(lpop, 1:3)) +
                              I(lag(lmilper, 1:3)) +
                              I(lag(ldeaths, 1:3)) +
                              I(lag(wardur, 1:3)) +
                              I(lag(democracy, 1:3)),
                            qoi = "att",
                            outcome.var = "lgdppc",
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 3)

# NN Matching - 10 Matches - 4 Lags
nn_match_10_4 <- PanelMatch(lag = 4,
                            time.id = "year",
                            unit.id = "ccode",
                            treatment = "pko_onset",
                            refinement.method = "mahalanobis",
                            size.match = 10,
                            data = merged,
                            covs.formula = ~ 
                              I(lag(lpop, 1:4)) +
                              I(lag(lmilper, 1:4)) +
                              I(lag(ldeaths, 1:4)) +
                              I(lag(wardur, 1:4)) +
                              I(lag(democracy, 1:4)),
                            qoi = "att",
                            outcome.var = "lgdppc",
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 4)

# IPW - 1 Lag
ipw_1 <- PanelMatch(lag = 1,
                  time.id = "year",
                  unit.id = "ccode",
                  treatment = "pko_onset",
                  refinement.method = "ps.weight",
                  data = merged,
                  covs.formula = ~
                    I(lag(lpop, 1)) +
                    I(lag(lmilper, 1)) +
                    I(lag(ldeaths, 1)) +
                    I(lag(wardur, 1)) +
                    I(lag(democracy, 1)),
                  qoi = "att",
                  outcome.var = "lgdppc",
                  restrict.control.period = 1)

# IPW - 2 Lags
ipw_2 <- PanelMatch(lag = 2,
                  time.id = "year",
                  unit.id = "ccode",
                  treatment = "pko_onset",
                  refinement.method = "ps.weight",
                  data = merged,
                  covs.formula = ~
                    I(lag(lpop, 1:2)) +
                    I(lag(lmilper, 1:2)) +
                    I(lag(ldeaths, 1:2)) +
                    I(lag(wardur, 1:2)) +
                    I(lag(democracy, 1:2)),
                  qoi = "att",
                  outcome.var = "lgdppc",
                  restrict.control.period = 2)

# IPW - 3 Lags
ipw_3 <- PanelMatch(lag = 3,
                  time.id = "year",
                  unit.id = "ccode",
                  treatment = "pko_onset",
                  refinement.method = "ps.weight",
                  data = merged,
                  covs.formula = ~
                    I(lag(lpop, 1:3)) +
                    I(lag(lmilper, 1:3)) +
                    I(lag(ldeaths, 1:3)) +
                    I(lag(wardur, 1:3)) +
                    I(lag(democracy, 1:3)),
                  qoi = "att",
                  outcome.var = "lgdppc",
                  restrict.control.period = 3)

# IPW - 4 Lags
ipw_4 <- PanelMatch(lag = 4,
                  time.id = "year",
                  unit.id = "ccode",
                  treatment = "pko_onset",
                  refinement.method = "ps.weight",
                  data = merged,
                  covs.formula = ~
                    I(lag(lpop, 1:4)) +
                    I(lag(lmilper, 1:4)) +
                    I(lag(ldeaths, 1:4)) +
                    I(lag(wardur, 1:4)) +
                    I(lag(democracy, 1:4)),
                  qoi = "att",
                  outcome.var = "lgdppc",
                  restrict.control.period = 4)

# Create the Composite Covariate Balance Plot
plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(3,4),
    pty = "s")

balance_scatter(
  nn_match_5_1,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  nn_match_5_2,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn_match_5_3,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn_match_5_4,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn_match_10_1,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  nn_match_10_2,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn_match_10_3,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn_match_10_4,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  ipw_1,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = ""
)

balance_scatter(
  ipw_2,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

balance_scatter(
  ipw_3,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

balance_scatter(
  ipw_4,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

mtext(1,text = "Standardized Mean Difference \n Before Refinement",
      line = 3.5,
      at = 0.52, outer = TRUE, cex = 1)
mtext(2, text = "Standardized Mean Difference \n After Refinement",
      line = 4, outer = TRUE)
mtext(2, text = "NN Matching \n Up to 5",
      line = 1.15, at = .82, outer = TRUE,
      cex = .8)
mtext(2, text = "NN Matching \n Up to 10",
      line = 1.15, at = .5, outer = TRUE,
      cex = .8)
mtext(2, text = "IPW",
      line = 1.15, at = .16, outer = TRUE,
      cex = .8)
mtext("One Year Lag",
      line = 0, at = 0.125, outer = TRUE, cex = .8)
mtext("Two Year Lag",
      line = 0, at = 0.375, outer = TRUE, cex = .8)
mtext("Three Year Lag",
      line = 0, at = 0.625, outer = TRUE, cex = .8)
mtext("Four Year Lag",
      line = 0, at = .875, outer = TRUE, cex = .8)

dev.off()

# Remove Objects
rm(ipw_1, ipw_1_plot, ipw_2, ipw_2_plot, ipw_3, ipw_3_plot, ipw_4, ipw_4_plot,
   nn_match_5_1, nn_match_5_2, nn_match_5_3, nn_match_5_4,
   nn_5_1_plot, nn_5_2_plot, nn_5_3_plot, nn_5_4_plot,
   nn_match_10_1, nn_match_10_2, nn_match_10_3, nn_match_10_4,
   nn_10_1_plot, nn_10_2_plot, nn_10_3_plot, nn_10_4_plot, cb_plot_onset)

################################################################################
########## Create a Covariate Balance Scatter Plot (PKO Termination) ###########
################################################################################
nn_match_5_1 <- PanelMatch(lag = 1,
                           time.id = "year",
                           unit.id = "ccode",
                           treatment = "pko_term",
                           refinement.method = "mahalanobis",
                           size.match = 5,
                           data = merged,
                           covs.formula = ~ 
                             I(lag(lpop, 1)) +
                             I(lag(lmilper, 1)) +
                             I(lag(ldeaths, 1)) +
                             I(lag(wardur, 1)) +
                             I(lag(democracy, 1)),
                           qoi = "att",
                           outcome.var = "lgdppc",
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 1)

nn_match_5_2 <- PanelMatch(lag = 2,
                           time.id = "year",
                           unit.id = "ccode",
                           treatment = "pko_term",
                           refinement.method = "mahalanobis",
                           size.match = 5,
                           data = merged,
                           covs.formula = ~ 
                             I(lag(lpop, 1:2)) +
                             I(lag(lmilper, 1:2)) +
                             I(lag(ldeaths, 1:2)) +
                             I(lag(wardur, 1:2)) +
                             I(lag(democracy, 1:2)),
                           qoi = "att",
                           outcome.var = "lgdppc",
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 2)

nn_match_5_3 <- PanelMatch(lag = 3,
                           time.id = "year",
                           unit.id = "ccode",
                           treatment = "pko_term",
                           refinement.method = "mahalanobis",
                           size.match = 5,
                           data = merged,
                           covs.formula = ~ 
                             I(lag(lpop, 1:3)) +
                             I(lag(lmilper, 1:3)) +
                             I(lag(ldeaths, 1:3)) +
                             I(lag(wardur, 1:3)) +
                             I(lag(democracy, 1:3)),
                           qoi = "att",
                           outcome.var = "lgdppc",
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 3)

nn_match_5_4 <- PanelMatch(lag = 4,
                           time.id = "year",
                           unit.id = "ccode",
                           treatment = "pko_term",
                           refinement.method = "mahalanobis",
                           size.match = 5,
                           data = merged,
                           covs.formula = ~ 
                             I(lag(lpop, 1:4)) +
                             I(lag(lmilper, 1:4)) +
                             I(lag(ldeaths, 1:4)) +
                             I(lag(wardur, 1:4)) +
                             I(lag(democracy, 1:4)),
                           qoi = "att",
                           outcome.var = "lgdppc",
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 4)

nn_match_10_1 <- PanelMatch(lag = 1,
                            time.id = "year",
                            unit.id = "ccode",
                            treatment = "pko_term",
                            refinement.method = "mahalanobis",
                            size.match = 10,
                            data = merged,
                            covs.formula = ~ 
                              I(lag(lpop, 1)) +
                              I(lag(lmilper, 1)) +
                              I(lag(ldeaths, 1)) +
                              I(lag(wardur, 1)) +
                              I(lag(democracy, 1)),
                            qoi = "att",
                            outcome.var = "lgdppc",
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 1)

nn_match_10_2 <- PanelMatch(lag = 2,
                            time.id = "year",
                            unit.id = "ccode",
                            treatment = "pko_term",
                            refinement.method = "mahalanobis",
                            size.match = 10,
                            data = merged,
                            covs.formula = ~ 
                              I(lag(lpop, 1:2)) +
                              I(lag(lmilper, 1:2)) +
                              I(lag(ldeaths, 1:2)) +
                              I(lag(wardur, 1:2)) +
                              I(lag(democracy, 1:2)),
                            qoi = "att",
                            outcome.var = "lgdppc",
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 2)

nn_match_10_3 <- PanelMatch(lag = 3,
                            time.id = "year",
                            unit.id = "ccode",
                            treatment = "pko_term",
                            refinement.method = "mahalanobis",
                            size.match = 10,
                            data = merged,
                            covs.formula = ~ 
                              I(lag(lpop, 1:3)) +
                              I(lag(lmilper, 1:3)) +
                              I(lag(ldeaths, 1:3)) +
                              I(lag(wardur, 1:3)) +
                              I(lag(democracy, 1:3)),
                            qoi = "att",
                            outcome.var = "lgdppc",
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 3)

nn_match_10_4 <- PanelMatch(lag = 4,
                            time.id = "year",
                            unit.id = "ccode",
                            treatment = "pko_term",
                            refinement.method = "mahalanobis",
                            size.match = 10,
                            data = merged,
                            covs.formula = ~ 
                              I(lag(lpop, 1:4)) +
                              I(lag(lmilper, 1:4)) +
                              I(lag(ldeaths, 1:4)) +
                              I(lag(wardur, 1:4)) +
                              I(lag(democracy, 1:4)),
                            qoi = "att",
                            outcome.var = "lgdppc",
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 4)

ipw_1 <- PanelMatch(lag = 1,
                    time.id = "year",
                    unit.id = "ccode",
                    treatment = "pko_term",
                    refinement.method = "ps.weight",
                    data = merged,
                    covs.formula = ~
                      I(lag(lpop, 1)) +
                      I(lag(lmilper, 1)) +
                      I(lag(ldeaths, 1)) +
                      I(lag(wardur, 1)) +
                      I(lag(democracy, 1)),
                    qoi = "att",
                    outcome.var = "lgdppc",
                    restrict.control.period = 1)

ipw_2 <- PanelMatch(lag = 2,
                    time.id = "year",
                    unit.id = "ccode",
                    treatment = "pko_term",
                    refinement.method = "ps.weight",
                    data = merged,
                    covs.formula = ~
                      I(lag(lpop, 1:2)) +
                      I(lag(lmilper, 1:2)) +
                      I(lag(ldeaths, 1:2)) +
                      I(lag(wardur, 1:2)) +
                      I(lag(democracy, 1:2)),
                    qoi = "att",
                    outcome.var = "lgdppc",
                    restrict.control.period = 2)

ipw_3 <- PanelMatch(lag = 3,
                    time.id = "year",
                    unit.id = "ccode",
                    treatment = "pko_term",
                    refinement.method = "ps.weight",
                    data = merged,
                    covs.formula = ~
                      I(lag(lpop, 1:3)) +
                      I(lag(lmilper, 1:3)) +
                      I(lag(ldeaths, 1:3)) +
                      I(lag(wardur, 1:3)) +
                      I(lag(democracy, 1:3)),
                    qoi = "att",
                    outcome.var = "lgdppc",
                    restrict.control.period = 3)

ipw_4 <- PanelMatch(lag = 4,
                    time.id = "year",
                    unit.id = "ccode",
                    treatment = "pko_term",
                    refinement.method = "ps.weight",
                    data = merged,
                    covs.formula = ~
                      I(lag(lpop, 1:4)) +
                      I(lag(lmilper, 1:4)) +
                      I(lag(ldeaths, 1:4)) +
                      I(lag(wardur, 1:4)) +
                      I(lag(democracy, 1:4)),
                    qoi = "att",
                    outcome.var = "lgdppc",
                    restrict.control.period = 4)

plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(3,4),
    pty = "s")

balance_scatter(
  nn_match_5_1,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  nn_match_5_2,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn_match_5_3,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn_match_5_4,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn_match_10_1,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  nn_match_10_2,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn_match_10_3,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn_match_10_4,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  ipw_1,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = ""
)

balance_scatter(
  ipw_2,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

balance_scatter(
  ipw_3,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

balance_scatter(
  ipw_4,
  data = merged,
  covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                 "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

mtext(1,text = "Standardized Mean Difference \n Before Refinement",
      line = 3.5,
      at = 0.52, outer = TRUE, cex = 1)
mtext(2, text = "Standardized Mean Difference \n After Refinement",
      line = 4, outer = TRUE)
mtext(2, text = "NN Matching \n Up to 5",
      line = 1.15, at = .82, outer = TRUE,
      cex = .8)
mtext(2, text = "NN Matching \n Up to 10",
      line = 1.15, at = .5, outer = TRUE,
      cex = .8)
mtext(2, text = "IPW",
      line = 1.15, at = .16, outer = TRUE,
      cex = .8)
mtext("One Year Lag",
      line = 0, at = 0.125, outer = TRUE, cex = .8)
mtext("Two Year Lag",
      line = 0, at = 0.375, outer = TRUE, cex = .8)
mtext("Three Year Lag",
      line = 0, at = 0.625, outer = TRUE, cex = .8)
mtext("Four Year Lag",
      line = 0, at = .875, outer = TRUE, cex = .8)

dev.off()

rm(ipw_1, ipw_1_plot, ipw_2, ipw_2_plot, ipw_3, ipw_3_plot, ipw_4, ipw_4_plot,
   nn_match_5_1, nn_match_5_2, nn_match_5_3, nn_match_5_4,
   nn_5_1_plot, nn_5_2_plot, nn_5_3_plot, nn_5_4_plot,
   nn_match_10_1, nn_match_10_2, nn_match_10_3, nn_match_10_4,
   nn_10_1_plot, nn_10_2_plot, nn_10_3_plot, nn_10_4_plot, cb_plot_term)


#################################################################
################# Create a Covariate Trend Plot #################
#################################################################
# Re-Define Matched Objects
nn_match_5_onset <- PanelMatch(lag = 3,
                               time.id = "year",
                               unit.id = "ccode",
                               treatment = "pko_onset",
                               refinement.method = "mahalanobis",
                               size.match = 5,
                               data = merged,
                               covs.formula = ~ 
                                 I(lag(lpop, 1:3)) +
                                 I(lag(lmilper, 1:3)) +
                                 I(lag(ldeaths, 1:3)) +
                                 I(lag(wardur, 1:3)) +
                                 I(lag(democracy, 1:3)),
                               qoi = "att",
                               outcome.var = "lgdppc",
                               lead = 0:8,
                               use.diagonal.variance.matrix = TRUE,
                               restrict.control.period = 3)

nn_match_5_term <- PanelMatch(lag = 3,
                              time.id = "year",
                              unit.id = "ccode",
                              treatment = "pko_term",
                              refinement.method = "mahalanobis",
                              size.match = 5,
                              data = merged,
                              covs.formula = ~ 
                                I(lag(lpop, 1:3)) +
                                I(lag(lmilper, 1:3)) +
                                I(lag(ldeaths, 1:3)) +
                                I(lag(wardur, 1:3)) +
                                I(lag(democracy, 1:3)),
                              qoi = "att",
                              outcome.var = "lgdppc",
                              lead = 0:8,
                              use.diagonal.variance.matrix = TRUE,
                              restrict.control.period = 3)
  
nn_match_10_onset <- PanelMatch(lag = 3,
                                time.id = "year",
                                unit.id = "ccode",
                                treatment = "pko_onset",
                                refinement.method = "mahalanobis",
                                size.match = 10,
                                data = merged,
                                covs.formula = ~ 
                                  I(lag(lpop, 1:3)) +
                                  I(lag(lmilper, 1:3)) +
                                  I(lag(ldeaths, 1:3)) +
                                  I(lag(wardur, 1:3)) +
                                  I(lag(democracy, 1:3)),
                                qoi = "att",
                                outcome.var = "lgdppc",
                                lead = 0:8,
                                use.diagonal.variance.matrix = TRUE,
                                restrict.control.period = 3)
  
nn_match_10_term <- PanelMatch(lag = 3,
                               time.id = "year",
                               unit.id = "ccode",
                               treatment = "pko_term",
                               refinement.method = "mahalanobis",
                               size.match = 10,
                               data = merged,
                               covs.formula = ~ 
                                 I(lag(lpop, 1:3)) +
                                 I(lag(lmilper, 1:3)) +
                                 I(lag(ldeaths, 1:3)) +
                                 I(lag(wardur, 1:3)) +
                                 I(lag(democracy, 1:3)),
                               qoi = "att",
                               outcome.var = "lgdppc",
                               lead = 0:8,
                               use.diagonal.variance.matrix = TRUE,
                               restrict.control.period = 3)
  
ipw_onset <- PanelMatch(lag = 3,
                        time.id = "year",
                        unit.id = "ccode",
                        treatment = "pko_onset",
                        refinement.method = "ps.weight",
                        data = merged,
                        covs.formula = ~
                          I(lag(lpop, 1:3)) +
                          I(lag(lmilper, 1:3)) +
                          I(lag(ldeaths, 1:3)) +
                          I(lag(wardur, 1:3)) +
                          I(lag(democracy, 1:3)),
                        qoi = "att",
                        outcome.var = "lgdppc",
                        lead = 0:8,
                        restrict.control.period = 3)
  
ipw_term <- PanelMatch(lag = 3,
                       time.id = "year",
                       unit.id = "ccode",
                       treatment = "pko_term",
                       refinement.method = "ps.weight",
                       data = merged,
                       covs.formula = ~
                         I(lag(lpop, 1:3)) +
                         I(lag(lmilper, 1:3)) +
                         I(lag(ldeaths, 1:3)) +
                         I(lag(wardur, 1:3)) +
                         I(lag(democracy, 1:3)),
                       qoi = "att",
                       outcome.var = "lgdppc",
                       lead = 0:8,
                       restrict.control.period = 3)

# Begin Creating the Graphic
plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(2,3),
    pty = "s")

get_covariate_balance(nn_match_5_onset$att,
                      data = merged,
                      covariates = c("lgdppc"),
                      plot = TRUE,
                      ylim = c(-2, 2),
                      ylab = "",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(nn_match_10_onset$att,
                      data = merged,
                      covariates = c("lgdppc"),
                      plot = TRUE,
                      ylim = c(-2, 2),
                      ylab = "",
                      legend = FALSE,
                      yaxt = "n")
abline(v = 3, lty = "dotted")

get_covariate_balance(ipw_onset$att,
                      data = merged,
                      covariates = c("lgdppc"),
                      plot = TRUE,
                      ylim = c(-2, 2),
                      ylab = "",
                      legend = FALSE,
                      yaxt = "n")
abline(v = 3, lty = "dotted")

get_covariate_balance(nn_match_5_term$att,
                      data = merged,
                      covariates = c("lgdppc"),
                      plot = TRUE,
                      ylim = c(-2, 2),
                      ylab = "",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(nn_match_10_term$att,
                      data = merged,
                      covariates = c("lgdppc"),
                      plot = TRUE,
                      ylim = c(-2, 2),
                      ylab = "",
                      legend = FALSE,
                      yaxt = "n")
abline(v = 3, lty = "dotted")

get_covariate_balance(ipw_term$att,
                      data = merged,
                      covariates = c("lgdppc"),
                      plot = TRUE,
                      ylab = "",
                      ylim = c(-2, 2),
                      yaxt = "n",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

mtext(1,text = "Years Before Treatment",
      line = 3.5,
      at = 0.5, outer = TRUE, cex = 1)
mtext(2, text = "Standardized Mean Difference",
      line = 4, outer = TRUE)
mtext(2, text = "PKO Deployment",
      line = 1.5, at = .73, outer = TRUE,
      cex = .8)
mtext(2, text = "PKO Withdrawal",
      line = 1.5, at = .23, outer = TRUE,
      cex = .8)
mtext("NN Matching - Up to 5 \n",
      line = -2, at = 0.17, outer = TRUE, cex = .8)
mtext("NN Matching - Up to 10 \n",
      line = -2, at = 0.5, outer = TRUE, cex = .8)
mtext("IPW \n ",
      line = -2, at = 0.83, outer = TRUE, cex = .8)

dev.off()
