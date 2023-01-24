############################################################################
###############-------------PKO Withdrawal Setup-------------###############
############################################################################

# Covariate Balance Plot Post-Refinement
# NN Matching - 5 Matches - 1 Lag
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
                           lead = 0:4,
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 1)

# NN Matching - 5 Matches - 2 Lags
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
                           lead = 0:4,
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 2)

# NN Matching - 5 Matches - 3 Lags
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
                           lead = 0:4,
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 3)

# NN Matching - 5 Matches - 4 Lags
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
                           lead = 0:4,
                           use.diagonal.variance.matrix = TRUE,
                           restrict.control.period = 4)

# NN Matching - 10 Matches - 1 Lag
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
                            lead = 0:4,
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 1)

# NN Matching - 10 Matches - 2 Lags
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
                            lead = 0:4,
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 2)

# NN Matching - 10 Matches - 3 Lags
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
                            lead = 0:4,
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 3)

# NN Matching - 10 Matches - 4 Lags
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
                            lead = 0:4,
                            use.diagonal.variance.matrix = TRUE,
                            restrict.control.period = 4)

# IPW - 1 Lag
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
                    lead = 0:4,
                    restrict.control.period = 1)

# IPW - 2 Lags
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
                    lead = 0:4,
                    restrict.control.period = 2)

# IPW - 3 Lags
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
                    lead = 0:4,
                    restrict.control.period = 3)

# IPW - 4 Lags
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
                    lead = 0:4,
                    restrict.control.period = 4)

# Create the Composite Covariate Balance Plot
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(3,4),
    pty = "s")
plot.new()

nn_5_1_plot <- balance_scatter(nn_match_5_1,
                               data = merged,
                               covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                              "democracy"),
                               main = "",
                               x.axis.label = "",
                               y.axis.label = "")

nn_5_2_plot <- balance_scatter(nn_match_5_2,
                               data = merged,
                               covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                              "democracy"),
                               main = "",
                               x.axis.label = "",
                               y.axis.label = "")

nn_5_3_plot <- balance_scatter(nn_match_5_3,
                               data = merged,
                               covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                              "democracy"),
                               main = "",
                               x.axis.label = "",
                               y.axis.label = "")

nn_5_4_plot <- balance_scatter(nn_match_5_4,
                               data = merged,
                               covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                              "democracy"),
                               main = "",
                               x.axis.label = "",
                               y.axis.label = "")

nn_10_1_plot <- balance_scatter(nn_match_10_1,
                                data = merged,
                                covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                               "democracy"),
                                main = "",
                                x.axis.label = "",
                                y.axis.label = "")

nn_10_2_plot <- balance_scatter(nn_match_10_2,
                                data = merged,
                                covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                               "democracy"),
                                main = "",
                                x.axis.label = "",
                                y.axis.label = "")

nn_10_3_plot <- balance_scatter(nn_match_10_3,
                                data = merged,
                                covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                               "democracy"),
                                main = "",
                                x.axis.label = "",
                                y.axis.label = "")

nn_10_4_plot <- balance_scatter(nn_match_10_4,
                                data = merged,
                                covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                               "democracy"),
                                main = "",
                                x.axis.label = "",
                                y.axis.label = "")

ipw_1_plot <- balance_scatter(ipw_1,
                              data = merged,
                              covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                             "democracy"),
                              main = "",
                              x.axis.label = "",
                              y.axis.label = "")

ipw_2_plot <- balance_scatter(ipw_2,
                              data = merged,
                              covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                             "democracy"),
                              main = "",
                              x.axis.label = "",
                              y.axis.label = "")

ipw_3_plot <- balance_scatter(ipw_3,
                              data = merged,
                              covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                             "democracy"),
                              main = "",
                              x.axis.label = "",
                              y.axis.label = "")

ipw_4_plot <- balance_scatter(ipw_4,
                              data = merged,
                              covariates = c("lpop", "lmilper", "ldeaths", "wardur",
                                             "democracy"),
                              main = "",
                              x.axis.label = "",
                              y.axis.label = "")

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

cb_plot_term <- recordPlot()

# Remove Objects
rm(ipw_1, ipw_1_plot, ipw_2, ipw_2_plot, ipw_3, ipw_3_plot, ipw_4, ipw_4_plot,
   nn_match_5_1, nn_match_5_2, nn_match_5_3, nn_match_5_4,
   nn_5_1_plot, nn_5_2_plot, nn_5_3_plot, nn_5_4_plot,
   nn_match_10_1, nn_match_10_2, nn_match_10_3, nn_match_10_4,
   nn_10_1_plot, nn_10_2_plot, nn_10_3_plot, nn_10_4_plot, cb_plot_term)