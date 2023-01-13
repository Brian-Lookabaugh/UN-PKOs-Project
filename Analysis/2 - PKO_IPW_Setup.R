############################################################################
###############------------PKO IPW/Matching Set Up-----------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "broom", # Converting Model Output to Data Frames
  "WeightIt", # IPW
  "MatchIt", # Matching
  "cobalt", # Assessing Balance
  "ggpubr", # Combining Plots Together
  install = FALSE
)

#######-------Conflict-Level-------#######

# Remove NA Values in Covariates
con <- con %>%
  filter_at(vars(lgdppc, lpop, lnatres, lmilper, democracy, wardur, eth_war),
            all_vars(!is.na(.)))

# Inverse Probability Weighting (IPW) - (Manually Initially to Filter Propensity Scores)
con_model <- glm(pko ~ lgdppc + lpop + lnatres + lmilper + democracy + wardur + eth_war,
                        family = binomial(link = "logit"),
                        data = con)

con_ipw <- augment_columns(con_model, con,
                              type.predict = "response") %>%
  rename(propensity = .fitted) %>%
  # Filter Propensity Scores Less than 0.05 (There are None > 0.95)
  filter(propensity >= 0.05)

# Generate the Weights
con_weights <- weightit(pko ~ lgdppc + lpop + lnatres + lmilper + democracy + wardur + eth_war,
                        data = con,
                        estimand = "ATT",
                        method = "ps")

# Merge the Weights Into the Data Set
con_ipw <- con %>%
  mutate(ipw = con_weights$weights)

# Mahalanobis Nearest-Neighbor (NN) Matching
con_mmatch <- matchit(pko ~ lgdppc + lpop + lnatres + lmilper + democracy + wardur + eth_war,
                         data = con,
                         method = "nearest",
                         estimand = "ATT",
                         distance = "mahalanobis",
                         replace = TRUE)

# Coarsened Exact Matching (CEM)
con_cmatch <- matchit(pko ~ lgdppc + lpop + lnatres + lmilper + democracy + wardur + eth_war,
                         data = con,
                         method = "cem",
                         estimand = "ATT")

# Balance Tables
bal.tab(pko ~ lgdppc + lpop + lnatres + lmilper + democracy + wardur + eth_war, # Not Weighted/Matched
        data = con,
        estimand = "ATT",
        thresholds = c(m = .05))

bal.tab(con_weights, # IPW Weighted
        stats = c("m", "v"),
        thresholds = c(m = .05))

bal.tab(con_mmatch, # NN Matched
        stats = c("m", "v"),
        thresholds = c(m = .05))

bal.tab(con_cmatch, # CEM Matched
        stats = c("m", "v"),
        thresholds = c(m = .05))

# Generate New Labels for Covariates
cv_names <- data.frame(old = c("lnatres", "lgdppc", "lpop", "lmilper", "democracy",
                               "wardur", "eth_war"),
                      new = c("Natural Resources pc", 
                              "GDP pc", "Population", "Military Personnel pc",
                              "Democracy", "War Duration", "Ethnic War")
)

# Covariate Balance Love Plot
con_cb_lplot <- love.plot(pko ~ lnatres + lgdppc + lpop + lmilper + democracy + wardur + eth_war,
                      data = con, estimand = "ATT",
                      weights = list(w1 = get.w(con_weights),
                                     w2 = get.w(con_mmatch),
                                     w3 = get.w(con_cmatch)),
                      abs = TRUE,
                      stars = "raw",
                      line = TRUE,
                      thresholds = c(m = .05),
                      var.order = "unadjusted",
                      var.names = cv_names,
                      colors = c("#440154", "#2d708e", "#52c569", "#c2df23"),
                      sample.names = c("Original", "IPW", "NN Matching", "CEM")
) +
  labs(caption = "* indicates the reporting of raw difference in means") +
  theme(plot.caption.position = "plot")

ggsave(
  "con_cb_lplot.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)

# K-S Love Plot
c_ks_plot <- love.plot(pko ~ lnatres + lgdppc + lpop + lmilper + democracy + wardur + eth_war,
                     data = con, estimand = "ATT",
                     stats = "ks.statistics",
                     weights = list(w1 = get.w(con_weights),
                                    w2 = get.w(con_mmatch),
                                    w3 = get.w(con_cmatch)),
                     abs = TRUE,
                     line = TRUE,
                     thresholds = c(m = .05),
                     var.order = "unadjusted",
                     var.names = cv_names,
                     colors = c("#440154", "#2d708e", "#52c569", "#c2df23"),
                     sample.names = c("Original", "IPW", "NN Matching", "CEM"))

ggsave(
  "c_ks_plot.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)

# Density and Bar Plots (Purely for EDA - Not To Be Included in the Paper)
c_gdp_den <- bal.plot(pko ~ lgdppc, data = con,
                    weights = list(NN = con_mmatch,
                                   CEM = con_cmatch,
                                   IPW = con_weights),
                    var.name = "lgdppc", which = "both") +
  labs(title = "Distributional Balances for Covariates (Conflict Sample)", 
       x = "Log(GDP per capita)") +
  scale_fill_discrete(name = "PKO")

c_natres_den <- bal.plot(pko ~ lnatres, data = con,
                      weights = list(NN = con_mmatch,
                                     CEM = con_cmatch,
                                     IPW = con_weights),
                      var.name = "lnatres", which = "both") +
  labs(title = "", y = "", x = "Log(Natural Resources per capita)") +
  scale_fill_discrete(name = "PKO") + 
  theme(legend.position = "none")

c_milper_den <- bal.plot(pko ~ lmilper, data = con,
                      weights = list(NN = con_mmatch,
                                     CEM = con_cmatch,
                                     IPW = con_weights),
                      var.name = "lmilper", which = "both") +
  labs(title = "", y = "", x = "Log(Military Personnel per capita)") +
  scale_fill_discrete(name = "PKO") + 
  theme(legend.position = "none")

c_pop_den <- bal.plot(pko ~ lpop, data = con,
                      weights = list(NN = con_mmatch,
                                     CEM = con_cmatch,
                                     IPW = con_weights),
                      var.name = "lpop", which = "both") +
  labs(title = "", y = "", x = "Log(Population)") +
  scale_fill_discrete(name = "PKO") +
  theme(legend.position = "none")

c_dem_den <- bal.plot(pko ~ democracy, data = con,
                      weights = list(NN = con_mmatch,
                                     CEM = con_cmatch,
                                     IPW = con_weights),
                      var.name = "democracy", which = "both") +
  labs(title = "", y = "", x = "Democracy") +
  scale_fill_discrete(name = "PKO") +
  theme(legend.position = "none")

c_wd_den <- bal.plot(pko ~ wardur, data = con,
                      weights = list(NN = con_mmatch,
                                     CEM = con_cmatch,
                                     IPW = con_weights),
                      var.name = "wardur", which = "both") +
  labs(title = "", y = "", x = "War Duration") +
  scale_fill_discrete(name = "PKO") +
  theme(legend.position = "none")

c_eth_bar <- bal.plot(pko ~ eth_war, data = con,
                      weights = list(NN = con_mmatch,
                                     CEM = con_cmatch,
                                     IPW = con_weights),
                      var.name = "eth_war", which = "both") +
  labs(title = "", y = "", x = "Ethnic War") +
  scale_fill_discrete(name = "PKO") +
  theme(legend.position = "none")

#######-------Post-Conflict Level-------#######

# Remove NA Values in Covariates
pcon <- pcon %>%
  filter_at(vars(lgdppc, lpop, lmilper, lnatres, democracy, pwar_dur, peth_war,
                 pp_agg, p_int),
            all_vars(!is.na(.)))

# Inverse Probability Weighting (IPW) - (Manually Initially to Filter Propensity Scores)
pcon_model <- glm(pko ~ lgdppc + lpop + lmilper + lnatres + democracy + pwar_dur + peth_war +
                 pp_agg + p_int,
                 family = binomial(link = "logit"),
                 data = pcon)

pcon_ipw <- augment_columns(pcon_model, pcon,
                           type.predict = "response") %>%
  rename(propensity = .fitted) %>%
  # Filter Propensity Scores Less than 0.05 (There are None > 0.95)
  filter(propensity >= 0.05)

# Generate the Weights
pcon_weights <- weightit(pko ~ lgdppc + lpop + lmilper + lnatres + democracy + pwar_dur + peth_war +
                           pp_agg + p_int,
                        data = pcon,
                        estimand = "ATT",
                        method = "ps")

# Merge the Weights Into the Data Set
pcon_ipw <- pcon %>%
  mutate(ipw = pcon_weights$weights)

# Mahalanobis Nearest-Neighbor (NN) Matching
pcon_mmatch <- matchit(pko ~ lgdppc + lpop + lmilper + lnatres + democracy + pwar_dur + peth_war +
                         pp_agg + p_int,
                      data = pcon,
                      method = "nearest",
                      estimand = "ATT",
                      distance = "mahalanobis",
                      replace = TRUE)

# Coarsened Exact Matching (CEM)
pcon_cmatch <- matchit(pko ~ lgdppc + lpop + lmilper + lnatres + democracy + pwar_dur + peth_war +
                         pp_agg + p_int,
                      data = pcon,
                      method = "cem",
                      estimand = "ATT")

# Balance Tables
bal.tab(pko ~ lgdppc + lpop + lmilper + lnatres + democracy + pwar_dur + peth_war +
          pp_agg + p_int, # Not Weighted/Matched
        data = pcon,
        estimand = "ATT",
        thresholds = c(m = .05))

bal.tab(pcon_weights, # IPW Weighted
        stats = c("m", "v"),
        thresholds = c(m = .05))

bal.tab(pcon_mmatch, # NN Matched
        stats = c("m", "v"),
        thresholds = c(m = .05))

bal.tab(pcon_cmatch, # CEM Matched
        stats = c("m", "v"),
        thresholds = c(m = .05))

# Generate New Labels for Covariates
pcv_names <- data.frame(old = c("lnatres", "lgdppc", "lpop", "lmilper", "democracy",
                               "pwar_dur", "peth_war", "p_int", "pp_agg"),
                       new = c("Natural Resources pc", 
                               "GDP pc", "Population", "Military Personnel pc",
                               "Democracy", "Prior War Duration", "Prior Ethnic War",
                               "Prior Conflict Intensity", "Prior Peace Agreement")
)

# Covariate Balance Love Plot
pcon_cb_lplot <- love.plot(pko ~ lgdppc + lpop + lmilper + lnatres + democracy + pwar_dur + peth_war +
                             pp_agg + p_int,
                          data = pcon, estimand = "ATT",
                          weights = list(w1 = get.w(pcon_weights),
                                         w2 = get.w(pcon_mmatch),
                                         w3 = get.w(pcon_cmatch)),
                          abs = TRUE,
                          stars = "raw",
                          line = TRUE,
                          thresholds = c(m = .05),
                          var.order = "unadjusted",
                          var.names = pcv_names,
                          colors = c("#440154", "#2d708e", "#52c569", "#c2df23"),
                          sample.names = c("Original", "IPW", "NN Matching", "CEM")
) +
  labs(caption = "* indicates the reporting of raw difference in means") +
  theme(plot.caption.position = "plot")

ggsave(
  "pcon_cb_lplot.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)

# K-S Love Plot
pc_ks_plot <- love.plot(pko ~ lgdppc + lpop + lmilper + lnatres + democracy + pwar_dur + peth_war +
                          pp_agg + p_int,
                       data = pcon, estimand = "ATT",
                       stats = "ks.statistics",
                       weights = list(w1 = get.w(pcon_weights),
                                      w2 = get.w(pcon_mmatch),
                                      w3 = get.w(pcon_cmatch)),
                       abs = TRUE,
                       line = TRUE,
                       thresholds = c(m = .05),
                       var.order = "unadjusted",
                       var.names = pcv_names,
                       colors = c("#440154", "#2d708e", "#52c569", "#c2df23"),
                       sample.names = c("Original", "IPW", "NN Matching", "CEM"))

ggsave(
  "pc_ks_plot.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)

# Density and Bar Plots (Purely for EDA - Not To Be Included in the Paper)
pc_gdp_den <- bal.plot(pko ~ lgdppc, data = pcon,
                      weights = list(NN = pcon_mmatch,
                                     CEM = pcon_cmatch,
                                     IPW = pcon_weights),
                      var.name = "lgdppc", which = "both") +
  labs(title = "Distributional Balances for Covariates (Post-Conflict Sample)", 
       x = "Log(GDP per capita)") +
  scale_fill_discrete(name = "PKO")

pc_natres_den <- bal.plot(pko ~ lnatres, data = pcon,
                         weights = list(NN = pcon_mmatch,
                                        CEM = pcon_cmatch,
                                        IPW = pcon_weights),
                         var.name = "lnatres", which = "both") +
  labs(title = "", y = "", x = "Log(Natural Resources per capita)") +
  scale_fill_discrete(name = "PKO") + 
  theme(legend.position = "none")

pc_milper_den <- bal.plot(pko ~ lmilper, data = pcon,
                         weights = list(NN = pcon_mmatch,
                                        CEM = pcon_cmatch,
                                        IPW = pcon_weights),
                         var.name = "lmilper", which = "both") +
  labs(title = "", y = "", x = "Log(Military Personnel per capita)") +
  scale_fill_discrete(name = "PKO") + 
  theme(legend.position = "none")

pc_pop_den <- bal.plot(pko ~ lpop, data = pcon,
                      weights = list(NN = pcon_mmatch,
                                     CEM = pcon_cmatch,
                                     IPW = pcon_weights),
                      var.name = "lpop", which = "both") +
  labs(title = "", y = "", x = "Log(Population)") +
  scale_fill_discrete(name = "PKO") +
  theme(legend.position = "none")

pc_dem_den <- bal.plot(pko ~ democracy, data = pcon,
                      weights = list(NN = pcon_mmatch,
                                     CEM = pcon_cmatch,
                                     IPW = pcon_weights),
                      var.name = "democracy", which = "both") +
  labs(title = "", y = "", x = "Democracy") +
  scale_fill_discrete(name = "PKO") +
  theme(legend.position = "none")

pc_wd_den <- bal.plot(pko ~ pwar_dur, data = pcon,
                     weights = list(NN = pcon_mmatch,
                                    CEM = pcon_cmatch,
                                    IPW = pcon_weights),
                     var.name = "pwar_dur", which = "both") +
  labs(title = "", y = "", x = "Prior War Duration") +
  scale_fill_discrete(name = "PKO") +
  theme(legend.position = "none")

pc_eth_bar <- bal.plot(pko ~ peth_war, data = pcon,
                      weights = list(NN = pcon_mmatch,
                                     CEM = pcon_cmatch,
                                     IPW = pcon_weights),
                      var.name = "peth_war", which = "both") +
  labs(title = "", y = "", x = "Prior Ethnic War") +
  scale_fill_discrete(name = "PKO") +
  theme(legend.position = "none")

pc_pagg_bar <- bal.plot(pko ~ pp_agg, data = pcon,
                       weights = list(NN = pcon_mmatch,
                                      CEM = pcon_cmatch,
                                      IPW = pcon_weights),
                       var.name = "pp_agg", which = "both") +
  labs(title = "", y = "", x = "Prior Peace Agreement") +
  scale_fill_discrete(name = "PKO") +
  theme(legend.position = "none")

pc_int_bar <- bal.plot(pko ~ p_int, data = pcon,
                       weights = list(NN = pcon_mmatch,
                                      CEM = pcon_cmatch,
                                      IPW = pcon_weights),
                       var.name = "p_int", which = "both") +
  labs(title = "", y = "", x = "Prior Conflict Intensity") +
  scale_fill_discrete(name = "PKO") +
  theme(legend.position = "none")

# Convert the Matched Objects to Data Sets
con_mmatch <- match.data(con_mmatch)
con_cmatch <- match.data(con_cmatch)
pcon_mmatch <- match.data(pcon_mmatch)
pcon_cmatch <- match.data(pcon_cmatch)

# Remove Graphics From Memory
rm(con_cb_lplot, pcon_cb_lplot, c_ks_plot, pc_ks_plot, 
   c_gdp_den, pc_gdp_den, c_natres_den, pc_natres_den, c_milper_den, pc_milper_den, 
   c_pop_den, pc_pop_den, c_dem_den, pc_dem_den, c_wd_den, pc_wd_den, 
   c_eth_bar, pc_eth_bar, pc_pagg_bar, pc_int_bar, cv_names, pcv_names)
