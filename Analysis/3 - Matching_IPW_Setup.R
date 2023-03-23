############################################################################
###############-------------Matching/IPW Set Up--------------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "PanelMatch", # Matching/Weighting Set-Up With Panel Data
  install = FALSE
)

# Create a Treatment Variation Plot
merged <- merged %>%
  mutate(year = as.integer(year))

merged <- as.data.frame(merged)

merged <- merged %>% # Drop Observations Pre-1994 and Post-2016 for Plot
  filter(year >= 1994, year < 2017) 

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
  scale_x_discrete(breaks = c(1995, 2000, 2005, 2010, 2015))

ggsave(
  "tv_plot.png",
  width = 6,
  height = 8,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/UN PKOs Project/Graphics"
)

# Create Matched Sets (ATT)
merged <- merged %>%
  mutate(ccode = as.integer(ccode)) %>%
  select(-c(stateabb)) # PanelMatch Will Not Run With Non-Numeric/Integer Data

## NN Matching - 1 Match - 1 Lag
nn.dep.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1)) +
    I(lag(lmilper, 1)) +
    I(lag(ldeaths, 1)) +
    I(lag(democracy, 1)),
  qoi = "att",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 1
)

## NN Matching - 1 Match - 2 Lags
nn.dep.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:2)) +
    I(lag(lmilper, 1:2)) +
    I(lag(ldeaths, 1:2)) +
    I(lag(democracy, 1:2)),
  qoi = "att",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 2
)

## NN Matching - 1 Match - 3 Lags
nn.dep.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:3)) +
    I(lag(lmilper, 1:3)) +
    I(lag(ldeaths, 1:3)) +
    I(lag(democracy, 1:3)),
  qoi = "att",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 3
)

## NN Matching - 1 Match - 4 Lags
nn.dep.4 <- PanelMatch(
  lag = 4,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:4)) +
    I(lag(lmilper, 1:4)) +
    I(lag(ldeaths, 1:4)) +
    I(lag(democracy, 1:4)),
  qoi = "att",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 4
)

## NN Matching - 5 Matches - 1 Lag
nn.dep.5.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 5,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1)) +
    I(lag(lmilper, 1)) +
    I(lag(ldeaths, 1)) +
    I(lag(democracy, 1)),
  qoi = "att",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 1
)

## NN Matching - 5 Matches - 2 Lags
nn.dep.5.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 5,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:2)) +
    I(lag(lmilper, 1:2)) +
    I(lag(ldeaths, 1:2)) +
    I(lag(democracy, 1:2)),
  qoi = "att",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 2
)

## NN Matching - 5 Matches - 3 Lags
nn.dep.5.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 5,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:3)) +
    I(lag(lmilper, 1:3)) +
    I(lag(ldeaths, 1:3)) +
    I(lag(democracy, 1:3)),
  qoi = "att",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 3
)

## NN Matching - 5 Matches - 4 Lags
nn.dep.5.4 <- PanelMatch(
  lag = 4,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 5,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:4)) +
    I(lag(lmilper, 1:4)) +
    I(lag(ldeaths, 1:4)) +
    I(lag(democracy, 1:4)),
  qoi = "att",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 4
)

## IPW - 1 Lag
ipw.dep.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1)) +
    I(lag(lmilper, 1)) +
    I(lag(ldeaths, 1)) +
    I(lag(democracy, 1)),
  qoi = "att",
  outcome.var = "lgdppc",
  restrict.control.period = 1
)

## IPW - 2 Lags
ipw.dep.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:2)) +
    I(lag(lmilper, 1:2)) +
    I(lag(ldeaths, 1:2)) +
    I(lag(democracy, 1:2)),
  qoi = "att",
  outcome.var = "lgdppc",
  restrict.control.period = 2
)

## IPW - 3 Lags
ipw.dep.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:3)) +
    I(lag(lmilper, 1:3)) +
    I(lag(ldeaths, 1:3)) +
    I(lag(democracy, 1:3)),
  qoi = "att",
  outcome.var = "lgdppc",
  restrict.control.period = 3
)

## IPW - 4 Lags
ipw.dep.4 <- PanelMatch(
  lag = 4,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:4)) +
    I(lag(lmilper, 1:4)) +
    I(lag(ldeaths, 1:4)) +
    I(lag(democracy, 1:4)),
  qoi = "att",
  outcome.var = "lgdppc",
  restrict.control.period = 4
)

# Do the Same While Estimating the ART
## NN Matching - 1 Match - 1 Lag
nn.wth.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1)) +
    I(lag(lmilper, 1)) +
    I(lag(ldeaths, 1)) +
    I(lag(democracy, 1)),
  qoi = "art",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 1
)

## NN Matching - 1 Match - 2 Lags
nn.wth.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:2)) +
    I(lag(lmilper, 1:2)) +
    I(lag(ldeaths, 1:2)) +
    I(lag(democracy, 1:2)),
  qoi = "art",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 2
)

## NN Matching - 1 Match - 3 Lags
nn.wth.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:3)) +
    I(lag(lmilper, 1:3)) +
    I(lag(ldeaths, 1:3)) +
    I(lag(democracy, 1:3)),
  qoi = "art",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 3
)


## NN Matching - 1 Match - 4 Lags
nn.wth.4 <- PanelMatch(
  lag = 4,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:4)) +
    I(lag(lmilper, 1:4)) +
    I(lag(ldeaths, 1:4)) +
    I(lag(democracy, 1:4)),
  qoi = "art",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 4
)

## NN Matching - 5 Matches - 1 Lag
nn.wth.5.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 5,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1)) +
    I(lag(lmilper, 1)) +
    I(lag(ldeaths, 1)) +
    I(lag(democracy, 1)),
  qoi = "art",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 1
)

## NN Matching - 5 Matches - 2 Lags
nn.wth.5.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 5,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:2)) +
    I(lag(lmilper, 1:2)) +
    I(lag(ldeaths, 1:2)) +
    I(lag(democracy, 1:2)),
  qoi = "art",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 2
)

## NN Matching - 5 Matches - 3 Lags
nn.wth.5.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 5,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:3)) +
    I(lag(lmilper, 1:3)) +
    I(lag(ldeaths, 1:3)) +
    I(lag(democracy, 1:3)),
  qoi = "art",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 3
)

## NN Matching - 5 Matches - 4 Lags
nn.wth.5.4 <- PanelMatch(
  lag = 4,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "mahalanobis",
  size.match = 5,
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:4)) +
    I(lag(lmilper, 1:4)) +
    I(lag(ldeaths, 1:4)) +
    I(lag(democracy, 1:4)),
  qoi = "art",
  outcome.var = "lgdppc",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 4
)

## IPW - 1 Lag
ipw.wth.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1)) +
    I(lag(lmilper, 1)) +
    I(lag(ldeaths, 1)) +
    I(lag(democracy, 1)),
  qoi = "art",
  outcome.var = "lgdppc",
  restrict.control.period = 1
)

## IPW - 2 Lags
ipw.wth.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:2)) +
    I(lag(lmilper, 1:2)) +
    I(lag(ldeaths, 1:2)) +
    I(lag(democracy, 1:2)),
  qoi = "art",
  outcome.var = "lgdppc",
  restrict.control.period = 2
)

## IPW - 3 Lags
ipw.wth.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:3)) +
    I(lag(lmilper, 1:3)) +
    I(lag(ldeaths, 1:3)) +
    I(lag(democracy, 1:3)),
  qoi = "art",
  outcome.var = "lgdppc",
  restrict.control.period = 3
)

## IPW - 4 Lags
ipw.wth.4 <- PanelMatch(
  lag = 4,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:4)) +
    I(lag(lmilper, 1:4)) +
    I(lag(ldeaths, 1:4)) +
    I(lag(democracy, 1:4)),
  qoi = "art",
  outcome.var = "lgdppc",
  restrict.control.period = 4
)

# Examine the Coverage of Each With TV Plots

# Create Covariate Balance Plots

# Covariate Trend Plot
