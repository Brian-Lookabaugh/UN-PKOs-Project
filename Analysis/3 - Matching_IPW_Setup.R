############################################################################
###############-------------Matching/IPW Set Up--------------###############
############################################################################

pacman::p_load(
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
  lead = 0:3,
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
tv.nn.dep.1 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.dep.1$att,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.dep.2 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.dep.2$att,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.dep.3 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.dep.3$att,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.dep.4 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.dep.4$att,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.dep.5.1 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.dep.5.1$att,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.dep.5.2 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.dep.5.2$att,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.dep.5.3 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.dep.5.3$att,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.dep.5.4 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.dep.5.4$att,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.wth.1 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.wth.1$art,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.wth.2 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.wth.2$art,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.wth.3 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.wth.3$art,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.wth.4 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.wth.4$art,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.wth.5.1 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.wth.5.1$art,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.wth.5.2 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.wth.5.2$art,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.wth.5.3 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.wth.5.3$art,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )
tv.nn.wth.5.4 <-
  DisplayTreatment(
    data = merged,
    matched.set = nn.wth.5.4$art,
    unit.id = "ccode",
    time.id = "year",
    treatment = "pko",
    show.set.only = TRUE
  )

## Create Matched Sets With Unrestricted PKO Measure After Finding Correct Lag Criteria
alt.dep.ipw.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko2",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1)) +
    I(lag(lmilper, 1)) +
    I(lag(ldeaths, 1)) +
    I(lag(democracy, 1)),
  qoi = "att",
  lead = 0:3,
  outcome.var = "lgdppc",
  restrict.control.period = 1
)

alt.dep.ipw.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko2",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:2)) +
    I(lag(lmilper, 1:2)) +
    I(lag(ldeaths, 1:2)) +
    I(lag(democracy, 1:2)),
  qoi = "att",
  lead = 0:3,
  outcome.var = "lgdppc",
  restrict.control.period = 2
)

alt.dep.ipw.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko2",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:3)) +
    I(lag(lmilper, 1:3)) +
    I(lag(ldeaths, 1:3)) +
    I(lag(democracy, 1:3)),
  qoi = "att",
  lead = 0:3,
  outcome.var = "lgdppc",
  restrict.control.period = 3
)

alt.wth.ipw.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko2",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1)) +
    I(lag(lmilper, 1)) +
    I(lag(ldeaths, 1)) +
    I(lag(democracy, 1)),
  qoi = "art",
  lead = 0:3,
  outcome.var = "lgdppc",
  restrict.control.period = 1
)

alt.wth.ipw.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko2",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:2)) +
    I(lag(lmilper, 1:2)) +
    I(lag(ldeaths, 1:2)) +
    I(lag(democracy, 1:2)),
  qoi = "art",
  lead = 0:3,
  outcome.var = "lgdppc",
  restrict.control.period = 2
)

alt.wth.ipw.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "pko2",
  refinement.method = "ps.weight",
  data = merged,
  covs.formula = ~
    I(lag(eth_con, 1:3)) +
    I(lag(lmilper, 1:3)) +
    I(lag(ldeaths, 1:3)) +
    I(lag(democracy, 1:3)),
  qoi = "art",
  lead = 0:3,
  outcome.var = "lgdppc",
  restrict.control.period = 3
)

# Create Covariate Balance Plots
png("Graphics/cb_plot.png", width = 1600, height = 1600, res = 300)

plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(3,3),
    pty = "s")

balance_scatter(
  nn.dep.1,
  xlim = c(0, 2.5),
  ylim = c(0, 2.5),
  data = merged,
  covariates = c("lmilper", "ldeaths", "eth_con", "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  nn.dep.2,
  xlim = c(0, 2.5),
  ylim = c(0, 2.5),
  data = merged,
  covariates = c("lmilper", "ldeaths", "eth_con", "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn.dep.3,
  xlim = c(0, 2.5),
  ylim = c(0, 2.5),
  data = merged,
  covariates = c("lmilper", "ldeaths", "eth_con", "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn.dep.5.1,
  xlim = c(0, 2.5),
  ylim = c(0, 2.5),
  data = merged,
  covariates = c("lmilper", "ldeaths", "eth_con", "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  nn.dep.5.2,
  xlim = c(0, 2.5),
  ylim = c(0, 2.5),
  data = merged,
  covariates = c("lmilper", "ldeaths", "eth_con", "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  nn.dep.5.3,
  xlim = c(0, 2.5),
  ylim = c(0, 2.5),
  data = merged,
  covariates = c("lmilper", "ldeaths", "eth_con", "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  ipw.dep.1,
  xlim = c(0, 2.5),
  ylim = c(0, 2.5),
  data = merged,
  covariates = c("lmilper", "ldeaths", "eth_con", "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = ""
)

balance_scatter(
  ipw.dep.2,
  xlim = c(0, 2.5),
  ylim = c(0, 2.5),
  data = merged,
  covariates = c("lmilper", "ldeaths", "eth_con", "democracy"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

balance_scatter(
  ipw.dep.3,
  xlim = c(0, 2.5),
  ylim = c(0, 2.5),
  data = merged,
  covariates = c("lmilper", "ldeaths", "eth_con", "democracy"),
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
mtext(2, text = "NN Matching \n 1 Match",
      line = 1.15, at = .82, outer = TRUE,
      cex = .8)
mtext(2, text = "NN Matching \n Up to 5 Matches",
      line = 1.15, at = .5, outer = TRUE,
      cex = .8)
mtext(2, text = "IPW",
      line = 1.15, at = .16, outer = TRUE,
      cex = .8)
mtext("One Year Lag",
      line = 0, at = 0.17, outer = TRUE, cex = .8)
mtext("Two Year Lag",
      line = 0, at = 0.5, outer = TRUE, cex = .8)
mtext("Three Year Lag",
      line = 0, at = 0.83, outer = TRUE, cex = .8)

dev.off()

# Covariate Trend Plot
plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(2,2),
    pty = "s")

get_covariate_balance(ipw.dep.3$att,
                      data = merged,
                      covariates = c("lgdppc"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      xaxt = "n",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(alt.dep.ipw.3$att,
                      data = merged,
                      covariates = c("lgdppc"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      yaxt = "n",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(ipw.wth.3$art,
                      data = merged,
                      covariates = c("lgdppc"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(alt.wth.ipw.3$art,
                      data = merged,
                      covariates = c("lgdppc"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
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
mtext("Restricted PKO \n Measure",
      line = -1, at = 0.25, outer = TRUE, cex = .8)
mtext("Unrestricted PKO \n Measure",
      line = -1, at = 0.75, outer = TRUE, cex = .8)

dev.off()
