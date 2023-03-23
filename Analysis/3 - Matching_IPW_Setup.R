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

## NN Matching - 5 Matches - 1 Lag

## NN Matching - 5 Matches - 2 Lags

## NN Matching - 5 Matches - 3 Lags

## NN Matching - 5 Matches - 4 Lags

## NN Matching - 10 Matches - 1 Lag

## NN Matching - 10 Matches - 2 Lags

## NN Matching - 10 Matches - 3 Lags

## NN Matching - 10 Matches - 4 Lags

## IPW - 1 Lag

## IPW - 2 Lags

## IPW - 3 Lags

## IPW - 4 Lags

# Do the Same While Estimating the ART

## NN Matching - 5 Matches - 1 Lag

## NN Matching - 5 Matches - 2 Lags

## NN Matching - 5 Matches - 3 Lags

## NN Matching - 5 Matches - 4 Lags

## NN Matching - 10 Matches - 1 Lag

## NN Matching - 10 Matches - 2 Lags

## NN Matching - 10 Matches - 3 Lags

## NN Matching - 10 Matches - 4 Lags

## IPW - 1 Lag

## IPW - 2 Lags

## IPW - 3 Lags

## IPW - 4 Lags

# Examine the Coverage of Each With TV Plots

# Create Covariate Balance Plots

# Covariate Trend Plot
