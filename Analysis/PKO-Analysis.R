########################################################################
###############---------------PKO Analysis---------------###############
########################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "tidysynth", # Tidy Implementation of the Synthetic Control Method
  "DescTools", # Carrying Values of Observations Forward (LOCF)
  "haven", # Importing Data from Stata
  "sf", # Maps
  "rnaturalearth", # Maps
  "rnaturalearthdata", # Maps
  install = FALSE
)

############################################################
########--------Data Collection and Cleaning--------########
############################################################

## UCDP Data

load("C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Data/ucdp_prio_acd_221.RData")
ucdp <- ucdp_prio_acd_221

# Drop Non-Civil War Cases (1K Threshold)

ucdp <- ucdp %>%
  mutate(gwno_a = as.numeric(gwno_a)) %>% # Convert the country code to a numeric
  filter(type_of_conflict == 3 & cumulative_intensity == 1) %>%
  mutate(civ_war = 1) %>%
  
  # Collapse Data Into Country-Year Units
  group_by(gwno_a, year) %>%
  summarise(civ_war = max(civ_war)) %>%
  ungroup()

# Merge COW Country-Year Data

states <- read.csv("C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Data/system2016.csv")

ucdp <- full_join(ucdp, states,
                   by = c("gwno_a" = "ccode", "year"))

# Drop Cases That Have Never Experienced a Civil War

ucdp <- ucdp %>%
  rename(ccode = gwno_a) %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(prior_civ_war = LOCF(civ_war)) %>%
  ungroup() %>%
  filter(prior_civ_war == 1) %>%
  mutate(civ_war = if_else( # Replace NA Civil War Values With 0
    is.na(civ_war), 0, civ_war
  )) %>%

# Generate Conflict Recurrence and Termination Variables
  group_by(ccode) %>%
  mutate(lag_civ_war = lag(civ_war, n = 1, order_by = ccode)) %>%
  ungroup() %>%
  mutate(recur = if_else(
    civ_war == 1 & lag_civ_war == 0, 1, 0
  )) %>%
  mutate(recur = if_else(
    is.na(recur), 0, recur
  )) %>%
  mutate(termination = if_else(
    civ_war == 0 & lag_civ_war == 1, 1, 0
  )) %>%
  mutate(termination = if_else(
    is.na(termination), 0, termination
  ))

# Merge UCDP One-Sided Violence Data

load("C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Data/ucdp-onesided-221.RData")
osv <- ucdp_onesided_221

## PKO Data

pko <- 

## V-Dem Data

vdem <- 
  
# Re-Code Coups as Non-Civil Wars
  
## Military Capacity Data
  
mil_per <- read.csv("C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Data/NMC-60-abridged.csv")

mil_per <- mil_per %>%
  filter(milper != -9) %>%
  mutate(lmilper = log(milper + 1)) %>%
  select(ccode, year, lmilper)

ucdp <- left_join(ucdp, mil_per,
                  by = c("ccode", "year"))

## (Dependent Variable Data)
  
var <- 

## Merge Data Together and Clean Final Dataset (Rename)

synth_data <- 

###################################################################
########--------Generate a Map of PKO Distribution---------########
###################################################################

########################################################
########--------Synthetic Control Set-Up--------########
########################################################

#####################################################################
########--------Synthetic Control Graphics and Tables--------########
#####################################################################

## Time Series Plot

## Difference in Synthetic and Observed Plot

## Unit and Variable Weights Plot

## Table Comparing Synthetic to Observed

## Unit Placebos

## Unit Placebos With Extreme Values

## Post/Pre-MSPE

## Tables With Significance of Unit Treatments