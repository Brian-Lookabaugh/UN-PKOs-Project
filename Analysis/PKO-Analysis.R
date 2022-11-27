########################################################################
###############---------------PKO Analysis---------------###############
########################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "tidysynth", # Tidy Implementation of the Synthetic Control Method
  "DescTools", # Carrying Values of Observations Forward
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
  # Drop European Outlier
  # Drop Coups (Thyne 2017)
  
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
  select(-c(version, prior_civ_war))

# Drop Cases Where Conflict Never Ends

# Generate Conflict Recurrence Variable

# Generate Conflict Termination Variable

# Merge Relevant UCDP Information (War Outcome, Conflict Intensity)

# Clean Up Data

## PKO Data

pko <-

## V-Dem Data

vdem <- 

## (Dependent Variable Data)

## Merge Data Together

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