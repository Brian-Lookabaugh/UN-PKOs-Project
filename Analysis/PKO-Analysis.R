########################################################################
###############---------------PKO Analysis---------------###############
########################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "tidysynth", # Tidy Implementation of the Synthetic Control Method
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
  filter(type_of_conflict == 3)

# Collapse Data Into Country-Year Units

# Merge COW Country-Year Data

# Generate Civil War Dummy Variable

# Drop Non-Conflict Cases

# Drop Cases Where Conflict Never Ends

# Generate Conflict Recurrence Variable

# Generate Conflict Termination Variable

# Merge Relevant UCDP Information (War Outcome, Conflict Intensity)

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