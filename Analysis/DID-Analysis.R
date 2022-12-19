############################################################################
###############---------------PKO DiD Analysis---------------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "DescTools", # Carrying Values of Observations Forward (LOCF)
  "haven", # Importing Data from Stata
  "readxl", # Importing Data from Excel
  install = FALSE
)

############################################################################
###############---------Data Collection and Cleaning---------###############
############################################################################

# Load and Clean UCDP Georeferenced Event Data (GED)

load("Data/ucdp_ged_22_1.RData")
ged <- GEDEvent_v22_1
rm(GEDEvent_v22_1)

# Load and Clean UCDP Managing Intrastate Conflict (MIC) Africa Data

############################################################################
###############------------Map of PKO Distribution-----------###############
############################################################################

pacman::p_load(
  "sf", 
  "rnaturalearth", 
  "rnaturalearthdata", 
  install = FALSE
)

############################################################################
###############-------Difference-in-Differences Analysis-----###############
############################################################################

pacman::p_load()

## Testing Parallel Trends Assumption

# Graph of Trend

# Placebo Test

## Regression Models

# Base Model (M1)

# M2: Base Model With Controls (Matched and Covariates?)

# M3: Base Model With IPW

## Sensitivity Analysis


