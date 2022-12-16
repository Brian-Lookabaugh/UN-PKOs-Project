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

# Statistical Test

## Regression Models

# Base Model (M1): Two Time Periods, Binary Treatment, No Controls

# M2: Base Model With IPW

# M3: Base Model With Controls

# M4: Base Model With Multiple Time Periods

## Sensitivity Analysis


