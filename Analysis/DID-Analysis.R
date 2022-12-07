############################################################################
###############---------------PKO DiD Analysis---------------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "DescTools", # Carrying Values of Observations Forward (LOCF)
  "haven", # Importing Data from Stata
  "readxl", # Importing Data from Excel
  "sf", # Maps
  "rnaturalearth", # Maps
  "rnaturalearthdata", # Maps
  install = FALSE
)

############################################################################
###############---------Data Collection and Cleaning---------###############
############################################################################

# Load Geo-Coded Data

# Load GTD Data

# Load PKO Data

############################################################################
###############------------Map of PKO Distribution-----------###############
############################################################################



############################################################################
###############-----------Difference-in-Differences----------###############
############################################################################

# Plot and Test Parallell Trends Assumption

# Estimate the DiD Effect

# Robustness Checks

# Sensitivity Analysis
