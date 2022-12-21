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

# Collapse the Data to Grid-Year Level and Get Sums and Counts of Battle Deaths and 
# Lethal Events For Different Types of Violence 
# (Aggregate, State-Based, Non-State Based, and OSV)

agg_deaths <- ged %>%
  group_by(priogrid_gid, year) %>%
  summarise(agg_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE)) %>%
  ungroup()

agg_events <- ged %>%
  group_by(priogrid_gid, year) %>%
  summarise(agg_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

sb_deaths <- ged %>%
  group_by(priogrid_gid, year) %>%
  filter(type_of_violence == 1) %>%
  summarise(sb_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE)) %>%
  ungroup()

sb_events <- ged %>%
  group_by(priogrid_gid, year) %>%
  filter(type_of_violence == 1) %>%
  summarise(sb_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

nsb_deaths <- ged %>%
  group_by(priogrid_gid, year) %>%
  filter(type_of_violence == 2) %>%
  summarise(nsb_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE)) %>%
  ungroup()

nsb_events <- ged %>%
  group_by(priogrid_gid, year) %>%
  filter(type_of_violence == 2) %>%
  summarise(nsb_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

osv_deaths <- ged %>%
  group_by(priogrid_gid, year) %>%
  filter(type_of_violence == 3) %>%
  summarise(osv_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE)) %>%
  ungroup()

osv_events <- ged %>%
  group_by(priogrid_gid, year) %>%
  filter(type_of_violence == 3) %>%
  summarise(osv_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

# Merge The Collapsed Data

ged_col <- full_join(agg_deaths, agg_events,
                     by = c("priogrid_gid", "year")) %>%
  full_join(sb_deaths,
            by = c("priogrid_gid", "year")) %>%
  full_join(sb_events,
            by = c("priogrid_gid", "year")) %>%
  full_join(nsb_deaths,
            by = c("priogrid_gid", "year")) %>%
  full_join(nsb_events,
            by = c("priogrid_gid", "year")) %>%
  full_join(osv_deaths,
            by = c("priogrid_gid", "year")) %>%
  full_join(osv_events,
            by = c("priogrid_gid", "year"))

# Remove Individual Collapsed Data Sets

rm(agg_deaths)
rm(agg_events)
rm(sb_deaths)
rm(sb_events)
rm(nsb_deaths)
rm(nsb_events)
rm(osv_deaths)
rm(osv_events)

# Load, Clean, and Merge Geo-PKO Data

geo_pko <- readr::read_csv("Data/geo_pko_v.2.0.csv", col_types = cols(.default="c"))


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

# Testing Parallel Trends Assumption

# Graph of Trend (Aggregate)

# Graph of Trend (State-Based Violence)

# Graph of Trend (Non-State Based Violence)

# Graph of Trend (OSV)

# Placebo Test (Aggregate)

# Placebo Test (State-Based Violence)

# Placebo Test (Non-State Based Violence)

# Placebo Test (OSV)

# Regression Models

# M1: Aggregate Base Model

# M2: Aggregate Base Model With IPW

# M3: State-Based Violence Model With IPW

# M4: Non-State Based Violence Model With IPW

# M5: OSV Model With IPW

# Sensitivity Analysis

