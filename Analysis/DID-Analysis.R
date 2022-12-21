############################################################################
###############---------------PKO DiD Analysis---------------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
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
# (Aggregate, State-Based, Non-State Based, and OSV - One Sided Violence)

agg <- ged %>%
  group_by(priogrid_gid, year) %>%
  summarise(agg_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE),
            agg_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

sb <- ged %>%
  group_by(priogrid_gid, year) %>%
  filter(type_of_violence == 1) %>%
  summarise(sb_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE),
            sb_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

nsb <- ged %>%
  group_by(priogrid_gid, year) %>%
  filter(type_of_violence == 2) %>%
  summarise(nsb_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE),
            nsb_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

osv <- ged %>%
  group_by(priogrid_gid, year) %>%
  filter(type_of_violence == 3) %>%
  summarise(osv_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE),
            osv_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

# Merge The Collapsed Data

ged_col <- full_join(agg, sb,
                     by = c("priogrid_gid", "year")) %>%
  full_join(nsb,
            by = c("priogrid_gid", "year")) %>%
  full_join(osv,
            by = c("priogrid_gid", "year"))

# Merge Country Information Back In From Base GED Data Set

ged <- ged %>%
  select(priogrid_gid, year, country)

ged_col <- left_join(ged_col, ged,
                       by = c("priogrid_gid", "year"))

ged_col <- ged_col %>%
  distinct(priogrid_gid, .keep_all = TRUE)

# Remove Individual Collapsed Data Sets

rm(agg)
rm(sb)
rm(nsb)
rm(osv)

# Load, Clean, and Merge Geo-PKO Data

geo_pko <- readr::read_csv("Data/geo_pko_v.2.0.csv")

pko_ged <- full_join(ged_col, geo_pko,
                     by = c("priogrid_gid" = "prioid", "year"))

pko_ged <- pko_ged %>%
  # Filter X Country
  filter() %>%
  # Create the Treated/Non-Treated Variable
  mutate(treated = if_else(
    is.na(mission), 0, 1
  )) %>%
  # Create the Before/After Treatment Variable
  mutate() %>%
  # Select Important Columns to Reduce Size of Data Set
  select()
  

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

