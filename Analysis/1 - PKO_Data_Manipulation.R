############################################################################
###############--------PKO Data Collection & Cleaning--------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "DescTools", # LOCF (Last Observation Carried Forward) Command
  install = FALSE
)

#######-------Conflict-Level Data-------#######

# Load UCDP/PRIO Armed Conflict Data Set to Isolate Cases of Civil War
ucdp <- readr::read_csv("Data/ucdp-prio-acd-221.csv")

ucdp_con <- ucdp %>%
  mutate(gwno_a = as.numeric(gwno_a)) %>% # Convert the Country Code to a Numeric
  filter(type_of_conflict == 3) %>% # Filter Cases of Civil War
  group_by(gwno_a, year) %>%
  summarise(gwno_a = max(gwno_a)) %>% # Collapse Data to Country-Year Level
  ungroup()

# Load and Clean UCDP Georeferenced Data (GED) for Information on Deaths and Lethal Events
load("Data/ucdp_ged_22_1.RData")
ged <- GEDEvent_v22_1
rm(GEDEvent_v22_1)

ged <- ged %>%
  mutate(gwnoa = as.numeric(gwnoa)) %>%
  group_by(gwnoa, year) %>%
  summarise(deaths = max(best),
            events = n_distinct(id))

# Merge the GED Data In With the UCDP Data
merged_con <- left_join(ucdp_con, ged,
                        by = c("gwno_a" = "gwnoa", "year"))

# Replace NA Values for Deaths and Events With 0
merged_con <- merged_con %>%
  mutate_at(c('deaths', 'events'), 
            ~replace_na(., 0))

# Load Geo-PKO Data and Collapse It to the Country-Year Level
geo_pko <- readr::read_csv("Data/geo_pko_v.2.0.csv")

geo_pko <- geo_pko %>%
  mutate(pko = 1) %>%
  group_by(cow_code, year) %>%
  summarise(pko = max(pko)) %>%
  ungroup()

# Merge the Geo-PKO Data
merged_con <- left_join(merged_con, geo_pko,
                        by = c("gwno_a" = "cow_code", "year"))

# Replace NA Values for PKO With 0
merged_con <- merged_con %>%
  mutate(pko = if_else(
    is.na(pko), 0, pko
  ))

# Load and Merge V-Dem Data
vdem <- readr::read_csv("Data/selected_vdem_v12.csv")

merged_con <- left_join(merged_con, vdem,
                    by = c("gwno_a" = "COWcode", "year"))

# Create Log-Transformed Values
merged_con <- merged_con %>%
  mutate(lgdppc = log(e_gdppc + 1)) %>%
  mutate(lpop = log(e_pop)) %>%
  mutate(lnatres = log(e_total_resources_income_pc + 1)) %>%
  select(-c(e_total_fuel_income_pc, e_total_oil_income_pc, e_total_resources_income_pc,
            ...6, e_pop, e_gdppc, e_wb_pop, e_mipopula 
            ))

# Load, Clean, and Merge Correlates of War (COW) Data
cow <- readr::read_csv("Data/cow_nmc_v4.csv")

cow <- cow %>%
  filter(milper != -9) %>% # Remove NA Values
  select(c(ccode, year, milper)) # Keep Selected Columns

merged_con <- left_join(merged_con, cow,
                    by = c("gwno_a" = "ccode", "year"))

# Create the Log-Transformed Value of Military Personnel per capita
merged_con <- merged_con %>%
  mutate(lmilper = log(milper + 1)) %>%
  select(-c(milper))

#######-------Post-Conflict Level Data-------#######

# Use UCDP to Isolate Post-Conflict States
ucdp_pcon <- ucdp %>%
  mutate(gwno_a = as.numeric(gwno_a)) %>% # Convert the Country Code to a Numeric
  filter(type_of_conflict == 3) %>% # Filter Cases of Civil War
  mutate(civ_war = 1) %>% # Create Civil War Variable
  group_by(gwno_a, year) %>%
  summarise(civ_war = max(civ_war)) %>% # Collapse Data to Country-Year Level
  ungroup()

# Merge This Data With COW States Data to Identify All Country-Years
cow <- readr::read_csv("Data/system2016_cow.csv")

ucdp_pcon <- left_join(cow, ucdp_pcon,
                  by = c("ccode" = "gwno_a", "year"))

# Generate Prior War Variable So We Can Isolate Post-Conflict Cases
ucdp_pcon <- ucdp_pcon %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(ev_civwar = LOCF(civ_war)) %>%
  ungroup() %>%
  mutate(ev_civwar = if_else(
    is.na(ev_civwar), 0, ev_civwar
  )) %>% 
  mutate(civ_war = if_else(
    is.na(civ_war), 0, civ_war
  )) %>% # Replace NA Values for Civil War Variable With 0
  filter(ev_civwar == 1) %>%
  filter(civ_war == 0) %>% # Remove Conflict-Level Cases
  select(-c(civ_war, ev_civwar, version)) # Remove Unnecessary Variables

# Merge GED Data
merged_pcon <- left_join(ucdp_pcon, ged,
                        by = c("ccode" = "gwnoa", "year"))

# Replace NA Values for Deaths and Events With 0
merged_pcon <- merged_pcon %>%
  mutate_at(c('deaths', 'events'), 
            ~replace_na(., 0))

# Merge the Geo-PKO Data
merged_pcon <- left_join(merged_pcon, geo_pko,
                        by = c("ccode" = "cow_code", "year"))

# Replace NA Values for PKO With 0
merged_pcon <- merged_pcon %>%
  mutate(pko = if_else(
    is.na(pko), 0, pko
  ))

# Merge VDEM Data and Create Log-Transformed Values
merged_pcon <- left_join(merged_pcon, vdem,
                        by = c("ccode" = "COWcode", "year"))

merged_pcon <- merged_pcon %>%
  mutate(lgdppc = log(e_gdppc + 1)) %>%
  mutate(lpop = log(e_pop)) %>%
  mutate(lnatres = log(e_total_resources_income_pc + 1)) %>%
  select(-c(e_total_fuel_income_pc, e_total_oil_income_pc, e_total_resources_income_pc,
            ...6, e_pop, e_gdppc, e_wb_pop, e_mipopula 
  ))

# Load, Clean, and Merge Correlates of War (COW) Data
merged_pcon <- left_join(merged_pcon, cow,
                        by = c("ccode", "year"))

# Create the Log-Transformed Value of Military Personnel per capita
merged_pcon <- merged_pcon %>%
  mutate(lmilper = log(milper + 1)) %>%
  select(-c(milper))

# Remove Unnecessary Data Sets
rm(cow, ged, geo_pko, ucdp, vdem)
