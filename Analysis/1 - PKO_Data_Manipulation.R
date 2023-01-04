############################################################################
###############--------PKO Data Collection & Cleaning--------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "DescTools", # LOCF Command
  "countrycode", # Importing COW/GWN Codes for Non-State Based Violence Data
  install = FALSE
)

# Load UCDP/PRIO Armed Conflict Data Set to Isolate Cases of 
# Civil War and Post-Civil War States

prio <- readr::read_csv("Data/ucdp-prio-acd-221.csv")

prio <- prio %>%
  mutate(gwno_a = as.numeric(gwno_a)) %>% # Convert the country code to a numeric
  filter(type_of_conflict == 3) %>% # Filter Cases of Civil War
  mutate(civ_war = 1) %>% # Filter Civil War Variable
  group_by(gwno_a, year) %>%
  summarise(civ_war = max(civ_war)) %>% # Collapse Data to Country-Year Level
  ungroup()

# Merge This Data With COW States Data to Identify All Country-Years

cow <- readr::read_csv("Data/system2016_cow.csv")

prio <- left_join(cow, prio,
                  by = c("ccode" = "gwno_a", "year"))

# Generate Prior War Variable So We Can Isolate Post-Conflict Cases
# In Addition to Conflict-Cases

prio <- prio %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(ev_civwar = LOCF(civ_war)) %>%
  ungroup() %>%
  mutate(ev_civwar = if_else(
    is.na(ev_civwar), 0, ev_civwar
  )) %>%
  filter(ev_civwar == 1)

# Load and Clean UCDP Georeferenced Data (GED)

load("Data/ucdp_ged_22_1.RData")
ged <- GEDEvent_v22_1
rm(GEDEvent_v22_1)

ged <- ged %>%
  mutate(gwnoa = as.numeric(gwnoa))

# Collapse the GED Data to State-Year Level and Get Sums and Counts of Battle Deaths and 
# Lethal Events For Different Types of Violence 
# (State-Based, Non-State Based, and OSV - One Sided Violence)

# State-Based Violence
sb <- ged %>%
  group_by(gwnoa, year) %>%
  filter(type_of_violence == 1) %>%
  summarise(sb_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE),
            sb_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

# Non-State Based Violence
nsb <- ged %>%
  group_by(gwnoa, country, year) %>%
  filter(type_of_violence == 2) %>%
  summarise(nsb_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE),
            nsb_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

# Assign COW Codes to Non-State Based Incidents

nsb <- nsb %>%
  mutate(gwnoa = countrycode(
    nsb$country, "country.name", "gwn"
  )) %>%
  mutate(gwnoa = if_else(
    country == "Serbia (Yugoslavia)", 345, gwnoa # Replace NA Values for Yugoslavia
  ))

# OSV
osv <- ged %>%
  group_by(gwnoa, year) %>%
  filter(type_of_violence == 3) %>%
  summarise(osv_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE),
            osv_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

# Merge the Collapsed Data

ged_col <- full_join(sb, nsb,
                     by = c("gwnoa", "year")) %>%
  full_join(osv,
            by = c("gwnoa", "year"))

# Replace NA Values With 0

ged_col <- ged_col %>%
  mutate_at(c('sb_death', 'sb_event', 'nsb_death', 'nsb_event',
              'osv_death', 'osv_event'),
            ~replace_na(.,0))

# Remove Older Data Sets
rm(sb)
rm(nsb)
rm(osv)

# Merge the GED Data With the UCDP/PRIO Data

merged <- left_join(prio, ged_col,
                    by = c("ccode" = "gwnoa", "year"))

# Load and Clean Geo-PKO Data

geo_pko <- readr::read_csv("Data/geo_pko_v.2.0.csv")

# Collapse the Data to State-Year Level and Get Sums of PKO Troops

geo_pko <- geo_pko %>%
  mutate_at(c("no.troops"), as.numeric) %>% # Convert Troop Count to Numeric
  filter(no.troops != "unknown", na.rm = TRUE) %>% # Remove Unknown Values
  group_by(cow_code, year) %>% 
  summarise(pko_troops = sum(no.troops, na.rm = TRUE)) %>%
  mutate(pko = 1) %>% # Create a Variable Denoting the Presence of a PKO 
  ungroup()

# Merge the Geo-PKO Data With the UCDP GED Data

merged <- left_join(merged, geo_pko,
                    by = c("ccode" = "cow_code", "year"))

# Clean Up the Merged Data

merged <- merged %>%
  mutate(pko = if_else( # Replace NA Values for PKO With 0
    is.na(pko), 0, 1
  )) %>%
  mutate(pko_troops = if_else(
    is.na(pko_troops), 0, pko_troops)) # Replace NA Value for PKO Troops With 0

# Load and Merge V-Dem Data

vdem <- readr::read_csv("Data/selected_vdem_v12.csv")

merged <- left_join(merged, vdem,
                    by = c("ccode" = "COWcode", "year"))

# Load and Clean COW's Military Personnel Data

cow <- readr::read_csv("Data/cow_nmc_v4.csv")

cow <- cow %>%
  filter(milper != -9) # Remove NA Values

merged <- left_join(merged, cow,
                    by = c("ccode", "year"))

# Final Data Cleaning and Organization
merged <- merged %>% 
  # Generate Logged Values
  mutate(e_gdppc = if_else( # Dealing With Values Less Than 1 For Log-Transformation
    e_gdppc < 1, e_gdppc + 1, e_gdppc
  )) %>%
  mutate(lgdppc = log(e_gdppc)) %>%
  mutate(lpop = log(e_pop)) %>%
  mutate(milper = ifelse( 
    milper < 1, milper + 1, milper
  )) %>%
  mutate(lmilper = log(milper)) %>% 
  mutate(e_total_resources_income_pc = if_else(
    e_total_resources_income_pc < 1, e_total_resources_income_pc + 1, e_total_resources_income_pc
  )) %>%
  mutate(lnatres = log(e_total_resources_income_pc)) %>%
  # Replace NA Values for Civil War Variable With 0
  mutate(civ_war = if_else(
    is.na(civ_war), 0, civ_war
  )) %>%
  # Convert Outcome Variables to Numeric
  mutate_at(c("sb_death", "sb_event", "nsb_death", "nsb_event", "osv_death", "osv_event"),
            as.numeric) %>%
  # Keep Select Variables
  select(ccode, year, sb_death, sb_event, nsb_death, nsb_event, osv_death, osv_event,
         pko, pko_troops, lnatres, lgdppc, lpop, lmilper, civ_war) %>%
  # Remove NA Values for Outcome Variables With Missingness in All Variables
  filter_at(vars(sb_death, sb_event, nsb_death, nsb_event, osv_death, osv_event),
            all_vars(!is.na(.))) %>%
  # Remove NA Values for Covariates With Missingness in All Variables
  filter_at(vars(lnatres, lgdppc, lpop, lmilper, civ_war), all_vars(!is.na(.)))

# Remove Older Data Sets
rm(prio)
rm(ged)
rm(ged_col)
rm(geo_pko)
rm(vdem)
rm(cow)
