############################################################################
###############--------PKO Data Collection & Cleaning--------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "DescTools", # LOCF (Last Observation Carried Forward) Command
  "readxl", # Importing Excel Spreadsheets
  install = FALSE
)

# Create the Base Data Set to Identify The Presence of Civil War and Post-Civil War

# Load UCDP/PRIO Armed Conflict Data Set to Isolate Cases of Civil War
ucdp <- readr::read_csv("Data/ucdp-prio-acd-221.csv")

# Manipulate UCDP Data
ucdp <- ucdp %>%
  mutate(gwno_a = as.numeric(gwno_a)) %>%
  filter(type_of_conflict == 3) %>% # Filter Cases of Civil Conflict
  mutate(civ_war = 1) %>% # Set Civil War Equal to 1 for Civil War Cases
  group_by(gwno_a, year) %>% # Collapse This Data to the Country-Year Level
  summarise(civ_war = max(civ_war)) %>%
  ungroup()

# Merge This Data With COW States Data to Identify All Country-Years
cow <- readr::read_csv("Data/system2016_cow.csv")

ucdp <- left_join(cow, ucdp,
                  by = c("ccode" = "gwno_a", "year"))

# Re-Code Coups as Non-Civil Conflict Cases
vdem <- readr::read_csv("Data/selected_vdem_v12.csv") # Coup Information (Powell and Thyme 2011)

ucdp <- left_join(ucdp, vdem,
                  by = c("ccode" = "COWcode", "year"))

# Make the Coup Variable Binary
ucdp <- ucdp %>%
  mutate(coup = if_else(
    e_pt_coup > 0, 1, e_pt_coup
  )) %>%
  mutate(civ_war = if_else( # Re-Code Civil Wars As Not Cases of Civil War Where Coups Occurred
    coup == 1, 0, civ_war
  )) %>%
  select(-c(e_pt_coup, coup))

# Re-Code 3-Year or Less Peace Spells as Conflict Lulls
ucdp <- ucdp %>%
  group_by(ccode) %>%
  mutate(flag_civ_war = lag(civ_war, n = 1, order_by = ccode)) %>%
  mutate(flead_civ_war = lead(civ_war, n = 1, order_by = ccode)) %>%
  mutate(slag_civ_war = lag(civ_war, n = 2, order_by = ccode)) %>%
  mutate(slead_civ_war = lead(civ_war, n = 2, order_by = ccode)) %>%
  ungroup() %>%
  mutate(civ_war = if_else(
    flag_civ_war == 1 & flead_civ_war == 1, 1, civ_war
  )) %>%
  mutate(civ_war = if_else(
    slag_civ_war == 1 & slead_civ_war == 1, 1, civ_war
  )) %>%
  select(-c(slag_civ_war, slead_civ_war))
  
# Create an "Ever Civil War" Variable to Isolate Post-Conflict Cases  
ucdp <- ucdp %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(ev_civwar = LOCF(civ_war)) %>%
  ungroup() %>%
  mutate(ev_civwar = if_else(
    is.na(ev_civwar), 0, ev_civwar
  ))

# Replace NA Values for Civil War and Ever Civil War With 0
ucdp <- ucdp %>%
  mutate(civ_war = if_else(
    is.na(civ_war), 0, civ_war
  ))

# Create Conflict/Peace Spell Failure Variables (These Will Be Used for IDs)
ucdp <- ucdp %>%
  group_by(ccode) %>%
  mutate(con_fail = if_else( # Start By Creating a Conflict/Peace Termination (Failure) Variable
    lag(civ_war == 1) & civ_war == 0, 1, 0
  )) %>%
  mutate(peace_fail = if_else( 
    lag(civ_war == 0) & civ_war == 1, 1, 0
  )) %>%
  ungroup()

# Create the Conflict/Peace Spell ID Variable
# This Doubles As Both a Conflict And Peace Spell ID Once Filtered for Conflict/Post-Conflict Cases
ucdp <- ucdp %>%
  mutate(id = cumsum(peace_fail)) 

#######-------Conflict-Level Data-------#######

# Include Only Conflict-Year Data
con <- ucdp %>%
  filter(civ_war == 1)

# Load and Clean UCDP Georeferenced Data (GED) for Information on Deaths and Lethal Events
load("Data/ucdp_ged_22_1.RData")
ged <- GEDEvent_v22_1
rm(GEDEvent_v22_1)

ged <- ged %>%
  mutate(gwnoa = as.numeric(gwnoa)) %>%
  group_by(gwnoa, year) %>%
  summarise(deaths = max(best),
            events = n_distinct(id)) %>%
  ungroup()

# Merge the GED Data In With the UCDP Data
con <- left_join(con, ged,
                        by = c("ccode" = "gwnoa", "year"))

# Replace NA Values for Deaths and Events With 0
con <- con %>%
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
con <- left_join(con, geo_pko,
                        by = c("ccode" = "cow_code", "year"))

# Replace NA Values for PKO With 0
con <- con %>%
  mutate(pko = if_else(
    is.na(pko), 0, pko
  ))

# Create Log-Transformed Values
con <- con %>%
  mutate(lgdppc = log(e_gdppc + 1)) %>%
  mutate(lpop = log(e_pop)) %>%
  mutate(lnatres = log(e_total_resources_income_pc + 1)) %>%
  mutate(democracy = v2x_polyarchy) %>% # Rename Democracy
  select(-c(e_total_fuel_income_pc, e_total_oil_income_pc, e_total_resources_income_pc,
            ...6, e_pop, e_gdppc, e_wb_pop, e_mipopula, v2x_polyarchy 
            ))

# Load, Clean, and Merge Correlates of War (COW) Data
milcap <- readr::read_csv("Data/cow_nmc_v4.csv")

milcap <- milcap %>%
  filter(milper != -9) %>% # Remove NA Values
  select(c(ccode, year, milper)) # Keep Selected Columns

con <- left_join(con, milcap,
                    by = c("ccode", "year"))

# Create the Log-Transformed Value of Military Personnel per capita
con <- con %>%
  mutate(lmilper = log(milper + 1)) %>%
  select(-c(milper))

# Create War Duration Variable
con <- con %>%
  group_by(id) %>%
  mutate(wardur = row_number()) %>%
  ungroup()

# Create Data Set With Information
# Ethnic War, Conflict Intensity, and Military Victory
# This Will Be Used for Conflict and Post-Conflict Cases
ucdp_term <- read_excel("Data/ucdp-term-acd-3-2021.xlsx")

ucdp_term <- ucdp_term %>%
  mutate(gwno_a = as.numeric(gwno_loc)) %>%
  mutate(eth_war = if_else( # Create an Ethnic War Dummy
    incompatibility != 2, 1, 0
  )) %>%
  mutate(intensity = if_else( # Create a Conflict Intensity Dummy
    intensity_level == 2, 1, 0
  )) %>%
  mutate(mil_vic = if_else( # Create Military Victory Dummy
    outcome == 3, 1, 0
  )) %>%
  mutate(reb_vic = if_else( # Create Rebel Military Victory Dummy
    outcome == 4, 1, 0
  )) %>%
  filter(type_of_conflict == 3) %>% 
  group_by(gwno_a, year) %>% # Collapse This Data to the Country-Year Level
  summarise(eth_war = max(eth_war),
            mil_vic = max(mil_vic),
            reb_vic = max(reb_vic),
            intensity = max(intensity)) %>% 
  ungroup()

con <- left_join(con, ucdp_term,
                 by = c("ccode" = "gwno_a", "year"))

# Create Ethnic War Variable Computed by Conflict ID
con <- con %>%
  group_by(id) %>%
  mutate(eth_war = max(eth_war)) %>%
  ungroup()

#######-------Post-Conflict Level Data-------#######

# Include Only Post-Conflict-Year Data
pcon <- ucdp %>%
  filter(civ_war == 0 & ev_civwar == 1)

# Merge GED Data
pcon <- left_join(pcon, ged,
                        by = c("ccode" = "gwnoa", "year"))

# Replace NA Values for Deaths and Events With 0
pcon <- pcon %>%
  mutate_at(c('deaths', 'events'), 
            ~replace_na(., 0))

# Merge the Geo-PKO Data
pcon <- left_join(pcon, geo_pko,
                        by = c("ccode" = "cow_code", "year"))

# Replace NA Values for PKO With 0
pcon <- pcon %>%
  mutate(pko = if_else(
    is.na(pko), 0, pko
  ))

# Create Log-Transformed Values
pcon <- pcon %>%
  mutate(lgdppc = log(e_gdppc + 1)) %>%
  mutate(lpop = log(e_pop)) %>%
  mutate(lnatres = log(e_total_resources_income_pc + 1)) %>%
  mutate(democracy = v2x_polyarchy) %>%
  select(-c(e_total_fuel_income_pc, e_total_oil_income_pc, e_total_resources_income_pc,
            ...6, e_pop, e_gdppc, e_wb_pop, e_mipopula, v2x_polyarchy 
  ))

# Load, Clean, and Merge Correlates of War (COW) Data
pcon <- left_join(pcon, milcap,
                        by = c("ccode", "year"))

# Create the Log-Transformed Value of Military Personnel per capita
pcon <- pcon %>%
  mutate(lmilper = log(milper + 1)) %>%
  select(-c(milper))

# Merge Prior War Duration Variable
wd_con <- con %>%
  group_by(id) %>%
  summarise(pwar_dur = max(wardur)) %>%
  ungroup()

pcon <- inner_join(pcon, wd_con,
                   by = c("id"))

# Create Prior Ethnic War Variable
eth_con <- con %>%
  group_by(id) %>%
  summarise(peth_war = max(eth_war)) %>%
  ungroup()

pcon <- inner_join(pcon, eth_con,
                    by = c("id"))

# Create Prior Intensity Variable
int_con <- con %>%
  group_by(id) %>%
  summarise(pcint = max(intensity)) %>%
  ungroup()

pcon <- inner_join(pcon, int_con,
                   by = c("id"))

# Create Prior Military Victory Variable
mvic_con <- con %>%
  group_by(id) %>%
  summarise(pmilvic = max(mil_vic)) %>%
  ungroup()

pcon <- inner_join(pcon, mvic_con,
                   by = c("id"))

# Create Prior Rebel Victory Variable
rvic_con <- con %>%
  group_by(id) %>%
  summarise(rmilvic = max(reb_vic)) %>%
  ungroup()

pcon <- inner_join(pcon, rvic_con,
                   by = c("id"))

# Remove Unnecessary Columns
con <- con %>%
  select(-c(version, civ_war, flag_civ_war, flead_civ_war, ev_civwar, con_fail, 
            peace_fail, mil_vic, reb_vic, intensity))
  
pcon <- pcon %>%
  select(-c(version, civ_war, flag_civ_war, flead_civ_war, ev_civwar, con_fail,
            peace_fail))

# Remove Unnecessary Data Sets
rm(cow, milcap, ged, geo_pko, ucdp, vdem, eth_con, int_con, mvic_con, rvic_con, 
   ucdp_term, wd_con)
