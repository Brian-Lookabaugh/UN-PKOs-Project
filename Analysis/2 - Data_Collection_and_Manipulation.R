############################################################################
###############--------PKO Data Collection & Cleaning--------###############
############################################################################

pacman::p_load("tidyverse", # Data Manipulation and Visualization
               "DescTools", # Last Observation Carried Forward (LOCF) Command
               install = FALSE)

# Create the Base Data Set to Identify The Presence of Civil War and Post-Civil War
ucdp <- readr::read_csv("Data/ucdp-prio-acd-221.csv")

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


# Make Coups Count As Non-Civil War Observations
# Coup Information (Powell and Thyme 2011) Along With Confounders
vdem <- readr::read_csv("Data/selected_vdem_v12.csv")

vdem <- vdem %>%
  rename(eth_con = v2xpe_exlsocgr)

ucdp <- left_join(ucdp, vdem,
                  by = c("ccode" = "COWcode", "year"))

ucdp <- ucdp %>%
  mutate(coup = if_else(e_pt_coup > 0, 1, e_pt_coup)) %>%
  mutate(civ_war = if_else(# Re-Code Civil Wars As Not Cases of Civil War Where Coups Occurred
    coup == 1, 0, civ_war)) %>%
  select(-c(e_pt_coup, coup))

# Re-Code 3 Years or Less of Peace As Conflict Lulls
ucdp <- ucdp %>%
  group_by(ccode) %>%
  mutate(flag_civ_war = lag(civ_war, n = 1, order_by = ccode)) %>%
  mutate(flead_civ_war = lead(civ_war, n = 1, order_by = ccode)) %>%
  mutate(slag_civ_war = lag(civ_war, n = 2, order_by = ccode)) %>%
  mutate(slead_civ_war = lead(civ_war, n = 2, order_by = ccode)) %>%
  ungroup() %>%
  mutate(civ_war = if_else(flag_civ_war == 1 &
                             flead_civ_war == 1, 1, civ_war)) %>%
  mutate(civ_war = if_else(slag_civ_war == 1 &
                             slead_civ_war == 1, 1, civ_war)) %>%
  select(-c(flag_civ_war, flead_civ_war, slag_civ_war, slead_civ_war))

# Create an "Ever Civil War" Variable to Isolate Post-Conflict Cases
ucdp <- ucdp %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(ev_civwar = LOCF(civ_war)) %>%
  ungroup() %>%
  mutate(ev_civwar = if_else(is.na(ev_civwar), 0, ev_civwar))

# Replace NA Values for Civil War and Ever Civil War With 0
ucdp <- ucdp %>%
  mutate(civ_war = if_else(is.na(civ_war), 0, civ_war))

# Create Spell ID Variable
ucdp <- ucdp %>%
  group_by(ccode) %>%
  mutate(con_fail = if_else(# Start By Creating a Conflict/Peace Termination (Failure) Variable
    lag(civ_war == 1) & civ_war == 0, 1, 0)) %>%
  mutate(peace_fail = if_else(lag(civ_war == 0) & civ_war == 1, 1, 0)) %>%
  ungroup() %>%
  mutate(id = cumsum(peace_fail)) # The Actual ID (This Includes Non-Conflict Cases)

# Filter Non-Conflict Cases
ucdp <- ucdp %>%
  filter(ev_civwar > 0)

# Create the War Duration Variable
ucdp <- ucdp %>%
  group_by(id) %>%
  mutate(wardur = as.numeric(row_number())) %>% # Default Is Integer
  ungroup() %>%
  mutate(wardur = if_else(# Replace Peace-Year Values With 0
    civ_war == 0, 0, wardur))

# Manipulate and Merge GED Data
load("Data/ucdp_ged_22_1.RData")
ged <- GEDEvent_v22_1
rm(GEDEvent_v22_1)

ged <- ged %>%
  mutate(gwnoa = as.numeric(gwnoa)) %>%
  group_by(gwnoa, year) %>%
  summarise(deaths = max(best)) %>%
  ungroup()

ucdp <- left_join(ucdp, ged,
                  by = c("ccode" = "gwnoa", "year"))

ucdp <- ucdp %>% # Replace NA Values With 0
  mutate(deaths = as.numeric(deaths)) %>%
  mutate(deaths = if_else(is.na(deaths), 0, deaths))

# Merge the Geo-PKO Data
geo_pko <- readr::read_csv("Data/geo_pko_v.2.0.csv")

## Create PKO Dataset Omitting Interstate and No Troop Cases
geo_pko2 <- geo_pko %>%
  mutate(pko = 1) %>%
  mutate(pko = if_else(mission == "UNDOF", 0, pko)) %>%
  mutate(pko = if_else(mission == "UNTSO", 0, pko)) %>%
  mutate(pko = if_else(mission == "MINURSO", 0, pko)) %>%
  mutate(pko = if_else(mission == "UNIFIL" & year < 2006, 0, pko)) %>%
  mutate(pko = if_else(mission == "UNMOGIP", 0, pko)) %>%
  mutate(pko = if_else(mission == "UNMEE", 0, pko)) %>%
  group_by(cow_code, year) %>%
  summarise(pko = max(pko)) %>%
  ungroup()

geo_pko3 <- geo_pko %>%
  mutate(pko2 = 1) %>%
  group_by(cow_code, year) %>%
  summarise(pko2 = max(pko2)) %>%
  ungroup()

ucdp <- left_join(ucdp, geo_pko2,
                  by = c("ccode" = "cow_code", "year"))

ucdp <- left_join(ucdp, geo_pko3,
                  by = c("ccode" = "cow_code", "year"))

ucdp <- ucdp %>% # Replace NA Values for PKO With 0
  mutate(pko = if_else(is.na(pko), 0, pko)) %>%
  mutate(pko2 = if_else(is.na(pko2), 0, pko2))

# Load and Clean Correlates of War (COW) Data for Military Capacity
milcap <- readr::read_csv("Data/nmc_cow_v6.csv")

milcap <- milcap %>%
  filter(milper != -9) %>% # Remove NA Values
  select(c(ccode, year, milper)) # Keep Selected Columns

ucdp <- left_join(ucdp, milcap,
                  by = c("ccode", "year"))

# Generate Log-Transformed Values
ucdp <- ucdp %>%
  mutate(lgdppc = log(e_gdppc + 1)) %>%
  mutate(lpop = log(e_pop)) %>%
  mutate(lmilper = log(milper + 1)) %>%
  mutate(ldeaths = log(deaths + 1)) %>%
  rename(democracy = v2x_polyarchy)

# Remove Unnecessary Columns
merged <- ucdp %>%
  select(
    -c(
      e_total_fuel_income_pc,
      e_total_oil_income_pc,
      e_total_resources_income_pc,
      ...6,
      e_pop,
      e_gdppc,
      e_wb_pop,
      e_mipopula,
      version,
      con_fail,
      peace_fail,
      milper
    )
  )

# Remove Unnecessary Data Sets
rm(cow, ged, geo_pko, milcap, vdem, ucdp)
