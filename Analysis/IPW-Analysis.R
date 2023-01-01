############################################################################
###############---------------PKO IPW Analysis---------------###############
############################################################################

############################################################################
###############----------------------DAG---------------------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "ggdag", # Plot DAGs with ggplot
  "dagitty", # DAG Math
  "broom", # Converting Model Output to Data Frames
  install = FALSE
)

# Generate the DAG

pko_dag <- dagify(peace ~ pko + democ + nat_res + gov_cap + pop + eth_frac + dev,
                  pko ~ dev + gov_cap + nat_res,
                  democ ~ nat_res,
                  gov_cap ~ dev + nat_res,
                  pop ~ dev,
                  dev ~ eth_frac + nat_res + democ,
                  exposure = "pko",
                  outcome = "peace",
                  coords = list(x = c(peace = 3.5, pko = 1, democ = 8, nat_res = 1,
                                      gov_cap = 3, pop = 8, eth_frac = 8, dev = 4),
                                y = c(peace = 3, pko = 4, democ = 3, nat_res = 1,
                                      gov_cap = 4, pop = 1, eth_frac = 4, dev = 1)),
                  labels = c(peace = "Peace", pko = "PKO", democ = "Democracy",
                             nat_res = "Natural Resources", gov_cap = "Government Capacity", 
                             pop = "Population", eth_frac = "Ethnic Diversity", 
                             dev = "Development"))

adjustmentSets(pko_dag)

# Convert DAG Object Into a Tidy Frame for Plotting

tidy_pko_dag <- pko_dag %>%
  tidy_dagitty() %>%
  node_status()

status_colors <- c(exposure = "#21918c", outcome = "#440154", latent = "grey50")

# Further Graph Customization

dag <- ggplot(tidy_pko_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(aes(label = label, fill = status),
                       color = "white", fontface = "bold") +
  scale_color_manual(values = status_colors, na.value = "grey20") +
  scale_fill_manual(values = status_colors, na.value = "grey20") +
  guides(color = "none", fill = "none") +
  theme_dag()

ggsave(
  "pko_dag.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)

############################################################################
###############---------Data Collection and Cleaning---------###############
############################################################################

pacman::p_load(
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

# Collapse the GED Data to State-Year Level and Get Sums and Counts of Battle Deaths and 
# Lethal Events For Different Types of Violence 
# (State-Based, Non-State Based, and OSV - One Sided Violence)

# State-Based Violence
sb <- ged %>%
  mutate(gwnoa = as.numeric(gwnoa)) %>%
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
  
# Assign Cow Codes to Non-State Based Incidents

nsb <- nsb %>%
  mutate(gwnoa = countrycode(
    nsb$country, "country.name", "gwn"
  )) %>%
  mutate(gwnoa = if_else(
    country == "Serbia (Yugoslavia)", 345, gwnoa # Replace NA Values for Yugoslavia
  ))

# OSV
osv <- ged %>%
  mutate(gwnoa = as.numeric(gwnoa)) %>%
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
  # Convert Outcome Variables to Numeric
  mutate_at(c("sb_death", "sb_event", "nsb_death", "nsb_event", "osv_death", "osv_event"),
              as.numeric) %>%
  # Keep Select Variables
  select(ccode, year, sb_death, sb_event, nsb_death, nsb_event, osv_death, osv_event,
         pko, pko_troops, lnatres, lgdppc, lpop, lmilper)

# Remove Older Data Sets
rm(prio)
rm(ged)
rm(ged_col)
rm(geo_pko)
rm(vdem)
rm(cow)

############################################################################
###############--------------IPW/Matching Set-Up-------------###############
############################################################################

#######-------State-Based-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths

# IPW: Dummy PKO Treatment, ATT, Events

# Inspect for Extreme Weights

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths

# Matching: CEM, Dummy PKO Treatment, ATT, Events

# Balancing Tables

#######-------Non-State Based-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths

# IPW: Dummy PKO Treatment, ATT, Events

# Inspect for Extreme Weights

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths

# Matching: CEM, Dummy PKO Treatment, ATT, Events

# Balancing Tables

#######-------One-Sided Violence-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths

# IPW: Dummy PKO Treatment, ATT, Events

# Inspect for Extreme Weights

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths

# Matching: CEM, Dummy PKO Treatment, ATT, Events

# Balancing Tables

############################################################################
###############-------------IPW/Matching Analysis------------###############
############################################################################

#######-------State-Based-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths

# IPW: Dummy PKO Treatment, ATT, Events

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths

# Matching: CEM, Dummy PKO Treatment, ATT, Events

# Covariate Adjustment, Deaths

# Covariate Adjustment, Events

# Plot Effects

# Regression Table

#######-------Non-State Based-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths

# IPW: Dummy PKO Treatment, ATT, Events

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths

# Matching: CEM, Dummy PKO Treatment, ATT, Events

# Covariate Adjustment, Deaths

# Covariate Adjustment, Events

# Plot Effects

# Regression Table

#######-------One-Sided Violence-------#######

# IPW: Dummy PKO Treatment, ATT, Deaths

# IPW: Dummy PKO Treatment, ATT, Events

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATT, Events

# Matching: CEM, Dummy PKO Treatment, ATT, Deaths

# Matching: CEM, Dummy PKO Treatment, ATT, Events

# Covariate Adjustment, Deaths

# Covariate Adjustment, Events

# Plot Effects

# Regression Table

############################################################################
###############-------------Sensitivity Analysis-------------###############
############################################################################

### State-Based Deaths

### Non-State Based Deaths

### One-Sided Violence
