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

# Load and Clean UCDP Georeferenced Data (GED)

load("Data/ucdp_ged_22_1.RData")
ged <- GEDEvent_v22_1
rm(GEDEvent_v22_1)

# Collapse the Data to State-Year Level and Get Sums and Counts of Battle Deaths and 
# Lethal Events For Different Types of Violence 
# (State-Based, Non-State Based, and OSV - One Sided Violence)

# State-Based Violence
sb <- ged %>%
  group_by(country, gwnoa, year) %>%
  filter(type_of_violence == 1) %>%
  summarise(sb_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE),
            sb_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

# Non-State Based Violence
nsb <- ged %>%
  group_by(country, gwnoa, year) %>%
  filter(type_of_violence == 2) %>%
  summarise(nsb_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE),
            nsb_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

# OSV
osv <- ged %>%
  group_by(country, year) %>%
  filter(type_of_violence == 3) %>%
  summarise(osv_death = sum(deaths_a + deaths_b + deaths_civilians, na.rm = TRUE),
            osv_event = n_distinct(id, na.rm = TRUE)) %>%
  ungroup()

# Merge the Collapsed Data

ged_col <- full_join(sb, nsb,
                     by = c("country", "year")) %>%
  full_join(osv,
            by = c("country", "year"))

# Remove Older Data Sets
rm(sb)
rm(nsb)
rm(osv)

# Load and Clean Geo-PKO Data

geo_pko <- readr::read_csv("Data/geo_pko_v.2.0.csv")

# Collapse the Data to State-Year Level and Get Sums of PKO Troops

geo_pko <- geo_pko %>%
  mutate_at(c('no.troops'), as.numeric) %>% # Convert Troop Count to Numeric
  filter(no.troops != "unknown", na.rm = TRUE) %>% # Remove Unknown Values
  group_by(country, year) %>% 
  summarise(pko_troops = sum(no.troops, na.rm = TRUE)) %>%
  mutate(pko = 1) %>% # Create a Variable Denoting the Presence of a PKO 
  ungroup()

# Merge the Geo-PKO Data With the UCDP GED Data

ged_pko <- full_join(ged_col, geo_pko,
                     by = c('country', 'year'))

# Clean Up the Merged Data

ged_pko <- ged_pko %>%
  mutate(pko = if_else( # Replace NA Values for PKO With 0
    is.na(pko), 0, 1
  )) %>%
  mutate(pko_troops = if_else(
    is.na(pko_troops), 0, pko_troops)) # Replace NA Value for PKO Troops With 0

# Load and Clean V-Dem Data

############################################################################
###############--------------IPW/Matching Set-Up-------------###############
############################################################################

#######-------State-Based-------#######

# IPW: Dummy PKO Treatment, ATE, Deaths

# IPW: Dummy PKO Treatment, ATE, Events

# IPW: Continuous PKO Treatment, ATE, Deaths

# IPW: Continuous PKO Treatment, ATE, Events

# Inspect for Extreme Weights

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Events

# Matching: CEM, Dummy PKO Treatment, ATE, Deaths

# Matching: CEM, Dummy PKO Treatment, ATE, Events

# Graph Weights

# Balancing Tables

# Density Plots

#######-------Non-State Based-------#######

# IPW: Dummy PKO Treatment, ATE, Deaths

# IPW: Dummy PKO Treatment, ATE, Events

# IPW: Continuous PKO Treatment, ATE, Deaths

# IPW: Continuous PKO Treatment, ATE, Events

# Inspect for Extreme Weights

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Events

# Matching: CEM, Dummy PKO Treatment, ATE, Deaths

# Matching: CEM, Dummy PKO Treatment, ATE, Events

# Graph Weights

# Balancing Tables

# Density Plots

#######-------One-Sided Violence-------#######

# IPW: Dummy PKO Treatment, ATE, Deaths

# IPW: Dummy PKO Treatment, ATE, Events

# IPW: Continuous PKO Treatment, ATE, Deaths

# IPW: Continuous PKO Treatment, ATE, Events

# Inspect for Extreme Weights

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Events

# Matching: CEM, Dummy PKO Treatment, ATE, Deaths

# Matching: CEM, Dummy PKO Treatment, ATE, Events

# Graph Weights

# Balancing Tables

# Density Plots

############################################################################
###############-------------IPW/Matching Analysis------------###############
############################################################################

#######-------State-Based-------#######

# IPW: Dummy PKO Treatment, ATE, Deaths

# IPW: Dummy PKO Treatment, ATE, Events

# IPW: Continuous PKO Treatment, ATE, Deaths

# IPW: Continuous PKO Treatment, ATE, Events

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Events

# Matching: CEM, Dummy PKO Treatment, ATE, Deaths

# Matching: CEM, Dummy PKO Treatment, ATE, Events

# Covariate Adjustment, Deaths

# Covariate Adjustment, Events

# Plot Effects

# Regression Table

#######-------Non-State Based-------#######

# IPW: Dummy PKO Treatment, ATE, Deaths

# IPW: Dummy PKO Treatment, ATE, Events

# IPW: Continuous PKO Treatment, ATE, Deaths

# IPW: Continuous PKO Treatment, ATE, Events

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Events

# Matching: CEM, Dummy PKO Treatment, ATE, Deaths

# Matching: CEM, Dummy PKO Treatment, ATE, Events

# Covariate Adjustment, Deaths

# Covariate Adjustment, Events

# Plot Effects

# Regression Table

#######-------One-Sided Violence-------#######

# IPW: Dummy PKO Treatment, ATE, Deaths

# IPW: Dummy PKO Treatment, ATE, Events

# IPW: Continuous PKO Treatment, ATE, Deaths

# IPW: Continuous PKO Treatment, ATE, Events

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Deaths

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE, Events

# Matching: CEM, Dummy PKO Treatment, ATE, Deaths

# Matching: CEM, Dummy PKO Treatment, ATE, Events

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
