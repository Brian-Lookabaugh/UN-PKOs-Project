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

# Custom ggplot Function

# Generate the DAG

pko_dag <- dagify(peace ~ pko + democ + nat_res + peace_agg + gov_cap + pop + eth_frac +
                    dev + bat_deaths,
                  pko ~ bat_deaths + dev + gov_cap + nat_res,
                  democ ~ nat_res,
                  peace_agg ~ gov_cap + bat_deaths + pko + nat_res,
                  gov_cap ~ dev + nat_res,
                  pop ~ dev,
                  dev ~ eth_frac + nat_res + democ,
                  bat_deaths ~ eth_frac + gov_cap + peace_agg,
                  exposure = "pko",
                  outome = "peace",
                  coords = list(x = c(peace = 7, pko = 3, democ = 4, nat_res = 5, peace_agg = 6,
                                      gov_cap = 7, pop = 1, eth_frac = 4, dev = 5, bat_deaths = 6),
                                y = c(peace = 2, pko = 2, democ = 3, nat_res = 1, peace_agg = 3,
                                      gov_cap = 4, pop = 5, eth_frac = 5, dev = 2, bat_deaths = 2)),
                  labels = c(peace = "Peace", pko = "PKO", democ = "Democracy",
                             nat_res = "Natural Resources", peace_agg = "Peace Agreement",
                             gov_cap = "Government Capacity", pop = "Population",
                             eth_frac = "Ethnic Diversity", dev = "Development",
                             bat_deaths = "Battle Deaths"))

# Convert DAG Object Into a Tidy Frame for Plotting

tidy_pko_dag <- pko_dag %>%
  tidy_dagitty() %>%
  node_status()

status_color <- c(exposure = "#21918c", outcome = "#440154", latent = "grey50")

# Further Graph Customization

ggplot(tidy_pko_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = "status")) +
  geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                       color = "white", fontface = "bold") +
  scale_color_manual(values = status_colors, na.value = "grey20") +
  scale_fill_manual(values = status_color, na.value = "grey20") +
  guides(color = FALSE, fill = FALSE) +
  theme_dag()

############################################################################
###############---------Data Collection and Cleaning---------###############
############################################################################


############################################################################
###############--------------IPW/Matching Set-Up-------------###############
############################################################################

### Aggregate Deaths
# IPW: Dummy PKO Treatment, ATE

# IPW: Continuous PKO Treatment, ATE

# Inspect for Extreme Weights

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE

# Matching: CEM, Dummy PKO Treatment, ATE

# Graph Weights

# Balancing Tables

# Density Plots

### State-Based Deaths
# IPW: Dummy PKO Treatment, ATE

# IPW: Continuous PKO Treatment, ATE

# Inspect for Extreme Weights

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE

# Matching: CEM, Dummy PKO Treatment, ATE

# Graph Weights

# Balancing Tables

# Density Plots

### Non-State Based Deaths
# IPW: Dummy PKO Treatment, ATE

# IPW: Continuous PKO Treatment, ATE

# Inspect for Extreme Weights

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE

# Matching: CEM, Dummy PKO Treatment, ATE

# Graph Weights

# Balancing Tables

# Density Plots

### One-Sided Violence
# IPW: Dummy PKO Treatment, ATE

# IPW: Continuous PKO Treatment, ATE

# Inspect for Extreme Weights

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE

# Matching: CEM, Dummy PKO Treatment, ATE

# Graph Weights

# Balancing Tables

# Density Plots

############################################################################
###############-------------IPW/Matching Analysis------------###############
############################################################################

### Aggregate Deaths
# IPW: Dummy PKO Treatment, ATE

# IPW: Continuous PKO Treatment, ATE

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE

# Matching: CEM, Dummy PKO Treatment, ATE

# Covariate Adjustment

# Plot Effects

# Regression Table

### State-Based Deaths
# IPW: Dummy PKO Treatment, ATE

# IPW: Continuous PKO Treatment, ATE

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE

# Matching: CEM, Dummy PKO Treatment, ATE

# Covariate Adjustment

# Plot Effects

# Regression Table

### Non-State Based Deaths
# IPW: Dummy PKO Treatment, ATE

# IPW: Continuous PKO Treatment, ATE

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE

# Matching: CEM, Dummy PKO Treatment, ATE

# Covariate Adjustment

# Plot Effects

# Regression Table

### One-Sided Violence
# IPW: Dummy PKO Treatment, ATE

# IPW: Continuous PKO Treatment, ATE

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE

# Matching: CEM, Dummy PKO Treatment, ATE

# Covariate Adjustment

# Plot Effects

# Regression Table

############################################################################
###############-------------Sensitivity Analysis-------------###############
############################################################################

### Aggregate Deaths

### State-Based Deaths

### Non-State Based Deaths

### One-Sided Violence
