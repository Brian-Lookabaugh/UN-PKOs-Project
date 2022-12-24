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
