############################################################################
###############---------------PKO IPW Analysis---------------###############
############################################################################

############################################################################
###############---------Data Collection and Cleaning---------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  install = FALSE
)

############################################################################
###############----------------------DAG---------------------###############
############################################################################

pacman::p_load(
  "ggdag", # Plot DAGs with ggplot
  "dagitty" # DAG Math
)

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

### State-Based Deaths
# IPW: Dummy PKO Treatment, ATE

# IPW: Continuous PKO Treatment, ATE

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE

# Matching: CEM, Dummy PKO Treatment, ATE

# Covariate Adjustment

# Plot Effects

### Non-State Based Deaths
# IPW: Dummy PKO Treatment, ATE

# IPW: Continuous PKO Treatment, ATE

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE

# Matching: CEM, Dummy PKO Treatment, ATE

# Covariate Adjustment

# Plot Effects

### One-Sided Violence
# IPW: Dummy PKO Treatment, ATE

# IPW: Continuous PKO Treatment, ATE

# Matching: Mahalanobis Distance, Dummy PKO Treatment, ATE

# Matching: CEM, Dummy PKO Treatment, ATE

# Covariate Adjustment

# Plot Effects

############################################################################
###############-------------Sensitivity Analysis-------------###############
############################################################################

### Aggregate Deaths

### State-Based Deaths

### Non-State Based Deaths

### One-Sided Violence
