############################################################################
###############--------------------PKO DAG-------------------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation
  "dagitty", # DAG Math
  "ggdag", # DAG Visualization
  install = FALSE
)

# Introductory DAG

idag_coords <- list(
  x = c(X = 0.2, Y = 0.8, M = 0.5, Z = 0.5, C = 0.5, PX = 0.1, PY = 0.9),
  y = c(X = 0.5, Y = 0.5, M = 0.5, Z = 0.8, C = 0.2, PX = 0.6, PY = 0.6)
)

intro_dag <- dagify(
  Y ~ M + Z + PY,
  X ~ Z + PX,
  M ~ X,
  C ~ X + Y,
  exposure = "X",
  outcome = "Y",
  coords = idag_coords %>%
  tidy_dagitty()
  )

idag_plot <- ggplot(
  intro_dag,
  aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_dag_edges() +
  geom_dag_text(
    color = "black",
    size = 8,
    family = "serif"
  ) +
  guides(color = "none", fill = "none") +
  theme_dag()

ggsave(
  "intro_dag.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)

# PKO DAG