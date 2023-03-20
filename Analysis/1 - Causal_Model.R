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
node_info <- tribble(
  ~name, ~ label, ~x, ~y,
  "X", "X", 0.2, 0.5,
  "Y", "Y", 0.8, 0.5,
  "M", "M", 0.5, 0.5,
  "Z", "Z", 0.5, 0.8,
  "C", "C", 0.5, 0.2,
  "PX", "PX", 0.1, 0.6,
  "PY", "PY", 0.9, 0.6
)

## Creating a Node Labels Object
node_labels <- node_info$label
names(node_labels) <- node_info$name

## Creating and Tidying the DAG Object
intro_dag <- dagify(
  Y ~ M + Z + PY,
  X ~ Z + PX,
  M ~ X,
  C ~ X + Y,
  exposure = "X",
  outcome = "Y",
  coords = node_info,
  labels = node_labels) %>%
  tidy_dagitty()

## Assigning Types to the Objects (Treatment, Outcome, etc.)
intro_dag <- intro_dag %>%
  mutate(
    type = case_when(
      name == "X" ~ 1,
      name == "Y" ~ 2,
      name == "Z" ~ 3,
      name == "PY" ~ 4,
      name %in% c("PX", "M", "C") ~ 5
    )
  )

## Creating the DAG Plot
idag_plot <-
  ggplot(intro_dag, aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_dag_edges() +
  geom_dag_point(aes(color = as.factor(type))) +
  geom_dag_label_repel(
    aes(label = label, fill = as.factor(type)),
    seed = 9002,
    color = "white",
    fontface = "bold"
  ) +
  scale_color_manual(values = c("#42be71", "#228b8d", "#471164", "#34608d", "grey20")) +
  scale_fill_manual(values = c("#42be71", "#228b8d", "#471164", "#34608d", "grey20")) +
  guides(color = "none", fill = "none") +
  theme_dag()

ggsave(
  "intro_dag.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/UN PKOs Project/Graphics"
)

# PKO DAG
node_info2 <- tribble(
  ~name, ~label, ~x, ~y,
  "dev", "Development", 0.5, 1.4,
  "pko", "UN PKO", 0, 1.4,
  "ci", "Conflict Intensity", 0.5, 1,
  "gmc", "Gov. Military Capacity", -0.5, 1.55,
  "dem", "Democracy", 1, 1.5,
  "eth", "Ethnic Contention", 0.25, 1.75
)

## Creating a Node Labels Object
node_labels2 <- node_info2$label
names(node_labels2) <- node_info2$name

## Creating and Tidying the DAG Object
pko_dag <- dagify(
  dev ~ pko + ci + gmc + dem + eth,
  pko ~ ci + gmc + eth,
  ci ~ gmc + eth,
  dem ~ eth,
  exposure = "pko",
  outcome = "dev",
  coords = node_info2,
  labels = node_labels2) %>%
  tidy_dagitty()

## Assigning Types to the Objects (Treatment, Outcome, etc.)
pko_dag <- pko_dag %>%
  mutate(
    type = case_when(
      name == "pko" ~ 1,
      name == "dev" ~ 2,
      name %in% c("ci", "gmc", "eth") ~ 3,
      name == "dem" ~ 4
    )
  )

## Creating the DAG Plot
pko_plot <-
  ggplot(pko_dag, aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_dag_edges() +
  geom_dag_point(aes(color = as.factor(type))) +
  geom_dag_label_repel(
    aes(label = label, fill = as.factor(type)),
    seed = 1234,
    color = "white",
    fontface = "bold",
    box.padding = 2
  ) +
  scale_color_manual(values = c("#42be71", "#228b8d", "#471164", "#34608d")) +
  scale_fill_manual(values = c("#42be71", "#228b8d", "#471164", "#34608d")) +
  guides(color = "none", fill = "none") +
  theme_dag()

ggsave(
  "pko_dag.png",
  width = 10,
  height = 6,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/UN PKOs Project/Graphics"
)