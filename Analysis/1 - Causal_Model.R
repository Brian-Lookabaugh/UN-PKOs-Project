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
  ~name, ~ label, ~x, ~y, ~node_color,
  "X", "X", 0.2, 0.5, "#008000",
  "Y", "Y", 0.8, 0.5, "#0000FF",
  "M", "M", 0.5, 0.5, "#808080",
  "Z", "Z", 0.5, 0.8, "#FF0000",
  "C", "C", 0.5, 0.2, "#808080",
  "PX", "PX", 0.1, 0.6, "#008000",
  "PY", "PY", 0.9, 0.6, "#0000FF"
)

node_labels <- node_info$label
names(node_labels) <- node_info$name

colors <- c(X = "#008000", Y = "#0000FF", M = "#808080", Z = "#FF0000",
            C = "#808080", PX = "#008000", PY = "#0000FF")

intro_dag <- dagify(
  Y ~ M + Z + PY,
  X ~ Z + PX,
  M ~ X,
  C ~ X + Y,
  exposure = "X",
  outcome = "Y",
  coords = node_info,
  labels = node_labels)

tidy_idag <- intro_dag %>%
  tidy_dagitty() %>%
  node_status()

ggplot(tidy_idag, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = colors)) +
  geom_dag_label_repel(aes(label = label, fill = status), seed = 8373,
                       color = "white", fontface = "bold") +
  scale_color_manual(values = node_colors, na.value = "grey20") +
  scale_fill_manual(values = node_colors, na.value = "grey20") +
  guides(color = FALSE, fill = FALSE) + 
  theme_dag()

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
    size = 6,
    family = "serif"
  ) +
  guides(color = "none", fill = "none") +
  theme_dag()

ggsave(
  "intro_dag.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/UN PKOs Project/Graphics"
)

# PKO DAG
pdag_coords <- list(
  x = c(pko = 0, dev = 1, ci = 0.5, gmc = -1,
        dem = 1, nrw = -1, eth = 0.5),
  y = c(pko = 1.4, dev = 1.4, ci = 2.3, gmc = 2,
        dem = 2, nrw = 0.75, eth = 0.5)
)
  
pdag_labs <- list(
  dev = "Development",
  pko = "UN PKO",
  ci = "Conflict Intensity",
  gmc = "Gov. Military Capacity",
  dem = "Democracy",
  nrw = "Natural Resources",
  eth = "Ethnic Contention"
)

pko_dag <- dagify(
  dev ~ pko + ci + gmc + dem + eth + nrw,
  pko ~ ci + gmc + eth + nrw,
  ci ~ gmc + nrw + eth,
  gmc ~ nrw,
  dem ~ eth + nrw,
  eth ~ nrw,
  exposure = "pko",
  outcome = "dev",
  coords = pdag_coords,
  labels = pdag_labs) %>%
  tidy_dagitty()

pko_dag <- pko_dag %>% 
  mutate(
    .text_color = case_when(
      name == "pko" ~ "green",
      name %in% c("dev", "dem") ~ "blue",
      name %in% c("ci", "gmc", "eth", "nrw") ~ "red"
    ) 
  )

pdag_plot <- ggplot(
  pko_dag,
  aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_dag_edges() +
  geom_dag_text(
    aes(label = label, color = .text_color),
    size = 4,
    family = "serif"
  ) +
  guides(color = "none", fill = "none") +
  theme_dag()

ggsave(
  "pko_dag.png",
  width = 11,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/UN PKOs Project/Graphics"
)