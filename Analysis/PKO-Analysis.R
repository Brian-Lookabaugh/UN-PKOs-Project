########################################################################
###############---------------PKO Analysis---------------###############
########################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "tidysynth", # Tidy Implementation of the Synthetic Control Method
  "DescTools", # Carrying Values of Observations Forward (LOCF)
  "haven", # Importing Data from Stata
  "sf", # Maps
  "rnaturalearth", # Maps
  "rnaturalearthdata", # Maps
  install = FALSE
)

############################################################
########--------Data Collection and Cleaning--------########
############################################################

## UCDP Data

load("C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Data/ucdp_prio_acd_221.RData")
ucdp <- ucdp_prio_acd_221

# Drop Non-Civil War Cases (1K Threshold)

ucdp <- ucdp %>%
  mutate(gwno_a = as.numeric(gwno_a)) %>% # Convert the country code to a numeric
  filter(type_of_conflict == 3 & cumulative_intensity == 1) %>%
  mutate(civ_war = 1) %>%
  
  # Collapse Data Into Country-Year Units
  group_by(gwno_a, year) %>%
  summarise(civ_war = max(civ_war)) %>%
  ungroup()

# Merge COW Country-Year Data

states <- read.csv("C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Data/system2016.csv")

ucdp <- full_join(ucdp, states,
                   by = c("gwno_a" = "ccode", "year"))

# Drop Cases That Have Never Experienced a Civil War

ucdp <- ucdp %>%
  rename(ccode = gwno_a) %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(prior_civ_war = LOCF(civ_war)) %>%
  ungroup() %>%
  filter(prior_civ_war == 1) %>%
  mutate(civ_war = if_else( # Replace NA Civil War Values With 0
    is.na(civ_war), 0, civ_war
  )) %>%

# Generate Conflict Recurrence and Termination Variables
  group_by(ccode) %>%
  mutate(lag_civ_war = lag(civ_war, n = 1, order_by = ccode)) %>%
  ungroup() %>%
  mutate(recur = if_else(
    civ_war == 1 & lag_civ_war == 0, 1, 0
  )) %>%
  mutate(recur = if_else(
    is.na(recur), 0, recur
  )) %>%
  mutate(termination = if_else(
    civ_war == 0 & lag_civ_war == 1, 1, 0
  )) %>%
  mutate(termination = if_else(
    is.na(termination), 0, termination
  ))

# Merge UCDP One-Sided Violence Data

load("C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Data/ucdp-onesided-221.RData")
osv <- ucdp_onesided_221

## PKO Data

pko <- read_dta("Data/formattedmullenbach2013pkodata.dta")

ucdp <- left_join(ucdp, pko,
                  by = c("ccode", "year"))

ucdp <- ucdp %>%
  group_by(ccode) %>%
  mutate(pko_pres = LOCF(PKO)) %>% # Dummy for If a PKO Was Present In a Country in the Past
  mutate(ever_pko = pko_pres) %>% # Simply a Dummy for Whether a PKO Was Present In a Country At All (For Visualizations)
  fill(ever_pko, .direction = "downup") %>%
  fill(ever_pko, .direction = "updown") %>%
  mutate(ever_pko = if_else(
    is.na(ever_pko), 0, ever_pko
  )) %>%
  ungroup()

## V-Dem Data

vdem <- read.csv("C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Data/selectedvdemdata.csv")

vdem <- vdem %>%
  rename(ccode = COWcode) %>%
  select(ccode, country_name, year, v2x_polyarchy, e_peinfmor, e_peaveduc, e_pop, e_gdppc)

ucdp <- left_join(ucdp, vdem,
                  by = c("ccode", "year"))
  
## Military Capacity Data
  
mil_per <- read.csv("C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Data/NMC-60-abridged.csv")

mil_per <- mil_per %>%
  filter(milper != -9) %>%
  mutate(lmilper = log(milper + 1)) %>%
  select(ccode, year, lmilper)

ucdp <- left_join(ucdp, mil_per,
                  by = c("ccode", "year"))

## (Dependent Variable Data)
  
var <- 

## Final Data Cleaning (Rename)

synth_data <- ucdp %>%

###################################################################
########--------Generate a Map of PKO Distribution---------########
###################################################################

# Load the Earth Spatial Polygons

world_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Rename Country IDs for Merging That Don't Match

world_sf <- world_sf %>%
  mutate(sovereignt = str_replace(
    sovereignt, "Republic of Serbia", "Serbia"))

# Merge Earth Data With Synth Data Set

map_data <- ucdp %>%
  full_join(world_sf, by = c("country_name" = "sovereignt")) %>%
  # Drop Antarctica
  filter(name_long != "Antarctica")

# Create Text PKO Data

map_data <- map_data %>%
  mutate(ever_pko_txt = case_when(
    ever_pko == 1 ~ "PKO",
    ever_pko == 0 ~ "No PKO"
  ))

# Start Creating the Map

pko_map <- map_data %>% 
  ggplot() + 
  geom_sf(
    aes(geometry = geometry, fill = ever_pko_txt),
    color = "black",
    size = .2,
    na.rm = T
  ) +
  
# Adjust Color Scales
  
  scale_fill_viridis_d(
    na.translate = FALSE,
    begin = 0.90,
    end = 0.50,
    option = "mako"
  ) +
  
# Legend and Margins Customization
  
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(),  
    axis.ticks.y=element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 8, family = "serif", vjust = 1),
    legend.key.height = unit(0.25, 'cm'),
    legend.text = element_text(size = 6, family = "serif"),
    legend.key.width = unit(1, 'cm'),
    plot.margin = unit(c(-1, -0.7, -1, -0.7), "cm")
  ) +
  
  # Add Labels
  
  labs(
    fill = "")

# Save the Map

ggsave(
  "pko_map.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)

########################################################
########--------Synthetic Control Set-Up--------########
########################################################

#####################################################################
########--------Synthetic Control Graphics and Tables--------########
#####################################################################

## Time Series Plot

## Difference in Synthetic and Observed Plot

## Unit and Variable Weights Plot

## Table Comparing Synthetic to Observed

## Unit Placebos

## Unit Placebos With Extreme Values

## Post/Pre-MSPE

## Tables With Significance of Unit Treatments