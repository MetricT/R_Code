library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(viridis)
library(cowplot)

# Enable caching since some of these maps are big
options(tigris_use_cache = TRUE)

# Locations of the datafiles in the Microsoft github:

# Get maps from Census/TIGRIS
map_zip   <- zctas(year = 2021)
map_state <- states(year = 2021) %>% filter(STATEFP == "47")
map_co    <- counties(year = 2021, state = "47")

# Take the ZIP code map of the US, and find the intersection with TN
map <- map_zip %>% st_intersection(map_state)
# Or to do a single county
#map <- map_zip %>% st_intersection(map_co %>% filter(COUNTYFP == "037"))

# Download datasets on broadband usage from Microsoft's github repo
data_ms <- 
  "https://raw.githubusercontent.com/microsoft/USBroadbandUsagePercentages/master/dataset/broadband_data_zipcode.csv" %>%
  read_csv() %>%
  select(`POSTAL CODE`, `BROADBAND USAGE`) %>%
  rename(GEOID20 = 1, broadband_usage = 2) %>%
  mutate(GEOID20 = as.character(GEOID20))

data_fcc <- 
  "https://raw.githubusercontent.com/microsoft/USBroadbandUsagePercentages/master/dataset/broadband_data_2020October.csv" %>%
  read_csv(col_type = "ccccc", skip = 18) %>%
  filter(ST == "TN") %>%
  select(2, 4) %>%
  rename(GEOID = 1, fcc_broadband = 2) %>%
  mutate(GEOID = str_pad(GEOID, 5, pad = "0", side = "left")) %>%
  mutate(fcc_broadband = as.numeric(fcc_broadband))

# Merge the maps with the data
map_data <- map    %>% left_join(data_ms, by = "GEOID20")
map_fcc  <- map_co %>% left_join(data_fcc, by = "GEOID")

# Plot the graphs
g_map_ms <-
  ggplot() +
  theme_void() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    legend.key.width = unit(1, 'cm'),
    legend.position = "bottom"
    ) + 
  geom_sf(data = map_data, size = 0.1, aes(fill = broadband_usage)) + 
  scale_fill_viridis(direction = -1, option = "magma", labels = scales::percent_format(accuracy = 1), name = "") +
  labs(title = "Broadband usage in TN based on Microsoft data",
       caption = "Data:  https://github.com/microsoft/USBroadbandUsagePercentages")

g_map_fcc <-
  ggplot() +
  theme_void() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    legend.key.width = unit(1, 'cm'),
    legend.position = "bottom"
  ) + 
  geom_sf(data = map_fcc, size = 0.1, aes(fill = fcc_broadband)) + 
  scale_fill_viridis(direction = -1, option = "magma", limits = c(0, 1), 
                     labels = scales::percent_format(accuracy = 1), name = "") +
  geom_sf(data = map_co, size = 0.3, fill = NA, color = "darkseagreen4") +
  labs(title = "Broadband availability in TN according to FCC")

plot_grid(
  g_map_fcc,
  g_map_ms,
  ncol = 1, align = "hv")