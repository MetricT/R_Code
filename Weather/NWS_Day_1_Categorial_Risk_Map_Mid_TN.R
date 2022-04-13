library(tidyverse)
library(tigris)
library(maps)
library(sf)

### Plots the NWS Day 1 Categorical Risk map for Middle TN
### Data from:  https://www.spc.noaa.gov/gis/

### Cache TIGRIS data to speed up future map renders
options(tigris_use_cache = TRUE)

### Load shapefile with the current day's outlook from NOAA
day1_categorical <- read_sf("https://www.spc.noaa.gov/products/outlook/day1otlk_cat.kmz")

### Bounding box for our map
map_x_min <- -84.0
map_x_max <- -90.0
map_y_min <-  34.5
map_y_max <-  37.3

### Load map of US cities
map_cities <-
  maps::world.cities %>% 
  filter(
    country.etc == "USA", 
    long >= map_x_max, long <= map_x_min, 
    lat >= map_y_min, lat <= map_y_max)

### Create a local map using a bounding box, and change colors to their standard representation
map_weather <- 
  day1_categorical %>%
  st_transform(crs = "NAD83") %>%
  st_crop(., xmin = map_x_min, xmax = map_x_max, ymin = map_y_min, ymax = map_y_max) %>%
  mutate(color = case_when(
    Name == "General Thunder" ~ "lightgreen",
    Name == "Marginal Risk"   ~ "green",
    Name == "Slight Risk"     ~ "yellow",
    Name == "Enhanced Risk"   ~ "orange",
    Name == "Moderate Risk"   ~ "red",
    Name == "Severe Risk"     ~ "darkred",
  )) 

### Load state/county outline maps from TIGRIS
map_county <- 
  tigris::counties() %>%
  st_crop(., xmin = map_x_min, xmax = map_x_max, ymin = map_y_min, ymax = map_y_max)

map_state <- 
  tigris::states() %>%
  st_crop(., xmin = map_x_min, xmax = map_x_max, ymin = map_y_min, ymax = map_y_max)

map_interstates <-
  tigris::primary_roads() %>%
  filter(RTTYP == "I") %>%
  st_crop(., xmin = map_x_min, xmax = map_x_max, ymin = map_y_min, ymax = map_y_max)

weather_map <-
  ggplot() +
  theme_void() + 
  theme(
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
    ) +
  labs(title = "NWS Day 1 Categorical Risk Map", subtitle = "(Middle TN, 2022-04-13 16:26:58 UTC 2022)") +
  geom_sf(data = map_state, fill = NA, size = 0.4) + 
  geom_sf(data = map_county, fill = NA, size = 0.2) +
  geom_sf(data = map_interstates, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = map_interstates, fill = NA, color = "white", linetype = "dotted", size = 0.1) +
  #geom_point(data = map_cities, aes(x = long, y = lat)) + 
  geom_sf(data = map_weather, fill = map_weather$color, size = 0.2, alpha = 0.5) # +
  # This should work, but doesn't
  #coord_sf(xlim = c(map_x_min, map_x_max), ylim = c(map_y_min, map_y_max), expand = FALSE) +

print(weather_map)
